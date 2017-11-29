/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


// -*-C++-*-
//                  LNO Fission Gather/Scatter Conditional Code
//                  -------------------------------------------
//

/* ====================================================================
 * ====================================================================
 *
 * Module: fis_gthr.cxx
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.fis_gthr.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Fiss conditional code to improve code generation
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.fis_gthr.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <stdlib.h>
#include "defs.h"
#include "wn.h"
#include "wn_map.h"
#include "model.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "lwn_util.h"
#include "ff_utils.h"
#include "scalar_expand.h"
#include "fission.h"
#include "lnopt_main.h"
#include "opt_du.h"
#include "access_vector.h"
#include "btree.h"
#include "reduc.h"
#include "lno_bv.h"
#include "snl.h"
#include "name.h"
#include "inner_fission.h"

#include "targ_sim.h"
#include "config_targ_opt.h"
#include "config_targ.h"
#include "cxx_template.h"
#include "cxx_hash.h"
#include "wintrinsic.h"
#include "stab.h"
#include "fis_gthr.h"
#include "sxlimit.h"

#pragma weak New_Construct_Id

typedef HASH_TABLE<WN*,VINDEX16> WN2VINDEX;
typedef HASH_TABLE<WN*,UINT> WN2UINT;
typedef HASH_TABLE<WN*,INT> WN2INT;
typedef DYN_ARRAY<UINT> UINT_DYN_ARRAY;
typedef DYN_ARRAY<VINDEX16> VINDEX16_DYN_ARRAY;

#define ESTIMATED_SIZE 100	// used to initialized hash table, etc.
#define Iteration_Count_Threshold 10 // threshold to determine if a loop
                                     // has too few a number of iterations

extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
static ARRAY_DIRECTED_GRAPH16 *adg;	// PU array dep. graph
extern DU_MANAGER *Du_Mgr;          	// PU DU manager

static MEM_POOL PHASE25_default_pool;	// phase 25 private mem pool
static INT index_counter = 0;
static INT64 access_counter = 0;

extern "C" {
  void dump_tree (WN *wn);
  void dump_wn (WN *wn);
}

static BOOL has_unbalanced_if(WN *loop)
{
  Is_True(WN_opcode(loop) == OPC_DO_LOOP, ("loop must be an OPC_DO_LOOP"));
  
  for (WN* stmt=WN_first(WN_do_body(loop)); stmt; stmt=WN_next(stmt)){
    if ((WN_opcode(stmt)==OPC_IF) && WN_else_is_empty(stmt))
      return TRUE;
  }
  
  return FALSE;
}

static
void Gather_Scatter_Walk(WN *wn) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && 
	!Do_Loop_Has_Gotos(wn) &&
	!Do_Loop_Has_Calls(wn) && !Do_Loop_Is_Mp(wn) && has_unbalanced_if(wn)){
      Fiss_Gather_Inner_Loop(wn);
    } else
      Gather_Scatter_Walk(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      Gather_Scatter_Walk(stmt);
      stmt = WN_next(stmt);
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Gather_Scatter_Walk(WN_kid(wn,kidno));
    }
}

// initializes the static global variables and the mem pools used in
// phase 25
extern void Fiss_Gather_Loop(WN* func_nd,
			     ARRAY_DIRECTED_GRAPH16 *array_dependence_graph)
{
  adg = array_dependence_graph;
  MEM_POOL_Initialize(&PHASE25_default_pool, "PHASE25_default_pool", FALSE);
  MEM_POOL_Push(&PHASE25_default_pool);
  Gather_Scatter_Walk(func_nd);
  MEM_POOL_Pop(&PHASE25_default_pool);
  MEM_POOL_Delete(&PHASE25_default_pool);
}

// Borrowed from "separate_loop_and_scalar_expand".
// Only perform loop separation, no scalar_expansion;
// also don't care about reg_model.
static void separate_loop_by_scc
(
 UINT_DYN_ARRAY* new_loops,
 FF_STMT_LIST* scc,
 WN* loop,
 WN* result_loops[],
 WN* if_handles[],
 BOOL first_loop_empty=FALSE)
{
  WN* body=WN_do_body(loop);
  UINT total_loops= new_loops->Elements();
  UINT *loop_size=CXX_NEW_ARRAY(UINT,total_loops,&PHASE25_default_pool);

  UINT i = 0;
  if (first_loop_empty){
    loop_size[0] = 0;
    i = 1;
  }

  // Reorder the statements in the loop according to SCCs
  for (; i<total_loops; i++) {
    UINT seed_scc=(*new_loops)[i];
    UINT total_stmt=0;
    FF_STMT_ITER s_iter(&scc[seed_scc]);
    for (FF_STMT_NODE* stmt_node = s_iter.First(); !s_iter.Is_Empty();
      stmt_node=s_iter.Next()) {
      WN* stmt=stmt_node->Get_Stmt();
      LWN_Insert_Block_Before(body,NULL,LWN_Extract_From_Block(stmt));
      total_stmt++;
    }
    loop_size[i]=total_stmt;
  }

  // Separate the loop into multiple loops
  if (total_loops>1) {
    BOOL has_calls_or_gotos_or_inner_loops = FALSE;
    DO_LOOP_INFO* loop_info=Get_Do_Loop_Info(loop, FALSE);
    if (loop_info->Has_Calls || loop_info->Has_Gotos || !loop_info->Is_Inner) {
      has_calls_or_gotos_or_inner_loops = TRUE;
    }

    WN* tmp_loop1=loop;
    WN** wn_starts=CXX_NEW_ARRAY(WN*, total_loops, &PHASE25_default_pool);
    WN** wn_ends=CXX_NEW_ARRAY(WN*, total_loops, &PHASE25_default_pool);
    WN** wn_steps=CXX_NEW_ARRAY(WN*, total_loops, &PHASE25_default_pool);
    WN** new_loops=CXX_NEW_ARRAY(WN*, total_loops, &PHASE25_default_pool);

    wn_starts[0]=WN_kid0(WN_start(tmp_loop1));
    wn_ends[0]=WN_end(tmp_loop1);
    wn_steps[0]=WN_kid0(WN_step(tmp_loop1));
    new_loops[0]=loop;
    result_loops[0]=loop;
    WN* stmt=WN_first(body);

    for (i=0; i<total_loops-1; i++) {
  
      INT size=loop_size[i];

      for (INT j=0; j<size; j++)
        stmt=WN_next(stmt);

      WN* tmp_loop2;
      Separate(tmp_loop1, WN_prev(stmt), 1, &tmp_loop2, (i==0 && first_loop_empty));
      LWN_Parentize(tmp_loop2);
      DO_LOOP_INFO* new_loop_info =
        CXX_NEW(DO_LOOP_INFO(loop_info,&LNO_default_pool), &LNO_default_pool);
      Set_Do_Loop_Info(tmp_loop2,new_loop_info);
      if (has_calls_or_gotos_or_inner_loops) {
        // should check gotos and calls when they are allowed to be in
        // loops handled by phase 3
      }
      wn_starts[i+1]=WN_kid0(WN_start(tmp_loop2));
      wn_ends[i+1]=WN_end(tmp_loop2);
      wn_steps[i+1]=WN_kid0(WN_step(tmp_loop2));
      new_loops[i+1]=tmp_loop2;
      result_loops[i+1] = tmp_loop2;      
      
      // Move the IF_test part of the current loop to the first loop
      WN *do_body = WN_do_body(tmp_loop2);
      WN *if_stmt = WN_first(do_body);
      WN *then_body = WN_then(if_stmt);

      if (WN_prev(if_stmt)!=NULL) {
	stmt = WN_prev(if_stmt);
      } else
	stmt = WN_first(body);

      WN* ins_loc = WN_prev(if_stmt);
      LWN_Extract_From_Block(if_stmt);

      // move the then_body to the do_body
      LWN_Copy_Frequency(do_body,then_body);
      WN* kid;
      while ((kid = WN_first(then_body)) != NULL) {
	LWN_Insert_Block_After(do_body,ins_loc,LWN_Extract_From_Block(kid));
	ins_loc=kid;
      }
      
      stmt = ins_loc;

      // Move the if_stmt with empty body to the first loop
      LWN_Insert_Block_Before(WN_do_body(result_loops[0]), NULL, if_stmt);

      // Remember the location of the if statement.
      if_handles[i+1] = if_stmt;
      
      tmp_loop1=tmp_loop2;
    }

    Fission_DU_Update(Du_Mgr,red_manager,wn_starts,wn_ends,wn_steps,total_loops,new_loops,TRUE);

    adg->Fission_Dep_Update(new_loops[0],total_loops);
  } else 
    result_loops[0]=loop;

  if (LNO_Test_Dump)
    Print_Def_Use(LWN_Get_Parent(loop), stdout);

}

//
// Gather/Scatter scalar expansion. 
// 1. Allocate the space.
// 2. Copy the scalars into the expanded arrays in the first loop.
// 3. Rename scalar reference in the use_loop with expanded array.
//
// Borrowed from Scalar_Expand with simplification and changes. 
// It should be kept in sych with any future revision of Scalar_Expand.
//
static void
Gather_Scatter_Scalar_Expand(WN*                                loop,
			     WN*                                inc_ld,
			     WN* 				tile_loop, 
			     WN*                                use_loop,
			     const DYN_ARRAY<SCALAR_NODE*>&     symbol,
			     const DYN_ARRAY<SCALAR_NODE*>&     use_site,
			     WN*                                ins_before,
			     WN**                               alloc_loop,
			     WN**                               dealloc_loop
			     )

{
  FmtAssert(WN_opcode(loop) == OPC_DO_LOOP, ("loop must be an OPC_DO_LOOP"));
  FmtAssert(WN_opcode(use_loop) == OPC_DO_LOOP, ("use_loop must be an OPC_DO_LOOP"));

  UINT total_var = symbol.Elements();

  Is_True(total_var>0,("No scalar to expand \n"));
  if (total_var<=0) return;

  static INT unique_gs_id = 0;
  unique_gs_id = unique_gs_id + total_var;

  // array to store index expression for each variable
  WN** indxs = CXX_NEW_ARRAY(WN*, total_var, &PHASE25_default_pool);

  // array to store inc expression for each variable
  WN** incxs = CXX_NEW_ARRAY(WN*, total_var, &PHASE25_default_pool);

  if (LNO_Verbose){
    for (INT i=0; i<total_var; ++i){
      fprintf(stdout, "Gather/Scatter scalar expanding variable %s\n", 
	      symbol[i]->_scalar.Name());
      fprintf(TFile, "Gather/Scatter scalar expanding variable %s\n", 
	      symbol[i]->_scalar.Name());
    }
  }

  // step 1: how much space do we need, in bytes.  Put that in bsz.
  // also, while we are at it, store in bound the number of elements
  // in that dimension.  store in indxs the indexing expression.

  TYPE_ID wtype = symbol[0]->_scalar.Type;
  INT sz0 = (wtype == MTYPE_I8 || wtype == MTYPE_U8 ||
	     wtype == MTYPE_F8 || wtype == MTYPE_C4) ? 8 :
  (wtype == MTYPE_I4 || wtype == MTYPE_U4 ||
   wtype == MTYPE_F4) ? 4 :
#if defined(TARG_IA64) || defined(TARG_X8664)
  (wtype == MTYPE_F10) ? 16 :
#endif
  (wtype == MTYPE_FQ || wtype == MTYPE_C8) ? 16 :
#if defined(TARG_IA64) || defined(TARG_X8664)
  (wtype == MTYPE_C10 || wtype == MTYPE_CQ) ? 32 :
#else
  (wtype == MTYPE_CQ) ? 32 :
#endif
  (wtype == MTYPE_I2 || wtype == MTYPE_U2) ? 2 :
  (wtype == MTYPE_I1 || wtype == MTYPE_U1) ? 1 : 0 ;

  FmtAssert(sz0 > 0, ("Bad type in gather/scatter scalar expansion"));

  UINT sz = sz0 * total_var;

  TYPE_ID bsztype = Do_Wtype(loop);

  WN* bsz = LWN_Make_Icon(Promote_Type(bsztype), sz);

  OPCODE opmpy = OPCODE_make_op(OPR_MPY, Promote_Type(bsztype), MTYPE_V);

  WN* d = loop;
  TYPE_ID ty = WN_desc(WN_start(d));
  OPCODE opsubty = OPCODE_make_op(OPR_SUB, Promote_Type(ty), MTYPE_V);
  OPCODE opldty = OPCODE_make_op(OPR_LDID, Promote_Type(ty), ty);
  OPCODE opaddty = OPCODE_make_op(OPR_ADD, Promote_Type(ty), MTYPE_V);
  OPCODE opminty = OPCODE_make_op(OPR_MIN, Promote_Type(ty), MTYPE_V); 

  // each bounds expression is (ub-(lb-1))*total_var 
  // (tuned for constant folding)
  WN* bexp = NULL; 
  if (tile_loop == NULL) {
    WN* one = LWN_Make_Icon(Promote_Type(ty), 1);
    WN* lb = LWN_Copy_Tree(WN_kid0(WN_start(d)));
    LWN_Copy_Def_Use(WN_kid0(WN_start(d)), lb, Du_Mgr);
    Upper_Bound_Standardize(WN_end(loop));
    WN* ub = LWN_Copy_Tree(SNL_UBexp(WN_end(d)));
    LWN_Copy_Def_Use(SNL_UBexp(WN_end(d)), ub, Du_Mgr);
    bexp = LWN_CreateExp2(opsubty, ub, LWN_CreateExp2(opsubty, lb, one));  
  } else {
    WN* one = LWN_Make_Icon(Promote_Type(ty), 1);
    WN* lb = LWN_Copy_Tree(WN_kid0(WN_start(tile_loop)));
    LWN_Copy_Def_Use(WN_kid0(WN_start(tile_loop)), lb, Du_Mgr);
    Upper_Bound_Standardize(WN_end(tile_loop));
    WN* ub = LWN_Copy_Tree(SNL_UBexp(WN_end(tile_loop)));
    LWN_Copy_Def_Use(SNL_UBexp(WN_end(tile_loop)), ub, Du_Mgr);
    WN* sb1exp = LWN_CreateExp2(opsubty, ub, LWN_CreateExp2(opsubty, lb, one)); 
    WN* sb2exp = LWN_Make_Icon(Promote_Type(bsztype), (INT) Step_Size(tile_loop, 0));
    bexp = LWN_CreateExp2(opminty, sb1exp, sb2exp);  
  }
  WN* bound = LWN_Copy_Tree(bexp);
  LWN_Copy_Def_Use(bexp, bound, Du_Mgr);

  // each index expression is ind_var+i
  UINT i;
  for (i=0; i<total_var; ++i){
    WN* indx_ldid = LWN_CreateLdid(opldty, WN_step(use_loop));
    SNL_Add_Du_To_Index_Ldid(use_loop, indx_ldid, Du_Mgr, TRUE);
    WN* num_var = LWN_Make_Icon(Promote_Type(bsztype), total_var);

    WN* incx_ldid = LWN_Copy_Tree(inc_ld);
    LWN_Copy_Def_Use(inc_ld,incx_ldid,Du_Mgr);

    WN* indx_con = LWN_Make_Icon(Promote_Type(bsztype), i);
    indxs[i] = LWN_CreateExp2(opaddty,
			      LWN_CreateExp2(opmpy,
					     num_var,
					     indx_ldid),
			      indx_con);

    num_var = LWN_Make_Icon(Promote_Type(bsztype), total_var);
    indx_con = LWN_Make_Icon(Promote_Type(bsztype), i);
    incxs[i] = LWN_CreateExp2(opaddty,
			      LWN_CreateExp2(opmpy,
					     num_var,
					     incx_ldid),
			      indx_con);
  }

  bsz = LWN_CreateExp2(opmpy, bsz,  LWN_Int_Type_Conversion(bexp, bsztype));
  bsz = LWN_Int_Type_Conversion(bsz, Pointer_type);

  // step two:  Get the space.
  char          gs[3]="gs";

  SYMBOL        gs_ptrsym;
  SE_Symbols_For_SE(&gs_ptrsym, gs, unique_gs_id, wtype);
  Update_MP_Local_Var(gs_ptrsym.St(), 0, LWN_Get_Parent(*alloc_loop));
  WN*           newstdf = 
    Get_Expansion_Space(gs_ptrsym,bsz,gs,unique_gs_id,wtype,
			*alloc_loop,use_loop,*dealloc_loop);


  if (!LNO_Use_Malloc) {  // insure that pushes and pops to stack come
			  // in stack order
    // push alloc loop backwards so that next call will come before this one
    *alloc_loop = newstdf;
    while (WN_operator(*alloc_loop) != OPR_INTRINSIC_CALL ||
           ((WN_intrinsic(*alloc_loop) != INTRN_U8READSTACKPOINTER) &&
	    (WN_intrinsic(*alloc_loop) != INTRN_U4READSTACKPOINTER))) {
      *alloc_loop = WN_prev(*alloc_loop);
    }
    // push dealloc loop forwards so that next call will come after this one
    while (WN_operator(*dealloc_loop) != OPR_INTRINSIC_CALL ||
	 ((WN_intrinsic(*dealloc_loop) != INTRN_U8I8SETSTACKPOINTER) &&
	  (WN_intrinsic(*dealloc_loop) != INTRN_U4I4SETSTACKPOINTER))) {
      *dealloc_loop = WN_next(*dealloc_loop);
    }
  }
  Is_True(newstdf,("No memory was allocated for scalar expansion \n"));

  OPCODE        ldop = OPCODE_make_op(OPR_LDID, Pointer_type, Pointer_type);

  // we've got a pointer name: newstdf. We now can replace every reference
  // to the st/offset of symbol within the region.  Every reference is
  // replaced by the expression in index, where our array statement tells
  // us the appropriate bounds.
  // Or a simpler way is to create LOAD statements at the beginning of
  // use_loop.  But if the original loop index is lived, recovering
  // the value of loop index is a pain.

  // Note that when the walk is done, in theory all DO annotations for
  // this expanded variable are gone.

  // TODO OK: make more efficient by using SSA graph, so we don't have to
  // walk the whole region.  But the lexcount's important ... .  Perhaps
  // can use similar information from SNL code.

  MEM_POOL_Push(&PHASE25_default_pool);
  {
    DYN_ARRAY<WN_REFERENCE_INFO> deflist(&PHASE25_default_pool);
    DYN_ARRAY<WN_REFERENCE_INFO> uselist(&PHASE25_default_pool);

    INT lexcount = 0;

    //-----------------------added for memorization---------------
    // Let's first insert the memorization code to the end of loop.
    // 'X(total_var*inc+k) = ID(symbol(k))'
    //------------------------------------------------------------

    // A place to hold alias info
    WN * alias_host = NULL;
    INT each_var;
    for (each_var=0; each_var<total_var; ++each_var){

      /* make the array statement */
      OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
      WN* wn_array = WN_Create(op_array, 1+1+1);

      WN_element_size(wn_array) = sz0;
      WN_array_base(wn_array) = WN_CreateLdid(ldop, gs_ptrsym.WN_Offset(),
					      gs_ptrsym.St(),
					      ST_type(gs_ptrsym.St()));

//      Create_alias(Alias_Mgr,WN_array_base(wn_array));
      LWN_Set_Parent(WN_array_base(wn_array), wn_array);
      WN_array_index(wn_array,0) = LWN_Copy_Tree(incxs[each_var]);
      WN_array_dim(wn_array,0) = LWN_Copy_Tree(bound);
      LWN_Set_Parent(WN_array_index(wn_array,0), wn_array);
      LWN_Set_Parent(WN_array_dim(wn_array,0), wn_array);
      LWN_Copy_Def_Use(incxs[each_var], WN_array_index(wn_array,0),Du_Mgr);
      LWN_Copy_Def_Use(bound, WN_array_dim(wn_array,0),Du_Mgr);

      WN* one_use = use_site[each_var]->Bottom_nth(0)->Wn;
      Is_True(one_use, ("Cannot find the use node of an expanded variable"));

      DEF_LIST * defs = Du_Mgr->Ud_Get_Def(one_use);
      WN *loop_stmt = (defs) ? defs->Loop_stmt() : NULL;

      if (SYMBOL(one_use) == SYMBOL(WN_start(loop))){
	loop_stmt = loop;
      }

      // Make an assignment from a scalar to the expanded array.
      OPCODE loadop = OPCODE_make_op(OPR_LDID, Promote_Type(wtype), wtype);
      WN* rhs = LWN_CreateLdid(loadop,one_use);

      // Take care of the DU chains
      for (INT each_def=0; each_def<symbol[each_var]->Elements(); ++each_def){
	WN* cur_def = symbol[each_var]->Bottom_nth(each_def)->Wn;
	Is_True(cur_def,("No definition site for an expanded variable \n"));
	Du_Mgr->Add_Def_Use(cur_def,rhs);
	Du_Mgr->Ud_Get_Def(rhs)->Set_loop_stmt(loop_stmt);
      }

      OPCODE storeop = OPCODE_make_op(OPR_ISTORE, MTYPE_V, wtype);
      TY_IDX pty = Make_Pointer_Type(Be_Type_Tbl(wtype));
      FmtAssert(pty, ("null ty pointer for wtype=%d", wtype));
      WN* store = LWN_CreateIstore(storeop, 0, pty, rhs, wn_array);

      // Get the alias id right
      if (alias_host==NULL) {
	Create_unique_pointer_alias(Alias_Mgr, gs_ptrsym.St(), 
				WN_array_base(wn_array), store);
	alias_host = store;
	Copy_alias_info(Alias_Mgr, WN_array_base(wn_array), newstdf);
      } else {
	Copy_alias_info(Alias_Mgr, newstdf, WN_array_base(wn_array));
	Copy_alias_info(Alias_Mgr, alias_host, store);
      }

      Du_Mgr->Add_Def_Use(newstdf, WN_array_base(wn_array));	  
//      WN_MAP_Set(LNO_Info_Map, WN_array_base(wn_array), 
//		 (void*) (unique_gs_id-total_var+each_var+1));

      LWN_Copy_Frequency_Tree(store, ins_before);

      LWN_Insert_Block_Before(WN_do_body(loop), ins_before, store);

      LWN_Copy_Linenumber(use_loop,store);
      INT idx = deflist.Newidx();
      deflist[idx].wn = LWN_Get_Parent(wn_array);
      deflist[idx].lexcount = lexcount++;

      // make an access vector for the wn_array and put it in the table,
      // and make a vertex for the graph.

      DOLOOP_STACK do_stack(&PHASE25_default_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_array), &do_stack);
      LNO_Build_Access(wn_array, &do_stack, &LNO_default_pool);
      VINDEX16 new_sv = adg->Add_Vertex(LWN_Get_Parent(wn_array));
      if (new_sv == 0) 
	LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_array), adg); 
    }

    //------------------------------------------------------------
    // And insert restoration code to the start of the use loop
    // ID(symbol(k)) = X(total_var*ind+k)
    //------------------------------------------------------------
    for (each_var=total_var-1; each_var>=0; --each_var){

      /* make the array statement */
      OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
      WN* wn_array = WN_Create(op_array, 1+1+1);
      WN_element_size(wn_array) = sz0;
      WN_array_base(wn_array) = WN_CreateLdid(ldop, gs_ptrsym.WN_Offset(),
					      gs_ptrsym.St(),
					      ST_type(gs_ptrsym.St()));

      Copy_alias_info(Alias_Mgr, newstdf, WN_array_base(wn_array));
      Du_Mgr->Add_Def_Use(newstdf, WN_array_base(wn_array));
      LWN_Set_Parent(WN_array_base(wn_array), wn_array);

      WN_array_index(wn_array,0) = LWN_Copy_Tree(indxs[each_var]);
      WN_array_dim(wn_array,0) = LWN_Copy_Tree(bound);

      LWN_Set_Parent(WN_array_index(wn_array,0), wn_array);
      LWN_Set_Parent(WN_array_dim(wn_array,0), wn_array);
      LWN_Copy_Def_Use(indxs[each_var], WN_array_index(wn_array,0),Du_Mgr);
      LWN_Copy_Def_Use(bound, WN_array_dim(wn_array,0),Du_Mgr);

      OPCODE loadop = OPCODE_make_op(OPR_ILOAD, Promote_Type(wtype), wtype);
      TY_IDX wty = Be_Type_Tbl(wtype);
      TY_IDX pty = Make_Pointer_Type(Be_Type_Tbl(wtype));
      WN* load = LWN_CreateIload(loadop, 0, wty, pty, wn_array);

      Is_True(alias_host!=NULL,("No alias info is available\n"));
      Copy_alias_info(Alias_Mgr, alias_host, load);

//      WN_MAP_Set(LNO_Info_Map, WN_array_base(wn_array),
//		 (void*) (unique_gs_id-total_var+each_var+1));


      // Make an assignment from the expanded array to the scalar.
      WN* one_use = use_site[each_var]->Bottom_nth(0)->Wn;
      Is_True(one_use, ("Cannot find the use node of an expanded variable"));
      OPCODE stop = OPCODE_make_op(OPR_STID, MTYPE_V, wtype);
      WN* store = LWN_CreateStid(stop,one_use,load);
      
      LWN_Insert_Block_After(WN_do_body(use_loop), NULL, store);
      LWN_Copy_Linenumber(use_loop,store);

      LWN_Copy_Frequency_Tree(store, WN_do_body(use_loop));

      // Let's rename the new scalar variable
      SYMBOL old_sym(store);
      char new_name[64];
      if (strlen(old_sym.Name())<48) 
	sprintf(new_name, "%s_gs_rn_%d", old_sym.Name(), index_counter++);
      else
	sprintf(new_name, "%s_gs_rn_%d", "name_too_long", index_counter++);

      SYMBOL new_sym = Create_Preg_Symbol(new_name, wtype);

      // Take care of the DU chains
      for (INT each_use=0; each_use<use_site[each_var]->Elements(); ++each_use){
	WN* cur_use = use_site[each_var]->Bottom_nth(each_use)->Wn;

	// First remove all the def-use chains from outside of the use_loop
	DEF_LIST * def_list = Du_Mgr->Ud_Get_Def(cur_use);

	def_list->Set_loop_stmt(NULL);

	DEF_LIST_ITER iter_def(def_list);
	for (DU_NODE* def_node=iter_def.First(); !iter_def.Is_Empty(); ){
	  WN* def = def_node->Wn();
	  def_node= (DU_NODE *) iter_def.Next();

	  WN* stmt_loc = Find_Stmt_Under(def,WN_do_body(use_loop));
	  if (stmt_loc == NULL){
	    Du_Mgr->Delete_Def_Use(def,cur_use);
	  }
	}

	// Add the current store to the def-use chain
	Du_Mgr->Add_Def_Use(store,cur_use);

	// It is safe to erase the reduction because otherwise the loop
	// cannot be fissed.
	if (red_manager && red_manager->Which_Reduction(cur_use) != RED_NONE)
	  red_manager->Erase(cur_use);

	Replace_Symbol(cur_use,old_sym,new_sym,NULL);
	Create_alias(Alias_Mgr,cur_use);

      }

      Replace_Symbol(store,old_sym,new_sym,NULL);
      Create_alias(Alias_Mgr,store);

      INT idx = uselist.Newidx();
      uselist[idx].wn = LWN_Get_Parent(wn_array);
      uselist[idx].lexcount = lexcount++;

      // make an access vector for the wn_array and put it in the table,
      // and make a vertex for the graph.

      DOLOOP_STACK do_stack(&PHASE25_default_pool);
      Build_Doloop_Stack(LWN_Get_Parent(wn_array), &do_stack);
      LNO_Build_Access(wn_array, &do_stack, &LNO_default_pool);
      VINDEX16 new_sv = adg->Add_Vertex(LWN_Get_Parent(wn_array));
      if (!new_sv) 
        LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(wn_array), adg); 
    }

    // Build dependence graph for expanded scalar
    SE_Fix_Dependence(deflist,uselist);

    // Rebuild the access arrays for the use loop
    DOLOOP_STACK do_stack(&PHASE25_default_pool);
    Build_Doloop_Stack(LWN_Get_Parent(use_loop), &do_stack);
    LNO_Build_Access(use_loop, &do_stack, &LNO_default_pool);

    for (i = 0; i < total_var; i++) {
      LWN_Delete_Tree(indxs[i]);
      LWN_Delete_Tree(incxs[i]);
    }

    LWN_Delete_Tree(bound);

    CXX_DELETE_ARRAY(indxs, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(incxs, &PHASE25_default_pool);
  }

  MEM_POOL_Pop(&PHASE25_default_pool);
}

// Add all scalar accessed in the tree 'wn' to the appropriate stack.
extern INT64 Gather_Scalar_References(WN *wn,
				     STACK<WN*> *writes,
				     STACK<WN*> *reads)
{
  
  WN *kid;

  // if encounters a block whirl node
  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first(wn);
    while (kid) {
      Gather_Scalar_References(kid,writes,reads);
      kid = WN_next(kid);
    }
    return access_counter;
  } 
  
  if (OPCODE_is_load(WN_opcode(wn)) ||
      OPCODE_is_store(WN_opcode(wn)))
    access_counter++;
  
  if (WN_operator(wn)==OPR_LDID && reads!=NULL)
    reads->Push(wn);
  else if (WN_operator(wn)==OPR_STID && writes!=NULL)
    writes->Push(wn);
  else {
    // for other kinds of whirl node, search recursively
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      kid = WN_kid(wn,kidno);
      Gather_Scalar_References(kid,writes,reads);
    }
  }

  return access_counter;
}

// Good for gather if LNO_Gather_Scatter==2 or no nested ifs
static BOOL Good_for_gath(WN* if_stmt)
{
  Is_True(WN_opcode(if_stmt) == OPC_IF, ("stmt must be an OPC_IF \n"));
  if (LNO_Gather_Scatter==2) return TRUE;

  LWN_ITER* wniter = LWN_WALK_TreeIter(WN_then(if_stmt));
  while (wniter){
    WN *ref = wniter->wn;
    wniter = LWN_WALK_TreeNext(wniter);
    if (WN_opcode(ref) == OPC_IF) {
      LWN_WALK_Abort(wniter);
      return FALSE;
    }
  }
  return TRUE;
}

// Determine if an if_test can be moved to an early loop
static BOOL movable_if_test(WN* if_stmt, WN* loop)
{
  Is_True(WN_opcode(if_stmt) == OPC_IF, ("stmt must be a OPC_IF \n"));

  LWN_ITER* wniter = LWN_WALK_TreeIter(WN_if_test(if_stmt));
  while (wniter){
    WN *ref = wniter->wn;
    wniter = LWN_WALK_TreeNext(wniter);
    
    if (OPCODE_is_load(WN_opcode(ref))){
      OPERATOR opr = WN_operator(ref);

      if (opr==OPR_LDID){
	// Determine exposed scalar uses
	DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(ref);
	DEF_LIST_ITER iter_def(def_list);
	
	for (DU_NODE* def_node=iter_def.First(); !iter_def.Is_Empty();
	     def_node=(DU_NODE *) iter_def.Next()){
      
	  // For each definition,  determines if it belongs the same loop
	  WN* def = def_node->Wn();
	  Is_True(def,("Null pointer in def-use chain \n"));
      
	  WN* stmt_block = Find_Stmt_Under(def,if_stmt);
      
	  if (stmt_block != NULL) {
	    if (wniter)      // if, because wniter has advanced.
	      LWN_WALK_Abort(wniter);
	    return FALSE;
	  }
	}

      } else if ((WN_kid_count(ref)==1) &&
		 (WN_offset(ref) == 0) &&
		 (WN_operator(WN_kid0(ref)) == OPR_ARRAY)){

	VINDEX16 array_v=adg->Get_Vertex(ref);
	Is_True(array_v!=0, ("Array reference on in dependence graph \n"));
      
	EINDEX16 in_edge=adg->Get_In_Edge(array_v);
	while (in_edge){
	  if (adg->Depv_Array(in_edge)->Max_Level() >= Do_Loop_Depth(loop)){
	      WN* src_ref=adg->Get_Wn(adg->Get_Source(in_edge));
	      WN* stmt_block = Find_Stmt_Under(src_ref,if_stmt);
	      if (stmt_block != NULL) return FALSE;
	    }
	  in_edge = adg->Get_Next_In_Edge(in_edge);
	}

      } else {
	if (wniter)      // if, because wniter has advanced.
          LWN_WALK_Abort(wniter);
	return FALSE;
      }

    }

  }

  return TRUE;

}

// Count the number of floating point operations
INT64 Get_FP_Counts(WN* wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_leaf(opcode))
    return 0;
  else if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    INT64 count = 0;
    while (kid) {
      count++; 
      count += Get_FP_Counts(kid);
      kid = WN_next(kid);
    }
    return count;
  }

  OPERATOR oper = OPCODE_operator(opcode);
  
  INT64 count = 0;
  if ((oper == OPR_TRUNC) || (oper == OPR_RND) ||
      (oper == OPR_CEIL) || (oper == OPR_FLOOR) || (oper == OPR_INTRINSIC_OP)) {
    count++;
  } else if ((oper == OPR_REALPART) || (oper == OPR_IMAGPART) ||
	     (oper == OPR_PARM) || (oper == OPR_PAREN)) {
    // no-ops
  } else if (OPCODE_is_expression(opcode) && !OPCODE_is_load(opcode) &&
	     (oper != OPR_CONST)) {
    // an fp expression
    if ((OPCODE_desc(opcode)==MTYPE_FQ) || (OPCODE_rtype(opcode)==MTYPE_FQ) ||
	(OPCODE_desc(opcode)==MTYPE_CQ) || (OPCODE_rtype(opcode)==MTYPE_CQ) ||
	(OPCODE_desc(opcode)==MTYPE_F4) || (OPCODE_desc(opcode)==MTYPE_F8) ||
	(OPCODE_rtype(opcode)==MTYPE_F4)|| 
	(OPCODE_rtype(opcode)==MTYPE_F8)|| (OPCODE_desc(opcode)==MTYPE_C4) || 
	(OPCODE_desc(opcode)==MTYPE_C8) || (OPCODE_rtype(opcode)==MTYPE_C4)||
#if defined(TARG_IA64) || defined(TARG_X8664)
	OPCODE_desc(opcode) == MTYPE_F10 || OPCODE_rtype(opcode) == MTYPE_F10 ||
	OPCODE_desc(opcode) == MTYPE_C10 || OPCODE_rtype(opcode) == MTYPE_C10 ||
#endif
	(OPCODE_rtype(opcode)==MTYPE_C8))  {
      
      if ((oper == OPR_MAX) || (oper == OPR_MIN) || 
	  (oper == OPR_ADD) || (oper == OPR_SUB) || (oper == OPR_MPY) ||
	  (oper == OPR_NEG))
        count++;
      else if ((oper == OPR_DIV || oper == OPR_SQRT))
	count = count + 10;
    }
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    count += Get_FP_Counts(kid);
  }

  return count;
  
}

// ====================================================================
// Identify leaf level conditional segments among the SCCs and
// perform gather/scatter fission.
// ====================================================================
extern void
Perform_Gather_Scatter(
        SCC_DIRECTED_GRAPH16 *dep_g_p,  // dependence graph
        UINT		total_scc,	// total number of SCCs
        FF_STMT_LIST*      scc,            // list of statements for SCCs
	WN*		loop,		// loop enclosing the SCCs
	SCC_DIRECTED_GRAPH16 *scc_dep_g, // SCC dependence graph
        WN2VINDEX*       stmt_to_vertex) // map stmt to vertex in dpg
{
  // create an array to hold conditional SCCs
  // because we will usually have only one conditional SCC
  // the dynamic array won't grow too many times
  UINT_DYN_ARRAY *cond_scc = CXX_NEW(UINT_DYN_ARRAY(&PHASE25_default_pool),
				     &PHASE25_default_pool);

  UINT_DYN_ARRAY *cond_scc1 = CXX_NEW(UINT_DYN_ARRAY(&PHASE25_default_pool),
				     &PHASE25_default_pool);

  // The array of SCCs to hold other SCCs
  UINT_DYN_ARRAY *other_scc=CXX_NEW(UINT_DYN_ARRAY(&PHASE25_default_pool),
				    &PHASE25_default_pool);

  // The array of SCCs to hold new SCCs
  UINT_DYN_ARRAY *new_scc=CXX_NEW(UINT_DYN_ARRAY(&PHASE25_default_pool),
				  &PHASE25_default_pool);

  VINDEX16 *queue=CXX_NEW_ARRAY(VINDEX16,total_scc,&PHASE25_default_pool);
  scc_dep_g->Level_Sort(queue);

  // In this first version, we are only interested in the leaf level 
  // conditional codes.  It simplifies the problem quite a bit
  //
  // TODO: handle non-leaf level conditional codes
  //
  UINT i;
  for (i=0; i<total_scc; ++i) {
    UINT cur = queue[i];
    FF_STMT_LIST* scc_cur = &scc[cur];

    FF_STMT_NODE* node = scc_cur->Head();
    if (scc_dep_g->Get_Out_Edge(cur) == 0 && node->Next() == NULL){ 
      // found a single leaf node
      WN* stmt = node->Get_Stmt();

      if (WN_opcode(stmt)==OPC_IF && WN_else_is_empty(stmt) 
	  && Good_for_gath(stmt))
	// and, it is a conditional statement,
        // for now, we only handle unbalanced branch statement
        //
        // TODO: handle conditional codes with both branchs
        // as long as no cycle exists between its THEN and
        // ELSE parts
        
	cond_scc->AddElement(cur);
      else 
	other_scc->AddElement(cur);
    } else
      other_scc->AddElement(cur);
  }

  // Return if no conditional loop was found.
  if (!cond_scc->Elements()){
    CXX_DELETE(cond_scc, &PHASE25_default_pool);
    CXX_DELETE(new_scc, &PHASE25_default_pool);
    CXX_DELETE(other_scc, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(queue, &PHASE25_default_pool);
    return;
  }

  // ----------------------------------------------------------------------
  // Now find out the exposed uses of variables in each of the 
  // conditional SCCs.  
  //   1. The scalar variables will have to be expanded (memorized).
  //   2. The loop index variable will also have to be expanded (memorized),
  //      if it occurs in any subscript in the SSC.
  //   3. The arrays used in the conditional SCCs don't need to be memorizied
  //      in the definition SCCs.  There shouldn't be any anti-dependences
  //      among the definition SCCs and the use SCCs.  Otherwise, they will
  //      belong to the same SCCs because our dependence analysis does look
  //      for expandable arrays.
  //   4. We cannot use the statement level scalar dependence graph to find
  //      out the exposed-use, it doesn't contain enough information.  So we
  //      do the analysis ourselve.
  // ------------------------------------------------------------------------

  // An array of stacks for exposed-uses, one for each conditional scc
  SCALAR_STACK** exposed_use = CXX_NEW_ARRAY(SCALAR_STACK*,
					     cond_scc->Elements(),
					     &PHASE25_default_pool);
  SCALAR_STACK** exposed_site = CXX_NEW_ARRAY(SCALAR_STACK*,
					     cond_scc->Elements(),
					     &PHASE25_default_pool);

  // A stack of all uses
  STACK<WN*> all_use(&PHASE25_default_pool);

  INT64 *size_est = CXX_NEW_ARRAY(INT64,cond_scc->Elements(),&PHASE25_default_pool);

  UINT ind_new_scc=0;
  for (i=0; i<cond_scc->Elements(); ++i) {
    WN* if_stmt = scc[(*cond_scc)[i]].Head()->Get_Stmt();

    exposed_use[i] = CXX_NEW(SCALAR_STACK(&PHASE25_default_pool),
			     &PHASE25_default_pool);
    exposed_site[i] = CXX_NEW(SCALAR_STACK(&PHASE25_default_pool),
			     &PHASE25_default_pool);

    // test if the if_test can be moved to loop_first
    if (!movable_if_test(if_stmt,loop)){
      other_scc->AddElement((*cond_scc)[i]);
      continue;
    }

    all_use.Clear();
    WN* if_body = WN_then(if_stmt);

    // First, let's gather all the scalar uses
    access_counter=0;
    Gather_Scalar_References(if_body,NULL,&all_use);

    size_est[ind_new_scc] = Get_FP_Counts(if_body);
    if (LNO_Verbose) 
      fprintf(stdout, "Size estimate is %lld\n", size_est[ind_new_scc]);

    // Determine exposed scalar uses
    for (UINT j=0; j<all_use.Elements(); ++j){
      WN* ref = all_use.Bottom_nth(j);
      BOOL use_stored=FALSE;
      
      DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(ref);
      if (!def_list || def_list->Incomplete()) continue;

      DEF_LIST_ITER iter_def(def_list);
      for (DU_NODE* def_node=iter_def.First(); !iter_def.Is_Empty();
	   def_node=(DU_NODE *) iter_def.Next()){

	// For each definition,  determines if it belongs to
        // other SCCs inside the same loop.
	WN* def = def_node->Wn();
	Is_True(def,("Null pointer in def-use chain \n"));

	WN* stmt_block = Find_Stmt_Under(def,WN_do_body(loop));

	// For a definition inside the same loop, determine
	// its SSC number.  If it belongs to a different SCC,
	// store the variable symbol.
	if (stmt_block != NULL){
	  // assume the SCC id is not changed
	  VINDEX16 scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt_block));
	  if (scc_id != (*cond_scc)[i]){
	    // first test if it is scalar expandable,
	    // if not, then it does not need to be scalar expanded.
	    // (This only happens with two in the same loop)
	    if (Scalar_Expandable(ref, loop, Du_Mgr) != SE_NONE) {

	      // enlist the variable if it is not already enlisted
	      exposed_use[ind_new_scc]->Add_Scalar(def,0);

	      if (!use_stored){
		exposed_site[ind_new_scc]->Add_Scalar(ref,0);
		use_stored = TRUE;
	      }
	    }
	  }
	} else if (def == WN_start(loop) || stmt_block == WN_step(loop)){
	  // loop index is used
	  exposed_use[ind_new_scc]->Add_Scalar(def,0);
	  if (!use_stored){
	    exposed_site[ind_new_scc]->Add_Scalar(ref,0);
	    use_stored = TRUE;
	  }
	}
      } // end for ***definition***
    } // end for ***all use***

    IF_INFO *ii = Get_If_Info(if_stmt, TRUE);
    if (size_est[ind_new_scc] > 3 + 
	2 * exposed_use[ind_new_scc]->Elements() ||
	(ii!=NULL && ii->Freq_True>=0.0 && 
	 (1-ii->Freq_True)*size_est[ind_new_scc] > 
	 3 + 2 * exposed_use[ind_new_scc]->Elements())) {
      cond_scc1->AddElement((*cond_scc)[i]);
      ind_new_scc++;
    } else {
      exposed_site[ind_new_scc]->Clear();      
      exposed_use[ind_new_scc]->Clear();
      other_scc->AddElement((*cond_scc)[i]);
    }

  } // end for ***all cond_scc***

  // Return if no worthy conditional loop was found.
  if (!cond_scc1->Elements()){
    CXX_DELETE(cond_scc, &PHASE25_default_pool);
    CXX_DELETE(cond_scc1, &PHASE25_default_pool);
    CXX_DELETE(new_scc, &PHASE25_default_pool);
    CXX_DELETE(other_scc, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(size_est, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(queue, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(exposed_use, &PHASE25_default_pool);
    CXX_DELETE_ARRAY(exposed_site, &PHASE25_default_pool);
    return;
  }

  if (LNO_Verbose){
    fprintf(stdout, "Gather/Scatter create %d new loops\n", 
	    cond_scc1->Elements());
    fprintf(TFile, "Gather/Scatter create %d new loops\n", 
	    cond_scc1->Elements());
  }

  // Introduce a tile loop, if necessary, to limit the size of dynamically
  // created arrays. 
  WN* tile_loop = NULL; 
  if (!Get_Trace(TP_LNOPT, TT_LNO_BIG_SCALAR_TILES)) { 
    tile_loop = SE_Tile_Inner_Loop(loop, &LNO_default_pool); 
  } 

  // Merge all the non-conditional loops and the
  // no-worthy conditional loops into a single SCC
  // Notice if there are no non-conditional loops, we
  // still want to split the loop.
  INT big_o = -1;
  if (other_scc->Elements()){
    big_o = (*other_scc)[0];
    new_scc->AddElement(big_o);
  
    for (i=1; i<other_scc->Elements(); ++i){
      // merge the cur into big_o
      UINT cur = (*other_scc)[i];
      scc[big_o].Append_List(&scc[cur]);
    }
  } else
    new_scc->AddElement(0);


  for (i=0; i<cond_scc1->Elements(); ++i){
    new_scc->AddElement((*cond_scc1)[i]);
  }
  
  // Now, we have all the SCCs.  Let's split the loop.
  INT num_loop = new_scc->Elements();
  WN** result_loops = CXX_NEW_ARRAY(WN*,num_loop,&PHASE25_default_pool);
  WN** if_handles = CXX_NEW_ARRAY(WN*,num_loop,&PHASE25_default_pool);
  separate_loop_by_scc(new_scc, scc, loop, result_loops, if_handles, (big_o<0));

  // Generate memorization code in the first loop and the conditional SCCs
  //    a. Create a new variable as an index to the expanded arrays
  //    b. Scalar expand the memorized variables and rename the uses in
  //       the current conditional SCC, and add the memorization code
  //       in the first loop.
  //    d. Move the conditional increment into the first loop

  // change the index variable of the new loops to new variables
  // and adjust the loop bounds.


  WN* loop_first = result_loops[0];
  TYPE_ID ind_type = Do_Wtype(loop_first);
  OPCODE stop = OPCODE_make_op(OPR_STID, MTYPE_V, ind_type);
  OPCODE ldop = OPCODE_make_op(OPR_LDID, Promote_Type(ind_type), ind_type);
  OPCODE addop = OPCODE_make_op(OPR_ADD, Promote_Type(ind_type), MTYPE_V);
  OPCODE ltop = OPCODE_make_op(OPR_LT, Boolean_type, Promote_Type(ind_type));
  WN* alloc_loop = tile_loop == NULL ? loop_first : tile_loop; 

  for (i=1; i<num_loop; ++i){
    WN*  loop_current = result_loops[i];
    WN* dealloc_loop = tile_loop == NULL ? loop_current : tile_loop;

    // create a new 'ind' variable
    char newinc_name[64];

    // Incremental variable for each new loop
    sprintf(newinc_name, "$inc_%d", index_counter);
    SYMBOL new_inc = Create_Preg_Symbol(newinc_name, ind_type);
    
    // insert the initial assignment 'inc=0' before the first loop.
    WN* cons_0 = LWN_Make_Icon(Promote_Type(ind_type),0);
    WN* store_init = LWN_CreateStid(stop,
				    new_inc.WN_Offset(),
				    new_inc.St(),
				    Be_Type_Tbl(ind_type),
				    cons_0);
    LWN_Copy_Linenumber(loop_current,store_init);
    LWN_Insert_Block_Before(LWN_Get_Parent(loop_first), loop_first, store_init);
    
    // Generate a conditional increment at the end of the first loop,
    // and remove the IF statement in the current loop
    WN *if_stmt = if_handles[i];
    WN *then_body = WN_then(if_stmt);

    // Insert the increment into the then_body
    // create a 'new_inc = new_inc + 1'.
    WN* ld_inc = LWN_CreateLdid(ldop, store_init);
    WN* cons_1 = LWN_Make_Icon(Promote_Type(ind_type), 1);
    WN* step_expr = LWN_CreateExp2(addop, ld_inc, cons_1);
    WN* new_store = LWN_CreateStid(stop,
				   new_inc.WN_Offset(),
				   new_inc.St(),
				   Be_Type_Tbl(ind_type),
				   step_expr);
    Du_Mgr->Add_Def_Use(store_init,ld_inc);
    Du_Mgr->Add_Def_Use(new_store,ld_inc);
    
    LWN_Copy_Frequency_Tree(new_store,then_body);
    LWN_Copy_Linenumber(then_body,new_store);
    
    // Insert the increment to the if statement
    LWN_Insert_Block_Before(then_body, NULL, new_store);

    // Now the conditional part is done, let work on the
    // new loop control information.
    // Index variable for each new loop
    char newind_name[64];
    sprintf(newind_name, "$ind_%d", index_counter++);
    SYMBOL new_ind = Create_Preg_Symbol(newind_name, ind_type);

    // create a 'new_ind' and replace the loop 'index'
    SYMBOL old_symbol(WN_index(loop_current));
    Replace_Symbol(WN_index(loop_current),old_symbol,new_ind,NULL);
    
    // create a 'new_ind = 0' and replace the loop 'start'
    WN* wn_start = WN_start(loop_current);
    Replace_Symbol(wn_start,old_symbol,new_ind,NULL);
    cons_0 = LWN_Copy_Tree(cons_0);
    WN* old_value = WN_kid0(wn_start);
    WN_kid0(wn_start) = cons_0;
    LWN_Set_Parent(cons_0,wn_start);
    LWN_Copy_Frequency(cons_0,old_value);

    LWN_Delete_Tree(old_value);
    
    // create a 'new_ind = new_id + 1' and replace the loop 'step'
    WN* wn_step = WN_step(loop_current);


    Replace_Symbol(wn_step,old_symbol,new_ind,NULL);
    cons_1 = LWN_Copy_Tree(cons_1);
    step_expr = WN_kid0(wn_step);
    WN* old_inc = WN_kid1(step_expr);
    WN_kid1(step_expr) = cons_1;
    LWN_Set_Parent(cons_1,step_expr);
    LWN_Delete_Tree(old_inc);
    LWN_Copy_Frequency_Tree(wn_step, then_body);

    // create a 'new_ind < new_inc' and replace the loop 'end'
    WN* ld_ind = LWN_CreateLdid(ldop, WN_start(loop_current));
    WN* ld_inc_copy = LWN_Copy_Tree(ld_inc);
    LWN_Copy_Def_Use(ld_inc,ld_inc_copy,Du_Mgr);

    WN* new_end = LWN_CreateExp2(ltop,ld_ind,ld_inc_copy);
    WN* wn_end = WN_end(loop_current);
    WN_end(loop_current) = new_end;
    LWN_Set_Parent(new_end,loop_current);
    LWN_Delete_Tree(wn_end);
    LWN_Copy_Frequency_Tree(new_end, then_body);

    Du_Mgr->Add_Def_Use(wn_start,ld_ind);
    Du_Mgr->Add_Def_Use(wn_step,ld_ind);
    Du_Mgr->Ud_Get_Def(ld_ind)->Set_loop_stmt(loop_current);


    // Rebuid the Access vector for the new loop.
    DOLOOP_STACK do_stack(&PHASE25_default_pool);
    Build_Doloop_Stack(LWN_Get_Parent(loop_current), &do_stack);
    LNO_Build_Do_Access(loop_current, &do_stack);

    // Now, the new loop header is fixed, we will
    //   1. Allocate the arrays for scalar memorization.
    //   2. Memorize the scalars in the first loop.
    //   3. Substitute uses in the current loop with the arrays.
    
    // scalar expand the memorized variables according to the
    // type of the exposed uses
    // 10 different types of symbols
    DYN_ARRAY<SCALAR_NODE*> use_I8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_I8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_U8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_U8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_F8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_F8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_C4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_C4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_I4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_I4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_U4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_U4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_F4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_F4(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_FQ(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_FQ(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_C8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_C8(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_CQ(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_CQ(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_I2(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_I2(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_U2(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_U2(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_I1(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_I1(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_U1(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_U1(&PHASE25_default_pool);
#if defined(TARG_IA64) || defined(TARG_X8664)
    DYN_ARRAY<SCALAR_NODE*> use_F10(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_F10(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> use_C10(&PHASE25_default_pool);
    DYN_ARRAY<SCALAR_NODE*> site_C10(&PHASE25_default_pool);
#endif

    for (UINT j=0; j<exposed_use[i-1]->Elements(); ++j){
      SYMBOL& cur_use = exposed_use[i-1]->Bottom_nth(j)->_scalar;

      switch (cur_use.Type){
      case MTYPE_I8:
	use_I8.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_I8.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_U8:
	use_U8.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_U8.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_F8:
	use_F8.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_F8.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_C4:
	use_C4.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_C4.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_I4:
	use_I4.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_I4.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_U4:
	use_U4.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_U4.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_F4:
	use_F4.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_F4.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_FQ:
	use_FQ.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_FQ.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_C8:
	use_C8.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_C8.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_CQ:
	use_CQ.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_CQ.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_I2:
	use_I2.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_I2.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_U2:
	use_U2.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_U2.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_I1:
	use_I1.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_I1.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
      case MTYPE_U1:
	use_U1.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_U1.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
#if defined(TARG_IA64) || defined(TARG_X8664)
      case MTYPE_F10:
        use_F10.AddElement(exposed_use[i-1]->Bottom_nth(j));
        site_F10.AddElement(exposed_site[i-1]->Bottom_nth(j));
        break;
      case MTYPE_C10:
	use_C10.AddElement(exposed_use[i-1]->Bottom_nth(j));
	site_C10.AddElement(exposed_site[i-1]->Bottom_nth(j));
	break;
#endif
      default:
	FmtAssert(0, ("Bad type in scalar memorization"));
      }
    }

    if (use_I8.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop, 
				   loop_current, use_I8, site_I8, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_U8.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_U8, site_U8, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_F8.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop, 
				   loop_current, use_F8, site_F8, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_C4.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop, 
				   loop_current, use_C4, site_C4, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_I4.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_I4, site_I4, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_U4.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop, 
				   loop_current, use_U4, site_U4, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_F4.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_F4, site_F4, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_FQ.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop, 
				   loop_current, use_FQ, site_FQ, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_C8.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_C8, site_C8, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_CQ.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_CQ, site_CQ, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_I2.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_I2, site_I2, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_U2.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_U2, site_U2, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_I1.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_I1, site_I1, 
				   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_U1.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_U1, site_U1, 
				   if_stmt, &alloc_loop,&dealloc_loop);
#if defined(TARG_IA64) || defined(TARG_X8664)
    if (use_F10.Elements())
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
                                   loop_current, use_F10, site_F10,
                                   if_stmt, &alloc_loop,&dealloc_loop);
    if (use_C10.Elements()) 
      Gather_Scatter_Scalar_Expand(loop_first, ld_inc, tile_loop,
				   loop_current, use_C10, site_C10, 
				   if_stmt, &alloc_loop,&dealloc_loop);
#endif
  } 

  CXX_DELETE(cond_scc, &PHASE25_default_pool);
  CXX_DELETE(new_scc, &PHASE25_default_pool);
  CXX_DELETE(other_scc, &PHASE25_default_pool);
  CXX_DELETE_ARRAY(queue, &PHASE25_default_pool);
  CXX_DELETE_ARRAY(exposed_use, &PHASE25_default_pool);
  CXX_DELETE_ARRAY(exposed_site, &PHASE25_default_pool);

}

// Fission a inner loop 'innerloop' if it contains a section
// of conditional codes at the end to eliminate speculative
// codes.  
// Most of the code is copied directly from Fission_Inner_Loop.
//
extern INT Fiss_Gather_Inner_Loop(WN* innerloop)
{

  DO_LOOP_INFO* dli=Get_Do_Loop_Info(innerloop);
  Is_True(dli,("Fiss_Gather_Inner_Loop:No DO_LOOP_INFO for the loop"));

  // if there are too few iterations, we will not fission
  if (dli->Est_Num_Iterations < Iteration_Count_Threshold)
    return 1;

  // Test if the loop upper bound can be standardized
  WN* ub = WN_end(innerloop);

  // Upper bound of unsigned do loop should be already be standardized
  if (Do_Loop_Is_Unsigned(innerloop) &&
      (UBvar(ub) == NULL || WN_operator(ub) != OPR_LE))
    return 0;
  
  // Upper_Bound_Standardize() could not solve for loop index
  BOOL ok = Solve_For(ub, SYMBOL(WN_index(innerloop)));
  if (ok == FALSE) return 0;

  // Surprise operator returned from Solve_For();
  OPCODE   opc = WN_opcode(ub);
  OPERATOR opr = OPCODE_operator(opc);
  if (opr != OPR_LT && opr != OPR_LE) return 0;

  MEM_POOL_Push(&PHASE25_default_pool);
  {

    // a dictionary used for looking up the bit position for a symbol
    BINARY_TREE<NAME2BIT> *mapping_dictionary = 
      CXX_NEW(BINARY_TREE<NAME2BIT>(&PHASE25_default_pool),&PHASE25_default_pool);

    // main statement dependence graph for statements in the loop
    SCC_DIRECTED_GRAPH16 *dep_g_p =
      CXX_NEW(SCC_DIRECTED_GRAPH16(ESTIMATED_SIZE,ESTIMATED_SIZE),
	      &PHASE25_default_pool);

    // hash table which associates the statements in the loop and vertices in the
    // above dependence graph 'dep_g_p'
    WN2VINDEX *stmt_to_vertex=
      CXX_NEW(WN2VINDEX(ESTIMATED_SIZE, &PHASE25_default_pool),
	      &PHASE25_default_pool);

    // hash table which maps a statement to a integer statement id
    WN2UINT *stmt_id=
      CXX_NEW(WN2UINT(ESTIMATED_SIZE, &PHASE25_default_pool),&PHASE25_default_pool);

    // hash table which maps a reference to either class scalar or array
    
    REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&PHASE25_default_pool),
				     &PHASE25_default_pool);
    REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&PHASE25_default_pool),
				    &PHASE25_default_pool);

    SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&PHASE25_default_pool),
					  &PHASE25_default_pool);
    SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&PHASE25_default_pool),
					 &PHASE25_default_pool);
    SCALAR_REF_STACK *params =
      CXX_NEW(SCALAR_REF_STACK(&PHASE25_default_pool), &PHASE25_default_pool);

    // stack used in collecting references
    DOLOOP_STACK stack(&PHASE25_default_pool);
    Build_Doloop_Stack(innerloop, &stack);

    // step 1: gather all (scalar and array) references in the loop
    //         allocate a vertex in the stmt. dep. graph for each stmt
    //         assign statement id for each statement
    WN* body=WN_do_body(innerloop);
    WN* stmt;
    UINT stmt_count=0;
    INT32 gather_status=0;
    Init_Ref_Stmt_Counter();
    for (stmt=WN_first(body); stmt && gather_status != -1; stmt=WN_next(stmt)) {
      stmt_to_vertex->Enter(stmt, dep_g_p->Add_Vertex());
      stmt_id->Enter(stmt, stmt_count++);
      gather_status=New_Gather_References(stmt,writes,reads,&stack,
	scalar_writes,scalar_reads,params,&PHASE25_default_pool);
    }
    if (gather_status == -1) {
	DevWarn("Error in gathering references");
	MEM_POOL_Pop(&PHASE25_default_pool);
	return 0;
    }

    // list of references that use scalar-expandable variables
    FF_STMT_LIST expandable_ref_list;

    // step 2: examine all reads and writes and do the following
    //		1. classify them as scalar or array
    //		2. create name to bit position mappings for new symbol names
    //		3. if the ref is STID, check if it is scalar expandable
    UINT sym_count=inner_fission_2(innerloop, scalar_reads, scalar_writes, 
                            reads, writes, mapping_dictionary, 
                            expandable_ref_list, &PHASE25_default_pool);
    
    // name set bit vector array for all statements
    // each entry correspond to a statement and is a bit vector which
    // shows which symbol's name appear in the statement
    BIT_VECTOR* stmt_name_set=
      CXX_NEW_ARRAY(BIT_VECTOR, stmt_count, &PHASE25_default_pool);
    
    // for each statement i, stmt_name_set[i] is a bit vector which is
    // initialized to be large enough to hold 'sym_count' symbols
    UINT i;
    for (i=0; i<stmt_count; i++)
      stmt_name_set[i].Init(sym_count, &PHASE25_default_pool);

    // we also need to have a set of expandable scalars
    BIT_VECTOR Expandable_Scalar_Set(sym_count, &PHASE25_default_pool);

    // now look at all references in 'expandable_ref_list' and set the
    // corresponding bit in 'Expandable_Scalar_Set'
    FF_STMT_ITER e_iter(&expandable_ref_list);
    for (FF_STMT_NODE* ref_node=e_iter.First(); !e_iter.Is_Empty();
	 ref_node=e_iter.Next()) {
      NAME2BIT temp_map;
      temp_map.Set_Symbol(ref_node->Get_Stmt());
      Expandable_Scalar_Set.Set(mapping_dictionary->Find(temp_map)->
				Get_Data()->Get_Bit_Position());
    }

    if (LNO_Test_Dump) {
      printf("Expandable_Scalar_Set=\n");
      Expandable_Scalar_Set.Print(stdout);
    }

    Register_Name_To_Statement(innerloop,scalar_reads,scalar_writes,reads,writes,
			       stmt_id,stmt_name_set,mapping_dictionary);
    
    WN_MAP sdm=WN_MAP_Create(&PHASE25_default_pool);
    ARRAY_DIRECTED_GRAPH16 *sdg =
      CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH),
	      &PHASE25_default_pool);
    
    for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
      if (!Map_Stmt_To_Level_Graph(stmt,sdg)) {
	DevWarn("Error in mapping stmt to level graph\n");
	WN_MAP_Delete(sdm);
	MEM_POOL_Pop(&PHASE25_default_pool);
	return 0;
      }
    }

    BOOL status=Generate_Scalar_Dependence_For_Statement_Dependence_Graph
      (innerloop, scalar_reads, scalar_writes, params, sdg, red_manager,
       &Expandable_Scalar_Set, mapping_dictionary);
    if (status==FALSE) {
      DevWarn("Statement dependence graph problem");
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&PHASE25_default_pool);
      return(0);
    }

    status=Generate_Array_Dependence_For_Statement_Dependence_Graph
      (innerloop, reads, writes, sdg, red_manager, adg);
    if (status==FALSE) {
      DevWarn("Statement dependence graph problem");
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&PHASE25_default_pool);
      return(0);
    }


    if (LNO_Test_Dump) {
      printf("Statement Dependence Graph of Phase 2.5:\n");
      sdg->Print(stdout);
    }


    EINDEX16 e=sdg->Get_Edge();
    while (e) {
      WN* source=sdg->Get_Wn(sdg->Get_Source(e));
      WN* sink=sdg->Get_Wn(sdg->Get_Sink(e));
      if (LWN_Get_Parent(source) == body || LWN_Get_Parent(sink) == body)
        // add edges only if the source and sink are immediate children
        dep_g_p->Add_Unique_Edge(
          stmt_to_vertex->Find(source),
          stmt_to_vertex->Find(sink));
      e=sdg->Get_Next_Edge(e);
    }


    if (LNO_Test_Dump) {
      printf("Statement SCC Graph of Phase 2.5:\n");
      dep_g_p->Print(stdout);
    }


    // ac_g is the acyclic condensation graph of dep_g_p
    // it stores dependence relations between SCCs
    SCC_DIRECTED_GRAPH16 *ac_g;
    ac_g = dep_g_p->Acyclic_Condensation(&PHASE25_default_pool);


    if (LNO_Test_Dump) {
      printf("SCC Dependence Graph of Phase 2.5:\n");
      ac_g->Print(stdout);
    }


    VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

    // scc[i] is a list of statemens in i-th SCC
    FF_STMT_LIST *scc;
    scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, &PHASE25_default_pool);

    // Append statements to the statement list of proper SCC.
    for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
      VINDEX16 scc_id;
      scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
      scc[scc_id].Append(stmt, &PHASE25_default_pool);  
    }

    if (LNO_Test_Dump)
      for (i=1; i<=total_scc; i++) {

	printf("PHASE 2.5:scc %d:", i);
	FF_STMT_ITER s_iter(&scc[i]);
	INT j=0;
	for (FF_STMT_NODE *stmt_node=s_iter.First(); !s_iter.Is_Empty();
	     stmt_node=s_iter.Next()) {
          stmt=stmt_node->Get_Stmt();
          Dump_WN(stmt,stdout,TRUE,4,4);
	  j++;
	}
      }

    // Try to gather/scatter the loop
    Perform_Gather_Scatter(dep_g_p,total_scc,scc,
			   innerloop, ac_g, stmt_to_vertex);

    // TODO need to update scalar dependence info here
    
    CXX_DELETE(ac_g, &PHASE25_default_pool);
    CXX_DELETE(dep_g_p, &PHASE25_default_pool);
    CXX_DELETE(sdg, &PHASE25_default_pool);
    WN_MAP_Delete(sdm);
  }
  MEM_POOL_Pop(&PHASE25_default_pool);

  if (LNO_Test_Dump) {
    printf("The dependence graph is \n");
    Array_Dependence_Graph->Print(stdout);
  }

  return 1;
  
}

