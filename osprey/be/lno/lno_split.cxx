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


//                     Statement Splitting
//                     -------------------
//
// Description:
//
//	Split large statements.  This allows phase 3 to do more
//	fission.
//
//
/* ====================================================================
 * ====================================================================
 *
 * Module: lno_split.cxx
 * $Revision: 1.6 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lno_split.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Split statements
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.lno_split.cxx $ $Revision: 1.6 $";

#include <sys/types.h>
#include "lnopt_main.h"
#include "dep_graph.h"
#include "model.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "config_targ_opt.h"
#include "config_targ.h"
#include "config_opt.h"
#include "opt_du.h"
#include "opt_alias_interface.h"
#include "reduc.h"

static INT Split_Statement(WN *statement, ARRAY_DIRECTED_GRAPH16 *dep_graph);
static BOOL Scalar_Interferes(WN *store, WN *expr, WN *split_point,
				ARRAY_DIRECTED_GRAPH16 *dep_graph);
static WN *Find_Inner(WN *wn);
static WN *Find_Split_Point(WN *store);
static INT Num_Leaves_Or_Arrays(WN *wn);
static WN *Find_Enough_Subtree(WN *wn, INT enough, INT *num_leaves);
static BOOL Need_To_Split(WN *region);
extern BOOL Variant_Array(WN *store, WN *split_point, 
				ARRAY_DIRECTED_GRAPH16 *dep_graph);
extern WN *Split_Using_Preg(WN *statement, WN *split_point,
				ARRAY_DIRECTED_GRAPH16 *dep_graph,
				BOOL recursive=TRUE);
extern INT Split_Array(WN *statement, WN *split_point,
				ARRAY_DIRECTED_GRAPH16 *dep_graph);

static BOOL is_ok_to_reassociate(OPCODE opc)
{
  // based on common/com/wn_simp_code.h
  // TODO: unify these predicates in a single, shared location.

   switch (OPCODE_operator(opc)) {
    case OPR_MAX:
    case OPR_MIN:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_LAND:
    case OPR_LIOR:
      return (TRUE);

    case OPR_ADD:
    case OPR_MPY:
      if ( MTYPE_is_integral(OPCODE_rtype(opc)) ) {
	 return TRUE;
      } else if ( MTYPE_is_float(OPCODE_rtype(opc)) ) {
	 return ( Enable_Cfold_Reassociate );
      } else {
	 /* Don't know what to do */
	 return FALSE;
      }
      
    default:
      return FALSE;
   }
}

static WN *reassoc_expr(WN *wn)
{
  if ( ! is_ok_to_reassociate( WN_opcode(wn) ) ) {
    for (INT i = 0; i < WN_kid_count(wn); ++i) {
      WN_kid(wn, i) = reassoc_expr(WN_kid(wn,i));
    }
    return wn;
  }
  OPERATOR opr   = WN_operator(wn);
  TYPE_ID  rtype = WN_rtype(wn);
  WN      *lhs   = reassoc_expr(WN_kid0(wn));
  WN      *rhs   = reassoc_expr(WN_kid1(wn));
  WN      *e     = rhs;
  WN      *e2    = e;

  WN_kid0(wn) = lhs;  // make sure childen connected properly.
  WN_kid1(wn) = rhs;

  //           wn                   rhs
  //          / |                  / |
  //         L  rhs               ~  f
  //           / |     ===>       e
  //          ~  f               / \ 
  //          e                 wn  b
  //         / \               /  \ 
  //        e2  b             L   e2

  while ( WN_operator(e2) == opr && WN_rtype(e2) == rtype ) {
    e  = e2;
    e2 = WN_kid0(e2);
  }
  if ( e2 != e ) {
    WN_kid1(wn) = e2;
    WN_kid0(e)  = wn;
    return rhs;
  }
  return wn;
}

// split all the statements that use too many register in a region 
// (where region is defined loosely).  
// Too many is at least Target_FPRs -2
// All stores in the region must be descendents of a do loop
//
// Do nothing if we have less than 32 registers (in that case model will
// always think we need too many registers regardless)
//
// Return 0 on error
INT Split_Region(WN *region, ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  if (Target_FPRs < 32) return 1;

  OPCODE opcode = WN_opcode(region);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(region);
    while (kid) {
      WN *next = WN_next(kid);
      if (!Split_Region(kid,dep_graph)) return 0;
      kid = next;
    }
  } else if (OPCODE_is_store(opcode) &&         // The following test ensures
                                                // that we don't try to split 
                                                // large initialization stids
                                                // under DO_LOOPS
             WN_opcode(LWN_Get_Parent(region)) == OPC_BLOCK) {
    if (Need_To_Split(region)) {
      if ( Enable_Cfold_Reassociate ) {
	OPERATOR opr = WN_operator(region);
	if ( opr == OPR_STID || opr == OPR_ISTORE ) {
	  // pv 593337: ensure canonical form, which may be
	  // destroyed by forward substitution.  Canonical
	  // form is well suited to spliting in a way that
	  // leads to benificial loop fission.
	  WN_kid0(region) = reassoc_expr(WN_kid0(region));
	  LWN_Parentize(region);
	}
      }
      if (!Split_Statement(region,dep_graph)) return 0;
    }
  } else if (!OPCODE_is_expression(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(region); kidno++) {
      if (!Split_Region(WN_kid(region,kidno),dep_graph)) return 0;
    }
  }
  return 1;
}

static BOOL Need_To_Split(WN *region)
{
  MEM_POOL_Push(&LNO_local_pool);
  REGISTER_MODEL *reg_model = CXX_NEW(REGISTER_MODEL(&LNO_local_pool), 
							&LNO_local_pool);
  reg_model->Add_Statement(region);
  INT fpr,intr,tlb;
  reg_model->Calculate_Register_Usage(Find_Inner(region),&fpr,&intr,&tlb);
  if (fpr >= (Target_FPRs-2) || intr >= Target_INTRs-2 ||
      tlb >= Mhd.L[0].TLB_Entries-2) {
    MEM_POOL_Pop(&LNO_local_pool);
    return TRUE;
  } 
  MEM_POOL_Pop(&LNO_local_pool);
  return FALSE;
}

// Given that statement needs to be split, split it in half
// Given a = b(...) + c(...), (where b and c are subtrees),
// we'd usually split this into
// preg = c(...)
// a = b(...) + preg
//
// if 'a' is a variant array references, i.e. 
// a[i] = b(...) + c(...)
// we instead split this into
// a[i] = c(...)
// a[i] = b(...) + a[i]
// this allows us to fission between the two statements without doing
// scalar expansion
// it's only legal if there is no all equals dependence to a[i] from 
// anything in b(...)
//
// return 0 on error
static INT Split_Statement(WN *statement, ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  WN *split_point = Find_Split_Point(statement);
  if (!WN_kid_count(split_point)) {
    return 1;  // don't split leaves
  }

  if ((WN_operator(statement)==OPR_STID || WN_operator(statement)==OPR_ISTORE)
      && split_point == WN_kid0(statement)) {

    // we're splitting around the entire RHS, so don't bother
    return 1;
  }

  if (Variant_Array(statement,split_point,dep_graph) &&
      (WN_rtype(split_point)  == WN_desc(statement))) {
    if (!Split_Array(statement,split_point,dep_graph)) {
      return 0;
    }
  } else {
    Split_Using_Preg(statement,split_point,dep_graph);
  }
  return 1;
}


extern INT Split_Array(WN *store, WN *split_point, 
		ARRAY_DIRECTED_GRAPH16 *dep_graph) 
{
  // make two copies of the array statement
  WN *array1 = LWN_Copy_Tree(WN_kid1(store),TRUE,LNO_Info_Map);
  LWN_Copy_Def_Use(WN_kid1(store),array1,Du_Mgr);
  WN *array2 = LWN_Copy_Tree(WN_kid1(store),TRUE,LNO_Info_Map);
  LWN_Copy_Def_Use(WN_kid1(store),array2,Du_Mgr);

  // load the array at the split point
  WN *split_point_parent = LWN_Get_Parent(split_point);
  TY_IDX addr_type = WN_ty(store);
  TY_IDX el_type = TY_pointed(addr_type);
  TYPE_ID mtype = WN_desc(store);
  OPCODE l_opcode = OPCODE_make_op(OPR_ILOAD,Promote_Type(mtype),mtype);
#ifndef KEY
  WN *load = 
    LWN_CreateIload(l_opcode,WN_offset(store),el_type,addr_type,array1);
#else
  WN *load = 
    LWN_CreateIload(l_opcode,WN_offset(store),el_type,addr_type,array1, 
		    WN_field_id(store));
#endif /* KEY */
  Copy_alias_info(Alias_Mgr,store,load);
  LWN_Set_Parent(load,split_point_parent);
  INT i=0;
  while (WN_kid(split_point_parent,i) != split_point) i++;
  WN_kid(split_point_parent,i) = load;
  if (dep_graph->Add_Deps_To_Copy_Block(WN_kid1(store),array1,FALSE)==0) {
     LNO_Erase_Dg_From_Here_In(load,dep_graph);
     if (red_manager) 
       red_manager->Erase(store);
     return 0;
  }

  // store the code below split_point into the array
#ifndef KEY
  WN *new_store = LWN_CreateIstore(WN_opcode(store),WN_offset(store),
	WN_ty(store),split_point,array2);
#else
  WN *new_store = LWN_CreateIstore(WN_opcode(store),WN_offset(store),
	WN_ty(store),split_point,array2, WN_field_id(store));
#endif /* KEY */
  Copy_alias_info(Alias_Mgr,store,new_store);
  LWN_Copy_Linenumber(store,new_store);
  LWN_Insert_Block_Before(LWN_Get_Parent(store),store,new_store);
  if (dep_graph->Add_Deps_To_Copy_Block(WN_kid1(store),array2,FALSE)==0) {
     LNO_Erase_Dg_From_Here_In(new_store,dep_graph);
     if (red_manager) { 
       red_manager->Erase(store);
       red_manager->Erase(new_store);
     } 
     return 0;
  }

  // fix the array dependence graph
  BOOL not_overflow;
  VINDEX16 new_sv = dep_graph->Add_Vertex(new_store);
  // Is_True(new_sv,("Graph overflow in Split_Array \n"));
  if (!new_sv) {
     LNO_Erase_Dg_From_Here_In(new_store,dep_graph);
     if (red_manager) { 
       red_manager->Erase(store);
       red_manager->Erase(new_store);
     } 
     return 0;
  }
  VINDEX16 new_lv = dep_graph->Add_Vertex(load);
  // Is_True(new_lv,("Graph overflow in Split_Array \n"));
  if (!new_lv) {
     LNO_Erase_Dg_From_Here_In(load,dep_graph);
     if (red_manager) { 
       red_manager->Erase(store);
       red_manager->Erase(new_store);
     } 
     return 0;
  }

  // new store gets all the dependences from old store
  // load gets the same dependences except not the load-load

  VINDEX16 store_v = dep_graph->Get_Vertex(store);
  EINDEX16 e = dep_graph->Get_Out_Edge(store_v);
  while (e) {
    VINDEX16 sink = dep_graph->Get_Sink(e);
    if (sink != store_v) {
      not_overflow = dep_graph->Add_Edge(new_sv,sink,
	  Create_DEPV_ARRAY(dep_graph->Depv_Array(e),&LNO_default_pool));
      // Is_True(not_overflow,("Graph overflow in Split_Array \n"));
      if (!not_overflow) {
         LNO_Erase_Dg_From_Here_In(store,dep_graph);
         if (red_manager) { 
	   red_manager->Erase(store);
	   red_manager->Erase(new_store);
         } 
         return 0;
      }
      if (!OPCODE_is_load(WN_opcode(dep_graph->Get_Wn(sink)))) {
        not_overflow = dep_graph->Add_Edge(new_lv,sink,
	  Create_DEPV_ARRAY(dep_graph->Depv_Array(e),&LNO_default_pool));
        // Is_True(not_overflow,("Graph overflow in Split_Array \n"));
        if (!not_overflow) {
           LNO_Erase_Dg_From_Here_In(store,dep_graph);
           if (red_manager) { 
	     red_manager->Erase(store);
	     red_manager->Erase(new_store);
           } 
           return 0;
        }
      }
    }
    e = dep_graph->Get_Next_Out_Edge(e);
  }

  e = dep_graph->Get_In_Edge(store_v);
  while (e) {
    VINDEX16 source = dep_graph->Get_Source(e);
    if (source != store_v) {
      not_overflow = dep_graph->Add_Edge(source, new_sv,
	  Create_DEPV_ARRAY(dep_graph->Depv_Array(e),&LNO_default_pool));
      // Is_True(not_overflow,("Graph overflow in Split_Array \n"));
      if (!not_overflow) {
        LNO_Erase_Dg_From_Here_In(store,dep_graph);
        if (red_manager) { 
	  red_manager->Erase(store);
	  red_manager->Erase(new_store);
	} 
        return 0;
      }
      if (!OPCODE_is_load(WN_opcode(dep_graph->Get_Wn(source)))) {
        not_overflow = dep_graph->Add_Edge(source, new_lv,
	  Create_DEPV_ARRAY(dep_graph->Depv_Array(e),&LNO_default_pool));
        // Is_True(not_overflow,("Graph overflow in Split_Array \n"));
        if (!not_overflow) {
          LNO_Erase_Dg_From_Here_In(store,dep_graph);
          if (red_manager) { 
	    red_manager->Erase(store);
	    red_manager->Erase(new_store);
          } 
          return 0;
        }
      }
    }
    e = dep_graph->Get_Next_In_Edge(e);
  }


  // get all the cross dependences
  WN *inner = Find_Inner(store);
  INT depth = Do_Loop_Depth(inner);
  INT good_depth = Good_Do_Depth(store);
  INT num_bad = depth - good_depth;

  EINDEX16 self_e = dep_graph->Get_Edge(store_v,store_v);
  if (!self_e) {
    DEPV_ARRAY *zero = Create_DEPV_ARRAY(1,good_depth+1,num_bad,
						&LNO_default_pool);
    for (INT ii=0; ii<=good_depth; ii++) {
      DEPV_Dep(zero->Depv(0),ii) = DEP_SetDirection(DIR_EQ);
    }
    DEPV_ARRAY *zero2 = Create_DEPV_ARRAY(zero,&LNO_default_pool);
    DEPV_ARRAY *zero3 = Create_DEPV_ARRAY(zero,&LNO_default_pool);

    not_overflow = dep_graph->Add_Edge(new_sv,store_v,zero);
    not_overflow = not_overflow && dep_graph->Add_Edge(new_sv,new_lv,zero2);
    not_overflow = not_overflow && dep_graph->Add_Edge(new_lv,store_v,zero3);
  } else {
    DEPV_ARRAY *self_dep = dep_graph->Depv_Array(self_e);
    DEPV *tmp_dep = DEPV_Create(&LNO_local_pool,good_depth+1);
    for (INT ii=0; ii<=good_depth; ii++) {
      DEPV_Dep(tmp_dep,ii) = DEP_SetDirection(DIR_EQ);
    }
    DEPV_LIST *tmp = CXX_NEW(DEPV_LIST(self_dep,&LNO_local_pool),
	&LNO_local_pool);
    tmp->Append(CXX_NEW(DEPV_NODE(tmp_dep),&LNO_local_pool));

    not_overflow = dep_graph->Add_Edge(new_sv,new_sv,
	Create_DEPV_ARRAY(self_dep,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(store_v,new_sv,
	Create_DEPV_ARRAY(self_dep,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(new_sv,store_v,
	Create_DEPV_ARRAY(tmp,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(new_lv,new_sv,
	Create_DEPV_ARRAY(self_dep,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(new_sv,new_lv,
	Create_DEPV_ARRAY(tmp,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(store_v,new_lv,
	Create_DEPV_ARRAY(self_dep,&LNO_default_pool));
    not_overflow = not_overflow && dep_graph->Add_Edge(new_lv,store_v,
	Create_DEPV_ARRAY(tmp,&LNO_default_pool));
  }

  // Is_True(not_overflow,("Graph overflow in Split_Array \n"));
  if (!not_overflow) {
    LNO_Erase_Dg_From_Here_In(store,dep_graph);
    if (red_manager) { 
      red_manager->Erase(store);
      red_manager->Erase(new_store);
    } 
    return 0;
  }


  // recalculate reductions
  if (red_manager && (red_manager->Which_Reduction(store) != RED_NONE)) {
    red_manager->Erase(store);
    red_manager->Erase(new_store);
    red_manager->Build(store,TRUE,TRUE,dep_graph);
    red_manager->Build(new_store,TRUE,TRUE,dep_graph);
  }


  // recurse on two split statements if we need to
  if (Need_To_Split(new_store)) {
    Split_Statement(new_store,dep_graph);
  }


  return 1;

}


// Split by creating a new preg for the temp value
// this is always safe
//
// return the new statment created
extern WN *Split_Using_Preg(WN *statement, WN *split_point,
				ARRAY_DIRECTED_GRAPH16 *dep_graph,
                                BOOL recursive)
{
  // create a new preg
  OPCODE store_opcode = WN_opcode(statement);
  OPCODE split_opcode = WN_opcode(split_point);
  TYPE_ID type = OPCODE_rtype(split_opcode);
  ST *preg_st = MTYPE_To_PREG(type);
  const char *orig_name;
  if (OPCODE_operator(store_opcode) == OPR_STID) {
    orig_name = ST_name(WN_st(statement));
  } else if ((WN_operator(WN_kid1(statement)) == OPR_ARRAY) &&
	     (OPCODE_has_sym(WN_opcode(WN_array_base(WN_kid1(statement)))))) {
    orig_name = ST_name(WN_st(WN_array_base(WN_kid1(statement))));
  } else {
    orig_name = "blank";
  }
  char new_name[20]; 
  INT length = strlen(orig_name);
  WN_OFFSET preg_num;
  if (length < 18) {
    strcpy(new_name,orig_name);
    new_name[length] = '_';
    new_name[length+1] = '1';
    new_name[length+2] = 0;
#ifdef _NEW_SYMTAB
    preg_num = Create_Preg(type,new_name);
  } else {
    preg_num = Create_Preg(type,NULL);
  }
#else
    preg_num = Create_Preg(type,new_name, NULL);
  } else {
    preg_num = Create_Preg(type,NULL,NULL);
  }
#endif

  // load the preg at split point
  WN *split_point_parent = LWN_Get_Parent(split_point);
  OPCODE preg_l_opcode = OPCODE_make_op(OPR_LDID,Promote_Type(type),type);
  WN *preg_load = WN_CreateLdid(preg_l_opcode,preg_num,preg_st,
						Be_Type_Tbl(type));
  LWN_Set_Parent(preg_load,split_point_parent);
  INT i=0;
  while (WN_kid(split_point_parent,i) != split_point) i++;
  WN_kid(split_point_parent,i) = preg_load;

  // store the preg before the store
  OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID,MTYPE_V,type);
  WN *preg_store = LWN_CreateStid(preg_s_opcode,
                                  preg_num,preg_st,
                                  Be_Type_Tbl(type),split_point);
  LWN_Copy_Linenumber(statement,preg_store);
  LWN_Insert_Block_Before(LWN_Get_Parent(statement),statement,preg_store);
  // in case the expression that we stored is an address...
  WN_set_st_addr_saved(split_point);

  // Fix up the du
  Du_Mgr->Add_Def_Use(preg_store,preg_load);


  // recalculate reductions
  if (red_manager && (red_manager->Which_Reduction(statement) != RED_NONE)) {
    red_manager->Erase(statement);
    red_manager->Erase(preg_store);
    red_manager->Build(statement,TRUE,TRUE,dep_graph);
    // preg_store can't be a reduction
  }

  // recurse on two split statements if we need to
  if (recursive &&
      WN_kid_count(split_point_parent) > 1 &&   // only if we created
                                                // something simpler
      Need_To_Split(preg_store)) {
    Split_Statement(preg_store,dep_graph);
  }

  if (recursive && Need_To_Split(statement)) {
    Split_Statement(statement,dep_graph);
  }
  return preg_store;
}

// Return true if
// 1. The store is a cleanly variant array reference
// 2. Other than underneath the split point there is no all equals dependent
//    load to the store 
// it's always conservative to return FALSE
extern BOOL Variant_Array(WN *store, WN *split_point, 
				ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  if (WN_operator(store) != OPR_ISTORE) return FALSE;

  // check the array
  if (WN_kid_count(store) != 2) return FALSE;
  WN *array = WN_kid1(store);
  if (WN_operator(array) != OPR_ARRAY) return FALSE;

  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);
  if (aa->Too_Messy) return FALSE;

  WN *inner = Find_Inner(store);
  if (!Do_Loop_Is_Good(inner) || Do_Loop_Has_Gotos(inner)) return FALSE;
  INT depth = Do_Loop_Depth(inner);

  BOOL seen_var = FALSE;
  for (INT i=0; i<aa->Num_Vec(); i++) {
    ACCESS_VECTOR *av = aa->Dim(i);
    if (av->Too_Messy || (av->Non_Const_Loops() == (depth+1))) return FALSE;
    if (av->Delinearized_Symbol)
      return FALSE; 
    if (av->Loop_Coeff(depth)) seen_var=TRUE;
  }
  if (!seen_var) return FALSE;

  // now the array is the right form, check for dependences from any 
  // load under store but not under split_point to the store
  VINDEX16 v = dep_graph->Get_Vertex(store);
  if (!v) return FALSE;
  EINDEX16 e;
  for (e=dep_graph->Get_In_Edge(v); e; e=dep_graph->Get_Next_In_Edge(e)) {
    if (dep_graph->Depv_Array(e)->Max_Level() > depth) {
      WN *source_wn = dep_graph->Get_Wn(dep_graph->Get_Source(e));
      if (Is_Descendent(source_wn,store) && 
	  !Is_Descendent(source_wn,split_point)) {
        return FALSE;
      }
    }
  }

  // now make sure none of the scalars are aliased to the store
  // this is extremely unlikely but we have to be sure
  if (Scalar_Interferes(store,WN_kid0(store),split_point,dep_graph)) {
    return FALSE;
  }
  return TRUE;
}



// do any of the scalars under expr alias with store (ignore things under
// the split point), we need to check these explicitly since they're not
// in the array dependence graph
static BOOL Scalar_Interferes(WN *store, WN *expr, WN *split_point,
				ARRAY_DIRECTED_GRAPH16 *dep_graph) 
{
  if (expr == split_point) {
    return FALSE;
  }

  if (OPCODE_is_load(WN_opcode(expr))) {
    VINDEX16 v = dep_graph->Get_Vertex(expr);
    if (v) return FALSE; // we've already checked this one
    return (Aliased(Alias_Mgr,store,expr) != NOT_ALIASED);
  } else {
    for (INT kidno=0; kidno < WN_kid_count(expr); kidno++) {
      if (Scalar_Interferes(store,WN_kid(expr,kidno),split_point,dep_graph)) {
	return TRUE;
      }
    }
    return FALSE;
  }
}



// find the innermost do loop surrounding this statement
// assumes there is one
static WN *Find_Inner(WN *wn)
{
  while (WN_opcode(wn) != OPC_DO_LOOP) wn = LWN_Get_Parent(wn);
  return wn;
}
  


// Given a statement, find a reasonable point to split it
// Return the root of the subtree that should be split
//
// Our goal is that both sides of the split be about the same size
// we accomplish this by searching bottom up until we hit a subtree with 
// about half the leaves
static WN *Find_Split_Point(WN *store)
{
  WN *rhs = WN_kid0(store);
  INT enough_leaves = (Num_Leaves_Or_Arrays(rhs) >> 1) ;
  INT tmp;
  WN *result = Find_Enough_Subtree(rhs,enough_leaves,&tmp);
  if (WN_operator(result) == OPR_PARM) {
    result = WN_kid0(result); // don't split a parm
  }
  if (WN_operator(result) == OPR_ARRAY &&
      WN_operator(LWN_Get_Parent(result)) == OPR_ILOAD) {
    // don't split underneath an ILOAD either; causes unnecessary address-save
    result = LWN_Get_Parent(result);
  }
  return result;
}

// how many leaves or arrays in this expression node (can't be an hcf node)
static INT Num_Leaves_Or_Arrays(WN *wn)
{
  if ((WN_kid_count(wn) == 0) ||
      (WN_operator(wn) == OPR_ARRAY)) {
    return 1;
  } 
  INT result = 0;
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    result += Num_Leaves_Or_Arrays(WN_kid(wn,kidno));
  }
  return result;
}

// Searching bottom up, find the first subtree that contains at least
// "enough" leaves or arrays
// on exit, set *num_leaves to the number of leaves/arrays under this tree
static WN *Find_Enough_Subtree(WN *wn, INT enough, INT *num_leaves)
{
  *num_leaves = 0;
  if ((WN_kid_count(wn) == 0) ||
      (WN_operator(wn) == OPR_ARRAY)) {
    *num_leaves = 1;
    return wn;
  } 

  INT temp = 0;
  WN *result;
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    result = Find_Enough_Subtree(WN_kid(wn,kidno),enough, &temp);
    if (temp >= enough) {
      *num_leaves = temp;
      return result;
    } else {
      *num_leaves += temp;
    }
  }
  return wn;
}

