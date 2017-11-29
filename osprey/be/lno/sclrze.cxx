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


//                     Array Scalarization
//                     -------------------
//
// Description:
//
// 	In loops, convert things that look like
//	do i
//	  a[i] = ...
//	  ... = a[i]
//
//	into
//
//	do i
//	  t = ...
//	  a[i] = t
//	  ... = t
//
//	This is useful because 
//	  1) It gets rid of loads
//	  2) If it gets rid of all the loads to a local array then
//	     the array equivalencing algorithm will get rid of the array
//
//	Because SWP will do 1 as well as we do, we'll only apply this
//	algorithm to local arrays (Although it's trivial to change this).
//
/* ====================================================================
 * ====================================================================
 *
 * Module: sclrze.cxx
 * $Revision: 1.7 $
 * $Date: 05/04/07 19:50:39-07:00 $
 * $Author: kannann@iridot.keyresearch $
 * $Source: be/lno/SCCS/s.sclrze.cxx $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Scalarize arrays 
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

const static char *source_file = __FILE__;
const static char *rcs_id = "$Source: be/lno/SCCS/s.sclrze.cxx $ $Revision: 1.7 $";

#include <sys/types.h>
#include "lnopt_main.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "reduc.h"
#include "sclrze.h"
#include "lnoutils.h"

static void Process_Store(WN *, VINDEX16 , ARRAY_DIRECTED_GRAPH16 *, BOOL,
			  BOOL, REDUCTION_MANAGER *red_manager);
static BOOL Intervening_Write(INT,VINDEX16, 
			VINDEX16 ,ARRAY_DIRECTED_GRAPH16 *);
static BOOL Is_Invariant(ACCESS_ARRAY *store, WN *store_wn);
static BOOL MP_Problem(WN *wn1, WN *wn2);
static HASH_TABLE<ST *, INT> * Array_Use_Hash;
static int DSE_Count = 0;
extern BOOL ST_Has_Dope_Vector(ST *);

// Query whether 'st' represents a local variable.
static BOOL Is_Local_Var(ST * st)
{
  if ((ST_sclass(st) == SCLASS_AUTO) 
      && (ST_base_idx(st) == ST_st_idx(st))
      && !ST_has_nested_ref(st)
      && (ST_class(st) == CLASS_VAR))
    return TRUE;

  return FALSE;
}

// Query whether use references to 'st' is tracked in "Mark_Array_Uses".
// Limit to local allocate arrays.
static BOOL Is_Tracked(ST * st)
{
  return (Is_Local_Var(st) && ST_Has_Dope_Vector(st));
}

// bit mask for symbol attributes.
#define HAS_ESCAPE_USE 1  // has a use not reachable by a dominating def.
#define ADDR_TAKEN 2 // is address-taken.
#define HAS_USE 4  // has a use.
#define IS_ALLOC 8  // is allocated/dealloacted.

// Query whether 'store_wn' is dead, i.e., its address is not taken and it has no use.
static BOOL Is_Dead_Store(WN * store_wn)
{
  OPERATOR opr = WN_operator(store_wn);
  if (!OPERATOR_is_scalar_store(opr)) {
    WN * base = WN_array_base(WN_kid(store_wn,1));
    if (WN_has_sym(base)) {
      ST * st = WN_st(base);
      if (st && Is_Tracked(st)
	  && Array_Use_Hash
	  && (Array_Use_Hash->Find(st) == IS_ALLOC))
	return TRUE;
    }
  }
  return FALSE;
}

// Given an array reference 'load_wn", query where there exists a reaching def that dominates it.
static BOOL Has_Dom_Reaching_Def(ARRAY_DIRECTED_GRAPH16 * dep_graph, WN * load_wn)
{
  OPERATOR opr = WN_operator(load_wn);
  if (!OPERATOR_is_load(opr) || OPERATOR_is_scalar_load(opr))
    return FALSE;

  VINDEX16 v = dep_graph->Get_Vertex(load_wn);
  if (!v)
    return FALSE;

  WN * kid = WN_kid0(load_wn);
  if (WN_operator(kid) != OPR_ARRAY)
    return FALSE;

  ACCESS_ARRAY * load = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);

  if (!load)
    return FALSE;
  
  for (EINDEX16 e = dep_graph->Get_In_Edge(v); e; e = dep_graph->Get_Next_In_Edge(e)) {
    VINDEX16 source = dep_graph->Get_Source(e);
    WN * store_wn = dep_graph->Get_Wn(source);
    OPERATOR opr = WN_operator(store_wn);
    if (OPERATOR_is_store(opr) && !OPERATOR_is_scalar_store(opr)) {
      kid = WN_kid1(store_wn);
      if (WN_operator(kid) != OPR_ARRAY)
	continue;

      ACCESS_ARRAY * store = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);
      if (Equivalent_Access_Arrays(store,load,store_wn,load_wn)
	  && (DEPV_COMPUTE::Base_Test(store_wn,NULL,load_wn,NULL) == DEP_CONTINUE)) {
	if (Dominates(store_wn, load_wn)
	    && !Intervening_Write(dep_graph->Depv_Array(e)->Max_Level(),
				  source, v, dep_graph)
	    && !MP_Problem(store_wn, load_wn))
	  return TRUE;
      }
    }
  }
  
  return FALSE;
}

// Given an array reference 'store_wn', query whether there exists a kill that post-dominates it.
static BOOL Has_Post_Dom_Kill(ARRAY_DIRECTED_GRAPH16 * dep_graph, WN * store_wn)
{
  OPERATOR opr = WN_operator(store_wn);
  if (!OPERATOR_is_store(opr) || OPERATOR_is_scalar_store(opr))
    return FALSE;

  VINDEX16 v = dep_graph->Get_Vertex(store_wn);
  if (!v)
    return FALSE;

  WN * kid = WN_kid1(store_wn);
  if (WN_operator(kid) != OPR_ARRAY)
    return FALSE;

  WN * base = WN_array_base(kid);

  if (!WN_has_sym(base))
    return FALSE;

  ST * st = WN_st(base);

  // limit it to local non-address-taken arrays.
  if (!Is_Local_Var(st) || !Array_Use_Hash
      || ((Array_Use_Hash->Find(st) & ADDR_TAKEN) != 0))
    return FALSE;
  
  ACCESS_ARRAY * store = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);
  if (!store)
    return FALSE;

  for (EINDEX16 e = dep_graph->Get_Out_Edge(v); e; e = dep_graph->Get_Next_Out_Edge(e)) {
    VINDEX16 sink = dep_graph->Get_Sink(e);
    WN * kill_wn = dep_graph->Get_Wn(sink);

    if (kill_wn == store_wn)
      continue;

    OPERATOR opr = WN_operator(kill_wn);
    if (OPERATOR_is_store(opr) && !OPERATOR_is_scalar_store(opr)) {
      kid = WN_kid1(kill_wn);
      if (WN_operator(kid) != OPR_ARRAY)
	continue;

      ACCESS_ARRAY * kill = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);
      if (Equivalent_Access_Arrays(store, kill, store_wn, kill_wn)
	  && (DEPV_COMPUTE::Base_Test(store_wn, NULL, kill_wn, NULL) == DEP_CONTINUE)) {
	if ((LWN_Get_Parent(store_wn) == LWN_Get_Parent(kill_wn))
	    && !MP_Problem(store_wn, kill_wn)) {
	  WN * next = WN_next(store_wn);
	  while (next) {
	    if (next == kill_wn)
	      return TRUE;
	    next = WN_next(next);
	  }
	}
      }
    }
  }

  return FALSE;
}

// Check usage of a local allocate array.
static void Check_use(ST * st, WN * wn, ARRAY_DIRECTED_GRAPH16 * dep_graph, BOOL escape)
{
  if (st && Is_Tracked(st)) {
    int val = Array_Use_Hash->Find(st);
    val |= HAS_USE;

    if (escape || !Has_Dom_Reaching_Def(dep_graph, wn)) {
      val |= HAS_ESCAPE_USE;	  
    }

    if (val) {
      Array_Use_Hash->Find_And_Set(st, val);
    }
  }
}

// Check use references to local allocate arrays and mark attributes.
// Limit it to Fortran programs currently.
static void Mark_Array_Uses(ARRAY_DIRECTED_GRAPH16 * dep_graph, WN * wn)
{
  FmtAssert(Array_Use_Hash, ("Expect a hash table."));

  WN * kid;
  ST * st;
  OPERATOR opr = WN_operator(wn);

  switch (opr) {
  case  OPR_BLOCK:
    kid = WN_first(wn);
    while (kid) {
      Mark_Array_Uses(dep_graph, kid);
      kid = WN_next(kid);
    }
    break;
  case OPR_LDA:
    st = WN_st(wn);
    if (Is_Tracked(st)) {
      int val = Array_Use_Hash->Find(st);
      BOOL is_pure = FALSE;	
      WN * wn_p = LWN_Get_Parent(wn);

      if (wn_p && ST_Has_Dope_Vector(st)) {
	OPERATOR p_opr = WN_operator(wn_p);
	if (p_opr == OPR_PARM) {
	  wn_p = LWN_Get_Parent(wn_p);
	  if (wn_p
	      && (WN_operator(wn_p) == OPR_CALL)) {
	    char * name = ST_name(WN_st_idx(wn_p));
	    if ((strcmp(name, "_DEALLOCATE") == 0)
		|| (strcmp(name, "_DEALLOC") == 0)
		|| (strcmp(name, "_F90_ALLOCATE_B") == 0)) {
	      is_pure = TRUE;
	      val |= IS_ALLOC;
	    }
	  }
	}
	else if ((p_opr == OPR_STID) && WN_has_sym(wn_p)) {
	  ST * p_st = WN_st(wn_p);
	  TY_IDX ty = ST_type(p_st);
	  if (strncmp(TY_name(ty), ".alloctemp.", 11) == 0)
	    is_pure = TRUE;
	}
      }

      if (!is_pure)
	val |= ADDR_TAKEN;

      if (val) {
	Array_Use_Hash->Find_And_Set(st, val);
      }
    }

    break;
  case OPR_LDID:
    if (WN_has_sym(wn)) {
      st = WN_st(wn);
      WN * store = Find_Containing_Store(wn);
      if (store && (WN_operator(store) == OPR_STID)
	  && (WN_st(store) != st)) {
	if (WN_kid0(store) == wn) {
	  // Be conservative for direct assignment to a different variable.
	  // Consider it as an escape use.
	  Check_use(st, wn, dep_graph, TRUE);
	}
	else
	  Check_use(st, wn, dep_graph, FALSE);
      }
      // Flag ADDR_TAKEN bit for parameters passed by reference.
      WN * wn_p = LWN_Get_Parent(wn);
      if (wn_p && (WN_operator(wn_p) == OPR_PARM)) {
	INT flag = WN_flag(wn_p);
	if (flag & WN_PARM_BY_REFERENCE) {
	  int val = Array_Use_Hash->Find(st);
	  val |= ADDR_TAKEN;
	  if (val) {
	    Array_Use_Hash->Find_And_Set(st, val);
	  }
	}
      }
    }
    break;

  case OPR_ILOAD:
    kid = WN_kid0(wn);
    if (WN_operator(kid) == OPR_ARRAY) {
      WN * base = WN_array_base(kid);
      if (WN_has_sym(base)) {
	st = WN_st(base);
	Check_use(st, wn, dep_graph, FALSE);
      }
    }
    break;

  default:
    ;
  }

  if (opr == OPR_FUNC_ENTRY)
    Mark_Array_Uses(dep_graph,WN_func_body(wn));
  else {
    INT start = (opr == OPR_ARRAY) ? 1 : 0;
    for (INT kidno = start; kidno < WN_kid_count(wn); kidno++) {
      kid = WN_kid(wn, kidno);
      Mark_Array_Uses(dep_graph, kid);
    }
  }
}

void Scalarize_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph,
		      BOOL do_variants, BOOL do_invariants, REDUCTION_MANAGER *red_manager, WN * wn)
{
  if (Get_Trace(TP_LNOPT,TT_LNO_SCLRZE)) {
    fprintf(TFile,"Scalarizing arrays \n");
  }

  Array_Use_Hash = NULL;
  PU & pu = Get_Current_PU();

  if (Do_Aggressive_Fuse && wn 
      && ((PU_src_lang(Get_Current_PU()) & PU_F90_LANG)
	  || (PU_src_lang(Get_Current_PU()) & PU_F77_LANG))) {
    ST * st;
    INT i;
    Array_Use_Hash = CXX_NEW((HASH_TABLE<ST *, INT>) (100, &LNO_default_pool),
			      &LNO_default_pool);
    Mark_Array_Uses(Array_Dependence_Graph, wn);
  }

  // search for a store
  VINDEX16 v;
  VINDEX16 v_next;
  DSE_Count = 0;
  for (v = dep_graph->Get_Vertex(); v; v = v_next) {
    v_next = dep_graph->Get_Next_Vertex_In_Edit(v);
    if (!dep_graph->Vertex_Is_In_Graph(v))
      continue;
    WN *wn = dep_graph->Get_Wn(v);
    OPCODE opcode = WN_opcode(wn);
    if (OPCODE_is_store(opcode) && (WN_kid_count(wn) == 2)) {
      WN *array = WN_kid1(wn);
      if (WN_operator(array) == OPR_ARRAY) {
        WN *base = WN_array_base(array);
        OPERATOR base_oper = WN_operator(base);
        if ((base_oper == OPR_LDID) || (base_oper == OPR_LDA)) {
          ST *st = WN_st(base);
          // is it local
#ifdef _NEW_SYMTAB
          if (ST_level(st) == CURRENT_SYMTAB) {
#else
          if (ST_symtab_id(st) == SYMTAB_id(Current_Symtab)) {
#endif
	    if (ST_sclass(st) == SCLASS_AUTO &&
		ST_base_idx(st) == ST_st_idx(st)) {
	      if (!ST_has_nested_ref(st)) 
                Process_Store(wn,v,dep_graph,do_variants,do_invariants,
			      red_manager);
            }
          }
	  else if (Is_Global_As_Local(st)) {
	    Process_Store(wn,v,dep_graph,do_variants,do_invariants, red_manager);
	  }
        }
      }
    }
  }

  if (DSE_Count > 0) {
    if (Get_Trace(TP_LNOPT,TT_LNO_SCLRZE))
      printf("####Func: %d scalar replacement delete %d stores.\n", Current_PU_Count(), DSE_Count);
  }

  if (Array_Use_Hash)
    CXX_DELETE(Array_Use_Hash, &LNO_default_pool);
}

// Given a store to an array element 'wn', query whether it is read outside its enclosing loop
// assuming unequal acccess arrays refer to disjointed locations.
static BOOL Live_On_Exit(WN * wn)
{
  ARRAY_DIRECTED_GRAPH16 * adg = Array_Dependence_Graph;
  OPERATOR opr = WN_operator(wn);
  FmtAssert(OPERATOR_is_store(opr) && !OPERATOR_is_scalar_store(opr), ("Expect a store."));
  
  VINDEX16 v = adg->Get_Vertex(wn);
  if (!v)
    return TRUE;

  WN * kid = WN_kid1(wn);
  if (WN_operator(kid) != OPR_ARRAY)
    return TRUE;

  ACCESS_ARRAY * store = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);

  if (!store)
    return TRUE;

  WN * loop_st = Enclosing_Loop(wn);

  for (EINDEX16 e = adg->Get_Out_Edge(v); e; e = adg->Get_Next_Out_Edge(e)) {
    VINDEX16 sink = adg->Get_Sink(e);
    WN * load_wn = adg->Get_Wn(sink);
    OPERATOR opr = WN_operator(load_wn);
    if (OPERATOR_is_load(opr) && !OPERATOR_is_scalar_load(opr)) {
      kid = WN_kid0(load_wn);
      if (WN_operator(kid) != OPR_ARRAY)
	continue;

      if (Enclosing_Loop(load_wn) == loop_st)
	continue;

      ACCESS_ARRAY * load = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, kid);
      if (Equivalent_Access_Arrays(store,load,wn,load_wn)
	  && (DEPV_COMPUTE::Base_Test(wn,NULL,load_wn,NULL) == DEP_CONTINUE)) {
	return TRUE;
      }
    }
  }

  return FALSE;
}

// Query whether 'wn' is located in a loop nest that allows dead store removal
// after scalarization.
static BOOL Do_Sclrze_Dse(WN * wn)
{
  WN * wn_p = LWN_Get_Parent(wn);
  while (wn_p) {
    if (WN_operator(wn_p) == OPR_DO_LOOP) {
      DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn_p);
      if (dli && (dli->Sclrze_Dse == 1))
	return TRUE;
    }
    wn_p = LWN_Get_Parent(wn_p);
  }
  return FALSE;
}

// Delete 'store_wn'.
static void Delete_Store(WN * store_wn, ARRAY_DIRECTED_GRAPH16 * dep_graph)
{
  UINT32 limit = LNO_Sclrze_Dse_Limit;
  if ((limit > 0) && (DSE_Count > limit))
    return;
  
  LWN_Update_Dg_Delete_Tree(store_wn, dep_graph);
  LWN_Delete_Tree(store_wn);	   
  DSE_Count++;
}

// Process a store.
static void Process_Store(WN *store_wn, VINDEX16 v, 
		ARRAY_DIRECTED_GRAPH16 *dep_graph, BOOL do_variants,
		BOOL do_invariants, REDUCTION_MANAGER *red_manager)
{
#ifdef TARG_X8664
  // Do not sclrze vector stores.
  if (MTYPE_is_vector(WN_desc(store_wn))) return;
#endif
#ifdef KEY // Bug 6162 - can not scalarize to MTYPE_M pregs.
  if (WN_desc(store_wn) == MTYPE_M) return;
#endif
  if (Inside_Loop_With_Goto(store_wn)) return;
  INT debug = Get_Trace(TP_LNOPT,TT_LNO_SCLRZE);

  BOOL scalarized_this_store = FALSE;
  WN_OFFSET preg_num=0;
  ST *preg_st=0;
  WN *preg_store = NULL;

  ACCESS_ARRAY *store = 
	(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,WN_kid1(store_wn));

  if (debug) {
    fprintf(TFile,"Processing the store ");
    store->Print(TFile);
    fprintf(TFile,"\n");
  }

  if (Do_Aggressive_Fuse) {
    if (Is_Dead_Store(store_wn)) {
      if (!red_manager || (red_manager->Which_Reduction(store_wn) == RED_NONE)) {
	Delete_Store(store_wn, dep_graph);
	return;
      }
    }
  }

  BOOL is_invariant = Is_Invariant(store,store_wn);
  if (!do_variants && !is_invariant) {
    return;
  }
  if (!do_invariants && is_invariant) {
    return;
  }

  // Don't scalarize reductions as that will break the reduction
  if (red_manager && (red_manager->Which_Reduction(store_wn) != RED_NONE)) {
    return;
  }

  char preg_name[20];
  TYPE_ID store_type = WN_desc(store_wn);
  TYPE_ID type = Promote_Type(store_type);

  EINDEX16 e,next_e=0;
  BOOL all_loads_scalarized = TRUE;
  BOOL has_post_dom_kill = FALSE;

  if (Do_Aggressive_Fuse)
    has_post_dom_kill = Has_Post_Dom_Kill(dep_graph, store_wn);

  for (e = dep_graph->Get_Out_Edge(v); e; e=next_e) {
    next_e = dep_graph->Get_Next_Out_Edge(e);
    VINDEX16 sink = dep_graph->Get_Sink(e);
    WN *load_wn = dep_graph->Get_Wn(sink);
    OPCODE opcode = WN_opcode(load_wn);
    if (OPCODE_is_load(opcode)) {
      if (OPCODE_operator(opcode) != OPR_LDID && 
	  // Do not scalarize MTYPE_M loads as this may result in a parent MTYPE_M store
	  // having a child that is not MTYPE_M and function 'Add_def' may not be able to 
	  // handle such stores during coderep creation. The check here catches 'uses' involving 
	  // MTYPE_M whereas the check at the beginning of 'Process_Store' catches 'defs'.
	  (WN_rtype(load_wn) != MTYPE_M) && (WN_desc(load_wn) != MTYPE_M)) {
        ACCESS_ARRAY *load = (ACCESS_ARRAY *) 
	  WN_MAP_Get(LNO_Info_Map,WN_kid0(load_wn));
        if (WN_operator(WN_kid0(load_wn)) == OPR_ARRAY && 
	    Equivalent_Access_Arrays(store,load,store_wn,load_wn) &&
            (DEPV_COMPUTE::Base_Test(store_wn,NULL,load_wn,NULL) ==
                       DEP_CONTINUE) 
#ifdef KEY
              &&
            //Bug 9134: scalarizing only if store to and load from the same field
             WN_field_id(store_wn)==WN_field_id(load_wn)
#endif
                      ) {
	  BOOL scalarized_this_load = FALSE;	  
	  if (Dominates(store_wn,load_wn)) {
           if (!red_manager || 
	     (red_manager->Which_Reduction(store_wn) == RED_NONE)) {
	    if (!Intervening_Write(dep_graph->Depv_Array(e)->Max_Level(),
				v,sink,dep_graph)) {
             if (!MP_Problem(store_wn,load_wn)) {
	       scalarized_this_load = TRUE;
	      if (!scalarized_this_store) { 
                if (debug) {
                  fprintf(TFile,"Scalarizing the load ");
                  load->Print(TFile);
                  fprintf(TFile,"\n");
                }
	        // Create a new preg
	        preg_st = MTYPE_To_PREG(type);
	        char *array_name =
			ST_name(WN_st(WN_array_base(WN_kid1(store_wn))));
	        INT length = strlen(array_name);
	        if (length < 18) {
		  strcpy(preg_name,array_name);
		  preg_name[length] = '_';
		  preg_name[length+1] = '1';
		  preg_name[length+2] = 0;
#ifdef _NEW_SYMTAB
                  preg_num = Create_Preg(type,preg_name); 
                } else {
                  preg_num = Create_Preg(type, NULL); 
                }
#else
                  preg_num = Create_Preg(type,preg_name, NULL); 
                } else {
                  preg_num = Create_Preg(type, NULL, NULL); 
                }
#endif
	        // replace A[i] = x with "preg = x; A[i] = preg"
	        OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID,MTYPE_V,type);
		// Insert CVTL if necessary (854441)
		WN *wn_value = WN_kid0(store_wn);
		if (MTYPE_byte_size(store_type) < MTYPE_byte_size(type))
		  wn_value = LWN_Int_Type_Conversion(wn_value, store_type);
	        preg_store = LWN_CreateStid(preg_s_opcode,preg_num,
		     preg_st, Be_Type_Tbl(type),wn_value);
                WN_Set_Linenum(preg_store,WN_Get_Linenum(store_wn));
		LWN_Copy_Frequency_Tree(preg_store,store_wn);
                LWN_Insert_Block_Before(LWN_Get_Parent(store_wn),
						store_wn,preg_store);
	        OPCODE preg_l_opcode = OPCODE_make_op(OPR_LDID, type,type);
                WN *preg_load = WN_CreateLdid(preg_l_opcode,preg_num,
			preg_st, Be_Type_Tbl(type));
		LWN_Copy_Frequency(preg_load,store_wn);
	        WN_kid0(store_wn) = preg_load;
                LWN_Set_Parent(preg_load,store_wn);

	        Du_Mgr->Add_Def_Use(preg_store,preg_load);
	      }
	      scalarized_this_store = TRUE;

	      // replace the load with the use of the preg
	      WN *new_load = WN_CreateLdid(OPCODE_make_op(OPR_LDID,
		type,type),preg_num,preg_st,Be_Type_Tbl(type));
	      LWN_Copy_Frequency_Tree(new_load,load_wn);

              WN *parent = LWN_Get_Parent(load_wn);
	      for (INT i = 0; i < WN_kid_count(parent); i++) {
	        if (WN_kid(parent,i) == load_wn) {
	          WN_kid(parent,i) = new_load;
		  LWN_Set_Parent(new_load,parent);
		  LWN_Update_Dg_Delete_Tree(load_wn, dep_graph);
	          LWN_Delete_Tree(load_wn);
		  break;
                }
              }

	      // update def-use for scalar
	      Du_Mgr->Add_Def_Use(preg_store,new_load);
	     }
	    }
	   if (!scalarized_this_load) 
	     all_loads_scalarized = FALSE;
	   }
	  }
        }
      }
    }
  }

  if (Do_Aggressive_Fuse) {
    OPERATOR opr = WN_operator(store_wn);

    if (!OPERATOR_is_scalar_store(opr)
	&& scalarized_this_store && all_loads_scalarized) {
      WN * base = WN_array_base(WN_kid(store_wn,1));
      if (WN_has_sym(base)) {
	ST * st = WN_st(base);
	if (st) {
	  int val = Array_Use_Hash ? Array_Use_Hash->Find(st) : 0;
	
	  if (Is_Global_As_Local(st) && ST_Has_Dope_Vector(st)) {
	    if (Do_Sclrze_Dse(store_wn)
		&& !Live_On_Exit(store_wn)
		&& (ST_export(st) == EXPORT_LOCAL)) {
	      Delete_Store(store_wn, dep_graph);
	    }
	  }
	  else if (val && ((val & ADDR_TAKEN) == 0)
		   && ((val & IS_ALLOC) != 0)) {
	    if (has_post_dom_kill 
		|| ((val & HAS_ESCAPE_USE) == 0)) {
	      Delete_Store(store_wn, dep_graph); 
	    }
	  }
	}
      }
    }
  }

}

// Given that there is a must dependence between store and load,
// is there ary other store that might occur between the the store and the
// load
// If there exists a store2, whose maximum dependence level wrt the 
// load is greater than store's dependence level (INT level below), 
// then store2 is intervening.
//
// If there exists a store2 whose maximum dependence level is equal
// to store's, and there a dependence from store to store2 with dependence
// level >= the previous level, then store2 is intervening 
static BOOL Intervening_Write(INT level,VINDEX16 store_v, 
			VINDEX16 load_v,ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  EINDEX16 e;
  for (e=dep_graph->Get_In_Edge(load_v); e; e=dep_graph->Get_Next_In_Edge(e)) {
    INT level2 = dep_graph->Depv_Array(e)->Max_Level();
    if (level2 > level) { 
      return TRUE;
    } else if (level2 == level) {
      VINDEX16 store2_v = dep_graph->Get_Source(e);
      EINDEX16 store_store_edge = dep_graph->Get_Edge(store_v,store2_v);
      if (store_store_edge) {
	INT store_store_level = 
		dep_graph->Depv_Array(store_store_edge)->Max_Level();
        if (store_store_level >= level) {
	  return TRUE;
        }
      }
    }
  }
  return FALSE;
}

// Is this reference invariant in its inner loop
static BOOL Is_Invariant(ACCESS_ARRAY *store, WN *store_wn)
{
  // find the do loop info of the store
  WN *wn = LWN_Get_Parent(store_wn);
  while (WN_opcode(wn) != OPC_DO_LOOP) {
    wn = LWN_Get_Parent(wn);
  }
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
  INT depth = dli->Depth;
  if (store->Too_Messy || (store->Non_Const_Loops() > depth)) {
    return FALSE;
  }

  for (INT i=0; i<store->Num_Vec(); i++) {
    ACCESS_VECTOR *av = store->Dim(i);
    if (av->Too_Messy || av->Loop_Coeff(depth)) {
      return FALSE;
    }
  }
  return TRUE;
}

// Don't scalarize across parallel boundaries
static BOOL MP_Problem(WN *wn1, WN *wn2) 
{
  if (Contains_MP) {
    WN *mp1 = LWN_Get_Parent(wn1);
    while (mp1 && (!Is_Mp_Region(mp1)) &&
	   ((WN_opcode(mp1) != OPC_DO_LOOP) || !Do_Loop_Is_Mp(mp1))) {
      mp1 = LWN_Get_Parent(mp1);
    }
    WN *mp2 = LWN_Get_Parent(wn2);
    while (mp2 && (!Is_Mp_Region(mp2)) &&
            ((WN_opcode(mp2) != OPC_DO_LOOP) || !Do_Loop_Is_Mp(mp2))) {
      mp2 = LWN_Get_Parent(mp2);
    }
    if ((mp1 || mp2) && (mp1 != mp2)) return TRUE;
  }
  return FALSE;
}
