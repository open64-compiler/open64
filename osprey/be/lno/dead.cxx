/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


//                     Array Dead-Store and Partial Dead-Store Elimination
//                     --------------------------------------------------
//
// Description:
//
// 	In loops given
//	do i
//	  a[i] = ...
//	      ...
//	  a[i] = ...
//
//      dead store eliminate the first store
//
//      also, convert
//
//	do i
//	   a[i] = ...
//	   ...
//	   if (...)
//           a[i] = ...
//
//	into
//
//	do i
//	  t = ...
//	  ...
//	  if (...)
//	    a[i] = ...
//	  else 
//	     a[i] = t
//
/* ====================================================================
 * ====================================================================
 *
 * Module: dead.cxx
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Dead Store Elimination
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static const char *source_file = __FILE__;
static const char *rcs_id = "$Source: be/lno/SCCS/s.dead.cxx $ $Revision: 1.5 $";

#include <sys/types.h>
#include "lnopt_main.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "opt_du.h"
#include "dead.h"
#include "lnoutils.h"

static BOOL Dominates_and_Reverse_Postdominates(WN *wn1, WN *wn2);
static BOOL Intervening_Ref(INT, VINDEX16, VINDEX16 ,ARRAY_DIRECTED_GRAPH16 *);

// The following function is used to fix PV 654191. 

static BOOL Vertex_Is_In_Expr(WN* wn_tree, 
			      VINDEX16 vertex, 
			      ARRAY_DIRECTED_GRAPH16* dg) 
{
  if (dg->Get_Vertex(wn_tree) == vertex) 
    return TRUE; 
  for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
    if (Vertex_Is_In_Expr(WN_kid(wn_tree, i), vertex, dg))
      return TRUE; 
  return FALSE; 
} 
			      
void Dead_Store_Eliminate_Arrays(ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  if (Get_Trace(TP_LNOPT,TT_LNO_DEAD)) {
    fprintf(TFile,"Dead Store Eliminating Arrays\n");
  }

  // search for a store
  VINDEX16 v,next_v=0;
  for (v = dep_graph->Get_Vertex(); v; v = next_v) {
    next_v = dep_graph->Get_Next_Vertex(v);
    WN *wn = dep_graph->Get_Wn(v);
    OPCODE opcode = WN_opcode(wn);
    if (OPCODE_is_store(opcode) && (WN_kid_count(wn) == 2)) {
      WN *array = WN_kid1(wn);
      if (WN_operator(array) == OPR_ARRAY) {
        WN *base = WN_array_base(array);
        OPERATOR base_oper = WN_operator(base);
        if ((base_oper == OPR_LDID) || (base_oper == OPR_LDA)) {
	  while (next_v != 0 && Vertex_Is_In_Expr(wn, next_v, dep_graph))
	    next_v = dep_graph->Get_Next_Vertex(next_v);
          Process_Store(wn,v,dep_graph);
        }
      }
    }
  }
}


// Process a store
extern void Process_Store(WN *store_wn, VINDEX16 v, 
		ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  if (Inside_Loop_With_Goto(store_wn)) return;
  INT debug = Get_Trace(TP_LNOPT,TT_LNO_DEAD);

  WN_OFFSET preg_num=0;
  ST *preg_st=0;
  WN *preg_store = NULL;

  ACCESS_ARRAY *store = 
	(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,WN_kid1(store_wn));
#ifdef KEY //bug 5970: see explanation below
  if(!store) return;
#endif
  if (debug) {
    fprintf(TFile,"Processing the store ");
    store->Print(TFile);
    fprintf(TFile,"\n");
  }

  char preg_name[20];
  TYPE_ID type = WN_desc(store_wn);

  EINDEX16 e,next_e=0;
  for (e = dep_graph->Get_Out_Edge(v); e; e=next_e) {
    next_e = dep_graph->Get_Next_Out_Edge(e);
    VINDEX16 sink = dep_graph->Get_Sink(e);
    WN *store2_wn = dep_graph->Get_Wn(sink);
    OPCODE opcode = WN_opcode(store2_wn);
    if (OPCODE_is_store(opcode) && (store2_wn != store_wn)) {
      if (OPCODE_operator(opcode) != OPR_STID) {
        ACCESS_ARRAY *store2 = (ACCESS_ARRAY *) 
	  WN_MAP_Get(LNO_Info_Map,WN_kid1(store2_wn));
#ifdef KEY
	// Bug 5970 - some address expressions are not canonicalized.
	// In such cases, there may be a must dependency but the sink for
	// the dependence out edge will not have an entry in the access array.
	if (!store2) continue;
#endif	
        if (Equivalent_Access_Arrays(store,store2,store_wn,store2_wn) &&
	    (WN_offset(store_wn) == WN_offset(store2_wn)) &&
            (DEPV_COMPUTE::Base_Test(store_wn,NULL,store2_wn,NULL) ==
                       DEP_CONTINUE)) {
	  if (Dominates_and_Reverse_Postdominates(store_wn,store2_wn)) {
	    if (!Intervening_Ref(dep_graph->Depv_Array(e)->Max_Level(),
				v,sink,dep_graph)) {
              if (debug) {
                fprintf(TFile,"Removing the store");
                store->Print(TFile);
                fprintf(TFile,"\n");
              }
	      LWN_Delete_Tree(store_wn);
	      return;
            }
	  } else {
	    WN *grandparent = LWN_Get_Parent(LWN_Get_Parent(store2_wn));
	    if ((WN_opcode(grandparent) == OPC_IF) &&
		Dominates_and_Reverse_Postdominates(store_wn,grandparent)) {
	      if (!Intervening_Ref(dep_graph->Depv_Array(e)->Max_Level(),
				v,sink,dep_graph)) {
                if (debug) {
                  fprintf(TFile,"Partial dead store eliminating ");
                  store->Print(TFile);
                  fprintf(TFile,"\n");
		}
		// is store2 in the then or the else
		BOOL in_then = 
		  (LWN_Get_Parent(store2_wn) == WN_then(grandparent));

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

	        // replace A[i] = x with preg = x
	        OPCODE preg_s_opcode = OPCODE_make_op(OPR_STID,MTYPE_V,type);
	        preg_store = LWN_CreateStid(preg_s_opcode,preg_num,
                                preg_st, Be_Type_Tbl(type),WN_kid0(store_wn));
	        WN_Set_Linenum(preg_store,WN_Get_Linenum(store_wn));
	        LWN_Insert_Block_Before(LWN_Get_Parent(store_wn),
	           store_wn,preg_store);

                // now insert A[i] = preg into the opposite cluase of the if
	        OPCODE preg_l_opcode = OPCODE_make_op(OPR_LDID,
						Promote_Type(type),type);
	        WN *preg_load = WN_CreateLdid(preg_l_opcode,preg_num,
						preg_st, Be_Type_Tbl(type));
	        WN_kid0(store_wn) = preg_load;
		LWN_Set_Parent(preg_load,store_wn);
		LWN_Extract_From_Block(store_wn);
		if (in_then) {
	         LWN_Insert_Block_After(WN_else(grandparent),NULL,store_wn);
                } else {
	         LWN_Insert_Block_After(WN_then(grandparent),NULL,store_wn);
                }
		Du_Mgr->Add_Def_Use(preg_store,preg_load);

		return;
	      }
	    }
          }
        }
      }
    }
  }
}

// Does wn1 dominate wn2  and does wn2 postdominate wn1
// be conservative in that you can always say FALSE
// Wn1 and Wn2 must be statements
// we assume this is a good loop without any gotos
static BOOL Dominates_and_Reverse_Postdominates(WN *wn1, WN *wn2)
{
  Is_True(!OPCODE_is_expression(WN_opcode(wn1)),
    ("Non statement 1 in Dominates"));
  Is_True(!OPCODE_is_expression(WN_opcode(wn2)),
    ("Non statement 2 in Dominates"));

  // they must have the same parent
  if (LWN_Get_Parent(wn1) != LWN_Get_Parent(wn2)) {
    return FALSE;
  }

  // can we reach wn2 going forward from wn1 
  WN *tmp = WN_next(wn1);
  while (tmp && (tmp != wn2)) tmp = WN_next(tmp);
  return (tmp == wn2);
}


// Given that there is a must dependence between store1 and store2,
// is there ary other ref that might occur between the store1 and store2

// If there exists a ref, whose maximum dependence level wrt store2 
// is greater than store1's dependence level (INT level below), 
// then ref is intervening.
//
// If there exists a ref whose maximum dependence level is equal
// to store1's, and there a dependence from store1 to ref with dependence
// level >= the previous level, then ref is intervening 
static BOOL Intervening_Ref(INT level, VINDEX16 store1_v, 
	VINDEX16 store2_v,ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  EINDEX16 e;
  for (e=dep_graph->Get_In_Edge(store2_v);e;e=dep_graph->Get_Next_In_Edge(e)) {
    INT level2 = dep_graph->Depv_Array(e)->Max_Level();
    if (level2 > level) { 
      return TRUE;
    } else if (level2 == level) {
      VINDEX16 ref_v = dep_graph->Get_Source(e);
      EINDEX16 ref_store_edge = dep_graph->Get_Edge(store1_v,ref_v);
      if (ref_store_edge) {
	INT ref_store_level = 
		dep_graph->Depv_Array(ref_store_edge)->Max_Level();
        if (ref_store_level >= level) {
	  return TRUE;
        }
      }
    }
  }
  return FALSE;
}

