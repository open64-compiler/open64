/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "fusion.h"
#include "lwn_util.h"
#include "lnopt_main.h"
#include "errors.h"
#include "dep_graph.h"
#include "lnoutils.h"
#include "ff_utils.h"
#include "wn_simp.h"
#include "soe.h"
#include "cond.h"
#include "config_targ.h"
#include "opt_du.h"
#include "reduc.h"
#include "reverse.h"
#include "glob.h"
#include "fission.h"
#include "tlog.h"
#include "parallel.h"
#include "bitset.h"

#pragma weak New_Construct_Id 

typedef HASH_TABLE<INT,void *> INT2PTR;

// we will handle at most 16-nested loops

#define MAX_INT32 0x7fffffff
#define MIN_INT32 0x80000000

static BOOL fusion_initialized=FALSE;

static void fusion_verbose_info(
  SRCPOS	srcpos1,
  SRCPOS	srcpos2,
  UINT32	fusion_level,
  const char*	message)
{
  printf("#### Fusion(%d+%d:%d): %s\n",
    Srcpos_To_Line(srcpos1),
    Srcpos_To_Line(srcpos2),
    fusion_level, message);
}

static void fusion_analysis_info(
  BOOL		success,
  SRCPOS	srcpos1,
  SRCPOS	srcpos2,
  UINT32	fusion_level,
  const char*	message)
{

  if (success)
    fprintf(LNO_Analysis,"( LNO_Fusion_Success ");
  else
    fprintf(LNO_Analysis,"( LNO_Fusion_Failure ");

  fprintf(LNO_Analysis,"(%s %d) (%s %d) %d \"%s\" )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos1),
    Cur_PU_Name, Srcpos_To_Line(srcpos2),
    fusion_level, message);
}

static void fusion_tlog_info(
  FISSION_FUSION_STATUS     status,
  WN*		loop1,
  WN*		loop2,
  UINT32	fusion_level,
  const char*	message)
{
  char in_string[30];
  char out_string[30];
  SRCPOS srcpos1=WN_Get_Linenum(loop1);
  SRCPOS srcpos2=WN_Get_Linenum(loop2);

  sprintf(in_string,"%d %d %d",
          Srcpos_To_Line(srcpos1), Srcpos_To_Line(srcpos2), fusion_level);
  sprintf(out_string,"%d",status);
  Generate_Tlog("LNO","fusion", Srcpos_To_Line(srcpos1),
                ST_name(WN_st(WN_index(loop1))),
                in_string, out_string, message);
}


static void pre_peeling_verbose_info(
  SRCPOS	srcpos,
  UINT32	iter_count)
{
  printf("#### Pre_peeling(%d): for %d iteration(s)\n",
    Srcpos_To_Line(srcpos), iter_count);
}

static void pre_peeling_analysis_info(
  SRCPOS	srcpos,
  UINT32	iter_count)
{

  fprintf(LNO_Analysis,"( LNO_Pre_Peel ");

  fprintf(LNO_Analysis,"(%s %d) %d )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos), iter_count);
}

static void pre_peeling_tlog_info(
  WN*		loop,
  UINT32	iter_count)
{
  char in_string[30];
  SRCPOS srcpos=WN_Get_Linenum(loop);

  sprintf(in_string,"%d %d", Srcpos_To_Line(srcpos), iter_count);
  Generate_Tlog("LNO","pre_peeling", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(loop))),
                in_string, "", "");
}

static void post_peeling_verbose_info(
  SRCPOS	srcpos,
  UINT32	iter_count)
{
  printf("#### Post_peeling(%d): for %d iteration(s)\n",
    Srcpos_To_Line(srcpos), iter_count);
}

static void post_peeling_analysis_info(
  SRCPOS	srcpos,
  UINT32	iter_count)
{

  fprintf(LNO_Analysis,"( LNO_Post_Peel ");

  fprintf(LNO_Analysis,"(%s %d) %d )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos), iter_count);
}

static void post_peeling_tlog_info(
  WN*		loop,
  UINT32	iter_count)
{
  char in_string[30];
  SRCPOS srcpos=WN_Get_Linenum(loop);

  sprintf(in_string,"%d %d", Srcpos_To_Line(srcpos), iter_count);
  Generate_Tlog("LNO","post_peeling", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(loop))),
                in_string, "", "");
}

MEM_POOL FUSION_default_pool;   // pool used by fusion only

static UINT New_Name_Count=0;

// check if variable is live outside this loop.
static BOOL var_is_live_on_exit(WN * wn_def, WN * loop)
{
  USE_LIST *use_list=Du_Mgr->Du_Get_Use(wn_def);
  
  if (use_list->Incomplete())
    return TRUE;

  USE_LIST_ITER u_iter(use_list);
  for (DU_NODE *use_node=(DU_NODE *)u_iter.First(); !u_iter.Is_Empty();
       use_node=(DU_NODE *)u_iter.Next()) {
  
    WN* use=use_node->Wn();
    while (use) {
      if (use == loop)
	break;
      use = LWN_Get_Parent(use);
    }

    if (use != loop)
      return TRUE;	// loop index var is live outside the loop
  }

  return FALSE;

}

// TODO: to really check if loop index wn is live after this loop
static BOOL loop_var_is_live_on_exit(WN* loop) {

  WN* loop_start=WN_start(loop);
  USE_LIST *use_list=Du_Mgr->Du_Get_Use(loop_start);

  if (use_list->Incomplete())
   return TRUE;

  USE_LIST_ITER u_iter(use_list);
  for (DU_NODE *use_node=(DU_NODE *)u_iter.First(); !u_iter.Is_Empty();
       use_node=(DU_NODE *)u_iter.Next()) {
  
    WN* use=use_node->Wn();
    while (use && (use != loop) && (WN_opcode(use)!=OPC_FUNC_ENTRY))
      use=LWN_Get_Parent(use);
    if (use != loop)
      return TRUE;	// loop index var is live outside the loop
  }

  return FALSE;
}

// Get the only child loop inside the current simply-nested loop.
// Only immediate child loop will be returned, i.e., loop inside a IF
// structure would not be returned. Return NULL if there is no such
// loop or more than one loops are found.

extern WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok) {
  
  WN* wn1=WN_first(WN_do_body(wn));
  WN* first_loop=NULL;

  while (wn1) {
    OPCODE opc=WN_opcode(wn1);
    if (opc==OPC_DO_LOOP) {
      if (!first_loop)
	first_loop=wn1;
      else
	return NULL;
    } else if (opc==OPC_IF) {
      IF_INFO* ii=Get_If_Info(wn1);
      if (ii->Contains_Do_Loops || 
          (ii->Contains_Regions && regions_ok==FALSE)) {
	return NULL;
      }
    } else if (opc==OPC_DO_WHILE || opc==OPC_WHILE_DO) {
      return NULL;
    } else if (opc==OPC_REGION)
      if (regions_ok==FALSE)
        return NULL;
    wn1=WN_next (wn1);
  }
  return first_loop;

}

//----------------------------------------------------------------------
// NAME: Compare_Bounds
// FUNCTION:
//	Compare the lower (or upper) bounds of two loops
//	return 0 if the tree of the input bounds are identical
//	return -1 if the tree of the input bounds are different
// ARGUMENTS:
//	bound1 --	the tree of the first bound
//	index1 --	the loop index of the first bound
//	bound2 --	the tree of the second bound
//	index2 --	the loop index of the second bound
//----------------------------------------------------------------------

extern INT Compare_Bounds(
		WN* bound1,
		WN* index1,
		WN* bound2,
		WN* index2)
{

  OPCODE opc1=WN_opcode(bound1);
  OPCODE opc2=WN_opcode(bound2);
  if (opc1!=opc2)
    return -1;

  if (!WN_Equiv(bound1,bound2))
    if (OPCODE_has_sym(opc1)) {
      SYMBOL sym1(bound1);
      SYMBOL sym2(bound2);
      if (sym1!=sym2) {
        SYMBOL loop_sym1(index1);
        SYMBOL loop_sym2(index2);
        if (sym1!=loop_sym1 || sym2!=loop_sym2)
          return -1;
      } else
        return -1;
    } else
      return -1;

  for (INT kidno=0; kidno<WN_kid_count(bound1); kidno++) {
    if (Compare_Bounds(
	  WN_kid(bound1,kidno),index1,
    	  WN_kid(bound2,kidno),index2)!=0)
     return -1;
  }

  return 0;

}

// Check whether two loops have the same bounds.
BOOL Same_Bounds(WN * wn1, WN * wn2)
{
  if ((Compare_Bounds(WN_start(wn1), WN_index(wn1),
		     WN_start(wn2), WN_index(wn2)) == 0)
      && (Compare_Bounds(WN_end(wn1), WN_index(wn1),
			 WN_end(wn2), WN_index(wn2)) == 0))
    return TRUE;

  return FALSE;
}

// Computes the maximal dependence distance in iterations for level i (i is
// between 0 and max_dv_dim-1) between EVERY pair of references from 
// source_list and sink_list. 'step[]' contains the (absolute value of)
// stride in each loop level. So the result is the dependence distance
// in loop index values for each loop level with positive loop stride
// or the negation of the dependence distance in loop index values
// for each loop level with negative loop stride.  If 'dg' is non-NULL,
// build dependence graph. If 'vts' is non-NULL,  collect dynamic arrays of
// to-vertexes for edges having positive dependence distance.
//
// E.g.
//
// 	1	3	5 ...
//	sink
//			source
// has distance -4(=1-5) because this loop has positive stride. However,
// 	5	3	1 ...
//	sink
//			source
// has the same distance -4 because this loop has negative stride.

static mINT32* Max_Dep_Distance(
  REF_LIST_STACK *source_list, REF_LIST_STACK *sink_list,
  mUINT8 common_nest, mUINT8 max_dv_dim, INT32 step[],
  BOOL use_bounds, ARRAY_DIRECTED_GRAPH16 *dg, DYN_ARRAY<VINDEX16> ** vts,
  mINT32 * max_neg_dist
  )
{

  INT i;

  mINT32 *max_distance=CXX_NEW_ARRAY(mINT32, max_dv_dim, &FUSION_default_pool);

  for (i = 0; i< max_dv_dim; i++)  max_distance[i] = MIN_INT32;

  *max_neg_dist = 0;

  if (max_dv_dim>LNO_MAX_DO_LOOP_DEPTH) {
    Is_True(0, ("Loops nested too deep (>%d) in Max_Dep_Distance()\n",
      LNO_MAX_DO_LOOP_DEPTH));
    max_distance[max_dv_dim-1] = MAX_INT32;
    return max_distance;
  }

  DEP * saved_dep = CXX_NEW_ARRAY(DEP, max_dv_dim, &FUSION_default_pool);

  // for every pair of references from source_list and sink_list ..
  for (INT ii=0;ii<source_list->Elements(); ii++) {
    for (INT jj=0;jj<sink_list->Elements(); jj++) {
      ST *base1 = source_list->Bottom_nth(ii)->ST_Base;
      ST *base2 = sink_list->Bottom_nth(jj)->ST_Base;

      if (base1 && base2 && (base1 != base2))
        continue;
  
      REFERENCE_ITER iter1(source_list->Bottom_nth(ii));
      for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty();
        n1=iter1.Next()) {

        REDUCTION_TYPE red_type;
        if (red_manager)
          red_type=red_manager->Which_Reduction(LWN_Get_Parent(n1->Wn));
        else
          red_type=RED_NONE;

        REFERENCE_ITER iter2(sink_list->Bottom_nth(jj));
        for (REFERENCE_NODE *n2=iter2.First();!iter2.Is_Empty();
          n2=iter2.Next()) {

          if (red_type!=RED_NONE &&
              red_manager->Which_Reduction(LWN_Get_Parent(n2->Wn))==red_type)
            continue;

          MEM_POOL_Push(&FUSION_default_pool);

          mINT8 local_common_nest = 
	          (n1->Stack->Elements()<n2->Stack->Elements())?
	          n1->Stack->Elements() : n2->Stack->Elements();
          mINT8 dv_dim;
  
          if (local_common_nest>=common_nest) {
	    local_common_nest = common_nest;
	    dv_dim = max_dv_dim;
          } else
	    dv_dim = local_common_nest - (common_nest-max_dv_dim);
  
          // compute the dependences ..
          WN* src_wn=n1->Wn;
          WN* sink_wn=n2->Wn;
	  for (i = 0; i < max_dv_dim; i++) saved_dep[i] = 0;

          DEPV_LIST *tmp = CXX_NEW(DEPV_LIST(src_wn,sink_wn, local_common_nest,
            dv_dim,use_bounds,&FUSION_default_pool,n1->Stack,n2->Stack),
	    &FUSION_default_pool);

	  if (!tmp->Is_Empty()) {	// dependences exists

	    if (LNO_Test_Dump) {
	      // dump backward dependences for fusion
	      Dump_WN(src_wn, stdout, TRUE, 4, 4);
	      printf("-->");
	      Dump_WN(sink_wn, stdout, TRUE, 4, 4);
	      tmp->Print(stdout);
	      printf("\n");
	    }
  
	    DEPV_ITER dep_iter(tmp);
  
	    // for each dependence
	    for (DEPV_NODE *dn=dep_iter.First();
		 !dep_iter.Is_Empty(); dn=dep_iter.Next()) {
  
	      // for each dimension
	      for (i=0; i<dv_dim; i++) {
		DEP dep = DEPV_Dep(dn->Depv,i);

		if ( !DEP_IsDistance(dep) ) {
		  if (i==0) {
		    // a dep. direction '*' at outer-most loop,
		    // give up fusion
		    MEM_POOL_Pop(&FUSION_default_pool);
		    max_distance[max_dv_dim-1] = MAX_INT32;
		    return (max_distance);
		  } else {
		    // a '*' occurs at some inner loop
		    // find an outer loop whose offset distance
		    // can be used to guarantees that this '*' can be ignored
		    INT j;
		    for (j=i-1; j>=0; j--)
		      if (max_distance[j]<0)
			break;
		    if (j<0) {
		      max_distance[i-1]+=step[i-1];
		    } else {
		      // j-dimension has no constraint on distance
		      // so it has a MIN_INT32 value
		      max_distance[j]=step[j];
		    }
		    break;
		  }
		} else if (max_distance[i] > step[i]*DEP_Distance(dep)) {
		  saved_dep[i] = dep;
		  break;
		}
		// all depdence in inner loops are satisfied, too
		else if (max_distance[i] == step[i]*DEP_Distance(dep)) {
		  saved_dep[i] = dep;
		  continue;
		}
		else {
		  max_distance[i] = step[i]*DEP_Distance(dep);
		  saved_dep[i] = dep;
		}
	      }
	    }
	  }

          MEM_POOL_Pop(&FUSION_default_pool);

	  if (dg) {
	    for (i = 0; i < max_dv_dim; i++) {
	      DEP dep = saved_dep[i];
	      if (dep) {
		mINT32 distance = step[i] * DEP_Distance(dep);
		if (distance > 0) {
		  WN * wn_from = sink_wn;
		  WN * wn_to = src_wn;
		  VINDEX16 from = dg->Get_Vertex(wn_from);
		  if (!from)
		    from = dg->Add_Vertex(wn_from);
		  VINDEX16 to = dg->Get_Vertex(wn_to);
		  if (!to)
		    to = dg->Add_Vertex(wn_to);
		  if (!dg->Get_Edge(from, to))
		    dg->Add_Edge(from, to, dep);
		  if (vts) {
		    if (vts[i] == NULL)
		      vts[i] = CXX_NEW(DYN_ARRAY<VINDEX16>(&FUSION_default_pool),
				       &FUSION_default_pool);
		    FmtAssert(to > 0, ("Illegal to vertex."));
		    vts[i]->AddElement(to);
		  }
		}
		else if (distance < 0) {
		  *max_neg_dist = MAX(-1 * distance, *max_neg_dist);
		  // Clear ST_IS_GLOBAL_AS_LOCAL bit if negative distances exist
		  // among global-as-local symbol references so that store references
		  // won't be removed.
		  OPERATOR opr = WN_operator(sink_wn);
		  WN * array = OPERATOR_is_store(opr) ? WN_kid1(sink_wn) : WN_kid0(sink_wn);
		  WN * base = (WN_operator(array) == OPR_ARRAY) ? WN_array_base(array) : NULL;
		  if (base && WN_has_sym(base)) {
		    ST * st = WN_st(base);
		    if (ST_is_global_as_local(st))
		      Clear_ST_is_global_as_local(st);
		  }
		}
	      }
	    }
	  }
        }
      }
    }
  }
  return(max_distance);
}

// Check whether scalar-dependence inputs of 'wn' are outside of 'wn_loop'.
static BOOL
Scalar_Input_Is_Invariant(WN * wn, WN * wn_loop)
{
  for (int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_kid = WN_kid(wn, i);

    if (WN_operator(wn_kid) == OPR_ARRAY)
      continue;

    DEF_LIST * def_list = Du_Mgr->Ud_Get_Def(wn_kid);
    DEF_LIST_ITER iter(def_list);
    const DU_NODE * nnext = NULL;
    const DU_NODE * node = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
      nnext = iter.Next();
      WN * def = node->Wn();
      WN * wn_p = LWN_Get_Parent(def);
      while (wn_p) {
	if (wn_p == wn_loop)
	  return FALSE;
	wn_p = LWN_Get_Parent(wn_p);
      }
    }
    
    if (!Scalar_Input_Is_Invariant(wn_kid, wn_loop))
      return FALSE;
  }

  return TRUE;
}

#define PRECOM_INTERNAL 1  // Internal nodes in a pre-computation tree.
#define PRECOM_ROOT 2  // Root of a pre-computation tree.
#define PRECOM_IS_DEF(x)  ((x == PRECOM_INTERNAL) || (x == PRECOM_ROOT))

// Check whether array-dependence inputs of 'wn' have the same parent and the parent is
// 'wn_parent'. If so, annotate inputs in 'map' and return number of 
// unique inputs.
static BOOL
Array_Input_Has_Same_Parent(WN * wn, WN_MAP map, WN * wn_parent, int * input_cnt)
{
  for (int i = 0; i < WN_kid_count(wn); i++) {
    WN * wn_kid = WN_kid(wn, i);
    VINDEX16 v = Array_Dependence_Graph->Get_Vertex(wn_kid);
    if (v) {
      for (EINDEX16 in_edge=Array_Dependence_Graph->Get_In_Edge(v); 
	   in_edge; in_edge=Array_Dependence_Graph->Get_Next_In_Edge(in_edge)) {
	VINDEX16 v_src = Array_Dependence_Graph->Get_Source(in_edge);
	WN * wn_src = Array_Dependence_Graph->Get_Wn(v_src);
	if (map) {
	  UINT val = (UINT) (INTPTR) WN_MAP_Get(map, wn_src);
	  if (!val) {
	    WN_MAP_Set(map, wn_src, (void *) (INTPTR) (UINT) PRECOM_INTERNAL);
	    (*input_cnt)++;
	  }
	}
	
	if (LWN_Get_Parent(wn_src) != wn_parent)
	  return FALSE;
	else if (!Array_Input_Has_Same_Parent(wn_src, map, wn_parent, input_cnt))
	  return FALSE;
      }
    }

    if (!Array_Input_Has_Same_Parent(wn_kid, map, wn_parent, input_cnt))
      return FALSE;
  }
  return TRUE;
}

// Computes the maximal dependence distance for level i (i is
// between 0 and dv_dim-1) if the step of level i is positive,
// or the negative value of the minimal dependence distance
// if the step of level i is negative, between EVERY pair of 
// 1) WRITE in in_loop1 and WRITE in_loop2
// 2) READ in in_loop1 and WRITE in_loop2
// 3) WRITE in in_loop1 and READ in_loop2
// Return a dependence graph 'dg' for data dependences between two loops.
// Return an array of vertexes 'vst' in the dependence graph whose value
// can be pre-computed.
static mINT32* Max_Dep_Distance(
  WN *in_loop1, WN *in_loop2,
  mUINT8 dv_dim, INT32 step[], BOOL use_bounds,
  ARRAY_DIRECTED_GRAPH16 *dg, DYN_ARRAY<VINDEX16> ** vts, mINT32 * max_neg_dist)
{
    mINT8 i;
    WN * wn1 = in_loop1;
    WN * wn2 = in_loop2;

    DO_LOOP_INFO * dli1 = Get_Do_Loop_Info(wn1);
    DO_LOOP_INFO * dli2 = Get_Do_Loop_Info(wn2);

    WN* loop_body1=WN_do_body(wn1);	// get the loop bodies (blocks)
    WN* loop_body2=WN_do_body(wn2);

    mINT32 *max_distance = CXX_NEW_ARRAY(mINT32, dv_dim, &FUSION_default_pool);
    for (i = 0; i< dv_dim; i++) max_distance[i] = MIN_INT32;
  
    REF_LIST_STACK *writes1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(in_loop1, stack1);

    REF_LIST_STACK *writes2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_REF_STACK *params1 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    SCALAR_REF_STACK *params2 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    DOLOOP_STACK *stack2=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(in_loop2, stack2);

    // Make a list of all the references
    INT32 status = 0;
    Init_Ref_Stmt_Counter();
    WN* wn = 0;
    for (wn=WN_first(loop_body1); wn && status!= -1; wn=WN_next(wn)) {
      status=New_Gather_References(wn,writes1,reads1,stack1,
        scalar_writes1,scalar_reads1,
        params1,&FUSION_default_pool);
    }
    if (status == -1) {
      max_distance[dv_dim-1] = MAX_INT32;
      return max_distance;
    }

    // Make a list of all the references
    for (wn=WN_first(loop_body2); wn && status!= -1; wn=WN_next(wn)) {
      status=New_Gather_References(wn,writes2,reads2,stack2,
        scalar_writes2,scalar_reads2,
        params2,&FUSION_default_pool);
    }
    if (status == -1) {
      max_distance[dv_dim-1] = MAX_INT32;
      return max_distance;
    }

    mINT32 *tmp;
    mUINT8 common_nest =
      ((DO_LOOP_INFO*)WN_MAP_Get(LNO_Info_Map,in_loop1))->Depth+dv_dim;

    BOOL abort=FALSE;

    // compute max dependence distance between writes in in_loop2
    // and writes in in_loop1
    tmp = Max_Dep_Distance(writes2, writes1,common_nest, 
			   dv_dim, step, use_bounds, NULL, NULL, max_neg_dist);

    for (i = 0; i< dv_dim; i++) {
      if (tmp[i] > max_distance[i]) max_distance[i] = tmp[i];
      // if non-constant dependences or unknown dependences
      if (tmp[i] == MAX_INT32) 
	abort = TRUE;
    }
    if (abort) {
      max_distance[dv_dim-1] = MAX_INT32;
      return max_distance;
    }

    // compute max dependence distance between writes in in_loop2
    // and reads in in_loop1
    tmp = Max_Dep_Distance(writes2, reads1, common_nest, 
			   dv_dim, step, use_bounds, NULL, NULL, max_neg_dist);
    for (i = 0; i< dv_dim; i++) {
      if (tmp[i] > max_distance[i]) max_distance[i] = tmp[i];
      if (tmp[i] == MAX_INT32) 
	abort = TRUE;
    }
    if (abort) {
      max_distance[dv_dim-1] = MAX_INT32; 
      return max_distance;
    }

    // compute max dependence distance between reads in in_loop2
    // and writes in in_loop1

    tmp = Max_Dep_Distance(reads2, writes1, common_nest,
			   dv_dim, step, use_bounds, dg, vts, max_neg_dist);
    for (i = 0; i< dv_dim; i++) {
      if (tmp[i] > max_distance[i]) max_distance[i] = tmp[i];
      if (tmp[i] == MAX_INT32) 
	abort = TRUE;
    }

    if (abort) {
      max_distance[dv_dim-1] = MAX_INT32;
      if (dg) {
	dg->Erase_Graph();
	for (i = 0; i < dv_dim; i++) {
	  DYN_ARRAY<VINDEX16> * v_array = vts[i];
	  if (v_array) {
	    CXX_DELETE(v_array, &FUSION_default_pool);
	    vts[i] = NULL;
	  }
	}
      }
      return max_distance;
    }
    else if (vts) {
      // Legality check for pre-computation.
      int input_cnt = 0;
      WN_MAP input_map = WN_MAP_Create(&FUSION_default_pool);

      for (int i = 0; i < dv_dim; i++) {
	DYN_ARRAY<VINDEX16> * v_array = vts[i];
	if (v_array && (step[i] == 1)) {
	  DYN_ARRAY<VINDEX16> * defs = 
	    CXX_NEW(DYN_ARRAY<VINDEX16>(&FUSION_default_pool),
		    &FUSION_default_pool);
  
	  for (int j = 0; j < v_array->Elements(); j++) {
	    VINDEX16 v = (*v_array)[j];
	    EINDEX16 e = dg->Get_In_Edge(v);
	    DEP dep = dg->Dep(e);
	    VINDEX16 v_def = dg->Get_Source(e);
	    WN * wn_from = dg->Get_Wn(v_def);
	    WN * wn_to = dg->Get_Wn(v);
	    int count = 0;

	    // A use should have single definition and contained in a store statement.
	    while (e) {
	      count++;
	      e = dg->Get_Next_In_Edge(e);
	    }

	    if ((count > 1) 
		|| !Find_Containing_Store(wn_to)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    FmtAssert(WN_operator(wn_from) == OPR_ISTORE, ("Illegal source."));
	    FmtAssert(WN_operator(wn_to) == OPR_ILOAD, ("Illegal sink."));

	    // The definition is at a dominating point in in_loop1s body,
	    // and the definition's enclosing loops are perfectly-nested
	    // Do-loops.
	    WN * wn_parent = LWN_Get_Parent(wn_from);
	    WN * last_loop = NULL;
	    while (wn_parent && (wn_parent != in_loop1)) {
	      switch (WN_operator(wn_parent)) {
	      case OPR_BLOCK:
		wn_parent = LWN_Get_Parent(wn_parent);
		break;
	      case OPR_DO_LOOP:
		if (last_loop 
		    && (Get_Only_Loop_Inside(wn_parent,FALSE) != last_loop)) {
		  wn_parent = NULL;
		}
		else {
		  last_loop = wn_parent;
		  wn_parent = LWN_Get_Parent(wn_parent);
		}
		break;
	      default:
		wn_parent = NULL;
		;
	      }
	    }

	    if (wn_parent != in_loop1) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // The def and the use should be in a do-loop nest with the same depth.
	    if (Loop_Depth(wn_to) != Loop_Depth(wn_from)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // Inputs of scalar dependence should have definitions outside of the loop.
	    if (!Scalar_Input_Is_Invariant(wn_from, in_loop1)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // Inputs of array dependence should have the same parent as wn_from.
	    if (!Array_Input_Has_Same_Parent(wn_from, input_map, 
					     LWN_Get_Parent(wn_from), &input_cnt)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // Compare def and use's ACCESS_VECTORs, they should have no dependence 
	    // on all but one dimensions.  For the dimension having dependence,
	    // loop coefficient should be 1 for the outermost loop, and difference
	    // of dependence vectors should be a constant.
	    // For example, if the loop nest is (j,k,i)
	    // A pair of def and use should have a pattern like: 
	    // [i][j][k] and [i][j+1][k].
	    // where, no dependence exists in the first and the last dimension,
	    // for the second dimension, loop coefficient is 1 for the outermost loop.
	    // the different between 'j+1' and 'j' is a constant.
	    
	    WN * addr_from = WN_kid(wn_from, 1);
	    WN * addr_to = WN_kid(wn_to, 0);
	    ACCESS_ARRAY * array_from = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, addr_from);
	    ACCESS_ARRAY * array_to = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, addr_to);

	    FmtAssert(array_from && array_to, ("Missing ACCESS_ARRAY."));
	    int count0 = 0;
	    int count1 = 0;
	    BOOL abort = FALSE;
	    FmtAssert(array_from->Num_Vec() == array_to->Num_Vec(), ("Unmatched dimensions."));

	    for (int iter = 0; iter < array_from->Num_Vec(); iter++) {
	      ACCESS_VECTOR * to_vec = array_to->Dim(iter);

	      // Skip those having non linear terms and variant symbolics.
	      if (to_vec->Contains_Non_Lin_Symb()
		  || (to_vec->Non_Const_Loops() != 0)) {
		abort = TRUE;
		break;
	      }
		
	      if ((*array_from->Dim(iter)) == (*array_to->Dim(iter)))
		continue;
	      count0++;

	      ACCESS_VECTOR * from_vec = array_from->Dim(iter);
	      ACCESS_VECTOR * diff = Subtract(to_vec, from_vec, &FUSION_default_pool);
	      if (diff && diff->Is_Const() && (diff->Const_Offset == DEP_Distance(dep))
		  && (from_vec->Loop_Coeff(0) == 1)
		  && (from_vec->Const_Offset == 0)) {
		count1++;
		CXX_DELETE(diff, &FUSION_default_pool);
	      }
	      else {
		count1 = 0;
		if (diff)
		  CXX_DELETE(diff, &FUSION_default_pool);
		break;
	      }
	    }
	    
	    if (abort || (count0 != count1)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // Use has no array dependence in its nesting loop.
	    VINDEX16 v_adep = Array_Dependence_Graph->Get_Vertex(wn_to);
	    if (v_adep && Array_Dependence_Graph->Get_In_Edge(v_adep)) {
	      CXX_DELETE(v_array, &FUSION_default_pool);
	      vts[i] = NULL;
	      break;
	    }

	    // Def's access array must be equal to those of its array dependence uses 
	    // in its nesting loop.
	    v_adep = Array_Dependence_Graph->Get_Vertex(wn_from);
	    if (v_adep) {
	      BOOL use_satisfy = TRUE;
	      for (EINDEX16 edge = Array_Dependence_Graph->Get_Out_Edge(v_adep); edge;
		   edge = Array_Dependence_Graph->Get_Next_Out_Edge(edge)) {
		VINDEX16 v_cur = Array_Dependence_Graph->Get_Sink(edge);
		WN * wn_cur = Array_Dependence_Graph->Get_Wn(v_cur);

		if (WN_operator(wn_cur) == OPR_ILOAD) {
		  ACCESS_ARRAY * array_cur = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,
									 WN_kid(wn_cur, 0));
		  if (*array_from == *array_cur) 
		    ;
		  else {
		    use_satisfy = FALSE;
		    break;
		  }
		}
	      }
	      if (!use_satisfy) {
		CXX_DELETE(v_array, &FUSION_default_pool);
		vts[i] = NULL;
		break;
	      }
	    }

	    BOOL seen = FALSE;
	    for (int k = 0; k < defs->Elements(); k++) {
	      VINDEX16 v_cur =  (*defs)[k];
	      if (v_cur == v_def) {
		seen = TRUE;
		break;
	      }
	    }
	    if (!seen) 
	      defs->AddElement(v_def);
	  }

	  CXX_DELETE(defs, &FUSION_default_pool);
	}
	else {
	  CXX_DELETE(v_array, &FUSION_default_pool);
	  vts[i] = NULL;
	}
      }
      
      // Estimate cost and benefit of pre-computation, invalidate returned value
      // if not worth it.
      DO_LOOP_INFO * dli1 = Get_Do_Loop_Info(in_loop1);
      if (dli1 && (dli1->Prefer_Fuse == 1)
	  && (input_cnt > LNO_Fusion_Ddep_Limit)) {
	for (int i = 0; i < dv_dim; i++) {
	  if (vts[i])
	    CXX_DELETE(vts[i], &FUSION_default_pool);
	  vts[i] = NULL;
	}
      }

      WN_MAP_Delete(input_map);
    }
    
    return max_distance;
}

static WN* Scalar_Dependence_Prevent_Fusion(WN* in_loop1, WN* in_loop2)
{
    MEM_POOL_Push(&FUSION_default_pool);

    REF_LIST_STACK *writes1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(in_loop1, stack1);

    REF_LIST_STACK *writes2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_REF_STACK *params1 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    SCALAR_REF_STACK *params2 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    DOLOOP_STACK *stack2=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(in_loop2, stack2);

    Init_Ref_Stmt_Counter();
    New_Gather_References(in_loop1,writes1,reads1,stack1,
      scalar_writes1,scalar_reads1, params1,&FUSION_default_pool,
      Gather_Scalar_Refs | Gather_Params);

    New_Gather_References(in_loop2,writes2,reads2,stack2,
      scalar_writes2,scalar_reads2, params2,&FUSION_default_pool,
      Gather_Scalar_Refs | Gather_Params);

    INT si;
    for (si=0; si<scalar_writes1->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_writes1->Bottom_nth(si);
      WN* write1=sinode->Bottom_nth(0)->Wn;
      INT sj;
      for (sj=0; sj<scalar_writes2->Elements(); sj++) {
        SCALAR_NODE* sjnode=scalar_writes2->Bottom_nth(sj);
        WN* write2=sjnode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,write1,write2)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write1=sinode->Bottom_nth(sii)->Wn;
            REDUCTION_TYPE red_type;
            if (red_manager)
              red_type=red_manager->Which_Reduction(write1);
            else
              red_type=RED_NONE;
            for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
              write2=sjnode->Bottom_nth(sjj)->Wn;
              if (red_type==RED_NONE ||
                  red_manager->Which_Reduction(write2)!=red_type) {
                MEM_POOL_Pop(&FUSION_default_pool);
                return write1;
              }
            }
          }
        }
      }
      for (sj=0; sj<scalar_reads2->Elements(); sj++) {
        SCALAR_NODE* sjnode=scalar_reads2->Bottom_nth(sj);
        WN* read2=sjnode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,write1,read2)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write1=sinode->Bottom_nth(sii)->Wn;
            REDUCTION_TYPE red_type;
            if (red_manager)
              red_type=red_manager->Which_Reduction(write1);
            else
              red_type=RED_NONE;
            for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
              read2=sjnode->Bottom_nth(sjj)->Wn;
              if (red_type==RED_NONE ||
                  red_manager->Which_Reduction(read2)!=red_type) {
                MEM_POOL_Pop(&FUSION_default_pool);
                return write1;
              }
            }
          }
        }
      }
      for (sj=0; sj<params2->Elements(); sj++) {
        SCALAR_REF sjnode=params2->Bottom_nth(sj);
        WN* param=sjnode.Wn;
        //   k= ..
        //    = call(&k)        <= aliased
        //   and
        //   k= ..
        //    = call(&m)        <= not aliased
        if (Aliased(Alias_Mgr,param,write1)!=NOT_ALIASED) {
          MEM_POOL_Pop(&FUSION_default_pool);
          return write1;
        }
      }
    }
    for (si=0; si<scalar_reads1->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_reads1->Bottom_nth(si);
      WN* read1=sinode->Bottom_nth(0)->Wn;
      for (INT sj=0; sj<params2->Elements(); sj++) {
        SCALAR_REF sjnode=params2->Bottom_nth(sj);
        WN* param=sjnode.Wn;
        //    = .. k
        //    = call(&k)        <= aliased
        //   and
        //    = .. k
        //    = call(&m)        <= not aliased
        if (Aliased(Alias_Mgr,param,read1)!=NOT_ALIASED) {
          MEM_POOL_Pop(&FUSION_default_pool);
          return read1;
        }
      }
    }

    for (si=0; si<scalar_writes2->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_writes2->Bottom_nth(si);
      WN* write2=sinode->Bottom_nth(0)->Wn;
      INT sj;
      for (sj=0; sj<scalar_reads1->Elements(); sj++) {
        SCALAR_NODE* sjnode=scalar_reads1->Bottom_nth(sj);
        WN* read1=sjnode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,write2,read1)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write2=sinode->Bottom_nth(sii)->Wn;
            REDUCTION_TYPE red_type;
            if (red_manager)
              red_type=red_manager->Which_Reduction(write2);
            else
              red_type=RED_NONE;
            for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
              read1=sjnode->Bottom_nth(sjj)->Wn;
              if (red_type==RED_NONE ||
                  red_manager->Which_Reduction(read1)!=red_type) {
                MEM_POOL_Pop(&FUSION_default_pool);
                return write2;
              }
            }
          }
        }
      }
      for (sj=0; sj<params1->Elements(); sj++) {
        SCALAR_REF sjnode=params1->Bottom_nth(sj);
        WN* param=sjnode.Wn;
        //   k= ..
        //    = call(&k)        <= aliased
        //   and
        //   k= ..
        //    = call(&m)        <= not aliased
        if (Aliased(Alias_Mgr,param,write2)!=NOT_ALIASED) {
          MEM_POOL_Pop(&FUSION_default_pool);
          return write2;
        }
      }
    }
    for (si=0; si<scalar_reads2->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_reads2->Bottom_nth(si);
      WN* read2=sinode->Bottom_nth(0)->Wn;
      for (INT sj=0; sj<params1->Elements(); sj++) {
        SCALAR_REF sjnode=params1->Bottom_nth(sj);
        WN* param=sjnode.Wn;
        //    = .. k
        //    = call(&k)        <= aliased
        //   and
        //    = .. k
        //    = call(&m)        <= not aliased
        if (Aliased(Alias_Mgr,param,read2)!=NOT_ALIASED) {
          MEM_POOL_Pop(&FUSION_default_pool);
          return read2;
        }
      }
    }

    MEM_POOL_Pop(&FUSION_default_pool);

    return NULL;

}

// Delete dependence graph and related data structures.
static void Delete_Dep_Graph
(ARRAY_DIRECTED_GRAPH16 *dg, DYN_ARRAY<VINDEX16> ** vts, int fusion_level)
{
  if (dg) {
    CXX_DELETE(dg, &FUSION_default_pool);
    dg->Delete_Map();
    if (vts) {
      for (int i = 0; i < fusion_level; i++) {
	DYN_ARRAY<VINDEX16> * v_array = vts[i];
	if (v_array) {
	  CXX_DELETE(v_array, &FUSION_default_pool);
	  vts[i] = NULL;
	}
      }
      CXX_DELETE(vts, &FUSION_default_pool);
    }
  }
}

// Given a comparison expression `wn_compare', check whether the condition can be
// evaluated to 'cval', where 'cval' is a TRUE or FALSE, (e.g., an equal comparison
// with a value outside of the loop bounds can be evaluated to a FALSE).  
// If 'prolog_out' or 'epilog_out' is non-NULL, check whether loop peeling can be 
// used to make the evaluation possible, and return number of peeling iterations 
// in 'prolog_out' and 'epilog_out'. 
// TODO: implementation to be completed.
static BOOL
Removable_branch(WN * loop, WN * wn_compare, int * prolog_out, int * epilog_out, BOOL cval)
{
  WN * lower_bound;
  WN * upper_bound;
  WN * wn_step;
  OPCODE ub_compare;
  WN * kid0;
  WN * kid1;

  switch (WN_operator(wn_compare)) {
  case OPR_CIOR: 
    if (!cval) {
      if (Removable_branch(loop, WN_kid(wn_compare, 0), prolog_out, epilog_out, cval)
	  && Removable_branch(loop, WN_kid(wn_compare, 1), prolog_out, epilog_out, cval))
	return TRUE;
    }
    else {
      if (Removable_branch(loop, WN_kid(wn_compare, 0), prolog_out, epilog_out, cval)
	  || Removable_branch(loop, WN_kid(wn_compare, 1), prolog_out, epilog_out, cval))
	return TRUE;
    }

    break;
  case OPR_EQ:
    kid0 = WN_kid(wn_compare, 0);
    kid1 = WN_kid(wn_compare, 1);

    if (WN_Simp_Compare_Trees(kid0, kid1) == 0) {
      if (!cval)
	return FALSE;
      else
	return TRUE;
    }
    else {
      WN * wn_const = (WN_operator(kid1) == OPR_INTCONST) ? kid1 : 
	((WN_operator(kid0) == OPR_INTCONST) ? kid0 : NULL);
      if (wn_const) {
	WN * wn_ref = (wn_const == kid0) ? kid1 : kid0;
	switch (WN_operator(wn_ref)) {
	case OPR_ADD:
	  kid0 = WN_kid(wn_ref, 0);
	  kid1 = WN_kid(wn_ref, 1);
	  if ((WN_operator(kid0) == OPR_INTCONST)
	      && (WN_operator(kid1) == OPR_INTCONST)
	      && ((WN_const_val(kid0) + WN_const_val(kid1)) == WN_const_val(wn_const))) {
	    if (!cval)
	      return FALSE;
	    else
	      return TRUE;
	  }
	  break;
	case OPR_INTCONST:
	  if (!cval)
	    return (WN_const_val(wn_const) != WN_const_val(wn_ref));
	  else
	    return (WN_const_val(wn_const) == WN_const_val(wn_ref));
	  break;

	default:
	  ;
	}
      }
    }

    if (loop) {
      lower_bound = WN_LOOP_LowerBound(loop);
      upper_bound = WN_LOOP_UpperBound(loop, &ub_compare, TRUE);
      wn_step = WN_step(loop);
      wn_step = wn_step ? WN_kid(wn_step, 0) : NULL;
      wn_step = wn_step ? WN_kid(wn_step, 0) : NULL;

      if (wn_step && (WN_Simp_Compare_Trees(WN_kid(wn_compare,0), wn_step) == 0)) {
	WN * wn_kid = WN_kid(wn_compare, 1);
	if (WN_Simp_Compare_Trees(wn_kid, lower_bound) == 0) {
	  if (!cval) {
	    // Pre-peeling one iteration can make the condition expression FALSE.
	    if (prolog_out) {
	      *prolog_out = 1;
	      return TRUE;
	    }
	  }
	}
	else if (WN_Simp_Compare_Trees(wn_kid, upper_bound) == 0) {
	  // Post-peeling one iteration can make the condition expression FALSE.
	  if (!cval) {
	    if (epilog_out) {
	      *epilog_out = 1;
	      return TRUE;
	    }
	  }
	}
	else {
	  // An equal comparison to a value outside the loop bound can be evaluated to
	  // a 'FALSE'.
	  int diff = 0;
	  if (!cval) {
	    if (WN_has_const_diff(wn_kid, lower_bound, &diff) && (diff < 0)) 
	      return TRUE;

	    if (WN_has_const_diff(wn_kid, upper_bound, &diff)) {
	      if  (diff > 0)
		return TRUE;

	      OPERATOR opr = OPCODE_operator(ub_compare);
	      BOOL is_incr;
	      WN * wn_incr = WN_LOOP_Increment(loop, &is_incr);
	      if ((opr == OPR_LT) && (diff == -1) && is_incr 
		  && (WN_operator(wn_incr) == OPR_INTCONST)
		  && (WN_const_val(wn_incr) == 1)) {
		*epilog_out = 1;
		return TRUE;
	      }
	    }
	  }
	}
      }
    }
    break;
  default:
    ;
  }

  return FALSE;
}

// Given a 'loop', find a condition branch in the loop that can be removed by loop peeling.
static WN *
Removable_branch_loop_peeling(WN * loop, ARRAY_DIRECTED_GRAPH16 * dg, DYN_ARRAY<VINDEX16> * use, int * prolog_out, int * epilog_out)
{
  *prolog_out = 0;
  *epilog_out = 0;
  
  for (int i = 0; i < use->Elements(); i++) {
    VINDEX16 v = (*use)[i];
    WN * wn = dg->Get_Wn(v);
    WN * wn_p = LWN_Get_Parent(wn);
    while (wn_p && (wn_p != loop)) {
      if (WN_operator(wn_p) == OPR_IF) {
	if (Removable_branch(loop, WN_if_test(wn_p), prolog_out, epilog_out, FALSE)
	    && (*prolog_out || *epilog_out))
	  return wn_p;
      }
      wn_p = LWN_Get_Parent(wn_p);
    }
  }

  return NULL;
}

// Check whether all condition branches in the WHILE tree 'wn' that can be removed by pre/post
// peeling of its enclosing loop.  Return the enclosing loop that peeling is applied to
// in 'loop', return peeling iterations in 'prolog' and 'epilog'. 
BOOL Check_Removable_Branch(WN * wn, int *prolog, int *epilog, WN ** loop)
{
  OPERATOR opr = WN_operator(wn);
  WN * wn_ret = NULL;
  
  if (opr == OPR_BLOCK) {
    for (WN * wn_iter = WN_first(wn); wn_iter; wn_iter = WN_next(wn_iter)) {
      if (!Check_Removable_Branch(wn_iter, prolog, epilog, loop)) {
	*loop = NULL;
	return FALSE;
      }
    }
  }
  else if (opr == OPR_IF) {
    WN * wn_p = LWN_Get_Parent(wn);
    while (wn_p) {
      if (WN_operator(wn_p) == OPR_DO_LOOP) {
	int cur_prolog = 0;
	int cur_epilog = 0;
	if (Removable_branch(wn_p, WN_if_test(wn), &cur_prolog, &cur_epilog, FALSE)) {
	  WN * wn_tmp = *loop;
	  if ((wn_tmp == NULL)
	      || ((wn_tmp == wn_p) 
		  && (cur_prolog == *prolog) 
		  && (cur_epilog == *epilog))) {
	    *loop = wn_p;
	    *prolog = cur_prolog;
	    *epilog = cur_epilog;
	    return TRUE;
	  }
	}
      }
      wn_p = LWN_Get_Parent(wn_p);
    }
    *loop = NULL;
    return FALSE;
  }
  
  for (int i = 0; i < WN_kid_count(wn); i++) {
    if (!Check_Removable_Branch(WN_kid(wn,i), prolog, epilog, loop)) {
      *loop = NULL;
      return FALSE;
    }
  }

  return TRUE;
}

// For every array reference in 'wn', if it has a loop coefficient,
// check whether its loop coefficient is 1 and its constant offset is 0.
// e.g, it has a pattern like: [i][j][k], [i][j][k][0] and etc.
BOOL Has_Unit_Refs(WN * wn, int dim)
{
  OPERATOR opr = WN_operator(wn);
  WN * array = NULL;

  if (opr == OPR_BLOCK) {
    for (WN * wn_iter = WN_first(wn); wn_iter; wn_iter = WN_next(wn_iter)) {
      if (!Has_Unit_Refs(wn_iter, dim))
	return FALSE;
    }
  }
  else if (opr == OPR_ARRAY)
    array = wn;
  
  if (array && (WN_operator(array) == OPR_ARRAY)) {
    ACCESS_ARRAY * access = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, array);
    if (access && (access->Num_Vec() > dim)) {
      ACCESS_VECTOR * vec = access->Dim(dim);
      if (vec) {
	if (vec->Contains_Non_Lin_Symb())
	  return FALSE;
	int coeff = vec->Loop_Coeff(dim);
	if (coeff && ((coeff != 1) || (vec->Const_Offset != 0)))
	  return FALSE;
      }
    }
  }

  for (int i = 0; i < WN_kid_count(wn); i++) {
    if (!Has_Unit_Refs(WN_kid(wn, i), dim))
      return FALSE;
  }
  return TRUE;
}

// Check whether 'wn' is a loop start or loop increment.
static BOOL Is_Loop_Index(WN * wn)
{
  WN * wn_iter = Enclosing_Loop(wn);
  while (wn_iter) {
    if ((WN_start(wn_iter) == wn) 
	|| (WN_step(wn_iter) == wn)) {
      return TRUE;
    }
    WN * parent = Enclosing_Loop(wn_iter);
    wn_iter = (parent == wn_iter) ? NULL : parent;
  }
  return FALSE;
}

// Check whether there is scalar loop-carried dependenence in the 'loop'.
static BOOL Has_Scalar_Loop_Carried_Dependence
(SCALAR_STACK *source_list, SCALAR_STACK *sink_list, WN * loop)
{
  if (loop_var_is_live_on_exit(loop))
    return TRUE;

  // check definitions.
  for (int i = 0; i < source_list->Elements(); i++) {
    SCALAR_NODE *src_node = source_list->Bottom_nth(i);
    for (int ii = 0; ii < src_node->Elements(); ii++) {
      WN * src_wn = src_node->Bottom_nth(ii)->Wn;
      if (Is_Loop_Index(src_wn))
	  continue;
      OPERATOR opr = WN_operator(src_wn);

      if (OPERATOR_is_store(opr)) {
	if (var_is_live_on_exit(src_wn, loop))
	  return TRUE;

	if (opr == OPR_STID) {
	  ST * st = WN_st(src_wn);
	  if (!st || (ST_class(st) != CLASS_VAR)
	      || ST_addr_passed(st) || ST_addr_saved(st))
	    return TRUE;
	  if ((WN_sclass(src_wn) != SCLASS_AUTO)
	      && (WN_sclass(src_wn) != SCLASS_REG))
	    return TRUE;
	}
      }
    }
  }

  // check uses.  
  for (int j = 0; j < sink_list->Elements(); j++) {
    SCALAR_NODE *sink_node = sink_list->Bottom_nth(j);
    for (int jj = 0; jj < sink_node->Elements(); jj++) {
      WN * sink_wn = sink_node->Bottom_nth(jj)->Wn;
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(sink_wn);
      
      if (def_list->Incomplete())
	return TRUE;
      
      // No induction definitions.
      int inside_def_cnt = 0;
      int outside_def_cnt = 0;
      DEF_LIST_ITER d_iter(def_list);
      for (DU_NODE *def_node=(DU_NODE *)d_iter.First(); !d_iter.Is_Empty();
	   def_node = (DU_NODE *) d_iter.Next()) {
	WN * def = def_node->Wn();
	while (def && (def != loop) && (WN_opcode(def) != OPC_FUNC_ENTRY))
	  def = LWN_Get_Parent(def);
	if (def != loop)
	  outside_def_cnt++;
	else
	  inside_def_cnt++;
      }

      if (inside_def_cnt && outside_def_cnt)
	return TRUE;
    }
  }

  return FALSE;
}

// Check whether pairs of memory references from source_list and sink_list
// have loop-carried dependence at the given nesting level 'dim'.
// Current implementation is conservative.
static BOOL Has_Array_Loop_Carried_Dependence
(REF_LIST_STACK * source_list, REF_LIST_STACK * sink_list, mUINT8 dim)
{
  for (INT ii=0;ii<source_list->Elements(); ii++) {
    for (INT jj=0;jj<sink_list->Elements(); jj++) {
      REFERENCE_ITER iter1(source_list->Bottom_nth(ii));
      for (REFERENCE_NODE *n1=iter1.First(); !iter1.Is_Empty();
	   n1=iter1.Next()) {
	REFERENCE_ITER iter2(sink_list->Bottom_nth(jj));
	for (REFERENCE_NODE *n2=iter2.First();!iter2.Is_Empty();
	     n2=iter2.Next()) {
	  WN * src_wn = n1->Wn;
	  WN * sink_wn = n2->Wn;

	  if (DEPV_COMPUTE::Base_Test(src_wn,NULL,sink_wn,NULL) == DEP_INDEPENDENT)
	    continue;

	  if (!Array_Dependence_Graph->Get_Vertex(src_wn))
	    return TRUE;

	  if (OPERATOR_is_store(WN_operator(src_wn)))
	    src_wn = WN_kid1(src_wn);

	  if (OPERATOR_is_store(WN_operator(sink_wn)))
	    sink_wn = WN_kid1(sink_wn);

	  if (!Has_Unit_Refs(src_wn, dim) 
	      || !Has_Unit_Refs(sink_wn, dim))
	    return TRUE;
	}
      }
    }
  }
  return FALSE;
}

// Check whether there is loop-carried dependence in the given loop.
BOOL Has_Loop_Carried_Dependence(WN * loop)
{
  if (Do_Loop_Has_Calls(loop))
    return TRUE;

  MEM_POOL_Push(&FUSION_default_pool);
  REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
				   &FUSION_default_pool);
  REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
				  &FUSION_default_pool);
  SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
					&FUSION_default_pool);
  SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
				       &FUSION_default_pool);
  SCALAR_REF_STACK *params = CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool),
				     &FUSION_default_pool);
  DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
			      &FUSION_default_pool);
  Build_Doloop_Stack(loop, stack);
  
  // Make a list of all the references
  WN* loop_body=WN_do_body(loop);
  INT32 status=0;
  for (WN* wn=WN_first(loop_body); wn && status!= -1; wn=WN_next(wn)) 
    status=New_Gather_References(wn,writes,reads,stack,scalar_writes,scalar_reads,
				 params,&FUSION_default_pool);
  
  if (status == -1) {
    DevWarn("Aborted from New_Gather_References");
    MEM_POOL_Pop(&FUSION_default_pool);
    return FALSE;
  }

  DO_LOOP_INFO * dli = Get_Do_Loop_Info(loop);
  mUINT8 dim = dli->Depth;
  BOOL ret_val = FALSE;

  if (Has_Array_Loop_Carried_Dependence(writes, reads, dim)
      || Has_Array_Loop_Carried_Dependence(writes, writes, dim)
      || Has_Scalar_Loop_Carried_Dependence(scalar_writes, scalar_reads, loop))
    ret_val = TRUE;
  
  MEM_POOL_Pop(&FUSION_default_pool);
  return ret_val;
}

// Test routine for multiply-nesting fusion.
// Returns an array which gives the offset at each level.
// in_loop1 and in_loop2 are outer-most loop of the two loop nests to be
// fused. fusion_level specify how many levels to be fused.
// prolog_out, epilog_out and epilog_loop_out are the number of peeled
// iterations in prolog (which is always from in_loop1), epilog (which
// can be from in_loop1 or in_loop2) and the loop which results in
// iterations in epilog, prolog2_out and epilog2_out are the number of
// peeled iterations in prolog and epilog from in_loop2.
// Returns prolog==epilog==offset[inner_most_level]=MAX_INT32 if the loop
// nests are not in the fuse-able form.
// Return a dependence graph 'dg' for data dependences between two loops.
// Return an array of vertexes 'vst' in the dependence graph whose value
// can be pre-computed.
static FISSION_FUSION_STATUS
Fuse_Test(WN* in_loop1, WN* in_loop2, mUINT8 fusion_level, UINT32 threshold, 
UINT64 *prolog_out, UINT64 *epilog_out, UINT64 * prolog2_out, UINT64 *epilog2_out, WN** epilog_loop_out,
	  mINT32 offset[], ARRAY_DIRECTED_GRAPH16 * dg, DYN_ARRAY<VINDEX16> ** vts, UINT64 *precom_out)
{

  WN* loop_nest1[LNO_MAX_DO_LOOP_DEPTH];
  WN* loop_nest2[LNO_MAX_DO_LOOP_DEPTH];
  UINT8 i, j;

  char loop1_var_name[80];
  char loop2_var_name[80];
  if (strlen(ST_name(WN_st(WN_index(in_loop1))))>=80) {
    DevWarn("Loop var name %s too long",ST_name(WN_st(WN_index(in_loop1))));
    strcpy(loop1_var_name,"name_too_long");
  } else
    strcpy(loop1_var_name,ST_name(WN_st(WN_index(in_loop1))));
  if (strlen(ST_name(WN_st(WN_index(in_loop2))))>=80) {
    DevWarn("Loop var name %s too long",ST_name(WN_st(WN_index(in_loop2))));
    strcpy(loop2_var_name,"name_too_long");
  } else
    strcpy(loop2_var_name,ST_name(WN_st(WN_index(in_loop2))));

  SRCPOS srcpos1=WN_Get_Linenum(in_loop1);
  SRCPOS srcpos2=WN_Get_Linenum(in_loop2);

  FmtAssert(WN_opcode(in_loop1)==OPC_DO_LOOP, 
    ("non-loop input node in Fuse_Test()\n") );
  FmtAssert(WN_opcode(in_loop2)==OPC_DO_LOOP, 
    ("non-loop input node in Fuse_Test()\n") );

  DO_LOOP_INFO * dli1 = Get_Do_Loop_Info(in_loop1);
  DO_LOOP_INFO * dli2 = Get_Do_Loop_Info(in_loop2);

  UINT8 inner_most_level = fusion_level - 1;
  //mINT32 *offset = CXX_NEW_ARRAY(mINT32, fusion_level, &FUSION_default_pool);
  offset[inner_most_level] = MAX_INT32;
  for (i=1; i<fusion_level-1; i++) offset[i] = 0;

  *prolog_out = *epilog_out = *prolog2_out = *epilog2_out = MAX_INT32;
  *precom_out = 0;
  
  if (WN_next(in_loop1)!=in_loop2) {
    DevWarn("non-adjacent input loop nodes in Fuse_Test()");
    return Failed;
  } 

  // loop_nest1[] and loop_nest2[] store the loop nodes at different
  // levels

  // initially, two outer-most loops
  loop_nest1[0]=in_loop1;
  OPERATOR opr=
           WN_operator(WN_kid0(WN_start(in_loop1)));
  if (opr == OPR_MAX || opr==OPR_MIN) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    return Failed;
  }
  opr=WN_operator(WN_kid1(WN_end(in_loop1)));
  if (opr == OPR_MAX || opr==OPR_MIN) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    return Failed;
  }

  loop_nest2[0]=in_loop2;
  opr=WN_operator(WN_kid0(WN_start(in_loop2)));
  if (opr == OPR_MAX || opr==OPR_MIN) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loop with MIN, MAX lower bound cannot be fused.");
    return Failed;
  }
  opr=WN_operator(WN_kid1(WN_end(in_loop2)));
  if (opr == OPR_MAX || opr==OPR_MIN) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loop with MIN, MAX upper bound cannot be fused.");
    return Failed;
  }

  if (!Do_Loop_Is_Good(in_loop1) || !Do_Loop_Is_Good(in_loop2) ||
       Do_Loop_Has_Calls(in_loop1) || Do_Loop_Has_Calls(in_loop2) ||
       Do_Loop_Has_Gotos(in_loop1) || Do_Loop_Has_Gotos(in_loop2)) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    return Failed;
  }

#ifdef KEY
 {
   if (LNO_Run_Simd > 0 && LNO_Simd_Avoid_Fusion && 
       (dli1->Vectorizable ^ dli2->Vectorizable)) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
	"Vectorizable loop can not be fused with a serial loop.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
	"Vectorizable loop can not be fused with a serial loop.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
	"Vectorizable loop can not be fused with a serial loop.");
     return Failed;
   }
 }
#endif
  for (i=1; i<fusion_level; i++) {
    WN* lwn= Get_Only_Loop_Inside(loop_nest1[i-1],FALSE);
    if (!lwn) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      return Failed;
    }

    opr=WN_operator(WN_kid0(WN_start(lwn)));
    if (opr == OPR_MAX || opr==OPR_MIN) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      return Failed;
    }
    opr=WN_operator(WN_kid1(WN_end(lwn)));
    if (opr == OPR_MAX || opr==OPR_MIN) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      return Failed;
    }

    loop_nest1[i] = lwn;

    lwn= Get_Only_Loop_Inside(loop_nest2[i-1],FALSE);
    if (!lwn) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Non-simply nested loops cannot be fused.");
      return Failed;
    }

    opr=WN_operator(WN_kid0(WN_start(lwn)));
    if (opr == OPR_MAX || opr==OPR_MIN) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loop with MIN, MAX lower bound cannot be fused.");
      return Failed;
    }
    opr=WN_operator(WN_kid1(WN_end(lwn)));
    if (opr == OPR_MAX || opr==OPR_MIN) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loop with MIN, MAX upper bound cannot be fused.");
      return Failed;
    }

    loop_nest2[i] = lwn;

  }


  DO_LOOP_INFO *loop_info1[LNO_MAX_DO_LOOP_DEPTH];
  DO_LOOP_INFO *loop_info2[LNO_MAX_DO_LOOP_DEPTH];
  INT32 lb_diff[LNO_MAX_DO_LOOP_DEPTH];	// diff in lower bound
  INT32 ub_diff[LNO_MAX_DO_LOOP_DEPTH];	// diff in upper bound
  INT32 step[LNO_MAX_DO_LOOP_DEPTH];	// absolute value of the constant step
  BOOL is_positive[LNO_MAX_DO_LOOP_DEPTH];		// sign of step
  BOOL steps_are_constant[LNO_MAX_DO_LOOP_DEPTH];
  BOOL bounds_are_equal=TRUE;

  for (i=0; i<fusion_level; i++) {
    
    ACCESS_VECTOR *diff;	

    loop_info1[i]=(DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, loop_nest1[i]);
    loop_info2[i]=(DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, loop_nest2[i]);
    DO_LOOP_INFO* dli1=loop_info1[i];
    DO_LOOP_INFO* dli2=loop_info2[i];

    if (!loop_info1[i]->Step->Is_Const() || !loop_info2[i]->Step->Is_Const()) {
      // step has to be known constant when there is dependence
      // otherwise do not know dep. direction
      // however, if there is no dependence, then non-constant step is ok
      steps_are_constant[i]=FALSE;
    } else
      steps_are_constant[i]=TRUE;

    diff = Subtract(loop_info1[i]->Step, loop_info2[i]->Step,
			    &FUSION_default_pool);

    if (!diff->Is_Const() || diff->Const_Offset != 0) {
      
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Steps have to be equal in both loops.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Steps have to be equal in both loops.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Steps have to be equal in both loops.");
      return Failed;		// step has to be equal for both loops
    }

    // now we get the step
    if (steps_are_constant[i]) {
      step[i] = loop_info1[i]->Step->Const_Offset;
    } else {
      step[i]=1; // pretend to be 1
                 // fusion should be illegal if we find later that
                 // there is any offset at this level
    }
    is_positive[i] = step[i] > 0;
    if (!is_positive[i]) step[i] = - step[i];

    // TODO: cannot handle
    //  DO i=M,N,2
    //  DO i=M+K,N+K,2
    //  for now. May require returnning access vector.

    // when step is negative, UB is loop_start while LB is loop_end
    BOOL identical_expression=FALSE;
    if (is_positive[i]) {
      if (Compare_Bounds(
	  WN_start(loop_nest1[i]),WN_index(loop_nest1[i]),
	  WN_start(loop_nest2[i]),WN_index(loop_nest2[i]))==0) {

	  identical_expression=TRUE;

      } else if (Bound_Is_Too_Messy(dli1->LB) || dli1->LB->Num_Vec()!=1 ||
      		 Bound_Is_Too_Messy(dli2->LB) || dli2->LB->Num_Vec()!=1){
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        return Failed;
      } else {

        diff = Subtract(loop_info1[i]->LB->Dim(0), loop_info2[i]->LB->Dim(0),
                          &FUSION_default_pool);
      }

    } else {
      if (Compare_Bounds(
	  WN_end(loop_nest1[i]),WN_index(loop_nest1[i]),
	  WN_end(loop_nest2[i]),WN_index(loop_nest2[i]))==0) {

	  identical_expression=TRUE;

      } else if (Bound_Is_Too_Messy(dli1->UB) || dli1->UB->Num_Vec()!=1 ||
      		 Bound_Is_Too_Messy(dli2->UB) || dli2->UB->Num_Vec()!=1){
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        return Failed;
      } else {

        diff = Subtract(loop_info1[i]->UB->Dim(0), loop_info2[i]->UB->Dim(0),
                          &FUSION_default_pool);
      }
    }

    if (identical_expression)
	lb_diff[i] = 0;
    else if (!diff->Is_Const()) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Difference in lower bounds must be constant.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Difference in lower bounds must be constant.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Difference in lower bounds must be constant.");
      return Failed;		// lower bounds must have const diff
    } else {
      INT64 loop_coeff;
      if (is_positive[i])
        loop_coeff=loop_info1[i]->LB->Dim(0)->Loop_Coeff(
                       loop_info1[i]->LB->Dim(0)->Nest_Depth()-1);
	// this is how we get orig. stride after normalization
      else
        loop_coeff=loop_info1[i]->UB->Dim(0)->Loop_Coeff(
                       loop_info1[i]->UB->Dim(0)->Nest_Depth()-1);
      if ( (diff->Const_Offset % loop_coeff) != 0) {
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Difference in lower bounds must be constant.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Difference in lower bounds must be constant.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Difference in lower bounds must be constant.");
        return Failed;		// lower bounds must have const diff
      } else {
        lb_diff[i] = diff->Const_Offset / loop_coeff;
        bounds_are_equal = bounds_are_equal && (lb_diff[i]==0);
      }
      // lb_diff[i] = - diff->Const_Offset;
    }

    identical_expression=FALSE;
    if (is_positive[i]) {
      if (Compare_Bounds(
	  WN_end(loop_nest1[i]),WN_index(loop_nest1[i]),
	  WN_end(loop_nest2[i]),WN_index(loop_nest2[i]))==0) {

	  identical_expression=TRUE;

      } else if (Bound_Is_Too_Messy(dli1->UB) || dli1->UB->Num_Vec()!=1 ||
      		 Bound_Is_Too_Messy(dli2->UB) || dli2->UB->Num_Vec()!=1){
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Loops with messy upper bounds cannot be fused.");
        return Failed;
      } else {

        diff = Subtract(loop_info1[i]->UB->Dim(0), loop_info2[i]->UB->Dim(0),
                          &FUSION_default_pool);
     }
    } else {
      if (Compare_Bounds(
	  WN_start(loop_nest1[i]),WN_index(loop_nest1[i]),
	  WN_start(loop_nest2[i]),WN_index(loop_nest2[i]))==0) {

	  identical_expression=TRUE;

      } else if (Bound_Is_Too_Messy(dli1->LB) || dli1->LB->Num_Vec()!=1 ||
      		 Bound_Is_Too_Messy(dli2->LB) || dli2->LB->Num_Vec()!=1){
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Loops with messy lower bounds cannot be fused.");
        return Failed;
      } else {

        diff = Subtract(loop_info1[i]->LB->Dim(0), loop_info2[i]->LB->Dim(0),
                          &FUSION_default_pool);
      }
    }

    if (identical_expression)
      ub_diff[i]=0;
    else if (!diff->Is_Const()) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Difference in upper bounds must be constant.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Difference in upper bounds must be constant.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Difference in upper bounds must be constant.");
      return Failed;		// upper bounds must have const diff
    } else {
      INT64 loop_coeff;
      if (is_positive[i])
        loop_coeff=loop_info1[i]->UB->Dim(0)->Loop_Coeff(
                       loop_info1[i]->UB->Dim(0)->Nest_Depth()-1);
      else
        loop_coeff=loop_info1[i]->LB->Dim(0)->Loop_Coeff(
                       loop_info1[i]->LB->Dim(0)->Nest_Depth()-1);
      if ( (diff->Const_Offset % loop_coeff) != 0) {
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Difference in upper bounds must be constant.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Difference in upper bounds must be constant.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Difference in upper bounds must be constant.");
        return Failed;		// upper bounds must have const diff
      } else {
        ub_diff[i] = diff->Const_Offset / loop_coeff;
        bounds_are_equal = bounds_are_equal && (ub_diff[i]==0);
      }
      // ub_diff[i] = diff->Const_Offset;
    }

    // Note: although we know that the diff in upper bound and lower
    // bound are constants, the constant values might change because
    // the offset values determined later for outer loop could change
    // the constant differences.

  }

  WN* scalar_wn;
  if (scalar_wn=Scalar_Dependence_Prevent_Fusion(in_loop1,in_loop2)) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Failed because of scalar dependences.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Failed because of scalar dependences.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Failed because of scalar dependences.");
    if (LNO_Test_Dump) {
      SYMBOL tmp_symbol(scalar_wn);
      printf("Scalar Dependence caused by %s.\n", tmp_symbol.Name());
    }
    return Failed;
  }

  mINT32 max_neg_dist = 0;
  // get dist[] which gives max dep. dist. at each level
  mINT32* dist =
    Max_Dep_Distance(in_loop1,in_loop2,fusion_level,step,bounds_are_equal, dg, vts, &max_neg_dist);

  if (dist[fusion_level-1]==MAX_INT32) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Failed because of backward dependences.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Failed because of backward dependences.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Failed because of backward dependences.");
    return Failed;
  }

  // from the dep. dist. and the diff in lower bound, we compute the
  // necessary offset at each level
  // i.e. offset = max(dep. dist, lb diff)

  BOOL completely_aligned = TRUE;
  BOOL outer_peeling = FALSE;
  BOOL outer_precompute = FALSE;
  int precom_dist = dist[0];
  BOOL peel_loop2 = FALSE;

  if ((fusion_level == 1) && (precom_dist > 0) && vts && vts[0]
      && is_positive[0] && (lb_diff[0] < 0)
      && (Enclosing_Loop(in_loop1) == in_loop1)
      && !Has_Loop_Carried_Dependence(in_loop1)) {
    // Check dependence in the inner loops.
    VINDEX16 v = (*vts[0])[0];
    WN * wn_use = dg->Get_Wn(v);
    int common_depth = Loop_Depth(wn_use);
    INT32 c_step[LNO_MAX_DO_LOOP_DEPTH];
    mINT32 inner_neg_dist;
    // Pretend step is 1.
    for (int i = 0; i < common_depth; i++) c_step[i] = 1;
    mINT32 * c_dist = Max_Dep_Distance(in_loop1, in_loop2, common_depth + 1, c_step, FALSE, 
				       NULL, NULL, &inner_neg_dist);
    BOOL inner_no_dep = TRUE;
    for (int i = 1; i < common_depth; i++) {
      if (c_dist[i] > 0) {
	inner_no_dep = FALSE;
	break;
      }
    }
    
    // Attempt pre-computation if two loops have inter-loop dependence,
    // the loops are the outermost loops and no inter-loop dependence exists
    // in the inner loops.  Limit it to the case that the fusion
    // level is 1, loop step is positive, and in_loop1's lower bound is less than 
    // in_loop2's lower bound.
    if (inner_no_dep) {
      dist[0] = 0;
      outer_precompute = TRUE;
    
      if (LNO_Verbose)
	fusion_verbose_info(srcpos1,srcpos2,precom_dist, "Attempt precomputation !!");
    }
  }
  else {
    // For loops containing pre-computation defs and uses, if in_loop1's lower bound
    // is larger that in_loop2' lower bound and if in_loop1's upper bound is smaller than
    // in_loop2's upper bound, attempt peeling for in_loop2 so that its bounds are aligned
    // with in_loop1's bounds to avoid shifting of array address in in_loop2 which will
    // shut down scalar replacement opportunities. Also limit it to the case that
    // the fusion level is 1, loop step is positive and the in_loop2 has no loop-carried
    // dependence.
    if ((fusion_level == 1) && (dli1->Has_Precom_Def == 1) 
	&& (dli2->Has_Precom_Use == 1) && (dist[0] == 0) 
	&& is_positive[0] && (lb_diff[0] > 0) && (ub_diff[0] < 0)
	&& !Has_Loop_Carried_Dependence(in_loop2)) 
      peel_loop2 = TRUE;
  }

  if (!outer_precompute && vts) {
    for (int i = 0; i < fusion_level; i++) {
      if (vts[i])
	CXX_DELETE(vts[i], &FUSION_default_pool);
      vts[i] = NULL;
    }
  }

  // no prolog or epilog is allowed except for the innermost loop
  // i.e. outer loops must be completely aligned
  for (i=0; i<fusion_level; i++) {

    if (!steps_are_constant && offset[i]!=0) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
   "Failed due to non-zero offset required at a level with non-constant step");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
   "Failed due to non-zero offset required at a level with non-constant step");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
   "Failed due to non-zero offset required at a level with non-constant step");
      return Failed;
    }
    
    if (is_positive[i]) { // step is positive
      if ((offset[i]=lb_diff[i]) < dist[i])
        offset[i] = dist[i];
      // adjust the offset so that the resulting lowerbounds of the fused
      // loops are multiples of step apart
      INT32 r = ( -lb_diff[i] + offset[i] )%step[i];
      if (r!=0) offset[i] += (step[i] - r);

      if (offset[i]>dist[i])
	for (INT j=i+1; j<fusion_level; j++)
	  dist[j]=MIN_INT32;
	  // if we want alignment for memory re-use, we could leave
	  // the dist[inner_most_level] unchanged here
	  // but outer loop bounds still need to be completely aligned
  
      // test if it is completely aligned

      if (offset[i]!=lb_diff[i] ||
  	  lb_diff[i] != ub_diff[i]) {
            if (abs(offset[i]-lb_diff[i])>threshold ||
                abs(offset[i]-ub_diff[i])>threshold) {
              if (LNO_Verbose)
                fusion_verbose_info(srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              if (LNO_Analysis)
                fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              if (LNO_Tlog)
                fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              offset[inner_most_level] = MAX_INT32;
              return Failed; // needs peeling larger than threshold
            } else if (i==0 && i!=inner_most_level)
	      outer_peeling=TRUE;
            else if (i!=inner_most_level) {
              if (LNO_Verbose)
                fusion_verbose_info(srcpos1,srcpos2,fusion_level,
                  "Peeling needed for middle loops.");
              if (LNO_Analysis)
                fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
                  "Peeling needed for middle loops.");
              if (LNO_Tlog)
                fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
                  "Peeling needed for middle loops.");
              offset[inner_most_level] = MAX_INT32;
              return Failed; // needs peeling for middle loops
            }
	    completely_aligned = FALSE;
      }
  	  
      // adjust the bound diff due to the offset at the current level
      for (j=i+1; j<fusion_level; j++) {
        lb_diff[j] +=
	  (offset[i] *
	   loop_info1[j]->LB->Dim(0)->Loop_Coeff(loop_info1[i]->Depth));
        ub_diff[j] +=
	  (offset[i] *
	   loop_info1[j]->UB->Dim(0)->Loop_Coeff(loop_info1[i]->Depth));
      }


    } else {
      
      if ((offset[i]= - lb_diff[i]) < dist[i])
        offset[i] = dist[i];
      // adjust the offset so that the resulting lowerbounds of the fused
      // loops are multiples of step apart
      INT32 r = ( lb_diff[i] + offset[i] )%step[i];
      if (r!=0) offset[i] += (step[i] - r);
  
      if (offset[i]>dist[i])
	for (INT j=i+1; j<fusion_level; j++)
	  dist[j]=MIN_INT32;
	  // if we want alignment for memory re-use, we could leave
	  // the dist[inner_most_level] unchanged here
	  // but outer loop bounds still need to be completely aligned
  
      // test if it is completely aligned

      if (offset[i] != - lb_diff[i] ||
  	  lb_diff[i] != ub_diff[i]) {
            if (abs(offset[i]+lb_diff[i])>threshold ||
                abs(offset[i]+ub_diff[i])>threshold) {
              if (LNO_Verbose)
                fusion_verbose_info(srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              if (LNO_Analysis)
                fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              if (LNO_Tlog)
                fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
              offset[inner_most_level] = MAX_INT32;
              return Failed; // needs peeling larger than threshold
            } else if (i==0 && i!=inner_most_level)
	      outer_peeling=TRUE;
            else if (i!=inner_most_level) {
              if (LNO_Verbose)
                fusion_verbose_info(srcpos1,srcpos2,fusion_level,
                  "Peeling needed for middle loops.");
              if (LNO_Analysis)
                fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
                  "Peeling needed for middle loops.");
              if (LNO_Tlog)
                fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
                  "Peeling needed for middle loops.");
              offset[inner_most_level] = MAX_INT32;
              return Failed; // needs peeling for middle loops
            }
	    completely_aligned = FALSE;
      }
  	  
      // adjust the bound diff due to the offset at the current level
      for (j=i+1; j<fusion_level; j++) {
        lb_diff[j] -=
	  (offset[i] *
	   loop_info1[j]->UB->Dim(0)->Loop_Coeff(loop_info1[i]->Depth));
        ub_diff[j] -=
	  (offset[i] *
	   loop_info1[j]->LB->Dim(0)->Loop_Coeff(loop_info1[i]->Depth));
      }

    }

  }
  
  // test for special case when there is no prolog or epilog needed
  if (completely_aligned && !outer_precompute) {
    
      *prolog_out = *epilog_out = *epilog2_out = 0;
      *epilog_loop_out = NULL;
      return Succeeded;
  
  } else if (outer_peeling) {

    offset[inner_most_level] = MAX_INT32;
    return Try_Level_By_Level;

  }

  // if the inner-most loops cannot be aligned completely
  
  INT64 loop1_lb_ub;
  INT64 loop2_lb_ub;

  // get in_loop1 iteration count
  // this is really iter_count-1 (i.e. ub-lb)
  ACCESS_VECTOR *loop1_iter = Add(loop_info1[inner_most_level]->UB->Dim(0),
			     loop_info1[inner_most_level]->LB->Dim(0),
                             &FUSION_default_pool);
  
  BOOL iteration_count_unknown = FALSE;

  // if loop count is unknown, step must be 1, otherwise
  // cannot compute prolog, etc
  // E.g. do i=M,  N,  2
  //      do i=M+4,N+2,  2
  // The epilog would depend on the relation between M and N
  // and we only generate peeled iterations instead of
  // generating loop with unknown bounds
  if (! loop1_iter->Is_Const()) {
    if ( step[inner_most_level] != 1) {
      offset[inner_most_level] = MAX_INT32;
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Iteration count has to be constant for non-stride-1 loop.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Iteration count has to be constant for non-stride-1 loop.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Iteration count has to be constant for non-stride-1 loop.");
      return Failed;
    } else // step[inner_most_level] == 1
      iteration_count_unknown = TRUE;
  } else {
  
    ACCESS_VECTOR *loop2_iter =
			     Add(loop_info2[inner_most_level]->UB->Dim(0),
			     loop_info2[inner_most_level]->LB->Dim(0),
                             &FUSION_default_pool);
  
    loop1_lb_ub = loop1_iter->Const_Offset;
    loop2_lb_ub = loop2_iter->Const_Offset;

    if (!is_positive[inner_most_level]) {
      loop1_lb_ub = - loop1_lb_ub;
      loop2_lb_ub = - loop2_lb_ub;
    }
  
    loop1_lb_ub -= loop1_lb_ub%step[inner_most_level];	// adjusted ub-lb
    loop2_lb_ub -= loop2_lb_ub%step[inner_most_level];
  
  }

  if (step[inner_most_level]>0) { // step is positive
      
    INT64 prolog;
    INT64 epilog;
  
    // prolog loop : DO i = L1, MIN(U1',L2+offset-step), step
    if (iteration_count_unknown) {
      prolog = - lb_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level] ;
    } else {
      prolog = - lb_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level] ;
      if (loop1_lb_ub < prolog)
        prolog = loop1_lb_ub;
    }
    if (prolog < 0 ) {
      if (peel_loop2) 
	*prolog2_out = -prolog;
      prolog = 0;
    }
    else {
      FmtAssert(prolog%step[inner_most_level] ==0, ("misaligned prolog\n"));
      prolog = prolog / step[inner_most_level];
      prolog ++;
    }
  
    // main fused loop : DO i = L2+offset, MIN(U1', U2'+Offset), step
  
    // now works on epilog
    // epilog loop:
    // if (U1'>U2'+offset)			epilog is loop1
    //   DO i = MAX(L1,U2'+offset+step), U1', step
    // where U1' and U2' are adjusted upperbounds to align with step
    // note: U1'-U2' == L1 + loop1_lb_ub - L2 - loop2_lb_ub
  
    if (ub_diff[inner_most_level] > offset[inner_most_level]) {
      
      if (iteration_count_unknown) {	// step[inner_most_level] == 1
	epilog = ub_diff[inner_most_level] -
		 offset[inner_most_level] - step[inner_most_level];
      } else {
        epilog = lb_diff[inner_most_level] +
  	     loop1_lb_ub - loop2_lb_ub -
	     offset[inner_most_level] - step[inner_most_level];
        if (loop1_lb_ub < epilog)
          epilog = loop1_lb_ub;
      }
      *epilog_loop_out = loop_nest1[inner_most_level];
  
    } else {
    // if (U1'<=U2'+offset)			epilog is loop2
    //   DO i = MAX(L2+offset,U1'+step), U2'+offset, step
    // note: U2'-U1' == L2 - L1 - loop1_lb_ub + loop2_lb_ub
      
      if (iteration_count_unknown) {	// step[inner_most_level] == 1
        epilog = - ub_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level];
      } else {
        epilog = - lb_diff[inner_most_level] -
  	     loop1_lb_ub + loop2_lb_ub +
	     offset[inner_most_level] - step[inner_most_level];
        if (loop2_lb_ub < epilog)
          epilog = loop2_lb_ub;
      }
      *epilog_loop_out = loop_nest2[inner_most_level];
    }
    if (epilog<0) {
      epilog = 0;
      *epilog_loop_out = NULL;
    } else {
      if (peel_loop2 && ((*epilog_loop_out) == loop_nest2[inner_most_level])) {
	*epilog2_out = epilog;
	epilog = 0;
      }
      else {
	FmtAssert(epilog%step[inner_most_level] ==0, ("misaligned epilog\n"));
	epilog = epilog / step[inner_most_level];
	epilog ++;
      }
    }

    if (outer_precompute) {
      // If no peeling is from in_loop2, check whether extra peelings can be added to
      // remove conditional branches in_loop2.
      if ((prolog >= 0) && (epilog >= 0)
	  && (((*epilog_loop_out) == NULL)
	      || ((*epilog_loop_out) == loop_nest1[inner_most_level]))) {
	int extra_prolog = 0;
	int extra_epilog = 0;
	Removable_branch_loop_peeling(loop_nest2[inner_most_level], dg, vts[0],
				      &extra_prolog, &extra_epilog);
	prolog += extra_prolog;
	epilog += extra_epilog;
	*prolog2_out = extra_prolog;
	*epilog2_out = extra_epilog;
      }
      
      // in_loop1 may need extra pre-peeling to satisfy negative distance dependencies.
      if (max_neg_dist > 0) 
	prolog = MAX(prolog, max_neg_dist);

      // in_loop1 may need an extra post-peeling to make sure its iteration space
      // after loop fusion does not exceed the original program. 
      // in_loop2 may also need a post-peeling.
      if (precom_dist > epilog) {
	if (*epilog_loop_out) 
	  FmtAssert((*epilog_loop_out) == loop_nest1[inner_most_level],
		    ("Unexpected epilog."));
	*epilog_loop_out = loop_nest1[inner_most_level];
	epilog = precom_dist;

	if (ub_diff[0] < epilog)
	  *epilog2_out = epilog - ub_diff[0];
      }
    }
  
    *prolog_out = prolog;
    *epilog_out = epilog;
    if (outer_precompute)
      *precom_out = precom_dist;
    if (peel_loop2)
      offset[0] = 0;
  } else {	// step is negative
    
    INT64 prolog;
    INT64 epilog;
  
    // prolog loop : DO i = L1, MAX(U1',L2-offset+step), -step
    if (iteration_count_unknown) {	// step[inner_most_level] == 1
      prolog = lb_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level] ;
    } else {
      prolog = lb_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level] ;
      if (loop1_lb_ub < prolog)
        prolog = loop1_lb_ub;
    }
    if (prolog<0) prolog = 0;
    else {
      FmtAssert(prolog%step[inner_most_level] ==0, ("misaligned prolog\n"));
      prolog = prolog / step[inner_most_level];
      prolog ++;
    }
  
    // main fused loop : DO i = L2-offset, MAX(U1', U2'-Offset), -step
  
    // now works on epilog
    // epilog loop:
    // if (U1'<U2'-offset)			epilog is loop1
    //   DO i = MIN(L1,U2'-offset-step), U1', -step
    // where U1' and U2' are adjusted upperbounds to align with step
    // note: U1'-U2' == L1 - loop1_lb_ub - L2 + loop2_lb_ub
  
    if (ub_diff[inner_most_level] < -offset[inner_most_level]) {
      
      if (iteration_count_unknown) {	// step[inner_most_level] == 1
        epilog = - ub_diff[inner_most_level] -
	       offset[inner_most_level] - step[inner_most_level];
      } else {
        epilog = - lb_diff[inner_most_level] +
  	       loop1_lb_ub - loop2_lb_ub -
	       offset[inner_most_level] - step[inner_most_level];
        if (loop1_lb_ub < epilog)
          epilog = loop1_lb_ub;
      }
      *epilog_loop_out = loop_nest1[inner_most_level];
  
    } else {
    // if (U1'>=U2'-offset)			epilog is loop2
    //   DO i = MIN(L2-offset,U1'-step), U2'-offset, -step
    // note: U1'-U2' == L1 - L2 - loop1_lb_ub + loop2_lb_ub
      
      if (iteration_count_unknown) {	// step[inner_most_level] == 1
        epilog = ub_diff[inner_most_level] +
	     offset[inner_most_level] - step[inner_most_level];
      } else {
        epilog = lb_diff[inner_most_level] -
  	     loop1_lb_ub + loop2_lb_ub +
	     offset[inner_most_level] - step[inner_most_level];
        if (loop2_lb_ub < epilog)
          epilog = loop2_lb_ub;
      }
      *epilog_loop_out = loop_nest2[inner_most_level];
    }
    if (epilog<0) {
      epilog = 0;
      *epilog_loop_out = NULL;
    } else {
      FmtAssert(epilog%step[inner_most_level] ==0, ("misaligned epilog\n"));
      epilog = epilog / step[inner_most_level];
      epilog ++;
    }
  
    *prolog_out = prolog;
    *epilog_out = epilog;
  

  }
  return Succeeded; 

}

// Change loop_stmt for LDID's in the tree rooted at 'wn'
void Loop_Stmt_Update(
  WN* wn,		// root of the WN 
  WN* old_loop,		// old loop WN
  WN* new_loop)		// new loop WN
{
    MEM_POOL_Push(&LNO_local_pool);
 
    REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                     &LNO_local_pool);
    REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                    &LNO_local_pool);
    SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                          &LNO_local_pool);
    SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                          &LNO_local_pool);
    DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                 &LNO_local_pool);

    Build_Doloop_Stack(wn, stack);

    SCALAR_REF_STACK *params =
          CXX_NEW(SCALAR_REF_STACK(&LNO_local_pool), &LNO_local_pool);

    // Make a list of all the references
    Init_Ref_Stmt_Counter();
    New_Gather_References(wn,writes,reads,stack,
        scalar_writes,scalar_reads,
        params,&LNO_local_pool,Gather_Scalar_Refs);

    for (INT sj=0; sj<scalar_reads->Elements(); sj++) {
      SCALAR_NODE* sjnode=scalar_reads->Bottom_nth(sj);
      for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
        WN* read=sjnode->Bottom_nth(sjj)->Wn;
        if (!Du_Mgr->Ud_Get_Def(read)) {
          DevWarn ("Exposed use before def: \n");
          Dump_WN (read, stdout, TRUE, 4, 4);
        }
        else if (Du_Mgr->Ud_Get_Def(read)->Loop_stmt()==old_loop)
          Du_Mgr->Ud_Get_Def(read)->Set_loop_stmt(new_loop);
      }
    }

    MEM_POOL_Pop(&LNO_local_pool);

}

// Check whether a WHIRL tree rooted at 'wn' contains a SYMBOL that matches 'symbol'.
static BOOL Has_symbol(WN * wn, SYMBOL symbol)
{
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_LDID)
    return (symbol == SYMBOL(wn));

  for (int k = 0; k < WN_kid_count(wn); k++) {
    if (Has_symbol(WN_kid(wn, k), symbol))
      return TRUE;
  }

  return FALSE;
}

// Walk WHIRL tree rooted at 'wn', find array index expressions that use the symbol
// 'symbol', add 'offset' to the expression.
static void Add_index_offset(WN * wn, SYMBOL symbol, int offset, OPERATOR step_operator)
{
  if (WN_operator(wn) == OPR_ARRAY) {
    for (int index = 0; index < WN_num_dim(wn); index++) {
      WN * wn_dim = WN_array_index(wn, index);
      if (Has_symbol(wn_dim, symbol)) {
	WN_array_index(wn, index) = 
	  LWN_CreateExp2(
			 OPCODE_make_op(step_operator, WN_rtype(wn_dim), MTYPE_V),
			 wn_dim,
			 LWN_Make_Icon(WN_rtype(wn_dim), offset));
      }
    }
  }
  else {
    for (int i = 0; i < WN_kid_count(wn); i++)
      Add_index_offset(WN_kid(wn,i), symbol, offset, step_operator);
  }
}

// if 'unrolled' is TRUE (default)
// peel 'iter_count' iterations from the beginning of in_loop
// E.g.
//      DO i=m,n,step
//        S(i)
//      END DO
// becomes
//      if (m<=n)
//        S(m)
//      end if
//      if (m+step<=n)
//        S(m+step)
//      end if
//      ...
//      if (m+step*(iter_count-1) <=n)
//        S(m+iter_count-1)
//      end if
//      do i=m+iter_count,n,step
//        S(i)
//      end do
//
// otherwise, 'unrolled' is FALSE, the peeled portion is a loop:
//      do i=m,min(m+iter_count-1,n)
//        S(i)
//      end do
//      end if
//      do i=m+iter_count,n
//        S(i)
//      end do
// and the 'if' can be optimized away if the condition is known
//
// if loop index 'i' is live on loop exit, the final value is preserved
// by making another copy of it after the loop
//

extern void Pre_loop_peeling(WN* in_loop, UINT32 iter_count,
BOOL unrolled, BOOL preserve_loop_index, BOOL precom_map)
{

  ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph; // LNO array graph
  BOOL do_precom = precom_map ? TRUE : FALSE;

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node\n") );

  // test for trivial case
  if (iter_count<=0) return;
  if (WN_first(WN_do_body(in_loop))==NULL)
    return;

  // abort if bound is not 'i<=n'
  // Upper_Bound_Standardize(WN_end(in_loop));
  if (preserve_loop_index && loop_var_is_live_on_exit(in_loop)) {
    Finalize_Index_Variable(in_loop,FALSE);
    scalar_rename(WN_start(in_loop));
  }

  MEM_POOL_Push(&LNO_local_pool);

  // remember the boundary in order to update dependence info later
  WN *prev_stmt=WN_prev(in_loop);
  WN *parent = LWN_Get_Parent(in_loop);

  TYPE_ID index_type = WN_desc(WN_start(in_loop));
  SYMBOL index_symbol(WN_start(in_loop));
  OPERATOR step_operator =
    WN_operator(WN_kid0(WN_step(in_loop)));

  if (unrolled==FALSE) {
    // generate a loop for the peeled portion
  
    WN **new_iter=CXX_NEW_ARRAY(WN*, 2, &LNO_local_pool);

    new_iter[0]=WN_do_body(in_loop);
    BOOL out_of_edge=FALSE;
    new_iter[1] = LWN_Copy_Tree(WN_do_body(in_loop),TRUE,LNO_Info_Map);

    if (do_precom) {
      LWN_Copy_Map(new_iter[0], new_iter[1], precom_map);
      if (LNO_Verbose) {
	DO_LOOP_INFO * dli = Get_Do_Loop_Info(in_loop);
	printf("####Add precomputation loop to loop num: %d\n", (dli ? dli->Get_Id() : 0));
      }
    }

    BOOL all_internal = WN_Rename_Duplicate_Labels(WN_do_body(in_loop),
                          new_iter[1], Current_Func_Node, &LNO_local_pool);
    Is_True(all_internal, ("external labels renamed"));

                    // copy access vector
    if (!adg->Add_Deps_To_Copy_Block(new_iter[0],new_iter[1], FALSE)) {
                    // do not copy internal edges
      out_of_edge=TRUE;
      LWN_Update_Dg_Delete_Tree(new_iter[1], adg);
    }
    if (out_of_edge) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop),adg);
    }

    Unrolled_DU_Update(new_iter, 2, Do_Loop_Depth(in_loop));
    WN* last_stmt=WN_last(new_iter[0]);
    LWN_Insert_Block_Before(new_iter[0], NULL, new_iter[1]);
    DYN_ARRAY<FF_STMT_LIST> loop(&LNO_local_pool);
    loop.Newidx(); loop[0].Init(NULL);
    loop.Newidx(); loop[1].Init(NULL);
    WN *wn;
    for (wn=WN_first(new_iter[0]); wn!=last_stmt; wn=WN_next(wn)) {
      loop[0].Append(wn,&LNO_local_pool);
    }
    loop[0].Append(last_stmt,&LNO_local_pool);
    for (wn=WN_next(last_stmt); wn; wn=WN_next(wn)) {
      loop[1].Append(wn,&LNO_local_pool);
    }
    Separate_And_Update(in_loop, loop, 1);
    WN* new_loop=WN_next(in_loop);

    // at this point, we have created two loops 'in_loop' and
    // 'new_loop' which is right after 'in_loop'
    // since we want new_loop be in front of in_loop (in_loop should
    // point to the main loop) but the DU has
    // been updated assuming in_loop to be before the new_loop, we
    // swap their kids
    for (INT i=0; i<WN_kid_count(in_loop); i++) {
      wn=WN_kid(in_loop,i);
      WN_kid(in_loop,i)=WN_kid(new_loop,i);
      LWN_Set_Parent(WN_kid(in_loop,i), in_loop);
      WN_kid(new_loop,i)=wn;
      LWN_Set_Parent(WN_kid(new_loop,i), new_loop);
    }
    LWN_Extract_From_Block(new_loop);
    LWN_Insert_Block_Before(LWN_Get_Parent(in_loop), in_loop, new_loop);
    Loop_Stmt_Update(in_loop,new_loop,in_loop);
    Loop_Stmt_Update(new_loop,in_loop,new_loop);
    Get_Do_Loop_Info(in_loop)->Est_Num_Iterations= -1;
    Get_Do_Loop_Info(new_loop)->Est_Num_Iterations= -1;

    if (!do_precom)
      WN_kid0(WN_start(in_loop))=
	LWN_CreateExp2(				// m+(iter_count)
		       OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
		       WN_kid0(WN_start(in_loop)),
		       LWN_Make_Icon(index_type,(iter_count)*Step_Size(in_loop)));

    wn=LWN_Copy_Tree(WN_kid0(WN_start(new_loop)),TRUE,LNO_Info_Map);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_kid0(WN_start(in_loop)), wn, FALSE))
      LNO_Erase_Dg_From_Here_In(WN_kid0(WN_start(in_loop)),
        Array_Dependence_Graph);

    LWN_Copy_Def_Use(WN_kid0(WN_start(new_loop)),wn, Du_Mgr);
    WN_kid1(WN_end(new_loop))=
      LWN_CreateExp2(				// min(lb+(iter_count-1),ub)
        OPCODE_make_op(OPR_MIN, index_type, MTYPE_V),
        LWN_CreateExp2(
          OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
          wn,
          LWN_Make_Icon(index_type,(iter_count-1)*Step_Size(in_loop))),
        WN_kid1(WN_end(new_loop)));

    if (do_precom) {
      WN * wn_iter = new_loop;
      while (wn_iter && (WN_operator(wn_iter) == OPR_DO_LOOP)) {
	wn_iter = WN_do_body(wn_iter);
	wn_iter = WN_first(wn_iter);
      }
      
      FmtAssert(wn_iter, ("Unexpected NULL body"));
      WN * wn_next;

      // Pre-computation initialization.
      for (; wn_iter; wn_iter = wn_next) {
	wn_next = WN_next(wn_iter);
	UINT val = (INTPTR) WN_MAP_Get(precom_map, wn_iter);
	if (!PRECOM_IS_DEF(val)) {
	  // Delete wn_iter.
	  LWN_Update_Def_Use_Delete_Tree(wn_iter, Du_Mgr);
	  LNO_Erase_Dg_From_Here_In(wn_iter, Array_Dependence_Graph);
	  LWN_Extract_From_Block(wn_iter);
	  LWN_Delete_Tree(wn_iter);
	}
      }

      DO_LOOP_INFO * dli_new = Get_Do_Loop_Info(new_loop);
      dli_new->Is_Precom_Init = 1;

      wn_iter = in_loop;
      while (wn_iter && (WN_operator(wn_iter) == OPR_DO_LOOP)) {
	DO_LOOP_INFO * dli_iter = Get_Do_Loop_Info(wn_iter);
	if (dli_iter)
	  dli_iter->Has_Precom_Def = 1;
	wn_iter = WN_do_body(wn_iter);
	wn_iter = WN_first(wn_iter);
      }
      
      FmtAssert(wn_iter, ("Unexpected NULL body"));
      WN * wn_insert = wn_iter;
      WN* wn_block = wn_iter;
      while (wn_block && (WN_operator(wn_block) != OPR_BLOCK))
	wn_block = LWN_Get_Parent(wn_block);
      FmtAssert(wn_block, ("Unexpected NULL block."));
      
      for (; wn_iter; wn_iter = wn_next) {
	wn_next = WN_next(wn_iter);
	UINT val = (UINT) (INTPTR) WN_MAP_Get(precom_map, wn_iter);
	// shift offset of pre-computed array references in in_loop.	
	if (val) {
	  WN * wn_shift = NULL;
	  if (val == PRECOM_INTERNAL) {
	    // make a copy
	    wn_shift = LWN_Copy_Tree(wn_iter, TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(WN_kid(wn_iter, 1), WN_kid(wn_shift,1), Du_Mgr);
	    LWN_Copy_Def_Use(WN_kid(wn_iter, 0), WN_kid(wn_shift,0), Du_Mgr);
	    LWN_Insert_Block_Before(wn_block, wn_insert, wn_shift);
	  }
	  else if (val == PRECOM_ROOT) {
	    wn_shift = wn_iter;
	    if (wn_shift != wn_insert) {
	      WN_EXTRACT_FromBlock(wn_block, wn_shift);
	      WN_INSERT_BlockBefore(wn_block, wn_insert, wn_shift);
	    }
  	  }
	  
	  // Add a constant offset to array index expressions.
	  Add_index_offset(wn_shift, index_symbol, iter_count, step_operator);
	}
      }
    }

    DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(in_loop), loop_stack);

    // parentize, build access vectors and update dependences
    LWN_Parentize(in_loop);
    LWN_Parentize(new_loop);

    LNO_Build_Access(new_loop, loop_stack, &LNO_default_pool);

    if (do_precom)
      LNO_Build_Access(in_loop, loop_stack, &LNO_default_pool);

    LNO_Build_Do_Access(in_loop, loop_stack);
    
    if (!adg->Build_Region(new_loop,in_loop,loop_stack,TRUE,TRUE)) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop), adg);  
    } 
  } else {
    FmtAssert(!do_precom, ("TODO. precomp pre-peeling."));
    WN *index_expr;
    WN **new_iter=CXX_NEW_ARRAY(WN*, iter_count+1, &LNO_local_pool);

    new_iter[0]=WN_do_body(in_loop);
    BOOL out_of_edge=FALSE;
    INT i;
    for (i=1; i<=iter_count; i++) {

      new_iter[i] = LWN_Copy_Tree(WN_do_body(in_loop),TRUE,LNO_Info_Map);
      BOOL all_internal = WN_Rename_Duplicate_Labels(WN_do_body(in_loop),
                            new_iter[i], Current_Func_Node, &LNO_local_pool);
      Is_True(all_internal, ("external labels renamed"));
                    // do not copy access vector
      if (Good_Do_Depth(in_loop)>0 &&
          !adg->Add_Deps_To_Copy_Block(new_iter[0],new_iter[i], FALSE)) {
                      // do not copy internal edges
        out_of_edge=TRUE;
        LWN_Update_Dg_Delete_Tree(new_iter[i], adg);
      }
    }
    
    if (out_of_edge) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop),adg);
    }

    Unrolled_DU_Update(new_iter, iter_count+1, Do_Loop_Depth(in_loop));
    WN* wn_tmp=LWN_Get_Parent(in_loop);
    while (wn_tmp && WN_opcode(wn_tmp)!=OPC_DO_LOOP)
     wn_tmp=LWN_Get_Parent(wn_tmp);
    for (i=1; i<=iter_count; i++)
      Loop_Stmt_Update(new_iter[i],in_loop,wn_tmp);

    WN* wn;

    index_expr = LWN_Copy_Tree(WN_kid0(WN_start(in_loop)),TRUE,LNO_Info_Map);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_kid0(WN_start(in_loop)), index_expr, FALSE))
      LNO_Erase_Dg_From_Here_In(WN_kid0(WN_start(in_loop)),
        Array_Dependence_Graph);

    LWN_Copy_Def_Use(WN_kid0(WN_start(in_loop)), index_expr, Du_Mgr);

    WN_kid0(WN_start(in_loop))=
      LWN_CreateExp2(				// m+(iter_count)
        OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
        WN_kid0(WN_start(in_loop)),
        LWN_Make_Icon(index_type,(iter_count)*Step_Size(in_loop)));

    LWN_Parentize(WN_start(in_loop));

    for (i=0; i<iter_count; i++) {
      
      WN* guard= LWN_Copy_Tree(WN_end(in_loop),TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(WN_end(in_loop), guard, FALSE))
        LNO_Erase_Dg_From_Here_In(WN_end(in_loop), Array_Dependence_Graph);

      LWN_Copy_Def_Use(WN_end(in_loop), guard, Du_Mgr);

      Replace_Ldid_With_Exp_Copy(index_symbol, guard, index_expr,
        Du_Mgr);
      // guard=WN_Simplify_Tree(guard); // could mess up DU info

      Replace_Ldid_With_Exp_Copy(index_symbol,new_iter[i+1],index_expr,
        Du_Mgr);

      WN* stride=
        LWN_Copy_Tree(WN_kid1(WN_kid0(WN_step(in_loop))),TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(WN_kid1(WN_kid0(WN_step(in_loop))),stride,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid1(WN_kid0(WN_step(in_loop))),
          Array_Dependence_Graph);


      index_expr=LWN_CreateExp2(
        OPCODE_make_op(step_operator, index_type, MTYPE_V),
        index_expr, stride);

      WN *if_wn;

      if_wn=LWN_CreateIf(
          guard,					// WN *test,
          new_iter[i+1],				// WN *if_then,
          WN_CreateBlock());			// WN *if_else;
      LWN_Copy_Linenumber(in_loop, if_wn);

      IF_INFO* if_info =
        CXX_NEW(IF_INFO(&LNO_default_pool,
                !Get_Do_Loop_Info(in_loop)->Is_Inner,
                Find_SCF_Inside(in_loop,OPC_REGION)!=NULL),
          &LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map, if_wn, (void*)if_info);

      LWN_Copy_Frequency_Tree(if_wn, in_loop);
      LWN_Insert_Block_Before(parent, in_loop, if_wn);

      DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(in_loop), loop_stack);

      LNO_Build_If_Access(if_wn, loop_stack);
      COND_IF_INFO cond_result=COND_If_Info(if_wn, &LNO_local_pool);

      if (cond_result==COND_IF_THEN_ONLY) {

        LWN_Extract_From_Block(if_wn);
        LWN_Insert_Block_Before(parent, in_loop, new_iter[i+1]);
        WN_then(if_wn)=WN_CreateBlock();
        LWN_Update_Def_Use_Delete_Tree(if_wn,Du_Mgr);
        LWN_Delete_Tree(if_wn);

      } else if (cond_result==COND_IF_ELSE_ONLY) {

        LWN_Delete_Tree(if_wn);
        LWN_Update_Def_Use_Delete_Tree(if_wn,Du_Mgr);
        //LWN_Delete_From_Block(parent, if_wn);

      }
      if (i==iter_count-1) {
        LWN_Update_Def_Use_Delete_Tree(index_expr,Du_Mgr);
        LWN_Delete_Tree(index_expr);
      }


    }


    WN* start_stmt;
      
    if (prev_stmt)
      start_stmt = WN_next(prev_stmt);
    else
      start_stmt = WN_first(parent);

    DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                       &LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(start_stmt), loop_stack);

    for (wn=start_stmt; wn!=in_loop; wn=WN_next(wn)) {
      LNO_Build_Access(wn, loop_stack, &LNO_default_pool);
      Remark_Depth(wn, loop_stack->Elements());
    }
    Get_Do_Loop_Info(in_loop)->Est_Num_Iterations= -1;
    LNO_Build_Do_Access(in_loop, loop_stack);

    if (!adg->Build_Region(start_stmt,in_loop,loop_stack,TRUE,TRUE)) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop), adg);  
    }
    // change loop heads and frequencies
    LWN_Adjust_Frequency_Tree(WN_step(in_loop), -iter_count);
    LWN_Adjust_Frequency_Tree(WN_end(in_loop), -iter_count);

  }

  MEM_POOL_Pop(&LNO_local_pool);

  SRCPOS srcpos=WN_Get_Linenum(in_loop);
  if (LNO_Verbose)
    pre_peeling_verbose_info(srcpos,iter_count);
  if (LNO_Analysis)
    pre_peeling_analysis_info(srcpos,iter_count);
  if (LNO_Tlog)
    pre_peeling_tlog_info(in_loop,iter_count);
}

static void Build_Regions(WN* in_loop, 
			  WN* wn_tree, 
			  DOLOOP_STACK* loop_stack)
{
  ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph; // LNO array graph
  if (WN_operator(wn_tree) == OPR_DO_LOOP) {
    if (!adg->Build_Region(wn_tree,wn_tree,loop_stack,TRUE,TRUE)) {
      DevWarn("Array dependence graph overflowed in Post_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop), adg);
    } 
    return; 
  } 

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Build_Regions(in_loop, wn, loop_stack);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Build_Regions(in_loop, WN_kid(wn_tree, i), loop_stack);
  } 
}
  
// if 'unrolled' is TRUE (default)
// peel 'iter_count' iterations from the end of in_loop
// E.g.
//      DO i=m,n,step
//        S(i)
//      END DO
// becomes
//      do i=m,n-step*iter_count-(n-m)%step,step
//        S(i)
//      end do
//      if (m<=n-step*(iter_count-1)-(n-m)%step)
//        S(n-step*(iter_count-1)-(n-m)%step)
//      end if
//      if (m<=n-step*(iter_count-2)-(n-m)%step)
//        S(n-step*(iter_count-2)-(n-m)%step)
//      end if
//      ...
//      if (m<=n-(n-m)%step)
//        S(n-(n-m)%step)
//      end if
// otherwise, 'unrolled' is FALSE, the peeled portion is a loop:
//      do i=m,n-iter_count
//        S(i)
//      end do
//      do i=max(m,n-iter_count+1),n
//        S(i)
//      end do
//      end if
// and the lower bound of the second loop can be optimized 
//
// if loop index 'i' is live on loop exit, the final value is preserved
// by making another copy of it after the loop
//
extern void Post_loop_peeling(WN* in_loop, UINT32 iter_count,
BOOL unrolled, BOOL preserve_loop_index)
{
  ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph; // LNO array graph

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node\n") );

  // test for trivial case
  if (iter_count<=0) return;

  // abort if the upper bound is not 'i<=n'
  // Upper_Bound_Standardize(WN_end(in_loop));
  if (preserve_loop_index && loop_var_is_live_on_exit(in_loop)) {
    Finalize_Index_Variable(in_loop,FALSE);
    scalar_rename(WN_start(in_loop));
  }

  MEM_POOL_Push(&LNO_local_pool);

  WN *wn;

  WN *next_stmt=WN_next(in_loop);
  WN *parent = LWN_Get_Parent(in_loop);

  SYMBOL loop_index_symbol(WN_start(in_loop));
  TYPE_ID index_type = WN_desc(WN_start(in_loop));
  OPERATOR step_operator =
    WN_operator(WN_kid0(WN_step(in_loop)));
  OPERATOR inverse_step_operator;
  if (step_operator == OPR_ADD)
    inverse_step_operator = OPR_SUB;
  else
    inverse_step_operator = OPR_ADD;

  if (unrolled==FALSE) {
   // generate loop for the peeled portion
  
    WN **new_iter=CXX_NEW_ARRAY(WN*, 2, &LNO_local_pool);

    new_iter[0]=WN_do_body(in_loop);
    BOOL out_of_edge=FALSE;
    new_iter[1] = LWN_Copy_Tree(WN_do_body(in_loop),TRUE,LNO_Info_Map);
    BOOL all_internal = WN_Rename_Duplicate_Labels(WN_do_body(in_loop),
                          new_iter[1], Current_Func_Node, &LNO_local_pool);
    Is_True(all_internal, ("external labels renamed"));

                    // copy access vector
    if (!adg->Add_Deps_To_Copy_Block(new_iter[0],new_iter[1], FALSE)) {
                    // do not copy internal edges
      out_of_edge=TRUE;
      LWN_Update_Dg_Delete_Tree(new_iter[1], adg);
    }
    if (out_of_edge) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop),adg);
    }

    Unrolled_DU_Update(new_iter, 2, Do_Loop_Depth(in_loop));
    WN* last_stmt=WN_last(new_iter[0]);
    LWN_Insert_Block_Before(new_iter[0], NULL, new_iter[1]);
    DYN_ARRAY<FF_STMT_LIST> loop(&LNO_local_pool);
    loop.Newidx(); loop[0].Init(NULL);
    loop.Newidx(); loop[1].Init(NULL);
    for (wn=WN_first(new_iter[0]); wn!=last_stmt; wn=WN_next(wn)) {
      loop[0].Append(wn,&LNO_local_pool);
    }
    loop[0].Append(last_stmt,&LNO_local_pool);
    for (wn=WN_next(last_stmt); wn; wn=WN_next(wn)) {
      loop[1].Append(wn,&LNO_local_pool);
    }
    Separate_And_Update(in_loop, loop, 1, FALSE);
    WN* new_loop=WN_next(in_loop);

    scalar_rename(WN_start(new_loop));
    Get_Do_Loop_Info(new_loop)->Est_Num_Iterations= -1;
    Get_Do_Loop_Info(in_loop)->Est_Num_Iterations= -1;
    // now we have in_loop followed by the new loop
    // they are identical except that the loop index var for the
    // new_loop has been renamed
    // next we need to adjust the loop bounds

    WN_kid1(WN_end(in_loop))=			// do i=m,n-iter_count
      LWN_CreateExp2(OPCODE_make_op(OPR_SUB, index_type, MTYPE_V),
        WN_kid1(WN_end(in_loop)),
        LWN_Make_Icon(index_type,iter_count*Step_Size(in_loop)));

    if (abs(Step_Size(new_loop)==1)) {
      WN* loop_end=LWN_Copy_Tree(WN_kid1(WN_end(new_loop)),TRUE,LNO_Info_Map);
					// n
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_kid1(WN_end(new_loop)), loop_end, FALSE))
      LNO_Erase_Dg_From_Here_In(WN_kid1(WN_end(new_loop)),
        Array_Dependence_Graph);

      LWN_Copy_Def_Use(WN_kid1(WN_end(new_loop)),loop_end, Du_Mgr);
      WN_kid0(WN_start(new_loop))=
        LWN_CreateExp2(			// max(lb,lb-(iter_count-1))
          OPCODE_make_op(OPR_MAX, index_type, MTYPE_V),
          WN_kid0(WN_start(new_loop)),
          LWN_CreateExp2(		// n-iter_count+1
            OPCODE_make_op(OPR_SUB, index_type, MTYPE_V),
            loop_end,
            LWN_Make_Icon(index_type,(iter_count-1)*Step_Size(new_loop))));
    } else {
      LWN_Update_Def_Use_Delete_Tree(WN_kid0(WN_start(new_loop)),Du_Mgr);
      LWN_Delete_Tree(WN_kid0(WN_start(new_loop)));
      WN* wn_orig = Find_Node(WN_index(in_loop), WN_end(in_loop));
      wn=LWN_Copy_Tree(wn_orig);        // i
      LWN_Copy_Def_Use(wn_orig, wn, Du_Mgr);
      Du_Mgr->Ud_Get_Def(wn)->Set_loop_stmt(NULL);
      WN_kid0(WN_start(new_loop))=wn;
    }

    // parentize the loop, build access vectors and update dependences

    DOLOOP_STACK* loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                     &LNO_local_pool);
    Build_Doloop_Stack(LWN_Get_Parent(in_loop), loop_stack);
    LWN_Parentize(in_loop);
    LWN_Parentize(new_loop);
    LNO_Build_Do_Access(in_loop, loop_stack);
    LNO_Build_Access(new_loop, loop_stack, &LNO_default_pool);

    if (!adg->Build_Region(in_loop,new_loop,loop_stack,TRUE,TRUE)) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop), adg);  
    } 
  } else {

    WN *index_expr;
    WN **new_iter=CXX_NEW_ARRAY(WN*, iter_count+1, &LNO_local_pool);

    new_iter[0]=WN_do_body(in_loop);
    BOOL out_of_edge=FALSE;
    INT i;
    for (i=1; i<=iter_count; i++) {
      new_iter[i] = LWN_Copy_Tree(WN_do_body(in_loop),TRUE,LNO_Info_Map);
      BOOL all_internal = WN_Rename_Duplicate_Labels(WN_do_body(in_loop),
                            new_iter[i], Current_Func_Node, &LNO_local_pool);
      Is_True(all_internal, ("external labels renamed"));
      if (Good_Do_Depth(in_loop)>0 &&
          !adg->Add_Deps_To_Copy_Block(new_iter[0],new_iter[i], FALSE)) {
        out_of_edge=TRUE;
        LWN_Update_Dg_Delete_Tree(new_iter[i], adg);
      }
    }

    if (out_of_edge) {
      DevWarn("Array dependence graph overflowed in Pre_loop_peeling()");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(in_loop),adg);
    }
    Unrolled_DU_Update(new_iter, iter_count+1, Do_Loop_Depth(in_loop));
    WN* wn_tmp=LWN_Get_Parent(in_loop);
    while (wn_tmp && WN_opcode(wn_tmp)!=OPC_DO_LOOP)
     wn_tmp=LWN_Get_Parent(wn_tmp);
    for (i=1; i<=iter_count; i++)
      Loop_Stmt_Update(new_iter[i],in_loop,wn_tmp);

    SYMBOL kid0_symbol;
    SYMBOL kid1_symbol;
    if (WN_operator(WN_kid0(WN_end(in_loop)))==OPR_LDID)
      kid0_symbol.Init(WN_kid0(WN_end(in_loop)));
    if (WN_operator(WN_kid1(WN_end(in_loop)))==OPR_LDID)
      kid1_symbol.Init(WN_kid1(WN_end(in_loop)));
    OPERATOR opr=WN_operator(WN_end(in_loop));

    if (opr==OPR_LT) {
      if (loop_index_symbol == kid0_symbol) {
        WN_set_opcode(WN_end(in_loop),
          OPCODE_make_op(OPR_LE, Boolean_type, index_type));
        opr=OPR_LE;
        WN_kid1(WN_end(in_loop)) = LWN_CreateExp2(
          OPCODE_make_op(OPR_SUB, index_type, MTYPE_V),
          WN_kid1(WN_end(in_loop)), LWN_Make_Icon(index_type, 1));
      } else if (loop_index_symbol == kid1_symbol) {
        WN_set_opcode(WN_end(in_loop),
          OPCODE_make_op(OPR_LE, Boolean_type, index_type));
        opr=OPR_LE;
        WN_kid0(WN_end(in_loop)) = LWN_CreateExp2(
          OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
          WN_kid0(WN_end(in_loop)), LWN_Make_Icon(index_type, 1));
      }
    } else if (opr==OPR_GT) {
      if (loop_index_symbol == kid0_symbol) {
        WN_set_opcode(WN_end(in_loop),
          OPCODE_make_op(OPR_GE, Boolean_type, index_type));
        opr=OPR_GE;
        WN_kid1(WN_end(in_loop)) = LWN_CreateExp2(
          OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
          WN_kid1(WN_end(in_loop)), LWN_Make_Icon(index_type, 1));
      } else if (loop_index_symbol == kid1_symbol) {
        WN_set_opcode(WN_end(in_loop),
          OPCODE_make_op(OPR_GE, Boolean_type, index_type));
        opr=OPR_GE;
        WN_kid0(WN_end(in_loop)) = LWN_CreateExp2(
          OPCODE_make_op(OPR_SUB, index_type, MTYPE_V),
          WN_kid0(WN_end(in_loop)), LWN_Make_Icon(index_type, 1));
      }
    }

    if ((opr==OPR_GE || opr==OPR_LE) &&
        (loop_index_symbol == kid0_symbol || loop_index_symbol == kid1_symbol)) {

      // upper bound is either (i op ..) or (.. op i)

      WN* UB; 
      WN* LB=WN_kid0(WN_start(in_loop));
      WN* STEP=WN_kid1(WN_kid0(WN_step(in_loop)));
      WN* END=WN_end(in_loop);

      if (kid0_symbol!=loop_index_symbol) {
        OPERATOR new_operator;
        switch(opr) {
          case OPR_GE: new_operator=OPR_LE; break;
          case OPR_LE: new_operator=OPR_GE; break;
        }
        WN_set_opcode(END,
          OPCODE_make_op(new_operator, Boolean_type, index_type));
        WN* temp_wn=WN_kid1(END);
        WN_kid1(END)=WN_kid0(END);
        WN_kid0(END)=temp_wn;
      }
      UB=WN_kid1(END);

      if (WN_operator(STEP)==OPR_INTCONST &&
          WN_const_val(STEP)==1) {

        wn=LWN_Make_Icon(Promote_Type(index_type), 0);

      } else {

        // (UB-LB)
        WN* kid0=LWN_Copy_Tree(UB,TRUE,LNO_Info_Map);
        if (!Array_Dependence_Graph->
          Add_Deps_To_Copy_Block(UB, kid0, FALSE))
          LNO_Erase_Dg_From_Here_In(UB, Array_Dependence_Graph);
        LWN_Copy_Def_Use(UB, kid0, Du_Mgr);

        WN* kid1=LWN_Copy_Tree(LB,TRUE,LNO_Info_Map);
        if (!Array_Dependence_Graph->
          Add_Deps_To_Copy_Block(LB, kid1, FALSE))
          LNO_Erase_Dg_From_Here_In(LB, Array_Dependence_Graph);
        LWN_Copy_Def_Use(LB, kid1, Du_Mgr);

        wn = LWN_CreateExp2(
          OPCODE_make_op(OPR_SUB, index_type, MTYPE_V),
          kid0, kid1);

        // abs(UB-LB)
        wn = LWN_CreateExp1(
          OPCODE_make_op(OPR_ABS, index_type, MTYPE_V),
          wn);

        // abs(UB-LB) % STEP
        kid1=LWN_Copy_Tree(STEP,TRUE,LNO_Info_Map);
        if (!Array_Dependence_Graph->
          Add_Deps_To_Copy_Block(STEP, kid1, FALSE))
          LNO_Erase_Dg_From_Here_In(STEP, Array_Dependence_Graph);

        LWN_Copy_Def_Use(STEP, kid1, Du_Mgr);
        wn = LWN_CreateExp2(
          OPCODE_make_op(OPR_MOD, index_type, MTYPE_V),
          wn, kid1);
      }

      // UB - abs(UB-LB) % STEP
      WN* kid0=LWN_Copy_Tree(UB,TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(UB, kid0, FALSE))
        LNO_Erase_Dg_From_Here_In(UB, Array_Dependence_Graph);
      LWN_Copy_Def_Use(UB, kid0, Du_Mgr);
      wn = LWN_CreateExp2(
        OPCODE_make_op(inverse_step_operator, Promote_Type(index_type),MTYPE_V),
        kid0, wn);

    } else {
      // upper bound is not in the desired form
      // so the last peeled iteration will be i+step*(offset-1)
      // where i here is the last value of loop index after the peeled loop
    
      WN* wn1=LWN_Copy_Tree(WN_kid0(WN_kid0(WN_step(in_loop))));
      WN* wn2=
        LWN_Copy_Tree(WN_kid1(WN_kid0(WN_step(in_loop))),TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(WN_kid1(WN_kid0(WN_step(in_loop))),wn2,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid1(WN_kid0(WN_step(in_loop))),
          Array_Dependence_Graph);

      LWN_Copy_Def_Use(WN_kid0(WN_kid0(WN_step(in_loop))), wn1, Du_Mgr);
      LWN_Copy_Def_Use(WN_kid1(WN_kid0(WN_step(in_loop))), wn2, Du_Mgr);
#ifndef KEY // bug 6239
      Du_Mgr->Ud_Get_Def(wn1)->Set_loop_stmt(NULL);
#else
      if (WN_operator(wn1) != OPR_CVT)
	Du_Mgr->Ud_Get_Def(wn1)->Set_loop_stmt(NULL);
      else
	Du_Mgr->Ud_Get_Def(WN_kid0(wn1))->Set_loop_stmt(NULL);
#endif
      // wn1 is a copy of the loop var index but is not enclosed in the loop

      wn = LWN_CreateExp2(
        OPCODE_make_op(OPR_ADD, index_type, MTYPE_V),
        wn1,
        LWN_CreateExp2(OPCODE_make_op(OPR_MPY, index_type, MTYPE_V),
          wn2,
          LWN_Make_Icon(index_type, iter_count-1))
        );

    }

    LWN_Adjust_Frequency_Tree(WN_end(in_loop), -iter_count);
    LWN_Adjust_Frequency_Tree(WN_step(in_loop), -iter_count);

    // now generate peeled iteration, last one first
    index_expr = wn;
    for (i=0; i<iter_count; i++) {

      // LB <= INDEX
      WN* kid0=LWN_Copy_Tree(WN_kid0(WN_start(in_loop)),TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(WN_kid0(WN_start(in_loop)),kid0,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid0(WN_start(in_loop)),
          Array_Dependence_Graph);
      LWN_Copy_Def_Use(WN_kid0(WN_start(in_loop)), kid0, Du_Mgr);
      WN* kid1=LWN_Copy_Tree(index_expr,TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(index_expr,kid1,FALSE))
        LNO_Erase_Dg_From_Here_In(index_expr,Array_Dependence_Graph);
      LWN_Copy_Def_Use(index_expr, kid1, Du_Mgr);

      WN* guard = 
        LWN_CreateExp2(
          WN_opcode(WN_end(in_loop)),
          kid0,
          kid1);

      Replace_Ldid_With_Exp_Copy(
        loop_index_symbol,new_iter[i+1],index_expr,Du_Mgr);

      WN* if_wn=LWN_CreateIf(
          guard,					// WN *test,
          new_iter[i+1],				// WN *if_then,
          WN_CreateBlock());			// WN *if_else;
      LWN_Copy_Linenumber(in_loop, if_wn);
      
      LWN_Copy_Frequency_Tree(if_wn, in_loop);
      LWN_Insert_Block_After(parent, in_loop, if_wn);

      IF_INFO* if_info =
        CXX_NEW(IF_INFO(&LNO_default_pool,
                !Get_Do_Loop_Info(in_loop)->Is_Inner,
                Find_SCF_Inside(in_loop,OPC_REGION)!=NULL),
          &LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map, if_wn, (void*)if_info);

      DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(in_loop), loop_stack);

      LNO_Build_If_Access(if_wn, loop_stack);
      COND_IF_INFO cond_result=COND_If_Info(if_wn, &LNO_local_pool);

      if (cond_result==COND_IF_THEN_ONLY) {

        LWN_Insert_Block_After(parent, in_loop, new_iter[i+1]);
        WN_then(if_wn)=WN_CreateBlock();
        LWN_Extract_From_Block(if_wn);
        LWN_Delete_Tree(if_wn);

      } else if (cond_result==COND_IF_ELSE_ONLY) {

        LWN_Delete_Tree(if_wn);
        //LWN_Delete_From_Block(parent, if_wn);

      }

      kid1=LWN_Copy_Tree(WN_kid1(WN_kid0(WN_step(in_loop))),TRUE,LNO_Info_Map);
      if (!Array_Dependence_Graph->
        Add_Deps_To_Copy_Block(WN_kid1(WN_kid0(WN_step(in_loop))),kid1,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid1(WN_kid0(WN_step(in_loop))),
          Array_Dependence_Graph);

      LWN_Copy_Def_Use(WN_kid1(WN_kid0(WN_step(in_loop))), kid1, Du_Mgr);

      index_expr = LWN_CreateExp2(
        OPCODE_make_op(inverse_step_operator, Promote_Type(index_type),MTYPE_V),
        index_expr,
        kid1);

    }

    LWN_Delete_Tree(index_expr);

    Add_To_Symbol(WN_end(in_loop), iter_count, loop_index_symbol);
    // this assumes that the loop is normalized, otherwise, it
    // should be i+(iter_count*step)

    Upper_Bound_Standardize(WN_end(in_loop),/*ok_to_fail*/TRUE);
      
    DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                     &LNO_local_pool);

    Build_Doloop_Stack(LWN_Get_Parent(in_loop), loop_stack);

    Get_Do_Loop_Info(in_loop)->Est_Num_Iterations= -1;
    LNO_Build_Do_Access(in_loop, loop_stack);

    for (wn=WN_next(in_loop); wn!=next_stmt; wn=WN_next(wn)) {
      LNO_Build_Access(wn, loop_stack, &LNO_default_pool);
      Remark_Depth(wn, loop_stack->Elements());
    }

    WN *end_stmt;
    if (next_stmt) {
      end_stmt = WN_prev(next_stmt);
    } else {
      WN* wnn = NULL; 
      for (WN* wn = in_loop; wn != NULL; wn = WN_next(wn)) 
        wnn = wn; 
      end_stmt = wnn; 
      FmtAssert(end_stmt != NULL, 
	("Post_loop_peeling: Could not find last statement"));
    }

    if (Enclosing_Do_Loop(LWN_Get_Parent(in_loop))) { 
      adg->Build_Region(in_loop, end_stmt, loop_stack, TRUE, TRUE);
    } else { 
      for (wn = in_loop; wn != WN_next(end_stmt); wn = WN_next(wn))  
	Build_Regions(in_loop, wn, loop_stack);
      
    } 
  }

  MEM_POOL_Pop(&LNO_local_pool);

  SRCPOS srcpos=WN_Get_Linenum(in_loop);
  if (LNO_Verbose)
    post_peeling_verbose_info(srcpos,iter_count);
  if (LNO_Analysis)
    post_peeling_analysis_info(srcpos,iter_count);
  if (LNO_Tlog)
    post_peeling_tlog_info(in_loop,iter_count);
}

extern BOOL Move_Adjacent(WN* stmt1, WN* stmt2, BOOL move_down_only) {

  ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph; // LNO array graph
  WN* parent;
  if (stmt1 && stmt2) {
    if (WN_next(stmt1)==stmt2) return TRUE;
    parent=LWN_Get_Parent(stmt1);
    if (LWN_Get_Parent(stmt2)!=parent) {
      DevWarn("Input statements have different parents in Move_Adjacent()");
      return FALSE;
    }
  } else if (stmt1) {
    if (WN_next(stmt1)==stmt2) return TRUE;
    else if (!WN_next(stmt1)) return FALSE;
    parent=LWN_Get_Parent(stmt1);
  } else if (stmt2) {
    if (WN_prev(stmt2)==stmt1) return TRUE;
    else if (!WN_prev(stmt2)) return FALSE;
    parent=LWN_Get_Parent(stmt2);
  } else return FALSE;

  WN* parent_loop=LWN_Get_Parent(parent);
  if (WN_opcode(parent_loop)!=OPC_DO_LOOP || !Do_Loop_Is_Good(parent_loop) ||
		Do_Loop_Has_Gotos(parent_loop)) {
    // Is_True(0,("Statements not enclosed by a loop."));
    return FALSE;
  }

  MEM_POOL_Push(&FUSION_default_pool);

  WN_MAP sdm=WN_MAP_Create(&FUSION_default_pool);

  ARRAY_DIRECTED_GRAPH16* sdg=Build_Statement_Dependence_Graph(
	parent_loop, red_manager, adg, sdm,
        &FUSION_default_pool);
  Statement_Dependence_Graph = sdg; 
  if (sdg==NULL) {
    DevWarn("Statement dependence graph problem");
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&FUSION_default_pool);
    return FALSE;
  }

  if (stmt1 && !move_down_only) {
    INT2PTR* up_hash_table=
      CXX_NEW(INT2PTR(50, &FUSION_default_pool),&FUSION_default_pool);
    up_hash_table->Enter((INT)sdg->Get_Vertex(stmt1), (void*)1);
    WN* next_stmt=NULL;
    WN* stmt = 0;
    for (stmt=WN_next(stmt1); stmt && stmt != stmt2;
       stmt=next_stmt) {
       next_stmt = WN_next(stmt);
       VINDEX16 stmt_v = sdg->Get_Vertex(stmt);
       if (stmt_v==0) {
         // conservative assumption, statement depends on everything
         CXX_DELETE(sdg,&FUSION_default_pool);
         WN_MAP_Delete(sdm);
         MEM_POOL_Pop(&FUSION_default_pool);
         return FALSE;
       }

       EINDEX16 dep_e = sdg->Get_In_Edge(stmt_v);
       BOOL can_be_moved_up = TRUE;
       while (dep_e) {
	 if (sdg->Get_Level_Property(dep_e,HAS_ALL_ZERO)) {
           VINDEX16 source_v = sdg->Get_Source(dep_e);
	   if (up_hash_table->Find((INT)source_v)) {
	     can_be_moved_up = FALSE;
             up_hash_table->Enter((INT)stmt_v, (void*)1);
	     break;  // has to stay in between stmt1 and stmt2
	   }
	 }
	 dep_e = sdg->Get_Next_In_Edge(dep_e);
       }
#ifdef KEY
       // Bug 5206 - do not move a barrier pragma to move loops together.
       if (WN_operator(stmt) == OPR_PRAGMA &&
	   WN_pragma(stmt) == WN_PRAGMA_BARRIER)
	 can_be_moved_up = FALSE;
#endif
       if (can_be_moved_up) {
	 LWN_Insert_Block_Before(parent,stmt1,LWN_Extract_From_Block(stmt));
       }
    }
    Is_True(stmt==stmt2, ("Incorrect order of input in Move_Adjacent()\n"));
    if (stmt!=stmt2) {
      CXX_DELETE(sdg,&FUSION_default_pool);
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&FUSION_default_pool);
      return FALSE;
    }
  }

  if (stmt2) {
    INT2PTR* down_hash_table=
      CXX_NEW(INT2PTR(50, &FUSION_default_pool),&FUSION_default_pool);
    down_hash_table->Enter((INT)sdg->Get_Vertex(stmt2), (void*)1);
    WN* prev_stmt=NULL;
    WN* stmt = 0;
    for (stmt=WN_prev(stmt2); stmt && stmt != stmt1;
       stmt=prev_stmt) {
       prev_stmt = WN_prev(stmt);
       VINDEX16 stmt_v = sdg->Get_Vertex(stmt);
       if (stmt_v==0) {
         // conservative assumption, statement depends on everything
         CXX_DELETE(sdg,&FUSION_default_pool);
         WN_MAP_Delete(sdm);
         MEM_POOL_Pop(&FUSION_default_pool);
         return FALSE;
       }
       EINDEX16 dep_e = sdg->Get_Out_Edge(stmt_v);
       BOOL can_be_moved_down = TRUE;
       while (dep_e) {
	 if (sdg->Get_Level_Property(dep_e,HAS_ALL_ZERO)) {
           VINDEX16 sink_v = sdg->Get_Sink(dep_e);
	   if (down_hash_table->Find((INT)sink_v)) {
	     can_be_moved_down = FALSE;
             down_hash_table->Enter((INT)stmt_v, (void*)1);
	     break;  // has to stay in between stmt1 and stmt2
	   }
	 }
	 dep_e = sdg->Get_Next_Out_Edge(dep_e);
       }
#ifdef KEY
       // Bug 5206 - do not move a barrier pragma to move loops together.
       if (WN_operator(stmt) == OPR_PRAGMA &&
	   WN_pragma(stmt) == WN_PRAGMA_BARRIER)
	 can_be_moved_down = FALSE;
#endif
       if (can_be_moved_down) {
	 LWN_Insert_Block_After(parent,stmt2,LWN_Extract_From_Block(stmt));
       }
    }
    Is_True(stmt==stmt1, ("Incorrect order of input in Move_Adjacent()\n"));
    if (stmt!=stmt1) {
      CXX_DELETE(sdg,&FUSION_default_pool);
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&FUSION_default_pool);
      return FALSE;
    }
  }

  Statement_Dependence_Graph = NULL; 
  CXX_DELETE(sdg,&FUSION_default_pool);
  WN_MAP_Delete(sdm);
  MEM_POOL_Pop(&FUSION_default_pool);

  if (stmt1 && WN_next(stmt1)==stmt2)
    return TRUE;
  else if (stmt2 && WN_prev(stmt2)==stmt1)
    return TRUE;
  else
    return FALSE;
}


// Changes def for uses of loop variable to the additional assignment
// because the original loop start may be changed after fusion
/*
static void Update_Du_For_Loop_Start(DU_MANAGER* Du_Mgr,
  WN* loop1, WN* loop2, WN* lv1, WN* lv2)
{

  MEM_POOL_Push(&FUSION_default_pool);

  // first work on the uses of first loop's index variable
  USE_LIST *use_list=Du_Mgr->Du_Get_Use(WN_start(loop1));
  USE_LIST_ITER u_iter1(use_list);
  for (DU_NODE *use_node=(DU_NODE *)u_iter1.First();
       !u_iter1.Is_Empty(); ) {
      WN* use=use_node->Wn();
      use_node=(DU_NODE *)u_iter1.Next();
      WN* new_loop=use;
      BOOL add_def_use=TRUE;
      while (new_loop !=loop1 && WN_opcode(new_loop)!=OPC_FUNC_ENTRY) {
          new_loop=LWN_Get_Parent(new_loop);
          if (new_loop==lv1 || new_loop==lv2)
            add_def_use=FALSE;
      }
      if (add_def_use && new_loop!=loop1) {
        // a use outside the first loop
        Du_Mgr->Delete_Def_Use(WN_start(loop1),use);
        Du_Mgr->Delete_Def_Use(WN_step(loop1),use);
        Du_Mgr->Add_Def_Use(lv1,use);
      }
  }

  // then work on the uses of second loop's index variable
  USE_LIST *use_list2=Du_Mgr->Du_Get_Use(WN_start(loop2));
  USE_LIST_ITER u_iter2(use_list2);
  for (use_node=(DU_NODE *)u_iter2.First(); !u_iter2.Is_Empty(); ) {
      WN* use=use_node->Wn();
      use_node=(DU_NODE *)u_iter2.Next();
      WN* new_loop=use;
      while (new_loop !=loop2 && WN_opcode(new_loop)!=OPC_FUNC_ENTRY)
          new_loop=LWN_Get_Parent(new_loop);
      if (new_loop!=loop2) {
        // a use outside the second loop
        Du_Mgr->Add_Def_Use(lv2,use);
        Du_Mgr->Delete_Def_Use(WN_start(loop2),use);
        Du_Mgr->Delete_Def_Use(WN_step(loop2),use);
      }
  }

  MEM_POOL_Pop(&FUSION_default_pool);

}
*/

// Updates DU-chains for references in different loops
// DU-chains have changed for def in one loop and use in another
// and vice versa

static void Fusion_Du_Update(
       WN** loop_nest1,
       WN** loop_nest2,
       DU_MANAGER* Du_Mgr)
{
  MEM_POOL_Push(&FUSION_default_pool);
  // add extra scope so that stack vars are deallocated before pop
  {

    REF_LIST_STACK *writes1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads1 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads1 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(loop_nest1[0], stack1);

    REF_LIST_STACK *writes2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_REF_STACK *params1 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    SCALAR_REF_STACK *params2 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    DOLOOP_STACK *stack2=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(loop_nest2[0], stack2);

    // Make a list of all the references
    Init_Ref_Stmt_Counter();
    WN* loop_body=WN_do_body(loop_nest1[0]);
    WN* wn = 0;
    for (wn=WN_first(loop_body); wn; wn=WN_next(wn)) {
      New_Gather_References(wn,writes1,reads1,stack1,
        scalar_writes1,scalar_reads1,
        params1,&FUSION_default_pool,
	Gather_Scalar_Refs | Gather_Params);
    }

    // Make a list of all the references
    loop_body=WN_do_body(loop_nest2[0]);
    for (wn=WN_first(loop_body); wn; wn=WN_next(wn)) {
      New_Gather_References(wn,writes2,reads2,stack2,
        scalar_writes2,scalar_reads2,
        params2,&FUSION_default_pool,
	Gather_Scalar_Refs | Gather_Params);
    }

    // Update DU chains
    // Theoretically, only scalar with reduction reads and writes
    // can appear in two loops to be fused, given high enough roundoff
    // tolerance. But we also want to handle fusion ordered by user
    // where the legality test is suppressed.

    // First, add DU chains for all writes from the second loop
    // to all reads in the first loop
    INT si;
    for (si=0; si<scalar_writes2->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_writes2->Bottom_nth(si);
      WN* write=sinode->Bottom_nth(0)->Wn;
      for (INT sj=0; sj<scalar_reads1->Elements(); sj++) {
        SCALAR_NODE* sjnode=scalar_reads1->Bottom_nth(sj);
        WN* read=sjnode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,write,read)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write=sinode->Bottom_nth(sii)->Wn;
            for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
              read=sjnode->Bottom_nth(sjj)->Wn;
              Du_Mgr->Delete_Def_Use(write,read);
              // try delete first to avoid duplicate edges
              Du_Mgr->Add_Def_Use(write,read);
            }
          }
        }
      }
    }
    // Second, add DU chains for all writes from the first loop
    // to all reads in the second loop
    for (si=0; si<scalar_writes1->Elements(); si++) {
      SCALAR_NODE* sinode=scalar_writes1->Bottom_nth(si);
      WN* write=sinode->Bottom_nth(0)->Wn;
      for (INT sj=0; sj<scalar_reads2->Elements(); sj++) {
        SCALAR_NODE* sjnode=scalar_reads2->Bottom_nth(sj);
        WN* read=sjnode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,write,read)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write=sinode->Bottom_nth(sii)->Wn;
            for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
              read=sjnode->Bottom_nth(sjj)->Wn;
              Du_Mgr->Delete_Def_Use(write,read);
              // try delete first to avoid duplicate edges
              Du_Mgr->Add_Def_Use(write,read);
            }
          }
        }
      }
    }
    INT sj;
    for (sj=0; sj<params1->Elements(); sj++) {
      SCALAR_REF sjnode=params1->Bottom_nth(sj);
      WN* param=sjnode.Wn;
      INT si;
      for (si=0; si<scalar_writes1->Elements(); si++) {
        SCALAR_NODE* sinode=scalar_writes1->Bottom_nth(si);
        WN* write=sinode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,param,write)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write=sinode->Bottom_nth(sii)->Wn;
            Du_Mgr->Delete_Def_Use(write,param);
            // try delete first to avoid duplicate edges
            Du_Mgr->Add_Def_Use(write,param);
          }
        }
      }
      for (si=0; si<scalar_writes2->Elements(); si++) {
        SCALAR_NODE* sinode=scalar_writes2->Bottom_nth(si);
        WN* write=sinode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,param,write)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            write=sinode->Bottom_nth(sii)->Wn;
            Du_Mgr->Delete_Def_Use(write,param);
            // try delete first to avoid duplicate edges
            Du_Mgr->Add_Def_Use(write,param);
          }
        }
      }
      for (si=0; si<scalar_reads1->Elements(); si++) {
        SCALAR_NODE* sinode=scalar_reads1->Bottom_nth(si);
        WN* read=sinode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,param,read)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            read=sinode->Bottom_nth(sii)->Wn;
            Du_Mgr->Delete_Def_Use(LWN_Get_Parent(param),read);
            // try delete first to avoid duplicate edges
            Du_Mgr->Add_Def_Use(LWN_Get_Parent(param),read);
          }
        }
      }
      for (si=0; si<scalar_reads2->Elements(); si++) {
        SCALAR_NODE* sinode=scalar_reads2->Bottom_nth(si);
        WN* read=sinode->Bottom_nth(0)->Wn;
        if (Aliased(Alias_Mgr,param,read)!=NOT_ALIASED) {
          for (INT sii=0; sii<sinode->Elements(); sii++) {
            read=sinode->Bottom_nth(sii)->Wn;
            Du_Mgr->Delete_Def_Use(LWN_Get_Parent(param),read);
            // try delete first to avoid duplicate edges
            Du_Mgr->Add_Def_Use(LWN_Get_Parent(param),read);
          }
        }
      }
    }

  }

  MEM_POOL_Pop(&FUSION_default_pool);
}

// Update Loop_stmt info in the def_list of uses
// Basically, the uses of the second loop nests will have to change
// Loop_stmt info to point to the corresponding nest in the
// first loop after fusion

static void Fusion_Loop_Stmt_Update(
       WN** loop_nest1,
       WN** loop_nest2,
       UINT fusion_level, 
       DU_MANAGER* Du_Mgr)
{
  MEM_POOL_Push(&FUSION_default_pool);
  // add extra scope so that stack vars are deallocated before pop
  {

    REF_LIST_STACK *writes2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                     &FUSION_default_pool);
    REF_LIST_STACK *reads2 = CXX_NEW(REF_LIST_STACK(&FUSION_default_pool),
                                    &FUSION_default_pool);
    SCALAR_STACK *scalar_writes2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_STACK *scalar_reads2 = CXX_NEW(SCALAR_STACK(&FUSION_default_pool),
                                          &FUSION_default_pool);
    SCALAR_REF_STACK *params2 =
          CXX_NEW(SCALAR_REF_STACK(&FUSION_default_pool), &FUSION_default_pool);
    DOLOOP_STACK *stack2=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                 &FUSION_default_pool);
    Build_Doloop_Stack(loop_nest2[0], stack2);

    // Make a list of all the references
    Init_Ref_Stmt_Counter();
    WN* loop_body=WN_do_body(loop_nest2[0]);
    for (WN* wn=WN_first(loop_body); wn; wn=WN_next(wn)) {
      New_Gather_References(wn,writes2,reads2,stack2,
        scalar_writes2,scalar_reads2,
        params2,&FUSION_default_pool,Gather_Scalar_Refs);
    }

    SYMBOL *loop2_var_symbol=
      CXX_NEW_ARRAY(SYMBOL, fusion_level, &FUSION_default_pool);
    for (INT i=0; i<fusion_level; i++)
      loop2_var_symbol[i].Init(WN_index(loop_nest2[i]));

    INT si;
    for (si=0; si<scalar_reads2->Elements(); si++) {
      SCALAR_NODE* snode=scalar_reads2->Bottom_nth(si);
      SYMBOL read_symbol(snode->Bottom_nth(0)->Wn);

      // first we test to see if this is a read for a loop index var.
      INT i;
      for (i=0; i<fusion_level; i++)
        if (loop2_var_symbol[i]==read_symbol)
          break;
      if (i!=fusion_level)  // a loop index var read, do not update loop_stmt
          continue;

      for (INT sj=0; sj<snode->Elements(); sj++) {
        WN* read=snode->Bottom_nth(sj)->Wn;
        DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(read);
        WN* old_loop_stmt=def_list->Loop_stmt();

        // find out which level of old loopnest2
        // corresponds to the original loop_stmt
        for (i=0; i<fusion_level; i++)
          if (old_loop_stmt==loop_nest2[i])
            break;

        if (i==fusion_level)
          continue;
        else if (i!=0)
          FmtAssert(0, ("Strange Loop_Stmt for use in the fused loop."));
          // the reason is, in a multi-level fusion where fusion_level>1
          // e.g., a loop_nest2 such as
          // do i1
          //   s=          <-- S1
          //   do i        <-- old loop_stmt
          //     ..=s+..
          //     s=..
          // In this case, this loop_nest cannot be fused with
          // multi-level fusion because statement S1 would sit between
          // the two inner loop nest to be fused. It can be fused
          // level-by-level which implies that old loop_stmt will always
          // be that of level 0.
        else
          def_list->Set_loop_stmt(loop_nest1[0]);
      }
    }
  }

  MEM_POOL_Pop(&FUSION_default_pool);
}

// Remove conditional branches in the given WHIRL tree whose condition expressions can be
// evaluated to a TRUE or FALSE according to lower/upper bounds of the given loop 'loop'.
void
Remove_Cond_Branch(WN * wn, WN * loop)
{
  OPERATOR opr = WN_operator(wn);
  WN * wn_next;
  switch (opr) {
  case OPR_BLOCK:
    for (WN * wn_iter = WN_first(wn); wn_iter; wn_iter = wn_next) {
      wn_next = WN_next(wn_iter);
      Remove_Cond_Branch(wn_iter, loop);
    }
    break;
  case OPR_DO_LOOP:
    Remove_Cond_Branch(WN_do_body(wn), loop);
    break;
  case OPR_REGION:
    Remove_Cond_Branch(WN_region_body(wn), loop);
    break;
  case OPR_DO_WHILE:
  case OPR_WHILE_DO:
    Remove_Cond_Branch(WN_while_body(wn), loop);
    break;
  case OPR_IF: 
    {
      WN * wn_else = WN_else(wn);
      WN * wn_then = WN_then(wn);
      WN * wn_cond = WN_if_test(wn);
      WN * wn_block = LWN_Get_Parent(wn);

      if (Removable_branch(loop, WN_if_test(wn), NULL, NULL, FALSE)) {
	LWN_Insert_Block_Before(wn_block, WN_next(wn), wn_else);
	LWN_Update_Def_Use_Delete_Tree(wn_then, Du_Mgr);
	LWN_Update_Def_Use_Delete_Tree(wn_cond, Du_Mgr);
	LWN_Delete_Tree(wn_then);
	LWN_Delete_Tree(wn_cond);
	LWN_Delete_From_Block(wn_block, wn);
      }
      else if (Removable_branch(loop, WN_if_test(wn), NULL, NULL, TRUE)) {
	LWN_Insert_Block_Before(wn_block, WN_next(wn), wn_then);
	LWN_Update_Def_Use_Delete_Tree(wn_else, Du_Mgr);
	LWN_Update_Def_Use_Delete_Tree(wn_cond, Du_Mgr);
	LWN_Delete_Tree(wn_else);
	LWN_Delete_Tree(wn_cond);
	LWN_Delete_From_Block(wn_block, wn);
      }
      else {
	Remove_Cond_Branch(WN_then(wn), loop);
	Remove_Cond_Branch(WN_else(wn), loop);
      }
    }
    break;
  default:
    ;
  }
}

// Given a pair of def and use 'wn_def' and 'wn_use', add nodes and edge to 'dg'.
static void
Add_Dep_Graph_U(ARRAY_DIRECTED_GRAPH16 * dg, WN * wn_def, WN * wn_use)
{
  VINDEX16 v_def = dg->Get_Vertex(wn_def);
  if (!v_def)
    v_def = dg->Add_Vertex(wn_def);
  VINDEX16 v_use = dg->Get_Vertex(wn_use);
  if (!v_use)
    v_use = dg->Add_Vertex(wn_use);
  if (!dg->Get_Edge(v_def, v_use))
    dg->Add_Edge(v_def, v_use, 0, FALSE);
}

// Given 'dg', query whether there exists a sink node of 'def' that dominates 'use'.
static BOOL 
Has_Dominate_Use(ARRAY_DIRECTED_GRAPH16 * dg, WN * def, WN * use)
{
  VINDEX16 v = dg->Get_Vertex(def);
  if (v) {
    WN * stmt = Find_Containing_Store(use);
    if (!stmt)
      return FALSE;

    for (EINDEX16 edge = dg->Get_Out_Edge(v); edge; edge = dg->Get_Next_Out_Edge(edge)) {
      VINDEX16 v_sink = dg->Get_Sink(edge);
      WN * sink = dg->Get_Wn(v_sink);
      WN * sink_stmt = Find_Containing_Store(sink);
      if (sink_stmt && ((sink_stmt == stmt) || Dominates(sink_stmt, stmt)))
	return TRUE;
    }
  }

  return FALSE;
}

// Walk WHIRL tree 'wn', find nodes mapped in 'LNO_Precom_Map", if the node is a dominating use,
// connect it to its def in 'dg'. Limited to the case that defs and uses have the same enclosing
// loop.
static void
Connect_Def_Use(ARRAY_DIRECTED_GRAPH16 * dg, WN * wn) {
  OPERATOR opr = WN_operator(wn);
  
  if (opr == OPR_BLOCK) {
    for (WN * wn_iter = WN_first(wn); wn_iter; wn_iter = WN_next(wn_iter)) {
      Connect_Def_Use(dg, wn_iter);
    }
  }
  else if (OPERATOR_is_load(opr)) {
    if (WN_MAP_Get(LNO_Precom_Map, wn)) {
      ARRAY_DIRECTED_GRAPH16 * adg = Array_Dependence_Graph;
      VINDEX16 v = adg->Get_Vertex(wn);
      WN * loop_u = Enclosing_Loop(wn);

      for (EINDEX16 edge = adg->Get_In_Edge(v); edge; edge = adg->Get_Next_In_Edge(edge)) {
	VINDEX16 v_def = adg->Get_Source(edge);
	WN * wn_def = adg->Get_Wn(v_def);
	if (!OPERATOR_is_store(WN_operator(wn_def)))
	  continue;

	WN * loop_d = Enclosing_Loop(wn_def);
	if (loop_d != loop_u) 
	  continue;
	
	if (!Has_Dominate_Use(dg, wn_def, wn))
	  Add_Dep_Graph_U(dg, wn_def, wn);
      }
    }
  }
  else {
    if (OPERATOR_is_store(opr)) 
      Connect_Def_Use(dg, WN_kid0(wn));
    else {
      for (int i = 0; i < WN_kid_count(wn); i++) 
	Connect_Def_Use(dg, WN_kid(wn, i));
    }
  }
}

// Pass down Prefer_Fuse bit to the immediate child loop.
extern BOOL Pass_Child_Prefer_Fuse(WN * wn)
{
  DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn);
  if (dli->Prefer_Fuse == 1) {
    WN * wn_child = WN_first(WN_do_body(wn));
    if (wn_child && (WN_operator(wn_child) == OPR_DO_LOOP)) {
      DO_LOOP_INFO * dli_child = Get_Do_Loop_Info(wn_child);
      if (dli_child)
	dli_child->Prefer_Fuse = 1;
    }
  }
}

// Walk input statements of 'wn', build dependence graph.
static void Build_Dep_Graph(ARRAY_DIRECTED_GRAPH16 * dg, WN * wn)
{
  int kid_count = WN_kid_count(wn);
  OPERATOR opr = WN_operator(wn);

  if (OPERATOR_is_store(opr))
    kid_count = 1;
  else if (OPERATOR_is_load(opr))
    kid_count = 0;

  for (int i = 0; i < kid_count; i++) {
    WN * wn_kid = WN_kid(wn, i);
    VINDEX16 v = Array_Dependence_Graph->Get_Vertex(wn_kid);
    if (v) {
      FmtAssert(OPERATOR_is_load(WN_operator(wn_kid)), ("Expect a load."));
      WN * stmt = Find_Containing_Store(wn_kid);
      for (EINDEX16 in_edge=Array_Dependence_Graph->Get_In_Edge(v); 
	   in_edge; in_edge=Array_Dependence_Graph->Get_Next_In_Edge(in_edge)) {
	VINDEX16 v_src = Array_Dependence_Graph->Get_Source(in_edge);
	WN * wn_src = Array_Dependence_Graph->Get_Wn(v_src);
	FmtAssert(OPERATOR_is_store(WN_operator(wn_src)), ("Expect a store"));
	Add_Dep_Graph_U(dg, wn_src, stmt);
	Build_Dep_Graph(dg, wn_src);
      }
    }
    Build_Dep_Graph(dg, wn_kid);
  }
}

// Starting from 'wn', walk input edges of 'dg', map nodes to 'id'.
static void Grow_Cluster(ARRAY_DIRECTED_GRAPH16 * dg, WN * wn, WN_MAP map, UINT id)
{
  if (!WN_MAP_Get(map, wn)) {
    VINDEX16 v = dg->Get_Vertex(wn);
    if (v) {
      WN_MAP_Set(map, wn, (void *) (INTPTR) id);
      for (EINDEX16 e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) {
	VINDEX16 v_src = dg->Get_Source(e);
	WN * wn_src = dg->Get_Wn(v_src);
	Grow_Cluster(dg, wn_src, map, id);
      }
    }
  }
}

// Instantiate expressions that belong to the same cluster as 'use' and insert them 
// before 'use' assuming all such expressions are contained in 'block'.
static void Instantiate_Def(ARRAY_DIRECTED_GRAPH16 * dg, WN * use, WN_MAP map, WN * block)
{
  VINDEX16 v = dg->Get_Vertex(use);
  if (v) {
    WN * ins = Find_Containing_Store(use);
    UINT use_cluster = (UINT) (INTPTR) WN_MAP_Get(map, use);
    WN_MAP ins_map = WN_MAP_Create(&FUSION_default_pool);
    
    for (VINDEX16 v = dg->Get_Vertex(); v; v = dg->Get_Next_Vertex(v)) {
      if (!dg->Get_Out_Edge(v)) {
	WN * wn = dg->Get_Wn(v);
	UINT id = (UINT) (INTPTR) WN_MAP_Get(map, wn);
	if (id == use_cluster)
	  Grow_Cluster(dg, wn, ins_map, id);
      }
    }

    WN * wn_next;
    WN * first_def = NULL;
    for (WN * wn_iter = WN_first(block); wn_iter; wn_iter = wn_next) {
      wn_next = WN_next(wn_iter);
      if (wn_iter == first_def)
	break;

      if (wn_iter != ins) {
	UINT id = (UINT) (INTPTR) WN_MAP_Get(ins_map, wn_iter);
	if (id == use_cluster) {
	  UINT def_cluster = (UINT) (INTPTR) WN_MAP_Get(map, wn_iter);
	  WN * def = NULL;
	  if (def_cluster == use_cluster) 
	    def = LWN_Extract_From_Block(wn_iter);
	  else {
	    def = LWN_Copy_Tree(wn_iter, TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(wn_iter, def, Du_Mgr);
	  }
	  if (!first_def)
	    first_def = def;

	  LWN_Insert_Block_Before(NULL, ins, def);
	}
      }   
    }

    WN_MAP_Delete(ins_map);
  }
}

// Instantiate pre-computation definitions at the first dominating appearances of uses.
static BOOL Instantiate_Defs(WN * wn)
{
  WN_MAP sdm = WN_MAP_Create(&FUSION_default_pool);
  ARRAY_DIRECTED_GRAPH16 * dg = CXX_NEW(ARRAY_DIRECTED_GRAPH16(100, 500, sdm, DEP_ARRAY_GRAPH),
					&FUSION_default_pool);

  // Connect defs and uses of precomputation expressions.
  Connect_Def_Use(dg, wn);

  if (!dg->Get_Vertex()) {
    Delete_Dep_Graph(dg, NULL, 0);
    return FALSE;
  }
  
  WN * block = NULL;
  // Complete dependence graph.
  for (VINDEX16 v = dg->Get_Vertex(); v; v = dg->Get_Next_Vertex(v)) {
    if (dg->Get_Out_Edge(v)) {
      WN * wn_iter = dg->Get_Wn(v);      
      Build_Dep_Graph(dg, wn_iter);
      WN * parent = LWN_Get_Parent(wn_iter);
      FmtAssert((!block || (block == parent)), ("Unexpected def."));
      block = parent;
    }
  }

  DYN_ARRAY<WN *> * first_uses = CXX_NEW(DYN_ARRAY<WN *>(&FUSION_default_pool),
				   &FUSION_default_pool);
  WN_MAP cluster_map = WN_MAP_Create(&FUSION_default_pool);
  UINT id = 0;

  // Find clusters.
  for (VINDEX16 v = dg->Get_Vertex(); v; v = dg->Get_Next_Vertex(v)) {
    if (!dg->Get_Out_Edge(v)) {
      WN * wn_iter = dg->Get_Wn(v);
      WN * stmt_iter = Find_Containing_Store(wn_iter);
      UINT cur_id = 0;

      for (int i = 0; i < first_uses->Elements(); i++) {
	WN * use = (*first_uses)[i];
	WN * stmt = Find_Containing_Store(use);
	BOOL is_first_use = Dominates(stmt_iter, stmt) ? TRUE : FALSE;

	if ((stmt == stmt_iter) || Dominates(stmt, stmt_iter) || is_first_use) {
	  cur_id = (UINT) (INTPTR) WN_MAP_Get(cluster_map, use);
	  if (is_first_use) 
	    first_uses->ReplaceElement(i, wn_iter);
	  break;
	}
      }

      if (!cur_id) {
	cur_id = (++id);
	first_uses->AddElement(wn_iter);
      }

      Grow_Cluster(dg, wn_iter, cluster_map, cur_id);
    }
  }

  for (int i = first_uses->Elements() - 1; i >= 0; i--) {
    WN * use = (*first_uses)[i];
    UINT id = (UINT) (INTPTR) WN_MAP_Get(cluster_map, use);
    WN * stmt = Find_Containing_Store(use);
    Instantiate_Def(dg, use, cluster_map, block);
  }

  CXX_DELETE(first_uses, &FUSION_default_pool);
  WN_MAP_Delete(cluster_map);
  Delete_Dep_Graph(dg, NULL, 0);

  return TRUE;
}

// multiply-nesting fusion routine.
// returns TRUE if fusion is done, FALSE otherwise.
// in_loop1 and in_loop2 are outer-most loop of the two loop nests to be
// fused. fusion_level specify how many levels to be fused.
// threshold limits number of peeled iterations in either prolog or epilog.
// prolog_out, epilog_out and epilog_loop_out are the number of peeled
// iterations in prolog (which is always from in_loop1), epilog (which
// can be from in_loop1 or in_loop2) and the loop which results in
// iterations in epilog.
// an array offset_out[] which gives the offset at each level is returned.
// Returns prolog==epilog==offset[inner_most_level]=MAX_INT32 if the loop
// nests are not in the fuse-able form.
// all of the prolog, epilog, epilog_loop and offset can be given so
// that no new dependence analysis is done to compute these info.
// if offset_out==NULL or offset_out[inner_most_level]== MAX_INT32,
// dependence analysis is performed.
// if offset_out != NULL, the final offset info is copied out.
//
// This routine also does pre-computation transformations to enable scalar replacement.
// Original code:
// do i=2,n-1
//   a[i] = b[i]
// end do
// do i=2,n-1
//   c[i] = a[i+1] + a[i]
// end do
//
// after pre-computation transformation:
// do i=2,n-2
//   a[i] = b[i]
//   a[i+1] = b[i+1]  <==  add extra computation/rematerization to remove loop-carried dependences.
//  c[i] = a[i+1] + a[i]
// end do
//
// do i=n-1
// c[i] = a[i+1] + a[i]  <=  post-peeling to keep the orginal semantics.
// end do
// 
// Thus the transformation enables scalar replacement in sclrze.cxx
// do i=2,n-2
//   t1 = b[i]    
//   t2 = b[i+1]  
//   c[i] = t2 + t1
// end do
// do i=n-1
//   c[i] = a[i+1] + a[i]
// end do
// Notice that the pre-computation transformation introduces extra computations. It is only
// useful for programs that are stressed with high memory bandwidths and can afford
// sacrificing some CPU cycles to reduce bandwidth demand.  We currently enable this under
// "-mso" and use a simple heuristic that limits number of scalar replacement patterns to
// 9 or above.
extern FISSION_FUSION_STATUS
Fuse(WN* in_loop1, WN* in_loop2, mUINT8 fusion_level,
UINT32 threshold, BOOL peeling_unrolled,
UINT64 *prolog_out, UINT64 *epilog_out,
WN** epilog_loop_out, mINT32 offset_out[])
{
  ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph; // LNO array graph
  WN* loop_nest1[LNO_MAX_DO_LOOP_DEPTH];
  WN* loop_nest2[LNO_MAX_DO_LOOP_DEPTH];
  INT32 i;

  char loop1_var_name[80];
  char loop2_var_name[80];
  if (strlen(ST_name(WN_st(WN_index(in_loop1))))>=80) {
    DevWarn("Loop var name %s too long",ST_name(WN_st(WN_index(in_loop1))));
    strcpy(loop1_var_name,"name_too_long");
  } else
    strcpy(loop1_var_name,ST_name(WN_st(WN_index(in_loop1))));
  if (strlen(ST_name(WN_st(WN_index(in_loop2))))>=80) {
    DevWarn("Loop var name %s too long",ST_name(WN_st(WN_index(in_loop2))));
    strcpy(loop2_var_name,"name_too_long");
  } else
    strcpy(loop2_var_name,ST_name(WN_st(WN_index(in_loop2))));

  SRCPOS srcpos1=WN_Get_Linenum(in_loop1);
  SRCPOS srcpos2=WN_Get_Linenum(in_loop2);

  FmtAssert(WN_opcode(in_loop1)==OPC_DO_LOOP, 
    ("non-loop input node in Fuse()\n") );
  FmtAssert(WN_opcode(in_loop2)==OPC_DO_LOOP, 
    ("non-loop input node in Fuse()\n") );
  FmtAssert(threshold < MAX_INT32,
    ("Alignment offset threshold too large (%d)\n", threshold) );

  DO_LOOP_INFO *dli1=Get_Do_Loop_Info(in_loop1);
  DO_LOOP_INFO *dli2=Get_Do_Loop_Info(in_loop2);

  // Enumerate loops for debugging.
  if (dli1 && (dli1->Get_Id() == 0))
    Enum_loops(in_loop1);

  if (dli2 && (dli2->Get_Id() == 0))
    Enum_loops(in_loop2);

  // Avoid aggressive fusion if both candidates are non-SNL and 'loop2' has possibly
  // interloop dependencies.
  if (Do_Aggressive_Fuse) {
    if ((dli2->Depth >= 2) && !Get_Only_Loop_Inside(in_loop1, FALSE)
	&& !Get_Only_Loop_Inside(in_loop2, FALSE)
	&& Has_Unit_Refs(in_loop1, dli1->Depth)
	&& !Has_Unit_Refs(in_loop2, dli2->Depth))
      return Failed;
  }

  BOOL is_precom_loop = FALSE;

  if (dli1 && (dli1->Has_Precom_Def == 1)
      && (dli2 && (dli2->Has_Precom_Use == 1))) 
    is_precom_loop = TRUE;

  // -- temporary fix for pv 597324: if index variables of candidate
  //    loops have different type then don't attempt to fuse.
  //    The long term (ideal) fix is to convert to the 'larger' type.
  TYPE_ID ty_index1 = WN_desc(WN_start(in_loop1));
  TYPE_ID ty_index2 = WN_desc(WN_start(in_loop2));
  if ( ty_index1 != ty_index2 ) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "(pv 597324 temp fix) loop index types differ, don't fuse");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "(pv 597324 temp fix) loop index types differ, don't fuse");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "(pv 597324 temp fix) loop index types differ, don't fuse");
    return Failed;
  }

  if (dli1->No_Fusion || dli2->No_Fusion
    || !Cannot_Concurrentize(in_loop1) && Cannot_Concurrentize(in_loop2)
    || Cannot_Concurrentize(in_loop1) && !Cannot_Concurrentize(in_loop2)) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loops with fission/fusion pragmas cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loops with fission/fusion pragmas cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loops with fission/fusion pragmas cannot be fused.");
    return Failed;
  }
/*
  if (Bound_Is_Too_Messy(dli1->LB) || Bound_Is_Too_Messy(dli1->UB) ||
      Bound_Is_Too_Messy(dli2->LB) || Bound_Is_Too_Messy(dli2->UB)) {
  
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loops with messy bounds cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loops with messy bounds cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loops with messy bounds cannot be fused.");
    return Failed;
  }
*/
  if (!Do_Loop_Is_Good(in_loop1) || !Do_Loop_Is_Good(in_loop2) ||
      Do_Loop_Has_Calls(in_loop1) || Do_Loop_Has_Calls(in_loop2)||
      Do_Loop_Has_Gotos(in_loop1) || Do_Loop_Has_Gotos(in_loop2)) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loops with calls, exits, or gotos cannot be fused.");
    return Failed;
  }

  // Move_Adjacent(in_loop1, in_loop2);

  if (WN_next(in_loop1)!=in_loop2) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Loops to be fused are not adjacent.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Loops to be fused are not adjacent.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Loops to be fused are not adjacent.");
    return Failed;
  }

  FmtAssert(fusion_level>0, ("Illegal level number."));

  if (Upper_Bound_Standardize(WN_end(in_loop1),TRUE)==FALSE) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Upper bound of loop1 is too complicated.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Upper bound of loop1 is too complicated.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Upper bound of loop1 is too complicated.");
    return Failed;
  }
  if (loop_var_is_live_on_exit(in_loop1)) 
    Finalize_Index_Variable(in_loop1,FALSE);

  scalar_rename(WN_start(in_loop1));

  if (Upper_Bound_Standardize(WN_end(in_loop2),TRUE)==FALSE) {
    if (LNO_Verbose)
      fusion_verbose_info(srcpos1,srcpos2,fusion_level,
        "Upper bound of loop2 is too complicated.");
    if (LNO_Analysis)
      fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
        "Upper bound of loop2 is too complicated.");
    if (LNO_Tlog)
      fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
        "Upper bound of loop2 is too complicated.");
    return Failed;
  }
  if (loop_var_is_live_on_exit(in_loop2)) {
    Finalize_Index_Variable(in_loop2,TRUE);
    scalar_rename(WN_start(in_loop2));
  }

  UINT8 inner_most_level = fusion_level - 1;
  loop_nest1[0] = in_loop1;
  loop_nest2[0] = in_loop2;
  if ( Do_Loop_Is_Backward(loop_nest1[0]) && 
      !Do_Loop_Is_Backward(loop_nest2[0]))
    if (RV_Is_Legal(loop_nest1[0]))
      RV_Reverse_Loop(loop_nest1[0]);
    else if (RV_Is_Legal(loop_nest2[0]))
      RV_Reverse_Loop(loop_nest2[0]);
  else if (!Do_Loop_Is_Backward(loop_nest1[0]) && 
            Do_Loop_Is_Backward(loop_nest2[0]))
    if (RV_Is_Legal(loop_nest2[0]))
      RV_Reverse_Loop(loop_nest2[0]);
    else if (RV_Is_Legal(loop_nest1[0]))
      RV_Reverse_Loop(loop_nest1[0]);

  // after reversal, it is possible that new statements are inserted
  // between loops.
  if (WN_next(in_loop1)!=in_loop2)
    if (Good_Do_Depth(in_loop1)>0)
      if (Move_Adjacent(in_loop1, in_loop2, FALSE)==FALSE) {
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
            "Cannot move statement in between after reversal");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
            "Cannot move statement in between after reversal");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
            "Cannot move statement in between after reversal");
          return Failed;
      }
      else;
    else {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Cannot move statement in between after reversal");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Cannot move statement in between after reversal");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Cannot move statement in between after reversal");
      return Failed; // outermost good loops cannot not be moved adjacent
    }

  for (i=1; i<fusion_level; i++) {
    WN* lwn= Get_Only_Loop_Inside(loop_nest1[i-1],FALSE);
    Is_True(lwn, ("Non-simply nested loops or not enough level encountered."));
    loop_nest1[i] = lwn;
    lwn= Get_Only_Loop_Inside(loop_nest2[i-1],FALSE);
    Is_True(lwn, ("Non-simply nested loops or not enough level encountered."));
    loop_nest2[i] = lwn;

    dli1=Get_Do_Loop_Info(loop_nest1[i]);
    dli2=Get_Do_Loop_Info(loop_nest2[i]);

    if (dli1->No_Fusion || dli2->No_Fusion
	|| !Cannot_Concurrentize(loop_nest1[i]) 
	&& Cannot_Concurrentize(loop_nest2[i])
	|| Cannot_Concurrentize(loop_nest1[i]) 
	&& !Cannot_Concurrentize(loop_nest2[i])) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loops with fission/fusion pragmas cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loops with fission/fusion pragmas cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loops with fission/fusion pragmas cannot be fused.");
      return Failed;
    }
/*
    if (Bound_Is_Too_Messy(dli1->LB) || Bound_Is_Too_Messy(dli1->UB) ||
        Bound_Is_Too_Messy(dli2->LB) || Bound_Is_Too_Messy(dli2->UB)) {
    
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Loops with messy bounds cannot be fused.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Loops with messy bounds cannot be fused.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Loops with messy bounds cannot be fused.");
      return Failed;
    }
*/
    if ( Do_Loop_Is_Backward(loop_nest1[i]) && 
        !Do_Loop_Is_Backward(loop_nest2[i]))
      if (RV_Is_Legal(loop_nest1[i]))
        RV_Reverse_Loop(loop_nest1[i]);
      else if (RV_Is_Legal(loop_nest2[i]))
        RV_Reverse_Loop(loop_nest2[i]);
    else if (!Do_Loop_Is_Backward(loop_nest1[i]) && 
              Do_Loop_Is_Backward(loop_nest2[i]))
      if (RV_Is_Legal(loop_nest2[i]))
        RV_Reverse_Loop(loop_nest2[i]);
      else if (RV_Is_Legal(loop_nest1[i]))
        RV_Reverse_Loop(loop_nest1[i]);

    if (Upper_Bound_Standardize(WN_end(loop_nest1[i]),TRUE)==FALSE) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Upper bound of loop1 is too complicated.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Upper bound of loop1 is too complicated.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Upper bound of loop1 is too complicated.");
      return Failed;
    }

    if (loop_var_is_live_on_exit(loop_nest1[i]))
      Finalize_Index_Variable(loop_nest1[i],FALSE);
    scalar_rename(WN_start(loop_nest1[i]));

    if (Upper_Bound_Standardize(WN_end(loop_nest2[i]),TRUE)==FALSE) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Upper bound of loop2 is too complicated.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Upper bound of loop2 is too complicated.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Upper bound of loop2 is too complicated.");
      return Failed;
    }
    if (loop_var_is_live_on_exit(loop_nest2[i])) {
      Finalize_Index_Variable(loop_nest2[i],TRUE);
      scalar_rename(WN_start(loop_nest2[i]));
    }

    lwn= loop_nest1[i];
    if (Move_Adjacent(lwn, NULL, FALSE)==FALSE) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Statements after the first loop forbid fusion.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Statements after the first loop forbid fusion.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Statements after the first loop forbid fusion.");
      if (LNO_Test_Dump) {
        Dump_WN(WN_next(lwn), stdout, TRUE, 4, 4);
      }
      return Try_Level_By_Level;
    }

    lwn= loop_nest2[i];
    if (Move_Adjacent(NULL, lwn, FALSE)==FALSE) {
      if (LNO_Verbose)
        fusion_verbose_info(srcpos1,srcpos2,fusion_level,
          "Statements before the second loop forbid fusion.");
      if (LNO_Analysis)
        fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
          "Statements before the second loop forbid fusion.");
      if (LNO_Tlog)
        fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
          "Statements before the second loop forbid fusion.");
      if (LNO_Test_Dump) {
        Dump_WN(WN_prev(lwn), stdout, TRUE, 4, 4);
      }
      return Try_Level_By_Level;
    }
    
  }

  UINT64 prolog = 0;
  UINT64 epilog = 0;
  WN* epilog_loop = NULL;
  UINT64 epilog2 = 0;
  UINT64 prolog2 = 0;
  UINT64 precom = 0;
  mINT32 *offset=CXX_NEW_ARRAY(mINT32, fusion_level, &FUSION_default_pool);
  ARRAY_DIRECTED_GRAPH16 * dg = NULL;
  WN_MAP sdm = 0;
  WN_MAP precom_map = 0;
  DYN_ARRAY<VINDEX16> ** vts = NULL;
  BOOL do_instantiate = FALSE;

  if (!offset_out || (offset_out)[inner_most_level]==MAX_INT32) {
    // an offset is not provided
    // !Block_is_empty(loop_body1) && !Block_is_empty(loop_body2) 

    if (Do_Aggressive_Fuse) {
      sdm = WN_MAP_Create(&FUSION_default_pool);
      dg = CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,DEP_ARRAY_GRAPH),
		   &FUSION_default_pool);
      vts = CXX_NEW_ARRAY(DYN_ARRAY<VINDEX16> *, fusion_level, &FUSION_default_pool);
      for (i = 0; i < fusion_level; i++)
	vts[i] = NULL;
    }

    FISSION_FUSION_STATUS status=
      Fuse_Test(in_loop1, in_loop2, fusion_level, threshold, &prolog, &epilog, &prolog2, &epilog2,
		&epilog_loop, offset, dg, vts, &precom);

    // Annotate WHIRLs that need a precomputation in precom_map.
    if (dg) {
      if (precom > 0) {
	precom_map = WN_MAP_Create(&FUSION_default_pool);
	for (i = 0; i < fusion_level; i++) {
	  DYN_ARRAY<VINDEX16> * v_array = vts[i];
	  if (v_array) {
	    for (int j = 0; j < v_array->Elements(); j++) {
	      VINDEX16 v = (*v_array)[j];
	      EINDEX16 e = dg->Get_In_Edge(v);
	      VINDEX16 v_def = dg->Get_Source(e);
	      WN * wn_def = dg->Get_Wn(v_def);
	      WN * wn_use = dg->Get_Wn(v);
	      WN_MAP_Set(LNO_Precom_Map, wn_use, wn_def);
	      WN * wn_iter = LWN_Get_Parent(wn_use);
	      while( wn_iter) {
		if (WN_operator(wn_iter) == OPR_DO_LOOP) {
		  DO_LOOP_INFO * dli_iter = Get_Do_Loop_Info(wn_iter);
		  if (dli_iter)
		    dli_iter->Has_Precom_Use = 1;
		}
		wn_iter = LWN_Get_Parent(wn_iter);
	      }

	      if (!WN_MAP_Get(precom_map, wn_def)) {
		WN_MAP_Set(precom_map, wn_def, (void *) (INTPTR) (UINT) PRECOM_ROOT);
		int count = 0;
		if (!Array_Input_Has_Same_Parent(wn_def, precom_map, 
						 LWN_Get_Parent(wn_def), &count))
		  FmtAssert(FALSE, ("Unexpected def for pre-computation."));
	      }
	    }
	  }
	}
      }
      Delete_Dep_Graph(dg, vts, fusion_level);	      
    }

    if (status!=Succeeded) {
      if (precom_map) 
	WN_MAP_Delete(precom_map);
      return status;
    }

    if (prolog > threshold) {
      if (precom_map)
	WN_MAP_Delete(precom_map);

      if (prolog<MAX_INT32) {
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
    "Failed because of too many pre-peeled iterations required after fusion.");
        return (Failed);
      } else 
        return Try_Level_By_Level;
    }

    if (epilog > threshold) {
      if (precom_map)
	WN_MAP_Delete(precom_map);

      if (epilog<MAX_INT32) {
        if (LNO_Verbose)
          fusion_verbose_info(srcpos1,srcpos2,fusion_level,
    "Failed because of too many post-peeled iterations required after fusion.");
        if (LNO_Analysis)
          fusion_analysis_info(FALSE,srcpos1,srcpos2,fusion_level,
    "Failed because of too many post-peeled iterations required after fusion.");
        if (LNO_Tlog)
          fusion_tlog_info(Failed,in_loop1,in_loop2,fusion_level,
    "Failed because of too many post-peeled iterations required after fusion.");
        return (Failed);
      } else
        return Try_Level_By_Level;
    }

    if (precom > 0) {
      do_instantiate = TRUE;
      FmtAssert(offset[0] == 0, ("Pre-computation expects zero offset."));

      if (prolog > 0) {
	if (prolog ==1)   // only 1 iteration, use the unrolled peeling 
	  Pre_loop_peeling(in_loop1, prolog, TRUE, FALSE, 0);
	else
	  Pre_loop_peeling(in_loop1, prolog, peeling_unrolled, FALSE, 0);
	prolog = 0;
      }
      
      if (epilog_loop == in_loop1 && epilog) {
	if (epilog==1)  // only 1 iteration, use the unrolled peeling
	  Post_loop_peeling(in_loop1, epilog, TRUE, FALSE);
	else
	  Post_loop_peeling(in_loop1, epilog, peeling_unrolled, FALSE);

	LWN_Insert_Block_Before(LWN_Get_Parent(in_loop1), in_loop1, 
				LWN_Extract_From_Block(WN_next(in_loop1)));
	epilog = 0;
	epilog_loop = NULL;
      }

      if (precom == 1)
	Pre_loop_peeling(in_loop1, precom, TRUE, FALSE, precom_map);
      else
	Pre_loop_peeling(in_loop1, precom, FALSE, FALSE, precom_map);

      precom = 0;
      if (precom_map) 
	WN_MAP_Delete(precom_map);
    }
    
    if (prolog_out) *prolog_out = prolog;
    if (epilog_out) *epilog_out = epilog;
    if (epilog_loop_out) *epilog_loop_out = epilog_loop;

    if (offset_out)
      for (i=0; i<fusion_level; i++)
        (offset_out)[i] = offset[i];

  } else {
    // caller provided offset info, etc.

    offset = CXX_NEW_ARRAY(mINT32, fusion_level, &FUSION_default_pool);
    for (i=0; i<fusion_level; i++)
      offset[i] = (offset_out)[i];
    prolog = *prolog_out;
    epilog = *epilog_out;
    epilog_loop = *epilog_loop_out;

  }

  SYMBOL loop1_var_symbol[LNO_MAX_DO_LOOP_DEPTH];
  SYMBOL loop2_var_symbol[LNO_MAX_DO_LOOP_DEPTH];
  SYMBOL new_loop_var_symbol[LNO_MAX_DO_LOOP_DEPTH];
  SYMBOL pre_loop_var_symbol[LNO_MAX_DO_LOOP_DEPTH];

  WN* wn = NULL;

  TYPE_ID index_type;
  OPERATOR step_operator;
  // adjust loop index to reflect the offset
  for (i=0; i<fusion_level; i++) {
    loop1_var_symbol[i].Init(WN_start(loop_nest1[i]));
    loop2_var_symbol[i].Init(WN_start(loop_nest2[i]));

    WN * wn_loop =  loop_nest1[i];
    index_type=WN_desc(WN_start(wn_loop));
    step_operator =
      WN_operator(WN_kid0(WN_step(wn_loop)));

    char new_loop_var_name[80];

    ST *st;
    const char* name;
    st=WN_st(WN_start(wn_loop));
    //if (st==Int_Preg || st==Float_Preg)
    if (ST_class(st) == CLASS_PREG)
      name=Preg_Name(WN_offset(WN_start(wn_loop)));
    else
      name=ST_name(st);

    // new loop variable
    if (strlen(name)>=70) {
      DevWarn("Loop var %s name too long",name);
      strcpy(new_loop_var_name,"name_too_long");
    } else
      sprintf(new_loop_var_name, "_%s_%d", name, New_Name_Count++);

    new_loop_var_symbol[i] = Create_Preg_Symbol(new_loop_var_name, index_type);

    // temporary used in pre_loop_peeling
    char pre_loop_var_name[80];
#ifdef KEY
    // 'name' may change after call to Create_Preg_Symbol (reallocation of 
    // memory) and point to illegal memory. So, need to reinitialize 'name'.
    // - exposed by bug 2658.
    if (ST_class(st) == CLASS_PREG)
      name=Preg_Name(WN_offset(WN_start(wn_loop)));
    else
      name=ST_name(st);
#endif
    if (strlen(name)>=70) {
      DevWarn("Loop var %s name too long",name);
      strcpy(pre_loop_var_name,"name_too_long");
    } else
      sprintf(pre_loop_var_name, "_i%d_tmp", Do_Loop_Depth(wn_loop)+1);

    pre_loop_var_symbol[i]=Create_Preg_Symbol(pre_loop_var_name, index_type);

    WN* start2=WN_start(loop_nest2[i]);
    WN* step2=WN_step(loop_nest2[i]);

    WN* alias1 = NULL;

    Replace_Symbol(WN_index(loop_nest1[i]),
      loop1_var_symbol[i], new_loop_var_symbol[i], alias1);
    Replace_Symbol(WN_start(loop_nest1[i]),
      loop1_var_symbol[i], new_loop_var_symbol[i], alias1);
    Replace_Symbol(WN_end(loop_nest1[i]),
      loop1_var_symbol[i], new_loop_var_symbol[i], alias1);
    Replace_Symbol(WN_step(loop_nest1[i]),
      loop1_var_symbol[i], new_loop_var_symbol[i], alias1);

    Replace_Symbol(WN_do_body(loop_nest1[i]),
      loop1_var_symbol[i], new_loop_var_symbol[i], alias1);

    WN* alias2 = NULL;

    Replace_Symbol(WN_index(loop_nest2[i]),
      loop2_var_symbol[i], new_loop_var_symbol[i], alias2);
    Replace_Symbol(WN_start(loop_nest2[i]),
      loop2_var_symbol[i], new_loop_var_symbol[i], alias2);
    Replace_Symbol(WN_end(loop_nest2[i]),
      loop2_var_symbol[i], new_loop_var_symbol[i], alias2);
    Replace_Symbol(WN_step(loop_nest2[i]),
      loop2_var_symbol[i], new_loop_var_symbol[i], alias2);

    OPERATOR inverse_step_operator;
    if (step_operator==OPR_ADD)
      inverse_step_operator=OPR_SUB;
    else
      inverse_step_operator=OPR_ADD;

    WN* ldid_wn;

    WN* tmp_wn=LWN_Copy_Tree(WN_kid0(start2),TRUE,LNO_Info_Map);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_kid0(start2),tmp_wn,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid0(start2), Array_Dependence_Graph);

    LWN_Copy_Def_Use(WN_kid0(start2), tmp_wn, Du_Mgr);
    WN* new_start2=LWN_CreateStid(
	WN_opcode(start2),
        WN_start(loop_nest1[i]),
        tmp_wn);
    LWN_Copy_Linenumber(loop_nest2[i], new_start2);
    LWN_Copy_Frequency_Tree(new_start2, start2);

    tmp_wn=LWN_Copy_Tree(WN_kid0(step2),TRUE,LNO_Info_Map);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_kid0(step2),tmp_wn,FALSE))
        LNO_Erase_Dg_From_Here_In(WN_kid0(step2), Array_Dependence_Graph);

    LWN_Copy_Def_Use(WN_kid0(step2), tmp_wn, Du_Mgr);
    WN* new_step2=LWN_CreateStid(
	WN_opcode(step2),
        WN_start(loop_nest1[i]),
        tmp_wn);
    LWN_Copy_Linenumber(loop_nest2[i], new_step2);
    LWN_Copy_Frequency_Tree(new_step2, step2);

    ldid_wn=LWN_CreateLdid(
      OPCODE_make_op(OPR_LDID, Promote_Type(index_type), index_type),
      new_start2);
    LWN_Copy_Frequency(ldid_wn, new_start2);

    Du_Mgr->Add_Def_Use(new_start2,ldid_wn);
    Du_Mgr->Add_Def_Use(new_step2,ldid_wn);
    Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt(loop_nest1[i]);

    Replace_Ldid_With_Exp_Copy(
      new_loop_var_symbol[i],
      new_step2,
      ldid_wn,
      Du_Mgr);

    // we want new loop index var to have non-null loop_stmt
    //Du_Mgr->Ud_Get_Def(ldid_wn)->Set_loop_stmt((WN*)NULL);

    wn = LWN_CreateExp2(
			OPCODE_make_op(inverse_step_operator, Promote_Type(index_type), MTYPE_V),
			ldid_wn,
			LWN_Make_Icon(Promote_Type(index_type), offset[i]));
    LWN_Copy_Frequency_Tree(wn, ldid_wn);

    // shift iterations of in_loop2 right for 'offset' distance
      
    WN_kid0(new_start2)=
      LWN_CreateExp2(OPCODE_make_op(step_operator, 
				    Promote_Type(index_type), MTYPE_V), WN_kid0(new_start2),
		     LWN_Make_Icon(Promote_Type(index_type), offset[i]));

    LWN_Copy_Frequency_Tree(WN_kid0(new_start2), new_start2);

    WN* old_end2=WN_end(loop_nest2[i]);
    SYMBOL kid0_symbol;
    SYMBOL kid1_symbol;
    if (WN_operator(WN_kid0(old_end2))==OPR_LDID)
      kid0_symbol.Init(WN_kid0(old_end2));
    if (WN_operator(WN_kid1(old_end2))==OPR_LDID)
      kid1_symbol.Init(WN_kid1(old_end2));

    if (kid0_symbol==new_loop_var_symbol[i]) {
      WN_kid1(old_end2)=
	LWN_CreateExp2(OPCODE_make_op(step_operator, 
				      Promote_Type(index_type), MTYPE_V), WN_kid1(old_end2),
		       LWN_Make_Icon(Promote_Type(index_type), offset[i]));
      LWN_Copy_Frequency_Tree(WN_kid1(old_end2), old_end2);
    } else if (kid1_symbol==new_loop_var_symbol[i]) {
      WN_kid0(old_end2)=
	LWN_CreateExp2(OPCODE_make_op(step_operator, 
				      Promote_Type(index_type), MTYPE_V),
		       WN_kid0(old_end2),
		       LWN_Make_Icon(Promote_Type(index_type), offset[i]));
      LWN_Copy_Frequency_Tree(WN_kid0(old_end2), old_end2);
    } else {
      Replace_Ldid_With_Exp_Copy(
				 new_loop_var_symbol[i],
				 old_end2,
				 wn,
				 Du_Mgr);
    }

    Replace_Ldid_With_Exp_Copy(
      new_loop_var_symbol[i],
      old_end2,
      ldid_wn,
      Du_Mgr);

    if (wn) {
      Replace_Ldid_With_Exp_Copy(
				 loop2_var_symbol[i],
				 WN_do_body(loop_nest2[i]),
				 wn,
				 Du_Mgr);
      
      LWN_Delete_Tree(wn);
    }
    else {
      Replace_Ldid_With_Exp_Copy(
				 loop2_var_symbol[i],
				 WN_do_body(loop_nest2[i]),
				 ldid_wn,
				 Du_Mgr);
      
    }

    //Update_Du_For_Loop_Start(Du_Mgr, loop_nest1[i], loop_nest2[i], lv1, lv2);

    LWN_Delete_Tree(start2);
    LWN_Delete_Tree(step2);

    WN_start(loop_nest2[i])=new_start2;
    LWN_Set_Parent(new_start2,loop_nest2[i]);
    LWN_Parentize(new_start2);
    WN_step(loop_nest2[i])=new_step2;
    LWN_Set_Parent(new_step2,loop_nest2[i]);
    LWN_Parentize(new_step2);

    if (i!=fusion_level-1) {

    WN* start1=WN_start(loop_nest1[i]);
    WN* step1=WN_step(loop_nest1[i]);
    //index_type=WN_desc(WN_start(loop_nest1[i]));

    wn = LWN_CreateLdid(
        OPCODE_make_op(OPR_LDID, index_type, index_type),
        start1);

    LWN_Copy_Frequency(wn,start1);
    Du_Mgr->Add_Def_Use(start1,wn);
    Du_Mgr->Add_Def_Use(step1,wn);
    Du_Mgr->Ud_Get_Def(wn)->Set_loop_stmt(loop_nest1[i]);

    Replace_Ldid_With_Exp_Copy(
       new_loop_var_symbol[i],
       WN_do_body(loop_nest2[i]),
       wn,
       Du_Mgr);

    LWN_Delete_Tree(wn);
    }

  }

  Fusion_Du_Update(loop_nest1, loop_nest2, Du_Mgr);
  Fusion_Loop_Stmt_Update(loop_nest1, loop_nest2, fusion_level-1, Du_Mgr);

  WN* inner_loop1 = loop_nest1[inner_most_level];
  WN* inner_loop2 = loop_nest2[inner_most_level];

  // get rid of the outer loops of the 2nd loop nests
  // and put the inner loop of in_loop2 right after the inner loop
  // of in_loop1
    
  for (i=fusion_level-2; i>=0; i--) {
    //LWN_Extract_From_Block(WN_do_body(loop_nest2[i]));
    LWN_Insert_Block_Before(LWN_Get_Parent(loop_nest1[i+1]),
                           NULL, WN_do_body(loop_nest2[i]));
    // TODO: check if the block WN has been deleted or not.
    LWN_Delete_Tree(WN_start(loop_nest2[i]));
    LWN_Delete_Tree(WN_step(loop_nest2[i]));
    LWN_Delete_Tree(WN_end(loop_nest2[i]));
    LWN_Delete_From_Block(LWN_Get_Parent(loop_nest2[i]), loop_nest2[i]);
  }

  WN* wn1 = WN_prev(inner_loop1);
  // peel the first loop for 'prolog' iterations
  if (prolog > 0) {
    if (prolog ==1)   // only 1 iteration, use the unrolled peeling 
      Pre_loop_peeling(inner_loop1, prolog, TRUE, FALSE, 0);
    else
      Pre_loop_peeling(inner_loop1, prolog, peeling_unrolled, FALSE, 0);
  }

  for (WN* stmt = WN_prev(inner_loop1); stmt != wn1; stmt=WN_prev(stmt))
    Replace_Symbol(stmt,
		   new_loop_var_symbol[inner_most_level],
		   pre_loop_var_symbol[inner_most_level], NULL);
  Replace_Symbol(WN_kid0(WN_start(inner_loop1)),
		 new_loop_var_symbol[inner_most_level],
		 pre_loop_var_symbol[inner_most_level], NULL);

  if (prolog2 && (prolog2 != MAX_INT32)) {
    WN * wn2 = WN_prev(inner_loop2);
    if (prolog2 > 0) {
      if (prolog2 == 1)
	Pre_loop_peeling(inner_loop2, prolog2, TRUE, FALSE, 0);
      else
	Pre_loop_peeling(inner_loop2, prolog2, peeling_unrolled, FALSE, 0);

      WN * wn_if = WN_prev(inner_loop2);
      if (WN_operator(wn_if) == OPR_IF) {
	WN * wn_then = WN_then(wn_if);
	Remove_Cond_Branch(wn_then, NULL);

	WN * wn_loop = wn_then ? WN_first(wn_then) : NULL;
	while (wn_loop) {
	  if (WN_operator(wn_loop) == OPR_DO_LOOP) 
	    break;
	  wn_loop = WN_next(wn_loop);
	}
	
	if (wn_loop && (WN_operator(wn_loop) == OPR_DO_LOOP)) {
	  DO_LOOP_INFO * dli_loop = Get_Do_Loop_Info(wn_loop);
	  if (dli_loop && dli1 && (dli1->Prefer_Fuse == 1)) {
	    dli_loop->Prefer_Fuse = 1;
	    Pass_Child_Prefer_Fuse(wn_loop);
	  }
	}
      }
    }

    for (WN* stmt = WN_prev(inner_loop2); stmt != wn2; stmt=WN_prev(stmt))
      Replace_Symbol(stmt,
        new_loop_var_symbol[inner_most_level],
        pre_loop_var_symbol[inner_most_level], NULL);
    Replace_Symbol(WN_kid0(WN_start(inner_loop2)),
      new_loop_var_symbol[inner_most_level],
      pre_loop_var_symbol[inner_most_level], NULL);
  }

  // peel either the 'in_loop1' or 'in_loop2' for epilog iterations
  if (epilog_loop == inner_loop1 && epilog) {
    if (epilog==1)  // only 1 iteration, use the unrolled peeling
      Post_loop_peeling(inner_loop1, epilog, TRUE, FALSE);
    else
      Post_loop_peeling(inner_loop1, epilog, peeling_unrolled, FALSE);
  } else if (epilog_loop == inner_loop2 && epilog) {
    if (epilog==1)  // only 1 iteration, use the unrolled peeling
      Post_loop_peeling(inner_loop2, epilog, TRUE, FALSE);
    else
      Post_loop_peeling(inner_loop2, epilog, peeling_unrolled, FALSE);
  }

  // peel'in_loop2' for epilog iterations.
  if (epilog2 && (epilog2 != MAX_INT32)) {
    if (epilog2 == 1)
      Post_loop_peeling(inner_loop2, epilog2, TRUE, FALSE);
    else
      Post_loop_peeling(inner_loop2, epilog2, peeling_unrolled, FALSE);

    WN * wn_if = WN_next(inner_loop2);
    if (WN_operator(wn_if) == OPR_IF) {
      WN * wn_then = WN_then(wn_if);
      Remove_Cond_Branch(wn_then, NULL);

      WN * wn_loop = (wn_then) ? WN_first(wn_then) : NULL;
      while (wn_loop) {
	if (WN_operator(wn_loop) == OPR_DO_LOOP) 
	  break;
	wn_loop = WN_next(wn_loop);
      }

      if (wn_loop && (WN_operator(wn_loop) == OPR_DO_LOOP)) {
	DO_LOOP_INFO * dli_loop = Get_Do_Loop_Info(wn_loop);
	if (dli_loop && dli1 && (dli1->Prefer_Fuse == 1)) {
	  dli_loop->Prefer_Fuse = 1;
	  Pass_Child_Prefer_Fuse(wn_loop);
	}
      }
    }
  }

  WN* start1=WN_start(inner_loop1);
  WN* step1=WN_step(inner_loop1);
  index_type=WN_desc(WN_start(inner_loop1));

  USE_LIST_ITER u_iter(Du_Mgr->Du_Get_Use(WN_start(inner_loop2)));
  for (DU_NODE* un=u_iter.First(); !u_iter.Is_Empty(); ) {
    WN* use=un->Wn();
    un=u_iter.Next();
    Du_Mgr->Add_Def_Use(start1,use);
    Du_Mgr->Add_Def_Use(step1,use);
  }

  if ((prolog2 && (prolog2 != MAX_INT32))
      || (epilog2 && (epilog2 != MAX_INT32)))
    Remove_Cond_Branch(WN_do_body(inner_loop2), inner_loop2);

  Fusion_Loop_Stmt_Update(&inner_loop1, &inner_loop2, 1, Du_Mgr);

  // now move body of the 'in_loop2' to the end of the 'in_loop1'
  LWN_Insert_Block_Before(WN_do_body(inner_loop1), NULL,
    WN_do_body(inner_loop2));

  DO_LOOP_INFO *loop_info1 = 
	        (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, inner_loop1);
  DO_LOOP_INFO *loop_info2 = 
	        (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, inner_loop2);

  // TODO: find proper way to delete inner_loop2
  LWN_Delete_Tree(WN_start(inner_loop2));
  LWN_Delete_Tree(WN_step(inner_loop2));
  LWN_Delete_Tree(WN_end(inner_loop2));
  LWN_Delete_From_Block(LWN_Get_Parent(inner_loop2),inner_loop2);

  BOOL inner_loop_removed = FALSE;
  WN* inner_loop_parent=LWN_Get_Parent(inner_loop1);

  // eliminate the loop body if it has 0 trip count
  // TODO: check if the loop index is live at the exit
  if (wn=WN_SimplifyExp2(WN_opcode(WN_end(inner_loop1)),
      LWN_Copy_Tree(WN_kid0(WN_start(inner_loop1))),
      LWN_Copy_Tree(WN_kid1(WN_end(inner_loop1))))) {
	if (WN_opcode(wn) == OPC_U4INTCONST && WN_const_val(wn) == 0) {

	  WN* tmp=LWN_Copy_Tree(WN_start(inner_loop1),TRUE,LNO_Info_Map);
          if (!Array_Dependence_Graph->
            Add_Deps_To_Copy_Block(WN_start(inner_loop1),tmp,FALSE))
            LNO_Erase_Dg_From_Here_In(WN_start(inner_loop1),
              Array_Dependence_Graph);

          USE_LIST_ITER u_iter(Du_Mgr->Du_Get_Use(WN_start(inner_loop1)));
          Du_Mgr->Create_Use_List(tmp);
          for (DU_NODE* un=u_iter.First(); !u_iter.Is_Empty(); ) {
            WN* use=un->Wn();
            un=u_iter.Next();
            Du_Mgr->Add_Def_Use(tmp,use);
          }
          LWN_Copy_Def_Use(WN_kid0(WN_start(inner_loop1)),
                           WN_kid0(tmp),Du_Mgr);
	  LWN_Insert_Block_Before(LWN_Get_Parent(in_loop1),in_loop1,tmp);
          LWN_Update_Def_Use_Delete_Tree(inner_loop1,Du_Mgr);
          LWN_Update_Dg_Delete_Tree(inner_loop1, adg);
          LWN_Delete_From_Block(LWN_Get_Parent(inner_loop1),inner_loop1);
          USE_LIST_ITER u1_iter(Du_Mgr->Du_Get_Use(tmp));
          if (u1_iter.Is_Empty()) {
            LWN_Update_Def_Use_Delete_Tree(tmp,Du_Mgr);
            LWN_Delete_From_Block(LWN_Get_Parent(tmp),tmp);
          }
          inner_loop_removed = TRUE;
          if (LNO_Verbose)
            fusion_verbose_info(srcpos1,srcpos2,fusion_level,
              "Empty innermost loop is removed after fusion!!");
          if (LNO_Analysis)
            fusion_analysis_info(TRUE,srcpos1,srcpos2,fusion_level,
              "Empty innermost loop is removed after fusion!!");
          if (LNO_Tlog) {
            char in_string[30];
            char out_string[30];
            sprintf(in_string,"%d %d %d", Srcpos_To_Line(srcpos1),
                              Srcpos_To_Line(srcpos2), fusion_level);
            sprintf(out_string,"%d",Succeeded);
            Generate_Tlog("LNO","fusion", Srcpos_To_Line(srcpos1),
                          ST_name(WN_st(WN_index(in_loop1))),
                          in_string, out_string,
              "Empty innermost loop is removed after fusion!!");
          }
	}
  }

  if (!inner_loop_removed || fusion_level>1) {
    WN_Simplify_Tree(in_loop1);
    LWN_Parentize(LWN_Get_Parent(in_loop1));
  }

  // Adjust the loop info for loops starting from the innermost one

  BOOL has_gotos = loop_info1->Has_Gotos || loop_info2->Has_Gotos;
  BOOL has_calls = loop_info1->Has_Calls || loop_info2->Has_Calls;
  BOOL has_unsummarized_calls = loop_info1->Has_Unsummarized_Calls || 
			loop_info2->Has_Unsummarized_Calls;
  BOOL is_inner = loop_info1->Is_Inner && loop_info2->Is_Inner;
  if (!inner_loop_removed) {
    loop_info1->Has_Gotos = has_gotos;
    loop_info1->Has_Calls = has_calls;
    loop_info1->Has_Unsummarized_Calls = has_unsummarized_calls;
    loop_info1->Is_Inner = is_inner;
    loop_info1->Is_Ivdep = FALSE;
    loop_info1->Is_Concurrent_Call = FALSE;
    loop_info1->Concurrent_Directive = FALSE;
  }

  for (i=fusion_level-2; i>=0; i--) {
    loop_info1 = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, loop_nest1[i]);
    loop_info1->Has_Gotos = has_gotos;
    loop_info1->Has_Calls = has_calls;
    loop_info1->Has_Unsummarized_Calls = has_unsummarized_calls;
    loop_info1->Is_Concurrent_Call = FALSE;
    loop_info1->Concurrent_Directive = FALSE;
  }

  // mark the info of the parent DO or IF about inner loop info
  wn=inner_loop_parent;
  WN* inner_loop_found=NULL;
  if (inner_loop_removed)
    while (wn) {
      OPCODE opc=WN_opcode(wn);
      if (opc==OPC_DO_LOOP || opc==OPC_IF) {
        if (!inner_loop_found)
          inner_loop_found=Find_SCF_Inside(wn,OPC_DO_LOOP);
        if (opc==OPC_DO_LOOP) {
          Get_Do_Loop_Info(wn)->Is_Inner=(inner_loop_found==NULL);
          inner_loop_found=wn;
        } else if (opc==OPC_IF)
          Get_If_Info(wn)->Contains_Do_Loops=(inner_loop_found!=NULL);
      }
      wn=LWN_Get_Parent(wn);
    }

  // now adjust the access vectors for the inner loop, prolog, and epilog

  // BOOL rebuild_access = (prolog!=0) || (epilog!=0);
  for (i=0; i<fusion_level; i++)
    if ((i<fusion_level-1 &&
           (WN_first(WN_do_body(loop_nest1[i]))!=loop_nest1[i+1] ||
            WN_last(WN_do_body(loop_nest1[i]))!=loop_nest1[i+1])
        ) ||
        (i==fusion_level-1 && !inner_loop_removed)) {
        DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
                                   &FUSION_default_pool);
        Build_Doloop_Stack(LWN_Get_Parent(loop_nest1[i]), loop_stack);
        LNO_Build_Access(loop_nest1[i], loop_stack, &LNO_default_pool);
        if (!adg->Build_Region(loop_nest1[i],loop_nest1[i],loop_stack, TRUE))  
          LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(loop_nest1[i]), adg);  
        break;
    }

  if (Do_Aggressive_Fuse) {
    if (Instantiate_Defs(inner_loop1)) {
      DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&FUSION_default_pool),
				       &FUSION_default_pool);
      Build_Doloop_Stack(LWN_Get_Parent(inner_loop1), loop_stack);
      LNO_Build_Access(inner_loop1, loop_stack, &LNO_default_pool);
      if (!adg->Build_Region(inner_loop1,inner_loop1,loop_stack, TRUE))
	LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(inner_loop1), adg);  

      // Flag 'Sclrze_Dse' bit for pre-computation initialization loop-nest
      // and pre-computation main loop-nest.  It is safe to scalarize
      // global-as-local candidates in these loops.
      WN * outermost = Outermost_Enclosing_Loop(inner_loop1);
      WN * wn_iter = WN_prev(outermost);
      while (wn_iter) {
	if (WN_operator(wn_iter) == OPR_DO_LOOP) {
	  DO_LOOP_INFO * dli_iter = Get_Do_Loop_Info(wn_iter);
	  if (dli_iter && (dli_iter->Is_Precom_Init == 1)) {
	    dli_iter->Sclrze_Dse = 1;
	    break;
	  }
	}
	wn_iter = WN_prev(wn_iter);
      }
      wn_iter = inner_loop1;
      while (wn_iter) {
	if (WN_operator(wn_iter) == OPR_DO_LOOP) {
	  DO_LOOP_INFO * dli_iter = Get_Do_Loop_Info(wn_iter);
	  if (dli_iter)
	    dli_iter->Sclrze_Dse = 1;
	}
	wn_iter = LWN_Get_Parent(wn_iter);
      }
    }
  }

  if (LNO_Test_Dump) adg->Print(stdout);

  if (LNO_Verbose) {
    fusion_verbose_info(srcpos1,srcpos2,fusion_level,
      "Successfully fused !!");
    if ((dli1->Get_Id() > 0) || (dli2->Get_Id() > 0))
      printf("     loop num(%d+%d)\n", dli1->Get_Id(), dli2->Get_Id());
  }

  if (LNO_Analysis)
    fusion_analysis_info(TRUE,srcpos1,srcpos2,fusion_level,
      "Successfully fused !!");
  if (LNO_Tlog) {
    char in_string[30];
    char out_string[30];
    sprintf(in_string,"%d %d %d", Srcpos_To_Line(srcpos1),
                      Srcpos_To_Line(srcpos2), fusion_level);
    sprintf(out_string,"%d",Succeeded);
    Generate_Tlog("LNO","##Fusion: ", Srcpos_To_Line(srcpos1),
                  ST_name(WN_st(WN_index(in_loop1))),
                  in_string, out_string, "Successfully fused !!");
  }

  // Pass down Prefer_Fuse bit to the immediate child loop.
  Pass_Child_Prefer_Fuse(in_loop1);

  // Do not want to fuse a loop containing both pre-computation defs and uses
  // with the other loops.
  if (is_precom_loop) 
    dli1->No_Fusion = TRUE;

  if (inner_loop_removed)
    return Succeeded_and_Inner_Loop_Removed;
  else
    return Succeeded;

}

// fusion init routine
extern void Fusion_Init()
{
  if (!fusion_initialized) {
    MEM_POOL_Initialize(&FUSION_default_pool,"FUSION_default_pool",FALSE);
    MEM_POOL_Push(&FUSION_default_pool);
    fusion_initialized=TRUE;
  }
}

// fusion finish routine
extern void Fusion_Finish()
{
  if (fusion_initialized) {
    MEM_POOL_Pop(&FUSION_default_pool);
    MEM_POOL_Delete(&FUSION_default_pool);
    fusion_initialized=FALSE;
  }
}


