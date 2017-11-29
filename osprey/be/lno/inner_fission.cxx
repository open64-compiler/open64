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
//                  LNO Inner Loops Fission Phase
//                  -----------------------------
//

/* ====================================================================
 * ====================================================================
 *
 * Module: inner_fission.cxx
 * $Revision: 1.6 $
 * $Date: 04/12/21 14:57:13-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.inner_fission.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Fission innermost loop to reduce register presure
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
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.inner_fission.cxx $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <stdlib.h>
#include "defs.h"
#include "config_targ_opt.h"
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
#include "dep_graph.h"
#include "access_vector.h"
#include "btree.h"
#include "reduc.h"
#include "lno_bv.h"
#include "snl.h"
#include "name.h"
#include "inner_fission.h"
#include "glob.h"
#include "snl_trans.h" 
#include "tlog.h" 
#include "sxlist.h"
#include "sxlimit.h"

typedef HASH_TABLE<WN*,VINDEX16> WN2VINDEX;
typedef HASH_TABLE<WN*,UINT> WN2UINT;
typedef HASH_TABLE<WN*,INT> WN2INT;
typedef DYN_ARRAY<UINT> UINT_DYN_ARRAY;

#define TWO_PASS 1	// run two pass merging algorithm if
			// LNO_Run_P3==TWO_PASS

#define ESTIMATED_SIZE 100	// used to initialized hash table, etc.
#define Iteration_Count_Threshold 10 // threshold to determine if a loop
                                     // has too few a number of iterations

extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
extern MEM_POOL SNL_local_pool;		// SNL private mem pool
static MEM_POOL INNER_FISSION_default_pool;
                                        // inner_fission phase private mem pool
static ARRAY_DIRECTED_GRAPH16 *adg;	// PU array dep. graph

static INT Total_FP_Register_Usage=0;
static INT Total_INT_Register_Usage=0;
static INT Total_TLB_Usage=0;

static ACCESS_VECTOR Dummy_Too_Messy_Access_Vector;
static ACCESS_VECTOR Dummy_Access_Vector;
// used to mask out the dimensions of array ref where they have
// non-zero innermost loop coefficient when
// comparing common array names
// e.g. we perfer to merge a(i,j+1) with a(i,j+2) than a(i+1,j+1) in
// do i
//   do j
// so the access vector for the second dimension is replaced with a copy
// of the Dummy_Access_Vector and has the don't-care effect

static void inner_fission_verbose_info(
  SRCPOS        srcpos,
  const char*   message)
{
  printf("#### Inner Fission(%d): %s\n", Srcpos_To_Line(srcpos), message);
}

static void inner_fission_tlog_info(
  INT		new_loop_number,
  SRCPOS        srcpos,
  WN*           innerloop,
  char*         message)
{
  char in_string[30];
  char out_string[30];
  sprintf(in_string,"%d", Srcpos_To_Line(srcpos));
  sprintf(out_string,"%d", new_loop_number);
  Generate_Tlog("LNO","inner_loop_fission",
                Srcpos_To_Line(WN_Get_Linenum(innerloop)),
                ST_name(WN_st(WN_index(innerloop))),
                in_string, out_string, message);
}

// Get the parent stmt which is directly under 'body'
extern WN* Find_Stmt_Under(WN* stmt,WN* body) {
 
  if (WN_opcode(stmt)==OPC_FUNC_ENTRY)
    return NULL;

  if (LWN_Get_Parent(stmt)==body)
    return stmt;

  WN* parent;
#ifndef KEY
  while ((parent=LWN_Get_Parent(stmt))!=body)
    if (WN_opcode(parent)==OPC_FUNC_ENTRY)
      return NULL;
    else
      stmt=parent;
#else
  // Bug 2795
  // For Analyse_Dependencies (from the vectorizer), we pass down a loop
  // copy without a parent (because it does not make sense to make a copy of
  // the entire WHIRL tree). When Analyse_Dependencies is called by 
  // Mark_Auto_Vectorizable_Loops (that is early in LNO phase), some
  // references in the loop (in this case, the loop termination test)
  // point to outside the loop. We do not have a complete view of the WHIRL 
  // tree and so should return NULL when the parent pointer becomes NULL.
  while ((parent=LWN_Get_Parent(stmt))!=body && parent)
    if (WN_opcode(parent)==OPC_FUNC_ENTRY)
      return NULL;
    else
      stmt=parent;
  if (!parent)
    return NULL;
#endif
  return stmt;
}

// determine if an array reference is loop invariant or not
static BOOL Is_Invariant(ACCESS_ARRAY* array, WN* loop) {

  INT loopno=Do_Loop_Depth(loop);
  if (array->Non_Const_Loops() > loopno)
    return FALSE;
  for (INT i=0; i<array->Num_Vec(); i++) {
  
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy)
      return FALSE;
    for (INT j=loopno; j<av->Nest_Depth(); j++)
      if ((av->Loop_Coeff(j) != 0))
        return FALSE;
  }

  return TRUE;
}

// Returns the number of FP registers available in the target arch.
static INT Get_Limit() {
  if (LNO_Fission_Inner_Register_Limit>0)
    return LNO_Fission_Inner_Register_Limit;
  else
    return Target_FPRs;
}

// returns the register usage assuming statements from the two
// lists -- 'base_loop' and 'test_loop' are merged
// 'parent_loop' provide the contex of the statement lists
static void reg_used_if_merged(
  FF_STMT_LIST* base_loop,
  FF_STMT_LIST* test_loop,
  WN* parent_loop,
  INT* num_fp_regs,
  INT* num_int_regs,
  INT* num_tlb) {
  
  REGISTER_MODEL merge_reg_model(&INNER_FISSION_default_pool);

  FF_STMT_ITER s_iter(base_loop);
  FF_STMT_NODE *stmt_node;
  for (stmt_node=s_iter.First(); !s_iter.Is_Empty();
    stmt_node=s_iter.Next()) {
    WN* stmt=stmt_node->Get_Stmt();
    merge_reg_model.Add_Statement(stmt);
  }

  FF_STMT_ITER t_iter(test_loop);
  for (stmt_node=t_iter.First(); !t_iter.Is_Empty();
    stmt_node=t_iter.Next()) {
    WN* stmt=stmt_node->Get_Stmt();
    merge_reg_model.Add_Statement(stmt);
  }

  merge_reg_model.Calculate_Register_Usage(
    parent_loop,num_fp_regs,num_int_regs,num_tlb);
}

// inner_fission_2 : examine all reads and writes and do the following
//	1. create name to bit position mappings for new symbol names
//	2. for STID, check if it is scalar expandable
extern  UINT inner_fission_2(
	WN* loop,		// enclosing loop
	SCALAR_STACK* scalar_reads,	// read refs to be examined
	SCALAR_STACK* scalar_writes,	// write refs to be examined
	REF_LIST_STACK* reads,	// read refs to be examined
	REF_LIST_STACK* writes,	// write refs to be examined
	BINARY_TREE<NAME2BIT> *mapping_dictionary,
		// dictionary to be updated which records mapping from
		// symbol names to bit positions
	FF_STMT_LIST& expandable_ref_list,
		// list contains all expandable refs after inner_fission_2
        MEM_POOL* mpool)
{
  
  UINT bit_position=0;

  REF_LIST_STACK *array_ref_list[2];
  array_ref_list[0]=reads;
  array_ref_list[1]=writes;

  // look at both reads and writes
  INT i;
  for (i=0; i<2; i++) {

    for (INT j=0; j<array_ref_list[i]->Elements(); j++) {
  
      REFERENCE_ITER ref_iter(array_ref_list[i]->Bottom_nth(j));
      for (REFERENCE_NODE *n1=ref_iter.First(); !ref_iter.Is_Empty();
           n1=ref_iter.Next()) {
        WN* array_node=n1->Wn;
	if (OPCODE_is_load(WN_opcode(array_node))) {
	  array_node = WN_kid0(array_node);
        } else {
	  array_node = WN_kid1(array_node);
        }
        if (WN_operator(array_node) == OPR_ADD) {
          if (WN_operator(WN_kid0(array_node)) == OPR_ARRAY) {
            array_node = WN_kid0(array_node);
	  } else {
            array_node = WN_kid1(array_node);
          }
        }

        if (!OPCODE_has_sym(WN_opcode(WN_array_base(array_node))))
          continue;

        NAME2BIT temp_map;

        temp_map.Set_Symbol(WN_array_base(array_node));
        ACCESS_ARRAY* ar=
          (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,array_node);
        if (Is_Invariant(ar, loop))
          temp_map.Set_Access_Array(ar);
        else {
          ACCESS_ARRAY *ar_new=CXX_NEW(ACCESS_ARRAY(ar,mpool),mpool);
          Dummy_Too_Messy_Access_Vector.Const_Offset=INT64_MAX;
          Dummy_Too_Messy_Access_Vector.Too_Messy=0;
          Dummy_Access_Vector.Too_Messy=0;
          // the access vectors for the dimensions having non-zero
          // innermost loop coefficient is replaced with a copy
          // of the Dummy_Access_Vector and has the don't-care effect
          for (INT k=0; k<ar->Num_Vec(); k++) {
            ACCESS_VECTOR *av=ar_new->Dim(k);
            if (av->Too_Messy || av->Contains_Non_Lin_Symb())
              ar_new->Dim(k)->Init(&Dummy_Too_Messy_Access_Vector, mpool);
            else if (av->Loop_Coeff(av->Nest_Depth()-1)!=0)
              ar_new->Dim(k)->Init(&Dummy_Access_Vector, mpool);
          }
          temp_map.Set_Access_Array(ar_new);
        }

        // create and enter new name to bit_position mapping if it has
        // not been created
        // also check if it is scalar expandable if the ref is a STID
        if (mapping_dictionary->Find(temp_map)==NULL) {

          if (LNO_Test_Dump) {
            temp_map.Get_Symbol().Print(stdout);
            ACCESS_ARRAY* ar;
            if ((ar=temp_map.Get_Access_Array())) {
              ar->Print(stdout);
            }
            printf("\t\tat bit %d\n", bit_position);
          }
          temp_map.Set_Bit_Position(bit_position);
          mapping_dictionary->Enter(temp_map);
          bit_position++;
        }
      }
    }
  }

  SCALAR_STACK *scalar_ref_list[2];
  scalar_ref_list[0]=scalar_reads;
  scalar_ref_list[1]=scalar_writes;

  // look at both reads and writes
  for (i=0; i<2; i++) {

    for (INT j=0; j<scalar_ref_list[i]->Elements(); j++) {
  
      WN* scalar_ref=scalar_ref_list[i]->Bottom_nth(j)->Bottom_nth(0)->Wn;
      NAME2BIT temp_map;

      temp_map.Set_Symbol(scalar_ref);

      // create and enter new name to bit_position mapping if it has
      // not been created
      // also check if it is scalar expandable if the ref is a STID
      if (mapping_dictionary->Find(temp_map)==NULL) {

        if (LNO_Test_Dump) {
          temp_map.Get_Symbol().Print(stdout);
          printf("\t\tat bit %d\n", bit_position);
        }
        temp_map.Set_Bit_Position(bit_position);
        mapping_dictionary->Enter(temp_map);
        bit_position++;
      }

      if (i==1) {
        SE_RESULT se_result = Scalar_Expandable(scalar_ref,loop, Du_Mgr);
	if (!Get_Trace(TP_LNOPT2, TT_LNO_DISABLE_SEFIN)
            && se_result != SE_NONE || se_result == SE_EASY)
          expandable_ref_list.Append(scalar_ref,mpool);
      }

    }
  }
  return bit_position;
}

// Examine the read and write reference lists and mark the bits
// corresponding to variables appearing in each statement in the entry of the
// bit vector array 'stmt_name_set[]'. Use 'bit_pos_mapping' to find the
// bit position for each name and 'stmt_id' to locate the entry for each stmt.
extern void Register_Name_To_Statement(
	WN* loop,		// enclosing loop
	SCALAR_STACK* scalar_reads,	// read refs to be examined
	SCALAR_STACK* scalar_writes,	// write refs to be examined
	REF_LIST_STACK* reads,	// read refs to be examined
	REF_LIST_STACK* writes,	// write refs to be examined
	HASH_TABLE<WN*,UINT>* stmt_id,
		// hash table which maps a statement to a integer statement id
	BIT_VECTOR* stmt_name_set,
		// name set bit vector array for all statements
	BINARY_TREE<NAME2BIT> *bit_pos_mapping)
		// dictionary which records mapping from
		// symbol names to bit positions
{
  
  MEM_POOL_Push(&LNO_local_pool);

  WN* body=WN_do_body(loop);
  REF_LIST_STACK *array_ref_list[2];
  array_ref_list[0]=reads;
  array_ref_list[1]=writes;

  // look at both reads and writes
  INT i;
  for (i=0; i<2; i++) {

    for (INT j=0; j<array_ref_list[i]->Elements(); j++) {
  
      REFERENCE_ITER ref_iter(array_ref_list[i]->Bottom_nth(j));
      for (REFERENCE_NODE *n1=ref_iter.First(); !ref_iter.Is_Empty();
           n1=ref_iter.Next()) {
        WN* array_node=n1->Wn;
	if (OPCODE_is_load(WN_opcode(array_node))) {
	  array_node = WN_kid0(array_node);
        } else {
	  array_node = WN_kid1(array_node);
        }
        if (WN_operator(array_node) == OPR_ADD) {
          if (WN_operator(WN_kid0(array_node)) == OPR_ARRAY) {
            array_node = WN_kid0(array_node);
	  } else {
            array_node = WN_kid1(array_node);
          }
        }

        if (!OPCODE_has_sym(WN_opcode(WN_array_base(array_node))))
          continue;

        // for array references, get the mapping from name of
        // variable to bit position
        NAME2BIT temp_map;

        temp_map.Set_Symbol(WN_array_base(array_node));
        ACCESS_ARRAY* ar=
          (ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,array_node);
        if (Is_Invariant(ar, loop))
          temp_map.Set_Access_Array(ar);
        else {
          ACCESS_ARRAY *ar_new=
            CXX_NEW(ACCESS_ARRAY(ar,&LNO_local_pool),&LNO_local_pool);
          Dummy_Too_Messy_Access_Vector.Const_Offset=INT64_MAX;
          Dummy_Too_Messy_Access_Vector.Too_Messy=0;
          Dummy_Access_Vector.Too_Messy=0;
          // the access vectors for the dimensions having non-zero
          // innermost loop coefficient is replaced with a copy
          // of the Dummy_Access_Vector and has the don't-care effect
          for (INT k=0; k<ar->Num_Vec(); k++) {
            ACCESS_VECTOR *av=ar_new->Dim(k);
            if (av->Too_Messy || av->Contains_Non_Lin_Symb())
              ar_new->Dim(k)->Init(&Dummy_Too_Messy_Access_Vector,
                                   &LNO_local_pool);
            else if (av->Loop_Coeff(av->Nest_Depth()-1)!=0)
              ar_new->Dim(k)->Init(&Dummy_Access_Vector, &LNO_local_pool);
          }
          temp_map.Set_Access_Array(ar_new);
        }

        //const NAME2BIT *name_map=bit_pos_mapping->Find(temp_map)->Get_Data();
        BINARY_TREE_NODE<NAME2BIT> *ptr=bit_pos_mapping->Find(temp_map);
        const NAME2BIT *name_map=ptr->Get_Data();
  
        // get the stmt containing this ref
        WN* wn=Find_Stmt_Under(array_node,body);
        UINT temp_stmt_id=stmt_id->Find(wn); // get the stmt_id

        stmt_name_set[temp_stmt_id].Set(name_map->Get_Bit_Position());
      }
    }
  }

  SCALAR_STACK *scalar_ref_list[2];
  scalar_ref_list[0]=scalar_reads;
  scalar_ref_list[1]=scalar_writes;

  // look at both reads and writes
  for (i=0; i<2; i++) {

    for (INT j=0; j<scalar_ref_list[i]->Elements(); j++) {
      SCALAR_NODE* sjnode=scalar_ref_list[i]->Bottom_nth(j);

      for (INT k=0; k<sjnode->Elements(); k++) {
  
        WN* scalar_ref=sjnode->Bottom_nth(k)->Wn;
        NAME2BIT temp_map;

        // for scalar references, get the mapping from name of
        // variable to bit position
        temp_map.Set_Symbol(scalar_ref);
        const NAME2BIT *name_map=bit_pos_mapping->Find(temp_map)->Get_Data();
  
        // get the stmt containing this ref
        WN* wn=Find_Stmt_Under(scalar_ref,body);
        UINT temp_stmt_id=stmt_id->Find(wn); // get the stmt_id

        stmt_name_set[temp_stmt_id].Set(name_map->Get_Bit_Position());
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

static UINT_DYN_ARRAY* merge_scc_to_form_new_loop(
	UINT		total_scc,	// total number of SCCs
        FF_STMT_LIST*	scc,		// list of statements for SCCs
	BIT_VECTOR*	scc_name_set,	// name bit_vectors for SCCs
	REGISTER_MODEL*	reg_model,	// reg. models for SCCs
	BIT_VECTOR	Expandable_Scalar_Set,
					// name bit_vectors for expandable
					// scalar variables
	WN*		loop,		// loop enclosing the SCCs
	SCC_DIRECTED_GRAPH16 *scc_dep_g	// SCC dependence graph
	)
{
  
  // the queue for SCC available to be merged, order is not important
  INT* scc_queue=CXX_NEW_ARRAY(INT,total_scc+1,&INNER_FISSION_default_pool);
  INT* scc_score=CXX_NEW_ARRAY(INT,total_scc+1,&INNER_FISSION_default_pool);
  UINT head=0;	// head of scc_queue
  UINT tail=0;	// tail of scc_queue

  // initially, only those SCCs without any predecessor are available
  UINT i;
  for (i=1; i<=total_scc; i++) {
    if (scc_dep_g->Get_In_Edge(i) == 0)
      scc_queue[head++]=i;
  }

  UINT fp_limit=Get_Limit();	// FP register number available on targ arch
  UINT int_limit=Target_INTRs;	// INT register number available on targ arch
  UINT fp_threshold=fp_limit;	// threshold<limit where we want to
				// stop merging SCCs because of high
				// failure rate
  UINT int_threshold=int_limit;
  UINT tlb_limit=Mhd.L[0].TLB_Entries;
  UINT tlb_threshold=tlb_limit;

  // store sccs chosen as seeds
  UINT_DYN_ARRAY *seed_scc=CXX_NEW(UINT_DYN_ARRAY(&INNER_FISSION_default_pool),
                    &INNER_FISSION_default_pool);

  while (tail<head) {

    // select SCC 's' which will result in most of the SCCs to be put into
    // the queue

    UINT best_score=0;
    UINT best_scc=tail;
    for (i=tail; i!=head; i++) {
      UINT score=0;	// counts how many successors can become
			// available after the current SCC is removed
      EINDEX16 e=scc_dep_g->Get_Out_Edge(scc_queue[i]);
      while (e) {
  
        VINDEX16 v=scc_dep_g->Get_Sink(e);
        if ((scc_dep_g->Get_In_Edge(v)==e) &&
            (scc_dep_g->Get_Next_In_Edge(e)==0)) {
          score++;
        }
        e=scc_dep_g->Get_Next_Out_Edge(e);
      }
      if (score>best_score) {
        best_score=score;
        best_scc=i;
      }
    }

    UINT seed_scc_id=scc_queue[best_scc];	// scc is the seed SCC
    scc_queue[best_scc]=scc_queue[--head];	// remove seed SCC from queue
    if (LNO_Test_Dump)
      printf("Choose scc %d as seed\n", seed_scc_id);
    UINT loop_id=seed_scc->Newidx();
    (*seed_scc)[loop_id]=seed_scc_id;

    // remove all out-edges of scc and put new candidate SCCs in queue
    EINDEX16 e=scc_dep_g->Get_Out_Edge(seed_scc_id);
    while (e) {
  
      VINDEX16 v=scc_dep_g->Get_Sink(e);
      scc_dep_g->Delete_Edge(e);
      if (scc_dep_g->Get_In_Edge(v)==0) {
        scc_queue[head++]=v;
      }
      e=scc_dep_g->Get_Next_Out_Edge(e);
    }

    INT fpr,intr,tlb;
    reg_model[seed_scc_id].Calculate_Register_Usage(loop,&fpr,&intr,&tlb);
    if (fpr<=fp_threshold && intr<=int_threshold && tlb <=tlb_threshold) {
      // need to merge with other SCC

      // now selecet the SCC to be merged. Use the following heuristics
      // score=
      //     +10 * reuse of names in expandable scalar set
      //                                     (avoid scalar expansion)
      //     -20 * new use of names in expandable scalar set
      //                                     (penalize new scalar expansion)
      //      +1 * reuse of names not in expandable scalar set
      //                                     (encourage reuse)
      //      -2 * new use of names not in expandable scalar set
      //                                     (penalize new names introduced)
      INT best_fp_reg_usage=fp_limit;	// to avoid recompute of reg usage
      INT best_int_reg_usage=int_limit;	// to avoid recompute of reg usage
      INT best_tlb_usage=tlb_limit;	// to avoid recompute of reg usage
      do {

        INT32 best_score;
        INT j;
        for (j=tail; j<head; j++) {
          BIT_VECTOR name_set= scc_name_set[seed_scc_id];
          BIT_VECTOR k= scc_name_set[scc_queue[j]];
          scc_score[j]=
            10* (k &  name_set &  Expandable_Scalar_Set).Pop_Count()
           -20* (k & ~name_set &  Expandable_Scalar_Set).Pop_Count()
           + 1* (k &  name_set & ~Expandable_Scalar_Set).Pop_Count()
           - 2* (k & ~name_set & ~Expandable_Scalar_Set).Pop_Count();
        }
        for (j=tail; j<head; j++) {
          best_score=scc_score[j];
          UINT best_score_j1=j;
          for (INT j1=j+1; j1<head; j1++) {
            if (scc_score[j1]>best_score) {
              best_score=scc_score[j1];
              best_score_j1=j1;
            }
          }
          if (best_score_j1!=j) {
            INT t=scc_score[j];
            scc_score[j]=scc_score[best_score_j1];
            scc_score[best_score_j1]=t;
            t=scc_queue[j];
            scc_queue[j]=scc_queue[best_score_j1];
            scc_queue[best_score_j1]=t;
          }
        }

        INT best_j= -1;			// best scc to be merged
        for (j=tail; j<head; j++) {
          if (LNO_Test_Dump) {
            printf("Test scc %d for merging, score=%d",
                   scc_queue[j],scc_score[j]);
          }
          INT temp_fp_reg_usage;
          INT temp_int_reg_usage;
          INT temp_tlb_usage;
          reg_used_if_merged(
              &scc[seed_scc_id], &scc[scc_queue[j]], loop,
	      &temp_fp_reg_usage, &temp_int_reg_usage, &temp_tlb_usage);
          if (temp_fp_reg_usage<=fp_limit && temp_int_reg_usage<=int_limit &&
	      temp_tlb_usage<=tlb_limit) {
  
            best_j=j;
            best_score=scc_score[j];
            best_fp_reg_usage=temp_fp_reg_usage;
            best_int_reg_usage=temp_int_reg_usage;
            best_tlb_usage=temp_tlb_usage;
            if (LNO_Test_Dump)
              printf(" is the best.\n");
            break;
          } else
            if (LNO_Test_Dump)
              printf(" but uses too many (%d+%d+%d) registers.\n",
                       temp_fp_reg_usage, temp_int_reg_usage,temp_tlb_usage);
            else;
        }

        if (LNO_Run_P3==TWO_PASS && best_score <= -20) {
          if (LNO_Test_Dump)
            printf("Best score is too low. Stop merging with the seed scc.\n");
          break;
        } else if (best_j != -1) {
  
          INT merged_scc=scc_queue[best_j]; // selected SCC to be merged
          if (LNO_Test_Dump) {
            printf("Merge with scc %d\n", merged_scc);
          }
          scc_queue[best_j]=scc_queue[--head];// remove selected SCC from queue

          // combine info related to the merged SCC to those of seed SCC
          FF_STMT_ITER s_iter(&scc[merged_scc]);
          for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
            stmt_node=s_iter.Next())
            reg_model[seed_scc_id].Add_Statement(stmt_node->Get_Stmt());
          scc[seed_scc_id].Append_List(&scc[merged_scc]);
          //scc_name_set[seed_scc_id].Set(scc_name_set[merged_scc]);
          scc_name_set[seed_scc_id]|=scc_name_set[merged_scc];

          // after merging the new SCC, remove edges out of the new SCC
          // if new SCC becomes available, put it into scc_queue[]
          EINDEX16 e=scc_dep_g->Get_Out_Edge(merged_scc);
          while (e) {
    
            VINDEX16 v=scc_dep_g->Get_Sink(e);
            scc_dep_g->Delete_Edge(e);
            if (scc_dep_g->Get_In_Edge(v)==0) {
              scc_queue[head++]=v;
            }
            e=scc_dep_g->Get_Next_Out_Edge(e);
          }
        } else break; // cannot find a scc to merge, stop searching
      } while (best_fp_reg_usage<=fp_threshold &&
               best_int_reg_usage<=int_threshold &&
	       best_tlb_usage<=tlb_threshold); // if reg usage is too high
		// after merging and future merging is not likely to succeed,
		// stop searching, too
    } 
  }

  if (LNO_Run_P3==TWO_PASS) {

    STACK<UINT> scc_stack(&INNER_FISSION_default_pool);

    for (INT j=seed_scc->Lastidx()-1; j>=0; j--) {
      UINT scc1=(*seed_scc)[j];
      UINT scc2=(*seed_scc)[j+1];
      INT temp_fp_reg_usage, temp_int_reg_usage, temp_tlb_usage;
      reg_used_if_merged(
            &scc[scc1], &scc[scc2], loop,
            &temp_fp_reg_usage, &temp_int_reg_usage, &temp_tlb_usage);
      if (temp_fp_reg_usage<=fp_limit && temp_int_reg_usage<=int_limit &&
	  temp_tlb_usage <= tlb_limit) {

        if (LNO_Test_Dump) {
          printf("Re-Merge scc %d with scc %d\n", scc1, scc2);
        }
        // combine info related to the merged SCC to those of seed SCC
        FF_STMT_ITER s_iter(&scc[scc2]);
        for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
          stmt_node=s_iter.Next())
          reg_model[scc1].Add_Statement(stmt_node->Get_Stmt());
        scc[scc1].Append_List(&scc[scc2]);
        scc_name_set[scc1]|=scc_name_set[scc2];
  
      } else
        scc_stack.Push(scc2);
    }
    UINT seed_scc0 = (*seed_scc)[0];
    scc_stack.Push(seed_scc0);

    seed_scc->Resetidx();
    for (i=0; i<scc_stack.Elements(); i++) {
      seed_scc->Newidx();
      (*seed_scc)[i]=scc_stack.Top_nth(i);
    }
  }

  return (seed_scc);
}

static BOOL fission_is_better(
	UINT_DYN_ARRAY* new_loops,
	FF_STMT_LIST* scc,
	REGISTER_MODEL* reg_model,
	WN* loop,
	double orig_cycles,
	FF_STMT_LIST& expandable_ref_list)
{
  WN* body=WN_do_body(loop);
  UINT total_loops=new_loops->Lastidx()+1;

  if (total_loops<2)
    return FALSE;

  // hash table which maps a statement to a result loop (id)
  WN2INT *stmt_to_loop=
  CXX_NEW(WN2INT(ESTIMATED_SIZE, &INNER_FISSION_default_pool),&INNER_FISSION_default_pool);

  UINT i;
  for (i=0; i<total_loops; i++) {

    UINT seed_scc=(*new_loops)[i];
    FF_STMT_ITER s_iter(&scc[seed_scc]);
    for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
      stmt_node=s_iter.Next()) {
      WN* stmt=stmt_node->Get_Stmt();
      stmt_to_loop->Enter(stmt,i);
    }
  }

  WN2INT *se_needed=
  CXX_NEW(WN2INT(ESTIMATED_SIZE, &INNER_FISSION_default_pool),
                                 &INNER_FISSION_default_pool);
  FF_STMT_ITER r_iter(&expandable_ref_list);
  for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
      ref_node=r_iter.Next()) {
      WN* ref=ref_node->Get_Stmt();
      WN* stmt0=Find_Stmt_Under(ref,body);
      WN* wn_eq_loop = NULL; 
      STACK<WN*>* equivalence_class=
        Scalar_Equivalence_Class(ref, Du_Mgr, &INNER_FISSION_default_pool,
          TRUE, &wn_eq_loop);
      BOOL need_expansion=FALSE;
      while (!equivalence_class->Is_Empty() && !need_expansion) {
        WN* ref1=equivalence_class->Pop();
        WN* stmt1=Find_Stmt_Under(ref1,body);
        if (stmt_to_loop->Find(stmt0)!=stmt_to_loop->Find(stmt1))
          need_expansion=TRUE;
      }
      if (need_expansion) {
        se_needed->Enter(ref,1);
      }
  }

  double total_cycles=0.0;
  for (i=0; i<total_loops; i++) {

    INT fpr;
    INT intr;
    INT tlb;
    double cycles;
    UINT seed_scc=(*new_loops)[i];
    reg_model[seed_scc].Evaluate(loop,se_needed,NULL,&cycles,&fpr,&intr,&tlb);

    if (fpr>Target_FPRs || intr>Target_FPRs || tlb>Mhd.L[0].TLB_Entries)
      return FALSE;

    total_cycles += cycles;

    if (total_cycles>orig_cycles)
      return FALSE;

  }

  return TRUE;

}

static void separate_loop_and_scalar_expand(
	UINT_DYN_ARRAY* new_loops,
	FF_STMT_LIST* scc,
	REGISTER_MODEL* reg_model,
	WN* loop,
	FF_STMT_LIST& expandable_ref_list,
        WN2UINT* stmt_id,
        DYN_ARRAY<UINT>* line_number)
{
  char tlog_message[1024];
  tlog_message[0]='\0';
  WN* body=WN_do_body(loop);
  UINT total_loops=new_loops->Lastidx()+1;
  UINT *loop_size=CXX_NEW_ARRAY(UINT,total_loops,&INNER_FISSION_default_pool);
  // hash table which maps a statement to a result loop (id)
  WN2INT *stmt_to_loop=
  CXX_NEW(WN2INT(ESTIMATED_SIZE, &INNER_FISSION_default_pool),&INNER_FISSION_default_pool);

  SRCPOS srcpos=Srcpos_To_Line(WN_Get_Linenum(loop));
  BOOL fission_ok = (total_loops>1);
  UINT i;
  for (i=0; i<total_loops; i++) {

    UINT seed_scc=(*new_loops)[i];
    UINT total_stmt=0;
    FF_STMT_ITER s_iter(&scc[seed_scc]);
    for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
      stmt_node=s_iter.Next()) {
      WN* stmt=stmt_node->Get_Stmt();
      stmt_to_loop->Enter(stmt,i);
      LWN_Insert_Block_Before(body,NULL,LWN_Extract_From_Block(stmt));
      total_stmt++;
    }
    loop_size[i]=total_stmt;
    INT fpr;
    INT intr;
    INT tlb;
    reg_model[seed_scc].Calculate_Register_Usage(loop,&fpr,&intr,&tlb);
    if (LNO_Verbose) {
      char message[256];
      sprintf(message, "New loop %d", i);
      sprintf(message+strlen(message),
             " has %d stmts and uses %d FP and %d INT and %d TLB registers\n", 
		  total_stmt, fpr,intr,tlb);
      inner_fission_verbose_info(srcpos, message);
    }
    if (LNO_Tlog) {
      char message[256];
      sprintf(message, "%d (%d %d %d) ", total_stmt, fpr,intr,tlb);
      if (strlen(tlog_message)+strlen(message)<1024)
        strcpy(tlog_message+strlen(tlog_message), message);
    }

  }

  if (LNO_Tlog && fission_ok)
    inner_fission_tlog_info(total_loops, srcpos, loop, tlog_message);

  if (LNO_Analysis) {

    if (fission_ok) {
      fprintf(LNO_Analysis, "( LNO_Inner_Loop_Fission_Success (%d %d %d %d) %d ",
           Srcpos_To_Line(WN_Get_Linenum(loop)),
           Total_FP_Register_Usage,
           Total_INT_Register_Usage, Total_TLB_Usage, total_loops);

      BOOL reg_pressure=FALSE;

      for (UINT i=0; i<total_loops; i++) {
        fprintf(LNO_Analysis,"( ");

        UINT seed_scc=(*new_loops)[i];
        UINT total_stmt=0;
        FF_STMT_ITER s_iter(&scc[seed_scc]);
        for (FF_STMT_NODE* stmt_node=s_iter.First(); !s_iter.Is_Empty();
          stmt_node=s_iter.Next()) {
          WN* stmt=stmt_node->Get_Stmt();
          total_stmt++;

          UINT sid=stmt_id->Find(stmt);
          if (fission_ok)
            fprintf(LNO_Analysis,"( %d %d ) ",
              (*line_number)[sid], (*line_number)[sid]);
        }

	INT fp_reg_used;
	INT int_reg_used;
	INT tlb_used;
        reg_model[seed_scc].Calculate_Register_Usage(loop,
            &fp_reg_used, &int_reg_used,&tlb_used);

        fprintf(LNO_Analysis,"%d) ", fp_reg_used);

        if (fp_reg_used>Get_Limit() || int_reg_used>Target_INTRs ||
	    tlb_used>Mhd.L[0].TLB_Entries)
          reg_pressure=TRUE;
      }

      if (reg_pressure==FALSE)
        fprintf(LNO_Analysis,
          "\"Register pressure eased by fission.\")\n");
      else
        fprintf(LNO_Analysis,
          "\"Register pressure partially eased by fission.\")\n");

    } else {

      fprintf(LNO_Analysis, "( LNO_Inner_Loop_Fission_Failure (%s %d %d %d %d) ",
           Cur_PU_Name, Srcpos_To_Line(WN_Get_Linenum(loop)),
           Total_FP_Register_Usage, Total_INT_Register_Usage, Total_TLB_Usage);
      fprintf(LNO_Analysis,
        "\"Fission failed due to strong dependences of statements.\")\n");
    }

  }

  if (total_loops>1) {
    BOOL has_calls_or_gotos_or_inner_loops = FALSE;
    DO_LOOP_INFO* loop_info=Get_Do_Loop_Info(loop, FALSE);
    if (loop_info->Has_Calls || loop_info->Has_Gotos || !loop_info->Is_Inner) {
      has_calls_or_gotos_or_inner_loops = TRUE;
    }

    STACK<WN*> se_stack(&INNER_FISSION_default_pool);
    STACK<BOOL> finalize_stack(&INNER_FISSION_default_pool);
    FF_STMT_ITER r_iter(&expandable_ref_list);
    BOOL need_expansion = FALSE; 
    BOOL need_finalization = FALSE; 
    for (FF_STMT_NODE* ref_node=r_iter.First(); !r_iter.Is_Empty();
        ref_node=r_iter.Next()) {
        WN* ref=ref_node->Get_Stmt();
        WN* stmt0=Find_Stmt_Under(ref,body);
        WN* wn_eq_loop = NULL; 
        STACK<WN*>* equivalence_class=
          Scalar_Equivalence_Class(ref, Du_Mgr, &INNER_FISSION_default_pool,
            TRUE, &wn_eq_loop);
	FmtAssert(wn_eq_loop == NULL || Wn_Is_Inside(loop, wn_eq_loop), 
          ("separate_loop_and_scalar_expand: not really scalar expandable")); 
        BOOL expand = FALSE;
        while (!equivalence_class->Is_Empty() && !expand) {
          WN* ref1=equivalence_class->Pop();
          WN* stmt1=Find_Stmt_Under(ref1,body);
          if (stmt_to_loop->Find(stmt0)!=stmt_to_loop->Find(stmt1))
            expand = TRUE;
        }
        if (expand) {
	  need_expansion = TRUE; 
          se_stack.Push(ref);
	  BOOL finalize = wn_eq_loop != NULL;  
	  finalize_stack.Push(finalize); 
	  if (finalize) 
	    need_finalization = TRUE; 
        }
    }

    WN* guard_tests[1];
    guard_tests[0] = NULL; 
    if (need_finalization) 
      SE_Guard_Tests(loop, 1, guard_tests, Do_Loop_Depth(loop));  
    WN* tile_loop = NULL; 
    if (need_expansion && !Get_Trace(TP_LNOPT, TT_LNO_BIG_SCALAR_TILES)) {
      tile_loop = SE_Tile_Inner_Loop(loop, &LNO_default_pool);
    }

    while (!se_stack.Is_Empty()) {
        WN* ref=se_stack.Pop();
        BOOL finalize = finalize_stack.Pop(); 
	if (tile_loop != NULL) {   
	  SYMBOL sym(ref);
	  WN* loops[1]; 
	  loops[0] = loop; 
	  WN* tile_loops[1];
	  tile_loops[0] = tile_loop;
	  INT strip_sizes[1]; 
	  strip_sizes[0] = (INT) Step_Size(tile_loop, 0);  
	  INT order[1]; 
	  order[0] = 0; 
	  Scalar_Expand(tile_loop, loop, ref, sym, loops,  order, 1, TRUE, 
            finalize, FALSE, guard_tests, NULL, tile_loops, strip_sizes, 1);  
	} else { 
	  INT dummy[1]={0};
	  SYMBOL sym(ref);
	  Scalar_Expand(loop, loop, ref, sym, &loop, dummy, 1, TRUE, finalize, 
	    FALSE, guard_tests);
	} 
    }

    WN* tmp_loop1=loop;
    WN** wn_starts=CXX_NEW_ARRAY(WN*, total_loops, &INNER_FISSION_default_pool);
    WN** wn_ends=CXX_NEW_ARRAY(WN*, total_loops, &INNER_FISSION_default_pool);
    WN** wn_steps=CXX_NEW_ARRAY(WN*, total_loops, &INNER_FISSION_default_pool);
    WN** new_loops=CXX_NEW_ARRAY(WN*, total_loops, &INNER_FISSION_default_pool);

    wn_starts[0]=WN_kid0(WN_start(tmp_loop1));
    wn_ends[0]=WN_end(tmp_loop1);
    wn_steps[0]=WN_kid0(WN_step(tmp_loop1));
    new_loops[0]=loop;
    WN* stmt=WN_first(body);
    for (i=0; i<total_loops-1; i++) {
  
      INT size=loop_size[i];

      for (INT j=0; j<size; j++)
        stmt=WN_next(stmt);

      WN* tmp_loop2;

      Separate(tmp_loop1, WN_prev(stmt), 1, &tmp_loop2);
      LWN_Parentize(tmp_loop2);
      DO_LOOP_INFO* new_loop_info =
        CXX_NEW(DO_LOOP_INFO(loop_info,&LNO_default_pool), &LNO_default_pool);
      Set_Do_Loop_Info(tmp_loop2,new_loop_info);
      if (has_calls_or_gotos_or_inner_loops) {
        // should check gotos and calls when they are allowed to be in
        // loops handled by inner_fission phase
      }
      wn_starts[i+1]=WN_kid0(WN_start(tmp_loop2));
      wn_ends[i+1]=WN_end(tmp_loop2);
      wn_steps[i+1]=WN_kid0(WN_step(tmp_loop2));
      new_loops[i+1]=tmp_loop2;

      tmp_loop1=tmp_loop2;
    }

    Fission_DU_Update(Du_Mgr,red_manager,wn_starts,wn_ends,wn_steps,
      total_loops,new_loops);
    for (i=0; i<total_loops-1; i++) {
      scalar_rename(LWN_Get_Parent(wn_starts[i]));
    }

    adg->Fission_Dep_Update(new_loops[0],total_loops);
  }

}

// Fission a inner loop 'innerloop' such that it uses less FP registers
// than that provided by the target architectrure or by user flag.
extern INT Fission_Inner_Loop(WN* innerloop,
BOOL known_to_have_register_allocation_problem)
{

  if (!Do_Loop_Is_Good(innerloop) || Do_Loop_Has_Calls(innerloop) ||
	Do_Loop_Has_Gotos(innerloop)) {
    Is_True(0, ("Bad loop passed to Fission_Inner_Loop().\n"));
    return 0;
  }
  if (!Do_Loop_Is_Inner(innerloop)) {
    Is_True(0, ("Non-innermost loop passed to Fission_Inner_Loop().\n"));
    return 0;
  }
  DO_LOOP_INFO* dli=Get_Do_Loop_Info(innerloop);
  if (dli->Has_Gotos || dli->Has_Calls) {
    Is_True(0, ("Loop with gotos or calls passed to Fission().\n"));
    return 0;
  }

  char source_line[80];
  if (strlen(Cur_PU_Name)>=65) {
    sprintf(source_line,"%s:%d", "name_too_long",
          Srcpos_To_Line(WN_Get_Linenum(innerloop)));
  } else
    sprintf(source_line,"%s:%d", Cur_PU_Name,
          Srcpos_To_Line(WN_Get_Linenum(innerloop)));
  SRCPOS srcpos=Srcpos_To_Line(WN_Get_Linenum(innerloop));

  // if there are too few iterations, we will not fission
  if (!dli->Aggressive_Inner_Fission &&
       (dli->Est_Num_Iterations < Iteration_Count_Threshold)) {
    if (LNO_Verbose)
      inner_fission_verbose_info(srcpos,
        "Loops with too few iterations for fission\n");
    return 1;
  }

  MEM_POOL_Push(&INNER_FISSION_default_pool);
  {

  WN2INT *se_needed=
    CXX_NEW(WN2INT(ESTIMATED_SIZE, &INNER_FISSION_default_pool),
                                 &INNER_FISSION_default_pool);

  REGISTER_MODEL *orig_reg_model=
	CXX_NEW(REGISTER_MODEL(&INNER_FISSION_default_pool),
	&INNER_FISSION_default_pool);
  WN *stmt;
  for (stmt=WN_first(WN_do_body(innerloop)); stmt; stmt=WN_next(stmt)) {
    orig_reg_model->Add_Statement(stmt);
  }
  double orig_cycles;
  INT orig_fpr;
  INT orig_intr;
  INT orig_tlb;
  orig_reg_model->Evaluate(
	innerloop,se_needed,NULL,&orig_cycles,&orig_fpr,&orig_intr,&orig_tlb);
  if (LNO_Verbose)
    printf("Before inner_fission: %f cycles, %d fpr, %d intr, %d tlb\n",
	orig_cycles,orig_fpr,orig_intr,orig_tlb);

  if (dli->Est_Register_Usage.Est_Fp_Regs() >= 0 ||
      dli->Est_Register_Usage.Est_Int_Regs() >= 0 ||
      dli->Est_Register_Usage.Est_TLB() >= 0) {
    Total_FP_Register_Usage = dli->Est_Register_Usage.Est_Fp_Regs();
    Total_INT_Register_Usage = dli->Est_Register_Usage.Est_Int_Regs();
    Total_TLB_Usage= dli->Est_Register_Usage.Est_TLB();
  }
  
  // if we do not know if this loop has register allocation problem,
  // we ask the model to see if all statements can be put into the same
  // loop and can still be register allocated
  if (!known_to_have_register_allocation_problem) {
    REGISTER_MODEL *test_reg_model=
        CXX_NEW(REGISTER_MODEL(&INNER_FISSION_default_pool),
	&INNER_FISSION_default_pool);
    WN* body=WN_do_body(innerloop);
    WN* stmt;
    for (stmt=WN_first(body); stmt; stmt=WN_next(stmt)) {
      test_reg_model->Add_Statement(stmt);
    }
    test_reg_model->Calculate_Register_Usage(innerloop,
	&Total_FP_Register_Usage,&Total_INT_Register_Usage,&Total_TLB_Usage);
    if (LNO_Verbose) {
      char message[256];
      sprintf(message,"Model estimates %d FP and %d INT and %d TLB registers required",
          Total_FP_Register_Usage, Total_INT_Register_Usage,Total_TLB_Usage);
      inner_fission_verbose_info(srcpos, message);
    }
    if (!dli->Aggressive_Inner_Fission &&
        Total_FP_Register_Usage<=Get_Limit() &&
        Total_INT_Register_Usage<=Target_INTRs &&
	Total_TLB_Usage<=Mhd.L[0].TLB_Entries) {
      CXX_DELETE(test_reg_model,&INNER_FISSION_default_pool);
      MEM_POOL_Pop(&INNER_FISSION_default_pool);
      if (LNO_Verbose) {
        char message[256];
        sprintf(message,
          "Est. %d FP (<=%d) and %d INT (<=%d) and %d TLB (<=%d) regs, no fission is needed",
          Total_FP_Register_Usage, Get_Limit(),
          Total_INT_Register_Usage, Target_INTRs,
	  Total_TLB_Usage,Mhd.L[0].TLB_Entries);
        inner_fission_verbose_info(srcpos, message);
      }
      return 1;
    }
    CXX_DELETE(test_reg_model,&INNER_FISSION_default_pool);
  }
  if (LNO_Verbose) {
    char message[256];
    sprintf(message,"Model estimates %d FP and %d INT registers required",
          Total_FP_Register_Usage, Total_INT_Register_Usage);
    inner_fission_verbose_info(srcpos, message);
  }

  INT Split_Region(WN *region, ARRAY_DIRECTED_GRAPH16 *dep_graph);
  if (!Split_Region(innerloop,adg)) {
    MEM_POOL_Pop(&INNER_FISSION_default_pool);
    return 0;
  }

  // a dictionary used for looking up the bit position for a symbol
  BINARY_TREE<NAME2BIT> *mapping_dictionary = 
    CXX_NEW(BINARY_TREE<NAME2BIT>(&INNER_FISSION_default_pool),&INNER_FISSION_default_pool);

  // TODO: break huge statements here

  // main statement dependence graph for statements in the loop
  SCC_DIRECTED_GRAPH16 *dep_g_p =
    CXX_NEW(SCC_DIRECTED_GRAPH16(ESTIMATED_SIZE,ESTIMATED_SIZE),
    &INNER_FISSION_default_pool);

  // hash table which associates the statements in the loop and vertices in the
  // above dependence graph 'dep_g_p'
  WN2VINDEX *stmt_to_vertex=
  CXX_NEW(WN2VINDEX(ESTIMATED_SIZE, &INNER_FISSION_default_pool),
    &INNER_FISSION_default_pool);

  // hash table which maps a statement to a integer statement id
  WN2UINT *stmt_id=
  CXX_NEW(WN2UINT(ESTIMATED_SIZE, &INNER_FISSION_default_pool),&INNER_FISSION_default_pool);

  DYN_ARRAY<UINT32> line_number(&INNER_FISSION_default_pool);


  REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&INNER_FISSION_default_pool),
        &INNER_FISSION_default_pool);
  REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&INNER_FISSION_default_pool),
        &INNER_FISSION_default_pool);

  SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&INNER_FISSION_default_pool),
        &INNER_FISSION_default_pool);
  SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&INNER_FISSION_default_pool),
        &INNER_FISSION_default_pool);
  SCALAR_REF_STACK *params =
        CXX_NEW(SCALAR_REF_STACK(&INNER_FISSION_default_pool), &INNER_FISSION_default_pool);

  // stack used in collecting references
  DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&INNER_FISSION_default_pool),
                              &INNER_FISSION_default_pool);
  Build_Doloop_Stack(innerloop, stack);

  // step 1: gather all (scalar and array) references in the loop
  //         allocate a vertex in the stmt. dep. graph for each stmt
  //         assign statement id for each statement
  UINT stmt_count=0;
  INT32 gather_status=0;
  Init_Ref_Stmt_Counter();
  WN* body=WN_do_body(innerloop);
  for (stmt=WN_first(body); stmt && gather_status!= -1; stmt=WN_next(stmt)) {
    UINT i=line_number.Newidx();
    line_number[i]=Srcpos_To_Line(WN_Get_Linenum(stmt));
    VINDEX16 v=dep_g_p->Add_Vertex();
    if (v==0) {
      MEM_POOL_Pop(&INNER_FISSION_default_pool);
      return(0);
    }
    stmt_to_vertex->Enter(stmt, v);
    stmt_id->Enter(stmt, stmt_count++);
    gather_status=New_Gather_References(stmt,writes,reads,stack,
        scalar_writes,scalar_reads,
        params,&INNER_FISSION_default_pool) ;
  }
  if (gather_status == -1) {
    DevWarn("Error in gathering references");
    MEM_POOL_Pop(&INNER_FISSION_default_pool);
    return 0;
  }

  line_number[line_number.Newidx()]=
    line_number[stmt_count-1]+1;
    //Srcpos_To_Line(WN_Get_Linenum(WN_next(innerloop)));

  // list of references that use scalar-expandable variables
  FF_STMT_LIST expandable_ref_list;

  // step 2: examine all reads and writes and do the following
  //		1. classify them as scalar or array
  //		2. create name to bit position mappings for new symbol names
  //		3. if the ref is STID, check if it is scalar expandable
  UINT sym_count=inner_fission_2(innerloop, scalar_reads, scalar_writes,
         reads, writes,
	 mapping_dictionary, expandable_ref_list, &INNER_FISSION_default_pool);

  // name set bit vector array for all statements
  // each entry correspond to a statement and is a bit vector which
  // shows which symbol's name appear in the statement
  BIT_VECTOR* stmt_name_set=
                CXX_NEW_ARRAY(BIT_VECTOR, stmt_count, &INNER_FISSION_default_pool);

  // for each statement i, stmt_name_set[i] is a bit vector which is
  // initialized to be large enough to hold 'sym_count' symbols
  UINT i;
  for (i=0; i<stmt_count; i++)
    stmt_name_set[i].Init(sym_count, &INNER_FISSION_default_pool);

  // we also need to have a set of expandable scalars
  BIT_VECTOR Expandable_Scalar_Set(sym_count, &INNER_FISSION_default_pool);

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

  Register_Name_To_Statement(
	innerloop,scalar_reads,scalar_writes,reads,writes,
	stmt_id,stmt_name_set,mapping_dictionary);

  WN_MAP sdm=WN_MAP_Create(&INNER_FISSION_default_pool);
  ARRAY_DIRECTED_GRAPH16 *sdg =
    CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH),
      &INNER_FISSION_default_pool);

  for (stmt = WN_first(body); stmt; stmt = WN_next(stmt)) {
    if (!Map_Stmt_To_Level_Graph(stmt,sdg)) {
      DevWarn("Statement dependence graph problem");
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&INNER_FISSION_default_pool);
      return(0);
    }
  }

  BOOL status=Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
    innerloop, scalar_reads, scalar_writes, params, sdg, red_manager,
    &Expandable_Scalar_Set, mapping_dictionary);
  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&INNER_FISSION_default_pool);
    return(0);
  }

  status=Generate_Array_Dependence_For_Statement_Dependence_Graph(
    innerloop, reads, writes, sdg, red_manager, adg);
  if (status==FALSE) {
    DevWarn("Statement dependence graph problem");
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&INNER_FISSION_default_pool);
    return(0);
  }

  if (LNO_Test_Dump) {
    fprintf(TFile,"Statement Dependence Graph of Inner_Fission Phase:\n");
    sdg->Print(TFile);
  }

  // dep_g_p would not overflow if sdg did not overflow so no checking
  // is needed
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
    fprintf(TFile,
            "Innerloop Statement Dependence Graph of Inner_Fission Phase:\n");
    dep_g_p->Print(TFile);
  }

  // ac_g is the acyclic condensation graph of dep_g_p
  // it stores dependence relations between SCCs
  SCC_DIRECTED_GRAPH16 *ac_g;
  ac_g = dep_g_p->Acyclic_Condensation(&INNER_FISSION_default_pool);

  if (LNO_Test_Dump) {
    fprintf(TFile,"SCC Dependence Graph of Inner_Fission Phase:\n");
    ac_g->Print(TFile);
  }

  VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

  // scc[i] is a list of statemens in i-th SCC
  FF_STMT_LIST *scc;
  scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, &INNER_FISSION_default_pool);

  // reg_model[i] stores FP reg usage for i-th SCC
  REGISTER_MODEL* reg_model=
    CXX_NEW_ARRAY(REGISTER_MODEL,total_scc+1, &INNER_FISSION_default_pool);
  // scc_name_set[i] is a BIT_VECTOR representing the names of the
  // variables used by statements of i-th SCC
  BIT_VECTOR* scc_name_set=
    CXX_NEW_ARRAY(BIT_VECTOR, total_scc+1, &INNER_FISSION_default_pool);

  UINT *scc_size=CXX_NEW_ARRAY(UINT, total_scc+1, &INNER_FISSION_default_pool);

  // initialize reg_model[i] and scc_name_set[i]
  for (i=1; i<=total_scc; i++) {
    reg_model[i].Init(&INNER_FISSION_default_pool);
    scc_name_set[i].Init(sym_count,&INNER_FISSION_default_pool);
    scc_size[i]=0;
  }

  // Append statements to the statement list of proper SCC
  // and proper reg_model. Also set proper bit in scc_name_set
  // according to stmt_name_set for statements in each SCC
  for (stmt = WN_first(WN_do_body(innerloop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id;
    scc_id = dep_g_p->Get_Scc_Id(stmt_to_vertex->Find(stmt));
    scc[scc_id].Append(stmt, &INNER_FISSION_default_pool);  
    reg_model[scc_id].Add_Statement(stmt);
    //scc_name_set[scc_id].Set(stmt_name_set[stmt_id->Find(stmt)]);
    scc_name_set[scc_id]|=stmt_name_set[stmt_id->Find(stmt)];
    scc_size[scc_id]++;
  }

  if (LNO_Test_Dump)
    for (i=1; i<=total_scc; i++) {

      printf("INNER_FISSION:scc %d:", i);
      FF_STMT_ITER s_iter(&scc[i]);
      INT j=0;
      for (FF_STMT_NODE *stmt_node=s_iter.First(); !s_iter.Is_Empty();
	   stmt_node=s_iter.Next()) {
          stmt=stmt_node->Get_Stmt();
          Dump_WN(stmt,stdout,TRUE,4,4);
        j++;
      }
      INT fpr, intr,tlb;
      reg_model[i].Calculate_Register_Usage(innerloop,&fpr,&intr,&tlb);
      printf(" has %d stmts and uses %d FP and %d INT and %d TLB rgisters\n", 
		  j, fpr,intr,tlb);
      printf("name_set=\n");
      scc_name_set[i].Print(stdout);

    }

  if (LNO_Analysis) {
    for (i=1; i<=total_scc; i++) {
      fprintf(LNO_Analysis,
        "( LNO_Inner_Loop_Fission_Scc (%s %d %d %d) ( ",
        Cur_PU_Name, Srcpos_To_Line(WN_Get_Linenum(innerloop)), i,scc_size[i]);
      FF_STMT_ITER s_iter(&scc[i]);
      for (FF_STMT_NODE *stmt_node=s_iter.First(); !s_iter.Is_Empty();
        stmt_node=s_iter.Next()) {
        stmt=stmt_node->Get_Stmt();
        fprintf(LNO_Analysis, "%d ", Srcpos_To_Line(WN_Get_Linenum(stmt)));
      }
      fprintf(LNO_Analysis, "))\n");
    }
  }

  UINT_DYN_ARRAY* new_loops;

  if (((DO_LOOP_INFO*)Get_Do_Loop_Info(innerloop))->Aggressive_Inner_Fission) {
    // do not merge sccs
    new_loops=CXX_NEW(UINT_DYN_ARRAY(&INNER_FISSION_default_pool),
                    &INNER_FISSION_default_pool);
    for (INT j=1; j<=total_scc; j++) {
      UINT i=new_loops->Newidx();
      (*new_loops)[i]=j;
    }
  } else {
    // choose seed SCC and merge it with other SCCs based on heuristics
    new_loops=merge_scc_to_form_new_loop(
        total_scc,scc,scc_name_set,reg_model,
        Expandable_Scalar_Set,innerloop, ac_g);
  }

  // new_loops[i] is the i-th seed SCC


  if (dli->Aggressive_Inner_Fission ||
	fission_is_better(new_loops,scc,reg_model,
	innerloop,orig_cycles,expandable_ref_list)) {
    // separate the loop and expand scalars which is expansdable and has
    // references in different fissions loops
    separate_loop_and_scalar_expand(new_loops,scc,reg_model,
	innerloop,expandable_ref_list,stmt_id,&line_number);

    WN* loop_wn=innerloop;
    if (LNO_Verbose) {
      printf("After inner_fission: ");
      for (i=0; i<new_loops->Elements(); i++) {

        REGISTER_MODEL *tmp_reg_model=
	      CXX_NEW(REGISTER_MODEL(&INNER_FISSION_default_pool),
	      &INNER_FISSION_default_pool);
        for (WN* stmt=WN_first(WN_do_body(loop_wn)); stmt; stmt=WN_next(stmt)) {
          tmp_reg_model->Add_Statement(stmt);
        }
        double tmp_cycles;
        INT tmp_fpr;
        INT tmp_intr;
        INT tmp_tlb;
        tmp_reg_model->Evaluate(
	      loop_wn,se_needed,NULL,&tmp_cycles,&tmp_fpr,&tmp_intr,&tmp_tlb);
        printf("(%f cycles, %d fpr, %d intr, %d tlb) ",
				tmp_cycles,tmp_fpr,tmp_intr, tmp_tlb);

        loop_wn=WN_next(loop_wn);
      }
      printf(" cycles\n");
    }
  } else {
    if (LNO_Verbose)
      printf ("Loop is not fissioned after evaluation\n");
  }

  // TODO need to update scalar dependence info here

  CXX_DELETE(dep_g_p, &INNER_FISSION_default_pool);
  CXX_DELETE(ac_g, &INNER_FISSION_default_pool);
  CXX_DELETE(sdg, &INNER_FISSION_default_pool);
  new_loops->Free_array();
  line_number.Free_array();
  WN_MAP_Delete(sdm);
  }
  MEM_POOL_Pop(&INNER_FISSION_default_pool);

  return 1;
  
}


static void Inner_Fission_Phase_Walk(WN* wn) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && Do_Loop_Is_Inner(wn) && 
	!Do_Loop_Has_Calls(wn) && !Do_Loop_Has_Gotos(wn)) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
      if (WN_first(WN_do_body(wn)) &&     /* make sure body is non-empty */
          (dli->Aggressive_Inner_Fission || 
           (!dli->No_Fission && !dli->Est_Register_Usage.Fits())))
        Fission_Inner_Loop(wn,dli->Est_Register_Usage.Does_Not_Fit());
    } else
      Inner_Fission_Phase_Walk(WN_do_body(wn));
  } else if (opc==OPC_BLOCK)
    for (WN* stmt=WN_first(wn); stmt;) {
      WN* next_stmt=WN_next(stmt);
      Inner_Fission_Phase_Walk(stmt);
      stmt=next_stmt;
    }
  else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Inner_Fission_Phase_Walk(WN_kid(wn,kidno));
    }
}

void Inner_Fission(
  WN* func_nd, 
  ARRAY_DIRECTED_GRAPH16* Array_Dependence_Graph
  )
{
  
  // initializes the static global variables and the mem pools used in
  // inner_fission phase

  MEM_POOL_Initialize(&INNER_FISSION_default_pool,"INNER_FISSION_default_pool",FALSE);
  MEM_POOL_Push(&INNER_FISSION_default_pool);

  adg=Array_Dependence_Graph;

  Inner_Fission_Phase_Walk(func_nd);

  MEM_POOL_Pop(&INNER_FISSION_default_pool);
  MEM_POOL_Delete(&INNER_FISSION_default_pool);

}



