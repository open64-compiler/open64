/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#ifndef opt_defs_INCLUDED
#include "opt_defs.h"
#endif

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.outer.cxx $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <limits.h> 
#include "pu_info.h"
#include "fusion.h"
#include "fiz_fuse.h"
#include "name.h"
#include "lwn_util.h"
#include "wn_simp.h"
#include "lno_bv.h"
#include "ff_utils.h"
#include "wn_map.h"
#include "btree.h"
#include "glob.h"
#include "tlog.h"
#include "parallel.h"
#include "permute.h"

typedef HASH_TABLE<WN*,UINT> WN2UINT;

typedef enum { NORMAL, SKIP } OUTER_FUSION_STATUS;

typedef enum { INFO, FAIL, SUCCEED } INFO_TYPE;

OUTER_FUSION_STATUS Outer_Loop_Fusion_Walk(WN* wn, FIZ_FUSE_INFO* ffi, WN2UINT *wn2ffi);

static void outer_fusion_verbose_info(
  SRCPOS        srcpos1,
  SRCPOS        srcpos2,
  const char*   message)
{
  
  printf("#### Outer Fusion(%d+%d): %s\n",
    Srcpos_To_Line(srcpos1),
    Srcpos_To_Line(srcpos2),
    message);
}

static void outer_fusion_analysis_info(
  INFO_TYPE     info_type,
  SRCPOS        srcpos1,
  SRCPOS        srcpos2,
  UINT32	snl_level1,
  UINT32	snl_level2,
  const char*   message)
{
  
  switch (info_type) {
    case INFO:
      fprintf(LNO_Analysis,"( LNO_Outer_Fusion_Info ");
      break;
    case FAIL:
      fprintf(LNO_Analysis,"( LNO_Outer_Fusion_Failure ");
      break;
    case SUCCEED:
      fprintf(LNO_Analysis,"( LNO_Outer_Fusion_Success ");
      break;
  }

  fprintf(LNO_Analysis,"(%s %d %d) (%s %d %d) \"%s\" )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos1), snl_level1,
    Cur_PU_Name, Srcpos_To_Line(srcpos2), snl_level2,
    message);
}

static void outer_fusion_tlog_info(
  INFO_TYPE     info_type,
  SRCPOS        srcpos1,
  SRCPOS        srcpos2,
  UINT32	snl_level1,
  UINT32	snl_level2,
  const char*   message)
{
  
  char tmp_string[300];
  sprintf(tmp_string,"%d %d %d %d %d",
            info_type, Srcpos_To_Line(srcpos1), Srcpos_To_Line(srcpos2),
            snl_level1, snl_level2);
  Generate_Tlog("LNO","outer_loop_fusion", Srcpos_To_Line(srcpos1), "",
          tmp_string, "", message);
}

#ifndef KEY // moved to common/com/config_lno.* and controllable by a flag
#define OLF_size_upperbound 100		// max size allowed for fusion
#define OLF_size_lowerbound 15		// remove several restriction
					// if size is smaller than this
#endif

static BINARY_TREE<NAME2BIT> *mapping_dictionary;
static UINT Bit_Position_Count=0;
static MEM_POOL OLF_default_pool;

static UINT
Stride_One_Level(REF_LIST_STACK *writes, REF_LIST_STACK *reads, 
	UINT outer_level, UINT level) {

  INT stride_one_level;

  UINT* hist=CXX_NEW_ARRAY(UINT,level+1,&OLF_default_pool);
  INT i;
  for (i=0; i<=level; i++) {
    hist[i] = 0;
  }

  REF_LIST_STACK* ref_list_stack[2];
  ref_list_stack[0]=writes;
  ref_list_stack[1]=reads;
  
  for (INT list_count=0; list_count<2; list_count++)
  for (INT ii=0;ii<ref_list_stack[list_count]->Elements(); ii++) {
    REFERENCE_ITER l_iter(ref_list_stack[list_count]->Bottom_nth(ii));
    for (REFERENCE_NODE* node=l_iter.First(); !l_iter.Is_Empty();
         node=l_iter.Next()) {
      WN* array_node=node->Wn;
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

      ACCESS_ARRAY* aa;
      aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,array_node);

      if (aa->Too_Messy)
        continue;
  
      ACCESS_VECTOR* av=aa->Dim(aa->Num_Vec()-1);
      if (av->Too_Messy || av->Non_Lin_Symb)
        continue;
  
      stride_one_level= -1;
      for (INT i=outer_level; i<=level; i++)
        if (av->Loop_Coeff(i)==0)
          continue;
        else if (av->Loop_Coeff(i)==1 || av->Loop_Coeff(i)==-1)
          if (stride_one_level== -1)
            stride_one_level=i;
          else {
            stride_one_level= -2;
            break;
          }
        else {
          stride_one_level= -2;
          break;
        }
  
      if (stride_one_level>=0)
        hist[stride_one_level]++;
    }
  }

  UINT max_freq=0;
  stride_one_level=-1;
  for (i=outer_level; i<=level; i++)
   if (max_freq<hist[i]) {
     max_freq=hist[i];
     stride_one_level=i;
   }

  return stride_one_level;
}

static BIT_VECTOR*
Array_Names_In_Loop(REF_LIST_STACK *writes, REF_LIST_STACK *reads) {

  BIT_VECTOR* bv=CXX_NEW(BIT_VECTOR(256,&OLF_default_pool), &LNO_local_pool);

  REF_LIST_STACK* ref_list_stack[2];
  ref_list_stack[0]=writes;
  ref_list_stack[1]=reads;
  
  for (INT list_count=0; list_count<2; list_count++)
  for (INT ii=0;ii<ref_list_stack[list_count]->Elements(); ii++) {
    REFERENCE_ITER l_iter(ref_list_stack[list_count]->Bottom_nth(ii));
    for (REFERENCE_NODE* node=l_iter.First(); !l_iter.Is_Empty();
         node=l_iter.Next()) {

       WN* array_node = node->Wn;
       if (OPCODE_is_load(WN_opcode(array_node))) {
         array_node = WN_kid0(array_node);
       } else {
         array_node = WN_kid1(array_node);
       }
       if (WN_operator(array_node) == OPR_ADD) {
         if (WN_operator(WN_kid0(array_node)) == OPR_ARRAY) {
           array_node = WN_kid0(array_node);
         } else if (
           WN_operator(WN_kid1(array_node)) == OPR_ARRAY) {
           array_node = WN_kid1(array_node);
         } else
           continue;
       }

       if (!OPCODE_has_sym(WN_opcode(WN_array_base(array_node))))
         continue;

       NAME2BIT temp_map(WN_array_base(array_node));

       UINT bit_position;
       if ((mapping_dictionary->Find(temp_map))==NULL) {
         if (Bit_Position_Count==256) {
           CXX_DELETE(bv,&LNO_local_pool);
           return NULL;
         }
         bit_position=Bit_Position_Count++;
         temp_map.Set_Bit_Position(bit_position);
         mapping_dictionary->Enter(temp_map);
       } else
         bit_position=
           mapping_dictionary->Find(temp_map)->Get_Data()->Get_Bit_Position();

       bv->Set(bit_position);
    }
  }
  return bv;
}

// Count number of data dependence between array references.
static int
Array_Data_Dependence_Count(REF_LIST_STACK * writes, REF_LIST_STACK * reads)
{
  int count = 0;

  for (int i = 0; i < writes->Elements(); i++) {
    REFERENCE_ITER w_iter(writes->Bottom_nth(i));

    for (REFERENCE_NODE * node1 = w_iter.First(); !w_iter.Is_Empty();
	 node1 = w_iter.Next()) {
      WN * w_node = node1->Wn;
      WN * w_array = (w_node && (WN_operator(w_node) == OPR_ISTORE)) ? WN_kid(w_node,1) : NULL;
      if (w_array && (WN_operator(w_array) != OPR_ARRAY))
	w_array = NULL;

      if (w_array == NULL)
	continue;

      ACCESS_ARRAY * array1 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, w_array);

      if (!array1)
	continue;
      
      for (int j = 0; j < reads->Elements(); j++) {
	REFERENCE_ITER r_iter(reads->Bottom_nth(j));

	for (REFERENCE_NODE * node2 = r_iter.First(); !r_iter.Is_Empty();
	     node2 = r_iter.Next()) {
	  WN * r_node = node2->Wn;
	  WN * r_array = (r_node && (WN_operator(r_node) == OPR_ILOAD)) ? WN_kid(r_node,0) : NULL;
	  
	  if (r_array == NULL)
	    continue;
	  
	  ACCESS_ARRAY * array2 = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, r_array);

	  if (!array2)
	    continue;
	  
	  if (*array1 == *array2) {
	    count++;
	  }
	}
      }
    }
  }
  
  return count;
}

// --------------------------------------------------
// Test if any of the loops in an SNL is parallizable
// --------------------------------------------------
static BOOL
Any_Loop_In_SNL_Parallelizable(WN* loop, INT depth)
{
  if (Get_Do_Loop_Info(loop)->Parallelizable) {
    return TRUE;
  }
  for (INT i = 1; i < depth; ++i) {
    loop = Get_Only_Loop_Inside(loop, FALSE);
    if (Get_Do_Loop_Info(loop)->Parallelizable) {
      return TRUE;
    }
  }
  return FALSE;
}

// Check whether there exists write to formal parameters in 'writes'.
static BOOL Has_parm_ref(REF_LIST_STACK * writes)
{
  for (int i = 0; i < writes->Elements(); i++) {
    REFERENCE_ITER w_iter(writes->Bottom_nth(i));
    for (REFERENCE_NODE * node1 = w_iter.First(); !w_iter.Is_Empty();
	 node1 = w_iter.Next()) {
      WN * wn_node = node1->Wn;
      if (wn_node) {
	OPERATOR opr = WN_operator(wn_node);
	if (OPERATOR_is_store(opr)) {
	  if (OPERATOR_is_scalar_store(opr)) {
	    if (WN_has_sym(wn_node)) {
	      ST *st = WN_st(wn_node);
	      if ((ST_sclass(st) == SCLASS_FORMAL)
		  || (ST_sclass(st) == SCLASS_FORMAL_REF)) {
		return TRUE;
	      }
	    }
	  }
	  else {
	    WN * base = WN_array_base(WN_kid(wn_node,1));
	    if (base && WN_has_sym(base)) {
	      ST * st = WN_st(base);
	      if ((ST_sclass(st) == SCLASS_FORMAL)
		  || (ST_sclass(st) == SCLASS_FORMAL_REF)) {
		return TRUE;
	      }
	    }
	  }
	}
      }
    }
  }
  return FALSE;
}

static FISSION_FUSION_STATUS Fuse_Outer_Loops(WN** loop1_p, WN** loop2_p,
                      FIZ_FUSE_INFO* ffi, WN2UINT *wn2ffi,
                      UINT* fusion_level_io) {

  WN * loop1 = *loop1_p;
  WN * loop2 = *loop2_p;

  Is_True(Do_Loop_Is_Good(loop1) && !Do_Loop_Has_Calls(loop1) &&
				    !Do_Loop_Has_Gotos(loop1), 
          ("Bad loop passed to Fuse_Outer_Loops()."));
  Is_True(Do_Loop_Is_Good(loop2) && !Do_Loop_Has_Calls(loop2) &&
				    !Do_Loop_Has_Gotos(loop2), 
          ("Bad loop passed to Fuse_Outer_Loops()."));

  char loop1_var_name[80];
  char loop2_var_name[80];
  if (strlen(ST_name(WN_st(WN_index(loop1))))>=80) {
    strcpy(loop1_var_name,"name_too_long");
  } else
    strcpy(loop1_var_name,ST_name(WN_st(WN_index(loop1))));
  if (strlen(ST_name(WN_st(WN_index(loop2))))>=80) {
    strcpy(loop2_var_name,"name_too_long");
  } else
    strcpy(loop2_var_name,ST_name(WN_st(WN_index(loop2))));

  SRCPOS srcpos1=WN_Get_Linenum(loop1);
  SRCPOS srcpos2=WN_Get_Linenum(loop2);

  DO_LOOP_INFO *dli1=Get_Do_Loop_Info(loop1);
  DO_LOOP_INFO *dli2=Get_Do_Loop_Info(loop2);

  if (dli1->No_Fusion || dli2->No_Fusion
      || !Cannot_Concurrentize(loop1) && Cannot_Concurrentize(loop2)
      || Cannot_Concurrentize(loop1) && !Cannot_Concurrentize(loop2)) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Loops with no_fusion pragmas cannot be outer fused.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with no_fusion pragmas cannot be outer fused.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with no_fusion pragmas cannot be outer fused.");
    return Failed;
  }
  if (dli1->LB->Too_Messy || dli1->UB->Too_Messy ||
      dli2->LB->Too_Messy || dli2->UB->Too_Messy) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Loops with messy bounds cannot be outer fused.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with messy bounds cannot be outer fused.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with messy bounds cannot be outer fused.");
    return Failed;
  }
  if (!Do_Loop_Is_Good(loop1) || !Do_Loop_Is_Good(loop2) ||
      Do_Loop_Has_Calls(loop1) || Do_Loop_Has_Calls(loop2) ||
      Do_Loop_Has_Gotos(loop1) || Do_Loop_Has_Gotos(loop2)) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Loops with calls, exits, or gotos cannot be outer fused.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with calls, exits, or gotos cannot be outer fused.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with calls, exits, or gotos cannot be outer fused.");
    return Failed;
  }

  INT * permutation = NULL;

  if (Do_Aggressive_Fuse) {
    // Choose a simply-nested loop as the candidate for permutation.
    WN * permute_loop = Get_Only_Loop_Inside(loop1, FALSE) ? loop1 : 
      (Get_Only_Loop_Inside(loop2, FALSE) ? loop2 : NULL);      

    if (permute_loop) {
      WN * ref_loop = (permute_loop == loop1) ? loop2 : loop1;
      WN * only_loop = Get_Only_Loop_Inside(permute_loop, FALSE);
      int prolog = 0;
      int epilog = 0;
      WN * peel_loop = NULL;
      WN * child_loop;
      int loop_enum = 0;

      // Attempt permutation followed by loop peeling to remove conditional branches. 
      // Limit peeled iterations to 1 to minimize perturbation of spatial locality.
      if (WN_has_compatible_iter_space(loop1, loop2, NULL, NULL, FALSE)
	  && !Get_Only_Loop_Inside(ref_loop, FALSE)
	  && Check_Removable_Branch(permute_loop, &prolog, &epilog, &peel_loop) 
	  && (peel_loop == Get_Only_Loop_Inside(permute_loop, FALSE))
	  && !Has_Loop_Carried_Dependence(peel_loop)
	  && (prolog <= 1)
	  && (epilog <= 1)) {
	child_loop = WN_first(WN_do_body(permute_loop));
	while (child_loop && (WN_operator(child_loop) == WN_operator(permute_loop))) {
	  loop_enum++;
	  if (child_loop == peel_loop) {
	    permutation = CXX_NEW_ARRAY(INT, loop_enum+1, &LNO_local_pool);
	    for (int i = 1; i < loop_enum; i++)
	      permutation[i] = i;
	    permutation[0] = loop_enum;
	    permutation[loop_enum] = 0;
	    if (SNL_Legal_Permutation(permute_loop, child_loop, permutation, loop_enum+1, TRUE)) {
	      permute_loop = SNL_INV_Permute_Loops(permute_loop, permutation, loop_enum+1, FALSE);
	      FISSION_FUSION_STATUS status;
	      if (ref_loop == loop1) {
		loop2 = permute_loop;
		*loop2_p = loop2;
		dli2 = Get_Do_Loop_Info(loop2);
		dli2->Has_Conditional = FALSE;
		status = Retry_and_Peel_Second_SNL;
	      }
	      else {
		loop1 = permute_loop;
		*loop1_p = loop1;
		dli1 = Get_Do_Loop_Info(loop1);
		dli1->Has_Conditional = FALSE;
		status = Retry_and_Peel_First_SNL;
	      }

	      // After permutation, peel loops and remove conditional branches.
	      if (prolog > 0) {
		Pre_loop_peeling(permute_loop, prolog, TRUE, FALSE);
		Remove_Cond_Branch(WN_prev(permute_loop), NULL);
	      }

	      if (epilog > 0) {
		Post_loop_peeling(permute_loop, epilog, TRUE, FALSE);
		Remove_Cond_Branch(WN_next(permute_loop), NULL);
	      }

	      Remove_Cond_Branch(WN_do_body(permute_loop), permute_loop);

	      return status;
	    }
	  }
	  child_loop = WN_first(WN_do_body(child_loop));
	}
      }

      OPCODE ub_compare;
      WN * lower_bound = WN_LOOP_LowerBound(permute_loop);
      WN * upper_bound = WN_LOOP_UpperBound(permute_loop, &ub_compare, TRUE);

      // Attempt permutation so that the outermost loop-nests have compatible iteration spaces
      // for loop fusion.
      if (!WN_has_compatible_iter_space(permute_loop, ref_loop, NULL, NULL, FALSE)
	  && lower_bound && upper_bound 
	  && ((WN_operator(lower_bound) != OPR_INTCONST)
	      || (WN_operator(upper_bound) != OPR_INTCONST))) {
	// Find a child loop in the permute_loop's loop-nest whose iteration space
	// is compatible to the ref_loop.
	child_loop = WN_first(WN_do_body(permute_loop));
	loop_enum = 0;
	while (child_loop && (WN_operator(child_loop) == WN_operator(permute_loop))) {
	  loop_enum++;
	  if (WN_has_compatible_iter_space(child_loop, ref_loop, NULL, NULL, FALSE)) {
	    permutation = CXX_NEW_ARRAY(INT, loop_enum+1, &LNO_local_pool);
	    for (int i = 1; i < loop_enum; i++)
	      permutation[i] = i;
	    permutation[0] = loop_enum;
	    permutation[loop_enum] = 0;
	    if (SNL_Legal_Permutation(permute_loop, child_loop, permutation, loop_enum+1, FALSE)) {
	      WN * wn_p = LWN_Get_Parent(permute_loop);
	      permute_loop = SNL_INV_Permute_Loops(permute_loop, permutation, loop_enum+1, FALSE);

	      if (ref_loop == loop1) {
		loop2 = permute_loop;
		*loop2_p = loop2;
		dli2 = Get_Do_Loop_Info(loop2);
	      }
	      else {
		loop1 = permute_loop;
		*loop1_p = loop1;
		dli1 = Get_Do_Loop_Info(loop1);
	      }
	    }
	    break;
	  }
	  child_loop = WN_first(WN_do_body(child_loop));
	}
      }
    }
  }

  UINT ffi_index=wn2ffi->Find(loop1);
  Is_True((permutation || ffi_index),("Missing SNL info for loop1 in Fuse_Outer_Loops()."));
  if (ffi_index == 0) {
    SNL_INFO snl_info(loop1);
    ffi_index=ffi->New_Snl(snl_info);
    wn2ffi->Enter(loop1,ffi_index);
  }
  UINT snl_level1=ffi->Get_Depth(ffi_index);
  SNL_TYPE type1=ffi->Get_Type(ffi_index);

  ffi_index=wn2ffi->Find(loop2);
  Is_True((permutation || ffi_index),("Missing SNL info for loop2 in Fuse_Outer_Loops()."));
  if (ffi_index == 0) {
    SNL_INFO snl_info(loop2);
    ffi_index=ffi->New_Snl(snl_info);
    wn2ffi->Enter(loop2,ffi_index);
  }
  UINT snl_level2=ffi->Get_Depth(ffi_index);
  SNL_TYPE type2=ffi->Get_Type(ffi_index);

  BOOL parallel1 = (Run_autopar && LNO_Run_AP > 0 &&
                    Any_Loop_In_SNL_Parallelizable(loop1, snl_level1));
  BOOL parallel2 = (Run_autopar && LNO_Run_AP > 0 &&
                    Any_Loop_In_SNL_Parallelizable(loop2, snl_level2));
  if (parallel1 != parallel2 && LNO_Fusion) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Do not fuse parallel with serial loop.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,0,0,
        "Do not fuse parallel with serial loop.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,0,0,
        "Do not fuse parallel with serial loop.");
    return Failed;
  }

  if (LNO_Verbose)
    outer_fusion_verbose_info(srcpos1,srcpos2,
      "Attempt to fuse outer loops to improve locality.");
  if (LNO_Analysis)
    outer_fusion_analysis_info(INFO,srcpos1,srcpos2,snl_level1,snl_level2,
      "Attempt to fuse outer loops to improve locality.");
  if (LNO_Tlog)
    outer_fusion_tlog_info(INFO,srcpos1,srcpos2,snl_level1,snl_level2,
      "Attempt to fuse outer loops to improve locality.");

  mapping_dictionary =
    CXX_NEW(BINARY_TREE<NAME2BIT>(&OLF_default_pool),&OLF_default_pool);
  Bit_Position_Count=0;

  REF_LIST_STACK *writes1 = CXX_NEW(REF_LIST_STACK(&OLF_default_pool),
                                    &OLF_default_pool);
  REF_LIST_STACK *reads1 = CXX_NEW(REF_LIST_STACK(&OLF_default_pool),
                                   &OLF_default_pool);
  SCALAR_STACK *scalar_writes1 = CXX_NEW(SCALAR_STACK(&OLF_default_pool),
                                         &OLF_default_pool);
  SCALAR_STACK *scalar_reads1 = CXX_NEW(SCALAR_STACK(&OLF_default_pool),
                                        &OLF_default_pool);
  SCALAR_REF_STACK *params1 = 
         CXX_NEW(SCALAR_REF_STACK(&OLF_default_pool), &OLF_default_pool);
  DOLOOP_STACK *stack1=CXX_NEW(DOLOOP_STACK(&OLF_default_pool),
                                &OLF_default_pool);
  Build_Doloop_Stack(loop1, stack1);
  Init_Ref_Stmt_Counter();
  UINT array_ref_count1=New_Gather_References(
	    WN_do_body(loop1),writes1,reads1,stack1,
            scalar_writes1,scalar_reads1,params1,&OLF_default_pool);

  if (array_ref_count1 == -1)
    return Failed;
  REF_LIST_STACK *writes2 = CXX_NEW(REF_LIST_STACK(&OLF_default_pool),
                                    &OLF_default_pool);
  REF_LIST_STACK *reads2 = CXX_NEW(REF_LIST_STACK(&OLF_default_pool),
                                   &OLF_default_pool);
  SCALAR_STACK *scalar_writes2 = CXX_NEW(SCALAR_STACK(&OLF_default_pool),
                                         &OLF_default_pool);
  SCALAR_STACK *scalar_reads2 = CXX_NEW(SCALAR_STACK(&OLF_default_pool),
                                        &OLF_default_pool);
  SCALAR_REF_STACK *params2 = 
         CXX_NEW(SCALAR_REF_STACK(&OLF_default_pool), &OLF_default_pool);
  DOLOOP_STACK *stack2=CXX_NEW(DOLOOP_STACK(&OLF_default_pool),
                                &OLF_default_pool);
  Build_Doloop_Stack(loop2, stack2);
  UINT array_ref_count2=New_Gather_References(
	    WN_do_body(loop2),writes2,reads2,stack2,
            scalar_writes2,scalar_reads2,params2,&OLF_default_pool);
  if (array_ref_count2 == -1)
    return Failed;

  // This is a workaround for Fortran program error that is exposed by
  // more aggressive loop fusions.
  if (LNO_Fusion >= 2) {
    if (Has_parm_ref(writes1)) 
      dli1->No_Fusion = 1;
    else if (Has_parm_ref(writes2)) 
      dli2->No_Fusion = 1;
    
    if (dli1->No_Fusion || dli2->No_Fusion) {
      if (LNO_Verbose)
	outer_fusion_verbose_info(srcpos1,srcpos2,
        "Loops with writes to reference parameter cannot be outer fused.");
      if (LNO_Analysis)
	outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with writes to reference paramater cannot be outer fused.");
      if (LNO_Tlog)
	outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,0,0,
        "Loops with writes to reference parameter cannot be outer fused.");
      return Failed;
    }
  }

  BOOL prefer_fuse = FALSE;
  if (Do_Aggressive_Fuse) {
    // If two loops have many array data dependencies, fusion can enable scalarization
    // to reduce memory bandwith.
    if ((dli1->Prefer_Fuse != 1)
	&& (Array_Data_Dependence_Count(writes1, reads2) >= LNO_Fusion_Ddep_Limit)) 
      dli1->Prefer_Fuse = 1;
    prefer_fuse = (dli1->Prefer_Fuse == 1) ? TRUE : FALSE;
  }

  if (!prefer_fuse && snl_level1!=snl_level2 && LNO_Fusion<2) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Unequal SNL levels.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Unequal SNL levels.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Unequal SNL levels.");
    *fusion_level_io=0;
    return Failed;
  }

  if (!prefer_fuse && type1!=type2 && LNO_Fusion<2) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Fusing SNL with non-SNL.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Fusing SNL with non-SNL.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Fusing SNL with non-SNL.");
    *fusion_level_io=0;
    return Failed;
  }

  if (array_ref_count1+array_ref_count2>OLF_size_upperbound && LNO_Fusion<2
      && !prefer_fuse) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Number of array references after merge is too big (>100)!!.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Number of array references after merge is too big (>100)!!.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Number of array references after merge is too big (>100)!!.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=0;
    return Failed;
  }

  INT outer_level1 = dli1->Depth; 
  INT outer_level2 = dli2->Depth; 
  if (!prefer_fuse && Stride_One_Level(writes1,reads1,outer_level1,snl_level1)
      !=Stride_One_Level(writes2,reads2,outer_level2,snl_level2) 
      && LNO_Fusion<2) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Stride-1 level differs.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Stride-1 level differs.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Stride-1 level differs.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=0;
    return Failed;
  }

  BIT_VECTOR* bv1=Array_Names_In_Loop(writes1,reads1);
  BIT_VECTOR* bv2=Array_Names_In_Loop(writes2,reads2);

  if (!bv1 || !bv2) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Too many (>256) array names in loops.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Too many (>256) array names in loops.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Too many (>256) array names in loops.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=0;
    return Failed;
  }

  if (LNO_Fusion<2)
  if (array_ref_count1+array_ref_count2>OLF_size_lowerbound &&
      2*((*bv1) & (*bv2)).Pop_Count() < (~(*bv1) & (*bv2)).Pop_Count() && 
      2*((*bv1) & (*bv2)).Pop_Count() < ((*bv1) & ~(*bv2)).Pop_Count()) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Too few common array names or too many new array names in loop2.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Too few common array names or too many new array names in loop2.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Too few common array names or too many new array names in loop2.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=0;
    return Failed;
  }

  UINT fusion_level=snl_level1;
  if (snl_level2<snl_level1)
    fusion_level=snl_level2;
  UINT fusion_level_tmp=fusion_level;
  UINT peeling_limit=LNO_Fusion_Peeling_Limit;
  if (dli1->Est_Num_Iterations>=0)
    peeling_limit=MIN(peeling_limit,dli1->Est_Num_Iterations);
  if (dli2->Est_Num_Iterations>=0)
    peeling_limit=MIN(peeling_limit,dli2->Est_Num_Iterations);
  if (!prefer_fuse && (snl_level1!=snl_level2 || type1!=Inner || type2!=Inner))
    peeling_limit=0;
  FISSION_FUSION_STATUS level_fusion_status;
  FISSION_FUSION_STATUS status=
    Fuse(loop1,loop2,fusion_level,peeling_limit,TRUE);

  if (status==Succeeded || status==Succeeded_and_Inner_Loop_Removed) {
    if (LNO_Verbose || Get_Trace(TP_WOPT2, PRO_TRANS_TRACE_FLAG)) {
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Successfully fused outer loops.");
      if ((dli1->Get_Id() > 0) || (dli2->Get_Id() > 0))
	printf("     loop num(%d+%d)\n", dli1->Get_Id(), dli2->Get_Id());
    }
    if (LNO_Analysis)
      outer_fusion_analysis_info(SUCCEED,srcpos1,srcpos2,snl_level1,snl_level2,
        "Successfully fused outer loops.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(SUCCEED,srcpos1,srcpos2,snl_level1,snl_level2,
        "Successfully fused outer loops.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    if (status==Succeeded_and_Inner_Loop_Removed)
      fusion_level--;
    *fusion_level_io=fusion_level;
    return status;
  }
  else if ((status == Retry_and_Peel_First_SNL) || (status == Retry_and_Peel_Second_SNL))
    return status;
  else if (((snl_level1>1 && status==Try_Level_By_Level) || LNO_Fusion==2) &&
	     (!prefer_fuse || (status != Failed)) && 
             ((level_fusion_status=Fuse_Level_By_Level(loop1,loop2,
                       &fusion_level_tmp,peeling_limit,
                       LNO_Fusion==2,TRUE,ffi))==Succeeded ||
              (level_fusion_status==Partially_fused && LNO_Fusion==2) ||
              level_fusion_status==Succeeded_and_Inner_Loop_Removed)) {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Successfully fused outer loops.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(SUCCEED,srcpos1,srcpos2,snl_level1,snl_level2,
        "Successfully fused outer loops.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(SUCCEED,srcpos1,srcpos2,snl_level1,snl_level2,
        "Successfully fused outer loops.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=fusion_level_tmp;
    return level_fusion_status;
  } else if (status==Partially_fused || 
             (status==Try_Level_By_Level &&
              level_fusion_status==Partially_fused)) {
    DevWarn("Partially fused loop is not restored");
    *fusion_level_io=fusion_level_tmp;
    return Partially_fused;
  } else {
    if (LNO_Verbose)
      outer_fusion_verbose_info(srcpos1,srcpos2,
        "Failed to fuse outer loops.");
    if (LNO_Analysis)
      outer_fusion_analysis_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Failed to fuse outer loops.");
    if (LNO_Tlog)
      outer_fusion_tlog_info(FAIL,srcpos1,srcpos2,snl_level1,snl_level2,
        "Failed to fuse outer loops.");
    CXX_DELETE(mapping_dictionary, &OLF_default_pool);
    *fusion_level_io=0;
    return Failed;
  }
}

#ifdef KEY
// Bug 1025
static BOOL Same_Loop_Body ( WN* wn, WN* copy, 
			     SYMBOL loop_index, SYMBOL copy_loop_index )
{
  if (!wn || !copy)
    return FALSE;

  OPERATOR wn_opr = WN_operator(wn);
  OPERATOR copy_opr = WN_operator(copy);

  if (wn_opr != wn_opr || WN_kid_count(wn) != WN_kid_count(copy)) 
    return FALSE;

  if (wn_opr == OPR_LDID || wn_opr == OPR_STID) {
    SYMBOL wn_sym(wn);
    SYMBOL copy_sym(copy);
    if (wn_sym != copy_sym && 
	wn_sym != loop_index && copy_sym != copy_loop_index)
      return FALSE;
  }
  else if (wn_opr == OPR_BLOCK) {
    WN* stmt_wn;
    WN* stmt_copy;
    for (stmt_wn=WN_first(wn), stmt_copy = WN_first(copy); 
	 stmt_wn && stmt_copy; 
	 stmt_wn= WN_next(stmt_wn), stmt_copy = WN_next(stmt_copy))
      if (!Same_Loop_Body(stmt_wn, stmt_copy, loop_index, copy_loop_index))
	return FALSE;
    if (stmt_wn == NULL && stmt_copy != NULL ||
	stmt_wn != NULL && stmt_copy == NULL)
      return FALSE;
  } 
  else if (wn_opr == OPR_DO_LOOP) {
    return Same_Loop_Body(WN_do_body(wn), WN_do_body(copy), 
			  loop_index, copy_loop_index);
  } 

  for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
    if (!Same_Loop_Body(WN_kid(wn, kid), WN_kid(copy, kid), 
			loop_index, copy_loop_index))
      return FALSE;
  
  return TRUE;
}
#endif

// Fuse peeled iterations in 'wn'.
static void Peel_Loop_Fusion_Walk(WN * wn, FIZ_FUSE_INFO* ffi, WN2UINT *wn2ffi) 
{
  if (WN_operator(wn) == OPR_IF) {
    wn = WN_then(wn);
    for (wn = WN_first(wn); wn; wn = WN_next(wn)) {
      if (WN_operator(wn) == OPR_DO_LOOP) {
	DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn);
	dli->Prefer_Fuse = 1;
	Pass_Child_Prefer_Fuse(wn);
	Outer_Loop_Fusion_Walk(wn, ffi, wn2ffi);
	break;
      }
    }
  }
}

// Fuse child loops nested in 'wn'.
static void Child_Loop_Fusion_Walk(WN * wn, FIZ_FUSE_INFO* ffi, WN2UINT *wn2ffi) 
{
  WN * wn_child = WN_first(WN_do_body(wn));
  if (wn_child && (WN_operator(wn_child) == OPR_DO_LOOP)) {
    DO_LOOP_INFO * dli_child = Get_Do_Loop_Info(wn_child);
    if (dli_child && (dli_child->Prefer_Fuse == 1)) {
      // Find next loop and move it adjacent to the 'wn_child'.
      WN * wn_next = WN_next(wn_child);
      while (wn_next && (WN_operator(wn_next) != OPR_DO_LOOP))
	wn_next = WN_next(wn_next);

      if (wn_next && (WN_operator(wn_next) == OPR_DO_LOOP)) {
	if (WN_next(wn_child) != wn_next) 
	  Move_Adjacent(wn_child,wn_next, FALSE);
	// Examine loops following 'wn_next', check whether to delay 
	// the fusion of 'wn_child' and 'wn_next'.  Delay is prefered
	// if 'wn_next' and its following loops have the same bound
	// but 'wn_next' and 'wn_child' do not have the same bound.
	if ((WN_next(wn_child) == wn_next) && !Same_Bounds(wn_next, wn_child)) {
	  WN * wn_iter = WN_next(wn_next);
	  int count = 0;
	  BOOL delay_fuse = TRUE;
	  while (wn_iter && (WN_operator(wn_iter) == OPR_DO_LOOP)) {
	    if (!Same_Bounds(wn_iter, wn_next)) {
	      delay_fuse = FALSE;
	      break;
	    }
	    else
	      count++;
	    wn_iter = WN_next(wn_iter);
	  }
	  if (delay_fuse && (count > 0)) {
	    // Try to fuse loops following 'wn_next'.
	    if (dli_child->Has_Precom_Def == 1) {
	      // Find the last loop containing precomputation uses.
	      wn_iter = wn_next;
	      WN * last_use = NULL;
	      int use_cnt = 0;
	      while (wn_iter && (WN_operator(wn_iter) == OPR_DO_LOOP)) {
		DO_LOOP_INFO * dli_iter = Get_Do_Loop_Info(wn_iter);
		if (dli_iter->Has_Precom_Use == 1) {
		  last_use = wn_iter;
		  use_cnt++;
		}
		wn_iter = WN_next(wn_iter);
	      }

	      if (use_cnt == 1) {
		int dim = Loop_Depth(wn_child);
		// Find the first loop in (wn_child, last_use) that potentially has
		// loop-carried dependencies that prevents loop fusion.
		wn_iter = WN_next(wn_child);
		WN * wn_dep = NULL;
		while (wn_iter && (WN_operator(wn_iter) == OPR_DO_LOOP)
		       && (wn_iter != last_use)) {
		  if (!Has_Unit_Refs(wn_iter, dim)) {
		    wn_dep = wn_iter;
		    break;
		  }
		  wn_iter = WN_next(wn_iter);
		}

		if (wn_dep) {
		  // Move loops in [wn_dep, last_use) below last_use and flag
		  // no fusion bit for 'wn_dep' .
		  if (Move_Adjacent(WN_prev(wn_dep), last_use, TRUE)) {
		    // Fuse loops starting from wn_dep, and flag No_Fusion bit.
		    Outer_Loop_Fusion_Walk(wn_dep, ffi, wn2ffi); 
		    if (WN_operator(wn_dep) == OPR_DO_LOOP) {
		      DO_LOOP_INFO * dli_dep = Get_Do_Loop_Info(wn_dep);
		      dli_dep->No_Fusion = TRUE;
		    }
		    else {
		      DevWarn("Loop may be deleted");
		    }
		  }
		}
	      }
	    }
	    wn_next = WN_next(wn_child);
	    DO_LOOP_INFO * dli_next = Get_Do_Loop_Info(wn_next);
	    if (!dli_next->No_Fusion) {
	      dli_next->Prefer_Fuse = 1;
	      Outer_Loop_Fusion_Walk(wn_next, ffi, wn2ffi); 
	    }
	  }
	}
      }
      Outer_Loop_Fusion_Walk(wn_child, ffi, wn2ffi); 
    }
  }
}

OUTER_FUSION_STATUS Outer_Loop_Fusion_Walk(WN* wn,
       FIZ_FUSE_INFO* ffi, WN2UINT *wn2ffi) {
  OPCODE opc=WN_opcode(wn);

  if (!OPCODE_is_scf(opc)) 
    return NORMAL;
  else if (opc==OPC_DO_LOOP) {
    if (Do_Loop_Is_Good(wn) && !Do_Loop_Has_Calls(wn) && 
	!Do_Loop_Has_Gotos(wn)) {
      WN* next_wn=WN_next(wn);

      // hacked to remove pragmas produced by inlining
      while (next_wn && 
             ((WN_operator(next_wn) == OPR_PRAGMA &&
               (WN_pragma(next_wn) == WN_PRAGMA_INLINE_BODY_START ||
                WN_pragma(next_wn) == WN_PRAGMA_INLINE_BODY_END ||
                WN_pragma(next_wn) == WN_PRAGMA_CLIST_SKIP_BEGIN ||
                WN_pragma(next_wn) == WN_PRAGMA_FLIST_SKIP_BEGIN ||
                WN_pragma(next_wn) == WN_PRAGMA_CLIST_SKIP_END ||
                WN_pragma(next_wn) == WN_PRAGMA_FLIST_SKIP_END))
              ||
              (WN_operator(next_wn) == OPR_XPRAGMA &&
               WN_pragma(next_wn) == WN_PRAGMA_COPYIN_BOUND))) {
        LWN_Delete_Tree_From_Block(next_wn);
        next_wn=WN_next(wn);
      } 

      OUTER_FUSION_STATUS state=SKIP;
      while (next_wn && WN_opcode(next_wn)==OPC_DO_LOOP &&
          Do_Loop_Is_Good(next_wn) && !Do_Loop_Has_Calls(next_wn) &&
				      !Do_Loop_Has_Gotos(next_wn)) {
        INT wn_index=wn2ffi->Find(wn);
        if (wn_index == 0) {
          //DevWarn("Missing SNL info for loop1 in Outer_Loop_Fusion_Walk().");
          SNL_INFO snl_info(wn);
          wn_index=ffi->New_Snl(snl_info);
          wn2ffi->Enter(wn,wn_index);
        }
        INT next_wn_index=wn2ffi->Find(next_wn);
        if (next_wn_index == 0) {
          //DevWarn("Missing SNL info for loop2 in Outer_Loop_Fusion_Walk().");
          SNL_INFO snl_info(next_wn);
          next_wn_index=ffi->New_Snl(snl_info);
          wn2ffi->Enter(next_wn,next_wn_index);
        }
        INT d1=ffi->Get_Depth(wn_index);
        INT d2=ffi->Get_Depth(next_wn_index);
        UINT fused_level;
#ifdef KEY
	// Bug 1025
	WN* next_next_copy;
	if (LNO_Fusion == 2)
	  next_next_copy = 
	    LWN_Copy_Tree(WN_next(next_wn), TRUE, LNO_Info_Map);	
#endif

	WN * wn_prev1 = WN_prev(wn);
	WN * wn_prev2 = WN_prev(next_wn);
	WN * wn_next2 = WN_next(next_wn);

        FISSION_FUSION_STATUS fusion_status=
	  Fuse_Outer_Loops(&wn,&next_wn,ffi,wn2ffi,&fused_level);
	wn_index = wn2ffi->Find(wn);	  

        if (fusion_status==Succeeded || fusion_status==Partially_fused) {
          WN* wn1=wn;
          WN* wn2=wn;
          INT level=1;
          while (wn1=Get_Only_Loop_Inside(wn1,FALSE)) {
            level++;
            wn2=wn1;
          }
          if (d1>level) {
            INT i=ffi->Copy_Snl(ffi,wn_index);
            wn1=WN_first(WN_do_body(wn2));
            while (WN_opcode(wn1)!=OPC_DO_LOOP) wn1=WN_next(wn1);
            ffi->Set_Depth(i,d1-level);
            ffi->Set_Wn(i,wn1);
            wn2ffi->Enter(wn1,i);
          }
          if (d2>level) {
            INT j=ffi->Copy_Snl(ffi,next_wn_index);
            wn1=WN_last(WN_do_body(wn2));
            while (WN_opcode(wn1)!=OPC_DO_LOOP) wn1=WN_prev(wn1);
            ffi->Set_Depth(j,d2-level);
            ffi->Set_Wn(j,wn1);
            wn2ffi->Enter(wn1,j);
          }
          if (ffi->Get_Type(wn_index)==Inner &&
              ffi->Get_Type(next_wn_index)==Inner &&
              fusion_status==Succeeded)
            ffi->Set_Type(wn_index,Inner);
          else
            ffi->Set_Type(wn_index,Not_Inner);
          ffi->Set_Depth(wn_index,level);
          ffi->Set_Type(next_wn_index,Invalid);
          wn2ffi->Enter(next_wn, 0);

	  if (Do_Aggressive_Fuse && Enclosing_Loop(wn)) {
	    // Fuse peeled iterations for inner loops.
	    DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn);
	    if (dli && (dli->Prefer_Fuse == 1)) {
	      WN * wn_iter = WN_next(wn_prev2);
	      WN * next_iter;
	      while (wn_iter && (wn_iter != wn_next2)) {
		next_iter = WN_next(wn_iter);
		Peel_Loop_Fusion_Walk(wn_iter, ffi, wn2ffi);
		wn_iter = next_iter;
	      }
	    }
	  }
        } else if (fusion_status==Succeeded_and_Inner_Loop_Removed) {
          ffi->Set_Type(wn2ffi->Find(next_wn),Invalid);
          wn2ffi->Enter(next_wn, 0);
          if (fused_level>0)
            ffi->Set_Depth(wn2ffi->Find(wn),fused_level);
          else {
            ffi->Set_Type(wn2ffi->Find(wn),Invalid);
            wn2ffi->Enter(wn, 0);
            return state; // the original loop has been removed
          }
        }
	else {
	  WN * wn_iter;
	  WN * next_iter;
	  if (fusion_status == Retry_and_Peel_First_SNL) {
	    // Fuse child loops in 'wn'.
	    WN * child = Get_Only_Loop_Inside(wn, FALSE);
	    FmtAssert(child, ("Expect a child loop."));
	    Pass_Child_Prefer_Fuse(child);
	    Child_Loop_Fusion_Walk(child, ffi, wn2ffi);

	    wn_iter = wn_prev1 ? WN_next(wn_prev1) : NULL;
	    // Fuse pre-peeled iterations.
	    while (wn_iter && (wn_iter != wn)) {
	      next_iter = WN_next(wn_iter);
	      Peel_Loop_Fusion_Walk(wn_iter, ffi, wn2ffi);
	      wn_iter = next_iter;
	    }

	    WN * parent = LWN_Get_Parent(wn);	      
	    wn_iter = WN_next(wn);
	    // Fuse post-peeled iterations and hoist up post-peeled iterations.
	    while (wn_iter && (wn_iter != next_wn)) {
	      next_iter = WN_next(wn_iter);
	      Peel_Loop_Fusion_Walk(wn_iter, ffi, wn2ffi);
	      LWN_Insert_Block_Before(parent, wn, LWN_Extract_From_Block(wn_iter));
	      wn_iter = next_iter;
	    }
	      
	    next_wn = WN_next(wn);
	    continue;
	  }
	  else if (fusion_status == Retry_and_Peel_Second_SNL) {
	    wn_iter = WN_prev(next_wn);
	    WN * parent = LWN_Get_Parent(next_wn);
	    // Fuse pre-peeled iterations and push down pre-peeled iterations.
	    while (wn_iter && (wn_iter != wn_prev2)) {
	      next_iter = WN_prev(wn_iter);
	      Peel_Loop_Fusion_Walk(wn_iter, ffi, wn2ffi);
	      LWN_Insert_Block_After(parent, next_wn, LWN_Extract_From_Block(wn_iter));
	      wn_iter = next_iter;
	    }
	    
	    // Fuse post-peeled iterations.
	    wn_iter = WN_next(next_wn);
	    while (wn_iter && (wn_iter != wn_next2)) {
	      next_iter = WN_next(wn_iter);
	      Peel_Loop_Fusion_Walk(wn_iter, ffi, wn2ffi);
	      wn_iter = next_iter;
	    }
	    
	    next_wn = WN_next(wn);
	    continue;
	  }
	  
          WN* new_next_wn=WN_next(wn);
          if (next_wn!=new_next_wn) {
            ffi->Set_Wn(wn2ffi->Find(next_wn),new_next_wn);
            wn2ffi->Enter(new_next_wn,wn2ffi->Find(next_wn));
            wn2ffi->Enter(next_wn,0);
          }

	  if (Do_Aggressive_Fuse) 
	    Child_Loop_Fusion_Walk(wn, ffi, wn2ffi);
	  
          return state;
        }
        next_wn=WN_next(wn);
        state=NORMAL;
#ifdef KEY
	// Bug 1025
	if (LNO_Fusion == 2 && next_wn &&
	    WN_operator(next_wn) == OPR_DO_LOOP &&
	    (!next_next_copy || WN_operator(next_next_copy) != OPR_DO_LOOP ||
	    // Make sure the new next_wn is not a remnant (post-peel) from the 
	    // last fusion. If so, then we have already tried fusing these two
	    // adjacent loops and we had a remainder loop and there is no 
	    // point trying to fuse them again.
	     (!Same_Loop_Body(next_wn, next_next_copy, 
			      WN_index(next_wn), WN_index(next_next_copy))))) {
	  if (Do_Aggressive_Fuse)
	    Child_Loop_Fusion_Walk(wn, ffi, wn2ffi);

	  return SKIP;
	}
#endif

        // hacked to remove pragmas produced by inlining
        while (next_wn && 
               ((WN_operator(next_wn) == OPR_PRAGMA &&
                 (WN_pragma(next_wn) == WN_PRAGMA_INLINE_BODY_START ||
                  WN_pragma(next_wn) == WN_PRAGMA_INLINE_BODY_END ||
                  WN_pragma(next_wn) == WN_PRAGMA_CLIST_SKIP_BEGIN ||
                  WN_pragma(next_wn) == WN_PRAGMA_FLIST_SKIP_BEGIN ||
                  WN_pragma(next_wn) == WN_PRAGMA_CLIST_SKIP_END ||
                  WN_pragma(next_wn) == WN_PRAGMA_FLIST_SKIP_END))
                ||
                (WN_operator(next_wn) == OPR_XPRAGMA &&
                 WN_pragma(next_wn) == WN_PRAGMA_COPYIN_BOUND))) {
          LWN_Delete_Tree_From_Block(next_wn);
          next_wn=WN_next(wn);
        }
      }

      if (Do_Aggressive_Fuse) 
	Child_Loop_Fusion_Walk(wn, ffi, wn2ffi);
    } else
      (void)Outer_Loop_Fusion_Walk(WN_do_body(wn),ffi,wn2ffi);
  } else if (opc==OPC_BLOCK) {
    for (WN* stmt=WN_first(wn); stmt; ) {
      WN* prev_stmt=WN_prev(stmt);
      WN* next_stmt=WN_next(stmt);
      OUTER_FUSION_STATUS status=Outer_Loop_Fusion_Walk(stmt,ffi,wn2ffi);
      if (!prev_stmt)
        if (WN_first(wn)!=stmt && status==NORMAL)
          stmt=WN_first(wn);	// a new stmt (loop) is created and
				// should retry
        else if (status==SKIP)		// status is SKIP so no more retry
          stmt=WN_next(WN_first(wn));
        else 			// if (WN_first(wn)==stmt)
          if (WN_next(stmt)==next_stmt)	// no new next stmt (loop) is created
            stmt=next_stmt;
          else;		// a new next stmt (loop) is created and should retry
      else
        if (WN_next(prev_stmt)!=stmt && status==NORMAL)
          stmt=prev_stmt;	// a new stmt (loop) is created and
				// should retry
        // e.g. a new peeled loop has been created before the current stmt
        // we will re-start from the prev_stmt
        else if (status==SKIP)		// status is SKIP so no more retry
          stmt=WN_next(WN_next(prev_stmt));
        else 			// if (WN_next(prev_stmt)==stmt)
          if (WN_next(stmt)==next_stmt)	// no new next stmt (loop) is created
            stmt=next_stmt;
        else;		// a new next stmt (loop) is created and should retry
    }
  } else
    for (UINT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      (void)Outer_Loop_Fusion_Walk(WN_kid(wn,kidno),ffi,wn2ffi);
    }

  return NORMAL;
}

void Outer_Loop_Fusion_Phase(WN* func_nd, FIZ_FUSE_INFO* ffi) {
  
  MEM_POOL_Initialize(&OLF_default_pool,"OLF_default_pool",FALSE);
  MEM_POOL_Push(&OLF_default_pool);

  WN2UINT* wn2ffi=CXX_NEW(WN2UINT(256,&OLF_default_pool),&OLF_default_pool);

  ffi->Copy_Snl(ffi,0);
  ffi->Set_Type(0,Invalid);
  // moved the record of 0th SNL to the end to avoid using 0 as an id
  // in hash table where 0 has special meaning (NULL)

  for (INT i=1; i<ffi->Num_Snl(); i++) {
    WN* loop=ffi->Get_Wn(i);
    wn2ffi->Enter(loop, i);
  }

  Outer_Loop_Fusion_Walk(func_nd,ffi,wn2ffi);

  CXX_DELETE(wn2ffi, &OLF_default_pool);

  MEM_POOL_Pop(&OLF_default_pool);
  MEM_POOL_Delete(&OLF_default_pool);

}


