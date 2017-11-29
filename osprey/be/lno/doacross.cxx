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


// -*-C++-*-
//                  	LNO DOACROSS Parallelization
//                  	----------------------------
//

/* ====================================================================
 * ====================================================================
 *
 * Module: doacross.cxx
 * $Revision: 1.7 $
 * $Date: 05/06/20 21:34:04-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: be/lno/SCCS/s.doacross.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Parallelize loops with loop-carried dependences
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
static char *rcs_id = "$Source: be/lno/SCCS/s.doacross.cxx $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>
#ifdef KEY // to get DBL_MAX
#include <float.h>
#endif
#include "pu_info.h"
#include "defs.h"
#include "glob.h"
#include "wn.h"
#include "wn_map.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "lego_util.h"
#include "tile.h"
#include "mat.h"
#include "snl.h"
#include "snl_trans.h"
#include "region_util.h"
#include "opt_du.h"
#include "mtypes.h"
#include "config_targ.h"
#include "config_cache.h"
#include "scalar_expand.h"
#include "strtab.h"
#include "small_trips.h"
#include "irbdata.h"
#include "data_layout.h"
#include "ff_utils.h"
#include "doacross.h"
#include "dvector.h"
#include "ara_loop.h"
#include "wn_pragmas.h"

// in parallel.cxx
extern BOOL Is_Privatizable_With_Context(WN* loop, WN* wn, BOOL defnitely);

typedef STACK<WN*> STACK_OF_WN;

static ST* Sync_Array_St;
static ST* Sync_Length_St;
static ST* Sync_Offset_St;
static ST* Max_Numthread_St;
static WN* Sync_Array_Alias_Host;
static WN* Sync_Length_Alias_Host;
static WN* Sync_Offset_Alias_Host;
static TY_IDX  Sync_Array_Ptr_Ty;
static BOOL Sync_Structure_Created;
static BOOL Doacross_Inited;
static STACK_OF_WN* sync_offset_stid_stack;
static STACK_OF_WN* sync_offset_ldid_stack;
static STACK_OF_WN* sync_length_stid_stack;
static STACK_OF_WN* sync_length_ldid_stack;
// single sync cycle == one remote read miss for a dirty line
static double Single_Sync_Cycle = 200.0;
static WN* Func_Root=NULL;
static MEM_POOL DOACROSS_default_pool;

#define MAX_INT64 0x7fffffffffffffff
#define MIN_INT64 0x8000000000000000

// forward declaration
static void Create_Sync_Structure ();
static WN* Create_Initialize_Loop (WN* processor_loop,
				ARRAY_DIRECTED_GRAPH16* dg,
				DU_MANAGER* du,
				BOOL Is_Pdo_Region);

void Update_Sync_Offset_Stid_DU(WN* sync_offset_stid) {
  for (INT i=0; i<sync_offset_ldid_stack->Elements(); i++)
    Du_Mgr->Add_Def_Use(sync_offset_stid,sync_offset_ldid_stack->Top_nth(i));
  sync_offset_stid_stack->Push(sync_offset_stid);
}

void Update_Sync_Offset_Ldid_DU(WN* sync_offset_ldid) {
  for (INT i=0; i<sync_offset_stid_stack->Elements(); i++)
    Du_Mgr->Add_Def_Use(sync_offset_stid_stack->Top_nth(i),sync_offset_ldid);
  sync_offset_ldid_stack->Push(sync_offset_ldid);
}

void Update_Sync_Length_Stid_DU(WN* sync_length_stid) {
  for (INT i=0; i<sync_length_ldid_stack->Elements(); i++)
    Du_Mgr->Add_Def_Use(sync_length_stid,sync_length_ldid_stack->Top_nth(i));
  sync_length_stid_stack->Push(sync_length_stid);
}

void Update_Sync_Length_Ldid_DU(WN* sync_length_ldid) {
  for (INT i=0; i<sync_length_stid_stack->Elements(); i++)
    Du_Mgr->Add_Def_Use(sync_length_stid_stack->Top_nth(i),sync_length_ldid);
  sync_length_ldid_stack->Push(sync_length_ldid);
}


//-----------------------------------------------------------------------
// NAME: Parallelize_Doacross_Loop
// FUNCTION:
//	Given a loop with the associated dependence info, perform
//	the doacross transformation and insert necessary synchronizations.
//	Update the dependences after transformation.
// ARGUMENTS:
//	'loop' -- the loop to be parallelized
//	'sync_distances' -- synchronization vectors
//	'dg' -- array dependence graph
//	'du' -- scalar dependences (du-chains)
//	'rm' -- reduction information
//-----------------------------------------------------------------------

extern WN* Parallelize_Doacross_Loop(
			WN* processor_loop,
			WN* processor_tile_loop,
			INT32 Doacross_Tile_Size,
			INT32 sync_distances[2],
			ARRAY_DIRECTED_GRAPH16* dg,
			DU_MANAGER* du)
{

  Doacross_Init(NULL);	// a minor init

  Create_Sync_Structure ();	// create entries in symbol table
				// for sync array and variables

  // compute number of sync array elements per cache line
  TYPE_ID index_type=MTYPE_I4;
  TYPE_ID sync_array_type=MTYPE_I8;
  INT cache_line_size=128;
  if (Mhd.L[1].Valid())
    cache_line_size=Mhd.L[1].Line_Size;
  INT element_per_cache_line=cache_line_size/MTYPE_byte_size(sync_array_type);

  // compute sync distances in number of tiles
  INT sync_tile_distances[2];
  sync_tile_distances[0]= sync_distances[0]/Doacross_Tile_Size;
  if (sync_distances[0]==NULL_DIST)
    sync_tile_distances[0]=NULL_DIST;
  sync_tile_distances[1]= sync_distances[1]/Doacross_Tile_Size;
  if (sync_distances[1]==NULL_DIST)
    sync_tile_distances[1]=NULL_DIST;

  WN* doacross_region=Get_MP_Region(processor_loop);
  BOOL Is_Pdo_Region=Get_Do_Loop_Info(processor_loop)->Mp_Info->Is_Pdo();
  if (Is_Pdo_Region) {
    doacross_region=LWN_Get_Parent(LWN_Get_Parent(doacross_region));
  }

  // remove the SYNC_DOACROSS pragma which is not needed after -mplist
  WN* pragmas=WN_region_pragmas(doacross_region);
  WN* next_wn=WN_first(pragmas);
  while (next_wn) {
    if (WN_opcode(next_wn)==OPC_PRAGMA)
      if ((WN_PRAGMA_ID)WN_pragma(next_wn)==WN_PRAGMA_SYNC_DOACROSS) {
	LWN_Delete_From_Block(LWN_Get_Parent(next_wn),next_wn);
	break;
      }
    next_wn=WN_next(next_wn);
  }

  MEM_POOL_Push(&LNO_local_pool);

  // create doacross tiles (if tile size >1) and move the tiling loop outward
  WN* loop1=WN_first(WN_do_body(processor_tile_loop));
  INT permutation[2]; permutation[0]=1; permutation[1]=0;
  SNL_Permute_Loops(processor_tile_loop, loop1, permutation, 2, TRUE, FALSE);
  WN* outer_doacross= loop1;
  if (Doacross_Tile_Size!= NULL_DIST && Doacross_Tile_Size>1) {
    outer_doacross= Tile_Loop(loop1, Doacross_Tile_Size, 0,
		     SNL_INV_DOACROSS_TILE,
                     NULL, &LNO_default_pool);
  } 

  // initialize needed portion of sync arrays
  WN* init_loop=Create_Initialize_Loop (processor_loop,dg,du,Is_Pdo_Region);

  OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, index_type);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_gt  = OPCODE_make_op(OPR_GT, Boolean_type, index_type);
  OPCODE op_lt  = OPCODE_make_op(OPR_LT, Boolean_type, index_type);

  OPCODE op_long_stid = OPCODE_make_op(OPR_STID, MTYPE_V, sync_array_type);
  OPCODE op_long_ldid = OPCODE_make_op(OPR_LDID, sync_array_type,
					         sync_array_type);
  OPCODE op_long_add = OPCODE_make_op(OPR_ADD, sync_array_type, MTYPE_V);
  OPCODE op_long_sub = OPCODE_make_op(OPR_SUB, sync_array_type, MTYPE_V);
  OPCODE op_long_mpy = OPCODE_make_op(OPR_MPY, sync_array_type, MTYPE_V);
  OPCODE op_long_gt  = OPCODE_make_op(OPR_GT, Boolean_type, sync_array_type);
  ST* index_type_preg_st = MTYPE_To_PREG(index_type);
  ST* sync_array_type_preg_st = MTYPE_To_PREG(sync_array_type);

  BOOL Reversed=FALSE;
  DO_LOOP_INFO* dli=Get_Do_Loop_Info(processor_tile_loop);
  if (dli->Is_Backward && dli->Auto_Parallelized) {
    Reversed=TRUE;
  }

  // create my_pid=pid
#ifdef _NEW_SYMTAB
  WN_OFFSET preg_num = Create_Preg(index_type, "my_pid");
#else
  WN_OFFSET preg_num = Create_Preg(index_type, "my_pid", NULL);
#endif
  WN* pid_ldid=LWN_CreateLdid(op_ldid, WN_start(processor_loop));
  du->Add_Def_Use(WN_start(processor_loop), pid_ldid);
  du->Add_Def_Use(WN_step(processor_loop), pid_ldid);
  du->Ud_Get_Def(pid_ldid)->Set_loop_stmt(processor_loop);
  WN* my_pid_stid = LWN_CreateStid(op_stid, preg_num,
                      index_type_preg_st, Be_Type_Tbl(index_type), pid_ldid);
  LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, my_pid_stid);

  // create my_step=1
#ifdef _NEW_SYMTAB
  preg_num = Create_Preg(sync_array_type, "my_step");
#else
  preg_num = Create_Preg(sync_array_type, "my_step", NULL);
#endif
  WN* my_step_stid = LWN_CreateStid(op_long_stid, preg_num,
	sync_array_type_preg_st, Be_Type_Tbl(sync_array_type),
	LWN_Make_Icon(sync_array_type, 1));
  LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, my_step_stid);

  // create my_step++
  WN* my_step_ldid = LWN_CreateLdid(op_long_ldid, my_step_stid);
  WN* my_step_inc = LWN_CreateExp2(
			op_long_add,
			my_step_ldid,
			LWN_Make_Icon(sync_array_type, 1));
  WN* my_step_inc_stid =
	LWN_CreateStid(op_long_stid, my_step_stid, my_step_inc);
  LWN_Insert_Block_Before(WN_do_body(outer_doacross), NULL, my_step_inc_stid);
  du->Add_Def_Use(my_step_inc_stid, my_step_ldid);
  du->Add_Def_Use(my_step_stid, my_step_ldid);

  // create sync_array(my_pid*epcl+offset)=my_step
  // where epcl is elements per cache line
  WN* my_step_ldid1 = LWN_Copy_Tree(my_step_ldid);
  LWN_Copy_Def_Use(my_step_ldid,my_step_ldid1,du);
  WN* sync_istore=LWN_Copy_Tree(WN_first(WN_do_body(init_loop)));
  LWN_Delete_Tree(WN_array_index(WN_kid1(sync_istore),0));
  WN* index=LWN_CreateLdid(op_ldid,my_pid_stid);
  du->Add_Def_Use(my_pid_stid,index);
  du->Ud_Get_Def(index)->Set_loop_stmt(NULL);
  WN* sync_offset_stid=WN_next(init_loop);
  WN* sync_offset_ldid=LWN_CreateLdid(op_ldid,sync_offset_stid);
  Update_Sync_Offset_Ldid_DU(sync_offset_ldid);
  WN_array_index(WN_kid1(sync_istore),0)=
      LWN_CreateExp2(
		op_add,
      		LWN_CreateExp2(op_mpy,
		     	       index,
		       	       LWN_Make_Icon(index_type,
					     element_per_cache_line)),
		sync_offset_ldid);
  WN_kid0(sync_istore)= my_step_ldid1;
  LWN_Parentize(sync_istore);

  // finish all ops before posting
  WN* fb = WN_CreateBarrier(TRUE, 0);	// forward
  WN* hw_sync=WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
		INTRN_SYNCHRONIZE,0,NULL);
  WN* bb = WN_CreateBarrier(FALSE, 0);	// backward
  LWN_Insert_Block_Before(WN_do_body(outer_doacross),my_step_inc_stid,fb);
  LWN_Insert_Block_Before(WN_do_body(outer_doacross),my_step_inc_stid,hw_sync);
  LWN_Insert_Block_Before(WN_do_body(outer_doacross),my_step_inc_stid,bb);
  LWN_Insert_Block_Before(
	WN_do_body(outer_doacross), my_step_inc_stid, sync_istore);
  WN *loop = Enclosing_Do_Loop(fb);
  if (loop && Do_Loop_Is_Good(loop)) {
    VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
    v = Array_Dependence_Graph->Add_Vertex(bb);
    if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
  }


  DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);
  Build_Doloop_Stack(LWN_Get_Parent(sync_istore), loop_stack);
  LNO_Build_Access(sync_istore, loop_stack, &LNO_default_pool);

  // initialize my sync array element for next sync_doacross
  WN* sync_init_istore=LWN_Copy_Tree(WN_first(WN_do_body(init_loop)));
  LWN_Delete_Tree(WN_array_index(WN_kid1(sync_init_istore),0));
  index=LWN_CreateLdid(op_ldid,my_pid_stid);
  du->Add_Def_Use(my_pid_stid,index);
  du->Ud_Get_Def(index)->Set_loop_stmt(NULL);
  sync_offset_ldid=LWN_CreateLdid(op_ldid,sync_offset_stid);
  Update_Sync_Offset_Ldid_DU(sync_offset_ldid);
  WN_array_index(WN_kid1(sync_init_istore),0)=
	LWN_CreateExp2(
		op_add,
		LWN_CreateExp2(op_mpy,
		       	       index,
		       	       LWN_Make_Icon(index_type,
					     element_per_cache_line)),
		LWN_Make_Icon(index_type, 1));
  WN_array_index(WN_kid1(sync_init_istore),0)=
	LWN_CreateExp2(op_sub,
		       WN_array_index(WN_kid1(sync_init_istore),0),
		       sync_offset_ldid);
  LWN_Parentize(sync_init_istore);
  fb = WN_CreateBarrier(TRUE, 0);	// forward
  hw_sync=WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
		INTRN_SYNCHRONIZE,0,NULL);
  bb = WN_CreateBarrier(FALSE, 0);	// backward
  LWN_Insert_Block_After(
	WN_do_body(processor_loop), outer_doacross, sync_init_istore);
  LWN_Insert_Block_After(WN_do_body(processor_loop),outer_doacross,bb);
  LWN_Insert_Block_After(WN_do_body(processor_loop),outer_doacross,hw_sync);
  LWN_Insert_Block_After(WN_do_body(processor_loop),outer_doacross,fb);
  loop = Enclosing_Do_Loop(fb);
  if (loop && Do_Loop_Is_Good(loop)) {
    VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
    v = Array_Dependence_Graph->Add_Vertex(bb);
    if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
  }

  Build_Doloop_Stack(LWN_Get_Parent(sync_init_istore), loop_stack);
  LNO_Build_Access(sync_init_istore, loop_stack, &LNO_default_pool);

  if (sync_tile_distances[0]!=NULL_DIST || sync_tile_distances[1]!=NULL_DIST) {
    // if synchronization is needed

    WN* left;
    WN* right;
    WN* upper_guard=NULL;
    WN* upper_sync_ldid=NULL;
    WN* step_upper_update_stid=NULL;
    WN* lower_guard=NULL;
    WN* lower_sync_ldid=NULL;
    WN* step_lower_update_stid=NULL;
    if (sync_tile_distances[1]!= NULL_DIST) {	// dependence from upper stripe

      // create upper_pid=pid-1
#ifdef _NEW_SYMTAB
      preg_num = Create_Preg(index_type, "upper_pid");
#else
      preg_num = Create_Preg(index_type, "upper_pid", NULL);
#endif
      WN* pid_ldid=LWN_CreateLdid(op_ldid, WN_start(processor_loop));
      du->Add_Def_Use(WN_start(processor_loop), pid_ldid);
      du->Add_Def_Use(WN_step(processor_loop), pid_ldid);
      du->Ud_Get_Def(pid_ldid)->Set_loop_stmt(processor_loop);
      if (!Reversed)
        pid_ldid=LWN_CreateExp2(op_sub,pid_ldid,LWN_Make_Icon(index_type,1));
      else
        pid_ldid=LWN_CreateExp2(op_add,pid_ldid,LWN_Make_Icon(index_type,1));
      WN* upper_pid_stid = LWN_CreateStid(op_stid, preg_num,
                                          index_type_preg_st, 
                                          Be_Type_Tbl(index_type), pid_ldid);
      LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, upper_pid_stid);

      // create step_upper=0, i.e. initialization
#ifdef _NEW_SYMTAB
      preg_num = Create_Preg(sync_array_type, "step_upper");
#else
      preg_num = Create_Preg(sync_array_type, "step_upper", NULL);
#endif
      WN* step_upper_stid = LWN_CreateStid(op_long_stid, preg_num,
                                           sync_array_type_preg_st,
                                           Be_Type_Tbl(sync_array_type),
                                           LWN_Make_Icon(sync_array_type, 0));
      LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, step_upper_stid);

      // create (my_step > step_upper+sync_dist[1])
      left = LWN_Copy_Tree(my_step_ldid);
      LWN_Copy_Def_Use(my_step_ldid,left,du);
      WN* step_upper_ldid = LWN_CreateLdid(op_long_ldid,step_upper_stid);
      du->Add_Def_Use(step_upper_stid,step_upper_ldid);
      right = LWN_CreateExp2(
			op_long_add,
  			step_upper_ldid,
			LWN_Make_Icon(sync_array_type,sync_tile_distances[1]));
      upper_guard=
        LWN_CreateExp2(op_long_gt, left,right);

      // create (my_step > step_upper+sync_dist[1] && my_pid>0)
      WN* ldid=LWN_CreateLdid(op_ldid,my_pid_stid);
      du->Add_Def_Use(my_pid_stid,ldid);
      left=ldid;
      WN* upper_boundary;
      if (!Reversed)
        upper_boundary=LWN_CreateExp2(op_gt,left,LWN_Make_Icon(index_type,0));
      else {
        right=LWN_Copy_Tree(WN_kid1(WN_end(processor_loop)));
        LWN_Copy_Def_Use(WN_kid1(WN_end(processor_loop)),right,du);
        upper_boundary=LWN_CreateExp2(op_lt, left, right);
      }
      upper_guard=LWN_CreateExp2(OPC_I4LAND, upper_guard, upper_boundary);

      // create step_upper=sync_array(pid_of_upper*epcl+offset)
      OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
      WN* wn_array = WN_Create(op_array, 3);
      WN_element_size(wn_array) = MTYPE_byte_size(sync_array_type);
      OPCODE ldaop = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
      WN_array_base(wn_array) = WN_CreateLda(ldaop,
					0,
					Sync_Array_Ptr_Ty,
					Sync_Array_St
					);

      ldid=LWN_CreateLdid(op_ldid, upper_pid_stid);
      du->Add_Def_Use(upper_pid_stid,ldid);
      du->Ud_Get_Def(ldid)->Set_loop_stmt(NULL);
      WN_array_index(wn_array,0) =
		LWN_CreateExp2(
			op_mpy, ldid,
                        LWN_Make_Icon(index_type,
					element_per_cache_line));
      sync_offset_ldid=LWN_CreateLdid(op_ldid,sync_offset_stid);
      Update_Sync_Offset_Ldid_DU(sync_offset_ldid);
      WN_array_index(wn_array,0)=
      	LWN_CreateExp2(op_add,
		     WN_array_index(wn_array,0),
		     sync_offset_ldid);
      WN_array_dim(wn_array,0) =
	LWN_Make_Icon(index_type,1024*element_per_cache_line);
      OPCODE loadop =
	OPCODE_make_op(OPR_ILOAD, sync_array_type, sync_array_type);
      TY_IDX wty = Be_Type_Tbl(sync_array_type);
      TY_IDX pty = Sync_Array_Ptr_Ty;
      WN* load = LWN_CreateIload(loadop, 0, wty, pty, wn_array);

      Copy_alias_info(Alias_Mgr, Sync_Array_Alias_Host, load);

      step_upper_update_stid=
        LWN_CreateStid(op_long_stid, step_upper_stid, load);
      LWN_Parentize(step_upper_update_stid);

      du->Add_Def_Use(step_upper_update_stid,step_upper_ldid);

    }

    if (sync_tile_distances[0]!= NULL_DIST) { // dependence from lower stripe

      // create lower_pid=pid+1
#ifdef _NEW_SYMTAB
      preg_num = Create_Preg(index_type, "lower_pid");
#else
      preg_num = Create_Preg(index_type, "lower_pid", NULL);
#endif
      WN* pid_ldid=LWN_CreateLdid(op_ldid, WN_start(processor_loop));
      du->Add_Def_Use(WN_start(processor_loop), pid_ldid);
      du->Add_Def_Use(WN_step(processor_loop), pid_ldid);
      du->Ud_Get_Def(pid_ldid)->Set_loop_stmt(processor_loop);
      if (!Reversed)
        pid_ldid=LWN_CreateExp2(op_add,pid_ldid,LWN_Make_Icon(index_type,1));
      else
        pid_ldid=LWN_CreateExp2(op_sub,pid_ldid,LWN_Make_Icon(index_type,1));
      WN* lower_pid_stid = LWN_CreateStid(op_stid, preg_num,
			index_type_preg_st, 
			Be_Type_Tbl(index_type), pid_ldid);
      LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, lower_pid_stid);

      // create step_lower=0, i.e. initialization
#ifdef _NEW_SYMTAB
      preg_num = Create_Preg(sync_array_type, "step_lower");
#else
      preg_num = Create_Preg(sync_array_type, "step_lower", NULL);
#endif
      WN* step_lower_stid = LWN_CreateStid(op_long_stid, preg_num,
                                           sync_array_type_preg_st,
                                           Be_Type_Tbl(sync_array_type),
	LWN_Make_Icon(sync_array_type, 0));
      LWN_Insert_Block_Before(
	LWN_Get_Parent(outer_doacross), outer_doacross, step_lower_stid);

      // create (my_step > step_lower+sync_dist[0])
      left = LWN_Copy_Tree(my_step_ldid);
      LWN_Copy_Def_Use(my_step_ldid,left,du);
      WN* step_lower_ldid=LWN_CreateLdid(op_long_ldid,step_lower_stid);
      right = LWN_CreateExp2(
			op_long_add,
  			step_lower_ldid,
			LWN_Make_Icon(sync_array_type,sync_tile_distances[0]));
      du->Add_Def_Use(step_lower_stid,step_lower_ldid);
      lower_guard= LWN_CreateExp2(op_long_gt, left,right);

      // create (my_step > step_lower+sync_dist[0] && my_pid<num_proc-1)
      WN* ldid=LWN_CreateLdid(op_ldid, my_pid_stid);
      du->Add_Def_Use(my_pid_stid,ldid);

      left=ldid;
      WN* upper_boundary;
      if (!Reversed) {
        right=LWN_Copy_Tree(WN_kid1(WN_end(processor_loop)));
        LWN_Copy_Def_Use(WN_kid1(WN_end(processor_loop)),right,du);
        upper_boundary=LWN_CreateExp2(op_lt, left, right);
      } else
        upper_boundary=LWN_CreateExp2(op_gt,left,LWN_Make_Icon(index_type,0));
      lower_guard=LWN_CreateExp2(OPC_I4LAND, lower_guard, upper_boundary);

      // create step_lower=sync_array(pid_of_lower*epcl+offset)
      OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
      WN* wn_array = WN_Create(op_array, 3);
      WN_element_size(wn_array) = MTYPE_byte_size(sync_array_type);
      OPCODE ldaop = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
      WN_array_base(wn_array) = WN_CreateLda(ldaop,
					0,
					Sync_Array_Ptr_Ty,
					Sync_Array_St
					);

      ldid=LWN_CreateLdid(op_ldid, lower_pid_stid);
      du->Add_Def_Use(lower_pid_stid,ldid);
      du->Ud_Get_Def(ldid)->Set_loop_stmt(NULL);
      WN_array_index(wn_array,0) =
		LWN_CreateExp2(
			op_mpy, ldid,
                        LWN_Make_Icon(index_type,element_per_cache_line));
      sync_offset_ldid=LWN_CreateLdid(op_ldid,sync_offset_stid);
      Update_Sync_Offset_Ldid_DU(sync_offset_ldid);
      WN_array_index(wn_array,0)=
      	LWN_CreateExp2(op_add,
		     WN_array_index(wn_array,0),
		     sync_offset_ldid);
      WN_array_dim(wn_array,0) =
	LWN_Make_Icon(index_type,1024*element_per_cache_line);
      OPCODE loadop =
	OPCODE_make_op(OPR_ILOAD, sync_array_type, sync_array_type);
      TY_IDX wty = Be_Type_Tbl(sync_array_type);
      TY_IDX pty = Sync_Array_Ptr_Ty;
      WN* load = LWN_CreateIload(loadop, 0, wty, pty, wn_array);

      Copy_alias_info(Alias_Mgr, Sync_Array_Alias_Host, load);

      step_lower_update_stid=
        LWN_CreateStid(op_stid, step_lower_stid, load);
      du->Add_Def_Use(step_lower_update_stid,step_lower_ldid);
      LWN_Parentize(step_lower_update_stid);

    }

    WN* wn_upper_do_while=NULL;
    if (upper_guard) {
      // create busy (while) loop and mem barriers for upper guard
      WN* do_while_guard=LWN_Copy_Tree(WN_kid1(upper_guard));
      LWN_Copy_Def_Use(WN_kid1(upper_guard),do_while_guard,du);
      wn_upper_do_while=LWN_CreateDoWhile(do_while_guard, WN_CreateBlock());
      fb = WN_CreateBarrier(TRUE, 0);	// forward
      bb = WN_CreateBarrier(FALSE, 0);	// backward
      LWN_Insert_Block_Before(WN_while_body(wn_upper_do_while),NULL,bb);
      if (step_upper_update_stid)
        LWN_Insert_Block_Before(
          WN_while_body(wn_upper_do_while),NULL,step_upper_update_stid);
      LWN_Insert_Block_Before(WN_while_body(wn_upper_do_while),NULL,fb);
      WN *loop = Enclosing_Do_Loop(fb);
      if (loop && Do_Loop_Is_Good(loop)) {
        VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
        v = Array_Dependence_Graph->Add_Vertex(bb);
        if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
      }
      Build_Doloop_Stack(LWN_Get_Parent(wn_upper_do_while), loop_stack);
      LNO_Build_Access(wn_upper_do_while, loop_stack, &LNO_default_pool);

      // create if and mem barriers for upper guard
      WN* wn_if=LWN_CreateIf(upper_guard, WN_CreateBlock(), WN_CreateBlock());
      LWN_Insert_Block_Before(WN_then(wn_if),NULL,wn_upper_do_while);
      fb = WN_CreateBarrier(TRUE, 0);	// forward
      hw_sync=WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
	INTRN_SYNCHRONIZE,0,NULL);
      bb = WN_CreateBarrier(FALSE, 0);	// backward
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,bb);
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,hw_sync);
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,fb);
      LWN_Insert_Block_After(WN_do_body(outer_doacross), NULL, wn_if);
      loop = Enclosing_Do_Loop(fb);
      if (loop && Do_Loop_Is_Good(loop)) {
        VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
        v = Array_Dependence_Graph->Add_Vertex(bb);
        if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
      }
      IF_INFO *if_info =
	  CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map,wn_if,(void *)if_info);
      LNO_Build_If_Access(wn_if, loop_stack);

    }

    WN* wn_lower_do_while=NULL;
    if (lower_guard) {
      // create busy (while) loop and mem barriers for upper guard
      WN* do_while_guard=LWN_Copy_Tree(WN_kid1(lower_guard));
      LWN_Copy_Def_Use(WN_kid1(lower_guard),do_while_guard,du);
      wn_lower_do_while=LWN_CreateDoWhile(do_while_guard, WN_CreateBlock());
      fb = WN_CreateBarrier(TRUE, 0);	// forward
      bb = WN_CreateBarrier(FALSE, 0);	// backward
      LWN_Insert_Block_Before(WN_while_body(wn_lower_do_while),NULL,bb);
      if (step_lower_update_stid)
        LWN_Insert_Block_Before(
          WN_while_body(wn_lower_do_while),NULL,step_lower_update_stid);
      LWN_Insert_Block_Before(WN_while_body(wn_lower_do_while),NULL,fb);
      WN *loop = Enclosing_Do_Loop(fb);
      if (loop && Do_Loop_Is_Good(loop)) {
        VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
        v = Array_Dependence_Graph->Add_Vertex(bb);
        if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
      }
      Build_Doloop_Stack(LWN_Get_Parent(wn_lower_do_while), loop_stack);
      LNO_Build_Access(wn_lower_do_while, loop_stack, &LNO_default_pool);

      // create if and mem barriers for lower guard
      WN* wn_if=LWN_CreateIf(lower_guard, WN_CreateBlock(), WN_CreateBlock());
      LWN_Insert_Block_Before(WN_then(wn_if),NULL,wn_lower_do_while);
      fb = WN_CreateBarrier(TRUE, 0);	// forward
      hw_sync=WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,
	INTRN_SYNCHRONIZE,0,NULL);
      bb = WN_CreateBarrier(FALSE, 0);	// backward
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,bb);
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,hw_sync);
      LWN_Insert_Block_After(WN_do_body(outer_doacross),NULL,fb);
      LWN_Insert_Block_After(WN_do_body(outer_doacross), NULL, wn_if);
      loop = Enclosing_Do_Loop(fb);
      if (loop && Do_Loop_Is_Good(loop)) {
        VINDEX16 v = Array_Dependence_Graph->Add_Vertex(fb);
        v = Array_Dependence_Graph->Add_Vertex(bb);
        if (!v) LNO_Erase_Dg_From_Here_In(bb,Array_Dependence_Graph);
      }
      IF_INFO *if_info =
	  CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map,wn_if,(void *)if_info);
      LNO_Build_If_Access(wn_if, loop_stack);

    }

  }

  // collect good loops outside processor loop
  STACK<WN*>* outer_loop_stack = CXX_NEW(STACK<WN*>(&LNO_local_pool), 
    &LNO_local_pool);
  WN* outer_good_loop=LWN_Get_Parent(processor_loop);
  while (outer_good_loop) {
    if (WN_opcode(outer_good_loop)==OPC_DO_LOOP &&
       Do_Loop_Is_Good(outer_good_loop)) {
      outer_loop_stack->Push(outer_good_loop);
    }
    outer_good_loop=LWN_Get_Parent(outer_good_loop);
  }

  // mark good loops outside processor loop bad and remove dimension
  // from the dep vectors
  while (!outer_loop_stack->Is_Empty()) {
    outer_good_loop=outer_loop_stack->Pop();
    Remove_Unity_Trip_Loop_Dep_Update(outer_good_loop, dg, TRUE);
    Get_Do_Loop_Info(outer_good_loop)->Has_Bad_Mem=TRUE;
    LNO_Erase_Vertices_In_Loop(outer_good_loop,dg);
  }

  // mark processor loop and doacross tile loop bad
  Remove_Unity_Trip_Loop_Dep_Update(processor_loop, dg, TRUE);
  Get_Do_Loop_Info(processor_loop)->Has_Bad_Mem=TRUE;
  Get_Do_Loop_Info(processor_loop)->Has_Calls=TRUE;	// sync
  Get_Do_Loop_Info(processor_loop)->Has_Unsummarized_Calls=TRUE;	// sync
  Remove_Unity_Trip_Loop_Dep_Update(outer_doacross, dg, TRUE);
  Get_Do_Loop_Info(outer_doacross)->Has_Bad_Mem=TRUE;
  Get_Do_Loop_Info(outer_doacross)->Has_Calls=TRUE;	// sync
  Get_Do_Loop_Info(outer_doacross)->Has_Unsummarized_Calls=TRUE;	// sync

  MEM_POOL_Pop(&LNO_local_pool);

  return processor_loop;

}

/***********************************************************************
 *
 * Return an ldid of the runtime __mp_max_numthreads variable.
 *
***********************************************************************/
static WN* Get_Runtime_Max_Numthreads_Ldid () {
  
  OPCODE ldid_op = OPCODE_make_op(OPR_LDID, MTYPE_I4, MTYPE_I4);
  WN* ldid_wn = WN_CreateLdid (ldid_op, 0, Max_Numthread_St,
                               Be_Type_Tbl(MTYPE_I4));
  Create_global_alias (Alias_Mgr, Max_Numthread_St, ldid_wn, NULL);
  Du_Mgr->Add_Def_Use (Func_Root, ldid_wn);
  return ldid_wn;
}

/***********************************************************************
 *
 * Create a Sync_Structure block for book-keeping variables and a sync
 * array.
 *
 ***********************************************************************/
static void Create_Sync_Structure () {

  if (Sync_Structure_Created)
    return;

  Sync_Structure_Created=TRUE;

  BOOL is_global=TRUE;
  BOOL is_local=FALSE;
  
#ifdef _NEW_SYMTAB
  TY_IDX array_ty_idx;
  TY& array_ty = New_TY(array_ty_idx);
  TY_Init (array_ty,
           1024*128,    // has to be in sync with libmp/dsmcrt.c
           KIND_ARRAY,
           MTYPE_UNKNOWN,
           Save_Str("array_I8"));
  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb,0,1024*16-1, // has to be in sync with libmp/dsmcrt.c
           1);
  Set_ARB_first_dimen (arb);
  Set_ARB_last_dimen (arb);
  Set_TY_align_exp(array_ty_idx,8);
  Set_TY_etype(array_ty,Be_Type_Tbl(MTYPE_I1));
  Set_TY_arb(array_ty,arb);
  Sync_Array_Ptr_Ty=Make_Pointer_Type(array_ty_idx);
  Set_TY_ptr_as_array(Sync_Array_Ptr_Ty);

#else
  TY_IDX array_ty          = New_TY(is_global);
  TY_IDX array_ty_idx = array_ty; 
  TY_kind(array_ty)     = KIND_ARRAY;
  TY_btype(array_ty)    = MTYPE_M;

  ARI *ari = New_ARI (1, is_global);
  ARI_etype(ari)        = Be_Type_Tbl(MTYPE_I8);
  ARI_const_zofst(ari)  = TRUE;
  ARI_zofst_val(ari)    = 0;
  ARB_const_lbnd(ARI_bnd(ari,0))    = TRUE;
  ARB_lbnd_val(ARI_bnd(ari,0))      = 0;
  ARB_const_ubnd(ARI_bnd(ari,0))    = TRUE;
  ARB_ubnd_val(ARI_bnd(ari,0))      = 1024*16-1;
  				    // has to be in sync with libmp/dsmcrt.c
  ARB_const_stride(ARI_bnd(ari,0))  = TRUE;
  ARB_stride_val(ARI_bnd(ari,0))    = 1;
  TY_size(array_ty)     = 1024*128; // has to be in sync with libmp/dsmcrt.c
  TY_align(array_ty)    = 8;
  TY_name(array_ty)     = Save_Str ("array_I8");
  TY_arinfo(array_ty)   = ari;
  Enter_TY (array_ty);
  Sync_Array_Ptr_Ty=Make_Pointer_Type(array_ty);
  Set_TY_ptr_as_array(Sync_Array_Ptr_Ty);
#endif

  ST* st        = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str("__sync_length"),
           CLASS_VAR,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           Be_Type_Tbl(MTYPE_I4));
  Clear_ST_addr_not_saved(st);
  Sync_Length_St=st;

  st        = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str("__sync_offset"),
           CLASS_VAR,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           Be_Type_Tbl(MTYPE_I4));
  Clear_ST_addr_not_saved(st);
  Sync_Offset_St=st;

  st        = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (st,
           Save_Str("__sync_array"),
           CLASS_VAR,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           array_ty_idx);
  Clear_ST_addr_not_saved(st);
  Sync_Array_St=st;

  TY_IDX vi4_ty = Copy_TY(Be_Type_Tbl(MTYPE_I4));
  Set_TY_is_volatile(vi4_ty);

  st        = New_ST(is_global ? GLOBAL_SYMTAB : CURRENT_SYMTAB);
  ST_Init (st,
#ifndef KEY
           Save_Str("__mp_max_numthreads"),
#else
           Save_Str("__ompc_max_numthreads"),
#endif
           CLASS_VAR,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           vi4_ty);
  Set_ST_not_gprel(st);
  Max_Numthread_St=st;

} /* Create_Sync_Structure */

/***********************************************************************
 *
 * Create a loop before the processor loop to initialize sync array
 *
 ***********************************************************************/
static WN* Create_Initialize_Loop (WN* processor_loop, 
				     ARRAY_DIRECTED_GRAPH16* dg,
				     DU_MANAGER* du,
				     BOOL Is_Pdo_Region) {

  MEM_POOL_Push(&LNO_local_pool);

  TYPE_ID sync_array_type=MTYPE_I8;
  TYPE_ID index_type=MTYPE_I4;
  INT cache_line_size=128;
  if (Mhd.L[1].Valid())
    cache_line_size=Mhd.L[1].Line_Size;
  INT element_per_cache_line=cache_line_size/MTYPE_byte_size(sync_array_type);
  OPCODE op_stid = OPCODE_make_op(OPR_STID, MTYPE_V, index_type);
  OPCODE op_ldid = OPCODE_make_op(OPR_LDID, index_type, index_type);
  OPCODE op_add = OPCODE_make_op(OPR_ADD, index_type, MTYPE_V);
  OPCODE op_sub = OPCODE_make_op(OPR_SUB, index_type, MTYPE_V);
  OPCODE op_mpy = OPCODE_make_op(OPR_MPY, index_type, MTYPE_V);
  OPCODE op_max = OPCODE_make_op(OPR_MAX, index_type, MTYPE_V);
  OPCODE op_min = OPCODE_make_op(OPR_MAX, index_type, MTYPE_V);
  OPCODE op_array = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
  OPCODE ldaop = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
  OPCODE istoreop = OPCODE_make_op(OPR_ISTORE, MTYPE_V, sync_array_type);
  ST* sync_array_type_preg_st = MTYPE_To_PREG(sync_array_type);
  ST* index_type_preg_st = MTYPE_To_PREG(index_type);
  WN* doacross_region=Get_MP_Region(processor_loop);
  WN* num_thread_ldid=NULL;
  if (Is_Pdo_Region) {
    doacross_region=LWN_Get_Parent(LWN_Get_Parent(doacross_region));
    num_thread_ldid=Get_Runtime_Max_Numthreads_Ldid();
  } else {
    WN* num_thread_stid=WN_prev(doacross_region);
    if (num_thread_stid==NULL) {
      // the num_thread_stid is outside an if for mp versioning
      num_thread_stid=WN_prev(LWN_Get_Parent(LWN_Get_Parent(doacross_region)));
    }
    // create ldids which will be used later
    num_thread_ldid=LWN_CreateLdid(op_ldid,num_thread_stid);
    du->Add_Def_Use(num_thread_stid,num_thread_ldid);
  }
  WN* sync_array_length_ldid = WN_CreateLdid(op_ldid,
                                        0,
                                        Sync_Length_St,
                                        ST_type(Sync_Length_St)
                                        );
  WN* sync_array_offset_ldid = WN_CreateLdid(op_ldid,
                                        0,
                                        Sync_Offset_St,
                                        ST_type(Sync_Offset_St)
                                        );

  // store alias hosts info or copy alias info
  if (Sync_Length_Alias_Host==NULL) {
    Create_global_alias(Alias_Mgr,Sync_Length_St,sync_array_length_ldid,NULL);
    Create_global_alias(Alias_Mgr,Sync_Offset_St,sync_array_offset_ldid,NULL);
    Sync_Length_Alias_Host=sync_array_length_ldid;
    Sync_Offset_Alias_Host=sync_array_offset_ldid;
  } else {
    Copy_alias_info(Alias_Mgr, Sync_Length_Alias_Host, sync_array_length_ldid);
    Copy_alias_info(Alias_Mgr, Sync_Offset_Alias_Host, sync_array_offset_ldid);
  }
 
  // create loop to initialize protion of sync_array
#ifdef _NEW_SYMTAB
  WN_OFFSET preg_num = Create_Preg(index_type, "sync_init");
#else
  WN_OFFSET preg_num = Create_Preg(index_type, "sync_init", NULL);
#endif

  WN* loop_start = LWN_CreateStid(
			op_stid, 
			preg_num, 
			index_type_preg_st,
			Be_Type_Tbl(index_type),
    		        LWN_CreateExp2(
				op_mpy,
                        	sync_array_length_ldid,
                   		LWN_Make_Icon(index_type,
					element_per_cache_line)));

  WN* ldid=LWN_CreateLdid(op_ldid,loop_start);
  WN* loop_end = LWN_CreateExp2(OPCODE_make_op(OPR_LT,Boolean_type,index_type),
                        ldid,
    		        LWN_CreateExp2(
				op_mpy,
                        	num_thread_ldid,
                   		LWN_Make_Icon(index_type,
					element_per_cache_line)));
  WN* ldid1=LWN_CreateLdid(op_ldid,loop_start);
  WN* loop_step = LWN_CreateStid(op_stid,loop_start,
			LWN_CreateExp2(op_add,
                        ldid1,
                        LWN_Make_Icon(index_type,1)));

  WN* loop_index = WN_CreateIdname(preg_num, index_type_preg_st);

  du->Add_Def_Use(loop_start,ldid);
  du->Add_Def_Use(loop_step,ldid);
  du->Add_Def_Use(loop_start,ldid1);
  du->Add_Def_Use(loop_step,ldid1);

  WN* init_loop=LWN_CreateDO(	loop_index,
				loop_start,
				loop_end,
				loop_step,
				WN_CreateBlock());

  du->Ud_Get_Def(ldid)->Set_loop_stmt(init_loop);
  du->Ud_Get_Def(ldid1)->Set_loop_stmt(init_loop);

  // create sync_array(sync_init)=0
  WN* wn_array = WN_Create(op_array, 3);
  WN_element_size(wn_array) = MTYPE_byte_size(sync_array_type);
  WN_array_base(wn_array) = WN_CreateLda(ldaop,
                                        0,
                                        Sync_Array_Ptr_Ty,
                                        Sync_Array_St
                                        );

  WN* ldid4=LWN_CreateLdid(op_ldid, loop_start);
  LWN_Copy_Def_Use(ldid,ldid4,du);
  WN_array_index(wn_array,0) = ldid4;
  WN_array_dim(wn_array,0) = LWN_Make_Icon(index_type,1024*16);
  TY_IDX pty = Sync_Array_Ptr_Ty;
  WN* store=LWN_CreateIstore(
	istoreop, 0, pty, LWN_Make_Icon(sync_array_type,0),wn_array);

  if (Sync_Array_Alias_Host==NULL) {
    Create_lda_array_alias(Alias_Mgr, WN_array_base(wn_array), store);
    Sync_Array_Alias_Host = store;
  } else {
    Copy_alias_info(Alias_Mgr, Sync_Array_Alias_Host, store);
  }

  LWN_Insert_Block_After(WN_do_body(init_loop),NULL,store);

  LWN_Parentize(init_loop);

  // insert init loop before doacross region
  LWN_Insert_Block_Before(
	LWN_Get_Parent(doacross_region),
	doacross_region,
	init_loop);

  // create DO_LOOP_INFO and access vectors for init loop
  DO_LOOP_INFO*  dli = (DO_LOOP_INFO *)
    CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,NULL,NULL,NULL,
	FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE) ,&LNO_default_pool);
  dli->Depth = Get_Do_Loop_Info(processor_loop)->Depth;
  WN_MAP_Set(LNO_Info_Map,init_loop,(void *)dli);

  DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                               &LNO_local_pool);

  Build_Doloop_Stack(LWN_Get_Parent(init_loop), loop_stack);

  LNO_Build_Access(init_loop, loop_stack, &LNO_default_pool);
  LNO_Build_Do_Access(init_loop, loop_stack);

  if (!dg->Build_Region(init_loop,init_loop,loop_stack, TRUE)) {
      DevWarn("Array dependence graph overflowed");
      LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(init_loop), dg);
  }

  //---------------------------------------------------------------
  // create sync_offset=1-sync_offset
  WN* sync_offset_update_stid=
	LWN_CreateStid(op_stid, sync_array_offset_ldid,
        	LWN_CreateExp2(	op_sub,
				LWN_Make_Icon(index_type,1),
				sync_array_offset_ldid));
  Update_Sync_Offset_Stid_DU(sync_offset_update_stid);
  Update_Sync_Offset_Ldid_DU(sync_array_offset_ldid);

  LWN_Insert_Block_After(
	LWN_Get_Parent(init_loop),
	init_loop,
	sync_offset_update_stid);
  //---------------------------------------------------------------
  // create sync_length = MAX(sync_length, frz_num_threads9)
  //
  // 4/10/98 DEM: This should be sync_length = frz_num_threads9
  // while the original is slightly more efficient, it leads to 
  // a race condition whenever the number of threads dynamically decreases
  // and then increases in a program


  //WN* ldid2=LWN_Copy_Tree(sync_array_length_ldid);
  //LWN_Copy_Def_Use(sync_array_length_ldid,ldid2,du);

  WN* ldid3=LWN_Copy_Tree(num_thread_ldid);
  LWN_Copy_Def_Use(num_thread_ldid,ldid3,du);
  WN* sync_length_update_stid=
        LWN_CreateStid(op_stid, sync_array_length_ldid, ldid3);

  Update_Sync_Length_Stid_DU(sync_length_update_stid);
  Update_Sync_Length_Ldid_DU(sync_array_length_ldid);
  //Update_Sync_Length_Ldid_DU(ldid2);

  LWN_Insert_Block_After(
	LWN_Get_Parent(init_loop),
	sync_offset_update_stid,
	sync_length_update_stid);

  MEM_POOL_Pop(&LNO_local_pool);
  return init_loop;
}

//-----------------------------------------------------------------------
// NAME: Doacross_Init
// FUNCTION:
//	Initialize for doacross transformation.
//	Should be called for each PU.
// ARGUMENTS:
//	func_nd	root of a pu
//	du	DU manager
//-----------------------------------------------------------------------

extern void Doacross_Init(
			WN* func_nd)
{
  
  if (func_nd!=NULL) {
    // record the root of PU
    Func_Root=func_nd;
    return;
  }

  if (Doacross_Inited) 
    // if init is done already
    return;

  Sync_Array_Alias_Host=NULL;
  Sync_Length_Alias_Host=NULL;
  Sync_Offset_Alias_Host=NULL;
  Sync_Structure_Created=FALSE;
  MEM_POOL_Initialize(&DOACROSS_default_pool,"DOACROSS_default_pool",FALSE);
  MEM_POOL_Push(&DOACROSS_default_pool);

  sync_offset_stid_stack=CXX_NEW(STACK_OF_WN(&DOACROSS_default_pool),
				 &DOACROSS_default_pool);
  sync_offset_ldid_stack=CXX_NEW(STACK_OF_WN(&DOACROSS_default_pool),
				 &DOACROSS_default_pool);
  sync_length_stid_stack=CXX_NEW(STACK_OF_WN(&DOACROSS_default_pool),
				 &DOACROSS_default_pool);
  sync_length_ldid_stack=CXX_NEW(STACK_OF_WN(&DOACROSS_default_pool),
				 &DOACROSS_default_pool);

  // prepare staccks for updating DU-chains of scalars referenced
  // across loops
  WN* wn=Func_Root;
  while (wn) {
    OPCODE opc=WN_opcode(wn);
    if (opc==OPC_FUNC_ENTRY || opc==OPC_ALTENTRY) {
      sync_offset_stid_stack->Push(wn);
      sync_length_stid_stack->Push(wn);
    } else if (opc==OPC_RETURN
#ifdef KEY
  	       || opc==OPC_GOTO_OUTER_BLOCK
#endif
	       ) {
      sync_offset_ldid_stack->Push(wn);
      sync_length_ldid_stack->Push(wn);
    }
    wn=LWN_Get_Next_Stmt_Node(wn);
  }
  Doacross_Inited=TRUE;
}

extern void Doacross_Finish()
{
  Func_Root=NULL;

  if (!Doacross_Inited) 
    return;

  MEM_POOL_Pop(&DOACROSS_default_pool);
  MEM_POOL_Delete(&DOACROSS_default_pool);
  Doacross_Inited=FALSE;
}

//--------------------------------------------------------------------
// NAME: Dep_Carried_By_Outer_Loop
// FUNCTION: Check if a dependence is carried by outer loop assuming
// ARGUMENTS:
//	parallel_depth	depth of the loop to be parallelized
//	dep_id		index into the dep matrix
//	doacross_dep_info
//			the dep. matrix
//--------------------------------------------------------------------
static BOOL Dep_Carried_By_Outer_Loop(
			INT outer_depth,
			INT parallel_depth,
			INT dep_id,
			SNL_DEP_MATRIX* doacross_dep_info)
{

  INT carried_by= -1;
  for (INT i=0; i<parallel_depth-outer_depth; i++) {
 
    SNL_DEP dep=(*doacross_dep_info)(dep_id,i);
    INT distance=dep.Distance;
    if (distance==0)
      continue;
    if (dep.Unbounded_Min() || dep.Unbounded_Max()) {
  
      if (dep.Unbounded_Min() && dep.Unbounded_Max())
        continue;   // a '*' distance
      else if (dep.Unbounded_Max() && distance>0) {
 
        carried_by=i;
        break;
      } else
        continue;
    } else {
 
      if (distance>0) {
 
        carried_by=i;
        break;
      } else if (distance<0)
        Is_True(0, ("strange dep. vector"));
    }
  }
  if (carried_by != -1) {
    return TRUE;               // a dep carried by outer loops
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Compute_Sync_Distances_From_Dep
// FUNCTION:
//	Given a dependence (d1,d2), return the sync vector that will be
//	synchronized in doacross execution
// ARGUMENTS:
//	'dep1' -- d1 (see above)
//	'dep2' -- d2 (see above)
//	'sync_distances' -- returned sync vectors. i.e.,
//		(sync_distances[0],-1) and (sync_distances[1],1)
//-----------------------------------------------------------------------

static void Compute_Sync_Distances_From_Dep(
			  SNL_DEP dep1,
			  SNL_DEP dep2,
			  INT sync_distances[])
{
  INT upper=0;
  INT lower=1;

  INT d1=dep1.Distance;
  if (dep1.Unbounded_Min() || d1<0) {
    sync_distances[upper]=0;
    sync_distances[lower]=0;
    return;
  }

  INT d2=dep2.Distance;
  if (!dep2.Unbounded_Min()) {
    if (d2<0)
      if (-d2>d1)
        sync_distances[upper]=0;
      else
        sync_distances[upper]=d1/(-d2);
    else
      sync_distances[upper]= NULL_DIST;
  } else
    sync_distances[upper]=0;

  if (!dep2.Unbounded_Max()) {
    if (d2>0)
      if (d2>d1)
        sync_distances[lower]=0;
      else
        sync_distances[lower]=d1/d2;
    else
      sync_distances[lower]= NULL_DIST;
  } else
    sync_distances[lower]=0;
}

//-----------------------------------------------------------------------
// NAME: Compute_Sync_Distances
// FUNCTION:
//	Given an SNL and a loop to be parallelized as doacross,
//	compute the sync vectors based on the array dependences in the SNL
// ARGUMENTS:
//	'wn_outer' -- outer loop of the SNL
//	'nloops' -- how many levels are there
//	'permutation' -- the permutation for the SNL
//	'parallel_depth' -- which loop to be parallelized as doacross
//	'sdm_inv' -- the set of array dependences
//	'sync_distances' -- returned sync vectors. i.e.,
//		(sync_distances[0],-1) and (sync_distances[1],1)
//-----------------------------------------------------------------------

extern void Compute_Sync_Distances(
			  WN* wn_outer,
			  INT nloops,
			  INT permutation[],
			  INT parallel_depth,
			  SNL_DEP_MATRIX** sdm_inv,
			  BOOL retained[],
			  INT sync_distances[])
{

  MEM_POOL_Push(&LNO_local_pool);

  sync_distances[0] = NULL_DIST;
  sync_distances[1] = NULL_DIST;
  INT outer_depth=Do_Loop_Depth(wn_outer);

  for (INT section=parallel_depth-outer_depth+1; section<nloops; section++) {

    if (!retained[section])
      continue;

    // examine dependences from all sections and apply permutation
    SNL_DEP_MATRIX* doacross_dep_info=
	  CXX_NEW(SNL_DEP_MATRIX(sdm_inv[section],&LNO_local_pool),
		  &LNO_local_pool);
    doacross_dep_info->Apply(permutation);
    //doacross_dep_info->Print(stdout);

    // compute necessary sync for each dep vector
    INT num_deps=doacross_dep_info->Ndep();
    for (INT d=0; d<num_deps; d++) {
    
	if (Dep_Carried_By_Outer_Loop(
	      outer_depth,parallel_depth,d,doacross_dep_info))
	continue;

      SNL_DEP d1=(*doacross_dep_info)(d,parallel_depth+1-outer_depth);
      SNL_DEP d2=(*doacross_dep_info)(d,parallel_depth-outer_depth);

      if (d1.Moreless == SNL_DEP::SNL_DEP_EXACT && d1.Distance==0 &&
          d2.Moreless == SNL_DEP::SNL_DEP_EXACT && d2.Distance==0)
        continue;

      INT sync_distances_tmp[2];
      Compute_Sync_Distances_From_Dep(d1,d2, sync_distances_tmp);

      if (sync_distances[0] == NULL_DIST)
	sync_distances[0] = sync_distances_tmp[0];
      else if (sync_distances_tmp[0] != NULL_DIST)
        if (sync_distances[0] == 0 || sync_distances_tmp[0] == 0)
          sync_distances[0] = 0;
	else
          sync_distances[0] = Min(sync_distances[0],sync_distances_tmp[0]);

      if (sync_distances[1] == NULL_DIST)
	sync_distances[1] = sync_distances_tmp[1];
      else if (sync_distances_tmp[1] != NULL_DIST)
        if (sync_distances[1] == 0 || sync_distances_tmp[1] == 0)
          sync_distances[1] = 0;
	else
          sync_distances[1] = Min(sync_distances[1],sync_distances_tmp[1]);

    }
  }

  MEM_POOL_Pop(&LNO_local_pool);
}

// the following is in fusion.h
extern WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok);

//-----------------------------------------------------------------------
// NAME: Get_Doacross_Tile_Size
// FUNCTION:
//	Given sync vectors (a,-1) and (b,1), return the max size of the
//	tiling that avoids the deadlock and initial delay
// ARGUMENTS:
//	'sync_distances[0]' is a (see above)
//	'sync_distances[1]' is b (see above)
//-----------------------------------------------------------------------

extern INT Get_Doacross_Tile_Size(
			INT sync_distances[],
			WN* wn_outer,
			INT permutation[],
			INT nloops,
			INT parallel_depth,
			INT num_procs,
			double work_estimate)
{

  INT tile_size;
  INT outer_depth=Do_Loop_Depth(wn_outer);

  if (sync_distances[0]==0 && sync_distances[1]==0)
    return 0;

  INT s1=sync_distances[0];
  INT s2=sync_distances[1];
  INT num_syncs=0;
  if (s1!=NULL_DIST)
    num_syncs++;
  if (s2!=NULL_DIST)
    num_syncs++;
  double C=Single_Sync_Cycle*(double)num_syncs;

  MEM_POOL_Push(&LNO_local_pool);

  WN** loops=CXX_NEW_ARRAY(WN*,nloops,&LNO_local_pool);
  WN* wn=wn_outer;
  INT i;
  for (i=0; i<nloops; i++) {
    loops[i]=wn;
    wn=Get_Only_Loop_Inside(wn,TRUE);
  }
  INT* iter_count_after_permutation=CXX_NEW_ARRAY(INT,nloops,&LNO_local_pool);
  for (i=0; i<nloops; i++) {
    iter_count_after_permutation[i]=
	Get_Do_Loop_Info(loops[permutation[i]])->Est_Num_Iterations;
  }

  double M=(double)iter_count_after_permutation[parallel_depth+1-outer_depth];
  double N=(double)iter_count_after_permutation[parallel_depth-outer_depth];
  double P=(double)num_procs;
  double t=work_estimate;
  for (i=nloops-1; i>parallel_depth+1-outer_depth; i--)
    t = t * iter_count_after_permutation[i];
  double T=t*N/P;

  // total execution time=(TxB+C)x(P-1)+TxM+Cx(M-1)/B
  // it is minimized when B=sqrt((CxM)/(Tx(P-1)))
  double tmp=sqrt((C*M)/(T*(P-1.0)));
  INT best_skewed_block_size=MAX(1,(INT)(tmp+.5));
  INT legal_block_size_limit;
  INT not_skewed_block_size;

  // if we use not_skewed_block_size, we can avoid the delay
  // on the other hand, the block size cannot exceed
  // legal_block_size_limit otherwise a dep cycle can be formed
  if (s1==NULL_DIST && s2==NULL_DIST) {
    not_skewed_block_size=INT_MAX;
    legal_block_size_limit=INT_MAX;
  } else if (s1==NULL_DIST) {
    not_skewed_block_size=s2;
    legal_block_size_limit=INT_MAX;
  } else if (s2==NULL_DIST) {
    not_skewed_block_size=s1;
    legal_block_size_limit=INT_MAX;
  } else {
    not_skewed_block_size=MIN(s1,s2);
    legal_block_size_limit=MAX(s1,s2);
  }

  extern INT Parallel_Debug_Level;
  INT parallel_debug_level = Get_Trace(TP_LNOPT2, TT_LNO_PARALLEL_DEBUG)
    ? Parallel_Debug_Level : 0;

  // find the appropriate block size
  if (LNO_Preferred_doacross_tile_size!=0 &&
      LNO_Preferred_doacross_tile_size<=legal_block_size_limit)
    tile_size=LNO_Preferred_doacross_tile_size;
  else if (not_skewed_block_size==0)
    if (best_skewed_block_size>legal_block_size_limit)
      tile_size=1;
    else
      tile_size=best_skewed_block_size;
  else if (best_skewed_block_size<=legal_block_size_limit) {
    double use_best = M * C;
    double use_no_skew = (double)not_skewed_block_size * C * (P-1.0) +
			  2.0 * sqrt(T*(P-1.0)*M*C);
    if (use_best>use_no_skew)
      tile_size=best_skewed_block_size;
    else
      tile_size=not_skewed_block_size;
  } else
      tile_size=not_skewed_block_size;

  if (parallel_debug_level >= 2) {
    printf("  C=%13.2f, M=%13.2f, N=%13.2f, T=%13.2f\n", C, M, N, T);
    printf("  P=%13.2f, ", P);
    if (s1==NULL_DIST) printf("s1=NULL_DIST, "); else printf("s1=%d, ", s1);
    if (s2==NULL_DIST) printf("s2=NULL_DIST\n"); else printf("s2=%d\n", s2);
    if (not_skewed_block_size==INT_MAX)
      printf("  not_skewed_block_size=inf\n");
    else
      printf("  not_skewed_block_size=%d\n", not_skewed_block_size);
    printf("  best_skewed_block_size=%d\n", best_skewed_block_size);
    printf("  preferred_doacross_tile_size=%d\n",
            LNO_Preferred_doacross_tile_size);
    if (legal_block_size_limit==INT_MAX)
      printf("  legal_block_size_limit=inf\n");
    else
      printf("  legal_block_size_limit=%d\n", legal_block_size_limit);
    printf("  doacross_tile_size=%d\n", tile_size);
  }

  MEM_POOL_Pop(&LNO_local_pool);

  return tile_size;

}

//-----------------------------------------------------------------------
// NAME: Compute_Doacross_Delay_Cycle
// FUNCTION:
//	Compute the maximal initial delay due to skewing in doacross
// ARGUMENTS:
//	'wn_outer' is the outer most loop in the SNL
//	'permutation' determines the loop order after permutation
//	'parallel_depth' gives the loop to be MP-tiled for doacross
//	'num_proc' is the number of processors
//	'doacross_tile_size' is the tile size for synchronization
//	if 'sync_distances[0]'=a, there is a sync vector (a,-1)
//	if 'sync_distances[1]'=b, there is a sync vector (b,1)
//	'machine_cycle' is the normalized cycle count for each processor's
//	innermost loop portion
//-----------------------------------------------------------------------

extern double Compute_Doacross_Delay_Cycle(
			WN* wn_outer,
			INT permutation[],
			INT parallel_depth,
			INT num_proc,
			INT doacross_tile_size,
			INT sync_distances[],
			double machine_cycles)
{

  INT outer_depth=Do_Loop_Depth(wn_outer);
  if (doacross_tile_size==INT_MAX)
    return (double)0.0;

  if (doacross_tile_size==0)
    return (double)DBL_MAX;

  INT orig_depth=permutation[parallel_depth+1-outer_depth];

  WN* loop=wn_outer;
  for (INT i=0; i<orig_depth; i++) {
    loop=Get_Only_Loop_Inside(loop, TRUE);
  }

  INT64 num_iter = Get_Do_Loop_Info(loop)->Est_Num_Iterations;

  BOOL need_skew = TRUE;
  if (sync_distances[0]>=doacross_tile_size &&
      sync_distances[1]>=doacross_tile_size)
    need_skew=FALSE;

  double delay_cycles;

  if (need_skew) {
    double cycle_per_tile=
	(double)machine_cycles*(double)doacross_tile_size/
	(double)(num_iter)+Single_Sync_Cycle;

    delay_cycles= cycle_per_tile * (double)(num_proc-1);
  } else
    delay_cycles= 0.0;

  return delay_cycles;
}

//-----------------------------------------------------------------------
// NAME: Compute_Doacross_Sync_Cycle
// FUNCTION:
//	Compute the cycle counts for inter-processor communacations
//	(i.e., synchronizations) for doacross parallelization
// ARGUMENTS:
//	'wn_outer' is the outer most loop in the SNL
//	'permutation' determines the loop order after permutation
//	'parallel_depth' gives the loop to be MP-tiled for doacross
//	'doacross_tile_size' is the tile size for synchronization
//-----------------------------------------------------------------------

extern double Compute_Doacross_Sync_Cycle(
			WN* wn_outer,
			INT permutation[],
			INT parallel_depth,
			INT doacross_tile_size,
			INT sync_distances[])
{
  INT outer_depth=Do_Loop_Depth(wn_outer);

  if (doacross_tile_size==INT_MAX)
    return (double)0.0;

  if (doacross_tile_size==0)
    return (double)DBL_MAX;

  INT orig_depth=permutation[parallel_depth+1-outer_depth];

  WN* loop=wn_outer;
  for (INT i=0; i<orig_depth; i++) {
    loop=Get_Only_Loop_Inside(loop, TRUE);
  }

  INT num_syncs=0;
  if (sync_distances[0]!=NULL_DIST)
    num_syncs++;
  if (sync_distances[1]!=NULL_DIST)
    num_syncs++;

  INT64 num_iter = Get_Do_Loop_Info(loop)->Est_Num_Iterations;

  INT num_tiles = num_iter / doacross_tile_size;
  if (num_iter % doacross_tile_size !=0)
    num_tiles++;
  double sync_cycles= Single_Sync_Cycle * (double)(num_syncs * num_tiles);

  return sync_cycles;
}

//-----------------------------------------------------------------------
// NAME: Depv_Carried_By_Outer_Loop
// FUNCTION:
//	Check if a dependence is carried by an outer loop
//	synchronizations
// ARGUMENTS:
//	'depv' -- the dependence vector to be checked
//	'level' -- the inner loop level
//-----------------------------------------------------------------------

extern BOOL Depv_Carried_By_Outer_Loop(
			DEPV* depv,
			INT level)
{
  for (INT i=0; i<level; i++) {
    DEP dep=DEPV_Dep(depv,i);
    DIRECTION dir=DEP_Direction(dep);
    if (dir==DIR_POS)
      return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Dep_Preserved
// FUNCTION:
//	Check if a dependence is preserved by a set of doacross
//	synchronizations or it is carried by another (serial loop)
// ARGUMENTS:
//	'depv' -- the dependence vector to be checked
//	'sync_distances' gives up to 2 sync vectors
// NOTE:
//	In addition to the two sync vectors, (1,0) is automatically
//	preserved because of the way the scheduling is done
//	(i.e., block scheduling)
//-----------------------------------------------------------------------

extern BOOL Dep_Preserved(
			DEPV* depv,
			INT doacross_dim,
			INT sync_distances[2])
{

  if (Depv_Carried_By_Outer_Loop(depv, doacross_dim))
    return TRUE;

  DEP dep1=DEPV_Dep(depv,doacross_dim+1);
  DEP dep2=DEPV_Dep(depv,doacross_dim);

  INT v0=sync_distances[0];
  INT v1=sync_distances[1];
  INT d1;
  INT d2;

  if (DEP_IsDistance(dep1))
    d1=DEP_Distance(dep1);
  else
    d1=DEP_DistanceBound(dep1);

  if (d1<0)
    return FALSE;

  if (DEP_IsDistance(dep2))
    d2=DEP_Distance(dep2);
  else {
    DIRECTION dir=DEP_Direction(dep2);
    switch (dir) {
      case DIR_EQ: return TRUE;
      case DIR_POS:
      case DIR_POSEQ: if (v1==0) return TRUE; else return FALSE;
      case DIR_NEG:
      case DIR_NEGEQ: if (v0==0) return TRUE; else return FALSE;
      case DIR_POSNEG:
      case DIR_STAR: return FALSE;
    }
  }

  if (d2==0)
    return TRUE;
  else if (d2<0) {
    if (v0==NULL_DIST)
      return FALSE;
    else if ( -d2 * v0 <= d1 )
      return TRUE;
    else
      return FALSE;
  } else {
    if (v1==NULL_DIST)
      return FALSE;
    else if ( d2 * v1 <= d1 )
      return TRUE;
    else
      return FALSE;
  }
}

extern BOOL Check_Doacross_Sync_Coverage(
			WN* doacross_loop,
			INT sync_distances[2])
{

    MEM_POOL_Push(&LNO_local_pool);

    ARRAY_DIRECTED_GRAPH16* adg=Array_Dependence_Graph;

    BOOL ok=TRUE;
    DO_LOOP_INFO *dli=Get_Do_Loop_Info(doacross_loop);

    INT doacross_depth=Do_Loop_Depth(doacross_loop);

    // first we collect all scalar references in the loop
    REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                  &LNO_local_pool);
    REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                 &LNO_local_pool);
    SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                       &LNO_local_pool);
    SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                      &LNO_local_pool);
    SCALAR_REF_STACK *params =
       CXX_NEW(SCALAR_REF_STACK(&LNO_local_pool), &LNO_local_pool);
    DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                &LNO_local_pool);
    Build_Doloop_Stack(doacross_loop, stack);
    Init_Ref_Stmt_Counter();
    INT32 status=New_Gather_References(doacross_loop,writes,reads,stack,
       scalar_writes,scalar_reads,params,&LNO_local_pool,
       Gather_Array_Refs);
    if (status == -1)
      return FALSE;

    REF_LIST_STACK *ref_list_stack[2];
    ref_list_stack[0]=reads;
    ref_list_stack[1]=writes;

    // examine all read and write refs
    for (INT ii=0; ii<2; ii++) {
  
      for (INT i=0;i<ref_list_stack[ii]->Elements(); i++) {
 
        REFERENCE_ITER iter(ref_list_stack[ii]->Bottom_nth(i));
        for (REFERENCE_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {
  
          WN* ref=n->Wn;
	  if (Is_Privatizable_With_Context(doacross_loop,ref,TRUE))
	  continue;

          VINDEX16 array_v=adg->Get_Vertex(ref);
	  if (array_v==0) {
	    DevWarn("Found array ref without vertex\n");
	    ok=FALSE;
	    continue;
	  } else {
	    EINDEX16 in_edge=adg->Get_In_Edge(array_v);
            while (in_edge) {
	      WN* source_wn=adg->Get_Wn(adg->Get_Source(in_edge));
	      if (Wn_Is_Inside(source_wn,doacross_loop) &&
		  (red_manager == NULL ||
		   red_manager->Which_Reduction(source_wn) !=
		   red_manager->Which_Reduction(ref))) {
  
	        DEPV_ARRAY* depv_array=adg->Depv_Array(in_edge);
	        if (depv_array->Num_Dim()>=doacross_depth) {
		  for (INT i=0; i<depv_array->Num_Vec(); i++)
		    if (!Dep_Preserved(depv_array->Depv(i),
			doacross_depth-depv_array->Num_Unused_Dim(),
			sync_distances)) {
	  		DevWarn("Array dep not preserved by doacross sync\n");
			ok=FALSE;
		    }
		}
	      }
              in_edge = adg->Get_Next_In_Edge(in_edge);
            }
	    EINDEX16 out_edge=adg->Get_Out_Edge(array_v);
            while (out_edge) {
	      WN* sink_wn=adg->Get_Wn(adg->Get_Sink(out_edge));
	      if (Wn_Is_Inside(sink_wn,doacross_loop) &&
		  (red_manager == NULL ||
		   red_manager->Which_Reduction(sink_wn) !=
		   red_manager->Which_Reduction(ref))) {

	        DEPV_ARRAY* depv_array=adg->Depv_Array(out_edge);
	        if (depv_array->Num_Dim()>=doacross_depth) {
		  for (INT i=0; i<depv_array->Num_Vec(); i++)
		    if (!Dep_Preserved(depv_array->Depv(i),
			doacross_depth-depv_array->Num_Unused_Dim(),
			sync_distances)) {
	  		DevWarn("Array dep not preserved by doacross sync\n");
			ok=FALSE;
		    }
	        }
	      }
              out_edge = adg->Get_Next_Out_Edge(out_edge);
            }
	  }
        }
      }
    }

    MEM_POOL_Pop(&LNO_local_pool);
    return ok;
}

