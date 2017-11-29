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

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.fiz_fuse.cxx $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <limits.h>  
#include "pu_info.h"
#include "lnoutils.h"
#include "defs.h"
#include "stab.h"
#include "fission.h"
#include "fusion.h"
#include "fiz_fuse.h"
#include "lwn_util.h"
#include "wn_map.h"
#include "ff_utils.h"
#include "lnopt_main.h"
#include "glob.h"
#include "tlog.h"
#include "whirl2src.h"
#include "parallel.h"

typedef HASH_TABLE<WN*,INT> WN2INT;
typedef enum { INFO, FAIL, SUCCEED } INFO_TYPE;

static void fiz_fuse_analysis_info(
  INFO_TYPE     info_type,
  SRCPOS        srcpos1,
  SRCPOS        srcpos2,
  UINT32        level,
  const char*         message)
{
  switch (info_type) {
    case INFO:
      fprintf(LNO_Analysis,"( LNO_Fiz_Fuse_Info ");
      break;
    case FAIL:
      fprintf(LNO_Analysis,"( LNO_Fiz_Fuse_Failure ");
      break;
    case SUCCEED:
      fprintf(LNO_Analysis,"( LNO_Fiz_Fuse_Success ");
      break;
  }

  fprintf(LNO_Analysis,"(%s %d) (%s %d) %d \"%s\")\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos1),
    Cur_PU_Name, Srcpos_To_Line(srcpos2),
    level, message);

  //if (Tlog_File) {
    //fprintf(Tlog_File,"LNO fiz_fuse\n");
    //fprintf(Tlog_File,"{ %d %d %d %d }\n",
            //info_type, Srcpos_To_Line(srcpos1), Srcpos_To_Line(srcpos2),
            //level);
    //fprintf(Tlog_File,"{ }\n");
    //fprintf(Tlog_File,"{ \"%s\" }\n", message);
    //fflush(Tlog_File);
  //}
}

static void fiz_fuse_tlog_info(
  INFO_TYPE     info_type,
  SRCPOS        srcpos1,
  SRCPOS        srcpos2,
  UINT32        level,
  const char*   message)
{
  char tmp_string[300];
  sprintf(tmp_string,"%d %d %d %d",
            info_type, Srcpos_To_Line(srcpos1), Srcpos_To_Line(srcpos2),
            level);
  Generate_Tlog("LNO","fiz_fuse", Srcpos_To_Line(srcpos1), "",
          tmp_string, "", message);
}

SNL_INFO::SNL_INFO(WN* loop) {
  _type=Invalid; _wn=loop; _depth=0;
  OPCODE opc=WN_opcode(loop);
  if (opc==OPC_DO_LOOP) {
    WN* last_loop=NULL;
    while (loop && (Do_Loop_Is_Good(loop) && !Do_Loop_Has_Exits(loop))) {
      _depth++;
      last_loop=loop;
      loop=Get_Only_Loop_Inside(loop,FALSE);
    }
    if (last_loop)
     if (Do_Loop_Is_Inner(last_loop))
        _type=Inner;
      else
        _type=Not_Inner;
    else
      _type=Non_SNL;
  } else if (opc==OPC_IF || opc==OPC_WHILE_DO || opc==OPC_DO_WHILE ||
	OPCODE_is_non_scf(opc) || opc==OPC_LABEL) {
    _type=Non_SNL;
  }
}

void FIZ_FUSE_INFO::Print(INT i, FILE* fp) {

  if (_snl_info[i]._type==Invalid)
    return;
  if (WN_opcode(_snl_info[i]._wn)==OPC_IF)
    fprintf(fp,"Print FIZ_FUSE_INFO for SNL %3d : is a IF structure\n", i);
  else if (WN_opcode(_snl_info[i]._wn)==OPC_REGION)
    fprintf(fp,
      "Print FIZ_FUSE_INFO for SNL %3d : is a REGION structure\n", i);
  else if (WN_opcode(_snl_info[i]._wn)==OPC_DO_WHILE)
    fprintf(fp,
      "Print FIZ_FUSE_INFO for SNL %3d : is a DO WHILE structure\n", i);
  else if (WN_opcode(_snl_info[i]._wn)==OPC_WHILE_DO)
    fprintf(fp,
      "Print FIZ_FUSE_INFO for SNL %3d : is a WHILE DO structure\n", i);
  else {

    ST *st;
    const char* name;
    st=WN_st(WN_index(_snl_info[i]._wn));
    if (ST_class(st) == CLASS_PREG)
      name=Preg_Name(WN_offset(WN_index(_snl_info[i]._wn)));
    else
      name=ST_name(st);

    fprintf(fp,"Print FIZ_FUSE_INFO for SNL %3d (%s): Depth %3d, Type %2d\n",
          i,name,_snl_info[i]._depth, _snl_info[i]._type);
    WN* loop=_snl_info[i]._wn;
    WN* func_nd=LWN_Get_Parent(loop);
    while (func_nd && WN_opcode(func_nd)!=OPC_FUNC_ENTRY)
      func_nd=LWN_Get_Parent(func_nd);
    Is_True(func_nd,("SNL loops not in a function?"));
    Whirl2Src_Init(func_nd);
    char indent[80];
    indent[0]='\0';
    for (INT j=0; j<_snl_info[i]._depth; j++) {

      st=WN_st(WN_index(loop));
      if (ST_class(st) == CLASS_PREG)
        name=Preg_Name(WN_offset(WN_index(loop)));
      else
        name=ST_name(st);

      fprintf(fp,"Line %5d %sDO %s=",
        Srcpos_To_Line(WN_Get_Linenum(loop)), indent, name);
      indent[j*2]=' ';
      indent[j*2+1]=' ';
      indent[j*2+2]='\0';
      Whirl2Src_Emit(fp,WN_kid0(WN_start(loop)));
      fprintf(fp,",");
      Whirl2Src_Emit(fp,WN_end(loop));
      fprintf(fp,",");
      Whirl2Src_Emit(fp,WN_kid1(WN_kid0(WN_step(loop))));
      fprintf(fp,"\n");
      loop=Get_Only_Loop_Inside(loop,FALSE);
    }
  }
}

void FIZ_FUSE_INFO::Check() {
  INT total_snl=_snl_info.Elements();
  WN2INT *snl_info_table=CXX_NEW(WN2INT(total_snl*5,_mpool),_mpool);
  for (INT i=0; i<total_snl; i++) {
    INT depth=_snl_info[i]._depth;
    SNL_TYPE type=_snl_info[i]._type;
    WN* wn=_snl_info[i]._wn;
    WN* outer_loop=NULL;
    if (type==Not_Inner || type==Inner) {
      for (INT j=0; j<depth; j++) {
        if (wn==NULL) {
          DevWarn("Not enough SNL level %d < %d in SNL info (%d), fixed",
                  j, depth, i);
          if (j==0)
            _snl_info[i]._type=Invalid;
          else {
            _snl_info[i]._depth=j;
            if (outer_loop)
              if (Find_SCF_Inside(outer_loop, OPC_DO_LOOP))
                _snl_info[i]._type=Not_Inner;
              else
                _snl_info[i]._type=Inner;
          }
          break;
        }
        INT old_index=snl_info_table->Find(wn);
        if (old_index) {
          DevWarn("Duplicate found for loop <0x%p> in SNL info (%d) and (%d), fixed",
            wn, old_index, i);
          if (j==0)
            _snl_info[i]._type=Invalid;
          else {
            _snl_info[i]._depth=j;
            if (outer_loop)
              if (Find_SCF_Inside(outer_loop,OPC_DO_LOOP))
                _snl_info[i]._type=Not_Inner;
              else
                _snl_info[i]._type=Inner;
          }
          break;
        }
        snl_info_table->Enter(wn,i);
        outer_loop=wn;
        if (j<depth-1)
          wn=Get_Only_Loop_Inside(wn,FALSE);
        else if (type==Inner && Find_SCF_Inside(wn,OPC_DO_LOOP)) {
          DevWarn("Type Inner SNL has other loops inside in SNL info (%d), fixed", i);
          _snl_info[i]._type=Not_Inner;
          break;
        }
      }
    }
  }
}

void FIZ_FUSE_INFO::Build(WN* root, BOOL all_loops)
{

  WN2INT *loop_table=CXX_NEW(WN2INT(1024,_mpool),_mpool);
  UINT snl_id=_snl_info.Newidx();
  _snl_info[snl_id].Init();

  WN_ITER* wn_walker=WN_WALK_SCFIter(root);
  while (WN_WALK_SCFNext(wn_walker)) {
    WN* wn=WN_ITER_wn(wn_walker);
    if (WN_opcode(wn)==OPC_DO_LOOP && loop_table->Find(wn)==0) {
      if (!all_loops && (!Do_Loop_Is_Good(wn) || Do_Loop_Has_Exits(wn) || 
						Do_Loop_Is_Mp(wn)))
	continue; 
      snl_id=_snl_info.Newidx();
      loop_table->Enter(wn,snl_id);
      UINT level=1;
      WN* inner_most_loop=wn;
      WN* inner_loop=Get_Only_Loop_Inside(wn,FALSE);
      while (inner_loop) {
        Is_True(loop_table->Find(inner_loop)==0, ("Strange traversal order"));
        loop_table->Enter(inner_loop,snl_id);
        level++;
        inner_most_loop=inner_loop;
        inner_loop=Get_Only_Loop_Inside(inner_loop,FALSE);
      }
      if (Find_SCF_Inside(inner_most_loop,OPC_DO_LOOP))
        _snl_info[snl_id]._type=Not_Inner;
      else
        _snl_info[snl_id]._type=Inner;
      _snl_info[snl_id]._depth=level;
      _snl_info[snl_id]._wn=wn;
    }
  }

}

extern FIZ_FUSE_INFO*
If_While_Region_Fiz_Fuse(WN* wn, FIZ_FUSE_INFO* snls, MEM_POOL* mpool) {

  FIZ_FUSE_INFO *rval;

  rval = CXX_NEW(FIZ_FUSE_INFO(mpool), mpool);
  rval->New_Snl(wn,0,Non_SNL);

  WN* bodies[2];
  bodies[0]=bodies[1]=NULL;

  OPCODE      opc = WN_opcode(wn);
  IF_INFO *ii;
  switch (opc) {
    case OPC_IF:
      ii=Get_If_Info(wn);
      if (!ii->Contains_Do_Loops)
        return rval;
      bodies[0]=WN_then(wn);
      bodies[1]=WN_else(wn);
      break;
    case OPC_REGION:
      bodies[0]=WN_region_body(wn);
      break;
    case OPC_DO_WHILE:
    case OPC_WHILE_DO:
      bodies[0]=WN_while_body(wn);
      break;
    default:
      Is_True(0, ("Invalid WN in If_While_Region_Fiz_Fuse."));
      return rval;
  }

  MEM_POOL_Push(&LNO_local_pool);

  // process all children of each body
  for (INT i=0; i<2 && bodies[i]!=NULL; i++)
    for (WN* wn1=WN_first(bodies[i]); wn1; ) {
      WN* next_wn=WN_next(wn1);
      opc=WN_opcode(wn1);
      if (opc==OPC_DO_LOOP && !Do_Loop_Is_Mp(wn1))
        *snls += *Fiz_Fuse(wn1, snls, &LNO_default_pool);
      else if (opc == OPC_IF || opc==OPC_REGION ||
               opc==OPC_DO_WHILE || opc==OPC_WHILE_DO)
        (void)If_While_Region_Fiz_Fuse(wn1, snls, &LNO_default_pool);
      wn1=next_wn;
    }

  MEM_POOL_Pop(&LNO_local_pool);
  return rval;

}

static void
fast_fuse_check_msg(const char *msg, WN *lp1, WN *lp2, UINT level)
{
  char   buf[200];
  SRCPOS srcpos1;
  SRCPOS srcpos2;

  if ( LNO_Verbose || LNO_Analysis || LNO_Tlog ) {
    sprintf(buf, "fast_fuse_check_msg %s", msg);
    srcpos1 = WN_Get_Linenum(lp1);
    srcpos2 = WN_Get_Linenum(lp2);
  }
  if (LNO_Verbose) {
    printf("#### fiz_fuse(%d+%d:%d): %s\n",
	   Srcpos_To_Line(srcpos1), Srcpos_To_Line(srcpos2), level, buf);
  }
  if (LNO_Analysis) {
    fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, level, buf);
  }
  if ( LNO_Tlog ) {
    fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, level, buf);
  }
}

// Return FALSE only if full fusion w/o peeling MUST fail.
// When in doubt return TRUE.
//   This is a mostly a compile-time optimization.
//   In rare cases re-fision fails.  Its better to do nothing then
//   to fail to revert to the original SNL form.
//   pv 547998 inspired this check (inner loop trip count incompat.).
static BOOL
fast_fuse_check_ok(WN *loop1, WN *loop2, UINT fusion_levels)
{
  WN *lp1 = loop1;
  WN *lp2 = loop2;

  for (INT j = 0; j < fusion_levels; j++) {

    FmtAssert(WN_opcode(lp1)==OPC_DO_LOOP,
	      ("lp1 non-loop in fast_fuse_check_ok()\n"));
    FmtAssert(WN_opcode(lp2)==OPC_DO_LOOP, 
	      ("lp2 non-loop in fast_fuse_check_ok()\n"));

    INT64 trips1 = Iterations(lp1, &LNO_default_pool);
    INT64 trips2 = Iterations(lp2, &LNO_default_pool);

    if ( trips1 != trips2 ) {  // mix const/symbolic or diff const.
      fast_fuse_check_msg("FALSE: const trips different.", lp1, lp2, j);
      return FALSE;
    }
    if ( trips1 == -1 ) {  // and trips2 == -1; both non-const.
      
      DO_LOOP_INFO* dli1 = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, lp1);
      DO_LOOP_INFO* dli2 = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, lp2);

      if ( dli1->No_Fusion || dli2->No_Fusion ) {
	fast_fuse_check_msg("FALSE: No_Fusion.", lp1, lp2, j);
	return FALSE;
      }
      if ( !Do_Loop_Is_Good(lp1) || !Do_Loop_Is_Good(lp2) ) {
	fast_fuse_check_msg("FALSE: not Good.", lp1, lp2, j);
	return FALSE;
      }
      if ( Do_Loop_Has_Calls(lp1) || Do_Loop_Has_Calls(lp2) ) {
	fast_fuse_check_msg("FALSE: has Calls.", lp1, lp2, j);
	return FALSE;
      }
      if ( Do_Loop_Has_Gotos(lp1) || Do_Loop_Has_Gotos(lp2) ) {
	fast_fuse_check_msg("FALSE: has Gotos.", lp1, lp2, j);
	return FALSE;
      }
      if ( !dli1->Step->Is_Const() || !dli2->Step->Is_Const() ) {
	fast_fuse_check_msg("TRUE: non-const step.", lp1, lp2, j);
	return TRUE;   // leave this for full test.
      }
      ACCESS_VECTOR *step_diff = Subtract(dli1->Step, dli2->Step,
					  &LNO_default_pool);

      if ( !step_diff->Is_Const() || step_diff->Const_Offset != 0 ) {
	fast_fuse_check_msg("TRUE: diff steps.", lp1, lp2, j);
	return TRUE;   // leave this for full test.
      }
      if ( Upper_Bound_Standardize( WN_end(lp1), TRUE ) == FALSE ) {
	fast_fuse_check_msg("FALSE: lp1 upper bnd not std.", lp1, lp2, j);
	return FALSE;
      }
      if ( Upper_Bound_Standardize( WN_end(lp2), TRUE ) == FALSE ) {
	fast_fuse_check_msg("FALSE: lp2 upper bnd not std.", lp1, lp2, j);
	return FALSE;
      }
      BOOL           step_pos  = (dli1->Step->Const_Offset > 0);
      BOOL           ident_exp = FALSE;
      ACCESS_VECTOR *lb_diff   = NULL;
      ACCESS_VECTOR *ub_diff   = NULL;

      if ( step_pos ) {
	if ( Compare_Bounds(WN_start(lp1), WN_index(lp1),
			    WN_start(lp2), WN_index(lp2)) ==0 ) {
	  ident_exp = TRUE;
	} else if ( Bound_Is_Too_Messy(dli1->LB) || dli1->LB->Num_Vec()!=1 ||
		    Bound_Is_Too_Messy(dli2->LB) || dli2->LB->Num_Vec()!=1 ){
	  fast_fuse_check_msg("FALSE: messy lower bnd.", lp1, lp2, j);
	  return FALSE;
	} else {
	  lb_diff = Subtract( dli1->LB->Dim(0), dli2->LB->Dim(0),
			      &LNO_default_pool );
	}
      } else {
	if ( Compare_Bounds(WN_end(lp1), WN_index(lp1),
			    WN_end(lp2), WN_index(lp2)) == 0 ) {
	  ident_exp=TRUE;
	} else if ( Bound_Is_Too_Messy(dli1->UB) || dli1->UB->Num_Vec()!=1 ||
		    Bound_Is_Too_Messy(dli2->UB) || dli2->UB->Num_Vec()!=1 ){
	  fast_fuse_check_msg("FALSE: messy upper bnd.", lp1, lp2, j);
	  return FALSE;
	} else {
	  lb_diff = Subtract( dli1->UB->Dim(0), dli2->UB->Dim(0),
			      &LNO_default_pool );
	}
      }
      if ( ident_exp ) {
	lb_diff = NULL;
      } else if ( !lb_diff->Is_Const() ) {
	fast_fuse_check_msg("FALSE: low bnd diff not const.", lp1, lp2, j);
	return FALSE;
      } else if ( lb_diff->Const_Offset ) {
	fast_fuse_check_msg("TRUE: low bnd diff const != 0.", lp1, lp2, j);
	return TRUE;  // leave for full test.
      }

      ident_exp=FALSE;
      if ( step_pos ) {
	if ( Compare_Bounds(WN_end(lp1), WN_index(lp1),
			    WN_end(lp2), WN_index(lp2)) == 0 ) {
	  ident_exp=TRUE;
	} else if ( Bound_Is_Too_Messy(dli1->UB) || dli1->UB->Num_Vec()!=1 ||
		    Bound_Is_Too_Messy(dli2->UB) || dli2->UB->Num_Vec()!=1 ){
	  fast_fuse_check_msg("FALSE: messy upper bnd.", lp1, lp2, j);
	  return FALSE;
	} else {
	  ub_diff = Subtract( dli1->UB->Dim(0), dli2->UB->Dim(0),
			      &LNO_default_pool );
	}
      } else {
	if ( Compare_Bounds(WN_start(lp1), WN_index(lp1),
			    WN_start(lp2), WN_index(lp2)) == 0 ) {
	  ident_exp=TRUE;
	} else if ( Bound_Is_Too_Messy(dli1->LB) || dli1->LB->Num_Vec()!=1 ||
		    Bound_Is_Too_Messy(dli2->LB) || dli2->LB->Num_Vec()!=1 ){
	  fast_fuse_check_msg("messy lower bnd.", lp1, lp2, j);
	  return FALSE;
	} else {
	  ub_diff = Subtract( dli1->LB->Dim(0), dli2->LB->Dim(0),
			      &LNO_default_pool );
	}
      }
      if ( ident_exp ) {
	ub_diff = NULL;
      } else if ( !ub_diff->Is_Const() ) {
	fast_fuse_check_msg("FALSE: upper bnd diff not const.", lp1, lp2, j);
	return FALSE;
      } else if ( ub_diff->Const_Offset != 0 ) {
	fast_fuse_check_msg("TRUE: upper bnd diff const != 0.", lp1, lp2, j);
	return TRUE;  // leave for full test.
      }
    }
    lp1 = Get_Only_Loop_Inside(lp1, FALSE);
    lp2 = Get_Only_Loop_Inside(lp2, FALSE);
  }
  fast_fuse_check_msg("pass all checks", lp1, lp2, 0);
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Version_Loop
// FUNCTION: Create two identical versions of 'wn_loop' under a bogus if
//   test.  
//-----------------------------------------------------------------------

static WN* Version_Loop(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  REDUCTION_MANAGER* rm = red_manager;
  WN_MAP version_map = WN_MAP_Create(&LNO_local_pool);
  WN* wn_copy = LWN_Copy_Tree(wn_loop, TRUE, LNO_Info_Map, TRUE, version_map);
  BOOL all_internal = WN_Rename_Duplicate_Labels(wn_loop, wn_copy,
    Current_Func_Node, &LNO_local_pool);
  Is_True(all_internal, ("external labels renamed"));

  // Clone the dependences for the scalar copy.
  WN* wn_array[2];
  wn_array[0] = wn_loop;
  wn_array[1] = wn_copy;
  Unrolled_DU_Update(wn_array, 2, Do_Loop_Depth(wn_loop) - 1, TRUE, FALSE);
  dg->Versioned_Dependences_Update(wn_loop, wn_copy, Do_Loop_Depth(wn_loop),
    version_map);
  WN_MAP_Delete(version_map);
  if (rm != NULL)
    rm->Unroll_Update(wn_array, 2);

  // Start with a condition of .TRUE. for the version test
  WN* wn_total_cond = LWN_Make_Icon(Boolean_type, 1);
  LWN_Extract_From_Block(wn_loop);
  WN* wn_if = LWN_CreateIf(wn_total_cond, WN_CreateBlock(), WN_CreateBlock());
  LWN_Insert_Block_After(WN_then(wn_if), NULL, wn_loop);
  LWN_Insert_Block_After(WN_else(wn_if), NULL, wn_copy);
  WN_Set_Linenum(wn_if, WN_Get_Linenum(wn_loop));
  IF_INFO *ii =
    CXX_NEW(IF_INFO(&LNO_default_pool, TRUE, TRUE), &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_if, (void *) ii);
  DOLOOP_STACK *stack = CXX_NEW(DOLOOP_STACK(&LNO_default_pool),
    &LNO_default_pool);
  Build_Doloop_Stack(wn_if, stack);
  LNO_Build_If_Access(wn_if, stack);
  return wn_if;
}

//-----------------------------------------------------------------------
// NAME: Version_and_Splice_Loop
// FUNCTION: Create two identical versions of 'wn_loop' under an if-test,
//   and splice them into the program graph.  
//-----------------------------------------------------------------------

static WN* Version_and_Splice_Loop(WN* wn_loop)
{ 
  WN* wn_parent = LWN_Get_Parent(wn_loop);
  WN* wn_prev = WN_prev(wn_loop);
  WN* wn_if = Version_Loop(wn_loop);
  LWN_Insert_Block_After(wn_parent, wn_prev, wn_if);
  return wn_if; 
} 

//-----------------------------------------------------------------------
// NAME: Version_Loop_Pair
// FUNCTION: Create two identical versions of the pair of candidate fusion
//   loops 'wn_loop1' and 'wn_loop2' under a bogus IF test.  The THEN part
//   will be transformed, and the ELSE part will save the original in case
//   it needs to be restored. 
//-----------------------------------------------------------------------

static WN* Version_Loop_Pair(WN* wn_loop1, 
			     WN* wn_loop2)
{
  WN* wn_if1 = Version_and_Splice_Loop(wn_loop1);
  WN* wn_if2 = Version_and_Splice_Loop(wn_loop2);
  WN* wn_then = WN_first(WN_then(wn_if2));
  LWN_Extract_From_Block(wn_then);
  WN* wn_thenlp = WN_first(WN_then(wn_if1));   
  LWN_Insert_Block_After(LWN_Get_Parent(wn_thenlp), wn_thenlp, wn_then);
  WN* wn_else = WN_first(WN_else(wn_if2));
  LWN_Extract_From_Block(wn_else);
  WN* wn_elselp = WN_first(WN_else(wn_if1));   
  LWN_Insert_Block_After(LWN_Get_Parent(wn_elselp), wn_elselp, wn_else);
  LWN_Delete_Tree(wn_if2);
  return wn_if1; 
} 

//-----------------------------------------------------------------------
// NAME: Extract_Branch
// FUNCTION: Extract the IF or ELSE branch 'wn_start' of the IF test 'wn_if',
//   throwing away the other branch.
//-----------------------------------------------------------------------

static void Extract_Branch(WN* wn_if,
                           WN* wn_start)
{ 
  WN* wnn = NULL; 
  for (WN* wn = WN_first(wn_start); wn != NULL; wn = wnn) { 
    wnn = WN_next(wn);
    LWN_Extract_From_Block(wn);
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_if), wn_if, wn);
  } 
  LWN_Delete_Tree(wn_if);
} 

//-----------------------------------------------------------------------
// NAME: Save_Corresponding_Loops
// FUNCTION: The two branch of 'wn_if' should have identical copies of 
//   code.  Save a mapping between the loops in the THEN branch and the 
//   loops in the ELSE branch.  Those in the THEN branch will appear in 
//   'loop_list_one'.  Those in the ELSE branch will appear in 
//   'loop_list_two'.
//-----------------------------------------------------------------------

static void Save_Corresponding_Loops(WN* wn_if,
				     DYN_ARRAY<WN*>* loop_list_one, 
				     DYN_ARRAY<WN*>* loop_list_two)
{ 
  WN* wn_block1 = WN_then(wn_if);
  WN* wn_block2 = WN_else(wn_if);
  WN* wn2 = WN_first(wn_block2); 
  for (WN* wn1 = WN_first(wn_block1); wn1 != NULL; wn1 = WN_next(wn1)) { 
    LWN_ITER* itr1 = LWN_WALK_TreeIter(wn1);
    LWN_ITER* itr2 = LWN_WALK_TreeIter(wn2);
    for (; itr1 != NULL; itr1 = LWN_WALK_TreeNext(itr1),
	itr2 = LWN_WALK_TreeNext(itr2)) {
      WN* wn_one = itr1->wn;
      WN* wn_two = itr2->wn;
      if (WN_operator(wn_one) == OPR_DO_LOOP) { 
	FmtAssert(WN_operator(wn_two) == OPR_DO_LOOP, 
	  ("Save_Corresponding_Loops: Nodes do not correspond"));
	loop_list_one->AddElement(wn1);
	loop_list_two->AddElement(wn2);
      } 
    } 
    wn2 = WN_next(wn2); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Replace_Corresponding_Loops
// FUNCTION: For each loop on the 'ffi' which appears on 'loop_list_one',
//   replace it with the corresponding loop on 'loop_list_two'. 
//-----------------------------------------------------------------------

static void Replace_Corresponding_Loops(FIZ_FUSE_INFO* ffi, 
					DYN_ARRAY<WN*>* loop_list_one,
					DYN_ARRAY<WN*>* loop_list_two)
{ 
  for (INT i = 0; i < ffi->Num_Snl(); i++) { 
    WN* wn_old = ffi->Get_Wn(i);
    INT j;
    for (j = 0; j <= loop_list_one->Lastidx(); j++)  
      if ((*loop_list_one)[j] == wn_old)
	break; 
    if (j <= loop_list_one->Lastidx())
      ffi->Set_Wn(i, (*loop_list_two)[j]);
  } 
} 

extern FISSION_FUSION_STATUS
       Fuse_Level_By_Level(WN* loop1, WN* loop2, UINT *fusion_level_io,
       UINT peeling_limit,
       BOOL allow_partial_fusion, BOOL allow_outer_peeling,
       FIZ_FUSE_INFO* ffi) {
  
  UINT fusion_level= *fusion_level_io;
  UINT fused_level=0;
  WN** fused_loop2=CXX_NEW_ARRAY(WN*, fusion_level, &LNO_local_pool);

  FISSION_FUSION_STATUS fusion_status;
  DYN_ARRAY<WN*> loop_list_one(&LNO_local_pool);
  DYN_ARRAY<WN*> loop_list_two(&LNO_local_pool);

  if (    fusion_level > 1
       && !allow_partial_fusion
       && !allow_outer_peeling
       && !fast_fuse_check_ok(loop1, loop2, fusion_level) ) {
    *fusion_level_io = 0;
    return Failed;          // failed basic compatibility check.
  }

  WN* wn_if = Version_Loop_Pair(loop1, loop2);
  Save_Corresponding_Loops(wn_if, &loop_list_one, &loop_list_two);

  for (INT j=0; j< fusion_level; j++) {

    if (Move_Adjacent(loop1,loop2, FALSE)==FALSE) {
      fusion_status=Failed;
      break;
    }
    UINT tmp_peeling_limit=peeling_limit;
    if (j< fusion_level-1)
      if (allow_outer_peeling && j==0)
        tmp_peeling_limit=INT32_MAX-1;
      else
        tmp_peeling_limit=0;
    fusion_status=Fuse(loop1, loop2, 1, tmp_peeling_limit, j==fusion_level-1);
    if (fusion_status==Succeeded) {
      fused_level++;
      if (j< fusion_level-1) {
        WN* body=WN_do_body(loop1);
        WN* wn=WN_first(body);
        while (WN_opcode(wn)!=OPC_DO_LOOP)
          wn=WN_next(wn);
        loop1=wn;
        wn=WN_next(wn);
        while (WN_opcode(wn)!=OPC_DO_LOOP)
          wn=WN_next(wn);
        loop2=wn;
        fused_loop2[j]=wn;
      }
    } else if (fusion_status==Succeeded_and_Inner_Loop_Removed) {
      // no more inner loops to be fused so break here
      break;
    } else	// Failed
      break;
  }
  if (fused_level==fusion_level) {
    // fuse successfully with level-by-level fusion
    *fusion_level_io=fused_level;
    Extract_Branch(wn_if, WN_then(wn_if));
    return fusion_status;
  } else if (fusion_status==Succeeded_and_Inner_Loop_Removed) {
    *fusion_level_io=fused_level;
    Extract_Branch(wn_if, WN_then(wn_if));
    return fusion_status;
  } else if (fused_level>0 && allow_partial_fusion) {
    *fusion_level_io=fused_level;
    Extract_Branch(wn_if, WN_then(wn_if));
    return Partially_fused;
  } else if (fused_level>0 && !allow_partial_fusion) {
    // otherwise, restore the loop back to before fusion
    *fusion_level_io = 0;
    Replace_Corresponding_Loops(ffi, &loop_list_one, &loop_list_two);
    Extract_Branch(wn_if, WN_else(wn_if));
    return Failed; 
  } 

  *fusion_level_io = 0;
  Replace_Corresponding_Loops(ffi, &loop_list_one, &loop_list_two);
  Extract_Branch(wn_if, WN_else(wn_if));
  return Failed;
}      

// Try fission and fusion upon a loop nest to create as many and as
// deeply simply nested SNL as possible.
// LNO_local_pool is used 

extern FIZ_FUSE_INFO* Fiz_Fuse(WN* loop, FIZ_FUSE_INFO* snls, MEM_POOL* mpool) {

  FIZ_FUSE_INFO *info, *rval;
  BOOL partition_based_fission = FALSE;
  BOOL fission_succeeded = FALSE;
  BOOL fusion_succeeded = FALSE;

  rval = CXX_NEW(FIZ_FUSE_INFO(mpool), mpool);

  MEM_POOL_Push(&LNO_local_pool);

  info = CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);

  // sloops records outer SNL loops
  DYN_ARRAY<WN*> sloops(&LNO_local_pool);

  WN* wn=loop;
  WN* wn1=loop;
  INT i;

  // first we go inside to reach the level where there is no single
  // child loop. I.e., the level where it is not simply-nested.
  INT level=0;
  while (wn) {
    
    if ((!Do_Loop_Is_Good(wn1) || 
	(Do_Loop_Has_Calls(wn1) || Do_Loop_Has_Gotos(wn1))) && 
       Do_Loop_Is_Good(wn) && 
	(!Do_Loop_Has_Calls(wn) && !Do_Loop_Has_Gotos(wn)))
      loop=wn;	// loop will point to the outermost good do loop

    if (Do_Loop_Is_Good(wn)&&!Do_Loop_Has_Calls(wn)&&!Do_Loop_Has_Gotos(wn)) {
      i=sloops.Newidx();
      sloops[i]=wn;
      level++;

    }

    wn1 = wn;
    wn=Get_Only_Loop_Inside(wn,FALSE);

    if (Do_Loop_Is_Good(wn1) && !Do_Loop_Has_Calls(wn1) && 
		!Do_Loop_Has_Gotos(wn1) &&
		!Do_Loop_Is_Inner(wn1))
      toplogical_reordering(wn1, level, Array_Dependence_Graph);
    
  }

  // now process all children loops by calling Fiz_Fuse
  for (wn=WN_first(WN_do_body(wn1)); wn;) {
    WN* next_stmt=WN_next(wn); // this is necessary because Fiz_Fuse
                 // may fission a loop stmt and create a new WN_next
    OPCODE opc=WN_opcode(wn);
    if (opc==OPC_DO_LOOP && !Do_Loop_Is_Mp(wn))
      *info += *Fiz_Fuse(wn, snls, &LNO_default_pool);
    else if (opc==OPC_IF || opc==OPC_REGION ||
             opc==OPC_DO_WHILE || opc==OPC_WHILE_DO)
      *info += *If_While_Region_Fiz_Fuse(wn, snls, &LNO_default_pool);
    wn=next_stmt;
  }

  if (!Do_Loop_Is_Good(loop) || Do_Loop_Has_Calls(loop) ||
	Do_Loop_Has_Gotos(loop)) {
    *snls += *info;
    sloops.Free_array();
    MEM_POOL_Pop(&LNO_local_pool);
    return rval;
  }

  // in case there is no child loop, we have just found an SNL
  // which is the input loop and the depth is number of enclosing loop
  if (info->Num_Snl()==0) {
    // put the outer loops in the returned structure
    if (Find_SCF_Inside(wn1,OPC_DO_LOOP))
      rval->New_Snl(loop,sloops.Lastidx()+1,Not_Inner);
    else
      rval->New_Snl(loop,sloops.Lastidx()+1,Inner);
    sloops.Free_array();
    MEM_POOL_Pop(&LNO_local_pool);
    return rval;
  }

  rval->New_Snl(loop,sloops.Lastidx()+1,Inner);
  INT relts = 1;
  WN* last_outer_loop=loop;
  INT last_loop=0;
  UINT last_fissioned_level=sloops.Lastidx()+1;
  UINT max_fissioned_level=0;

  for (i=1; i< info->Num_Snl(); i++) {
    INT d1 = info->Get_Depth(last_loop);
    INT d2 = info->Get_Depth(i);
    INT min_snl_level=MIN(d1,d2);
    INT peeling_limit= (d1==d2) ? LNO_Fusion_Peeling_Limit : 0;
    WN* wn1=info->Get_Wn(last_loop);
    WN* wn2=info->Get_Wn(i);
    SNL_TYPE type0= rval->Get_Type(relts-1);
    SNL_TYPE type1= info->Get_Type(last_loop);
    SNL_TYPE type2= info->Get_Type(i);
    WN* parent_wn=LWN_Get_Parent(LWN_Get_Parent(wn1));
    DO_LOOP_INFO* dli0=Get_Do_Loop_Info(parent_wn);
    DO_LOOP_INFO* dli1=NULL;
    DO_LOOP_INFO* dli2=NULL;
    SRCPOS srcpos1=WN_Get_Linenum(wn1);
    SRCPOS srcpos2=WN_Get_Linenum(wn2);


    if (!Do_Loop_Is_Good(rval->Get_Wn(relts-1)) ||
	 Do_Loop_Has_Gotos(rval->Get_Wn(relts-1)) ||
	 Do_Loop_Has_Calls(rval->Get_Wn(relts-1))) {
      // outer loops can become bad if the dep. graph capacity is exceeded
      snls->Copy_Snl(info,last_loop);
      for (INT j=i; j< info->Num_Snl(); j++) {
        snls->Copy_Snl(info,j);
      }
      break;
    }

    if (WN_opcode(wn1)==OPC_DO_LOOP)
      dli1=Get_Do_Loop_Info(wn1);
    if (WN_opcode(wn2)==OPC_DO_LOOP)
      dli2=Get_Do_Loop_Info(wn2);
    BOOL try_fusion = (LNO_Fusion!=0) &&
                      (((type0==Inner) && (type1==Inner) && (type2==Inner)) ||
                      ((type1==Not_Inner) && (type2==Not_Inner))) &&
                      (dli1 && !dli1->No_Fusion) &&
                      (dli2 && !dli2->No_Fusion) &&
                       min_snl_level>0		 &&
		       (!Do_Loop_Is_Mp(wn1) && !Do_Loop_Is_Mp(wn2)) &&
	(!Cannot_Concurrentize(wn1) && !Cannot_Concurrentize(wn2)
	||  Cannot_Concurrentize(wn1) && Cannot_Concurrentize(wn2)); 

    if ( try_fusion
	 && dli1->Parallelizable != dli2->Parallelizable
	 && !inside_parallelizable_loop( wn1 ) ) {

      if (LNO_Analysis) {
	fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, min_snl_level,
	 "don't fuse parallel with serial loop, unless inside parallel.");
      }
      if ( LNO_Tlog ) {
	fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, min_snl_level,
	 "don't fuse parallel with serial loop, unless inside parallel.");
      }
      try_fusion = FALSE;
    }
#ifdef KEY
    if ( try_fusion && LNO_Run_Simd > 0 && LNO_Simd_Avoid_Fusion &&
	 (dli1->Vectorizable ^ dli2->Vectorizable) ) {

      if (LNO_Analysis) {
	fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, min_snl_level,
	 "don't fuse vectorizable with serial loop.");
      }
      if ( LNO_Tlog ) {
	fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, min_snl_level,
	 "don't fuse vectorizable with serial loop.");
      }
      try_fusion = FALSE;
    }    
#endif

    BOOL prefer_fission = (LNO_Fission != 0) ? TRUE : FALSE;
    BOOL try_fission = prefer_fission && !dli0->No_Fission &&
       !Do_Loop_Is_Mp(Enclosing_Do_Loop(LWN_Get_Parent(wn1))) &&
			(type1 == Inner || type1 == Not_Inner) &&
       (type2 == Inner || type2 == Not_Inner) &&
       (((last_fissioned_level!=0) && (type1==Inner)) || (type2==Inner));
    BOOL cannot_fuse = FALSE;

    BOOL prefer_fusion = try_fusion &&
			((LNO_Fusion > LNO_Fission) ||
			 ((LNO_Fusion == LNO_Fission) && (d1==d2)));

    FISSION_FUSION_STATUS fusion_status=Failed;
    FISSION_FUSION_STATUS fission_status=Failed;

    if (prefer_fusion) {
      if (LNO_Analysis)
        fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, min_snl_level,
          "Attempted to fuse to create a deeper Simply Nested Loop");
      if ( LNO_Tlog )
        fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, min_snl_level,
          "Attempted to fuse to create a deeper Simply Nested Loop");

      // move the loops to be fused closer to each other
      if (Move_Adjacent(info->Get_Wn(last_loop), info->Get_Wn(i), FALSE)==TRUE)
        fusion_status= Fuse(wn1, wn2, min_snl_level,
			    peeling_limit,TRUE);
      else
        fusion_status= Failed;
      if (fusion_status==Succeeded || 
          fusion_status==Succeeded_and_Inner_Loop_Removed) {
        fusion_succeeded = TRUE;
        // successfully fused, continue to work on the next child loop
        if (LNO_Analysis)
          fiz_fuse_analysis_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");

        if (fusion_status==Succeeded_and_Inner_Loop_Removed)
          if (min_snl_level>1)
            info->Set_Depth(last_loop,min_snl_level-1);
          else { // both loops have disappeared
            info->Set_Type(last_loop,Invalid);
            last_loop= ++i;
          }
	continue;
      } else {
        if (LNO_Analysis)
          fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");

	cannot_fuse = TRUE;	// do not try fusion again
      }
    }
    if (try_fission) {
      if (LNO_Analysis)
        fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
      "Attempted to fission in between to create a deeper Simply Nested Loop");
      if ( LNO_Tlog )
        fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
      "Attempted to fission in between to create a deeper Simply Nested Loop");

      fission_status= Fission(parent_wn, wn1, wn2, sloops.Lastidx()+1, FALSE);
      if (fission_status==Succeeded)
      {
        // successfully fissioned
        fission_succeeded = TRUE;
        if (LNO_Analysis)
          fiz_fuse_analysis_info(SUCCEED, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Created a deeper Simply Nested Loop by fission");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Created a deeper Simply Nested Loop by fission");

        // if the last loop is Not_Inner, only report the depth of the
        // outer loop after fission
        // also put last loop in snls
        if (rval->Get_Type(relts-1)==Not_Inner) {
	  rval->Set_Depth(relts-1,sloops.Lastidx()+1-max_fissioned_level);
	  WN* tmp=info->Get_Wn(last_loop);
          for (INT k=0; k<last_fissioned_level; k++) {
            tmp=LWN_Get_Parent(LWN_Get_Parent(tmp));
          }
	  snls->New_Snl(tmp,info->Get_Depth(last_loop)+last_fissioned_level,
                        info->Get_Type(last_loop));
        } else if (info->Get_Type(last_loop)==Inner) {
	  rval->Set_Depth(relts-1,
                          sloops.Lastidx()+1+info->Get_Depth(last_loop));
        } else {
	  rval->Set_Depth(relts-1,sloops.Lastidx()+1);
	  rval->Set_Type(relts-1,Not_Inner);
        }

        // put the child loop i and the outer loops in rval
        last_outer_loop=WN_next(last_outer_loop);
        rval->New_Snl(last_outer_loop,sloops.Lastidx()+1,Inner);

	relts++;
	last_loop=i;
        last_fissioned_level=sloops.Lastidx()+1;
        max_fissioned_level=0;
	continue;
      } else {
        // fission failed
        if (LNO_Analysis)
          fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Failed to create a deeper Simply Nested Loop by fission");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Failed to create a deeper Simply Nested Loop by fission");
      }

    }
   if (try_fusion && !cannot_fuse) {
      if (LNO_Analysis)
        fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, min_snl_level,
          "Attempted to fuse to create a deeper Simply Nested Loop");
      if ( LNO_Tlog )
        fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, min_snl_level,
          "Attempted to fuse to create a deeper Simply Nested Loop");

      if (Move_Adjacent(info->Get_Wn(last_loop), info->Get_Wn(i), FALSE)==TRUE)
        fusion_status=Fuse(info->Get_Wn(last_loop), info->Get_Wn(i),
		min_snl_level, peeling_limit,TRUE);
      else
        fusion_status=Failed;
      if (fusion_status==Succeeded ||
          fusion_status==Succeeded_and_Inner_Loop_Removed) {
        fusion_succeeded = TRUE;
        if (LNO_Analysis)
          fiz_fuse_analysis_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");

        if (fusion_status==Succeeded_and_Inner_Loop_Removed)
          if (min_snl_level>1)
            info->Set_Depth(last_loop,min_snl_level-1);
          else { // both loops have disappeared
            info->Set_Type(last_loop,Invalid);
            last_loop= ++i;
          }
	continue;
      } else {
        if (LNO_Analysis)
          fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");

	cannot_fuse = TRUE;
      }
    }

    WN* last_loop_nest=info->Get_Wn(last_loop);
    WN* current_loop_nest=info->Get_Wn(i);

    // now try level-by-level fission and fusion
    INT fissioned_level=0;
    if (try_fusion && min_snl_level>1 && fusion_status==Try_Level_By_Level) {

      if (LNO_Analysis)
        fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, min_snl_level,
    "Attempting level-by-level fusion to create a deeper Simply Nested Loop");
      if ( LNO_Tlog )
        fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, min_snl_level,
    "Attempting level-by-level fusion to create a deeper Simply Nested Loop");

      UINT tmp_fused_level=min_snl_level;
      FISSION_FUSION_STATUS fusion_status=
        Fuse_Level_By_Level(last_loop_nest, current_loop_nest, 
                            &tmp_fused_level, peeling_limit,
                            FALSE,FALSE, info);
      if (fusion_status==Succeeded ||
          fusion_status==Succeeded_and_Inner_Loop_Removed) {

        fusion_succeeded = TRUE;
        if (LNO_Analysis)
          fiz_fuse_analysis_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, min_snl_level,
            "Fused to create a deeper Simply Nested Loop");

        if (fusion_status==Succeeded_and_Inner_Loop_Removed)
          if (min_snl_level>1)
            info->Set_Depth(last_loop,min_snl_level-1);
          else { // both loops have disappeared
            info->Set_Type(last_loop,Invalid);
            last_loop= ++i;
          }
	continue;
      } else {
        if (LNO_Analysis)
          fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, min_snl_level,
            "Failed to create a deeper Simply Nested Loop by fusion");

        if (tmp_fused_level!=0) {

          DevWarn("Unable to re-fission partially fused SNL: level=%d.\n",
		  tmp_fused_level);

          WN* tmp=info->Get_Wn(last_loop);
	  // Fuse_Level_By_level fused tmp_fused_level - 1 levels,
	  // but failed to fuse the tmp_fused_level'th level.
          for (INT k=0; k<tmp_fused_level-1; k++) {
            tmp=Get_Only_Loop_Inside(tmp,FALSE);
	    FmtAssert(tmp != NULL, ("Expected single loop.\n"));
          }
          WN* block=WN_do_body(tmp);
          tmp=WN_first(block);
          while (WN_opcode(tmp)!=OPC_DO_LOOP)
            tmp=WN_next(tmp);
          snls->New_Snl(tmp, info->Get_Depth(last_loop)-tmp_fused_level,
                             info->Get_Type(last_loop));
          tmp=WN_next(tmp);
          while (WN_opcode(tmp)!=OPC_DO_LOOP)
            tmp=WN_next(tmp);
          snls->New_Snl(tmp, info->Get_Depth(i)-tmp_fused_level,
                             info->Get_Type(i));

          info->Set_Depth(last_loop, tmp_fused_level);
          info->Set_Type(last_loop, Not_Inner);

          continue;
        }

        // because new loop nodes are created after fission
        // we cannot use old loop node of child loop i in info
        // and has to put in the new one
	info->Set_Wn(i,WN_next(info->Get_Wn(last_loop)));
      }
    }
    if (try_fission && sloops.Lastidx()+1>1) {
      
      if (LNO_Analysis)
        fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
    "Attempting level-by-level fission to create a deeper Simply Nested Loop");
      if ( LNO_Tlog )
        fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
    "Attempting level-by-level fission to create a deeper Simply Nested Loop");

      last_loop_nest=info->Get_Wn(last_loop);
      current_loop_nest=info->Get_Wn(i);

      INT j;
      for (j=0; j<sloops.Lastidx()+1; j++) {

        if (Fission(LWN_Get_Parent(LWN_Get_Parent(last_loop_nest)),
                    last_loop_nest, current_loop_nest, 1, FALSE)==Succeeded) {
          fission_succeeded = TRUE;
          fissioned_level++;
          last_loop_nest=LWN_Get_Parent(LWN_Get_Parent(last_loop_nest));
          current_loop_nest=LWN_Get_Parent(LWN_Get_Parent(current_loop_nest));
        } else
	  break;
      }
      if (fissioned_level==sloops.Lastidx()+1) {
	
        if (LNO_Analysis)
          fiz_fuse_analysis_info(SUCCEED, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Created a deeper Simply Nested Loop by fission");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Created a deeper Simply Nested Loop by fission");

        // if the last loop is Not_Inner, only report the depth of the
        // outer loop after fission
        // also put last loop in snls
        if (rval->Get_Type(relts-1)==Not_Inner) {
	  rval->Set_Depth(relts-1,sloops.Lastidx()+1-max_fissioned_level);
	  WN* tmp=info->Get_Wn(last_loop);
          for (INT k=0; k<last_fissioned_level; k++) {
            tmp=LWN_Get_Parent(LWN_Get_Parent(tmp));
          }
	  snls->New_Snl(tmp,info->Get_Depth(last_loop)+last_fissioned_level,
                        info->Get_Type(last_loop));
        } else if (info->Get_Type(last_loop)==Inner) {
	  rval->Set_Depth(relts-1,
                          sloops.Lastidx()+1+info->Get_Depth(last_loop));
        } else {
	  rval->Set_Depth(relts-1,sloops.Lastidx()+1);
	  rval->Set_Type(relts-1,Not_Inner);
        }

        // put the child loop i and the outer loops in rval
        last_outer_loop=WN_next(last_outer_loop);
        rval->New_Snl(last_outer_loop,sloops.Lastidx()+1,Inner);

	relts++;
	last_loop=i;
        last_fissioned_level=sloops.Lastidx()+1;
        max_fissioned_level=0;
	continue;
      } else if (fissioned_level>0) {
        // Failed in fissioning all the way out

        // set the outer loop to be Not_Inner
        // also the outer loop has less SNL level after partial fission
        rval->Set_Type(relts-1,Not_Inner);
        if (fissioned_level>max_fissioned_level)
          max_fissioned_level=fissioned_level;

        if (last_fissioned_level<fissioned_level)
          j=last_fissioned_level;
        else
          j=fissioned_level;
	WN* tmp=info->Get_Wn(last_loop);
        for (INT k=0; k<j; k++) {
          tmp=LWN_Get_Parent(LWN_Get_Parent(tmp));
        }
	snls->New_Snl(tmp, info->Get_Depth(last_loop)+j,
                      info->Get_Type(last_loop));

	last_loop=i;
        last_fissioned_level=fissioned_level;
        continue;
      } else {
        if (LNO_Analysis)
          fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Failed to create a deeper Simply Nested Loop by fission");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Failed to create a deeper Simply Nested Loop by fission");
        last_fissioned_level=0;
      }
    } else
      last_fissioned_level=0;

    // neither fission nor fusion works so move i-th child loop
    // closer to (i+1)th child loop
    if (i!=info->Num_Snl()-1)
      (void)Move_Adjacent(info->Get_Wn(i), info->Get_Wn(i+1), FALSE);

    snls->Copy_Snl(info,last_loop);
    last_loop=i;

    // because there are at least two child loops that cannot be
    // fissioned, the parent loop is Not_Inner
    rval->Set_Type(relts-1,Not_Inner);

  }

  // We do not want to undo what fusion did and we wish to avoid
  // a duplicate action with fission.  Also it is noteworth to
  // to consider the fact that fission operates on a divergent list
  // of fission points that what are discovered for partition bounaries
  // here, so the functionality really is exclusive.
  if ((LNO_Serial_Distribute!=0) && !Get_Do_Loop_Info(loop)->No_Fission &&
       !Do_Loop_Is_Mp(loop) && Do_Loop_Is_Inner(loop) && !Early_MP_Processing &&
       (fusion_succeeded == FALSE) && 
       ((OPT_unroll_level == 2) || 
        (Get_Do_Loop_Info(loop)->Multiversion_Alias)) &&
       (fission_succeeded == FALSE)) {
    FF_STMT_LIST stl_1;
    WN* wn1;
    WN* wn2;
    SRCPOS srcpos1;
    SRCPOS srcpos2;

    // try to get a distribution list
    Get_Distribution_List(stl_1, loop, sloops.Lastidx()+1);

    // try to fission at at scc boundaries.
    FF_STMT_NODE *stmt_node1, *stmt_node2;
    FF_STMT_ITER stmt_iter_1(&stl_1);

    last_loop=0;
    i=1;
    if (stmt_iter_1.First()) {
      for (stmt_node1 = stmt_iter_1.First(), stmt_node2 =  stmt_iter_1.Next();
        stmt_node1 && stmt_node2; 
        stmt_node1 = stmt_iter_1.Next(), stmt_node2 = stmt_iter_1.Next()) {
        FISSION_FUSION_STATUS fission_status=Failed;
        wn1 = stmt_node1->Get_Stmt();
        wn2 = stmt_node2->Get_Stmt();
        WN *loop_body = WN_do_body(loop);

        if ((LWN_Get_Parent(wn1)!=loop_body) || 
            (LWN_Get_Parent(wn2)!=loop_body))
          continue;

        srcpos1=WN_Get_Linenum(wn1);
        srcpos2=WN_Get_Linenum(wn2);
        if (LNO_Analysis)
          fiz_fuse_analysis_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Attempted Distrubted style fission on Simply Nested Loop");
        if ( LNO_Tlog )
          fiz_fuse_tlog_info(INFO, srcpos1, srcpos2, sloops.Lastidx()+1,
            "Attempted Distrubted style fission on Simply Nested Loop");

        partition_based_fission = TRUE;
        fission_status=Fission(loop, wn1, wn2, sloops.Lastidx()+1, 
                               partition_based_fission);
        if (fission_status==Succeeded)
        {
          // successfully fissioned
          if (LNO_Analysis)
            fiz_fuse_analysis_info(SUCCEED, srcpos1, 
                                   srcpos2, sloops.Lastidx()+1,
              "Serialially Distributed fission of a Simply Nested Loop");
          if ( LNO_Tlog )
            fiz_fuse_tlog_info(SUCCEED, srcpos1, srcpos2, sloops.Lastidx()+1,
              "Serialially Distributed fission of a Simply Nested Loop");

          rval->Set_Depth(relts-1, sloops.Lastidx()+1);

          // put the child loop i and the outer loops in rval
          WN *new_loop = LWN_Get_Parent(LWN_Get_Parent(wn2));
          rval->New_Snl(new_loop,sloops.Lastidx()+1,Inner);

          DO_LOOP_INFO* loop_info=Get_Do_Loop_Info(loop);
          DO_LOOP_INFO* new_loop_info =
            CXX_NEW(DO_LOOP_INFO(loop_info,
                                 &LNO_default_pool), &LNO_default_pool);
          Set_Do_Loop_Info(new_loop,new_loop_info);
          new_loop_info->No_Fusion = TRUE;

	  relts++;
	  last_loop=i;
          last_fissioned_level=sloops.Lastidx()+1;
          max_fissioned_level=0;
        } else {
          // fission failed
          if (LNO_Analysis)
            fiz_fuse_analysis_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
              "Failed to Distrubute Simply Nested Loop by fission");
          if ( LNO_Tlog )
            fiz_fuse_tlog_info(FAIL, srcpos1, srcpos2, sloops.Lastidx()+1,
              "Failed to Distrubute Simply Nested Loop by fission");
          break;
        }
        i++;
      }
    }
  }

  if (!Do_Loop_Is_Good(rval->Get_Wn(relts-1)) ||
       Do_Loop_Has_Gotos(rval->Get_Wn(relts-1)) ||
       Do_Loop_Has_Calls(rval->Get_Wn(relts-1))) {
    rval->Delete_Last_Snl();
    relts--;
  } else if (rval->Get_Type(relts-1)==Not_Inner) {
  // a Not_Inner loop can still be a snl with some depth
    rval->Set_Depth(relts-1,sloops.Lastidx()+1-max_fissioned_level);
    if (last_loop<info->Num_Snl()) {
      WN* tmp=info->Get_Wn(last_loop);
      for (INT k=0; k<last_fissioned_level; k++) {
        tmp=LWN_Get_Parent(LWN_Get_Parent(tmp));
      }
      snls->New_Snl(tmp,info->Get_Depth(last_loop)+last_fissioned_level,
                       info->Get_Type(last_loop));
    }
  } else if (last_loop<info->Num_Snl()) {
    SNL_TYPE t=info->Get_Type(last_loop);
    if (t==Inner || t==Not_Inner) {
      rval->Set_Depth(relts-1,sloops.Lastidx()+1+info->Get_Depth(last_loop));
      rval->Set_Type(relts-1,t);
    } else {
      rval->Set_Depth(relts-1,sloops.Lastidx()+1);
      if (t==Non_SNL &&		// could be a OPC_IF with no inner loop
          Find_SCF_Inside(info->Get_Wn(last_loop),OPC_DO_LOOP)==NULL)
        rval->Set_Type(relts-1,Inner);
      else
        rval->Set_Type(relts-1,Not_Inner);
    }
  } else
    rval->Set_Depth(relts-1,sloops.Lastidx()+1);

  sloops.Free_array();
  MEM_POOL_Pop(&LNO_local_pool);
  return rval;
}

