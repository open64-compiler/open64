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

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.fission.cxx $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include "fission.h"
#include "defs.h"
#include "stab.h"
#include "wn.h"
#include "lwn_util.h"
#include "errors.h"
#include "cxx_graph.h"
#include "lnoutils.h"
#include "ff_utils.h"
#include "lnopt_main.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "lno_scc.h"
#include "wn_map.h"
#include "opt_du.h"
#include "glob.h"
#include "tlog.h"

#pragma weak New_Construct_Id

typedef HASH_TABLE<WN *, INT> WN2INT;

extern WN* Get_Only_Loop_Inside(const WN* wn, BOOL regions_ok);

static ARRAY_DIRECTED_GRAPH16 *adg;	// array dep. graph
static BOOL fission_initialized=FALSE;

MEM_POOL FISSION_default_pool;	// pool used by fission only

const mUINT16  FISSION_BIT_STMT_REORDERING=            0x0001;
const mUINT16  FISSION_BIT_PERFECT_NESTING=            0x0002;
const mUINT16  FISSION_BIT_SINGLY_NESTING=             0x0004;
const mUINT16  FISSION_BIT_SCALAR_EXPANSION=           0x0008;
const mUINT16  FISSION_BIT_EXPRESSION_BASED=           0x0010;
const mUINT16  FISSION_BIT_TWO_LOOPS_ONLY=             0x0020;
const mUINT16  FISSION_BIT_PARTITION_BASED=            0x0040;
const mUINT16  FISSION_BIT_TEST_ONLY=                  0x1000;

const mUINT16 OPT_FLAG_STMT_SIMPLE=                  0;
const mUINT16 OPT_FLAG_STMT_PERFECT=		FISSION_BIT_PERFECT_NESTING;
//const mUINT16 OPT_FLAG_STMT_SINGLY=		FISSION_BIT_SINGLY_NESTING;
const mUINT16 OPT_FLAG_STMT_EXPANSION=		FISSION_BIT_SCALAR_EXPANSION;
const mUINT16 OPT_FLAG_STMT_REORDERING=		FISSION_BIT_STMT_REORDERING;
const mUINT16 OPT_FLAG_STMT_REORDERING_PERFECT=	FISSION_BIT_STMT_REORDERING |
						FISSION_BIT_PERFECT_NESTING;
//const mUINT16 OPT_FLAG_STMT_REORDERING_SINGLY=FISSION_BIT_STMT_REORDERING |
//						FISSION_BIT_SINGLY_NESTING;
const mUINT16 OPT_FLAG_STMT_BEST=		FISSION_BIT_STMT_REORDERING ||
						FISSION_BIT_PERFECT_NESTING ||
						FISSION_BIT_SCALAR_EXPANSION;


static void fission_verbose_info(
  BOOL          ,   // success
  SRCPOS        srcpos,
  UINT32        fission_level,
  const char*         message)
{
  printf("#### Fission(%d:%d): %s\n",
    Srcpos_To_Line(srcpos), fission_level, message);

}

static void distribute_verbose_info(
  BOOL          ,   // success
  SRCPOS        srcpos,
  UINT32        fission_level,
  const char*         message)
{
  printf("#### Distribute(%d:%d): %s\n",
    Srcpos_To_Line(srcpos), fission_level, message);

}

static void fission_analysis_info(
  BOOL          success,
  SRCPOS        srcpos,
  UINT32        fission_level,
  const char*         message)
{
  if (success)
    fprintf(LNO_Analysis,"( LNO_Fission_Success ");
  else
    fprintf(LNO_Analysis,"( LNO_Fission_Failure ");

  fprintf(LNO_Analysis,"(%s %d) %d \"%s\" )\n",
    Cur_PU_Name, Srcpos_To_Line(srcpos), fission_level, message);
}

static void fission_tlog_info(
  FISSION_FUSION_STATUS     status,
  WN*           loop,
  UINT32        level,
  const char*         message)
{
  char in_string[30];
  char out_string[30];
  SRCPOS srcpos=WN_Get_Linenum(loop);
  sprintf(in_string,"%d %d", Srcpos_To_Line(srcpos), level);
  sprintf(out_string,"%d", status);
  Generate_Tlog("LNO","fission", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(loop))), in_string, out_string, message);
}


/**
*** Separate the 'in_loop' at 'in_stmt' to result in two loops,
*** 'in_loop' and 'new_loop' and 'in_stmt'
*** is the last statement of 'in_loop'. 'level' specifies how many
*** level to separate.
*** if (create_empty_loop==TRUE && in_stmt==NULL) 
***    new_loop will contain all the stmts of the in_loop
*** if (create_empty_loop==TRUE && WN_next(in_stmt)==NULL)
***    new_loop will be an empty loop
**/

extern void 
Separate(WN* in_loop,WN* in_stmt, UINT8 level, WN** new_loop, BOOL create_empty_loop)
{

  WN* loop_body1;
  WN* loop_body2;
  WN* wn1;
  WN* wn2;

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Separate()\n") );
  if (in_stmt)
    FmtAssert(LWN_Get_Parent(LWN_Get_Parent(in_stmt))==in_loop,
      ("Separate point not at the first level in the loop in Separate()\n") );

  if (!in_stmt && !create_empty_loop) {
    Is_True(0, ("Null stmt passed into LNO:Separate()\n"));
    return;
  }
  

  if (in_stmt && !create_empty_loop && (WN_next(in_stmt) == NULL)) {
    // loop with single statement
    *new_loop=NULL;
    return;
  }

  // Create DO loop node for loop2, TODO should use wn_pool
  *new_loop =LWN_CreateDO (
	LWN_Copy_Tree(WN_index(in_loop),TRUE,LNO_Info_Map),	// index
	LWN_Copy_Tree(WN_start(in_loop),TRUE,LNO_Info_Map),	// start
	LWN_Copy_Tree(WN_end(in_loop),TRUE,LNO_Info_Map),	// end
	LWN_Copy_Tree(WN_step(in_loop),TRUE,LNO_Info_Map),	// step
  	WN_CreateBlock()			// body
	);

  if (!Array_Dependence_Graph->
    Add_Deps_To_Copy_Block(WN_start(in_loop), WN_start(*new_loop), FALSE))
  LNO_Erase_Dg_From_Here_In(in_loop,Array_Dependence_Graph);
  if (!Array_Dependence_Graph->
    Add_Deps_To_Copy_Block(WN_end(in_loop), WN_end(*new_loop), FALSE))
  LNO_Erase_Dg_From_Here_In(in_loop,Array_Dependence_Graph);
  if (!Array_Dependence_Graph->
    Add_Deps_To_Copy_Block(WN_step(in_loop), WN_step(*new_loop), FALSE))
  LNO_Erase_Dg_From_Here_In(in_loop,Array_Dependence_Graph);

  LWN_Copy_Linenumber(WN_do_body(in_loop),WN_do_body(*new_loop));

  // Assumption: start, end, and step expressions are all loop-invariant

  loop_body1=WN_do_body(in_loop);
  loop_body2=WN_do_body(*new_loop);

  // cut the loop body of loop1 at in_stmt
  if (in_stmt){
    if (WN_next(in_stmt)!=NULL){
      WN_first(loop_body2)=WN_next(in_stmt);
      WN_last(loop_body2)=WN_last(loop_body1);
      WN_last(loop_body1)=in_stmt;
      WN_prev(WN_first(loop_body2))=NULL;
      WN_next(WN_last(loop_body2))=NULL;
      WN_next(WN_last(loop_body1))=NULL;
    }
  } else {
    WN_first(loop_body2)=WN_first(loop_body1);
    WN_last(loop_body2)=WN_last(loop_body1);
    WN_first(loop_body1)=NULL;
    WN_last(loop_body1)=NULL;
  }

  // Re-assign the parent pointer for stmts in loop2
  for (wn1=WN_first(loop_body2); wn1; wn1=WN_next(wn1)) {
    LWN_Set_Parent(wn1,loop_body2);
  }

  wn1 = in_loop;
  wn2 = *new_loop;

  LWN_Copy_Linenumber(wn1, wn2);

/*
  WN* alias1 = NULL;

  Replace_Symbol(WN_index(wn2),
    loop1_var_symbol, new_loop_var_symbol, alias1);
  Replace_Symbol(WN_start(wn2),
    loop1_var_symbol, new_loop_var_symbol, alias1);
  Replace_Symbol(WN_end(wn2),
    loop1_var_symbol, new_loop_var_symbol, alias1);
  Replace_Symbol(WN_step(wn2),
    loop1_var_symbol, new_loop_var_symbol, alias1);
  Replace_Symbol(WN_do_body(wn2),
    loop1_var_symbol, new_loop_var_symbol, alias1);
*/

  for (INT i=1; i<level; i++) {
    WN* block=WN_CreateBlock();
    WN_first(block)=WN_last(block)=wn2;
    WN_prev(wn2)=WN_next(wn2)=NULL;
    LWN_Set_Parent(wn2,block);

    for (WN* stmt=WN_next(wn1); stmt; stmt=WN_next(stmt)) {
      LWN_Extract_From_Block(stmt);
      LWN_Insert_Block_After(block,wn2,stmt);
      LWN_Set_Parent(stmt,block);
      wn2=stmt;
    }

    wn1 = LWN_Get_Parent(LWN_Get_Parent(wn1));
    FmtAssert(WN_opcode(wn1)==OPC_DO_LOOP,
      ("Not enough level to separate\n"));

    wn2 =LWN_CreateDO (
	LWN_Copy_Tree(WN_index(wn1),TRUE,LNO_Info_Map),		// index
	LWN_Copy_Tree(WN_start(wn1),TRUE,LNO_Info_Map),		// start
	LWN_Copy_Tree(WN_end(wn1),TRUE,LNO_Info_Map),		// end
	LWN_Copy_Tree(WN_step(wn1),TRUE,LNO_Info_Map),		// step
  	block							// body
	);

    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_start(wn1), WN_start(wn2), FALSE))
    LNO_Erase_Dg_From_Here_In(wn1,Array_Dependence_Graph);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_end(wn1), WN_end(wn2), FALSE))
    LNO_Erase_Dg_From_Here_In(wn1,Array_Dependence_Graph);
    if (!Array_Dependence_Graph->
      Add_Deps_To_Copy_Block(WN_step(wn1), WN_step(wn2), FALSE))
    LNO_Erase_Dg_From_Here_In(wn1,Array_Dependence_Graph);


    LWN_Copy_Linenumber(wn1, wn2);

  }
  LWN_Insert_Block_After(LWN_Get_Parent(wn1),	// block
	wn1,						// wn
	wn2						// in
	);						// pool

}

// Separate_And_Update separate the statements in a loop and
// update the array and DU dependence information after the separation.
// It does not check for the legality of fission and that is the
// responsibility of the producer of loop[].
// if 'rename_loop_var' is TRUE (the default), the loop index var will be
// renamed except for the last separated loop.

void Separate_And_Update(
  WN* in_loop,
  DYN_ARRAY<FF_STMT_LIST>& loop,
  UINT fission_level,
  BOOL rename_loop_var)
{

  MEM_POOL_Push(&FISSION_default_pool);
  UINT total_loops=loop.Lastidx()+1;

  WN*** wn_starts=CXX_NEW_ARRAY(WN**, fission_level, &FISSION_default_pool);
  WN*** wn_ends=CXX_NEW_ARRAY(WN**, fission_level, &FISSION_default_pool);
  WN*** wn_steps=CXX_NEW_ARRAY(WN**, fission_level, &FISSION_default_pool);

  INT i;
  for (i=0; i<fission_level; i++) {
  
    wn_starts[i]=CXX_NEW_ARRAY(WN*, total_loops, &FISSION_default_pool);
    wn_ends[i]=CXX_NEW_ARRAY(WN*, total_loops, &FISSION_default_pool);
    wn_steps[i]=CXX_NEW_ARRAY(WN*, total_loops, &FISSION_default_pool);

  }

  // now really fission loops and update the dependence info

  // create new loop nests
  WN*** new_loop=CXX_NEW_ARRAY(WN**,fission_level,&FISSION_default_pool);

  DO_LOOP_INFO* loop_info[LNO_MAX_DO_LOOP_DEPTH];
  WN* wn=in_loop;
  WN* outer_most_loop;
  // first get the old loop info
  for (i=fission_level-1; i>=0; i--) {
    new_loop[i]=CXX_NEW_ARRAY(WN*,total_loops,&FISSION_default_pool);
    new_loop[i][0]=wn;
    loop_info[i]=(DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map,wn);

    wn_starts[i][0]=WN_kid0(WN_start(wn));
    wn_ends[i][0]=WN_end(wn);
    wn_steps[i][0]=WN_kid0(WN_step(wn));

    if (i==0)
      outer_most_loop=wn;

    wn=LWN_Get_Parent(LWN_Get_Parent(wn));
  }

  BOOL has_calls_or_gotos_or_inner_loops = FALSE;
  if (loop_info[fission_level-1]->Has_Calls ||
      loop_info[fission_level-1]->Has_Gotos || 
      !loop_info[fission_level-1]->Is_Inner) {
    has_calls_or_gotos_or_inner_loops = TRUE;
  }

  for (i=total_loops-1; i>0; i--) {
	
    WN* loop_body = WN_do_body(in_loop);
    WN* first_stmt = loop[i].Head()->Get_Stmt();
    FF_STMT_NODE* stmt_node_p;
    while(stmt_node_p = loop[i].Remove_Headnode()) {
      WN* stmt = stmt_node_p->Get_Stmt();

      stmt = LWN_Extract_From_Block(stmt);

      LWN_Insert_Block_Before(loop_body,NULL,stmt);

    }

    Separate(in_loop, WN_prev(first_stmt), fission_level, &wn);

    // wn points to the innermost loop of the 2nd nest

    // new loop info of the inner most loop in the new loop nests
    DO_LOOP_INFO* new_loop_info = 
      CXX_NEW(DO_LOOP_INFO(loop_info[fission_level-1],&LNO_default_pool),
	&LNO_default_pool);
    WN_MAP_Set(LNO_Info_Map, wn, (void*)new_loop_info);
    if (has_calls_or_gotos_or_inner_loops)
      FF_Mark_Inner_Loop_Info(wn);

    wn_starts[fission_level-1][i]=WN_kid0(WN_start(wn));
    wn_ends[fission_level-1][i]=WN_end(wn);
    wn_steps[fission_level-1][i]=WN_kid0(WN_step(wn));
    new_loop[fission_level-1][i]=wn;

    // create loop info for outer loops
    for (INT j=fission_level-2; j>=0; j--) {
      wn = LWN_Get_Parent(LWN_Get_Parent(wn));
      DO_LOOP_INFO* outer_loop_info = 
        CXX_NEW(DO_LOOP_INFO(loop_info[j],&LNO_default_pool),
	  &LNO_default_pool);
      outer_loop_info->Has_Calls = new_loop_info->Has_Calls ;
      outer_loop_info->Has_Unsummarized_Calls = 
			new_loop_info->Has_Unsummarized_Calls ;
      outer_loop_info->Has_Gotos = new_loop_info->Has_Gotos ;
      WN_MAP_Set(LNO_Info_Map, wn, (void*)outer_loop_info);

      wn_starts[j][i]=WN_kid0(WN_start(wn));
      wn_ends[j][i]=WN_end(wn);
      wn_steps[j][i]=WN_kid0(WN_step(wn));
      new_loop[j][i]=wn;

    }
  }

  // now adjust the original loop info if it is possible that the
  // calls, gotos and inner loops have been moved to other loops after fission
  if (has_calls_or_gotos_or_inner_loops)
    FF_Mark_Inner_Loop_Info(outer_most_loop);

  adg->Fission_Dep_Update(new_loop[0][0], total_loops);
  //sdg->Fission_Dep_Update(new_loop[0], total_loops, fission_level);

  wn=in_loop;
  for (i=fission_level-1; i>=0; i--, wn = LWN_Get_Parent(LWN_Get_Parent(wn)))
    Fission_DU_Update(Du_Mgr,red_manager,wn_starts[i],wn_ends[i],wn_steps[i],
      total_loops,new_loop[i]);

  if (rename_loop_var)
    for (i=0; i<total_loops-1; i++)
      for (INT j=0; j<fission_level; j++)
        scalar_rename(LWN_Get_Parent(wn_starts[j][i]));

  MEM_POOL_Pop(&FISSION_default_pool);
}


// determines if the current loop should be merged with the last
// loop given the option flags, prev_loop_has_loops_or_regions and
// current_loop_has_loops_or_regions info.
static
UINT16 Merge_loop(DYN_ARRAY<FF_STMT_LIST>& loop, mUINT16 opt_flag,
BOOL prev_loop_has_loops_or_regions, BOOL current_loop_has_loops_or_regions)
{

  mUINT16 m = loop.Lastidx();

  if (opt_flag & FISSION_BIT_PERFECT_NESTING) {
  // perfect-nesting fission
    if (!prev_loop_has_loops_or_regions) {
      if (!current_loop_has_loops_or_regions) {
        loop[m-1].Append_List(&loop[m]);
      } else {
        m=loop.Newidx();
        loop[m].Clear();
      }
    } else {
      m=loop.Newidx();
      loop[m].Clear();
    }
  } else if (opt_flag & FISSION_BIT_SINGLY_NESTING) {
    // singly-nesting fission
    if (prev_loop_has_loops_or_regions && current_loop_has_loops_or_regions) {
      m=loop.Newidx();
      loop[m].Clear();
    } else {
      loop[m-1].Append_List(&loop[m]);
    }
  } else {
     m=loop.Newidx();
     loop[m].Clear();
  }

  loop[m].Init((FF_STMT_NODE*)NULL);
  return m;
    
}

/**
*** Break 'in_loop' in ways depending on 'opt_flag'.
*** Statement order will be preserved in the loop.
*** 'dep_g_p' points to a statement based dependence graph
*** while 'dep_graph_map' provides the mapping from statements
*** in 'in-loop' to vertices on the depndence graph. It is assumed
*** that this mapping maps the first statament to the vertex with
*** index 1 and so on. Otherwise the mapping is illegal. The loop
*** will remain unchanged but a FF_STMT_LIST which contains the
*** stataments marking the end of loops are returned throught
*** 'break_point_list'.
***
*** note: for internal use only
**/

static void 
Find_Break_Point(WN* in_loop, mUINT16 opt_flag,
SCC_DIRECTED_GRAPH16 *dep_g_p, WN_MAP dep_graph_map,
DYN_ARRAY<FF_STMT_LIST>& loop, MEM_POOL* pool)
//FF_STMT_LIST *break_point_list)
{

  WN* begin_stmt;
  WN* stmt;
  //WN* prev_end_stmt = NULL; // for perfect nesting fission
  VINDEX16 i;
  mUINT16 m;
  BOOL prev_loop_has_loops_or_regions = TRUE;

  BOOL mapping_is_illegal = FALSE;

  for ( i=1,
        stmt = WN_first(WN_do_body(in_loop));
	stmt;
	stmt = WN_next(stmt), i++) {
	if ((mUINT32)(INTPTR)(WN_MAP_Get(dep_graph_map, stmt)) != i)
	    mapping_is_illegal = TRUE;
  }

  if (mapping_is_illegal == TRUE) {
    Is_True(0, ("Illegal mapping\n"));
    m=loop.Newidx();
    loop[m].Init((FF_STMT_NODE*)NULL);
    WN* stmt = WN_first(WN_do_body(in_loop));
    while (stmt) {
      loop[m].Append(stmt, pool);
      stmt=WN_next(stmt);
    }
    return;	// do not fission
  }


  m=loop.Newidx();
  loop[m].Init((FF_STMT_NODE*)NULL);
  begin_stmt = WN_first(WN_do_body(in_loop));
  while (begin_stmt) {
    // begin_stmt always points to the first statement for the
    // current loop
    // the following for-loop find the last statement to be
    // included in the current loop by checkingfor the backward
    // dependences
    VINDEX16 sink_v, end_v;
    EINDEX16 e;
    WN* sink_stmt;

    BOOL current_loop_has_loops_or_regions = FALSE;
    for (sink_stmt=begin_stmt,
	 sink_v=(mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, begin_stmt),
         // TODO: need to get a 16 bit mapping later
	 end_v = sink_v;
	 sink_v <= end_v;
         sink_stmt=WN_next(sink_stmt),sink_v++) {

      // try to find the source stmt of the longest backward dependence of
      // the current stmt
      e = dep_g_p->Get_In_Edge(sink_v);
      while (e) {
	if (dep_g_p->Get_Source(e) > end_v)
	  end_v = dep_g_p->Get_Source(e);
        e = dep_g_p->Get_Next_In_Edge(e);
      }
      OPCODE opc=WN_opcode(sink_stmt);
      if (opc==OPC_DO_LOOP || opc==OPC_REGION ||
          opc==OPC_DO_WHILE || opc==OPC_WHILE_DO ||
          (opc==OPC_IF && Get_If_Info(sink_stmt)->Contains_Do_Loops))
	// record seeing a loop (for perfect-nesting fission)
        current_loop_has_loops_or_regions = TRUE;


	loop[m].Append(sink_stmt, pool);

    }
    // TODO: should check for and implement scalar expansion here
  
    // end_stmt = sink_stmt;
    // begin_stmt = WN_next(end_stmt);
    begin_stmt = sink_stmt;
  
    m=Merge_loop(loop, opt_flag, prev_loop_has_loops_or_regions,
		current_loop_has_loops_or_regions);
    prev_loop_has_loops_or_regions = current_loop_has_loops_or_regions;

  }


  loop.Decidx();
      

}

/**
*** Partition the statments in 'in_loop' into smaller sets which satisfy
*** the requirements specified in 'opt_flag.  Statement reordering is
*** allowed based on the Strongly-Connected-Component (SCC) information
*** obtained from the dependence graph pointed by 'dep_g_p'. The
*** 'dep_graph_map' provides mapping from the statements to the nodes in
*** the graph.
**/

static void
SCC_reorder(WN* in_loop, mUINT16 opt_flag, SCC_DIRECTED_GRAPH16 *dep_g_p,
WN_MAP dep_graph_map, DYN_ARRAY<FF_STMT_LIST>& loop, MEM_POOL* pool)
{
  
    FF_STMT_LIST *scc;
    DYN_ARRAY<VINDEX16> *scc_group;

    mUINT16 *level;		// to hold level-sort result

    mUINT16 m;
    mUINT16 i;
    //SCC_DIRECTED_GRAPH16 *ac_g(0, 0);
    SCC_DIRECTED_GRAPH16 *ac_g;

    ac_g = dep_g_p->Acyclic_Condensation(pool);

    level = CXX_NEW_ARRAY(mUINT16,ac_g->Get_Vertex_Count()+1,
			pool);
    mUINT16 max_level = ac_g->Get_Level(level);

    CXX_DELETE(ac_g, pool);

    VINDEX16 total_scc = dep_g_p->Get_Scc_Count();

    // scc[i] is a list of statemens in i-th SCC
    scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, pool);

    // scc_group[i] is an (dynamic) array of SCC ids which correspond to SCCs
    // at level i
    scc_group = 
      CXX_NEW_ARRAY(DYN_ARRAY<VINDEX16>, max_level+1, pool);
    
    // initialize memory pool for dynamic storage allocation
    for (i=0; i<=max_level; i++) {
      scc_group[i].Set_Mem_Pool(pool);
    }
    
    for (i=1; i<=total_scc; i++) {
      mUINT16 j = level[i];	// get the level of i-th SCC
      mUINT16 k = scc_group[j].Newidx();	// allocate new index for i-th
    						// SCC at level j
      scc_group[j][k] = i;	// store SCC i at level j
    }
    
    for (WN* stmt = WN_first(WN_do_body(in_loop)); stmt;
      stmt = WN_next(stmt)) {
    
      // append each statement to the list corresponding to its SCC
      // note that we process statements from the beginning of
      // the loop so that statements which are closer to the beginning
      // of the loop will be appended (and extracted) earlier
      // this is to preserve the statement order within one SCC so that
      // the loop-independent dependences will be preserved
      // automatically
      VINDEX16 scc_id;
      scc_id = dep_g_p->Get_Scc_Id(
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt));
      scc[scc_id].Append(stmt, pool);  
    }

    BOOL prev_loop_has_loops_or_regions;
    BOOL current_loop_has_loops_or_regions;

    prev_loop_has_loops_or_regions = TRUE;

    m=loop.Newidx();
    loop[m].Init((FF_STMT_NODE*)NULL);

    for (i = 0; i<=max_level; i++) {

      // process one level at a time, starting from the 0-th level

      for (mUINT16 j = 0; j<=scc_group[i].Lastidx(); j++) {

        // process one SCC at this level

        // k is the SCC id of the j-th SCC at this level
        mUINT16 k = scc_group[i][j];

        WN* stmt;
        FF_STMT_NODE* stmt_node;

        current_loop_has_loops_or_regions = FALSE;
        while(stmt_node = scc[k].Remove_Headnode()) {
          stmt = stmt_node->Get_Stmt();

          OPCODE opc=WN_opcode(stmt);
          if (opc==OPC_DO_LOOP || opc==OPC_REGION ||
              opc==OPC_DO_WHILE || opc==OPC_WHILE_DO ||
              (opc==OPC_IF && Get_If_Info(stmt)->Contains_Do_Loops))
                current_loop_has_loops_or_regions = TRUE;

          loop[m].Append(stmt, pool);  

        }

        m=Merge_loop(loop, opt_flag, prev_loop_has_loops_or_regions,
		current_loop_has_loops_or_regions);
        prev_loop_has_loops_or_regions = current_loop_has_loops_or_regions;

      }
    }

    loop.Decidx();
      
}

/**
*** Forms smaller loops from 'in_loop' based on 'opt_flag', 'stl_1' and
*** 'stl_2'. If re-ordering is allowed, all statements from
*** the same strongly-connected component (SCC) are put together.
*** In addition, larger loops will be formed if perfecly-nested or
*** singly-nested (SNL) loops are needed. 'fission_level' also
*** specifies the level of the enclosing loops to be fissioned.
*** The result loop[i] is a FF_STMT_LIST which contains the statements
*** in i-th loop.
***
*** note: for internal use only
**/

extern void
Form_Loops(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
FF_STMT_LIST *stl_1, FF_STMT_LIST *stl_2, ARRAY_DIRECTED_GRAPH16* sdg,
DYN_ARRAY<FF_STMT_LIST>& loop, MEM_POOL* pool)
{
  WN_MAP dep_graph_map;	// map from stmts to vertices in dep_g
  WN* stmt;

  SCC_DIRECTED_GRAPH16 *dep_g_p = 
    CXX_NEW(SCC_DIRECTED_GRAPH16(0, 0), pool);

  dep_graph_map = WN_MAP_Create(pool);
  FmtAssert(dep_graph_map != -1,("Ran out of mappings in Fission"));


  for (stmt = WN_first(WN_do_body(in_loop));
    stmt; stmt = WN_next(stmt)) {
    // map the stmts to vertices in dep_g
#if HOST_IS_BIG_ENDIAN
    union {
      struct {
#if HOST_IS_64BIT
          mUINT32 padding1;
#endif
          mUINT16 padding0;
          VINDEX16 v;
      };
      void *p;
    } v;
#else
    union {
      VINDEX16 v;
      void *p;
    } v;
#endif
    v.p = NULL;
    v.v = dep_g_p->Add_Vertex();
    if (v.v==0) {
      DevWarn("Statement scc graph too big");
      INT m=loop.Newidx();
      loop[m].Clear();
      for (WN* stmt1 = WN_first(WN_do_body(in_loop));
        stmt1; stmt1 = WN_next(stmt1))
        loop[m].Append(stmt1, pool);  
      WN_MAP_Delete(dep_graph_map);
      CXX_DELETE(dep_g_p, pool);
      return;
    }
    WN_MAP_Set(dep_graph_map, stmt, v.p);
    // TODO: really want to use 16 bits in the map
  }

  DO_LOOP_INFO *loop_info;
  loop_info = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, in_loop);
  UINT8 in_loop_level = loop_info->Depth;
  for (stmt = WN_first(WN_do_body(in_loop));
    stmt; stmt = WN_next(stmt)) {

    VINDEX16 v=sdg->Get_Vertex(stmt);
    if (v ==0) {
      OPCODE opc=WN_opcode(stmt);
      if (opc!=OPC_LABEL && opc!=OPC_RETURN && opc!=OPC_GOTO
#ifdef KEY
  	  && opc!=OPC_GOTO_OUTER_BLOCK
#endif
	 ) {
        // depends on everything else
        DevWarn("Statement dependence graph problem");
        INT m=loop.Newidx();
        loop[m].Clear();
        for (WN* stmt1 = WN_first(WN_do_body(in_loop));
          stmt1; stmt1 = WN_next(stmt1))
          loop[m].Append(stmt1, pool);  
        WN_MAP_Delete(dep_graph_map);
        CXX_DELETE(dep_g_p, pool);
        return;
      }
    } else {
      EINDEX16 e=sdg->Get_Out_Edge(v);
      while (e) {
	
	if (sdg->Level(e) >= in_loop_level-fission_level+1) {
	  VINDEX16 v1 = sdg->Get_Sink(e);
	  WN* stmt1 = sdg->Get_Wn(v1);

          EINDEX16 e1=dep_g_p->Add_Unique_Edge(
            (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt),
            (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1));

          if (e1==0) {
            DevWarn("Statement scc graph too big");
            INT m=loop.Newidx();
            loop[m].Clear();
            for (WN* stmt1 = WN_first(WN_do_body(in_loop));
              stmt1; stmt1 = WN_next(stmt1))
              loop[m].Append(stmt1, pool);  
            WN_MAP_Delete(dep_graph_map);
            CXX_DELETE(dep_g_p, pool);
            return;
          }
        }
        e = sdg->Get_Next_Out_Edge(e);
      }
    }

  }

  // if no scalar expansion, added a->b if b->a is a scalar edge

  // if no stmt reordering, added a->b if b immed. follows a
  // but here we separate the with and the without re-ordering
  // case so we do not need these edges

  // if stmt list fission, added a<->b if b is on the same list as a
  if (stl_1 && stl_2) {
    FF_STMT_NODE *stmt_node;
    WN* stmt1;
      
    FF_STMT_ITER stmt_iter_1(stl_1);
    FF_STMT_ITER stmt_iter_2(stl_2);

    stmt_node = stmt_iter_1.First();
    if (stmt_node)
      stmt = stmt_node->Get_Stmt();
    FmtAssert(LWN_Get_Parent(LWN_Get_Parent(stmt))==in_loop,
      ("Statement not a immediate child of loop in Form_Loops\n"));

    for (stmt_node = stmt_iter_1.Next();
      stmt_node; stmt_node = stmt_iter_1.Next()) {
      stmt1 = stmt_node->Get_Stmt();
      EINDEX16 e1=dep_g_p->Add_Unique_Edge(
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt),
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1));
      EINDEX16 e2=dep_g_p->Add_Unique_Edge(
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1),
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt));
      if (e1==0 || e2==0) {
        DevWarn("Statement scc graph too big");
        INT m=loop.Newidx();
        loop[m].Clear();
        for (WN* stmt1 = WN_first(WN_do_body(in_loop));
          stmt1; stmt1 = WN_next(stmt1))
          loop[m].Append(stmt1, pool);  
        WN_MAP_Delete(dep_graph_map);
        CXX_DELETE(dep_g_p, pool);
        return;
      }
      stmt = stmt1;
      FmtAssert(LWN_Get_Parent(LWN_Get_Parent(stmt))==in_loop,
	("Statement not a immediate child of loop in Form_Loops\n"));
    }

    stmt_node = stmt_iter_2.First();
    if (stmt_node)
      stmt = stmt_node->Get_Stmt();
    FmtAssert(LWN_Get_Parent(LWN_Get_Parent(stmt))==in_loop,
      ("Statement not a immediate child of loop in Form_Loops\n"));

    for (stmt_node = stmt_iter_2.Next();
      stmt_node; stmt_node = stmt_iter_2.Next()) {
      stmt1 = stmt_node->Get_Stmt();
      EINDEX16 e1=dep_g_p->Add_Unique_Edge(
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt),
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1));
      EINDEX16 e2=dep_g_p->Add_Unique_Edge(
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1),
        (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt));
      if (e1==0 || e2==0) {
        DevWarn("Statement scc graph too big");
        INT m=loop.Newidx();
        loop[m].Clear();
        for (WN* stmt1 = WN_first(WN_do_body(in_loop));
          stmt1; stmt1 = WN_next(stmt1))
          loop[m].Append(stmt1, pool);  
        WN_MAP_Delete(dep_graph_map);
        CXX_DELETE(dep_g_p, pool);
        return;
      }
      stmt = stmt1;
      FmtAssert(LWN_Get_Parent(LWN_Get_Parent(stmt))==in_loop,
	("Statement not a immediate child of loop in Form_Loops\n"));
    }
  }

  if (opt_flag & FISSION_BIT_STMT_REORDERING) { // will try re-ordering
      
    SCC_reorder(in_loop, opt_flag, dep_g_p, dep_graph_map, loop, pool);

  } else {


    Find_Break_Point(in_loop, opt_flag, dep_g_p, dep_graph_map,
      loop, pool);

  }

  WN_MAP_Delete(dep_graph_map);

  CXX_DELETE(dep_g_p, pool);

}

static BOOL
has_back_edges(ARRAY_DIRECTED_GRAPH16* sdg, 
               SCC_DIRECTED_GRAPH16 *dep_g_p,
               WN_MAP dep_graph_map,
               WN *scc_first,  
               int j,
               VINDEX16 *queue, 
               VINDEX16 total_scc)
{
  BOOL ret_val = FALSE;
  int i;
  VINDEX16 v=sdg->Get_Vertex(scc_first);
  EINDEX16 e=sdg->Get_Out_Edge(v);
  while (e) {
    VINDEX16 v1 = sdg->Get_Sink(e);
    WN* stmt = sdg->Get_Wn(v1);
    VINDEX16 scc_id = dep_g_p->Get_Scc_Id(
      (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt));
    for (i = j - 1; i >= 0; i--) {
      if (queue[i] == scc_id) {
        ret_val = TRUE;
        break;
      }
    }
    if (ret_val) break;
    e = sdg->Get_Next_Out_Edge(e);
  }

  return ret_val;
}

static void
Find_Partitions(FF_STMT_LIST& stl_1, WN* in_loop, UINT8 fission_level,
ARRAY_DIRECTED_GRAPH16* sdg,
MEM_POOL* pool)
{
  SCC_DIRECTED_GRAPH16 *dep_g_p = CXX_NEW(SCC_DIRECTED_GRAPH16(0, 0), pool);
  WN_MAP dep_graph_map;	// map from stmts to vertices in dep_g
  WN* stmt;
  VINDEX16 i;
  int j, num_stmts;

  dep_graph_map = WN_MAP_Create(pool);
  FmtAssert(dep_graph_map != -1,("Ran out of mappings in Fission"));

  // now form the SCCs
  for (stmt = WN_first(WN_do_body(in_loop));
    stmt; stmt = WN_next(stmt)) {
    // map the stmts to vertices in dep_g
    union {
      VINDEX16 v;
      void *p;
    } v;
    v.p = NULL;
    v.v = dep_g_p->Add_Vertex();
    if (v.v==0) {
      DevWarn("Statement scc graph too big");
      WN_MAP_Delete(dep_graph_map);
      CXX_DELETE(dep_g_p, pool);
      return;
    }
    WN_MAP_Set(dep_graph_map, stmt, v.p);
    // TODO: really want to use 16 bits in the map
  }

  DO_LOOP_INFO *loop_info;
  loop_info = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, in_loop);
  UINT8 in_loop_level = loop_info->Depth;
  for (stmt = WN_first(WN_do_body(in_loop));
    stmt; stmt = WN_next(stmt)) {

    VINDEX16 v=sdg->Get_Vertex(stmt);
    if (v ==0) {
      OPCODE opc=WN_opcode(stmt);
      if (opc!=OPC_LABEL && opc!=OPC_RETURN && opc!=OPC_GOTO
#ifdef KEY
          && opc!=OPC_GOTO_OUTER_BLOCK
#endif
         ) {
        // depends on everything else
        DevWarn("Statement dependence graph problem");
        WN_MAP_Delete(dep_graph_map);
        CXX_DELETE(dep_g_p, pool);
        return;
      }
    } else {
      EINDEX16 e=sdg->Get_Out_Edge(v);
      while (e) {

        if (sdg->Level(e) >= in_loop_level-fission_level+1) {
          VINDEX16 v1 = sdg->Get_Sink(e);
          WN* stmt1 = sdg->Get_Wn(v1);

          EINDEX16 e1=dep_g_p->Add_Unique_Edge(
            (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt),
            (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt1));

          if (e1==0) {
            DevWarn("Statement scc graph too big");
            WN_MAP_Delete(dep_graph_map);
            CXX_DELETE(dep_g_p, pool);
            return;
          }
        }
        e = sdg->Get_Next_Out_Edge(e);
      }
    }
  }

  WN *loop_body = WN_do_body(in_loop);
  VINDEX16 total_scc = dep_g_p->Get_Scc_Count();
  VINDEX16 *queue=CXX_NEW_ARRAY(VINDEX16,total_scc+1,pool);
  long *dist_array=CXX_NEW_ARRAY(long,total_scc+1,pool);
  BOOL has_dist_list = FALSE;

  // scc[i] is a list of statemens in i-th SCC
  FF_STMT_LIST *scc = CXX_NEW_ARRAY(FF_STMT_LIST, total_scc+1, pool);

  // Append statements to the statement list of proper SCC.
  i = 0;
  num_stmts = 0;
  for (stmt = WN_first(WN_do_body(in_loop)); stmt; stmt = WN_next(stmt)) {
    VINDEX16 scc_id = dep_g_p->Get_Scc_Id(
      (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, stmt));
    FF_STMT_ITER s_iter(&scc[scc_id]);
    if (s_iter.Is_Empty())
      queue[i++] = scc_id;
    scc[scc_id].Append(stmt, pool);
    num_stmts++;
  }

  // Now we are going to look for breaks in connectivity in the 
  // condensed graph, these will be the places to fission or the
  // the distribution partition boundaries of the loop.
  WN *first = WN_first(loop_body);
  WN *last = WN_last(loop_body);
  VINDEX16 last_scc_id = dep_g_p->Get_Scc_Id(
    (mUINT32)(INTPTR)WN_MAP_Get(dep_graph_map, last));
  SCC_DIRECTED_GRAPH16 *ac_g;
  ac_g = dep_g_p->Acyclic_Condensation(pool);
  mUINT16 *level = CXX_NEW_ARRAY(mUINT16,ac_g->Get_Vertex_Count()+1,pool);
  
  // first clear the levels so that what ever we find it usable
  for (j = 0; j < ac_g->Get_Vertex_Count()+1; j++)
    level[j] = 0;

  ac_g->Get_Level(level);
  // Walk the scc nodes lexically from the bottom of the loop up.
  for (j=total_scc-1; j>=0; j--) {
    i = queue[j];
    dist_array[j] = 0;
    BOOL no_in_arcs = (ac_g->Get_In_Edge(i) == 0);
    BOOL no_out_arcs = (ac_g->Get_Out_Edge(i) == 0);
    WN *scc_first = NULL;
    if (no_in_arcs || no_out_arcs) {
      // We split before always.
      // However, we must check if this scc is the first in the loop as
      // we cannot fission at the start of the loop.
      FF_STMT_ITER s_iter(&scc[i]);
      if (!s_iter.Is_Empty()) {
        FF_STMT_NODE* stmt_node = s_iter.First();
        scc_first = stmt_node->Get_Stmt();
      }

      if (scc_first == NULL) continue;
      if (scc_first != first) {
        VINDEX16 scc_id = queue[j-1];

        // First check to see if scc_id is disconnected in the ac_g.
        // If both it and scc_first are disconnected, we want the partition
        // to exist before the first or after the last diconnected node in 
        // the ac_g, effectively grouping the empty nodes via partitioning.
        SRCPOS srcpos=WN_Get_Linenum(in_loop);
        if (no_in_arcs && no_out_arcs) {
          if ((ac_g->Get_In_Edge(scc_id) == 0) &&
              (ac_g->Get_Out_Edge(scc_id) == 0)) {
            if (LNO_Verbose) {
              char message[200];
              sprintf(message, "bypassing scc edge: %d with %d, reason: %s",
                      i, scc_id, "both edges are disconnected");
              distribute_verbose_info(FALSE,srcpos,fission_level,message);
            }
            continue;
          }
        } else if(level[scc_id] > level[i]) {
          // As per Muraoka's theses, this is the tree height test,
          // if we fail to reduce the tree height and the ith node
          // is not isolated, we do not provide a partition.
          if (level[i] != 0) {
            if (LNO_Verbose) {
              char message[200];
              sprintf(message, "bypassing scc edge: %d with %d: reason: %s",
                      i, scc_id, "no tree height reduction");
              distribute_verbose_info(FALSE,srcpos,fission_level,message);
            }
            continue;
          }
        }
        // see if the SCC graph has back edges from scc node i
        if (has_back_edges(sdg, dep_g_p, dep_graph_map, 
                           scc_first, j, queue, total_scc))
          continue;

        if (has_dist_list == FALSE)
          has_dist_list = TRUE;

        dist_array[j] = (long)scc_first;
        if (LNO_Verbose) {
          char message[200];
          sprintf(message, "partition found between scc(%d,%d) and scc(%d,%d)",
                  scc_id, level[scc_id], i, level[i]);
          distribute_verbose_info(FALSE,srcpos,fission_level,message);
        }
      }
    }
  }

  // only reorder if we have a distribution list
  if (has_dist_list) {
    // Reorder the statements in the loop according to SCCs, but in the
    // current ordering schema, this cleans up overlap.
    if (num_stmts > total_scc) {
      for (i=0; i<total_scc; ++i) {
        UINT cur = queue[i];
        FF_STMT_NODE* stmt_node;
        while(stmt_node = scc[cur].Remove_Headnode()) {
          stmt=stmt_node->Get_Stmt();
          LWN_Insert_Block_Before(loop_body, NULL, 
                                  LWN_Extract_From_Block(stmt));
        }
      }
    }

    // Now build the distribution list
    for (j=total_scc-1; j>=0; j--) {
      WN *scc_first = (WN*)dist_array[j];
      if (scc_first) {
        WN *scc_last = WN_prev(scc_first);
        stl_1.Append(scc_last, &LNO_local_pool);
        stl_1.Append(scc_first, &LNO_local_pool);
      }
    }
  }

  CXX_DELETE_ARRAY(queue, pool);
  CXX_DELETE_ARRAY(dist_array, pool);
  CXX_DELETE_ARRAY(scc, pool);
  CXX_DELETE_ARRAY(level, pool);
  CXX_DELETE(ac_g, pool);

  WN_MAP_Delete(dep_graph_map);
  CXX_DELETE(dep_g_p, pool);
}

extern void
Get_Distribution_List(FF_STMT_LIST& stl_1, WN* in_loop, UINT8 fission_level)
{
  MEM_POOL_Push(&FISSION_default_pool);

  WN_MAP sdm=WN_MAP_Create(&FISSION_default_pool);
  WN* outer_most_loop = in_loop;

  ARRAY_DIRECTED_GRAPH16* sdg=Build_Statement_Dependence_Graph(
        outer_most_loop, red_manager, adg, sdm,
        &FISSION_default_pool);
  Statement_Dependence_Graph = sdg;
  if (sdg==NULL) {
    DevWarn("Statement dependence graph problem");
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&FISSION_default_pool);    
    return;
  }

  Find_Partitions(stl_1, in_loop, fission_level, 
                  sdg, &FISSION_default_pool);

  Statement_Dependence_Graph = NULL;
  CXX_DELETE(sdg,&FISSION_default_pool);

  WN_MAP_Delete(sdm);
  MEM_POOL_Pop(&FISSION_default_pool);    
}

/**
*** Test fissioning the 'in_loop' based on two statement lists such that
*** the statements on different lists will be in different
*** loops afterwards.
***
*** Variations can be specified with 'opt_flag'
*** Return number of loops after fission. So a return value of '1'
*** indicates a failed fission.
**/

UINT32
Fission_Test(
WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
WN_MAP loop_map, FF_STMT_LIST *stl_1, FF_STMT_LIST *stl_2)
{
  WN* stmt;
  FF_STMT_NODE *stmt_node_p;

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Fission_Test()\n") );

  if (((DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map, in_loop))->Depth+1<
      fission_level) {
    Is_True(0,("Loop level not deep enough for fission with level %d\n",
      fission_level));
    return 1;
  }

  SRCPOS srcpos=WN_Get_Linenum(in_loop);

  WN* wn = in_loop;
  WN* outer_most_loop = in_loop;

  INT i;
  for (i=1; i<fission_level; i++) {
    outer_most_loop = LWN_Get_Parent(LWN_Get_Parent(outer_most_loop));
    if (WN_opcode(outer_most_loop)!=OPC_DO_LOOP) {
      Is_True(0,("Non-perfect loop for fission\n"));
      return 1;
    }
  }

  if (!Do_Loop_Is_Good(outer_most_loop) || Do_Loop_Has_Calls(outer_most_loop)
      || Do_Loop_Has_Gotos(outer_most_loop)) {
    // Is_True(0, ("Bad loop passed to Fission_Test."));
    if (LNO_Verbose)
      fission_verbose_info(FALSE,srcpos,fission_level,
       "Loops containing calls, exits, bad mem, or gotos cannot be fissioned.");
    if (LNO_Analysis)
      fission_analysis_info(FALSE,srcpos,fission_level,
       "Loops containing calls, exits, bad mem, or gotos cannot be fissioned.");
    if ( LNO_Tlog )
      fission_tlog_info(Failed, in_loop, fission_level,
       "Loops containing calls, exits, bad mem, or gotos cannot be fissioned.");

    return 1;
  }

  MEM_POOL_Push(&FISSION_default_pool);

  DYN_ARRAY<FF_STMT_LIST> loop(&FISSION_default_pool);

  WN_MAP sdm=WN_MAP_Create(&FISSION_default_pool);

  ARRAY_DIRECTED_GRAPH16* sdg=Build_Statement_Dependence_Graph(
	outer_most_loop, red_manager, adg, sdm,
        &FISSION_default_pool);
  Statement_Dependence_Graph = sdg; 
  if (sdg==NULL) {
    DevWarn("Statement dependence graph problem");
    loop.Free_array();
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&FISSION_default_pool);
    return 1;
  }

  // first we find if there is any backward dependences at outer loops
  // which prevents fission in the inner loop
  for (i=1; i<fission_level; i++) {
    WN* block = LWN_Get_Parent(wn);
    WN2INT* up_hash_table=
      CXX_NEW(WN2INT(50, &FISSION_default_pool),&FISSION_default_pool);
    WN* stmt = 0;
    for (stmt = WN_first(block); stmt && stmt != WN_next(wn);
	 stmt=WN_next(stmt)) {
      up_hash_table->Enter(stmt, 1);
    }

    for (stmt=wn; stmt; stmt=WN_next(stmt)) {

      EINDEX16 dep_e = sdg->Get_Out_Edge(sdg->Get_Vertex(stmt));
      while (dep_e) {
        VINDEX16 source_v=sdg->Get_Source(dep_e);
        VINDEX16 sink_v=sdg->Get_Sink(dep_e);
        WN* source_wn=sdg->Get_Wn(source_v);
        WN* sink_wn=sdg->Get_Wn(sink_v);
        if (up_hash_table->Find(sink_wn)==1)
	  if (source_v!=sink_v) {
	    if (LNO_Verbose || LNO_Analysis) {
              char message[200];
              sprintf(message,
        "Fission failed due to dependence from line %d to line %d.",
                Srcpos_To_Line(LWN_Get_Linenum(source_wn)),
                Srcpos_To_Line(LWN_Get_Linenum(sink_wn)));
              if (LNO_Verbose)
                fission_verbose_info(FALSE,srcpos,fission_level,message);
              if (LNO_Analysis)
                fission_analysis_info(FALSE,srcpos,fission_level,message);
              if ( LNO_Tlog )
                fission_tlog_info(Failed, in_loop, fission_level, message);
            }
            loop.Free_array();
            CXX_DELETE(sdg,&FISSION_default_pool);
            WN_MAP_Delete(sdm);
            MEM_POOL_Pop(&FISSION_default_pool);
	    return 1;
	  }
        dep_e = sdg->Get_Next_Out_Edge(dep_e);
      }

    }

    wn = LWN_Get_Parent(LWN_Get_Parent(wn));
  }

  Form_Loops(in_loop,opt_flag,fission_level, stl_1, stl_2, sdg, loop,
             &FISSION_default_pool);

  mUINT16 total_loops = loop.Lastidx() + 1;

  // append 3rd and remaining loops to the 2nd loop
  if (opt_flag & FISSION_BIT_TWO_LOOPS_ONLY) {
  
    for (i=2; i<total_loops; i++) {
      while(stmt_node_p = loop[i].Remove_Headnode()) {
        loop[1].Append(stmt_node_p);
      }
      loop.Decidx();
    }
    total_loops=2;
  }


  for (i=0; i<total_loops; i++) {

    while(stmt_node_p = loop[i].Remove_Headnode()) {
      stmt = stmt_node_p->Get_Stmt();
      WN_MAP_Set(loop_map, stmt, (void*)(INTPTR)(i+1));
    }
  }

  Statement_Dependence_Graph = NULL;
  CXX_DELETE(sdg,&FISSION_default_pool);

  loop.Free_array();
  WN_MAP_Delete(sdm);

  MEM_POOL_Pop(&FISSION_default_pool);

  return total_loops;

}

/**
*** Fission 'in_loop' after statement 'stmt'.
**/

extern FISSION_FUSION_STATUS
Fission(WN* in_loop, WN* stmt, UINT8 fission_level)
{

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Fission()\n") );

  WN *loop_body = WN_do_body(in_loop);

  FmtAssert(LWN_Get_Parent(stmt)==loop_body, 
    ("Statement not a immediate child of loop in Fission\n"));

  // check if stmt is the last stmt in in_loop
  if (WN_last(loop_body)==stmt)
    return Failed;

  // form two statement lists consisting statements before (and include)
  // 'stmt' and after 'stmt', respectively
  FF_STMT_LIST stl_1;
  WN *wn;
  for (wn=WN_first(loop_body); wn!=stmt; wn=WN_next(wn)) {
    stl_1.Append(wn,&FISSION_default_pool);
  }
  stl_1.Append(stmt,&FISSION_default_pool);
  FF_STMT_LIST stl_2;
  for (wn=WN_next(stmt); wn; wn=WN_next(wn)) {
    stl_2.Append(wn,&FISSION_default_pool);
  }

  // now fission based on the two statement lists
  return Fission(in_loop, OPT_FLAG_STMT_SIMPLE, fission_level, -1, 0,
    &stl_1, &stl_2);
}

/**
*** Without re-ordering, fission 'in_loop' to separate statements 'stmt1'
*** and 'stmt2'. Assuming 'stmt1' preceeds 'stmt2'. Statements in
*** between can be in either fissioned loops.
**/

extern FISSION_FUSION_STATUS
Fission(WN* in_loop, WN* stmt1, WN* stmt2, 
        UINT8 fission_level, BOOL partition_based)
{

  if (stmt1 == stmt2)
    return Failed;

  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Fission()\n") );

  WN *loop_body = WN_do_body(in_loop);

  FmtAssert(LWN_Get_Parent(stmt1)==loop_body, 
    ("Statement not a immediate child of loop in Fission\n"));
  FmtAssert(LWN_Get_Parent(stmt2)==loop_body, 
    ("Statement not a immediate child of loop in Fission\n"));
 
  // form two statement lists which contains statements up to 'stmt1'
  // and statements after 'stmt2', respectively
  FF_STMT_LIST stl_1;
  FF_STMT_LIST stl_2;
  WN *wn;
  if (partition_based) {
    for (wn=WN_first(loop_body); wn!=stmt2; wn=WN_next(wn)) {
      stl_1.Append(wn,&FISSION_default_pool);
    }

    for (wn=stmt2; wn; wn=WN_next(wn)) {
      stl_2.Append(wn,&FISSION_default_pool);
    }
  } else {
    for (wn=WN_first(loop_body); wn!=stmt1; wn=WN_next(wn)) {
      stl_1.Append(wn,&FISSION_default_pool);
    }
    stl_1.Append(stmt1,&FISSION_default_pool);

    for (wn=WN_next(stmt1); wn && wn!=stmt2; wn=WN_next(wn));

    FmtAssert(wn==stmt2,
      ("Incorrect ordering of stmt1 and stmt2 in Fission()\n"));

    for (wn=stmt2; wn; wn=WN_next(wn)) {
      stl_2.Append(wn,&FISSION_default_pool);
    }
  }

  // now fission 'in_loop' without reordering based on the
  // two statement lists
  if (partition_based) {
    return Fission(in_loop, 
      FISSION_BIT_TWO_LOOPS_ONLY|FISSION_BIT_PARTITION_BASED, 
      fission_level, -1, 0,
      &stl_1, &stl_2);
  } else {
    return Fission(in_loop, FISSION_BIT_TWO_LOOPS_ONLY, fission_level, -1, 0,
      &stl_1, &stl_2);
  }
}

/**
*** Fission the 'in_loop' based on two statement lists such that
*** the statements on different lists will be in different
*** loops afterwards.
***
*** Variations can be specified with 'opt_flag'
**/

extern FISSION_FUSION_STATUS
Fission(WN* in_loop, mUINT16 opt_flag, UINT8 fission_level,
WN_MAP loop_map, UINT32 total_loops, FF_STMT_LIST *stl_1, FF_STMT_LIST *stl_2)
{
  FF_STMT_NODE *stmt_node_p;
  FmtAssert(WN_opcode(in_loop)==OPC_DO_LOOP, 
    ("non-loop input node in Fission()\n") );

  if (Do_Loop_Depth(in_loop)+1<fission_level) {
    Is_True(0, ("Loop level not deep enough for fission with level %d\n",
           fission_level));
    return Failed;
  }

  DO_LOOP_INFO* dli=Get_Do_Loop_Info(in_loop);
  if (dli->Has_Gotos || dli->Has_Calls) {
    Is_True(0,("Loop with gotos or calls passed to Fission().\n"));
    return Failed;
  }
  SRCPOS srcpos=WN_Get_Linenum(in_loop);

  if (dli->No_Fusion || Do_Loop_Is_Mp(in_loop)) {
    // pv 577829.
    if (LNO_Verbose)
      fission_verbose_info(FALSE,srcpos,fission_level,
        "No_Fusion or MP, Loop cannot be fissioned.");
    if (LNO_Analysis)
      fission_analysis_info(FALSE,srcpos,fission_level,
        "No_Fusion or MP, Loop cannot be fissioned.");
    if ( LNO_Tlog )
      fission_tlog_info(Failed, in_loop, fission_level,
        "No_Fusion or MP, Loop cannot be fissioned.");
    return Failed;
  }

  WN* wn = in_loop;
  WN* outer_most_loop = in_loop;

  INT i;
  for (i=1; i<fission_level; i++) {
    outer_most_loop = LWN_Get_Parent(LWN_Get_Parent(outer_most_loop));
    FmtAssert (WN_opcode(outer_most_loop)==OPC_DO_LOOP,
      ("Non-perfect loop for fission\n"));
  }

  if (!Do_Loop_Is_Good(outer_most_loop) || Do_Loop_Has_Calls(outer_most_loop)
      || Do_Loop_Has_Gotos(outer_most_loop)) {
    // Is_True(0, ("Bad loop passed to Fission."));
    if (LNO_Verbose)
      fission_verbose_info(FALSE,srcpos,fission_level,
        "Loops containing calls, exits, or gotos cannot be fissioned.");
    if (LNO_Analysis)
      fission_analysis_info(FALSE,srcpos,fission_level,
        "Loops containing calls, exits, or gotos cannot be fissioned.");
    if ( LNO_Tlog )
      fission_tlog_info(Failed, in_loop, fission_level,
        "Loops containing calls, exits, or gotos cannot be fissioned.");
    return Failed;
  }

  if (WN_next(WN_first(WN_do_body(in_loop)))==NULL) {
    if (LNO_Verbose)
      fission_verbose_info(FALSE,srcpos,fission_level,
        "Loop has only one statement.");
    if (LNO_Analysis)
      fission_analysis_info(FALSE,srcpos,fission_level,
        "Loop has only one statement.");
    if ( LNO_Tlog )
      fission_tlog_info(Failed, in_loop, fission_level,
        "Loop has only one statement.");
    return Failed;
  }

  MEM_POOL_Push(&FISSION_default_pool);

  WN_MAP sdm=WN_MAP_Create(&FISSION_default_pool);

  ARRAY_DIRECTED_GRAPH16* sdg=Build_Statement_Dependence_Graph(
	outer_most_loop, red_manager, adg, sdm,
        &FISSION_default_pool);
  Statement_Dependence_Graph = sdg;  
  if (sdg==NULL) {
    DevWarn("Statement dependence graph problem");
    WN_MAP_Delete(sdm);
    MEM_POOL_Pop(&FISSION_default_pool);
    if (fission_level==1)
      return Failed;
    else
      return Try_Level_By_Level;
  }

  DYN_ARRAY<FF_STMT_LIST> loop(&FISSION_default_pool);

  // for each loop level, check if there is any backward dependences
  // which prevents from fission
  for (i=1; i<fission_level; i++) {
    WN* block = LWN_Get_Parent(wn);
    WN2INT* up_hash_table=
      CXX_NEW (WN2INT(50, &FISSION_default_pool), &FISSION_default_pool);
    WN* stmt = 0;
    for (stmt = WN_first(block); stmt && stmt != WN_next(wn);
	 stmt=WN_next(stmt)) {
      up_hash_table->Enter(stmt, 1);
    }

    for (stmt=wn; stmt; stmt=WN_next(stmt)) {

      VINDEX16 v = sdg->Get_Vertex(stmt);
      if (v==0) { // depends on everything else
	if (LNO_Verbose || LNO_Analysis) {
          char message[200];
          sprintf(message,
        "Fission failed due to conservative dependence from line %d.",
            Srcpos_To_Line(LWN_Get_Linenum(stmt)));
          if (LNO_Verbose)
            fission_verbose_info(FALSE,srcpos,fission_level,message);
          if (LNO_Analysis)
            fission_analysis_info(FALSE,srcpos,fission_level,message);
          if ( LNO_Tlog )
            fission_tlog_info(Failed, in_loop, fission_level,message);
        }
        WN_MAP_Delete(sdm);
        loop.Free_array();
    	CXX_DELETE(sdg,&FISSION_default_pool);
        MEM_POOL_Pop(&FISSION_default_pool);
	return Try_Level_By_Level;
      }
      EINDEX16 dep_e = sdg->Get_Out_Edge(sdg->Get_Vertex(stmt));
      while (dep_e) {
        VINDEX16 source_v=sdg->Get_Source(dep_e);
        VINDEX16 sink_v=sdg->Get_Sink(dep_e);
        WN* source_wn=sdg->Get_Wn(source_v);
        WN* sink_wn=sdg->Get_Wn(sink_v);
        if (up_hash_table->Find(sink_wn)==1)
	  if (source_v!=sink_v) {
	    if (LNO_Verbose || LNO_Analysis) {
              char message[200];
              sprintf(message,
        "Fission failed due to dependence from line %d to line %d.",
                Srcpos_To_Line(LWN_Get_Linenum(source_wn)),
                Srcpos_To_Line(LWN_Get_Linenum(sink_wn)));
              if (LNO_Verbose)
                fission_verbose_info(FALSE,srcpos,fission_level,message);
              if (LNO_Analysis)
                fission_analysis_info(FALSE,srcpos,fission_level,message);
              if ( LNO_Tlog )
                fission_tlog_info(Failed, in_loop, fission_level,message);
            }
            WN_MAP_Delete(sdm);
            loop.Free_array();
    	    CXX_DELETE(sdg,&FISSION_default_pool);
            MEM_POOL_Pop(&FISSION_default_pool);
	    return Try_Level_By_Level;
	  }
        dep_e = sdg->Get_Next_Out_Edge(dep_e);
      }

    }
    wn = LWN_Get_Parent(LWN_Get_Parent(wn));
  }

  BOOL no_input_map = (loop_map== -1);
  if (no_input_map) {
    // no loop mapping is provided
    // create a local loop map
    Form_Loops(in_loop,opt_flag,fission_level, stl_1, stl_2, sdg,loop,
               &FISSION_default_pool);
    total_loops = loop.Lastidx() + 1;

    // append 3rd and remaining loops to the 2nd loop
    if ((opt_flag & FISSION_BIT_TWO_LOOPS_ONLY) && (total_loops>2)) {
  
      for (i=2; i<total_loops; i++) {
        while(stmt_node_p = loop[i].Remove_Headnode()) {
          loop[1].Append(stmt_node_p);
        }
      }
      for (i=2; i<total_loops; i++)
        loop.Decidx();
      total_loops=2;
    }

  } else {

    loop.Initidx(total_loops-1);
    WN* loop_body = WN_do_body(in_loop);
    for (wn=WN_first(loop_body); wn; wn=WN_next(wn)) {
      INT loop_id = (INT32)(INTPTR)WN_MAP_Get(loop_map, wn)-1;
      loop[loop_id].Append(wn, &FISSION_default_pool);
    }
    
  }

  if (total_loops==1) { 
    if (((opt_flag & FISSION_BIT_PERFECT_NESTING) ||
      (opt_flag & FISSION_BIT_SINGLY_NESTING)) &&
      (Get_Only_Loop_Inside(in_loop,FALSE) || Do_Loop_Is_Inner(in_loop))) {
      if (LNO_Verbose)
        fission_verbose_info(FALSE,srcpos,fission_level,
          "Loop is already in the requested form.");
      if (LNO_Analysis)
        fission_analysis_info(FALSE,srcpos,fission_level,
          "Loop is already in the requested form.");
      if ( LNO_Tlog )
        fission_tlog_info(Failed, in_loop, fission_level,
          "Loop is already in the requested form.");
    } else {
      if (LNO_Verbose)
        fission_verbose_info(FALSE,srcpos,fission_level,
          "Failed because of a dependence cycle.");
      if (LNO_Analysis)
        fission_analysis_info(FALSE,srcpos,fission_level,
          "Failed because of a dependence cycle.");
      if ( LNO_Tlog )
        fission_tlog_info(Failed, in_loop, fission_level,
          "Failed because of a dependence cycle.");
    }
    WN_MAP_Delete(sdm);
    loop.Free_array();
    CXX_DELETE(sdg,&FISSION_default_pool);
    MEM_POOL_Pop(&FISSION_default_pool);
    return Failed;
  }

  // now really fission loops and update the dependence info

  INT clone_count = loop.Elements() - 1; 
  Separate_And_Update(in_loop, loop, fission_level);
  
  if (LNO_Test_Dump) sdg->Print(stdout);
  if (LNO_Test_Dump) adg->Print(stdout);

  Statement_Dependence_Graph = NULL;  
  CXX_DELETE(sdg,&FISSION_default_pool);
  WN_MAP_Delete(sdm);
  loop.Free_array();
  MEM_POOL_Pop(&FISSION_default_pool);

  if (total_loops>1) {
    if (LNO_Verbose)
      fission_verbose_info(TRUE,srcpos,fission_level,
        "Successfully fissioned !!");
    if (LNO_Analysis)
      fission_analysis_info(TRUE,srcpos,fission_level,
        "Successfully fissioned !!");
    if ( LNO_Tlog )
      fission_tlog_info(Succeeded, in_loop, fission_level,
        "Successfully fissioned !!");
    return Succeeded;
  } else
    return Failed;

}

/**
*** Initialization routine for fission.
**/

extern void Fission_Init()
{
  if (!fission_initialized) {
    MEM_POOL_Initialize(&FISSION_default_pool,"FISSION_default_pool",FALSE);
    MEM_POOL_Push(&FISSION_default_pool);
    adg=Array_Dependence_Graph;
    fission_initialized=TRUE;
  }
}

extern void Fission_Finish()
{
  if (fission_initialized) {
    MEM_POOL_Pop(&FISSION_default_pool);
    MEM_POOL_Delete(&FISSION_default_pool);
    fission_initialized=FALSE;
  }
}

