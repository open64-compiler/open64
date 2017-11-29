/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: be/lno/SCCS/s.ff_utils.cxx $ $Revision: 1.17 $";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include "wn.h"
#include "opcode.h"
#include "lwn_util.h"
#include "lnopt_main.h"
#include "wn_map.h"
#include "dep_graph.h"
#include "fission.h"
#include "ff_utils.h"
#include "scalar_expand.h"
#include "lnoutils.h"
#include "opt_du.h"
#include "reduc.h"
#include "name.h"
#include "access_vector.h"
#include "inner_fission.h"
#include "lego_util.h"
#include "lego_opts.h"
#include "wn_simp.h"

#define ESTIMATED_SIZE 100      // used to initialize hash table, etc.

static MEM_POOL FF_default_pool;
static BOOL mem_pool_initialized=FALSE;

static UINT64 ref_counter=0;

static void Update_Loop_Info (WN* wn,
                              SYMBOL* ref_symbol,
                              SYMBOL* new_symbol);


ARRAY_DIRECTED_GRAPH16* Statement_Dependence_Graph; 

static BOOL Is_Reduction_In_Prallel_Region(WN* scalar_ref)
{
  WN* wn=scalar_ref;
  while (wn) {
    WN* mp_region=Get_MP_Region(wn);
    if (!mp_region)
      return FALSE;
    WN* pragmas=WN_region_pragmas(mp_region);
    WN* next_wn=WN_first(pragmas);
    while (next_wn) {
      if (WN_opcode(next_wn)==OPC_PRAGMA)
        if ((WN_PRAGMA_ID)WN_pragma(next_wn)==WN_PRAGMA_REDUCTION) {
	  if (WN_st(scalar_ref)==WN_st(next_wn))
	    return TRUE;
        }
      next_wn=WN_next(next_wn);
    }
    wn=LWN_Get_Parent(mp_region);
  }
  return FALSE;
}

#ifdef KEY
static BOOL Is_Shared_Or_Reduction_In_Prallel_Region(WN* scalar_ref)
{
  WN* wn=scalar_ref;
  while (wn) {
    WN* mp_region=Get_MP_Region(wn);
    if (!mp_region)
      return FALSE;
    WN* pragmas=WN_region_pragmas(mp_region);
    WN* next_wn=WN_first(pragmas);
    while (next_wn) {
      if (WN_opcode(next_wn)==OPC_PRAGMA)
        if ((WN_PRAGMA_ID)WN_pragma(next_wn)==WN_PRAGMA_SHARED ||
	    (WN_PRAGMA_ID)WN_pragma(next_wn)==WN_PRAGMA_REDUCTION) {
	  if (WN_st(scalar_ref)==WN_st(next_wn))
	    return TRUE;
        }
      next_wn=WN_next(next_wn);
    }
    wn=LWN_Get_Parent(mp_region);
  }
  return FALSE;
}
#endif
extern BOOL Edge_Is_Reduction_Dependence(
EINDEX16 edge,
ARRAY_DIRECTED_GRAPH16 *dg,
REDUCTION_MANAGER *Red_Mgr)
{
  BOOL is_reduction_dependence=FALSE;

  if (Red_Mgr) {
    WN* source=dg->Get_Wn(dg->Get_Source(edge));
    REDUCTION_TYPE red_type= Red_Mgr->Which_Reduction(source);
    if (red_type!=RED_NONE) {
      WN* sink=dg->Get_Wn(dg->Get_Sink(edge));
      if (red_type==Red_Mgr->Which_Reduction(sink))
        is_reduction_dependence=TRUE;
    }
  }

  return is_reduction_dependence;
}

// Given statements 'stmt1', 'stmt2' at the stmt_level, this procedure
// adds corresponding edge in the statement dependence graph 'sdg'
// and adjust the level for that edge
static EINDEX16 Add_Stmt_Dependence(WN* stmt1, WN* stmt2, UINT stmt_level,
ARRAY_DIRECTED_GRAPH16 *sdg)
{

  WN* parent_loop=LWN_Get_Parent(LWN_Get_Parent(stmt1));

  // do not add dep edges for stmts not in the same loop
  if (WN_opcode(parent_loop)!=OPC_DO_LOOP) {
    Is_True(WN_opcode(parent_loop)==OPC_FUNC_ENTRY,
      ("Parent is not loop or func_entry."));
    return 0;
  }

  // do not add dep edges for stmts not in the same good do loop
  if (Do_Loop_Is_Good(parent_loop)==FALSE || Do_Loop_Has_Gotos(parent_loop))
    return 0;

  VINDEX16 source_vertex=sdg->Get_Vertex(stmt1);
  VINDEX16 sink_vertex=sdg->Get_Vertex(stmt2);

  // if source or sink is not mapped, it is dependent on everything else
  if (source_vertex==0 || sink_vertex==0)
    return 0;
  
  EINDEX16 stmt_e = sdg->Get_Edge(source_vertex,sink_vertex);
  if (stmt_e) {
  
    UINT8 old_level = sdg->Level(stmt_e);
    if (stmt_level > old_level)
      sdg->Set_Level(stmt_e,stmt_level);
  } else {
  
    stmt_e = sdg->Add_Edge(sdg->Get_Vertex(stmt1),
                sdg->Get_Vertex(stmt2), stmt_level);
    if (stmt_e==0) {
      sdg->Delete_Vertex(source_vertex);
      sdg->Delete_Vertex(sink_vertex);
    }
  }
  return stmt_e;
}

// Given two WNs wn1, and wn2, returns two WNs which correspond to the
// vertices in the statement dependence graph
extern UINT Get_Stmt_For_Stmt_Dep_Graph(
    WN* source_wn, WN* sink_wn,
    WN** source_stmt_out, WN** sink_stmt_out) {

    FmtAssert (WN_opcode(source_wn)!=OPC_FUNC_ENTRY &&
             WN_opcode(sink_wn)!=OPC_FUNC_ENTRY,
             ("FUNC_ENTRY wn passed to Get_Stmt_For_Stmt_Dep_Graph().\n")); 

    FmtAssert (WN_opcode(LWN_Get_Parent(source_wn))!=OPC_FUNC_ENTRY &&
             WN_opcode(LWN_Get_Parent(sink_wn))!=OPC_FUNC_ENTRY,
      ("BLOCK wn of a FUNC_ENTRY passed to Get_Stmt_For_Stmt_Dep_Graph().\n")); 

    // reach the ancestor of wn1 which is under a BLOCK WN
    WN* source_stmt = 0;
    for (source_stmt=source_wn;
      WN_opcode(LWN_Get_Parent(source_stmt))!=OPC_BLOCK;
      source_stmt = LWN_Get_Parent(source_stmt));
    // reach the ancestor of wn1 which is a (simple or compound) statement
    // immediately enclosed by a loop or a function
    WN* source_parent = 0;
    for (source_parent=LWN_Get_Parent(LWN_Get_Parent(source_stmt));
         WN_opcode(source_parent)!=OPC_DO_LOOP &&
         WN_opcode(source_parent)!=OPC_FUNC_ENTRY;
         source_stmt = source_parent,
         source_parent=LWN_Get_Parent(LWN_Get_Parent(source_stmt)));
    // reach the ancestor of wn2 which is under a BLOCK WN
    WN* sink_stmt = 0;
    for (sink_stmt=sink_wn;
      WN_opcode(LWN_Get_Parent(sink_stmt))!=OPC_BLOCK;
      sink_stmt = LWN_Get_Parent(sink_stmt));
    // reach the ancestor of wn2 which is a (simple or compound) statement
    // immediately enclosed by a loop or a function
    WN* sink_parent = 0;
    for (sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt));
         WN_opcode(sink_parent)!=OPC_DO_LOOP &&
         WN_opcode(sink_parent)!=OPC_FUNC_ENTRY;
         sink_stmt = sink_parent,
         sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt)));

    mUINT32 source_level;
    mUINT32 sink_level;
    // get the level that the source_stmt is at
    if (WN_opcode(source_parent)==OPC_DO_LOOP)
      source_level = Do_Loop_Depth(source_parent)+1;
    else
      source_level = 0;
    // get the level that the sink_stmt is at
    if (WN_opcode(sink_parent)==OPC_DO_LOOP)
      sink_level = Do_Loop_Depth(sink_parent)+1;
    else
      sink_level = 0;
    // move source_stmt up if it is too deep
    while (source_level>sink_level) {
      do {
        source_stmt = LWN_Get_Parent(source_stmt);
      } while (WN_opcode(source_stmt) != OPC_DO_LOOP);
      source_level--;
    }
    if (source_level>0) // skip other SCFs such as IF
      while (WN_opcode(source_parent=
           LWN_Get_Parent(LWN_Get_Parent(source_stmt))) != OPC_DO_LOOP)
           source_stmt=source_parent;
    else
      while (WN_opcode(source_parent=
           LWN_Get_Parent(LWN_Get_Parent(source_stmt))) != OPC_FUNC_ENTRY)
           source_stmt=source_parent;
    // move sink_stmt up if it is too deep
    while (sink_level>source_level) {
      do {
        sink_stmt = LWN_Get_Parent(sink_stmt);
      } while (WN_opcode(sink_stmt) != OPC_DO_LOOP);
      sink_level--;
    }
    if (sink_level>0) // skip other SCFs such as IF
      while (WN_opcode(sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt)))
           != OPC_DO_LOOP)
           sink_stmt=sink_parent;
    else
      while (WN_opcode(sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt)))
           != OPC_FUNC_ENTRY)
           sink_stmt=sink_parent;

    // now source_stmt and sink_stmt are at the same level but probably
    // under different loops
    // so move both up until they are under the same loop or the function
    INT level=source_level;
    while (LWN_Get_Parent(source_stmt) != LWN_Get_Parent(sink_stmt)) {
      OPCODE opc;
      source_parent=LWN_Get_Parent(LWN_Get_Parent(source_stmt));
      do {
        source_stmt=source_parent;
        source_parent=LWN_Get_Parent(LWN_Get_Parent(source_stmt));
        opc=WN_opcode(source_parent);
      } while (opc!=OPC_DO_LOOP && opc!=OPC_FUNC_ENTRY);
      sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt));
      do {
        sink_stmt=sink_parent;
        sink_parent=LWN_Get_Parent(LWN_Get_Parent(sink_stmt));
        opc=WN_opcode(sink_parent);
      } while (opc!=OPC_DO_LOOP && opc!=OPC_FUNC_ENTRY);
      level--;
    }
    *source_stmt_out= source_stmt;
    *sink_stmt_out= sink_stmt;
    return (level);
}

// construct a statement dependence edge from the array dependence edge.
EINDEX16 Array_Edge_To_Level_Edge(EINDEX16 dep_e,
ARRAY_DIRECTED_GRAPH16 *adg, ARRAY_DIRECTED_GRAPH16 *sdg)
{

    VINDEX16 dep_source = adg->Get_Source(dep_e);
    VINDEX16 dep_sink = adg->Get_Sink(dep_e);
    WN* source_wn = adg->Get_Wn(dep_source);
    WN* sink_wn = adg->Get_Wn(dep_sink);
    WN* source_stmt;
    WN* sink_stmt;

    UINT level=Get_Stmt_For_Stmt_Dep_Graph(
               source_wn,sink_wn,&source_stmt,&sink_stmt);
    if (level==0)
      return 0;	// no enclosing loop
    // Statement does not have corresponding vertex in dep graph
    if (sdg->Get_Vertex(source_stmt)==0)
      return 0;
    // Statement does not have corresponding vertex in dep graph
    if (sdg->Get_Vertex(sink_stmt)==0)
      return 0;

    DEPV_ARRAY* depv_array = adg->Depv_Array(dep_e);
    UINT8 pos_level = 0;
    BOOL has_all_zero = FALSE;
    for (INT i=depv_array->Num_Vec()-1; i>=0; i--) {
      DEPV* depv = depv_array->Depv(i);
      INT j = 0;
      while (j<depv_array->Num_Dim() &&
	     DEP_Direction(DEPV_Dep(depv,j))==DIR_EQ) j++;
      //UINT8 current_level = j+1+depv_array->Num_Unused_Dim();
      UINT8 current_level = j+depv_array->Num_Unused_Dim();
      // TODO should consider the lexical order of the stmt when
      //      the dep is all 0
      if (current_level>pos_level)
	pos_level = current_level;
      if (j==depv_array->Num_Dim())
        has_all_zero = TRUE;
    }

    EINDEX16 stmt_e=Add_Stmt_Dependence(source_stmt,sink_stmt,pos_level,sdg);

    if (stmt_e && has_all_zero)
      sdg->Set_Level_Property(stmt_e,HAS_ALL_ZERO);

    return stmt_e;

}

// check if a vertex exist for 'wn' in 'sdg
// if it is not then create a new vertex
// return zero if error occurs
extern INT Map_Stmt_To_Level_Graph(WN* wn, ARRAY_DIRECTED_GRAPH16 *sdg) {
  
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_expression(opcode))
    return 1;
  if (opcode==OPC_LABEL || opcode==OPC_RETURN || opcode==OPC_GOTO
#ifdef KEY
      || opcode==OPC_GOTO_OUTER_BLOCK
#endif
     )
    return 1;

  VINDEX16 v=sdg->Get_Vertex(wn);
  if (!v)
    v=sdg->Add_Vertex(wn);
  if (!v)
    return 0;

  if (opcode==OPC_PRAGMA || opcode==OPC_XPRAGMA) {
    return 1;
  } else if (opcode==OPC_DO_LOOP) {
    wn=WN_do_body(wn);

    for (WN* stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
	 if (!Map_Stmt_To_Level_Graph(stmt,sdg))
	   return 0; 
    }
  } else if (opcode==OPC_REGION) {
    wn=WN_region_body(wn);

    for (WN* stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
	 if (!Map_Stmt_To_Level_Graph(stmt,sdg))
	   return 0; 
    }
  } else if (opcode==OPC_DO_WHILE || opcode==OPC_WHILE_DO) {
    wn=WN_while_body(wn);

    for (WN* stmt = WN_first(wn); stmt; stmt = WN_next(stmt)) {
	 if (!Map_Stmt_To_Level_Graph(stmt,sdg))
	   return 0; 
    }
  } else if (opcode==OPC_IF) {
    WN* then_body=WN_then(wn);

    WN* stmt = 0;
    for (stmt = WN_first(then_body); stmt; stmt = WN_next(stmt)) {
	 if (!Map_Stmt_To_Level_Graph(stmt,sdg))
	   return 0; 
    }
    WN* else_body=WN_else(wn);

    for (stmt = WN_first(else_body); stmt; stmt = WN_next(stmt)) {
	 if (!Map_Stmt_To_Level_Graph(stmt,sdg))
	   return 0; 
    }
  } 
  return 1;
}

// Copy a DOLOOP_STACK
static DOLOOP_STACK *Copy_Doloop_Stack(DOLOOP_STACK *orig,MEM_POOL *pool)
{
  DOLOOP_STACK *copy = CXX_NEW(DOLOOP_STACK(pool),pool);
  INT elements = orig->Elements();
  for (INT i=0; i<elements; i++) {
    copy->Push(orig->Bottom_nth(i));
  }
  return(copy);
}

extern void toplogical_reordering(WN* in_loop, UINT depth,
  ARRAY_DIRECTED_GRAPH16* adg) {

  if (!Do_Loop_Is_Good(in_loop) || Do_Loop_Has_Calls(in_loop) ||
       Do_Loop_Has_Gotos(in_loop)) {
    Is_True(0, ("Bad loop passed to toplogical_reordering().\n"));
    return;
  }
  
  if (!mem_pool_initialized) {
    MEM_POOL_Initialize(&FF_default_pool,"FF_default_pool",FALSE);
    mem_pool_initialized=TRUE;
  }

  MEM_POOL_Push(&FF_default_pool);

  // start a scope for deallocation of stack vars before mempool pop
  {
    WN* stmt;
			
    FF_STMT_NODE *stmt_node_p;
    DYN_ARRAY<FF_STMT_LIST> loop(&FF_default_pool);

    WN_MAP sdm=WN_MAP_Create(&FF_default_pool);
    ARRAY_DIRECTED_GRAPH16* sdg=Build_Statement_Dependence_Graph(
      in_loop, red_manager, adg, sdm, &FF_default_pool);
    Statement_Dependence_Graph = sdg; 
    if (sdg==NULL) {
      DevWarn("Statement dependence graph problem");
      WN_MAP_Delete(sdm);
      MEM_POOL_Pop(&FF_default_pool); // cannot be moved to earlier
      return;
    }
    Form_Loops(in_loop,OPT_FLAG_STMT_REORDERING,depth, NULL, NULL, sdg, loop,
               &FF_default_pool);

    for (INT i=1; i<=loop.Lastidx(); i++) {

      WN* loop_body = WN_do_body(in_loop);
      while(stmt_node_p = loop[i].Remove_Headnode()) {
        stmt = stmt_node_p->Get_Stmt();
        stmt = LWN_Extract_From_Block(stmt);
        LWN_Insert_Block_Before(loop_body,NULL,stmt);
      }

    }
    Statement_Dependence_Graph = NULL; 
    CXX_DELETE(sdg,&FF_default_pool);
    WN_MAP_Delete(sdm);
  }

  MEM_POOL_Pop(&FF_default_pool); // cannot be moved to earlier

}

static INT rename_counter=0;    // counter for generating unique name
                                // for renamed variables



// do the fix up for one level of a parallel region
static void Rename_Update_MP_Region(WN *region, WN *scalar_ref,
	ST* new_st,WN_OFFSET new_offset, WN *loop, 
	BOOL is_outermost_region)
{

  WN *tmp = WN_last(WN_region_pragmas(region));
  ST *old_st = WN_st(scalar_ref);
  WN_OFFSET old_offset = WN_offset(scalar_ref);
  BOOL done = FALSE;
  BOOL found_it = FALSE;
  BOOL no_need = FALSE;

  // search for old variable in a pragma, if we find it copy the pragma
  // for the new variable
  while (!done) {
    FmtAssert(tmp != NULL, 
      ("Rename_Update_MP_Region: Found null pointer")); 
    if (WN_opcode(tmp) == OPC_PRAGMA || WN_opcode(tmp) == OPC_XPRAGMA) {
      if ((WN_pragma(tmp) == WN_PRAGMA_COPYIN) ||
          (WN_pragma(tmp) == WN_PRAGMA_LOCAL) ||
          (WN_pragma(tmp) == WN_PRAGMA_LASTLOCAL) ||
          (WN_pragma(tmp) == WN_PRAGMA_FIRSTPRIVATE) ||
          (WN_pragma(tmp) == WN_PRAGMA_SHARED) ||
          (WN_pragma(tmp) == WN_PRAGMA_REDUCTION)) {
	if ((new_st == WN_st(tmp)) && 
	    (new_offset == WN_pragma_arg1(tmp))) {
	  done = TRUE; // we've already updated this up
	  found_it = TRUE;
        } else if ((old_st == WN_st(tmp)) && 
		     (old_offset == WN_pragma_arg1(tmp))) {
	  WN *new_pragma = WN_CreatePragma((WN_PRAGMA_ID) WN_pragma(tmp),
		new_st, (INT32) new_offset, WN_pragma_arg2(tmp));
          WN_Set_Linenum(new_pragma,WN_Get_Linenum(tmp));
          LWN_Insert_Block_After(LWN_Get_Parent(tmp),tmp,new_pragma);
	  done = TRUE;
	  found_it = TRUE;
        }
      } else if (WN_pragma(tmp) == WN_PRAGMA_MASTER_BEGIN) {
	no_need = TRUE;
      }
    }
    if (!done) tmp = WN_prev(tmp);
    if (!tmp) done = TRUE;
  }


  if (no_need) return;

  // the old variable was not found, so use default mechanisms
  // if the old variable was a loop variable, do nothing, these are special
  // else if it was a preg, add a local pragma
  // else add a shared pragma
  if (!found_it && is_outermost_region) { 
    if (loop && (WN_st(WN_index(loop)) == old_st) &&
	  (WN_offset(WN_index(loop)) == old_offset)) {
        return;
    }
    if (ST_class(old_st) != CLASS_PREG) {
      WN *new_pragma = WN_CreatePragma(WN_PRAGMA_SHARED,
		new_st, (INT32) new_offset, 0);
      WN_Set_Linenum(new_pragma,WN_Get_Linenum(region));
      LWN_Insert_Block_After(WN_region_pragmas(region),
			     WN_last(WN_region_pragmas(region))
			     ,new_pragma);
    }
  }
}

// is this region the outermost region
static BOOL Outer_Mp_Region(WN *region) 
{
  WN *wn = LWN_Get_Parent(region);
  while (wn) {
    if (Is_Mp_Region(wn)) return FALSE;
    wn = LWN_Get_Parent(wn);
  }
  return TRUE;
}

// Fix up the MP pragmas when renaming inside a parallel region
// Assumes the new offset and the new st are pregs
static void Rename_Update_MP(WN *scalar_ref,ST* new_st, 
						WN_OFFSET new_offset)
{
  WN *region = LWN_Get_Parent(scalar_ref);
  while (region) {
    if (Is_Mp_Region(region)) {
      Rename_Update_MP_Region(region,scalar_ref,new_st,new_offset,NULL,
	Outer_Mp_Region(region));
    } else if ((WN_opcode(region) == OPC_DO_LOOP) && Do_Loop_Is_Mp(region)) {
      WN *loop = region;
      region = LWN_Get_Parent(LWN_Get_Parent(region));
#ifdef KEY
      if (PU_cxx_lang(Get_Current_PU()) && Is_Eh_Or_Try_Region(region))
        region = LWN_Get_Parent(LWN_Get_Parent(region));
#endif
      Rename_Update_MP_Region(region,scalar_ref,new_st,new_offset,loop,
	Outer_Mp_Region(region));
    }
    region = LWN_Get_Parent(region);
  }
}

// Return TRUE if the ref is an input to inline assembly.
static BOOL Is_Asm_Input(WN *ref)
{
  WN *parent = LWN_Get_Parent(ref);
  return parent != NULL && WN_operator(parent) == OPR_ASM_INPUT;
}

// Renames a variable's name if all of its references are in a given loop
extern BOOL scalar_rename(WN* ref, HASH_TABLE<WN*,INT>* checked) {

  if (Get_Trace(TP_LNOPT, TT_LNO_NORENAME))
    return FALSE;

  INT can_rename=TRUE;
  SYMBOL ref_symbol(ref);

  // Find all the references to this variable which is connected
  // through some DU chains. These are the references which has to be
  // renamed together.
  STACK<WN*>* equivalence_class=
              Scalar_Equivalence_Class(ref,Du_Mgr,&LNO_local_pool);

  if (equivalence_class==NULL)
    return FALSE;

  TYPE_ID desc_type = WN_desc(equivalence_class->Top_nth(0));
  if (desc_type == MTYPE_M) {
     can_rename = FALSE;
  }

  // for each reference in the equivalence class, check if it is FUNC_ENTRY
  // and if it has the same name as the variable to be renamed
  // in addition, we do not rename for volatiles and scalars with
  // inconsistent descriptor type
  INT i;
  for (i=0; can_rename && i<equivalence_class->Elements(); i++) {
  
    WN* scalar_ref=equivalence_class->Top_nth(i);
    if (OPCODE_has_sym(WN_opcode(scalar_ref))) {
      SYMBOL temp_symbol(scalar_ref);
      OPERATOR opr=WN_operator(scalar_ref);
      if ((opr!=OPR_STID && opr!=OPR_LDID) || (ref_symbol!=temp_symbol))
        can_rename= FALSE;
      else if (TY_is_volatile(WN_ty(scalar_ref)))
	can_rename = FALSE;
      else if (WN_desc(scalar_ref)!=desc_type)
	can_rename = FALSE;
#ifndef KEY
      else if (Is_Reduction_In_Prallel_Region(scalar_ref))
	can_rename = FALSE;
#else
      // Bug 6904 - cannot rename scalars that are shared variables in an
      // enclosing MP region. This combines the test for reduction variables.
      else if (Contains_MP && 
	       Is_Shared_Or_Reduction_In_Prallel_Region(scalar_ref))
	can_rename = FALSE;
      // Can not create CVT/CVTL for bit-field later on. 
      else if ((opr == OPR_STID) && WN_desc(scalar_ref) == MTYPE_BS)
	can_rename = FALSE;
      // Bug 1258 - can not promote temporary storing floating point complex 
      // type to pseudo-register.
      else if ((opr == OPR_STID) && MTYPE_is_complex(WN_desc(scalar_ref)))
        can_rename = FALSE;
      // Avoid promoting asm input operands to a larger size.
      else if (opr == OPR_LDID && desc_type != Promote_Type(desc_type) &&
               Is_Asm_Input(scalar_ref))
        can_rename = FALSE;
#endif      
    } else
      can_rename= FALSE;
  }
  if (can_rename) {

    SYMBOL new_symbol=Create_Preg_Symbol(ref_symbol.Name(), ref_symbol.Type);
    if (LNO_Verbose) { 
      fprintf(stdout, " Renaming %s\n", ref_symbol.Name());
      fprintf(TFile, " Renaming %s\n", ref_symbol.Name());
    } 
    for (i=0; i<equivalence_class->Elements(); i++) {
      WN* scalar_ref=equivalence_class->Top_nth(i);
      if (Contains_MP) {
	Rename_Update_MP(scalar_ref,new_symbol.St(),new_symbol.WN_Offset());
      }

      OPCODE scalar_op = WN_opcode(scalar_ref);
      TYPE_ID desc = OPCODE_desc(scalar_op);

      if (WN_operator(scalar_ref)==OPR_STID) {
        // rename for WN_index() of loops
        WN* parent=LWN_Get_Parent(scalar_ref);
        if (WN_opcode(parent)==OPC_DO_LOOP && WN_start(parent)==scalar_ref) {
          WN_st_idx(WN_index(parent))=ST_st_idx(new_symbol.St());
          WN_offset(WN_index(parent))=new_symbol.WN_Offset();
          Update_Loop_Info (parent, &ref_symbol, &new_symbol);
        }

        // cvtl for short types
	// this handles the rule that pregs have no implicit conversions
	// while memory does 
        OPCODE kid_opcode = WN_opcode(WN_kid0(scalar_ref));
        if (desc != Promote_Type(desc)) {
	  // special case step increments, since can't have cvts there
	  if ((WN_opcode(LWN_Get_Parent(scalar_ref)) != OPC_DO_LOOP) ||
	      (scalar_ref != WN_step(LWN_Get_Parent(scalar_ref)))) {
            INT Size = MTYPE_bit_size(desc);
            TYPE_ID cvtl_desc=OPCODE_rtype(kid_opcode);
	    cvtl_desc=Mtype_TransferSign(desc,cvtl_desc);
            OPCODE cvtl_o = OPCODE_make_op(OPR_CVTL,cvtl_desc,MTYPE_V);
            BOOL simp_state_save = WN_Simplifier_Enable(FALSE);
            WN *cvtl = LWN_CreateExp1(cvtl_o,WN_kid0(scalar_ref));
            WN_cvtl_bits(cvtl) = Size;
	    WN_Simplifier_Enable(simp_state_save);
            WN_set_ty(scalar_ref,Be_Type_Tbl(Promote_Type(desc)));
            WN_kid0(scalar_ref) = cvtl;
            LWN_Set_Parent(cvtl,scalar_ref);
          }
        } else if (MTYPE_bit_size(desc) < 
		   MTYPE_bit_size(OPCODE_rtype(kid_opcode))) {
	  if ((WN_opcode(LWN_Get_Parent(scalar_ref)) != OPC_DO_LOOP) ||
	      (scalar_ref != WN_step(LWN_Get_Parent(scalar_ref)))) {
            OPCODE cvt_o=OPCODE_make_op(OPR_CVT,desc,OPCODE_rtype(kid_opcode));
	    WN *cvt = LWN_CreateExp1(cvt_o,WN_kid0(scalar_ref));
            WN_kid0(scalar_ref) = cvt;
            LWN_Set_Parent(cvt,scalar_ref);
          }
        }
      }
      WN_st_idx(scalar_ref)=ST_st_idx(new_symbol.St());
      WN_offset(scalar_ref)=new_symbol.WN_Offset();

#ifdef KEY
      // Fix for bug 1129
      if (WN_operator(scalar_ref) == OPR_LDID &&
	  MTYPE_bit_size(desc) == 32 &&
	  MTYPE_bit_size(WN_rtype(scalar_ref)) == 64) {
	// this handles the rule that pregs have no implicit conversions
	// while memory does 
	OPCODE cvt_o=OPCODE_make_op(OPR_CVT,WN_rtype(scalar_ref), desc);
	WN* parent = LWN_Get_Parent(scalar_ref);
	INT kid;
	for (kid = 0; kid < WN_kid_count(parent); kid ++)
	  if (WN_kid(parent, kid) == scalar_ref)
	    break;
	WN *cvt = LWN_CreateExp1(cvt_o,scalar_ref);
	LWN_Set_Parent(cvt,parent);
	WN_kid(parent, kid) = cvt;
	WN_set_rtype(scalar_ref, desc);
      } 
      else       
#endif
      WN_set_opcode(scalar_ref,OPCODE_make_op(
                OPCODE_operator(scalar_op),
		OPCODE_rtype(scalar_op),
		Promote_Type(OPCODE_desc(scalar_op))));
      WN_set_ty(scalar_ref,Be_Type_Tbl(Promote_Type(desc))); 
      WN_set_field_id(scalar_ref, 0); // fix 819155

      if (Alias_Mgr) {
        Create_alias(Alias_Mgr,scalar_ref);
      }
      if (checked)
        checked->Enter(scalar_ref,1);
    }

    equivalence_class->Free();
    return TRUE;
  } else {
  
    if (checked)
      for (i=0; i<equivalence_class->Elements(); i++) {
        WN* scalar_ref=equivalence_class->Top_nth(i);
        checked->Enter(scalar_ref,1);
      }

    equivalence_class->Free();
    return FALSE;
  }
}

/***********************************************************************
 *
 * Walk the tree, looking for inside loops whose lego_info/mp_info
 * contains a reference to the old ref_symbol. Rename it to be the new_symbol.
 *
 ***********************************************************************/
static void Update_Loop_Info (WN* wn,
                              SYMBOL* ref_symbol,
                              SYMBOL* new_symbol) {
  DO_LOOP_INFO* dli;
  LEGO_INFO* li;
  MP_INFO* mpi;

  switch (WN_operator(wn)) {
  case OPR_DO_LOOP:
  {
    dli = Get_Do_Loop_Info (wn);
    li = dli->Lego_Info;
    mpi = dli->Mp_Info;
    if (li) {
      SYMBOL* old_sym = li->Pid_Sym0();
      if (old_sym && (*old_sym == *(ref_symbol))) {
        *old_sym = *new_symbol;
      }
      else {
        old_sym = li->Pid_Sym1();
        if (old_sym && (*old_sym == *(ref_symbol))) {
          *old_sym = *new_symbol;
        }
      }
    }
    if (mpi) {
      SYMBOL* old_sym = mpi->Pid_Sym0();
      if (old_sym && (*old_sym == *(ref_symbol))) {
        *old_sym = *new_symbol;
      }
      else {
        old_sym = mpi->Pid_Sym1();
        if (old_sym && (*old_sym == *(ref_symbol))) {
          *old_sym = *new_symbol;
        }
      }
    }
    Update_Loop_Info (WN_do_body(wn), ref_symbol, new_symbol);
    break;
  }
  case OPR_BLOCK:
  {
    wn = WN_first(wn);
    while (wn) {
      Update_Loop_Info (wn, ref_symbol, new_symbol);
      wn = WN_next(wn);
    }
    break;
  }
  default:
  {
    for (INT i=0; i<WN_kid_count(wn); i++)
      Update_Loop_Info (WN_kid(wn, i), ref_symbol, new_symbol);
    break;
  }
  }
} /* Update_Loop_Info */

// Scan all scalar variables defined in the given loop and determine if
// they can be renamed.
extern BOOL Scalar_Variable_Renaming(WN* root) {

  if (Get_Trace(TP_LNOPT, TT_LNO_NORENAME))
    return FALSE;

  if (!mem_pool_initialized) {
    MEM_POOL_Initialize(&FF_default_pool,"FF_default_pool",FALSE);
    mem_pool_initialized=TRUE;
  }

  BOOL renamed=FALSE;

  MEM_POOL_Push(&FF_default_pool);

  {
    // Start a new scope so that stack allocated hash-table and dyn-array
    // are deallocated before the mem_pool_pop.

    // hash table used to record references that have been examined
    HASH_TABLE<WN*,INT> checked(ESTIMATED_SIZE, &FF_default_pool);

    DYN_ARRAY<WN*> wns(&FF_default_pool);

    // first we collect all scalar references in the loop
    REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&FF_default_pool),
                                  &FF_default_pool);
    REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&FF_default_pool),
                                 &FF_default_pool);
    SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&FF_default_pool),
                                       &FF_default_pool);
    SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&FF_default_pool),
                                      &FF_default_pool);
    SCALAR_REF_STACK *params =
       CXX_NEW(SCALAR_REF_STACK(&FF_default_pool), &FF_default_pool);
    DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&FF_default_pool),
                                &FF_default_pool);
    Build_Doloop_Stack(root, stack);
    Init_Ref_Stmt_Counter();
    New_Gather_References(root,writes,reads,stack,
       scalar_writes,scalar_reads,params,&FF_default_pool,
       Gather_Scalar_Refs);

    // then try to rename each scalar variable defined in the loop if it
    // has not been tried to be renamed
    for (INT sj=0; sj<scalar_writes->Elements(); sj++) {
     SCALAR_NODE *sjnode = scalar_writes->Bottom_nth(sj);
     for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
      WN* ref=sjnode->Bottom_nth(sjj)->Wn;
      if (WN_operator(ref)==OPR_STID && !checked.Find(ref)) {
        INT i;
        for (i=wns.Lastidx(); i>=0; i--) {
          SYMBOL symbol1(wns[i]);
          SYMBOL symbol2(ref);
          if (symbol1==symbol2) {
            if (scalar_rename(ref,&checked))
              renamed=TRUE;
            else if (scalar_rename(wns[i],&checked)) {
              wns[i]=ref;
              renamed=TRUE;
            }
            break;
          }
        }

        if (i== -1) {
          i=wns.Newidx();
          wns[i]=ref;
          STACK<WN*>* equivalence_class=
            Scalar_Equivalence_Class(ref,Du_Mgr,&FF_default_pool);          
          
          if (equivalence_class!=NULL)
          for (INT i=0; i<equivalence_class->Elements(); i++) {
            WN* scalar_ref=equivalence_class->Top_nth(i);
            checked.Enter(scalar_ref,1);
          }
        }
      }
     }
    }
  }

  MEM_POOL_Pop(&FF_default_pool);
  return renamed;
  
}

// Update DU-chains in Du_Mgr after fission..
// wn_starts, wn_ends, and wn_steps contain whirl trees of expressions
// in loop start, end, and step for each loop.
extern void Fission_DU_Update(DU_MANAGER* Du_Mgr, REDUCTION_MANAGER* Red_Mgr,
  WN** wn_starts, WN** wn_ends, WN** wn_steps,
  UINT total_loops, WN** loops, BOOL index_DU_to_first)
{

    MEM_POOL_Push(&LNO_local_pool);

    UINT depth=Do_Loop_Depth(loops[0]);

    // create new DU-chains for variables defined out of the loop but
    // are used in loop start, end, and step
    Unrolled_DU_Update(wn_starts, total_loops, depth, TRUE, FALSE);
    Unrolled_DU_Update(wn_ends, total_loops, depth, TRUE, FALSE);
    Unrolled_DU_Update(wn_steps, total_loops, depth, TRUE, FALSE);

    // next change use of old loop variable to use of new loop variables
    // by check whether each use of old loop variable is still in the
    // first loop. if it is not then change the use and def chains so
    // the use is associated with def in the new loop containing it
    WN* parent=LWN_Get_Parent(loops[0]);
    WN* old_loop_index_def=WN_start(loops[0]);
    USE_LIST *use_list=Du_Mgr->Du_Get_Use(old_loop_index_def);
    USE_LIST_ITER u_iter(use_list);
    for (DU_NODE *use_node=(DU_NODE *)u_iter.First();
         !u_iter.Is_Empty(); ) {
      WN* use=use_node->Wn();
      use_node=(DU_NODE *)u_iter.Next();
      WN* new_loop=use;
      while (WN_opcode(new_loop)!=OPC_DO_LOOP ||
             Do_Loop_Depth(new_loop)>depth)
        if (WN_opcode(new_loop)==OPC_FUNC_ENTRY)
          break;
        else
          new_loop=LWN_Get_Parent(new_loop);

      BOOL inside_loop=TRUE;
      if (WN_opcode(new_loop)==OPC_FUNC_ENTRY ||
          Do_Loop_Depth(new_loop)<depth){
        // if the use is not in any of the fissioned loop, its def
        // must come from the last fissioned loop
        new_loop=loops[(index_DU_to_first)? 0: total_loops-1];
	inside_loop=FALSE;
	}
      else {
        INT j= -1;
        for (INT i=0; i<total_loops; i++) {
          if (new_loop==loops[i]) {
            j=i;
            break;
          }
        }
        if (j== -1){
          new_loop=loops[(index_DU_to_first)? 0: total_loops-1];
	  inside_loop=FALSE;
	}
      }
      if (new_loop!=loops[0]) {
        Du_Mgr->Delete_Def_Use(WN_start(loops[0]),use);
        Du_Mgr->Delete_Def_Use(WN_step(loops[0]),use);
        Du_Mgr->Add_Def_Use(WN_start(new_loop),use);
        Du_Mgr->Add_Def_Use(WN_step(new_loop),use);
	if (inside_loop) Du_Mgr->Ud_Get_Def(use)->Set_loop_stmt(new_loop);
      }
    }


    // next we fix the DU chain for reductions in different loops after fission
    for (INT i=0; i<total_loops; i++) {
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
      Build_Doloop_Stack(loops[i], stack);

      // Make a list of all the references
      Init_Ref_Stmt_Counter();
      New_Gather_References(WN_do_body(loops[i]),writes,reads,stack,
        scalar_writes,scalar_reads,params,&LNO_local_pool,
	Gather_Scalar_Refs);

      for (INT si=0; si<scalar_reads->Elements(); si++) {
        SCALAR_NODE* sinode=scalar_reads->Bottom_nth(si);
        WN* use=sinode->Bottom_nth(0)->Wn;
          REDUCTION_TYPE red_type;
          if (Red_Mgr)
            red_type=Red_Mgr->Which_Reduction(use);
          else
            red_type=RED_NONE;

        STACK<WN*> write_stack(&LNO_local_pool);

        for (INT sj=0; sj<sinode->Elements(); sj++) {
          use=sinode->Bottom_nth(sj)->Wn;

          if (red_type!=RED_NONE) {  // if a read is part of a reduction
            if (red_type!=Red_Mgr->Which_Reduction(use)) {
              red_type=RED_NONE;
            } else {
	      WN *def = LWN_Get_Parent(use);
	      while (!OPCODE_is_store(WN_opcode(def))) {
		FmtAssert(def,("Can't find store of reduction \n"));
		def=LWN_Get_Parent(def);
              }
              write_stack.Push(def);
            }
          }

          DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(use);
          if (def_list && def_list->Loop_stmt()==loops[0])
            def_list->Set_loop_stmt(loops[i]);
        }
        if (red_type!=RED_NONE)
        for (INT rsj=0; rsj<write_stack.Elements(); rsj++) {
          WN* write=write_stack.Bottom_nth(rsj);
          for (INT sk=0; sk<sinode->Elements(); sk++) {
            WN* read=sinode->Bottom_nth(sk)->Wn;
            //Du_Mgr->Add_Def_Use(write,read);
            Du_Mgr->Du_Add_Use(write,read);
            Du_Mgr->Ud_Add_Def(read,write);
          }
        }
      }
    }

    MEM_POOL_Pop(&LNO_local_pool);

}

// return -1 if statement containing wn1 is lexically before
//              statement containing wn2
// return  0 if statement containing wn1 is the same as
//              statement containing wn2
// return +1 if statement containing wn1 is lexically after
//              statement containing wn2
extern INT Lexical_Order(WN* wn1, WN* wn2)
{
  WN* stmt1;
  WN* stmt2;

  // get the statements which are the ancestors of wn1 and wn2 and
  // are enclosed by the same loop or function body
  Get_Stmt_For_Stmt_Dep_Graph(wn1,wn2,&stmt1,&stmt2);

  if (stmt1==stmt2)
    return 0;

  // see if stmt2 is placed after stmt1
  do {
    stmt1=WN_next(stmt1);
    if (stmt1==stmt2)
      return -1;
  } while (stmt1);

  return 1;

}

// find a SCF with OPCODE 'opc' enclosed by 'parent_wn'. Used to determine if
// 'parent_wn' has inner loop or not after fusion, vintr_fission, etc.

extern WN* Find_SCF_Inside(WN* parent_wn, OPCODE opc) {
  WN* wn=LWN_Get_Next_SCF_Node(parent_wn);
  while (wn && Wn_Is_Inside(wn,parent_wn) && WN_opcode(wn)!=opc)
    wn=LWN_Get_Next_SCF_Node(wn);

  if (!wn || !Wn_Is_Inside(wn,parent_wn))
    return (WN*)NULL;
  else 
    return wn;
}

extern void FF_Mark_Inner_Loop_Info(WN* loop) {

  DO_LOOP_INFO* dli=Get_Do_Loop_Info(loop);
  WN* innerloop=Find_SCF_Inside(loop,OPC_DO_LOOP);
  if (innerloop)
    dli->Is_Inner=FALSE;
  else
    dli->Is_Inner=TRUE;
  return;
}


static UINT32 Gather_Ref_Stmt_Counter=0;

extern void Init_Ref_Stmt_Counter() {
  Gather_Ref_Stmt_Counter=0;
}

// Add all reads, writes and params in the tree 'wn' to the
// appropriate list.
// Return number of ARRAY references gathered or '-1'
// if no OPR_ARRAY found under ILOAD/ISTORE.

extern INT32 New_Gather_References(WN *wn, REF_LIST_STACK *writes,
	REF_LIST_STACK *reads, DOLOOP_STACK *stack,
	SCALAR_STACK *scalar_writes, SCALAR_STACK *scalar_reads,
	SCALAR_REF_STACK* params, MEM_POOL *pool,
        INT mode) 
{
  INT32 array_ref_count=0;
  WN *kid;

  if (OPCODE_is_stmt(WN_opcode(wn)))
    Gather_Ref_Stmt_Counter++;

  if (WN_opcode(wn) == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      INT32 count=
        New_Gather_References(kid,writes,reads,stack,scalar_writes,
		scalar_reads,params,pool,mode);
      if (count == -1)
	return -1;
      array_ref_count+= count;
      kid = WN_next(kid);
    }
    return array_ref_count;
  } 
  
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    stack->Push(wn);
  } else if (OPCODE_is_load(WN_opcode(wn))) {
    if (WN_kid_count(wn) == 1) {
      if (mode & Gather_Array_Refs) {
        array_ref_count++;
        DOLOOP_STACK *s = Copy_Doloop_Stack(stack,&LNO_local_pool);
	WN *addr = WN_kid0(wn);
	if (WN_operator(addr) == OPR_ADD) {
	  if (WN_operator(WN_kid0(addr)) == OPR_ARRAY) {
	    addr = WN_kid0(addr);
          } else {
	    addr = WN_kid1(addr);
          }
        }
	if (WN_operator(addr) != OPR_ARRAY)
	  return -1;
        WN *base = WN_array_base(addr);
        ST *st_base=Get_ST_Base(base);
        INT i;
        for (i=0;i<reads->Elements()&&
	         !(reads->Bottom_nth(i)->ST_Base==st_base); i++); 
        if (i==reads->Elements()) {
	  reads->Push(CXX_NEW(REFERENCE_LIST(st_base, wn),pool));
        } 
        reads->Bottom_nth(i)->Append(CXX_NEW(REFERENCE_NODE(wn,s,
	       Gather_Ref_Stmt_Counter), pool));
      }
    } else if (WN_operator(wn) == OPR_LDID) {
      if (mode & Gather_Scalar_Refs) 
      //if (WN_kid0(LWN_Get_Parent(wn))!=wn ||
          //WN_operator(LWN_Get_Parent(wn))!=OPR_ARRAY)
        scalar_reads->Add_Scalar(wn,Gather_Ref_Stmt_Counter);
    } 
  } else if (OPCODE_is_store(WN_opcode(wn))) {
    if (WN_kid_count(wn) == 2) {
      if (mode & Gather_Array_Refs) {
        array_ref_count++;
        DOLOOP_STACK *s = Copy_Doloop_Stack(stack,&LNO_local_pool);
	WN *addr = WN_kid1(wn);
	if (WN_operator(addr) == OPR_ADD) {
	  if (WN_operator(WN_kid0(addr)) == OPR_ARRAY) {
	    addr = WN_kid0(addr);
          } else {
	    addr = WN_kid1(addr);
          }
        }
	if (WN_operator(addr) != OPR_ARRAY)
	  return -1;
        WN *base = WN_array_base(addr);
        ST *st_base=Get_ST_Base(base);
        INT i;
        for (i=0;i<writes->Elements() && 
	         !(writes->Bottom_nth(i)->ST_Base==st_base); i++); 
        if (i==writes->Elements()) {
	  writes->Push(CXX_NEW(REFERENCE_LIST(st_base,
	   wn),pool));
        } 
        writes->Bottom_nth(i)->Append(CXX_NEW(REFERENCE_NODE(wn,s,
	       Gather_Ref_Stmt_Counter), pool));
      }
    } else if (WN_operator(wn) == OPR_STID) {
      if (mode & Gather_Scalar_Refs)
        scalar_writes->Add_Scalar(wn,Gather_Ref_Stmt_Counter);
    }
  } else if (WN_operator(wn)==OPR_PARM) {
    if (WN_Parm_By_Reference(wn) && (mode & Gather_Params)) {
      SCALAR_REF scalar_ref(wn,Gather_Ref_Stmt_Counter);
      params->Push(scalar_ref); 
    }
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    INT32 count=
      New_Gather_References(kid,writes,reads,stack,
           scalar_writes,scalar_reads,params,pool,mode);
    if (count == -1)
      return -1;
    array_ref_count+= count;
  }

  if (WN_opcode(wn) == OPC_DO_LOOP) {
    stack->Pop();
  }
  return array_ref_count;
}

static BOOL Generate_Pragma_Dependence_For_Statement_Dependence_Graph(
  WN* wn,
  WN* parent_loop,
  ARRAY_DIRECTED_GRAPH16* sdg
  )
{
  OPCODE opcode=WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    WN* kid = WN_first (wn);
    while (kid) {
      if (Generate_Pragma_Dependence_For_Statement_Dependence_Graph(
          kid,parent_loop,sdg)==0)
        return 0;
      kid = WN_next(kid);
    }
    return 1;
  } else if (opcode==OPC_PRAGMA || opcode==OPC_XPRAGMA) {
    // put in edges between pragmas and statements before/after them
    VINDEX16 v=sdg->Get_Vertex(wn);
    VINDEX16 v1;
    UINT level = Do_Loop_Depth(parent_loop)+1;
    WN* after=wn;
    v1=0;
    do {
      after=WN_next(after);
      if (after)
        v1=sdg->Get_Vertex(after);
    } while (after && !v1);
    if (v1) {
      EINDEX16 e;
      if (!(e=sdg->Get_Edge(v,v1))) {
        e=sdg->Add_Edge(v,v1,level);
        if (!e)
          return 0;
      }
      sdg->Set_Level_Property(e,HAS_ALL_ZERO);
      if (!(e=sdg->Get_Edge(v1,v))) {
        e=sdg->Add_Edge(v1,v,level);
        if (!e)
          return 0;
      }
    }
    return 1;
  } else { 
    if (WN_opcode(wn)==OPC_DO_LOOP)
      parent_loop=wn;
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN* kid = WN_kid(wn,kidno);
      if (Generate_Pragma_Dependence_For_Statement_Dependence_Graph(
          kid,parent_loop,sdg)==0)
        return 0;
    }
    return 1;
  }
}

extern BOOL Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
  WN* in_loop,
  SCALAR_STACK *scalar_reads,
  SCALAR_STACK *scalar_writes,
  SCALAR_REF_STACK *params,
  ARRAY_DIRECTED_GRAPH16* sdg,
  REDUCTION_MANAGER* Red_Mgr,
  BIT_VECTOR *Expandable_Scalar_Set,  // set of expandable scalars
  BINARY_TREE<NAME2BIT> *mapping_dictionary
                // dictionary which records mapping from
                // symbol names to bit positions
  )
{
  // construct scalar dependences first

  UINT32 stmt_level=Do_Loop_Depth(in_loop);

  INT si;
  for (si=0; si<scalar_writes->Elements(); si++) {
  
    SCALAR_NODE *sinode = scalar_writes->Bottom_nth(si);
    WN *scalar_write = sinode->Bottom_nth(0)->Wn;

    for (INT sj=si; sj<scalar_writes->Elements(); sj++) {
      SCALAR_NODE *sjnode = scalar_writes->Bottom_nth(sj);
      WN *scalar_write1 = sjnode->Bottom_nth(0)->Wn;

      if (Aliased(Alias_Mgr,scalar_write,scalar_write1)!=NOT_ALIASED) {
      // TODO: should check if Overlapped_Base should be used

        BOOL scalar_expandable=FALSE;

        SYMBOL sym(scalar_write);
        SYMBOL sym1(scalar_write1);
        if (sym==sym1) {
          if (mapping_dictionary) {
            // consult Expandable_Scalar_Set to see if this scalar is expandable
            NAME2BIT temp_map(scalar_write);
            const NAME2BIT *name_map=
              mapping_dictionary->Find(temp_map)->Get_Data();
            if (Expandable_Scalar_Set->Test(name_map->Get_Bit_Position())) {
              // if it is not scalar expandable, include all statements
              // in a dependence cycle so they will stay in the same SCC
              scalar_expandable=TRUE;
            }
          }
        }

        for (INT sii=0; sii<sinode->Elements(); sii++) {
          WN* wn=sinode->Bottom_nth(sii)->Wn;
          UINT ii_stmt_number=sinode->Bottom_nth(sii)->Statement_Number;
          WN* ii_stmt;

          REDUCTION_TYPE red_type;
          if (Red_Mgr)
            red_type=Red_Mgr->Which_Reduction(wn);
          else
            red_type=RED_NONE;

          for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
            WN* wn1=sjnode->Bottom_nth(sjj)->Wn;
            if (red_type!=RED_NONE && Red_Mgr->Which_Reduction(wn1)==red_type)
              continue;
            if (wn==wn1)
              continue;
            UINT jj_stmt_number=sjnode->Bottom_nth(sjj)->Statement_Number;

            WN* jj_stmt;
            stmt_level=Get_Stmt_For_Stmt_Dep_Graph(wn,wn1,&ii_stmt,&jj_stmt);

            EINDEX16 e1;
            EINDEX16 e2=1;
            if (ii_stmt_number < jj_stmt_number) {
              e1=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
              if (!scalar_expandable)
                e2=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
            } else {
              e1=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
              if (!scalar_expandable)
                e2=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
            }
            if (e1==0 || e2==0) {
              return FALSE;
            }
            if (e1!=0)
              sdg->Set_Level_Property(e1, HAS_ALL_ZERO);
          }
        }
      }
    }
  }

  for (si=0; si<scalar_reads->Elements(); si++) {
  
    SCALAR_NODE *sinode = scalar_reads->Bottom_nth(si);
    WN *scalar_read = sinode->Bottom_nth(0)->Wn;

    for (INT sj=0; sj<scalar_writes->Elements(); sj++) {
      SCALAR_NODE *sjnode = scalar_writes->Bottom_nth(sj);
      WN *scalar_write = sjnode->Bottom_nth(0)->Wn;

      if (Aliased(Alias_Mgr,scalar_read,scalar_write)!=NOT_ALIASED) {

        BOOL scalar_expandable=FALSE;

        SYMBOL sym(scalar_write);
        SYMBOL sym1(scalar_read);
        if (sym==sym1) {
          if (mapping_dictionary) {
            // consult Expandable_Scalar_Set to see if this scalar is expandable
            NAME2BIT temp_map(scalar_write);
            const NAME2BIT *name_map=
              mapping_dictionary->Find(temp_map)->Get_Data();
            if (Expandable_Scalar_Set->Test(name_map->Get_Bit_Position())) {
              // if it is not scalar expandable, include all statements
              // in a dependence cycle so they will stay in the same SCC
              scalar_expandable=TRUE;
            }
          }
        }

        for (INT sii=0; sii<sinode->Elements(); sii++) {
          WN* wn=sinode->Bottom_nth(sii)->Wn;
          UINT ii_stmt_number=sinode->Bottom_nth(sii)->Statement_Number;
          WN* ii_stmt;

          REDUCTION_TYPE red_type;
          if (Red_Mgr)
            red_type=Red_Mgr->Which_Reduction(wn);
          else
            red_type=RED_NONE;

          for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
            WN* wn1=sjnode->Bottom_nth(sjj)->Wn;
            if (red_type!=RED_NONE && Red_Mgr->Which_Reduction(wn1)==red_type)
              continue;
            UINT jj_stmt_number=sjnode->Bottom_nth(sjj)->Statement_Number;

            WN* jj_stmt;
            stmt_level=Get_Stmt_For_Stmt_Dep_Graph(wn,wn1,&ii_stmt,&jj_stmt);

            EINDEX16 e1;
            EINDEX16 e2=1;
            if (ii_stmt_number < jj_stmt_number) {
              e1=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
              if (!scalar_expandable)
                e2=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
            } else {
              e1=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
              if (!scalar_expandable)
                e2=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
            }
            if (e1==0 || e2==0) {
              return FALSE;
            }
            if (e1!=0)
              sdg->Set_Level_Property(e1, HAS_ALL_ZERO);
          }
        }
      }
    }
  }

  for (si=0; si<params->Elements(); si++) {
  
    SCALAR_REF siref = params->Bottom_nth(si);
    WN *param = siref.Wn;
    UINT ii_stmt_number=siref.Statement_Number;

    WN* tmp=WN_kid0(param);
    OPERATOR opr=WN_operator(tmp);

    INT sj;
    for (sj=0; sj<scalar_writes->Elements(); sj++) {
      SCALAR_NODE *sjnode = scalar_writes->Bottom_nth(sj);
      WN *scalar_write = sjnode->Bottom_nth(0)->Wn;

      if (Aliased(Alias_Mgr,param,scalar_write)!=NOT_ALIASED) {

          for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
            WN* wn1=sjnode->Bottom_nth(sjj)->Wn;
            UINT jj_stmt_number=sjnode->Bottom_nth(sjj)->Statement_Number;

            WN* ii_stmt;
            WN* jj_stmt;
            stmt_level=
              Get_Stmt_For_Stmt_Dep_Graph(param,wn1,&ii_stmt,&jj_stmt);

            EINDEX16 e1;
            EINDEX16 e2;
            if (ii_stmt_number < jj_stmt_number) {
              e1=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
              e2=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
            } else {
              e1=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
              e2=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
            }
            if (e1==0 || e2==0) {
              return FALSE;
            }
            if (e1!=0)
              sdg->Set_Level_Property(e1, HAS_ALL_ZERO);
          }
      }
    }

    for (sj=0; sj<scalar_reads->Elements(); sj++) {
      SCALAR_NODE *sjnode = scalar_reads->Bottom_nth(sj);
      WN *scalar_read = sjnode->Bottom_nth(0)->Wn;

      if (Aliased(Alias_Mgr,param,scalar_read)!=NOT_ALIASED) {

          for (INT sjj=0; sjj<sjnode->Elements(); sjj++) {
            WN* wn1=sjnode->Bottom_nth(sjj)->Wn;
            UINT jj_stmt_number=sjnode->Bottom_nth(sjj)->Statement_Number;

            WN* ii_stmt;
            WN* jj_stmt;
            stmt_level=
              Get_Stmt_For_Stmt_Dep_Graph(param,wn1,&ii_stmt,&jj_stmt);

            EINDEX16 e1;
            EINDEX16 e2;
            if (ii_stmt_number < jj_stmt_number) {
              e1=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
              e2=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
            } else {
              e1=Add_Stmt_Dependence(jj_stmt, ii_stmt, stmt_level, sdg);
              e2=Add_Stmt_Dependence(ii_stmt, jj_stmt, stmt_level, sdg);
            }
            if (e1==0 || e2==0) {
              return FALSE;
            }
            if (e1!=0)
              sdg->Set_Level_Property(e1, HAS_ALL_ZERO);
          }
      }
    }
  }
  return Generate_Pragma_Dependence_For_Statement_Dependence_Graph(
    in_loop,in_loop,sdg);
}

extern BOOL Generate_Array_Dependence_For_Statement_Dependence_Graph(
  WN* in_loop,
  REF_LIST_STACK *reads,
  REF_LIST_STACK *writes,
  ARRAY_DIRECTED_GRAPH16* sdg,
  REDUCTION_MANAGER* Red_Mgr,
  ARRAY_DIRECTED_GRAPH16* adg) {

  UINT loop_depth=Do_Loop_Depth(in_loop);

  WN* loop_body=WN_do_body(in_loop);
  REF_LIST_STACK *ref_list_stack[2];
  ref_list_stack[0]=reads;
  ref_list_stack[1]=writes;

  // examine all read and write refs
  for (INT ii=0; ii<2; ii++) {

    for (INT i=0;i<ref_list_stack[ii]->Elements(); i++) {
  
      REFERENCE_ITER iter(ref_list_stack[ii]->Bottom_nth(i));
      for (REFERENCE_NODE *n=iter.First(); !iter.Is_Empty(); n=iter.Next()) {

        WN* ref=n->Wn;
        VINDEX16 array_v=adg->Get_Vertex(ref);
        if (array_v==0) {
          // Array reference not in array dep. graph
          // make the corresponding stmt dependent on everything
          WN* stmt=Find_Stmt_Under(ref,loop_body);
          VINDEX16 v=sdg->Get_Vertex(stmt);
          if (v!=0)
            sdg->Delete_Vertex(v);
        } else {
  
          WN* stmt=Find_Stmt_Under(ref,loop_body);

          // both in and out edges have to be handled because some
          // scalars may be equivalenced (aliased) to the array elements
          // so we need to look at both kind of edges for every
          // array references
  
          EINDEX16 in_edge=adg->Get_In_Edge(array_v);
          while (in_edge) {

            WN* source_ref=adg->Get_Wn(adg->Get_Source(in_edge));
            WN* source_stmt=Find_Stmt_Under(source_ref,loop_body);
            if (source_stmt) {
              if (!Edge_Is_Reduction_Dependence(in_edge,adg,Red_Mgr)) {
                EINDEX16 e1=Array_Edge_To_Level_Edge(in_edge, adg, sdg);
                if (e1==0)
                  return FALSE;
                else if (sdg->Level(e1)<loop_depth)
                  sdg->Delete_Edge(e1);
              }
            }

            in_edge = adg->Get_Next_In_Edge(in_edge);
          }
          EINDEX16 out_edge=adg->Get_Out_Edge(array_v);
          while (out_edge) {

            WN* sink_ref=adg->Get_Wn(adg->Get_Sink(out_edge));
            WN* sink_stmt=Find_Stmt_Under(sink_ref,loop_body);
            if (sink_stmt) {
              if (!Edge_Is_Reduction_Dependence(out_edge,adg,Red_Mgr)) {
                EINDEX16 e1=Array_Edge_To_Level_Edge(out_edge, adg, sdg);
                if (e1==0)
                  return FALSE;
                else if (sdg->Level(e1)<loop_depth)
                  sdg->Delete_Edge(e1);
              }
            }

            out_edge = adg->Get_Next_Out_Edge(out_edge);
          }
        }
      }
    }
  }
  return TRUE;
}

#ifdef KEY
static BOOL Loop_Has_Asm (WN* loop)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(WN_do_body(loop));
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* node = itr->wn;
    if (WN_operator(node) == OPR_ASM_STMT)
      return TRUE;
  }
  return FALSE;
}
#endif

extern ARRAY_DIRECTED_GRAPH16* Build_Statement_Dependence_Graph(
  WN* in_loop,
  REDUCTION_MANAGER* Red_Mgr,
  ARRAY_DIRECTED_GRAPH16* adg,
  WN_MAP sdm,
  MEM_POOL* pool)
{
#ifdef KEY //bug 14121: statement dependence graph will be incorrect
           //if the loop contains Asm statement.
  if(Loop_Has_Asm(in_loop))
   return NULL;
#endif

  MEM_POOL_Push(&LNO_local_pool);
  ARRAY_DIRECTED_GRAPH16 *sdg;
  // add extra scope so that stack vars are deallocated before pop
  {

    REF_LIST_STACK *writes = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                     &LNO_local_pool);
    REF_LIST_STACK *reads = CXX_NEW(REF_LIST_STACK(&LNO_local_pool),
                                    &LNO_local_pool);

    SCALAR_STACK *scalar_writes = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                          &LNO_local_pool);
    SCALAR_STACK *scalar_reads = CXX_NEW(SCALAR_STACK(&LNO_local_pool),
                                         &LNO_local_pool);
    SCALAR_REF_STACK *params = CXX_NEW(SCALAR_REF_STACK(&LNO_local_pool),
                                         &LNO_local_pool);

    DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                &LNO_local_pool);
    Build_Doloop_Stack(in_loop, stack);

    // Make a list of all the references
    WN* loop_body=WN_do_body(in_loop);
    Init_Ref_Stmt_Counter();
    INT32 status=0;
    for (WN* wn=WN_first(loop_body); wn && status!= -1; wn=WN_next(wn)) {
      status=New_Gather_References(
		wn,writes,reads,stack,scalar_writes,scalar_reads,
                params,&LNO_local_pool);
    }
    if (status == -1) {
      DevWarn("Aborted from New_Gather_References");
      MEM_POOL_Pop(&LNO_local_pool);
      return NULL; 
    }

    sdg = CXX_NEW(ARRAY_DIRECTED_GRAPH16(100,500,sdm,LEVEL_ARRAY_GRAPH), pool);

    for (WN* stmt = WN_first(loop_body); stmt; stmt = WN_next(stmt)) {
      if (!Map_Stmt_To_Level_Graph(stmt,sdg)) {
        DevWarn("Statement dependence graph problem");
        MEM_POOL_Pop(&LNO_local_pool);
        return NULL; 
      }
    }

    if (!Generate_Scalar_Dependence_For_Statement_Dependence_Graph(
      in_loop, scalar_reads, scalar_writes, params, sdg, Red_Mgr)) {
      CXX_DELETE(sdg,pool);
      sdg=NULL;
    } else if (!Generate_Array_Dependence_For_Statement_Dependence_Graph(
      in_loop, reads, writes, sdg, Red_Mgr, adg)) {
      CXX_DELETE(sdg,pool);
      sdg=NULL;
    }
  }

  MEM_POOL_Pop(&LNO_local_pool);
  return sdg;

}


