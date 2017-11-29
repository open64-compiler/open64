/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#define lnoutils_CXX      "lnoutils.cxx"

#include <alloca.h>
#include <sys/types.h>
#include <ctype.h>
#include <limits.h>

#include "pu_info.h"
#include "lnoutils.h"
#include "config.h"
#include "config_cache.h"
#include "config_lno.h"
#include "lnopt_main.h"
#include "stab.h"
#include "targ_const.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "strtab.h"
#include "targ_sim.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "wintrinsic.h"
#include "lno_bv.h"
#include "region_util.h"
#include "fb_whirl.h"
#include "lego_gen.h"
#include "snl_utils.h"
#include "dep_graph.h"
#include "wn_pragmas.h"
#include "ff_utils.h"
#include "move.h"
#include "w2op.h" 
#include "ipa_lno_util.h"
#include "debug.h" 
#include "wutil.h"
#ifdef KEY
#include "be_symtab.h" // for Be_preg_tab
#endif
#include "intrn_info.h"

#pragma weak New_Construct_Id

extern void LWN_Parentize_One_Level(const WN* wn);

extern WN* LWN_Make_Icon(TYPE_ID wtype, INT64 i)
{
  OPCODE intconst_opc = OPCODE_make_op(OPR_INTCONST, wtype, MTYPE_V);
  return WN_CreateIntconst(intconst_opc, i);
}

extern TYPE_ID Do_Wtype(WN* wn)
{
  Is_True(WN_opcode(wn) == OPC_DO_LOOP, ("Do_Wtype: requires do parameter"));
  Is_True(WN_start(wn) && WN_operator(WN_start(wn)) == OPR_STID,
    ("Do_Wtype: bad do start, op=%d", WN_opcode(WN_start(wn))));
  return WN_desc(WN_start(wn));
}

//-----------------------------------------------------------------------
// NAME: Step_Size
// FUNCTION: Step_Size sets the step to a new integer.  It also returns the
//   old value.  To not change it, pass a step of 0.  Returns 0 if
//   non-integral.  Pass either the loop or the step itself.
//-----------------------------------------------------------------------

extern INT64 Step_Size(WN* loop,
                       INT64 newstep)
{
  WN* step;

  if (WN_opcode(loop) == OPC_DO_LOOP) {
    step = WN_step(loop);
    WN* index = WN_index(loop);

    if (WN_st(step) != WN_st(index) || WN_offset(step) != WN_offset(index)) {
      DevWarn("Index %s/%d but assignment to %s/%d in step",
              ST_name(WN_st(step)), WN_offset(step),
              ST_name(WN_st(index)), WN_offset(index));
      FmtAssert(newstep == 0, ("Bug in Step_Size"));
      return 0;
    }
  }
  else {
    step = loop;
  }

  if (WN_operator(step) != OPR_STID) {
    DevWarn("Step expression operator not STID: %s",
            OPERATOR_name(WN_operator(step)));
    FmtAssert(newstep == 0, ("Bug in Step_Size"));
    return 0;
  }

  WN* kid = WN_kid0(step);

  OPERATOR opr = WN_operator(kid);
  if (opr != OPR_ADD && opr != OPR_SUB) {
    FmtAssert(newstep == 0,
              ("Require ADD or SUB for step, but saw `%s'",
               OPERATOR_name(opr)));
    return 0;
  }

  BOOL neg = (opr == OPR_SUB);

  WN* ldkid = WN_kid0(kid);
  WN* constkid = WN_kid1(kid);
  INT constkidno = 1;

  if (WN_operator(ldkid) != OPR_LDID) {
    if (!neg) {
      WN* tmp = ldkid;
      ldkid = constkid;
      constkid = tmp;
      constkidno = 0;
    }
    if (WN_operator(ldkid) != OPR_LDID) {
      FmtAssert(newstep == 0, ("Saw the add, but not of the right thing"));
      return 0;
    }
  }

  if (WN_operator(constkid) != OPR_INTCONST) {
    if (newstep != 0) {
      LWN_Delete_Tree(constkid);
      WN_kid(kid,constkidno) = LWN_Make_Icon(Do_Wtype(loop),
                                             neg?-newstep:newstep);
      LWN_Set_Parent(WN_kid(kid,constkidno), kid);
    }
    return 0;
  }
  else {
    INT64 rval = WN_const_val(constkid);
    if (newstep != 0)
      WN_const_val(constkid) = neg ? -newstep : newstep;
    return neg ? -rval : rval;
  }
}

//-----------------------------------------------------------------------
// NAME: Step_Size
// FUNCTION: Returns the step size of 'loop'.  Returns 0 if a non-integral
//   value of step size is found.
//-----------------------------------------------------------------------

extern INT64 Step_Size(WN* loop)
{
  return Step_Size((WN*)loop, 0);
}

//-----------------------------------------------------------------------
// NAME: Add_Barrier_Vertex
// FUNCTION: Adds a vertex to the barrier 'wn_barrier' if it is inside a 
//   loop. 
//-----------------------------------------------------------------------

static void Add_Barrier_Vertex(WN* wn_barrier)
{
  WN *loop = Enclosing_Do_Loop(wn_barrier);
  if (loop && Do_Loop_Is_Good(loop)) {
    VINDEX16 v = Array_Dependence_Graph->Add_Vertex(wn_barrier);
    if (!v)
      LNO_Erase_Dg_From_Here_In(wn_barrier, Array_Dependence_Graph);
  }
}

//-----------------------------------------------------------------------
// NAME: Create_Single_Region
// FUNCTION: Create a SINGLE PROCESS region around the tree of code 
//   rooted at 'wn_single'. 
// This creates a SINGLE_PROCESS region of the form: 
//   FORWARD_BARRIER 
//   REGION (SINGLE_PROCESS_BEGIN)
//     BACKWARD_BARRIER 
//     <code> 
//     FORWARD_BARRIER
//   BACKWARD_BARRIER
//-----------------------------------------------------------------------

extern void Create_Single_Region(WN* wn_parent, WN* wn_single,
				 WN* wn_end)
{
  // Insert barriers before and after the SINGLE PROCESS region
  WN* wn_block = WN_CreateBlock();
  WN* wn_prag_before = WN_CreateBarrier(TRUE, 0);
  LWN_Insert_Block_Before(wn_parent, wn_single, wn_prag_before);
  Add_Barrier_Vertex(wn_prag_before);
  WN* wnn = NULL; 

  WN *wn;
  for (wn = wn_single; wn != wn_end; wn = wnn) {
    wnn = WN_next(wn);
    LWN_Extract_From_Block(wn);
    LWN_Insert_Block_Before(wn_block, NULL, wn);
  } 
  WN* wn_prag_after = WN_CreateBarrier(FALSE, 0);
  LWN_Insert_Block_After(wn_parent, wn_prag_before, wn_prag_after);
  Add_Barrier_Vertex(wn_prag_after);

  // Insert the SINGLE_PROCESS directives
  RID* p_rid = Get_Enclosing_Region_ID(wn_prag_before);
  FmtAssert(p_rid, ("Can't find parent RID"));
  WN* wn_region = WN_CreateRegion(REGION_KIND_MP, wn_block,
                                  NULL, NULL, RID_CREATE_NEW_ID,
                                  (INITO_IDX) NULL);
  REGION_INFO* rgi = CXX_NEW(REGION_INFO(TRUE),
    &LNO_default_pool);
  WN_MAP_Set(LNO_Info_Map, wn_region, (void *) rgi);
  LWN_Parentize(wn_region);
  LWN_Insert_Block_After(wn_parent, wn_prag_before,
    wn_region);
  WN* wn_prag_sp = WN_CreatePragma(WN_PRAGMA_SINGLE_PROCESS_BEGIN,
    (ST_IDX) NULL, 0, 0);
  WN_set_pragma_compiler_generated(wn_prag_sp);
  LWN_Insert_Block_After(WN_region_pragmas(wn_region), NULL,
    wn_prag_sp);
  RID *rid = RID_Create(WN_region_id(wn_region),
    RID_depth(p_rid)+1, wn_region);
  RID_level(rid) = RL_LNO;
  RID_TYPE_mp_Set(rid);
  WN_MAP_Set(RID_map, wn_region, (void *)rid);
  RID_Add_kid(rid, p_rid);

  // Determine if it is OMP, and set the OMP if needed.
  WN* wn_first = NULL;
  for (wn = wn_region; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_REGION) {
      wn_first = WN_first(WN_region_pragmas(wn));
      if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA
          && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN)
        break;
    }
  }
  BOOL set_omp = TRUE;
  if (wn != NULL && !WN_pragma_omp(wn_first))
    set_omp = FALSE;
  if (set_omp)
    WN_set_pragma_omp(wn_prag_sp);

  // Insert barriers before and after the SINGLE PROCESS region
  wn_prag_before = WN_CreateBarrier(FALSE, 0);
  LWN_Insert_Block_After(wn_parent, NULL, wn_prag_before);
  wn_prag_after = WN_CreateBarrier(TRUE, 0);
  LWN_Insert_Block_Before(wn_parent, NULL, wn_prag_after);
  Add_Barrier_Vertex(wn_prag_before);
  Add_Barrier_Vertex(wn_prag_after);
}

//-----------------------------------------------------------------------
// dot product
//-----------------------------------------------------------------------

INT32 Dot_Product(const mINT32* v1, const mINT32* v2, INT cnt)
{
  INT32 s = 0;
  for (INT i = 0; i < cnt; i++)
    s += *v1++ * *v2++;
  return s;
}

INT64 Dot_Product(const mINT64* v1, const mINT64* v2, INT cnt)
{
  INT64 s = 0;
  for (INT i = 0; i < cnt; i++)
    s += *v1++ * *v2++;
  return s;
}

INT64 Dot_Product(const mINT32* v1, const mINT64* v2, INT cnt)
{
  INT64 s = 0;
  for (INT i = 0; i < cnt; i++)
    s += *v1++ * *v2++;
  return s;
}

INT64 Dot_Product(const mINT64* v1, const mINT32* v2, INT cnt)
{
  INT64 s = 0;
  for (INT i = 0; i < cnt; i++)
    s += *v1++ * *v2++;
  return s;
}

//-----------------------------------------------------------------------
// dumping WNs to a file
//-----------------------------------------------------------------------

static void printws(FILE* f, INT ws)
{
  char buf[80];

  INT i;
  for (i = 0; i < MIN(ws,79); i++)
    buf[i] = ' ';
  buf[i] = 0;
  fprintf(f, "%s", buf);
}

extern void wn_dumpexpr(WN* wn, INT fancy, FILE* f,
			ARRAY_DIRECTED_GRAPH16* dg,
                        WN** list, WN* parent, BOOL recursive)
{
  BOOL printed = FALSE;

  if (wn == NULL) {
    Is_True(0, ("wn_dumpexpr dumping a null expression"));
    fprintf(f, "<null>");
    return;
  }

  OPCODE   opc = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opc);

  fprintf(f, "(");

  if (list) {
    for (WN** listmem = list; *listmem; listmem++)
      if (*listmem == wn)
	fprintf(f, "**%p**", wn);
  }

  if (fancy > 3 && parent != LWN_Get_Parent(wn)) {
    fprintf(f, "%%%%%%%% parent=%p, real parent=%p %%%%%%%%",
	    LWN_Get_Parent(wn), parent);
  }

  if (fancy) {
    printed = TRUE;
    switch(opr) {
     case OPR_CONST:
      switch (OPCODE_rtype(opc)) {
       case MTYPE_F4:
	fprintf(f, "%g", STC_val(WN_st(wn)).vals.fval);
	break;
       case MTYPE_F8:
	fprintf(f, "%g", STC_val(WN_st(wn)).vals.dval);
	break;
       default:
	printed = FALSE;
	break;
      }
      break;
     case OPR_INTCONST:
      fprintf(f, "%lld", WN_const_val(wn));
      break;
     case OPR_LDID:
      fprintf(f, "%s", SYMBOL(wn).Name());
      if (fancy >= 2) {
        DEF_LIST* deflist = Du_Mgr->Ud_Get_Def(wn);
	if (deflist == NULL)
	  fprintf(f, " <<missing DU defs>>");
        else if (deflist->Incomplete())
	  fprintf(f, " <<incomplete DU defs>>");
        else if (fancy >= 3) {
          WN* stmt = deflist->Loop_stmt();
          if (stmt == NULL)
            fprintf(f, "<<loop_stmt=NULL>>");
          else
            if (WN_opcode(stmt)==OPC_DO_LOOP) {
              fprintf(f, "<<loop_stmt=%s(%p)>>",
                    SYMBOL(WN_index(stmt)).Name(), stmt);
            } else {
              fprintf(f, "<<loop_stmt=%%%%(%p)>>", stmt);
            }
        }
      }
      break;
     case OPR_ADD:
      fprintf(f, "+");
      break;
     case OPR_SUB:
      fprintf(f, "-");
      break;
     case OPR_MPY:
      fprintf(f, "*");
      break;
     case OPR_DIV:
      fprintf(f, "/");
      break;
     default:
      printed = FALSE;
      break;
    }
  }

  VINDEX16 v = dg ? dg->Get_Vertex(wn) : 0;
  if (v)
    fprintf(f, "[v%d]", v);

  if (!printed) {
    FmtAssert(strncmp(OPCODE_name(opc), "OPC_", 4) == 0,
	      ("opname=%s", OPCODE_name(opc)));

    fprintf(f, "%s", OPCODE_name(opc) + 4);
    if (OPCODE_has_sym(opc))
      fprintf(f, " %s", SYMBOL(wn).Name());
    if (fancy >= 2 && OPERATOR(opc) == OPR_STID) {
      if (Du_Mgr->Du_Get_Use(wn) == NULL && 
	  !((ST_class(WN_st(wn))==CLASS_PREG) &&
          Preg_Is_Dedicated(WN_offset(wn))))
	fprintf(f, " <<missing DU uses>>");
      else if (Du_Mgr->Du_Get_Use(wn)->Incomplete() )
	fprintf(f, " <<incomplete DU uses>>");
    }
    if (OPCODE_has_label(opc))
      fprintf(f, " LAB%d", WN_offset(wn));
    if (opr == OPR_INTRINSIC_OP || opr == OPR_INTRINSIC_CALL) {
      INTRINSIC        i = (INTRINSIC) WN_intrinsic(wn);
      if (i >= INTRINSIC_FIRST && i <= INTRINSIC_LAST)
        fprintf(f, "<%s>", INTRINSIC_name(i));
      else
        fprintf(f, "<bad intr #=%d>", i);
    }
    else if (opr == OPR_IO)
      fprintf(f, "<io=%d>", WN_io_statement(wn));
    else if (opr == OPR_IO_ITEM)
      fprintf(f, "<io item=%d>", WN_io_item(wn));
  }

  if (fancy >= 3)
    fprintf(f, " [%p]", wn);

  if (recursive) { 
    for (INT k = 0; k < WN_kid_count(wn); k++) {
      fprintf(f, " ");
      wn_dumpexpr(WN_kid(wn, k), fancy, f, dg, list, wn, recursive);
    }
  } 

  fprintf(f, ")");

  fflush(f);
}

extern int last_loop_num;

// Enumerate loops for WHIRL tree rooted at wn.
// Currently, only DO loops are enumerated.
void Enum_loops(WN * wn)
{
  switch (WN_opcode(wn)) {
  case OPC_BLOCK:
    {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
	Enum_loops(w);
    }
    break;
  case OPC_DO_LOOP:
    {
      DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn, TRUE);
      if (dli && (dli->Get_Id() == 0))
	dli->Set_Id(++last_loop_num);
    }

    Enum_loops(WN_do_body(wn));
    break;
    
  case OPC_IF:
    Enum_loops(WN_then(wn));
    Enum_loops(WN_else(wn));
    break;
    
  case OPC_WHILE_DO:
    Enum_loops(WN_while_body(wn));
    break;
    
  case OPC_DO_WHILE:
    Enum_loops(WN_while_body(wn));
    break;

  case OPC_COMPGOTO:
    Enum_loops(WN_kid(wn,0));
    break;
    
  case OPC_FUNC_ENTRY:
    Enum_loops(WN_kid(wn, WN_kid_count(wn)-1));
    break;
    
  case OPC_REGION:
    Enum_loops(WN_region_body(wn));
    break;
    
  default:
    ;
  }
}

void Dump_WN(WN* wn, FILE* f, INT fancy, INT ws, INT ws_inc,
             ARRAY_DIRECTED_GRAPH16* dg, WN** list, WN* parent,
	     BOOL recursive)
{
  if (list) {
    for (WN** listmem = list; *listmem; listmem++)
      if (*listmem == wn) {
	fprintf(f, "**%p**", wn);
	fflush(f);
      }
  }

  if (parent && parent != LWN_Get_Parent(wn)) {
    fprintf(f, "%%%%%% parent=%p, real parent=%p %%%%%%",
	    LWN_Get_Parent(wn), parent);
    fflush(f);
  }

  INT32  line = 0;
  if (OPCODE_has_next_prev(WN_opcode(wn))) {
    line = WN_Get_Linenum(wn);
    if (line == 0)
      DevWarn("Missing line number for wn=%p (opr=%s)",
              wn, OPERATOR_name(WN_operator(wn)));
  }

  switch (WN_opcode(wn)) {
   case OPC_BLOCK:
    {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
        Dump_WN(w, f, fancy, ws, ws_inc, dg, list, wn, recursive);
    }
    break;

   case OPC_DO_LOOP:
    printws(f, ws);
    fprintf(f, "FOR");
    if (fancy >= 2)
      fprintf(f, " [%p]", wn);
    fprintf(f, " indx=");
    fflush(f);
    wn_dumpexpr(WN_index(wn), fancy, f, dg, list, wn, recursive);
    fprintf(f, " (Line=%d)", line);
    {
      DO_LOOP_INFO * dli = Get_Do_Loop_Info(wn, TRUE);
      if (dli)
	fprintf(f, " (loop num=%d)\n", dli->Get_Id());
      fprintf(f, "\n");
    }
    if (fancy >= 3) {
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn, TRUE);
      if (dli)
        dli->Print(f, ws+4);
      printws(f, ws+4);
      fprintf(f, "lb=");
      fflush(f);
      wn_dumpexpr(WN_start(wn), fancy, f, dg, list, wn, recursive);
      fprintf(f, "\n");
      printws(f, ws+4);
      fprintf(f, "ub=");
      fflush(f);
      wn_dumpexpr(WN_end(wn), fancy, f, dg, list, wn, recursive);
      fprintf(f, "\n");
      printws(f, ws+4);
      fprintf(f, "step=");
      fflush(f);
      wn_dumpexpr(WN_step(wn), fancy, f, dg, list, wn, recursive);
      fprintf(f, "\n");
      fflush(f);
    }
    if (fancy >= 3)
      Dump_WN(WN_do_body(wn), f, fancy, ws+ws_inc, ws_inc, dg, list, wn,
        recursive);
    printws(f, ws);
    fprintf(f, "END FOR\n");
    break;

   case OPC_IF:
    printws(f, ws);
    fprintf(f, "IF ");
    if (fancy >= 2)
      fprintf(f, "[%p] ", wn);
    fflush(f);
    wn_dumpexpr(WN_if_test(wn), fancy, f, dg, list, wn, recursive);
    fprintf(f, "THEN (Line=%d)\n", line);
    Dump_WN(WN_then(wn), f, fancy, ws+ws_inc, ws_inc, dg, list, wn, recursive);
    printws(f, ws);
    fprintf(f, "ELSE\n");
    fflush(f);
    Dump_WN(WN_else(wn), f, fancy, ws+ws_inc, ws_inc, dg, list, wn, recursive);
    printws(f, ws);
    fprintf(f, "END IF\n");
    break;

   case OPC_WHILE_DO:
    printws(f, ws);
    fprintf(f, "WHILE ");
    if (fancy >= 2)
      fprintf(f, "[%p] ", wn);
    fflush(f);
    wn_dumpexpr(WN_while_test(wn), fancy, f, dg, list, wn, recursive);
    fprintf(f, "DO (Line=%d)\n", line);
    Dump_WN(WN_while_body(wn), f, fancy, ws+ws_inc, ws_inc, dg, list, wn,
      recursive);
    printws(f, ws);
    fprintf(f, "END WHILEDO\n");
    fflush(f);
    break;

   case OPC_DO_WHILE:
    printws(f, ws);
    fprintf(f, "DO");
    if (fancy >= 2)
      fprintf(f, " [%p]", wn);
    fprintf(f, " (Line=%d)\n", line);
    fflush(f);
    Dump_WN(WN_while_body(wn), f, fancy, ws+ws_inc, ws_inc, dg, list, wn, 
      recursive);
    fprintf(f, "WHILE\n");
    wn_dumpexpr(WN_while_test(wn), fancy, f, dg, list, wn, recursive);
    fprintf(f, "\n");
    printws(f, ws);
    fprintf(f, "END DOWHILE\n");
    fflush(f);
    break;

   case OPC_COMPGOTO:
    printws(f, ws);
    fprintf(f, "COMPGOTO");
    if (fancy >= 2)
      fprintf(f, " [%p]", wn);
    fprintf(f, " switch=");
    wn_dumpexpr(WN_kid(wn,0), fancy, f, dg, list, wn, recursive);
    fprintf(f, " (Line=%d)\n", line);
    Dump_WN(WN_kid(wn,1), f, fancy, ws+ws_inc, ws_inc, dg, list, wn, recursive);
    if (WN_kid_count(wn) == 3) {
      printws(f, ws);
      fprintf(f, "  default_label=");
      wn_dumpexpr(WN_kid(wn,2), fancy, f, dg, list, wn, recursive);
      fprintf(f, "\n");
    }
    break;

   case OPC_FUNC_ENTRY:
    {
      printws(f, ws);
      fprintf(f, "FUNCTION ");
      if (fancy >= 2)
        fprintf(f, "[%p] ", wn);
      fflush(f);
      for (INT i = 0; i < WN_kid_count(wn) - 1; i++)
        wn_dumpexpr(WN_kid(wn,i), fancy, f, dg, list, wn, recursive);
      fprintf(f, "\n");
      printws(f, ws);
      Dump_WN(WN_kid(wn,WN_kid_count(wn)-1), f, fancy, ws+ws_inc, ws_inc,
	      dg, list, wn, recursive);
      fprintf(f, "END FUNCTION\n");
      fflush(f);
    }
    break;

   case OPC_REGION:
    printws(f, ws);
    if (line) {
      fprintf(f, "(Line=%d) ", line);
    }
    wn_dumpexpr(wn, fancy, f, dg, list, parent, recursive);
    fprintf(f, "\n");
    Dump_WN(WN_region_body(wn), f, fancy, ws+ws_inc, ws_inc,
	    dg, list, wn, recursive);
    break;

   default:
    printws(f, ws);
    if (line) {
      fprintf(f, "(Line=%d) ", line);
    }
    wn_dumpexpr(wn, fancy, f, dg, list, parent, recursive);
    fprintf(f, "\n");
    break;
  }

  fflush(f);
}



// Given two indices to be added/subtracted/maxed/mined, what is the
// type that holds them?  It's not an easy question.
// If we have 32 bit signed and 32 bit unsigned, there is
// no correct answer other than 64 bit signed.  Since we are trying not
// to expand, given that the odds of there being a problem are infinitesimal,
// we risk a bug by choosing a 32 bit version, but it's the only real choice.
// Which 32 bits to choose??  Could be wrong either way.  We prefer the signed,
// because that's probably the right choice.
// Making every new index a signed 64 bit might be closer to correct.

// Thankfully, in FORTRAN it's rarely a problem, since everything's the same
// type.

TYPE_ID Max_Wtype(TYPE_ID a, TYPE_ID b)
{
  // if only one arg, use it
  if (a == MTYPE_V)
    return b;
  if (b == MTYPE_V)
    return a;

  // expand into at least 32 bits without loss of precision
  if (a == MTYPE_U1 || a == MTYPE_U2)
    a = MTYPE_U4;
  else if (a == MTYPE_I1 || a == MTYPE_I2)
    a = MTYPE_I4;

  if (b == MTYPE_U1 || b == MTYPE_U2)
    b = MTYPE_U4;
  else if (b == MTYPE_I1 || b == MTYPE_I2)
    b = MTYPE_I4;

  // if the same, use that type again (risk of overflow).  Otherwise,
  // unsigned 32 is the least favored, signed 64 the most favored.
  if (a == b)
    return a;
  if (a == MTYPE_U4)
    return b;
  if (b == MTYPE_U4)
    return a;
  return MTYPE_I8;
}

// Do_Depth(): how deep is this statement nested within do loops.
// The outermost loop is loop 0.

INT Do_Depth(WN* wn, WN** loops, INT mx)
{
  if (wn == NULL)
    return -1;

  INT depth = Do_Depth(LWN_Get_Parent(wn), loops, mx);
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    if (loops) {
      FmtAssert(depth < mx, ("Do_Depth: too deep"));
      loops[depth] = wn;
    }
    depth++;
  }

  return depth;
}

// Good_Do_Depth(): how deep is this statement nested within good do loops.
// The outermost loop is loop 0.  If good loops occur outside of bad loops,
// they don't count.

INT Good_Do_Depth(WN* wn, WN** loops, INT mx)
{
  if (wn == NULL)
    return -1;

  INT depth = Good_Do_Depth(LWN_Get_Parent(wn), loops, mx);
  if (WN_opcode(wn) == OPC_DO_LOOP) {
    if (!Do_Loop_Is_Good(wn))
      depth = -1;
    else {
      if (loops) {
	FmtAssert(depth < mx, ("Do_Depth: too deep"));
	loops[depth] = wn;
      }
      depth++;
    }
  }

  return depth;
}

static void Build_Doloop_Stack_Rec(WN* wn, WN *parent,DOLOOP_STACK* stack)
{
  if (parent) {
    Build_Doloop_Stack_Rec(parent,LWN_Get_Parent(parent),stack);
    if (WN_opcode(parent) == OPC_DO_LOOP &&
	WN_do_body(parent) == wn) {
      stack->Push(parent);
    }
  }
}

void Build_Doloop_Stack(WN* wn, DOLOOP_STACK* stack)
{
  if (wn) {
    WN *parent = LWN_Get_Parent(wn);
    Build_Doloop_Stack_Rec(wn,parent,stack);
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      stack->Push(wn);
    }
  }
}

void Replace_Symbol(WN* wn, SYMBOL symold, SYMBOL symnew,
                    WN* alias_wn, WN* ancestor)
{
  OPCODE op = WN_opcode(wn);

  if (op == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      Replace_Symbol(w, symold, symnew, alias_wn, ancestor);
  }
  else {
    if (OPCODE_has_sym(op) && symold == SYMBOL(wn)) {
      OPERATOR opr = OPCODE_operator(op);
      WN_st_idx(wn) = ST_st_idx(symnew.St());
      WN_offset(wn) = symnew.WN_Offset();
      if (opr == OPR_LDID || opr == OPR_STID) {
        if (alias_wn == NULL) {
          Is_True(ST_class(symnew.St()) == CLASS_PREG,
                  ("symnew must be a preg if NULL alias_wn"));
          Create_alias(Alias_Mgr, wn);
        }
        else {
          if (symnew.St() != SYMBOL(alias_wn).St()) {
            // this is not necessarily an error.
            const INT bufsz = 64;
            char buf[bufsz];
            DevWarn("Replace Symbol: syspect symbols %s and %s",
                    symnew.Name(), SYMBOL(alias_wn).Name(buf, bufsz));
          }
          FmtAssert(alias_wn != wn || symold == symnew, ("Ridiculous"));
          Copy_alias_info(Alias_Mgr, alias_wn, wn);
        }
        if (ancestor) {
          if (opr == OPR_LDID) {
            DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
            DEF_LIST_ITER iter(def_list);
            const DU_NODE* nnext = NULL;
            const DU_NODE* node = NULL;
            for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
              nnext = iter.Next();
              WN *def = node->Wn();
              WN *p;
              for (p = def; p; p = LWN_Get_Parent(p)) {
                if (p == ancestor)
                  break;
              }
              if (p == NULL) {
                Du_Mgr->Delete_Def_Use(def,wn);
              }
            }
          }
          else {
            USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn);
            USE_LIST_ITER iter(use_list);
            const DU_NODE* nnext = NULL;
            const DU_NODE* node = NULL;
            for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
              nnext = iter.Next();
              WN *use = node->Wn();
              WN *p;
              for (p = use; p; p = LWN_Get_Parent(p)) {
                if (p == ancestor)
                  break;
              }
              if (p == NULL) {
                Du_Mgr->Delete_Def_Use(wn, use);
              }
            }
          }
        }
      }
    }
    for (INT k = 0; k < WN_kid_count(wn); k++)
      Replace_Symbol(WN_kid(wn,k), symold, symnew, alias_wn, ancestor);
  }
}

void Replace_Symbols(WN* wn, SYMBOL* sold, SYMBOL* snew, INT count,
                     WN** alias_wn, WN** ancestors)
{
  OPCODE op = WN_opcode(wn);

  if (op == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      Replace_Symbols(w, sold, snew, count, alias_wn, ancestors);
  }
  else {
    if (OPCODE_has_sym(op)) {
      for (INT i = 0; i < count; i++) {
        if (sold[i] == SYMBOL(wn)) {
          OPERATOR opr = OPCODE_operator(op);
          WN_st_idx(wn) = ST_st_idx(snew[i].St());
          WN_offset(wn) = snew[i].WN_Offset();
          if (opr == OPR_LDID || opr == OPR_STID) {
            if (alias_wn == NULL || alias_wn[i] == NULL) {
              Is_True(ST_class(snew[i].St()) == CLASS_PREG,
                      ("snew must be a preg if no alias info passed in"));
              Create_alias(Alias_Mgr, wn);
            }
            else {
              if (snew[i].St() != SYMBOL(alias_wn[i]).St()) {
                // this is not necessarily an error.
                const INT bufsz = 64;
                char buf[bufsz];
                DevWarn("Replace Symbol: syspect symbols %s and %s",
                        snew[i].Name(), SYMBOL(alias_wn[i]).Name(buf, bufsz));
              }
              FmtAssert(alias_wn[i] != wn || snew[i] == sold[i], ("Ridiculous"));
              Copy_alias_info(Alias_Mgr, alias_wn[i], wn);
            }
            if (ancestors && ancestors[i]) {
              if (opr == OPR_LDID) {
                DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
                DEF_LIST_ITER iter(def_list);
                const DU_NODE* nnext = NULL;
                const DU_NODE* node = NULL;
                for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
                  nnext = iter.Next();
                  WN *def = node->Wn();
                  WN *p;
                  for (p = def; p; p = LWN_Get_Parent(p)) {
                    if (p == ancestors[i])
                      break;
                  }
                  if (p == NULL) {
                    Du_Mgr->Delete_Def_Use(def,wn);
                  }
                }
              }
              else {
                USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn);
                USE_LIST_ITER iter(use_list);
                const DU_NODE* nnext = NULL;
                const DU_NODE* node = NULL;
                for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
                  nnext = iter.Next();
                  WN *use = node->Wn();
                  WN *p;
                  for (p = use; p; p = LWN_Get_Parent(p)) {
                    if (p == ancestors[i])
                      break;
                  }
                  if (p == NULL) {
                    Du_Mgr->Delete_Def_Use(wn, use);
                  }
                }
              }
            }
          }
          break;
        }
      }
    }
    for (INT k = 0; k < WN_kid_count(wn); k++)
      Replace_Symbols(WN_kid(wn,k), sold, snew, count, alias_wn, ancestors);
  }
}

// Add_To_Symbol(): recursively replace st with st+i

BOOL Add_To_Symbol(WN* wn, INT64 i, SYMBOL sym, BOOL stok)
{
  INT k;
  BOOL rval = FALSE;

  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w)) {
      if (Add_To_Symbol(w, i, sym, stok)) {
	WN* w2 = WN_Simplify_Tree(w);
	Is_True(w == w2, ("WN_Simplify_Tree() on stmt not returning itself?"));
	LWN_Parentize(w);
	// rval is not true: blocks of blocks do not want multiple
	// recursive calls to Add_To_Symbol
      }
    }
  }
  else {
    // all children
    for (k = 0; k < WN_kid_count(wn); k++) {
      WN* kid = WN_kid(wn,k);
      if (Add_To_Symbol(kid, i, sym, stok)) {
	if (OPCODE_is_stmt(WN_opcode(wn))) {
	  kid = WN_Simplify_Tree(WN_kid(wn,k));
	  WN_kid(wn,k) = kid;
	  LWN_Set_Parent(kid, wn);
	  LWN_Parentize(kid);
	}
	else
	  rval = TRUE;
      }
    }

    if (opr == OPR_LDID) {
      if (SYMBOL(wn) == sym) {

        WN* parent = LWN_Get_Parent(wn);
        for (k = 0; k < WN_kid_count(parent); k++)
          if (WN_kid(parent,k) == wn)
            break;
        Is_True(k < WN_kid_count(parent), ("Missing kid!"));

        WN* wcon = LWN_Make_Icon(WN_rtype(wn), i);
        OPCODE add_opc = OPCODE_make_op(OPR_ADD, WN_rtype(wn), MTYPE_V);
        WN* addnode = LWN_CreateExp2(add_opc, wn, wcon);
        LWN_Set_Parent(addnode, parent);
        WN_kid(parent,k) = addnode;

        rval = TRUE;
      }
    }
    else if (opr == OPR_STID) {
      FmtAssert(stok || SYMBOL(wn) != sym,
                ("Writing to %s in Add_To_Symbol()", sym.Name()));
    }
  }

  return rval;
}

// TODO: I think this version should be made obsolete
BOOL Add_To_Symbol(WN* wn, INT64 i, ST* st,
		   WN_OFFSET offset, TYPE_ID wtype, BOOL stok)
{
  static INT printed_warning = FALSE;
  if (printed_warning == FALSE) {
    printed_warning = TRUE;
    printf("Using obsolete version of Add_To_Symbol() -- ok for now\n");
  }

  SYMBOL sym(st, offset, wtype);
  return Add_To_Symbol(wn, i, sym, stok);
}

// replace wn with a copy of expr, and adjust parent pointers
WN* Replace_Wnexp_With_Exp_Copy(WN* wn, WN* expr, DU_MANAGER* du, 
  BOOL* added_cvt, ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
#ifdef KEY
  // Bug 6132 - when replacing UnUmLDID with InImILOAD, need to complement
  // the signedness of the ILOAD to avoid sign-extension.
  BOOL make_unsigned = FALSE;
  if (WN_operator(wn) == OPR_LDID && WN_operator(expr) == OPR_ILOAD &&
      MTYPE_byte_size(WN_rtype(wn)) == MTYPE_byte_size(WN_rtype(expr)) &&
      MTYPE_byte_size(WN_desc(wn)) == MTYPE_byte_size(WN_desc(expr)) &&
      !MTYPE_is_signed(WN_rtype(wn)) && !MTYPE_is_signed(WN_desc(wn)) &&
      MTYPE_is_signed(WN_rtype(expr)) && MTYPE_is_signed(WN_desc(expr)))
    make_unsigned = TRUE;
#endif
  Is_True(OPCODE_is_expression(WN_opcode(wn)), ("wn must be expression"));
  INT added_convert = FALSE;  
  WN* parent = LWN_Get_Parent(wn);
  INT k = 0; 
  if (parent != NULL) { 
    for (k = 0; k < WN_kid_count(parent); k++) {
      if (WN_kid(parent,k) == wn)
	break;
    }
    Is_True(k < WN_kid_count(parent), ("broken parent pointer"));
  } 

  WN* copy = NULL;
  if (dep_graph != NULL)
    copy = LWN_Copy_Tree(expr, TRUE, LNO_Info_Map);
  else 
    copy = LWN_Copy_Tree(expr);
  LWN_Copy_Frequency_Tree(expr,wn);
  if (du != NULL)
    LWN_Copy_Def_Use(expr, copy, du);
  if (dep_graph != NULL) {
    if (!dep_graph->Add_Deps_To_Copy_Block(expr,copy,FALSE)) {
      LNO_Erase_Dg_From_Here_In(expr,dep_graph);
    }
  }
  TYPE_ID    mtype = WN_rtype(wn);
  TYPE_ID    ctype = WN_rtype(copy);
  if (mtype != ctype && MTYPE_is_integral(mtype)) {
    WN* new_copy = LWN_Int_Type_Conversion(copy, mtype);
    if (copy != new_copy && 
        (WN_operator(new_copy) == OPR_CVT || 
         WN_operator(new_copy) == OPR_CVTL))
      added_convert = TRUE; 
    copy = new_copy; 
  } 

  LWN_Delete_Tree(wn);
  if (parent != NULL) {
    WN_kid(parent, k) =  copy;
    LWN_Set_Parent(WN_kid(parent, k), parent);
  }
  if (added_cvt != NULL)
    *added_cvt = added_convert; 

#ifdef KEY
  // Bug 6132 - see above.
  if (make_unsigned) {
    WN_set_rtype(copy, MTYPE_complement(WN_rtype(copy)));
    WN_set_desc(copy, MTYPE_complement(WN_desc(copy)));
  }
#endif    
  return copy; 
}

WN* Replace_Scalar_Store_With_Array_Store(WN* scalar_store, 
					  WN* array_store, 
					  DU_MANAGER* du)
{
  WN* wn_store = LWN_Copy_Tree(array_store); 
  LWN_Copy_Frequency_Tree(array_store,scalar_store);
  if (du != NULL) 
    LWN_Copy_Def_Use(WN_kid1(array_store), WN_kid1(wn_store), du);    
  WN* wn_rhs_new = WN_kid0(wn_store); 
  WN* wn_rhs_old = WN_kid0(scalar_store); 
  WN_kid0(wn_store) = wn_rhs_old; 
  WN_kid0(scalar_store) = wn_rhs_new; 
  LWN_Set_Parent(wn_rhs_old, wn_store); 
  LWN_Set_Parent(wn_rhs_new, scalar_store); 
  LWN_Insert_Block_Before(LWN_Get_Parent(scalar_store), scalar_store, wn_store);
  LWN_Extract_From_Block(scalar_store); 
  LWN_Delete_Tree(scalar_store); 
  return wn_store; 
}

// replace all ldid recursively with a copy of expr, and adjust parent pointers

void Replace_Ldid_With_Exp_Copy(SYMBOL symbol, WN* wn, WN* expr,
			DU_MANAGER* du,ARRAY_DIRECTED_GRAPH16 *dep_graph)
{
  switch (WN_operator(wn)) {
   case OPR_BLOCK:
    {
      for (WN* wn2 = WN_first(wn); wn2; wn2 = WN_next(wn2))
        Replace_Ldid_With_Exp_Copy(symbol, wn2, expr, du,dep_graph);
    }
    break;
   case OPR_IF:
    Replace_Ldid_With_Exp_Copy(symbol, WN_if_test(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_then(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_else(wn), expr, du,dep_graph);
    break;
   case OPR_DO_WHILE:
   case OPR_WHILE_DO:
    Replace_Ldid_With_Exp_Copy(symbol, WN_while_test(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_while_body(wn), expr, du,dep_graph);
    break;
   case OPR_DO_LOOP:
    Replace_Ldid_With_Exp_Copy(symbol, WN_start(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_end(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_step(wn), expr, du,dep_graph);
    Replace_Ldid_With_Exp_Copy(symbol, WN_do_body(wn), expr, du,dep_graph);
    break;
   case OPR_LDID:
    if (symbol == SYMBOL(wn))
      (void) Replace_Wnexp_With_Exp_Copy(wn, expr, du,NULL,dep_graph);
    break;
   default:
    for (INT k = 0; k < WN_kid_count(wn); k++)
      Replace_Ldid_With_Exp_Copy(symbol, WN_kid(wn,k), expr, du,dep_graph);
    break;
  }
}


// TODO: I'm keeping this code around because when I inlined sz, it caused
// a compiler error.  It's bad, buggy code, but it should compile!

inline             /* compiled w/o inline, didn't compile with */
INT sz(TYPE_ID wtype)
{
  switch (wtype) {
   default:
    FmtAssert(0, ("bad call to LWN_Integer_Cast: %d", wtype));
   case MTYPE_I8:
   case MTYPE_U8:
    return 64;
   case MTYPE_I4:
   case MTYPE_U4:
    return 32;
  }
}

static INT bitcount(TYPE_ID wtype)
{
  switch (wtype) {
  case MTYPE_I8:
  case MTYPE_U8:
    return 64;
  case MTYPE_I4:
  case MTYPE_U4:
    return 32;
  case MTYPE_U2:
  case MTYPE_I2: 
    return 16;
  case MTYPE_U1:
  case MTYPE_I1:
    return 8;
  default:
    FmtAssert(0, ("bad call to bitcount: %d", wtype));
    return 0;
  }
}

static WN* LWN_Short_Integer_Cast(WN* tree, TYPE_ID* to, TYPE_ID from)
{
  WN* wn_new = NULL;
  WN* result = NULL;
  TYPE_ID restype = MTYPE_UNKNOWN;
  switch (from) {
  case MTYPE_I1:
    wn_new = WN_CreateCvtl(OPC_I4CVTL, 8, tree);
    restype = MTYPE_I4;
    break;
  case MTYPE_I2:
    wn_new = WN_CreateCvtl(OPC_I4CVTL, 16, tree);
    restype = MTYPE_I4;
    break;
  case MTYPE_U1:
    wn_new = WN_CreateCvtl(OPC_U4CVTL, 8, tree);
    restype = MTYPE_U4;
    break;
  case MTYPE_U2:
    wn_new = WN_CreateCvtl(OPC_U4CVTL, 16, tree);
    restype = MTYPE_U4;
    break;
  default: 
    wn_new = tree;
    restype = MTYPE_UNKNOWN;
  }
  *to = restype;
  LWN_Parentize_One_Level(wn_new);
  return wn_new;
} 

extern WN* LWN_Integer_Cast(WN* tree, TYPE_ID to, TYPE_ID from)
{
  INT szfrom = sz(from);
  INT szto = sz(to);

  if (szfrom != szto)
    return LWN_CreateExp1(OPCODE_make_op(OPR_CVT, to, from), tree);
  else
    return tree;
}

extern WN* LWN_Integer_Casts(WN* tree, TYPE_ID to, TYPE_ID from)
{
  TYPE_ID local_to = MTYPE_UNKNOWN;
  if (bitcount(to) < bitcount(from)) { 
    if (bitcount(to) < 32) { 
      switch (from) { 
      case MTYPE_I8: 
	tree = LWN_Integer_Cast(tree, MTYPE_I4, MTYPE_I8);
	switch (to) { 
	case MTYPE_I1: 
	case MTYPE_U1: 
	  tree = WN_CreateCvtl(OPC_I4CVTL, 8, tree);
	  break;
	case MTYPE_I2: 
	case MTYPE_U2: 
	  tree = WN_CreateCvtl(OPC_I4CVTL, 16, tree);
	  break;
	default: 
	  FmtAssert(FALSE, ("Bad TO type"));
	} 
        break; 
      case MTYPE_U8: 
	tree = LWN_Integer_Cast(tree, MTYPE_U4, MTYPE_U8);
	switch (to) { 
	case MTYPE_I1: 
	case MTYPE_U1: 
	  tree = WN_CreateCvtl(OPC_U4CVTL, 8, tree);
	  break;
	case MTYPE_I2: 
	case MTYPE_U2: 
	  tree = WN_CreateCvtl(OPC_U4CVTL, 16, tree);
	  break;
	default: 
	  FmtAssert(FALSE, ("Bad TO type"));
	} 
        break; 
      default: 
	FmtAssert(FALSE, ("LWN_Integer_Cast: Bad FROM type"));
      } 
      LWN_Parentize_One_Level(tree);
      return tree;
    } else { 
      return LWN_Integer_Cast(tree, to, from); 
    } 
  } 
  if (from != to) { 
    switch (from) { 
    case MTYPE_I1: 
    case MTYPE_I2: 
    case MTYPE_U1:
    case MTYPE_U2: 
      tree = LWN_Short_Integer_Cast(tree, &local_to, from);
    }   
  } 
  TYPE_ID new_to = local_to == MTYPE_UNKNOWN ? to : local_to;
  tree = LWN_Integer_Cast(tree, new_to, from); 
  return tree;
} 

SYMBOL Create_Preg_Symbol(const char* name, TYPE_ID type)
{
#ifdef _NEW_SYMTAB
  PREG_NUM reg = Create_Preg(type, (char*)name);
#else
  PREG_NUM reg = Create_Preg(type, (char*)name, NULL);
#endif
  return SYMBOL(MTYPE_To_PREG(type), reg, type);
}

SYMBOL Create_Stack_Symbol(const char *name, TYPE_ID type)
{
   ST* st = New_ST(CURRENT_SYMTAB);
   ST_Init (st,
            Save_Str((char*)name),
            CLASS_VAR,
            SCLASS_AUTO,
            EXPORT_LOCAL,
            Be_Type_Tbl(type));
   Set_ST_is_temp_var(st);
   return SYMBOL(st, 0, type);
}


ST* Lookup_Function_Name(const char* name)
{
  ST* st;
  INT i;

  FmtAssert(0,("Function untested with new symbol table"));

  FOREACH_SYMBOL (GLOBAL_SYMTAB,st,i) {
    if (ST_class(st) == CLASS_FUNC && strcmp(ST_name(st), name)==0)
      return st;
  }

  st = New_ST(GLOBAL_SYMTAB);
#ifdef _NEW_SYMTAB
  TY_IDX ty_idx;
  TY& ty = New_TY (ty_idx);
  TY_Init (ty,
           0,
           KIND_FUNCTION,
           MTYPE_UNKNOWN,
           Save_Str("__intrinsic_lno"));
  Set_TY_align(ty_idx, 0);
#else
  st = New_ST(TRUE);
  ST_name(st) = Save_Str((char*)name);
  ST_class(st) = CLASS_FUNC;
  TY *ty = New_TY(TRUE); // global type
  TY_name(ty) = Save_Str("__intrinsic_lno");
  TY_kind(ty) = KIND_FUNCTION;
  Set_TY_align(ty, 0);
  Set_TY_size(ty, 0);
#endif


#ifdef _NEW_SYMTAB

  TYLIST_IDX tylist_idx;
  TYLIST&    tylist_head = New_TYLIST (tylist_idx);
  TY_IDX ret_type_idx;
  if (strcmp(ST_name(st), "__builtin_malloc")==0)
  {
    ret_type_idx = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  } else if (strcmp(ST_name(st), "__builtin_free")==0) {
    ret_type_idx = Void_Type;
  } else {
    ret_type_idx = Be_Type_Tbl(MTYPE_I4); // returns an int
  }
  Tylist_Table [tylist_idx] = ret_type_idx;
  TYLIST_IDX first_tylist_idx = tylist_idx;
  TYLIST& tylist_tail = New_TYLIST (tylist_idx);
  Tylist_Table [tylist_idx] = 0;
  Set_TY_tylist (ty, first_tylist_idx);

  ST_Init (st,
           Save_Str(name),
           CLASS_FUNC,
           SCLASS_EXTERN,
           EXPORT_PREEMPTIBLE,
           ty_idx);

#else
  TY_ftinfo(ty) = New_FTI(0, TRUE); // global type with no specified parms
  Enter_TY(ty);
  if (strcmp(ST_name(st), "__builtin_malloc")==0)
  {
  
    // Create a function type, which returns an (void *).
    // TODO: Use intrinsics when they are fully supported.
    //
    TY_ret_type(ty) = Make_Pointer_Type(Be_Type_Tbl(MTYPE_U1));
  } else if (strcmp(ST_name(st), "__builtin_free")==0) {
    TY_ret_type(ty) = Void_Type;
  } else {
  
    // Create a function type, which returns an I4.
    // TODO: Use intrinsics when they are fully supported.
    //
    TY_ret_type(ty) = Be_Type_Tbl(MTYPE_I4); // returns an int
  }
  ST_type(st) = ty;
#endif

  Set_ST_sclass(st, SCLASS_TEXT);
  Enter_ST(st);
  return st;
}

static void reset_do(WN* body, INT depth)
{
  Is_True(body && WN_opcode(body) == OPC_BLOCK, ("Bad call to reset_do"));
  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_DO_LOOP:
      Reset_Do_Loop_Depths(wn, depth);
      break;
     case OPC_IF:
      reset_do(WN_then(wn), depth);
      reset_do(WN_else(wn), depth);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      reset_do(WN_while_body(wn), depth);
      reset_do(WN_while_test(wn), depth);
      break;
    }
  }
}

void Reset_Do_Loop_Depths(WN* loop, INT depth)
{
  Is_True(loop && WN_opcode(loop) == OPC_DO_LOOP,
	  ("Bad loop passed to Reset_Do_Loop_Depths()"));
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
  dli->Depth = depth;
  if (!dli->Is_Inner)
    reset_do(WN_do_body(loop), depth+1);
}

ACCESS_VECTOR* Difference_Inequality(ACCESS_VECTOR* lb,
				     ACCESS_VECTOR* ub,
				     INT var,
				     DIFFERENCE_KIND code,
				     MEM_POOL* pool)
{
  ACCESS_VECTOR* lb2 = lb;
  ACCESS_VECTOR* ub2 = ub;

  INT lval = lb->Loop_Coeff(var);
  INT uval = ub->Loop_Coeff(var);

  Is_True(lval < 0,
	  ("Setup_Difference_Inequality(): lval=%d depth=%d", lval, var));
  Is_True(uval > 0,
	  ("Setup_Difference_Inequality(): rval=%d depth=%d", uval, var));

  INT lcm = Lcm(lval, uval);
  Is_True(lcm > 0, ("Setup_Difference_Inequality(): lcm=%d", lcm));

  if (lcm != 1) {
    lb2 = Mul(lcm/-lval, CXX_NEW(ACCESS_VECTOR(lb, pool),pool), pool);
    ub2 = Mul(lcm/uval,  CXX_NEW(ACCESS_VECTOR(ub, pool),pool), pool);
  }

  // this gives us ub - lb >= 0.
  ACCESS_VECTOR* x = Add(lb2, ub2, pool);

  if (lb2 != lb)
    CXX_DELETE(lb2, pool);
  if (ub2 != ub)
    CXX_DELETE(ub2, pool);

  switch (code) {
   case DIFFERENCE_EXEC_NEVER_OR_ONCE:
    x->Negate_Me();
    x->Const_Offset += (lcm - 1);	// because when 2i, <= 1 is fine.
    break;
   case DIFFERENCE_EXEC_NEVER:
    x->Negate_Me();
    x->Const_Offset--;		// because want a difference of <0, i.e. <= -1
    break;
   case DIFFERENCE_EXEC_ALWAYS:
    break;
  }

  return x;
}

//-----------------------------------------------------------------------
// NAME: Statically_Safe_Node
// FUNCTION: Returns TRUE if the node is safe to speculate, FALSE   
//   otherwise. 
//-----------------------------------------------------------------------

extern BOOL Statically_Safe_Node(WN* wn)
{
  return WN_Can_Be_Speculative(wn, Alias_Mgr); 
}

//-----------------------------------------------------------------------
// NAME: Statically_Safe_Exp
// FUNCTION: Returns TRUE if the expression whose root in the node 'wn' 
//   is safe to speculate, FALSE otherwise. 
//-----------------------------------------------------------------------

extern BOOL Statically_Safe_Exp(WN *wn)
{
  return WN_Expr_Can_Be_Speculative(wn, Alias_Mgr); 
} 

// Print out the def-use chains for the whole procedure
void Print_Def_Use(WN *wn, FILE *fp)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Print_Def_Use(kid,fp);
      kid = WN_next(kid);
    }
  } else {
    if (opcode != OPC_IO) {
      for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
        WN *kid = WN_kid(wn,kidno);
        Print_Def_Use(kid,fp);
      }
    }
    OPERATOR opr=OPCODE_operator(opcode);
    switch (opr) {
      case OPR_LDID:
      case OPR_ILOAD:
      case OPR_ISTORE:
      case OPR_IO:
      case OPR_RETURN:
#ifdef KEY
      case OPR_GOTO_OUTER_BLOCK:
#endif
      case OPR_CALL:
      case OPR_ICALL:
      case OPR_INTRINSIC_CALL:
        fprintf(fp,"Visiting %s ", OPERATOR_name(opr));
        Dump_WN(wn, fp, 3, 0, 2, NULL, NULL, LWN_Get_Parent(wn));
        DEF_LIST *defs = Du_Mgr->Ud_Get_Def(wn);
        WN *loop_stmt = NULL;
        if (defs) {
          loop_stmt = defs->Loop_stmt();
        } else {
	  if (opr == OPR_LDID) DevWarn("NO DEF LIST ");
        }
        if (loop_stmt) {
          if (WN_opcode(loop_stmt)==OPC_DO_LOOP) {
	    fprintf(fp,"with loop_stmt for loop ");
            wn_dumpexpr(WN_index(loop_stmt), 3, fp, NULL, NULL, loop_stmt, 
              TRUE);
	    fprintf(fp,"\n");
          } else {
	    DevWarn("loop_stmt is not a DO_LOOP (%p,ls=%p)",
                    wn, loop_stmt);
	    Dump_WN(loop_stmt,fp,3,4,2,NULL,NULL,LWN_Get_Parent(loop_stmt));
          }
        }
	if (defs && defs->Incomplete()) {
	  fprintf(fp,"Its def list is incomplete \n");
        }
        fprintf(fp,"Its list of defs is \n");
        DEF_LIST_ITER iter(defs);
	if (iter.Is_Empty() && (opr == OPR_LDID)) {
	  DevWarn("Empty DEF LIST ");
        }
        for(const DU_NODE *node=iter.First();!iter.Is_Empty();node=iter.Next()){
          WN *def = (WN *) node->Wn();
	  if (WN_opcode(def) == OPC_FUNC_ENTRY) {
	    fprintf(fp,"ENTRY \n");
          } else {
	    Dump_WN(def, fp, 3, 4, 2, NULL, NULL, LWN_Get_Parent(def));
          }
        }
        fprintf(fp,"\n");
    }
    switch (opr) {
      case OPR_STID:
      case OPR_ISTORE:
      case OPR_IO:
      case OPR_FUNC_ENTRY:
      case OPR_CALL:
      case OPR_ICALL:
      case OPR_INTRINSIC_CALL:
        fprintf(fp,"Visiting %s ", OPERATOR_name(opr));
	if (opr != OPR_FUNC_ENTRY) {
          Dump_WN(wn, fp, 3, 0, 2, NULL, NULL, LWN_Get_Parent(wn));
        }
        fprintf(fp,"Its list of uses is \n");
        USE_LIST *uses = Du_Mgr->Du_Get_Use(wn);
        if (uses == NULL) {
	  if (opr == OPR_STID) DevWarn("NO USES LIST ");
        }
        USE_LIST_ITER iter(uses);
	if (iter.Is_Empty() && (opr == OPR_STID)) {
	  DevWarn("Empty USE LIST ");
        }
	if (uses && uses->Incomplete()) {
	  fprintf(fp,"Its use list is incomplete \n");
        }
        for(const DU_NODE *node=iter.First();!iter.Is_Empty();node=iter.Next()){
          WN *use = (WN *) node->Wn();
          Dump_WN(use, fp, 3, 4, 2, NULL, NULL, LWN_Get_Parent(use));
        }
        fprintf(fp,"\n");
    }
  }
}

// UPDATE DEF-USE, USE-DEF Chains after unrolling


// For each ldid/stid in body[0],
// Enter into the hash table a mapping from the original WNs to a new
// array of wns
// Enter each ldid or stid in body[0] into the appropriate stack.

static void Unrolled_DU_Update_V(WN **bodies, UINT u, 
   			HASH_TABLE<WN *,WN **> *hash_table,
			STACK<WN *> *load_stack, STACK<WN *> *store_stack)
{
  if (bodies[0] == NULL)
    return;

  OPCODE	opc = WN_opcode(bodies[0]);
  OPERATOR	opr = OPCODE_operator(opc);

  if (OPCODE_is_load(opc) || OPCODE_is_store(opc) || 
      OPCODE_is_call(opc) || (opr == OPR_IO) || (opr == OPR_DO_LOOP) 
      || (opr == OPR_PARM))  { 
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(bodies[0]);
    if (def_list) {
      DEF_LIST_ITER iter(def_list);
      if (!iter.Is_Empty()) {
        load_stack->Push(bodies[0]);
      } else {
        if (opr == OPR_LDID && !def_list->Incomplete()) {
          DevWarn("LDID %s without defs in Unrolled_DU_Update_V",
                  SYMBOL(bodies[0]).Name());
        }
      }
    } else {
      if (opr == OPR_LDID) {
        DevWarn("LDID %s without def_list in Unrolled_DU_Update_V",
                SYMBOL(bodies[0]).Name());
      }
    }
    USE_LIST *use_list = Du_Mgr->Du_Get_Use(bodies[0]);
    if (use_list) {
      USE_LIST_ITER iter(use_list);
      if (!iter.Is_Empty()) {
        store_stack->Push(bodies[0]);
      } else {
        if (opr == OPR_STID)
	  if (!use_list->Incomplete()) 
	    DevWarn("STID without uses in Unrolled_DU_Update_V");
#ifdef KEY // bug 8155
	  else store_stack->Push(bodies[0]);
#endif
      }
    } else {
      if(opr == OPR_STID) {
        DevWarn("STID without use_list in Unrolled_DU_Update_V");
      }
    }

    WN **newwn = CXX_NEW_ARRAY(WN*,u,&LNO_local_pool);
    for (INT i=0; i<u; i++) {
      newwn[i] = bodies[i];
    }
    hash_table->Enter(bodies[0],newwn);
  }

  // recurse

  if (opr == OPR_BLOCK) {
    WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
    for (INT i=0; i<u; i++) {
      new_bodies[i] = WN_first(bodies[i]);
    }
    while (new_bodies[0]) {
      Unrolled_DU_Update_V(new_bodies, u,hash_table,load_stack,store_stack);
      for (INT i=0; i<u; i++) {
        new_bodies[i] = WN_next(new_bodies[i]);
      }
    }
  } else if (WN_kid_count(bodies[0]) && (opr != OPR_IO)) {
    WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
    for (INT kidno=0; kidno<WN_kid_count(bodies[0]); kidno++) {
      for (INT i=0; i<u; i++) {
        new_bodies[i] = WN_kid(bodies[i],kidno);
      }
      Unrolled_DU_Update_V(new_bodies, u,hash_table,load_stack,store_stack);
    }
  }
}

// the map to keep track of the deleted loops (because of unroll, etc)
extern HASH_TABLE<WN*, BOOL> *Deleted_Loop_Map;

// get the edges right for updating after unrolling
//   Each edge in to or out of the region gets copied
//   For internal edges
//     If Loop_stmt is not more inner than loopno, make u^2 copies of each edge
//     (i.e. from body[1] to bodies[2..n], from body[2] to bodies[1..n],etc)
//     Otherwise, only make 'u' copies (i.e. from body[2] to body[2],
//     body[3] to body[3], etc)
//
// If index_loopno, don't put cross edges for ldids/stids to that symbol

static void Unrolled_DU_Update_E(UINT u, INT loopno,
			HASH_TABLE<WN *,WN **> *hash_table,
			STACK<WN *> *load_stack, STACK<WN *> *store_stack,
		BOOL update_pointers, SYMBOL *index_loopno)
{
  // visit the stores, we only care about external edges, internal edges
  // are handled when we visit the loads
#ifdef KEY // bug 8155
  // stores with incomplete uses also need to be taken into account; in such
  // cases they could be internal or external
#endif
  for (INT s=0; s<store_stack->Elements(); s++) {
    WN *stid = store_stack->Bottom_nth(s);
    USE_LIST *use_list = Du_Mgr->Du_Get_Use(stid);

    WN **stid_array = hash_table->Find(stid);

    if (use_list && use_list->Incomplete()) {
      for (INT i=1; i<u; i++) {
	Du_Mgr->Create_Use_List(stid_array[i]);
	USE_LIST *unrolled_use_list = Du_Mgr->Du_Get_Use(stid_array[i]);
	unrolled_use_list->Set_Incomplete();
      }
    }

    USE_LIST_ITER iter(use_list);
    for(const DU_NODE *node=iter.First();!iter.Is_Empty();node=iter.Next()){
      WN *use = (WN *) node->Wn();
      WN **ldid_array = hash_table->Find(use);

      if (!ldid_array) {  // an external edge
	 for (INT i=1; i<u; i++) {
	   Du_Mgr->Add_Def_Use(stid_array[i],use);
         }
      } 
    }
  }

  // now visit the loads
  for (INT l=0; l<load_stack->Elements(); l++) {
    WN *ldid = load_stack->Bottom_nth(l);
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ldid);

      // shall we make an n^2 copy the internal edges
    BOOL copy_in_edges = FALSE;
    WN *loop_stmt = def_list->Loop_stmt();
    
    // loop_stmt could be deleted, which is saved in a map
    // Deleted_Loop_Map by LWN_Delete_Tree
    if (loop_stmt && !Deleted_Loop_Map->Find(loop_stmt)) {
      INT loop_stmt_number = Do_Loop_Depth(loop_stmt);
      if (loop_stmt_number <= loopno) {
	copy_in_edges = TRUE;
      }
    }
    if (WN_operator(ldid) == OPR_LDID) {
      if (index_loopno && ((*index_loopno) == SYMBOL(ldid))) {
        copy_in_edges = FALSE;
      }
    }

    WN **ldid_array = hash_table->Find(ldid);

    if (def_list->Incomplete()) {
      for (INT i=1; i<u; i++) {
	Du_Mgr->Create_Def_List(ldid_array[i]);
	DEF_LIST *unrolled_def_list = Du_Mgr->Ud_Get_Def(ldid_array[i]);
	unrolled_def_list->Set_Incomplete();
      }
    }

    typedef STACK<WN *> WN_STACK;
    WN_STACK *ldid_zero_defs = 
	CXX_NEW(WN_STACK(&LNO_local_pool),&LNO_local_pool);

    DEF_LIST_ITER iter(def_list);
    const DU_NODE *node = iter.First();
    Is_True(!iter.Is_Empty(),("Empty def list in Unrolled_DU_Update_E"));
    for(; !iter.Is_Empty();node=iter.Next()){
      WN *def = (WN *) node->Wn();
      WN **stid_array = hash_table->Find(def);

      if (!stid_array) {  // an external edge
	for (INT i=1; i<u; i++) {
	  Du_Mgr->Add_Def_Use(def,ldid_array[i]);
        }
      } else {   // an internal edge
	// make the 'u' edges from copy i to copy i
	for (INT i=1; i<u; i++) {
	  Du_Mgr->Add_Def_Use(stid_array[i],ldid_array[i]);
        }
	if (copy_in_edges) { // make the 'u^2' edges
          INT i;
	  for (i=1; i<u; i++) {
	    for (INT j=i+1; j<u; j++) {
	      Du_Mgr->Add_Def_Use(stid_array[j],ldid_array[i]);
	      Du_Mgr->Add_Def_Use(stid_array[i],ldid_array[j]);
            }
          }
	  i = 0;
	  for (INT j=i+1; j<u; j++) {
	    // put these on the list later so we won't screw up the list
	    // we're currently iterating through
	    ldid_zero_defs->Push(stid_array[j]);

	    Du_Mgr->Add_Def_Use(stid_array[i],ldid_array[j]);
          }
        }
      }
    }
    // put the defs we saved on the ldid_zero_defs stack onto the real
    // DU chain
    for (INT j=0; j<ldid_zero_defs->Elements(); j++) {
      Du_Mgr->Add_Def_Use(ldid_zero_defs->Bottom_nth(j),ldid_array[0]);
    }
  
    // copy loop_stmt
    // loop_stmt could be deleted, which is saved in a map
    // Deleted_Loop_Map by LWN_Delete_Tree
    if (loop_stmt && !Deleted_Loop_Map->Find(loop_stmt)) {
      for (INT i=1; i<u; i++) {
        DEF_LIST *def_list_copy = Du_Mgr->Ud_Get_Def(ldid_array[i]);
	if (update_pointers) {
	  WN** stmt_array = hash_table->Find(loop_stmt);
#ifdef KEY
// bug 3388:
// When called from Fission_DU_Update, Unrolled_DU_Update is never called
// with the loop-stmt. So the above hash_table won't contain any entry for it.
// TODO: The correct thing may be to store a mapping from the original loop 
// to an array of the new fission-ed loops in hash_table, in which case the 
// correct thing would be done here, but we don't want to do it now.
//
// From the defn of loop_stmt in opt_du.h and other codes, it seems the use
// must be inside the loop_stmt, so check for that as a temporary solution.
	  WN* stmt;
	  if (stmt_array)
	    stmt = stmt_array[i];
	  else {
	    if (!loop_stmt || WN_opcode(loop_stmt) != OPC_DO_LOOP) 
	      stmt = loop_stmt;
	    else {
	      // Bug 3619 - for a SNL of depth > 1, loop_stmt may be
	      // several levels above ldid_array[i] and may not have an entry
	      // in hash_table. This separates the cases for bugs 3388 and 3619.
	      DO_LOOP_INFO* loop_info = Get_Do_Loop_Info(loop_stmt, FALSE);
	      if (loop_info->Is_Inner) {
	        if (Wn_Is_Inside (ldid_array[i], loop_stmt))
	 	  stmt = loop_stmt;
	        else
		  stmt = NULL;
	      } else 
	        stmt = loop_stmt;
	    }
	  }
#else
	  WN* stmt = stmt_array ? stmt_array[i] : loop_stmt;
#endif
	  def_list_copy->Set_loop_stmt(stmt);
	}
	else
	  def_list_copy->Set_loop_stmt(loop_stmt);
      }
    }
  }
}

// Fix the def-use and use-def chains after unrolling.
// Loopno is the number of the loop being unrolled u times.
// bodies[0..u-1] are identical copies of code
// bodies 0 is the original.
// All the annotations for nest depth had better be right.  There is no way
// to check, because it is legal to have bodies[0] not be connected to
// the func_nd.

void Unrolled_DU_Update(WN **bodies, UINT u, INT loopno, BOOL update_pointers,
	BOOL cross_index)
{
  MEM_POOL_Push(&LNO_local_pool);

  // a hash table to map old loads/stores to arrays of the 
  // corresponding new ones
  typedef HASH_TABLE<WN*, WN**> HTABLE_TYPE;
  HTABLE_TYPE *hash_table =
    CXX_NEW(HTABLE_TYPE(131,&LNO_local_pool), &LNO_local_pool);

  // a stack of the ldids and a stack of the stids in body[0]
  typedef STACK<WN *> WN_STACK;
  WN_STACK *load_stack = CXX_NEW(WN_STACK(&LNO_local_pool),&LNO_local_pool);
  WN_STACK *store_stack = CXX_NEW(WN_STACK(&LNO_local_pool),&LNO_local_pool);

  Unrolled_DU_Update_V(bodies,u,hash_table,load_stack,store_stack);

  SYMBOL *index_loopno = NULL;
  if (!cross_index && (load_stack->Elements())) {
    WN *tmp = load_stack->Bottom_nth(0);
    BOOL done = FALSE;
    while (tmp && !done) {
      if (WN_opcode(tmp) == OPC_DO_LOOP) {
	if (Do_Loop_Depth(tmp) == loopno) {
	  index_loopno = CXX_NEW(SYMBOL(WN_index(tmp)),
			&LNO_local_pool);
	  done = TRUE;
	}
      }
      tmp = LWN_Get_Parent(tmp);
    }
  }

  Unrolled_DU_Update_E(u,loopno,hash_table,load_stack,store_stack, 
	update_pointers,index_loopno);

  CXX_DELETE(hash_table,&LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

// Follow the base pointers up to the top
static ST *Get_ST_Base(ST *st)
{
  ST *base = ST_base(st);
  if (base == st) return base;
  ST *tmp= ST_base(base);
  while (tmp != base) {
    base = tmp;
    tmp = ST_base(tmp);
  }
  return base;
}


//
//         Find the ST_Base of a load.  If it's an OPR_LDA, just
//     Follow the chain of base pointers up to the top.
//     look at it's ST_Base(WN_st(ref)).  Otherwise, follow the def-use
//     chain backwards looking for a single OPR_LDA.  Return NULL if
//     can't find the base or if there is more than one possible base.
//     
ST *Get_ST_Base(WN *load)
{
  OPERATOR opr = WN_operator(load);
  if (opr == OPR_LDA) {
    return Get_ST_Base(WN_st(load));
  } else if (opr == OPR_LDID) {
    DEF_LIST *defs = Du_Mgr->Ud_Get_Def(load);
    if (!defs || defs->Incomplete()) return NULL;
    DEF_LIST_ITER iter(defs);
    const DU_NODE *node=iter.First();
    if (iter.Next()) {  // multiple defs
      return NULL;
    }
#ifdef KEY // bug 7624
    if (iter.Is_Empty())
      return NULL;
#endif
    WN *def = (WN *) node->Wn();
    if (WN_operator(def) == OPR_STID) {
      return Get_ST_Base(WN_kid0(def));
    } else {
      return NULL;
    }
  } else {
    return NULL;
  }
}

//-------------------------------------------------------------------------
// Index_Variable_Live_At_Exit
//
// For a DO loop index, there are only two stores: the one on
// the start, and the one on the step.  Look through the
// uses of those two.  Are all uses within the nest.
//-------------------------------------------------------------------------

static BOOL All_Uses_Within(WN* def, WN* region)
{
  USE_LIST *uses = Du_Mgr->Du_Get_Use(def);
  if (uses->Incomplete()) return FALSE;
  USE_LIST_ITER iter(uses);

  for (const DU_NODE* n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
    const WN *use = n->Wn();
    while (use != region) {
      use = LWN_Get_Parent(use);
      if (use == NULL)
	return FALSE;
    }
  }
  return TRUE;
}

BOOL Index_Variable_Live_At_Exit(WN* loop)
{
  FmtAssert(loop, ("Null loop passed to Index_Variable_Live_At_Exit"));
  return (!All_Uses_Within(WN_step(loop), loop) ||
          !All_Uses_Within(WN_start(loop), loop));
}

static BOOL Is_Used(WN* wn, const SYMBOL& sym)
{
  switch (WN_operator(wn)) {
   case OPR_BLOCK:
    {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
        if (Is_Used(w, sym))
	  return TRUE;
    }
    return FALSE;
   case OPR_LDID:
    return SYMBOL(wn) == sym;
   default:
    for (INT i = 0; i < WN_kid_count(wn); i++)
      if (Is_Used(WN_kid(wn,i), sym))
	return TRUE;
    return FALSE;
  }
}

BOOL Index_Variable_Live_At_Entry(WN* loop)
{
  return Is_Used(WN_start(loop), SYMBOL(WN_index(loop)));
}

extern INT Symbol_Count(WN* wn, const SYMBOL& sym)
{
  INT rval = (WN_operator(wn) == OPR_LDID && sym == SYMBOL(wn)) 
    ? 1 : 0;
  for (INT k = 0; k < WN_kid_count(wn); k++)
    rval += Symbol_Count(WN_kid(wn,k), sym);
  return rval;
}

static void Flip_Le_And_Ge(WN* wn)
{
  OPCODE    opc = WN_opcode(wn);
  OPERATOR  opr = OPCODE_operator(opc);

  switch (opr) {
   case OPR_GE: opr = OPR_LE; break;
   case OPR_LE: opr = OPR_GE; break;
   case OPR_GT: opr = OPR_LT; break;
   case OPR_LT: opr = OPR_GT; break;
   default: FmtAssert(0, ("Bad call to Flip_Le_And_Ge")); break;
  }

  WN_set_opcode(wn, OPCODE_make_op(opr, OPCODE_rtype(opc), OPCODE_desc(opc)));
}

BOOL Solve_For(WN* wn_top, const SYMBOL& sym)
{
  BOOL       ok = FALSE;

  INT        lcount = Symbol_Count(WN_kid0(wn_top), sym);
  INT        rcount = Symbol_Count(WN_kid1(wn_top), sym);
  
  OPERATOR opr_base = WN_operator(wn_top); 
  FmtAssert(opr_base == OPR_GT || opr_base == OPR_LT || opr_base == OPR_LE
    || opr_base == OPR_GE, ("Solve_For() called with bad RELOP"));  

  if (!(lcount == 1 && rcount == 0) && !(lcount == 0 && rcount == 1)) {
    return FALSE;
  }

  // put variable on lhs for the moment
  if (rcount) {
    Flip_Le_And_Ge(wn_top);
    WN* wn0 = WN_kid0(wn_top);
    WN_kid0(wn_top) = WN_kid1(wn_top);
    WN_kid1(wn_top) = wn0;
  }

  WN*        l = WN_kid0(wn_top);
  WN*        r = WN_kid1(wn_top);

  while (1) {
    // invariant at this location: the index is somewhere on the left (l))
    // invariant at this location: l and r will be wn_top's kids

    OPCODE     lopc = WN_opcode(l);
    OPERATOR   lopr = OPCODE_operator(lopc);

    // have we successfully solved for the index variable?
    if (OPCODE_is_load(lopc)) {
      ok = TRUE;
      break;
    }

    if (lopr == OPR_NEG) {
      Flip_Le_And_Ge(wn_top);
      TYPE_ID  type = WN_rtype(r);
      OPCODE   negop = OPCODE_make_op(OPR_NEG, type, MTYPE_V);
      r = WN_CreateExp1(negop, r);
      l = WN_kid0(l); 
      continue; 
    } 

    // A CVT of an expression containing the index varible
    // or any other single kid node is bad news at this point.
    if (lopr == OPR_CVT || WN_kid_count(l) == 1)
      return FALSE;

    lcount = Symbol_Count(WN_kid0(l), sym);
    rcount = Symbol_Count(WN_kid1(l), sym);

    Is_True((lcount == 1 && rcount == 0) ||
            (lcount == 0 && rcount == 1),
            ("Impossible: Counts messed up %d %d", lcount, rcount));

    if (rcount) {
      if (lopr == OPR_SUB) {
        // in order to commute below, must change sign and mul right size
        // through by -1.
        Flip_Le_And_Ge(wn_top);
        TYPE_ID  type = WN_rtype(r);
#ifdef TARG_X8664
	// Bug 2014 - complement the type if original type is unsigned because
	// we are going to negate and we have to obey the sign-extension rules
	// for Opteron Code generation.
	if (MTYPE_is_unsigned(type)) type = MTYPE_complement(type);
#endif
        OPCODE   negop = OPCODE_make_op(OPR_NEG, type, MTYPE_V);
        r = WN_CreateExp1(negop, r);
      }
      else if (lopr != OPR_ADD && lopr != OPR_MPY) // commutative
        break;
      WN* wn0 = WN_kid0(l);
      WN_kid0(l) = WN_kid1(l);
      WN_kid1(l) = wn0;
    }

    WN*        ll = WN_kid0(l);
    WN*        lr = WN_kid1(l);

    if (lopr == OPR_MPY) {
      TYPE_ID   type = OPCODE_rtype(lopc);

      switch (type) {
       case MTYPE_I4:
       case MTYPE_I8:
        break;
       default:
        goto out;        // escape before we change any code.
      }

      // rhs of mul must be a const so we know if we are dividing
      // through by a positive or negative.  Besides, it fits the
      // expected normalized pattern.

      OPCODE   lropc = WN_opcode(lr);
      if (OPCODE_operator(lropc) != OPR_INTCONST)
        break;

      INT v = WN_const_val(lr);
      if (v < 0) {
        Flip_Le_And_Ge(wn_top);
        WN_const_val(lr) = -v;
        OPCODE negop = OPCODE_make_op(OPR_NEG, OPCODE_rtype(lropc), MTYPE_V);
        r = WN_CreateExp1(negop, r);
      }

      BOOL use_ceil = WN_operator(wn_top) == OPR_GE
	|| WN_operator(wn_top) == OPR_LT; 
      if (use_ceil)
        r = LWN_CreateDivceil(type, r, lr);
      else
        r = LWN_CreateDivfloor(type, r, lr);

      WN_Delete(l);
      l = ll;
    }
    else if (lopr == OPR_ADD || lopr == OPR_SUB) {
      WN_kid0(l) = r;
      WN_kid1(l) = lr;
      r = l;
      l = ll;
      WN_set_opcode(r, OPCODE_make_op(lopr == OPR_ADD ? OPR_SUB : OPR_ADD,
                                      OPCODE_rtype(lopc), OPCODE_desc(lopc)));
    }
    else
      return FALSE;
  }

 out:


  WN_kid0(wn_top) = l;
  WN_kid1(wn_top) = r;
  LWN_Parentize(wn_top);

  return ok;
}

//-----------------------------------------------------------------------
// NAME: Do_Loop_Is_Unsigned
// FUNCTION: Returns TRUE if the do loop 'wn_loop' an unsigned index 
//   variable or if there is any unsigned multiply in the bound
//   FALSE otherwise.  
//-----------------------------------------------------------------------

extern BOOL Do_Loop_Is_Unsigned(WN* wn_loop) 
{
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP, 
    ("Do_Loop_Is_Unsigned() called on a non-do-loop."));
  TYPE_ID type = WN_desc(WN_end(wn_loop));
  return MTYPE_is_unsigned(type);
}

extern BOOL Upper_Bound_Standardize(WN* ub, BOOL ok_to_fail)
{
  WN* doloop = LWN_Get_Parent(ub);
  FmtAssert(WN_opcode(doloop) == OPC_DO_LOOP, ("Bad ub passed"));
  if (Do_Loop_Is_Unsigned(doloop) 
	&& (UBvar(ub) == NULL || WN_operator(ub) != OPR_LE)) {
    FmtAssert(ok_to_fail, 
      ("Upper bound of unsigned do loop should already be standardized."));
    return FALSE;  
  }
   
  BOOL ok = Solve_For(ub, SYMBOL(WN_index(doloop)));
  if (ok == FALSE) {
    FmtAssert(ok_to_fail, ("Upper_Bound_Standardize() could not solve for %s",
                            SYMBOL(WN_index(doloop)).Name()));
    return FALSE;
  }

  OPCODE      opc = WN_opcode(ub);
  OPERATOR    opr = OPCODE_operator(opc);
  if (opr != OPR_LT && opr != OPR_LE) {
    FmtAssert(ok_to_fail,
            ("surprise operator %s returned from Solve_For()",
	     OPCODE_name(opc)));
    return FALSE;
  }

  if (opr == OPR_LT) {
    if (MTYPE_is_integral(OPCODE_desc(opc))) {
      // change i < b to i <= b-1
      TYPE_ID desc = OPCODE_desc(opc);
      WN_set_opcode(ub, OPCODE_make_op(OPR_LE, OPCODE_rtype(opc), desc));
      OPCODE subop = OPCODE_make_op(OPR_SUB, desc, MTYPE_V);
      WN* ub1 = NULL; 
      ub1 = LWN_CreateExp2(subop, WN_kid1(ub), LWN_Make_Icon(desc, 1));
      WN_kid1(ub) = ub1;
      LWN_Copy_Frequency_Tree(ub1, ub);
      LWN_Set_Parent(ub1, ub);
    } else { 
      FmtAssert(ok_to_fail, ("Cannot convert LT to LE"));
      return FALSE; 
    } 
  } 

  return ok;
}

//-------------------------------------------------------------------------
// Outermost_Enclosing_Loop(WN* loop)  
//
// Returns the outermost loop enclosing 'loop', which may be 'loop' itself. 
//-------------------------------------------------------------------------

WN* Outermost_Enclosing_Loop(WN* loop) 
{ 
  WN* outerloop = loop; 
  while (Do_Loop_Depth(outerloop) > 0) { 
    outerloop = LWN_Get_Parent(outerloop); 
    while (WN_opcode(outerloop) != OPC_DO_LOOP) {
      outerloop = LWN_Get_Parent(outerloop); 
    } 
  }
  return outerloop; 
}
 
//-------------------------------------------------------------------------
// Outermost_Enclosing_Good_Loop(WN* loop)  
//
// Returns the outermost loop enclosing 'loop', which may be 'loop' itself.
//-------------------------------------------------------------------------

WN* Outermost_Enclosing_Good_Loop(WN* loop) 
{
  if (!Do_Loop_Is_Good(loop))
    return NULL; 

  WN* last_good_loop = loop; 
  WN* outerloop = loop;  
  while (Do_Loop_Depth(outerloop) > 0) { 
    outerloop = LWN_Get_Parent(outerloop); 
    while (WN_opcode(outerloop) != OPC_DO_LOOP) {
      outerloop = LWN_Get_Parent(outerloop); 
    }
    if (!Do_Loop_Is_Good(outerloop))
      break;  
    last_good_loop = outerloop;  
  }
  return last_good_loop;
}

//-----------------------------------------------------------------------
// NAME: Patch_Uses_In_Loop 
// FUNCTION: Make a def->use arc from 'wn_asg' to each possible load which 
//   is aliased to it in 'wn_loop'. 
//-----------------------------------------------------------------------

static void Patch_Uses_In_Loop(WN* wn_asg, 
 			       WN* wn_loop)
{
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_loop);
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn;
    if (OPCODE_is_load(WN_opcode(wn)) 
	&& (Aliased(Alias_Mgr, wn_asg, wn) != NOT_ALIASED))
      Du_Mgr->Add_Def_Use(wn_asg, wn); 
  }
}

//-----------------------------------------------------------------------
// NAME: Update_Nest_Depth_Traverse
// FUNCTION: Recursively update the Nest_Depth() of all access vectors in 
//   the tree rooted at 'wn_tree'.
//-----------------------------------------------------------------------

static void Update_Nest_Depth_Traverse(WN* wn_tree)
{
  if (OPCODE_operator(WN_opcode(wn_tree)) == OPR_ARRAY) {
    INT loop_count = 0;
    for (WN* wn = wn_tree; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_opcode(wn) == OPC_DO_LOOP)
        loop_count++;
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_tree);
    for (INT i = 0; i < aa->Num_Vec(); i++)
      aa->Dim(i)->Set_Nest_Depth(loop_count);
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Update_Nest_Depth_Traverse(wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Update_Nest_Depth_Traverse(WN_kid(wn_tree, i));
  }
}
 
//-----------------------------------------------------------------------
// NAME: Update_Nest_Depth
// FUNCTION: Update the Nest_Depth() of all access vectors in the tree 
//   rooted at 'wn_tree'. 
//-----------------------------------------------------------------------

static void Update_Nest_Depth(WN* wn_tree)
{ 
  Update_Nest_Depth_Traverse(wn_tree);
} 

//-------------------------------------------------------------------------
// NAME: Finalize_Index_Variable
// FUNCTION: Use this function to generate a final value expression for
//   the loop 'wn' after or before the loop.  If 'insert_after_loop', insert 
//   the final value expression after the loop, otherwise put it before the
//   loop.  If 'insert_after_loop' and 'try_sink', attempt to sink it out 
//   of as many loops as possible.
//-------------------------------------------------------------------------

extern WN* global_fd; 

void Finalize_Index_Variable(WN* loop, BOOL insert_after_loop, BOOL try_sink)
{ 
  // Find basic start, stop, and step expression quantities. 
  WN* start_exp = WN_kid0(WN_start(loop)); 
  SYMBOL index = SYMBOL(WN_start(loop));
  Upper_Bound_Standardize(WN_end(loop));   
  FmtAssert(WN_operator(WN_end(loop)) == OPR_LE,  
    ("Comparison for DO is not .LE. after standardization")); 

  WN* stop_exp = WN_kid1(WN_end(loop));
  WN* step_add = WN_kid0(WN_step(loop));
  WN* step_exp = WN_operator(WN_kid0(step_add)) == OPR_LDID 
    && SYMBOL(WN_kid0(step_add)) == index 
    ? WN_kid1(step_add) : WN_kid0(step_add);

  // Assert some basic things about the loop variable loads and stores.  
  // Form an expression for the number of iterations in the loop.  
  TYPE_ID wtype = Do_Wtype(loop); 
  WN* start_new = LWN_Copy_Tree(start_exp, TRUE, LNO_Info_Map); 
  LWN_Copy_Def_Use(start_exp,start_new,Du_Mgr);
  if (Array_Dependence_Graph != NULL) { 
    if (!Array_Dependence_Graph->
	 Add_Deps_To_Copy_Block(start_exp, start_new, FALSE))
      LNO_Erase_Dg_From_Here_In(loop,Array_Dependence_Graph);
  } 
  WN* stop_new = LWN_Copy_Tree(stop_exp, TRUE, LNO_Info_Map); 
  LWN_Copy_Def_Use(stop_exp,stop_new,Du_Mgr);
  if (Array_Dependence_Graph != NULL) { 
    if (!Array_Dependence_Graph->
	 Add_Deps_To_Copy_Block(stop_exp, stop_new, FALSE))
      LNO_Erase_Dg_From_Here_In(loop,Array_Dependence_Graph);
  } 
  WN* step_new = LWN_Copy_Tree(step_exp, TRUE, LNO_Info_Map); 
  LWN_Copy_Def_Use(step_exp,step_new,Du_Mgr);
  OPCODE add = OPCODE_make_op(OPR_ADD, Promote_Type(wtype), MTYPE_V);
  OPCODE sub = OPCODE_make_op(OPR_SUB, Promote_Type(wtype), MTYPE_V);
  WN* diff = LWN_CreateExp2(sub, stop_new, start_new);  
  WN* iters = LWN_CreateExp2(add, diff, step_new); 
  if (!(WN_operator(step_exp) == OPR_INTCONST 
    && WN_const_val(step_exp) == 1)) { 
    step_new = LWN_Copy_Tree(step_exp, TRUE, LNO_Info_Map); 
    LWN_Copy_Def_Use(step_exp,step_new,Du_Mgr);
    OPCODE div = OPCODE_make_op(OPR_DIV, Promote_Type(wtype), MTYPE_V);
    iters = LWN_CreateExp2(div, iters, step_new); 

    if (MTYPE_is_float(OPCODE_rtype(div))) {
      // if it is a float, need to do a trunc to get the integer part.
      TYPE_ID rtype;
      switch (OPCODE_rtype(div)) {
      case MTYPE_F4:
        rtype = MTYPE_I4;
        break;
      case MTYPE_F8:
        rtype = MTYPE_I8;
        break;
      default:
        FmtAssert(FALSE,
                  ("Weird floating-point index: cannot compute tripcount\n"));
        break;
      }
      iters = LWN_CreateExp1(OPCODE_make_op(OPR_TRUNC,rtype,OPCODE_rtype(div)),
                             iters);

      // now convert back to original float-type for remaining
      // arithmetic
      OPCODE cvt_op = OPCODE_make_op(OPR_CVT, OPCODE_rtype(div), rtype);
      iters = LWN_CreateExp1(cvt_op, iters);
    }

    step_new = LWN_Copy_Tree(step_exp, TRUE, LNO_Info_Map); 
    LWN_Copy_Def_Use(step_exp,step_new,Du_Mgr);
    OPCODE mul = OPCODE_make_op(OPR_MPY, Promote_Type(wtype), MTYPE_V);
    iters = LWN_CreateExp2(mul, iters, step_new); 
  } 
  start_new = LWN_Copy_Tree(start_exp, TRUE, LNO_Info_Map); 
  LWN_Copy_Def_Use(start_exp,start_new,Du_Mgr);
  if (Array_Dependence_Graph != NULL) { 
    if (!Array_Dependence_Graph->
	 Add_Deps_To_Copy_Block(start_exp, start_new, FALSE))
      LNO_Erase_Dg_From_Here_In(loop,Array_Dependence_Graph);
  } 
  WN* final_lhs = LWN_Copy_Tree(start_exp, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(start_exp, final_lhs, Du_Mgr);
  if (Array_Dependence_Graph != NULL) { 
    if (!Array_Dependence_Graph->
	 Add_Deps_To_Copy_Block(start_exp, final_lhs, FALSE))
      LNO_Erase_Dg_From_Here_In(loop,Array_Dependence_Graph);
  } 
  add = OPCODE_make_op(OPR_ADD, Promote_Type(wtype), MTYPE_V);  
  WN* final_rhs = LWN_CreateExp2(add, start_new, iters);
  OPCODE max = OPCODE_make_op(OPR_MAX, Promote_Type(wtype), MTYPE_V); 
  WN* final = LWN_CreateExp2(max, final_lhs, final_rhs); 
  OPCODE final_op = OPCODE_make_op(OPR_STID, MTYPE_V, wtype); 
  WN* final_store = LWN_CreateStid(final_op, WN_start(loop), final); 
  Du_Mgr->Create_Use_List(final_store);

  if (insert_after_loop) {
    // Place that final expression after the loop. 
    LWN_Insert_Block_After(LWN_Get_Parent(loop), loop, final_store);  
  } else {
    // Place that final expression after the loop. 
    LWN_Insert_Block_Before(LWN_Get_Parent(loop), loop, final_store);  
  }
  Update_Nest_Depth(final_store);

  // Add uses for this definition. 
  WN* start_asg = WN_start(loop);
  WN* stop_opr = WN_end(loop);
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(start_asg); 
  USE_LIST_ITER iter(use_list); 
  for (DU_NODE *use_node=(DU_NODE *)iter.First(); !iter.Is_Empty(); ) {
    WN* use=use_node->Wn();
    use_node=(DU_NODE *)iter.Next();
    WN* new_loop=use;
    while (new_loop && new_loop !=loop && WN_opcode(new_loop)!=OPC_FUNC_ENTRY)
        new_loop=LWN_Get_Parent(new_loop);
    if (new_loop!=loop) {
      // a use outside the first loop
      Du_Mgr->Delete_Def_Use(WN_start(loop),use);
      Du_Mgr->Delete_Def_Use(WN_step(loop),use);
      Du_Mgr->Add_Def_Use(final_store,use);
    }
  }

  // For start and step asg, make sure that all uses in the loop have 
  // DU chains to them.  
  USE_LIST* local_use_list = Du_Mgr->Du_Get_Use(WN_start(loop));
  if (local_use_list->Incomplete())
    Patch_Uses_In_Loop(WN_start(loop), loop); 
  local_use_list = Du_Mgr->Du_Get_Use(WN_step(loop)); 
  if (local_use_list->Incomplete())
    Patch_Uses_In_Loop(WN_step(loop), loop);  
 
  if (use_list->Incomplete())
    Du_Mgr->Du_Get_Use(final_store)->Set_Incomplete();
  if (!Do_Loop_Has_Calls(loop)) { 
    use_list->Reset_Incomplete();
    use_list = Du_Mgr->Du_Get_Use(WN_step(loop)); 
    use_list->Reset_Incomplete();
  }
  scalar_rename(WN_start(loop)); 

  if (insert_after_loop && try_sink) { 
    WN* wn_sink_loop = NULL; 
#ifndef KEY
    for (WN* wn = LWN_Get_Parent(loop); wn != NULL; wn = LWN_Get_Parent(wn))  
#else
    // Bug 2901 - can not sink out if final_store is inside an enclosing 'if'
    for (WN* wn = LWN_Get_Parent(loop); 
	 wn != NULL && WN_operator(wn) != OPR_IF; 
	 wn = LWN_Get_Parent(wn))  
#endif
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && Statement_Sinkable_Out_Of_Loop(final_store, wn))
	wn_sink_loop = wn; 
    if (wn_sink_loop != NULL) 
      Sink_Out_Sandwiched_Statement(final_store, wn_sink_loop); 
  }
} 

//-----------------------------------------------------------------------
// NAME: Finalize_Index_Variable_For_Remove_Unity_Trip_Loop
// FUNCTION: Same as the above, but doesn't require Upper_Bound_Standardize()
//   to work, because the final value is the initial value.
//-----------------------------------------------------------------------

void Finalize_Index_Variable_For_Remove_Unity_Trip_Loop(WN* loop, 
  BOOL insert_after_loop, BOOL try_sink)
{ 
  // Find basic start, stop, and step expression quantities. 
  WN* start_exp = WN_kid0(WN_start(loop)); 
  SYMBOL index = SYMBOL(WN_start(loop));

  // Assert some basic things about the loop variable loads and stores.  
  // Form an expression for the number of iterations in the loop.  
  TYPE_ID wtype = Do_Wtype(loop); 
  WN* start_new = LWN_Copy_Tree(start_exp, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(start_exp, start_new, Du_Mgr);
  if (Array_Dependence_Graph != NULL) { 
    if (!Array_Dependence_Graph->
	 Add_Deps_To_Copy_Block(start_exp, start_new, FALSE))
      LNO_Erase_Dg_From_Here_In(loop,Array_Dependence_Graph);
  } 
  WN* step_add = WN_kid0(WN_step(loop));
  WN* step_exp = WN_operator(WN_kid0(step_add)) == OPR_LDID
    && SYMBOL(WN_kid0(step_add)) == index
    ? WN_kid1(step_add) : WN_kid0(step_add);
  WN* step_new = LWN_Copy_Tree(step_exp, TRUE, LNO_Info_Map);
  LWN_Copy_Def_Use(step_exp,step_new,Du_Mgr);
  OPCODE add = OPCODE_make_op(OPR_ADD, Promote_Type(wtype), MTYPE_V);
  WN* final = LWN_CreateExp2(add, start_new, step_new);
  OPCODE final_op = OPCODE_make_op(OPR_STID, MTYPE_V, wtype); 
  WN* final_store = LWN_CreateStid(final_op, WN_start(loop), final); 
  Du_Mgr->Create_Use_List(final_store);

  if (insert_after_loop) {
    // Place that final expression after the loop. 
    LWN_Insert_Block_After(LWN_Get_Parent(loop), loop, final_store);  
  } else {
    // Place that final expression after the loop. 
    LWN_Insert_Block_Before(LWN_Get_Parent(loop), loop, final_store);  
  }

  // Add uses for this definition. 
  WN* start_asg = WN_start(loop);
  USE_LIST* use_list = Du_Mgr->Du_Get_Use(start_asg); 
  USE_LIST_ITER iter(use_list); 
  for (DU_NODE *use_node=(DU_NODE *)iter.First(); !iter.Is_Empty(); ) {
    WN* use=use_node->Wn();
    use_node=(DU_NODE *)iter.Next();
    WN* new_loop=use;
    while (new_loop !=loop && WN_opcode(new_loop)!=OPC_FUNC_ENTRY)
        new_loop=LWN_Get_Parent(new_loop);
    if (new_loop!=loop) {
      // a use outside the first loop
      Du_Mgr->Delete_Def_Use(WN_start(loop),use);
      Du_Mgr->Delete_Def_Use(WN_step(loop),use);
      Du_Mgr->Add_Def_Use(final_store,use);
    }
  }

  // For start and step asg, make sure that all uses in the loop have 
  // DU chains to them.  
  USE_LIST* local_use_list = Du_Mgr->Du_Get_Use(WN_start(loop));
  if (local_use_list->Incomplete())
    Patch_Uses_In_Loop(WN_start(loop), loop); 
  local_use_list = Du_Mgr->Du_Get_Use(WN_step(loop)); 
  if (local_use_list->Incomplete())
    Patch_Uses_In_Loop(WN_step(loop), loop);  
 
  if (use_list->Incomplete())
    Du_Mgr->Du_Get_Use(final_store)->Set_Incomplete();
  if (!Do_Loop_Has_Calls(loop)) { 
    use_list->Reset_Incomplete();
    use_list = Du_Mgr->Du_Get_Use(WN_step(loop)); 
    use_list->Reset_Incomplete();
  }
  scalar_rename(WN_start(loop)); 

  if (insert_after_loop && try_sink) { 
    WN* wn_sink_loop = NULL; 
    for (WN* wn = LWN_Get_Parent(loop); wn != NULL; wn = LWN_Get_Parent(wn))  
      if (WN_opcode(wn) == OPC_DO_LOOP 
	  && Statement_Sinkable_Out_Of_Loop(final_store, wn))
	wn_sink_loop = wn; 
    if (wn_sink_loop != NULL) 
      Sink_Out_Sandwiched_Statement(final_store, wn_sink_loop); 
  }
} 
#ifdef Is_True_On

//-------------------------------------------------------------------------
// LNO_Check_Du
//-------------------------------------------------------------------------

static BOOL LNO_Check_Du_HT(WN* orig,
                            WN* copy,
                            HASH_TABLE<WN*,WN*>* ht)
{
  if (orig == NULL || copy == NULL) {
    fprintf(stderr,
            "lnoutils detects PREOPT II failure: missing orig or copy\n");
    return FALSE;
  }
  if (WN_opcode(orig) != WN_opcode(copy)) {
    fprintf(stderr,
            "lnoutils detects PREOPT II failure: orig op=%d copy op=%d\n",
            WN_opcode(orig), WN_opcode(copy));
    return FALSE;
  }

  OPCODE	opc = WN_opcode(orig);
  OPERATOR	opr = OPCODE_operator(opc);

  if (opr == OPR_LDID || opr == OPR_STID ||
      opr == OPR_DO_LOOP || opr == OPR_FUNC_ENTRY ||
      OPCODE_is_call(opc))
    ht->Enter(copy, orig);

  if (WN_opcode(orig) == OPC_BLOCK) {
    WN* o = WN_first(orig);
    WN* c = WN_first(copy);
    while (o) {
      if (!LNO_Check_Du_HT(o, c, ht))
        return FALSE;
      o = WN_next(o);
      c = WN_next(c);
    }
  }
  else {
    for (INT k = 0; k < WN_kid_count(orig); k++)
      if (!LNO_Check_Du_HT(WN_kid(orig,k),WN_kid(copy,k), ht))
        return FALSE;
  }

  return TRUE;
}

static BOOL LNO_Check_Du_Check(HASH_TABLE<WN*,WN*>* ht)
{
  WN* orig;
  WN* copy;

  BOOL rval = TRUE;

  HASH_TABLE_ITER<WN*,WN*> it(ht);
  while (it.Step(&copy, &orig)) {

    OPCODE	opc = WN_opcode(orig);
    OPERATOR	opr = OPCODE_operator(opc);

    FmtAssert(opc == WN_opcode(copy), ("Flawed hashtable"));

    if (opr == OPR_LDID) {
      // every entry in the copy's deflist should also be in the original's.

      DEF_LIST* dl = Du_Mgr->Ud_Get_Def(copy);
      INT dl_len = dl == NULL ? 0 : dl->Len();
      if (dl_len == 0) {
        WN *cp;
        for (cp = LWN_Get_Parent(copy); cp; cp = LWN_Get_Parent(cp))
          if (WN_opcode(cp) == OPC_IO)
            break;
        FmtAssert(cp, ("Missing def list in copy"));
      }

      DEF_LIST* dl2 = Du_Mgr->Ud_Get_Def(orig);
      INT dl2_len = dl2 == NULL ? 0 : dl2->Len();
      // list could be missing in origninal -- that'll be reported below

      if (dl_len && dl2_len &&
	  ((dl2->Loop_stmt() == NULL &&
	    dl->Loop_stmt() != NULL) ||
	   (dl2->Loop_stmt() != NULL &&
	    dl2->Loop_stmt() != ht->Find(dl->Loop_stmt())))) {
	rval = FALSE;
	printf("DU check: Bad deflist do: use=%p[%p] do=%p[%p]\n",
	       orig, copy, dl2->Loop_stmt(), dl->Loop_stmt());
      }

      DEF_LIST_ITER iter(dl);
      for (DU_NODE* du = iter.First(); !iter.Is_Empty(); du = iter.Next()) {
	BOOL ok = FALSE;
	DEF_LIST_ITER iter2(dl2);
	WN* duorig = ht->Find(du->Wn());
	for (DU_NODE* du2 = iter2.First(); !iter2.Is_Empty(); du2 = iter2.Next()) {
	  if (du2->Wn() == duorig) {
	    ok = TRUE;
	    break;
	  }
	}
	if (ok == FALSE) {
	  rval = FALSE;
	  printf("DU check: Inadequate defs: use=%p[%p] def=%p[%p]\n",
		 orig, copy, duorig, du->Wn());
	}
      }
    }
    else if (opr == OPR_STID || OPCODE_is_call(opc)) {
      // every entry in the copy's deflist should also be in the original's.

      USE_LIST* ul = Du_Mgr->Du_Get_Use(copy);
      INT ul_len = ul == NULL ? 0 : ul->Len();
      if (!OPCODE_is_call(opc) && !ul_len &&
          !((ST_class(WN_st(orig))==CLASS_PREG) && 
	    Preg_Is_Dedicated((WN_offset(orig))))) {
	fprintf(stderr,"Missing use list in copy \n");
      }
//      FmtAssert(OPCODE_is_call(opc) || ul_len, ("Missing use list in copy"));

      USE_LIST* ul2 = Du_Mgr->Du_Get_Use(orig);
      // might be missing in original -- will be reported below

      USE_LIST_ITER iter(ul);
      for (DU_NODE* du = iter.First(); !iter.Is_Empty(); du = iter.Next()) {
	BOOL ok = FALSE;
	USE_LIST_ITER iter2(ul2);
	WN* duorig = ht->Find(du->Wn());
	for (DU_NODE* du2 = iter2.First(); !iter2.Is_Empty(); du2 = iter2.Next()) {
	  if (du2->Wn() == duorig) {
	    ok = TRUE;
	    break;
	  }
	}
	if (ok == FALSE) {
	  rval = FALSE;
	  printf("DU check: Inadequate uses: def=%p[%p] use=%p[%p]\n",
		 orig, copy, duorig, du->Wn());
	}
      }
    }
    else {
      FmtAssert(opr == OPR_DO_LOOP || opr == OPR_FUNC_ENTRY,
      		("Bad opcode in hash table"));
    }
  }

  return rval;
}

void LNO_Check_Du(WN* orig)
{
  WN* copy = LWN_Copy_Tree(orig);
  Set_Error_Phase ( "Pre-Optimizer DU" );
  copy = Pre_Optimizer(PREOPT_DUONLY_PHASE, copy, Du_Mgr, Alias_Mgr);
  Set_Error_Phase ( "Loop nest optimizer Post-DU" );
  LWN_Parentize(copy);
  Mark_Code(copy, FALSE, TRUE);
  LNO_Build_Access(copy, &LNO_default_pool);

  MEM_POOL_Push(&LNO_local_pool);
  HASH_TABLE<WN*,WN*> ht1(307, &LNO_local_pool);
  if (!LNO_Check_Du_HT(orig, copy, &ht1)) {
    fprintf(stderr, "LNO_Check_Du() failed to check DU chains because of preopt error\n");
  }
  else if (!LNO_Check_Du_Check(&ht1)) {
    // print some debugging information
    printf("*** ORIG\n");
    Dump_WN(orig, stdout, 3);
    printf("*** COPY\n");
    Dump_WN(copy, stdout, 3);
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

#endif

WN* WN_prev_executable(WN* wn)
{
  wn = WN_prev(wn);
  if (wn && OPCODE_is_not_executable(WN_opcode(wn)))
    wn = WN_prev(wn);
  return wn;
}

WN* WN_next_executable(WN* wn)
{
  wn = WN_next(wn);
  if (wn && OPCODE_is_not_executable(WN_opcode(wn)))
    wn = WN_next(wn);
  return wn;
}

static void LNO_Erase_Vertices_In_Loop_Rec(WN *wn, ARRAY_DIRECTED_GRAPH16 *dg)
{
  OPCODE opcode = WN_opcode(wn);
  if (WN_opcode(wn) == OPC_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w)) {
      LNO_Erase_Vertices_In_Loop_Rec(w, dg);
    }
  } else if (WN_opcode(wn) != OPC_DO_LOOP) {
    for (INT k = 0; k < WN_kid_count(wn); k++) {
      LNO_Erase_Vertices_In_Loop_Rec(WN_kid(wn,k), dg);
    }
  }
  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode) || 
      OPCODE_is_call(opcode) || (OPCODE_operator(opcode) == OPR_INTRINSIC_OP)
#ifdef KEY
      || (OPCODE_operator(opcode) == OPR_PURE_CALL_OP)
#endif
      ) {
    VINDEX16  v = dg->Get_Vertex(wn);
    if (v) {
      dg->Delete_Vertex(v);
    }
  }
}

void LNO_Erase_Vertices_In_Loop(WN *wn, ARRAY_DIRECTED_GRAPH16 *dg)
{
  while (wn && (WN_opcode(wn) != OPC_DO_LOOP)) wn = LWN_Get_Parent(wn);
  if (!wn) return;
  for (INT k = 0; k < WN_kid_count(wn); k++) {
    LNO_Erase_Vertices_In_Loop_Rec(WN_kid(wn,k),dg);
  }
}

void Du_Sanity_Check_Matching_Du(STACK<WN*>* read_stack,
  STACK<WN*>* write_stack, FILE* fp, UINT fancy)
{
  MEM_POOL_Push(&LNO_local_pool);

  {

  UINT write_count=write_stack->Elements();
  UINT read_count=read_stack->Elements();
  HASH_TABLE<WN*,INT> bit_pos(2*read_count, &LNO_local_pool);
  HASH_TABLE<WN*,INT> write_index(2*write_count, &LNO_local_pool);

  INT si;
  for (si=0; si<read_stack->Elements(); si++) {
    WN *read = read_stack->Bottom_nth(si);
    bit_pos.Enter(read,si+1);
  }

  BIT_VECTOR* write_vector=
    CXX_NEW_ARRAY(BIT_VECTOR, write_count+1, &LNO_local_pool);

  UINT* Use_Count=CXX_NEW_ARRAY(UINT, write_count+1, &LNO_local_pool);

  for (si=0; si<write_stack->Elements(); si++) {
    Use_Count[si+1]=0;
    write_vector[si+1].Init(read_count+1, &LNO_local_pool);
    WN *write = write_stack->Bottom_nth(si);
    write_index.Enter(write, si+1);
    USE_LIST* use_list=Du_Mgr->Du_Get_Use(write);
    USE_LIST_ITER u_iter(use_list);
    for (DU_NODE* u_node=u_iter.First(); !u_iter.Is_Empty();
      u_node=u_iter.Next()) {
      WN* use=u_node->Wn();
      Use_Count[si+1]++;
      UINT bit=bit_pos.Find(use);
      if (bit!=0)
        write_vector[si+1].Set(bit);
      else if (WN_operator(use)!=OPR_IO) {
        OPERATOR opr=WN_operator(write);
        DevWarn("%s %d [%p]has a non-matching DU relation with node: %d [%p]\n",
		OPERATOR_name(opr), WN_map_id(write), write, WN_map_id(use), use);
      }
    }
  }

  for (si=0; si<read_stack->Elements(); si++) {
    WN *read = read_stack->Bottom_nth(si);
    UINT bit=si+1;
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(read);
    DEF_LIST_ITER d_iter(def_list);
    for (DU_NODE* d_node=d_iter.First(); !d_iter.Is_Empty();
      d_node=d_iter.Next()) {
      WN* def=d_node->Wn();
      UINT index=write_index.Find(def);
      if (index!=0 && write_vector[index].Test(bit)) {
        // write_vector[index].Reset(bit);
        // Uncomment this once we have unique edge
        Use_Count[index]--;
      } else if (WN_operator(def)!=OPR_IO) {
        OPERATOR opr=WN_operator(read);
	DevWarn("%s %d [%p] has a non-matching DU relation with node: %d [%p]\n",
		OPERATOR_name(opr), WN_map_id(read), read, WN_map_id(def), def);
      }
    }
  }

  // The following should go away once pre-opt put unique edge in DU
  for (si=0; si<read_stack->Elements(); si++) {
    WN *read = read_stack->Bottom_nth(si);
    UINT bit=si+1;
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(read);
    DEF_LIST_ITER d_iter(def_list);
    for (DU_NODE* d_node=d_iter.First(); !d_iter.Is_Empty();
      d_node=d_iter.Next()) {
      WN* def=d_node->Wn();
      UINT index=write_index.Find(def);
      if (index!=0 && write_vector[index].Test(bit)) {
        write_vector[index].Reset(bit);
      }
    }
  }

  for (si=0; si<write_stack->Elements(); si++) {
    WN *write = write_stack->Bottom_nth(si);
    if (Use_Count[si+1]!=0) {
      INT i;
      UINT index=si+1;
      while ((i=write_vector[si+1].Least_Non_Zero()) != -1) {
        WN* use=read_stack->Bottom_nth(i-1);
        if (WN_operator(use)!=OPR_IO) {
          OPERATOR opr=WN_operator(write);
	  DevWarn(" %s %d %p has a non-matching DU relation with node: %d [%p]\n",
		  OPERATOR_name(opr), WN_map_id(write), write, WN_map_id(use), use);
        }
        write_vector[index].Reset(i);
      }
    }
  }

  }

  MEM_POOL_Pop(&LNO_local_pool);

}


static void Du_Sanity_Check_r(
  WN* wn, HASH_TABLE<WN*,INT>* h_table, UINT pass, FILE* fp, UINT fancy,
  STACK<WN*> *reads, STACK<WN*> *writes)
{
  OPCODE opc=WN_opcode(wn);
  OPERATOR opr=OPCODE_operator(opc);

  if (pass==0) {
    if (OPCODE_is_load(opc) || OPCODE_is_store(opc) || opr == OPR_ALTENTRY ||
        opr==OPR_FUNC_ENTRY || opr==OPR_RETURN || OPCODE_has_barrier(opc) ||
        opr==OPR_PARM || (opr==OPR_LABEL && WN_Label_Is_Handler_Begin(wn)) ||
        opr==OPR_IO || OPCODE_is_call(opc) || opr==OPR_INTRINSIC_OP
#ifdef KEY
        || opr==OPR_PURE_CALL_OP
        || opr==OPR_GOTO_OUTER_BLOCK
#endif
	)
      h_table->Enter(wn,1);
  } else {
    if (OPCODE_is_load(opc) || OPCODE_is_store(opc)) {
      if (Aliased(Alias_Mgr,wn,wn)==NOT_ALIASED) {
        DevWarn("%s %d [%p] is not aliased to itself\n", 
		OPERATOR_name(opr), WN_map_id(wn), wn);
      }
    }
    if (Du_Mgr->Ud_Get_Def(wn)) {
      reads->Push(wn);
      DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(wn);
      WN* loop=def_list->Loop_stmt();

#ifdef KEY //bug 12856: don't check Loop_stmt for iloads since
           //it will never be used
      if (loop && WN_operator(wn)!=OPR_ILOAD) {
#else
      if (loop) {
#endif
        if (WN_opcode(loop)!=OPC_DO_LOOP) {
          DevWarn("%s %d [%p] has a non-loop node as loop_stmt: %d (%p %p)\n", 
		  OPERATOR_name(opr), WN_map_id(wn), wn, WN_map_id(loop), wn, loop);
        }
        WN* wn1=wn;
        while (WN_opcode(wn1)!=OPC_FUNC_ENTRY) {
          if (wn1==loop)
            break;
          else
            wn1=LWN_Get_Parent(wn1);
        }
        if (wn1!=loop) {
          DevWarn("%s %d [%p] has a non-ancestor node as loop_stmt: %d\n", 
		  OPERATOR_name(opr), WN_map_id(wn), wn, WN_map_id(loop));
        }
      }
      BOOL ldid_in_do_loop_head=FALSE;
      WN* parent_loop;
      if (WN_operator(wn)==OPR_LDID) {
        parent_loop=LWN_Get_Parent(wn);
        while (!OPCODE_is_scf(WN_opcode(parent_loop)))
          parent_loop=LWN_Get_Parent(parent_loop);
        if (WN_opcode(parent_loop)==OPC_DO_LOOP && parent_loop==loop
            && SYMBOL(wn) == SYMBOL(WN_index(parent_loop)))
          ldid_in_do_loop_head=TRUE;
      }
      DEF_LIST_ITER d_iter(def_list);
      for (DU_NODE *def_node=(DU_NODE *)d_iter.First();
            !d_iter.Is_Empty(); def_node=(DU_NODE *)d_iter.Next()) {
        WN* def1=def_node->Wn();
        if (ldid_in_do_loop_head) {
          if (LWN_Get_Parent(def1)!=parent_loop) {
            DevWarn("%s %d [%p] is ldid in loop head but has def out of loop head: %d [%p]\n", 
	      OPERATOR_name(opr), WN_map_id(wn), wn, WN_map_id(def1), def1);
          }
        }
        if (!h_table->Find(def1)) {
          DevWarn("%s %d [%p] has a def outside the tree: %d\n",
		  OPERATOR_name(opr), WN_map_id(wn), wn, WN_map_id(def1));
        }
      }
    } else if (opr == OPR_LDID) {
      DevWarn(" %s %d [%p] is missing def_list\n", 
	      OPERATOR_name(opr), WN_map_id(wn), wn);
    }
    if (Du_Mgr->Du_Get_Use(wn)) {
      writes->Push(wn);
      USE_LIST* use_list=Du_Mgr->Du_Get_Use(wn);
      USE_LIST_ITER u_iter(use_list);
      for (DU_NODE *use_node=(DU_NODE *)u_iter.First();
            !u_iter.Is_Empty(); use_node=(DU_NODE *)u_iter.Next()) {
        WN* use1=use_node->Wn();
        if (!h_table->Find(use1)) {
          DevWarn("Def %d [%p] has a use outside the tree: %d [%p]\n", 
		  WN_map_id(wn), wn, WN_map_id(use1), use1);
        }
     }
    } else if ((opr == OPR_STID) && !TY_is_volatile(WN_ty(wn)) &&
       !((ST_class(WN_st(wn)) ==  CLASS_PREG) && 
		Preg_Is_Dedicated(WN_offset(wn)))) {
       DevWarn("%s %d [%p] is missing use_list\n", 
	       OPERATOR_name(opr), WN_map_id(wn), wn);
    }
  }

  if (opr==OPR_BLOCK)
    for (WN* stmt=WN_first(wn); stmt; stmt=WN_next(stmt))
      Du_Sanity_Check_r(stmt,h_table,pass,fp,fancy,reads,writes);
  else if ((pass == 0) || (opr != OPR_IO))
    /* Allow def-use to nodes under I/O, so allow I/O in phase 0 */
    for (INT i=0; i<WN_kid_count(wn); i++)
      Du_Sanity_Check_r(WN_kid(wn,i),h_table,pass,fp,fancy,reads,writes);
}

static void IV_Loop_Stmt_Check_X(STACK<SYMBOL>& symbols,
                                    STACK<WN*>&    loops,
                                    WN*             wn)
{
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      IV_Loop_Stmt_Check_X(symbols, loops, w);
  }
  else if (opr == OPR_IO)
    ;   // we don't care what's going on down here.
  else if (opr == OPR_DO_LOOP) {
    IV_Loop_Stmt_Check_X(symbols, loops, WN_start(wn));
    symbols.Push(SYMBOL(WN_index(wn)));       // i = i doesn't count
    loops.Push(wn);
    IV_Loop_Stmt_Check_X(symbols, loops, WN_end(wn));
    IV_Loop_Stmt_Check_X(symbols, loops, WN_step(wn));
    IV_Loop_Stmt_Check_X(symbols, loops, WN_do_body(wn));
    void(symbols.Pop());
    void(loops.Pop());
  }
  else {
    for (INT k = 0; k < WN_kid_count(wn); k++)
      IV_Loop_Stmt_Check_X(symbols, loops, WN_kid(wn,k));

    if (opr == OPR_LDID) {
      SYMBOL    ldsym = SYMBOL(wn);
      for (INT i = symbols.Elements()-1; i >= 0; i--) {
        if (symbols.Bottom_nth(i) == ldsym) {
          DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn);
          if (dl == NULL) {
            DevWarn("Missing def_list for induction variable %s",
                    ldsym.Name());
          }
          else if (dl->Loop_stmt() != loops.Bottom_nth(i)) {
	    if (!dl->Incomplete()) {
              DevWarn("Bad loop stmt %p for induction variable %s <fixed>",
                    dl->Loop_stmt(), ldsym.Name());
              dl->Set_loop_stmt(loops.Bottom_nth(i));
            }
          }
          break;
        }
      }
    }
  }
}

static void Initialize_Symbols(STACK<SYMBOL>& symbols,
                               STACK<WN*>&    loops,
                               WN*            wn)
{
  if (wn) {
    Initialize_Symbols(symbols, loops, LWN_Get_Parent(wn));
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      symbols.Push(SYMBOL(WN_index(wn)));
      loops.Push(wn);
    }
  }
}

void IV_Loop_Stmt_Check(WN* wn, MEM_POOL* pool)
{
  STACK<SYMBOL>        symbols(pool);
  STACK<WN*>           loops(pool);
  Initialize_Symbols(symbols, loops, LWN_Get_Parent(wn));
  IV_Loop_Stmt_Check_X(symbols, loops, wn);
}

void Du_Sanity_Check(WN* wn, FILE* fp, UINT fancy)
{
  if (LNO_Verbose)
    fprintf(fp, "Begin Du_Sanity_Check ..\n");

  MEM_POOL_Push(&LNO_local_pool);
  {
    IV_Loop_Stmt_Check(wn, &LNO_local_pool);

    STACK<WN*> reads(&LNO_local_pool);
    STACK<WN*> writes(&LNO_local_pool);

    HASH_TABLE<WN*,INT> h_table(256, &LNO_local_pool);
    Du_Sanity_Check_r(wn,&h_table,0,fp,fancy,&reads,&writes);
    Du_Sanity_Check_r(wn,&h_table,1,fp,fancy,&reads,&writes);
    Du_Sanity_Check_Matching_Du(&reads,&writes,fp,fancy);
  }
  MEM_POOL_Pop(&LNO_local_pool);

  if (LNO_Verbose)
    fprintf(fp, "End Du_Sanity_Check ..\n");
}

void FB_Sanity_Check(WN *wn)
{
  if (Cur_PU_Feedback && LNO_Test_Dump) {
    INT32 freq = 0;
    LWN_ITER *wniter = LWN_WALK_TreeIter(wn);

    while (wniter) {
      WN *cur = wniter->wn;
      wniter = LWN_WALK_TreeNext(wniter);
      freq = WN_MAP32_Get(WN_MAP_FEEDBACK, cur);
      if (freq == 0) {
	DevWarn("? Missing frequency count for wn=%p (opr=%s)",
		wn, OPERATOR_name(WN_operator(wn)));
      }
    }
  }
}

BOOL Wn_Is_Inside(WN* inner, const WN* outer)
{
  for (WN* wn = inner; wn; wn = LWN_Get_Parent(wn)) {
    if (wn == outer)
      return TRUE;
  }

  return FALSE;
}

INT64 LWN_Get_Linenum(const WN *wn)
{
  while (wn) {
    INT64 line = WN_Get_Linenum(wn);
    if (line != 0)
      return line;
    wn = LWN_Get_Parent(wn);
  }

  DevWarn("LWN_Get_Linenum() could not find a reasonable line number");
  return 0;
}

BOOL Is_Permutation_Vector(const INT order[], INT nloops) 
{ 
  INT* save = (INT*) alloca(sizeof(INT)*nloops);
  INT i;
  for (i = 0; i < nloops; i++) 
    save[i] = 0; 
  for (i = 0; i < nloops; i++) { 
    if (order[i] >= nloops || order[i] < 0) 
      return FALSE; 
    if (save[order[i]] != 0) 
      return FALSE; 
    save[order[i]] = 1;  
  } 
  return TRUE; 
}

extern BOOL Are_Permutations(const INT* order1, const INT* order2, INT count)
{
  // is each element in order1 unique

  INT i;
  for (i = 0; i < count; i++) {
    for (INT j = i+1; j < count; j++) {
      if (order1[i] == order1[j])
        return FALSE;
    }
  }

  // okay, now is each element in order1 also in order2
  for (i = 0; i < count; i++) {
    INT j;
    for (j = 0; j < count; j++) {
      if (order1[i] == order2[j])
        break;
    }
    if (j >= count)
      return FALSE;
  }

  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Is_Loop_Invariant_Use
// FUNCTION: Returns TRUE if use 'wn' is invariant in the 'outerloop'.
//   FALSE if it is unknown or not loop invariant.
//-----------------------------------------------------------------------

BOOL Is_Loop_Invariant_Use(WN* wn,
                           WN* outerloop)
{
  switch (WN_operator(wn)) { 
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_ISTORE:
  case OPR_IO:
  case OPR_RETURN:
#ifdef KEY
  case OPR_GOTO_OUTER_BLOCK:
#endif
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
    break; 
  default: 
    FmtAssert(0, ("Is_Loop_Invariant_Use called with improper node type."));
  } 

  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* def = node->Wn();
    if (Wn_Is_Inside(def, outerloop))
      return FALSE;
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Is_Loop_Invariant_Exp
// FUNCTION: Returns TRUE if all uses in the expression 'wn' are invariant 
//   in the 'outerloop'.  FALSE if it at least one of them are either unknown 
//   or not loop invariant.
//-----------------------------------------------------------------------

BOOL Is_Loop_Invariant_Exp(WN* wn, 
			   WN* outerloop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  OPERATOR opr = WN_operator(wn); 
  if (OPCODE_is_call(WN_opcode(wn)) || opr == OPR_ILOAD) {
    if (dg == NULL) 
      return FALSE; 
    VINDEX16 v = dg->Get_Vertex(wn); 
    if (v == 0) 
      return FALSE; 
    EINDEX16 e = 0; 
    for (e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) { 
      VINDEX16 v_source = dg->Get_Source(e);
      WN* wn_source = dg->Get_Wn(v_source);
      if (Wn_Is_Inside(wn_source, outerloop))
	return FALSE; 
    }
    for (INT kid = 0; kid < WN_kid_count(wn); kid++)  
      if (!Is_Loop_Invariant_Exp(WN_kid(wn, kid), outerloop))
	return FALSE; 
    return TRUE; 
  } else if (opr == OPR_INTRINSIC_OP
#ifdef KEY
	     || opr == OPR_PURE_CALL_OP
#endif
            ) { 
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      WN* wn_parm_node = WN_kid(wn, i);
      if (WN_Parm_By_Reference(wn_parm_node))
        return FALSE; 
      WN* wn_parameter = WN_kid0(wn_parm_node); 
      if (!Is_Loop_Invariant_Exp(wn_parameter, outerloop))
        return FALSE; 
    } 
    return TRUE;  
  } else if (opr == OPR_LDID) {
    return Is_Loop_Invariant_Use(wn, outerloop); 
  } else {
    if (!Statically_Safe_Node(wn)) 
      return FALSE; 
    for (INT kid = 0; kid < WN_kid_count(wn); kid++)
      if (!Is_Loop_Invariant_Exp(WN_kid(wn, kid), outerloop))
        return FALSE;
    return TRUE; 
  } 
} 

// Query whether wn represents the address of a constant-indexed array element.
BOOL Is_Const_Array_Addr(WN * wn)
{
  if (WN_operator(wn) == OPR_ARRAY) {
    if (WN_operator(WN_kid0(wn)) != OPR_LDID)
      return FALSE;

    for (int i = 0; i < WN_num_dim(wn); i ++) {
      WN * index = WN_array_index(wn, i);
      if (WN_operator(index) != OPR_INTCONST)
	return FALSE;
    }

    return TRUE;
  }

  return FALSE;
}

// Query whether given wn is a loop invariant indirect load
// wrt all the enclosing loops.
BOOL Is_Loop_Invariant_Indir(WN * wn)
{
  if ((WN_operator(wn) == OPR_ILOAD)
      && Is_Const_Array_Addr(WN_kid0(wn))) {
    WN * lp_wn = wn;
    while (lp_wn) {
      OPCODE opcode = WN_opcode(lp_wn);
      if ((opcode == OPC_DO_LOOP) 
	  || (opcode == OPC_DO_WHILE)
	  || (opcode == OPC_WHILE_DO)) {
	if (!Is_Loop_Invariant_Use(wn, lp_wn))
	  return FALSE;
      }
      lp_wn = LWN_Get_Parent(lp_wn);
    }
    return TRUE;
  }

  return FALSE;
}

typedef HASH_TABLE<WN*,WN*> LOOP_MAPPING;

// Note: this implementation does not trust the "Is_Inner" annotation.
// That's intentional, since some callers may have inconsistent such
// information and this only visits each statement, not each node.

static void SNL_Make_Loop_Mapping_Inside(WN*           orig,
                                         WN*           copy,
                                         LOOP_MAPPING* ht)
{
  Is_True(WN_opcode(orig) == OPC_BLOCK && WN_opcode(copy) == OPC_BLOCK,
          ("bad params to SNL_Make_Loop_Mapping_Inside"));

  WN* wo = WN_first(orig);
  WN* wc = WN_first(copy);

  for (;;) {
    OPCODE opo;
    OPCODE opc;
    while (wo && (opo = WN_opcode(wo)) != OPC_DO_LOOP && opo != OPC_IF)
      wo = WN_next(wo);
    while (wc && (opc = WN_opcode(wc)) != OPC_DO_LOOP && opc != OPC_IF)
      wc = WN_next(wc);
    if (wo == NULL || wc == NULL) {
      FmtAssert(wo == wc, ("Non-identical orig/copy structure"));
      break;
    }
    FmtAssert(opo == opc, ("Bad wo/wc"));
    switch (opo) {
     case OPC_DO_LOOP:
      ht->Enter(wo, wc);
      SNL_Make_Loop_Mapping_Inside(WN_do_body(wo), WN_do_body(wc), ht);
      break;
     case OPC_IF:
      SNL_Make_Loop_Mapping_Inside(WN_then(wo), WN_then(wc), ht);
      SNL_Make_Loop_Mapping_Inside(WN_else(wo), WN_else(wc), ht);
      break;
    }
    wo = WN_next(wo);
    wc = WN_next(wc);
  }
}

LOOP_MAPPING* Make_Loop_Mapping(WN*       orig,
                                WN*       copy,
                                MEM_POOL* pool)
{
  LOOP_MAPPING* ht = CXX_NEW(LOOP_MAPPING(13, pool), pool);

  // loops here in

  ht->Enter(orig, copy);
  SNL_Make_Loop_Mapping_Inside(WN_do_body(orig), WN_do_body(copy), ht);

  return ht;
}

typedef HASH_TABLE<WN*,BOOL> LOOPS_ONLY;

static void Find_Loops_Within_Walk(WN* wn, HASH_TABLE<WN*,BOOL>* ht)
{
  switch (WN_operator(wn)) {
   case OPR_IF:
    Find_Loops_Within_Walk(WN_then(wn), ht);
    Find_Loops_Within_Walk(WN_else(wn), ht);
    break;
   case OPR_DO_LOOP:
    ht->Enter(wn, TRUE);
    Find_Loops_Within_Walk(WN_do_body(wn), ht);
    break;
   case OPR_DO_WHILE:
   case OPR_WHILE_DO:
    Find_Loops_Within_Walk(WN_while_body(wn), ht);
    break;
   case OPR_BLOCK:
    {
      for (WN* w = WN_first(wn); w; w = WN_next(w))
        Find_Loops_Within_Walk(w, ht);
    }
    break;
  }
}

HASH_TABLE<WN*,BOOL>* Find_Loops_Within(WN* orig, MEM_POOL* pool)
{
  LOOPS_ONLY* ht = CXX_NEW(LOOPS_ONLY(13, pool), pool);
  Find_Loops_Within_Walk(orig, ht);
  return ht;
}

// do in postorder so in x[i] = x[i]+3, the read comes first.
void LS_IN_LOOP::Lexorder(WN* wn, ARRAY_DIRECTED_GRAPH16* dg, INT* lex, 
  BOOL use_scalars)
{
  OPCODE      op = WN_opcode(wn);
  OPERATOR    opr = OPCODE_operator(op);
  if (opr == OPR_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      Lexorder(w, dg, lex, use_scalars);
  }
  else {
    for (INT k = 0; k < WN_kid_count(wn); k++)
      Lexorder(WN_kid(wn,k), dg, lex, use_scalars);
  }

  if (OPCODE_is_load(op) 
	&& (opr != OPR_LDID || dg->Get_Vertex(wn) || use_scalars) 
      || OPCODE_is_store(op) 
	&& (opr != OPR_STID || dg->Get_Vertex(wn) || use_scalars) 
      || OPCODE_is_call(op)) {
    ++(*lex);
    _ht.Enter(wn, *lex);
  }
}

LS_IN_LOOP::LS_IN_LOOP(WN* loop, ARRAY_DIRECTED_GRAPH16* dg, MEM_POOL* pool, 
	BOOL use_scalars)
        : Loop(loop), _ht(HT_ELTS, pool),
          Depth(Do_Depth(loop)), Good_Depth(Good_Do_Depth(loop))
{
  INT lexcount = 0;
  Lexorder(loop, dg, &lexcount, use_scalars);
}

//-----------------------------------------------------------------------
// NAME: Num_Common_Loops 
// FUNCTION: Returns the number of loops that the array references 'wn1' 
//   and 'wn2' share in common. 
//-----------------------------------------------------------------------

INT Num_Common_Loops(WN *wn1, WN *wn2)
{
  WN* wn = LNO_Common_Loop(wn1, wn2);
  return Do_Depth(wn)+1;
}

// Find the common loop surrouding wn1 and wn2
// a bound is considered outside of the loop
WN* LNO_Common_Loop(WN *wn1, WN *wn2)
{
  WN* l1[LNO_MAX_DO_LOOP_DEPTH];
  WN* l2[LNO_MAX_DO_LOOP_DEPTH];
  WN *parent;
  INT i1 = 0;
  INT i2 = 0;

  if (WN_opcode(wn1) == OPC_DO_LOOP) {
    l1[i1++] = wn1;
  }
  while ((parent = LWN_Get_Parent(wn1)) != 0) {
    if (WN_opcode(parent) == OPC_DO_LOOP) {
      if (WN_do_body(parent) == wn1) { // not a bound
        l1[i1++] = parent;
      }
    }
    wn1 = parent;
  }

  if (WN_opcode(wn2) == OPC_DO_LOOP) {
    l2[i2++] = wn2;
  }
  while ((parent = LWN_Get_Parent(wn2)) != 0) {
    if (WN_opcode(parent) == OPC_DO_LOOP) {
      if (WN_do_body(parent) == wn2) { // not a bound
        l2[i2++] = parent;
      }
    }
    wn2 = parent;
  }

  WN* answer = NULL;
  while (i1 >= 1 && i2 >= 1 && l1[i1-1] == l2[i2-1]) {
    answer = l1[i1-1];
    i1--, i2--;
  }

  return answer;
} 

//-----------------------------------------------------------------------
// NAME: Equivalent_Access_Arrays 
// FUNCTION: Returns TRUE if the two access vectors are equivalent, with 
//   neither having symbolic terms varying in the common loops of the two 
//   references, FALSE otherwise.    
//-----------------------------------------------------------------------

BOOL Equivalent_Access_Arrays(ACCESS_ARRAY *array1, 
			     ACCESS_ARRAY *array2,                                			     WN *wn1, WN *wn2)
{
  FmtAssert(array1 != NULL && array2 != NULL,
    ("Equivalent_Access_Arrays: NULL access array"));

  if (!(*array1 == *array2)) {
    return FALSE;
  }

  if ((array1->Non_Const_Loops() == 0) && (array2->Non_Const_Loops() == 0)) {
    return (TRUE);  // short cut for the common case
  } else {
    INT common_loops = Num_Common_Loops(wn1,wn2);
    if ((array1->Non_Const_Loops() >= common_loops) ||
        (array2->Non_Const_Loops() >= common_loops)) {
       return FALSE;
    }
  }
  return TRUE;
}

//-----------------------------------------------------------------------
// NAME: Enclosing_Do_Loop 
// FUNCTION: Returns the DO_LOOP enclosing 'wn', NULL if there is no 
//   enclosing loop.  
//-----------------------------------------------------------------------

extern WN* Enclosing_Do_Loop(WN* wn)
{
  for (WN* wn_temp = wn; wn_temp != NULL; wn_temp= LWN_Get_Parent(wn_temp))
    switch (WN_operator(wn_temp)) {
    case OPR_DO_LOOP:
      return wn_temp;
  }
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Enclosing_Loop 
// FUNCTION: Returns the loop enclosing 'wn', NULL if there is no enclosing 
//   loop.  
//-----------------------------------------------------------------------

extern WN* Enclosing_Loop(WN* wn)
{
  for (WN* wn_temp = wn; wn_temp != NULL; wn_temp= LWN_Get_Parent(wn_temp))
    switch (WN_operator(wn_temp)) {
    case OPR_DO_LOOP:
    case OPR_DO_WHILE:
    case OPR_WHILE_DO:
      return wn_temp;
  }
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Enclosing_Loop_Body 
// FUNCTION: Returns the loop whose body encloses 'wn', NULL if there is no 
//   loop body enclosing 'wn'.  
//-----------------------------------------------------------------------

WN* Enclosing_Loop_Body(WN* wn)
{
  BOOL saw_block = FALSE;
  for (WN* wn_temp = wn; wn_temp != NULL; wn_temp= LWN_Get_Parent(wn_temp)) {
    switch (WN_operator(wn_temp)) {
    case OPR_DO_LOOP:
    case OPR_DO_WHILE:
    case OPR_WHILE_DO:
      if (saw_block)
        return wn_temp;
      break;
    case OPR_BLOCK:
      saw_block = TRUE; 
      break;
    }
  } 
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Enclosing_Proper_Do_Loop
// FUNCTION: Returns the closest loop for which 'wn_ref' is in its 
//   WN_do_body(), if there is such a loop, returns NULL otherwise. 
//-----------------------------------------------------------------------

extern WN* Enclosing_Proper_Do_Loop(WN* wn_ref)
{ 
  BOOL found_block = FALSE; 
  for (WN* wn = wn_ref; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) == OPC_BLOCK)
      found_block = TRUE; 
    if (WN_opcode(wn) == OPC_DO_LOOP && found_block) 
      return wn; 
  } 
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: Exp_Depends_On_Outer_Loop 
// FUNCTION: Returns 'TRUE' if the 'wn_exp' depends on an enclosing loop, 
//   FALSE otherwise.  Loads including the symbol 'index_var' are excluded. 
//-----------------------------------------------------------------------

static BOOL Exp_Depends_On_Outer_Loop(WN* wn_exp, SYMBOL index_var, 
  ARRAY_DIRECTED_GRAPH16* dg, DU_MANAGER* du)
{
  if (OPCODE_is_load(WN_opcode(wn_exp)) && SYMBOL(wn_exp) != index_var) {
    if (WN_operator(wn_exp) == OPR_LDID) {
      DEF_LIST *def_list = du->Ud_Get_Def(wn_exp);
      DEF_LIST_ITER iter(def_list);
      DU_NODE* dnode = NULL;
      for (dnode = iter.First(); !iter.Is_Empty(); dnode = iter.Next()) {
        WN* def = dnode->Wn();
        if (Enclosing_Loop(def) != NULL)
          return TRUE;
      }
    } else if (WN_operator(wn_exp) == OPR_ILOAD) {
      EINDEX16 e = 0;
      VINDEX16 v = dg->Get_Vertex(wn_exp);
      for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
        WN* def = dg->Get_Wn(dg->Get_Sink(e));
        if (Enclosing_Loop(def) != NULL)
            return TRUE;
      }
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_exp); i++) {
      if (Exp_Depends_On_Outer_Loop(WN_kid(wn_exp, i), index_var, dg, du))
        return TRUE;
    }
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Loop_Is_Trapezoidal 
// FUNCTION: Returns TRUE if the loop is trapezoidal, FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL Loop_Is_Trapezoidal(WN* wn_loop, 
			 ARRAY_DIRECTED_GRAPH16* dg, 
			 DU_MANAGER* du)
{
  SYMBOL index_var(WN_start(wn_loop));

  if (Exp_Depends_On_Outer_Loop(WN_start(wn_loop), index_var, dg, du))
    return TRUE;
  if (Exp_Depends_On_Outer_Loop(WN_end(wn_loop), index_var, dg, du))
    return TRUE;
  if (Exp_Depends_On_Outer_Loop(WN_step(wn_loop), index_var, dg, du))
    return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Remove_Redundant_Stids 
// FUNCTION: Removes all STIDs in the tree with root 'wn_start' which have
//   no uses, as indicated by the DU_MANAGER '*du'. 
//-----------------------------------------------------------------------

void Remove_Redundant_Stids(WN* wn_start, 
			    DU_MANAGER* du) 
{
  if (WN_operator(wn_start) == OPR_STID) {
    USE_LIST *use_list = du->Du_Get_Use(wn_start);
    if (use_list == NULL) {
      LWN_Delete_Tree(LWN_Extract_From_Block(wn_start));
    } else if (!use_list->Incomplete()) {
      USE_LIST_ITER iter(use_list);
      if (iter.First() == NULL) {
	LWN_Delete_Tree(LWN_Extract_From_Block(wn_start));
      }
    }
    return; 
  }
 
  if (WN_opcode(wn_start) == OPC_BLOCK) { 
    WN* wn_next = NULL; 
    for (WN* wn = WN_first(wn_start); wn != NULL; wn = wn_next) {
      wn_next = WN_next(wn);  
      Remove_Redundant_Stids(wn, du);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_start); i++)  
      Remove_Redundant_Stids(WN_kid(wn_start, i), du); 
  }
} 

//-----------------------------------------------------------------------
// NAME: Matching_Load_Opcode
// FUNCTION: Returns the load opcode which corresponds to the store opcode
//   'store_op'.
//-----------------------------------------------------------------------

extern OPCODE Matching_Load_Opcode(OPCODE store_op)
{
  FmtAssert(OPCODE_is_store(store_op), ("Bad opcode: Matching_Load_Opcode"));
  OPERATOR store_opr = OPCODE_operator(store_op);
  OPERATOR load_opr = OPERATOR_FIRST;
  switch (store_opr) {
  case OPR_STID:
    load_opr = OPR_LDID;
    break;
  case OPR_ISTORE:
    load_opr = OPR_ILOAD;
    break;
  case OPR_ISTOREX:
    load_opr = OPR_ILOADX;
    break;
  case OPR_MSTORE:
    load_opr = OPR_MLOAD;
    break;
  default:
    FmtAssert(0, ("Bad opcode: Matching_Load_Opcode"));
  }
  OPCODE load_op = OPCODE_make_op(load_opr, Promote_Type(OPCODE_desc(store_op)),    OPCODE_desc(store_op));
  FmtAssert(OPCODE_is_load(load_op), ("Bad opcode: Matching_Load_Opcode"));
  return load_op;
}

//-----------------------------------------------------------------------
// NAME: Create_ILoad_From_IStore
// FUNCTION: Return an array load patterned after the array store 'wn_store'.
//   If 'du' is non-null, copy the DU information from 'wn_store' to the
//   newly created array node.
//-----------------------------------------------------------------------

extern WN* Create_ILoad_From_IStore(WN* wn_store,
                                    DU_MANAGER* du, 
				    ARRAY_DIRECTED_GRAPH16* dg)
{
  WN* wn_array = LWN_Copy_Tree(WN_kid1(wn_store));
  if (du != NULL)
    LWN_Copy_Def_Use(WN_kid1(wn_store), wn_array, du);
  dg->Add_Deps_To_Copy_Block(WN_kid1(wn_store), wn_array); 
  OPCODE loadop = Matching_Load_Opcode(WN_opcode(wn_store));
  WN* wn_load = LWN_CreateIload(loadop, WN_offset(wn_store),
    TY_pointed(WN_ty(wn_store)), WN_ty(wn_store), wn_array);
  Duplicate_alias_info(Alias_Mgr, wn_store, wn_load);
  return wn_load;
}  

BOOL Is_Local_Array_Reference(WN* array)
{
  if (WN_operator(array) != OPR_ARRAY)
    return FALSE;

  WN* base = WN_array_base(array);
  OPERATOR base_oper = WN_operator(base);
  if ((base_oper == OPR_LDID) || (base_oper == OPR_LDA)) {
    ST *st = WN_st(base);
#ifdef _NEW_SYMTAB
    if (ST_level(st) == CURRENT_SYMTAB &&
	ST_base_idx(st) == ST_st_idx(st))
#else
    if (ST_symtab_id(st) == SYMTAB_id(Current_Symtab) &&
	 ST_sclass(st) != SCLASS_BASED)
#endif
      return TRUE;
  }
  return FALSE;
}

// Find containing store statement for 'wn'.
WN * Find_Containing_Store(WN * wn)
{
  while (wn) {
    OPERATOR opr = WN_operator(wn);
    if (OPERATOR_is_store(opr))
      return wn;
    wn = LWN_Get_Parent(wn);
  }
  return NULL;
}

// Query whether a global symbol can be treated as a local one,
// i.e., uses of the global variable are not upward-exposed in
// any funtion.  IPA analysis sets this bit.
BOOL Is_Global_As_Local(ST * st)
{
  if (st && ST_is_global_as_local(st))
    return TRUE;
  return FALSE;
}

#ifdef Is_True_On
void LNO_Check_Graph(ARRAY_DIRECTED_GRAPH16* dg)
{
  dg->Check_Graph();

  for (EINDEX16 e = dg->Get_Edge(); e; e = dg->Get_Next_Edge(e)) {
    VINDEX16    v1 = dg->Get_Source(e);
    VINDEX16    v2 = dg->Get_Sink(e);
    FmtAssert(v1 != 0 && v2 != 0, ("missing source or sink for edge=%d", e));
    WN*         wn1 = dg->Get_Wn(v1);
    WN*         wn2 = dg->Get_Wn(v2);
    FmtAssert(wn1 && wn2, ("missing Get_Wn() mapping"));
    OPCODE      o1 = WN_opcode(wn1);
    OPCODE      o2 = WN_opcode(wn2);
    WN*         common = LNO_Common_Loop(wn1, wn2);
    INT         dgood = Good_Do_Depth(common);
    DEPV_ARRAY* dv_array = dg->Depv_Array(e);

    FmtAssert(OPCODE_is_load(o1) || OPCODE_is_store(o1) || OPCODE_is_call(o1),
              ("Bad opcode for vertex"));
    FmtAssert(OPCODE_is_load(o2) || OPCODE_is_store(o2) || OPCODE_is_call(o2),
              ("Bad opcode for vertex"));
    FmtAssert(dgood + 1 == dv_array->Num_Dim(),
              ("LNO dep graph check fails: e=%d good=%d components=%d",
               e, dgood, dv_array->Num_Dim()));
  }
}

extern void MP_Sanity_Check_Func(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    for (WN* tmp = WN_first(wn); tmp != NULL; tmp = WN_next(tmp)) {
      MP_Sanity_Check_Func(tmp);
    }
  } else {
    if ((opcode == OPC_DO_LOOP) && Do_Loop_Is_Mp(wn)) {
      FmtAssert(WN_opcode(LWN_Get_Parent(LWN_Get_Parent(wn))) == OPC_REGION,
	("MP Do loop with a non-region grandparent %p",wn));
    }
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      MP_Sanity_Check_Func(WN_kid(wn,i));
    }
  }
}

#endif /* Is_True_On */

extern BOOL Is_Mp_Region(WN *wn)
{
  if (WN_opcode(wn) == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    FmtAssert(rid != NULL, ("Is_Mp_Region(): Missing rid")); 
    if (RID_TYPE_mp(rid)) return TRUE;
  }
  return FALSE;
}

#ifdef KEY
extern BOOL Is_Eh_Or_Try_Region(WN *wn)
{
  if (WN_opcode(wn) == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    FmtAssert(rid != NULL, ("Is_Eh_Or_Try_Region(): Missing rid")); 
    if (RID_TYPE_eh(rid) || RID_TYPE_try(rid)) return TRUE;
  }
  return FALSE;
}
#endif

extern BOOL Do_Loop_Is_Mp(WN *wn)
{
  if (LWN_Get_Parent(wn) == NULL)
    return FALSE;
  WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn));
#ifdef KEY
  if (PU_cxx_lang(Get_Current_PU()) && Is_Eh_Or_Try_Region(wn_region))
    wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_region));
#endif
  if (!Is_Mp_Region(wn_region))
    return FALSE; 
  WN* wn_pragma = WN_first(WN_region_pragmas(wn_region));  
  if (wn_pragma == NULL)
    return FALSE;
  if (WN_opcode(wn_pragma) == OPC_PRAGMA 
    && (WN_pragma(wn_pragma) == WN_PRAGMA_DOACROSS
        || WN_pragma(wn_pragma) == WN_PRAGMA_PDO_BEGIN
        || WN_pragma(wn_pragma) == WN_PRAGMA_PARALLEL_DO))
    return TRUE; 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
  if (dli != NULL && dli->Mp_Info != NULL) 
    return TRUE; 
  return FALSE; 
}

extern RID * Get_Enclosing_Region_ID(WN *wn)
{
  Is_True(wn!=NULL,("Get_Enclosing_Region_ID: Null wn pointer"));
  WN *pwn = LWN_Get_Parent(wn);
  while (pwn && WN_operator(pwn) != OPR_REGION &&
         WN_operator(pwn) != OPR_FUNC_ENTRY)
    pwn = LWN_Get_Parent(pwn);
  return REGION_get_rid(pwn);
}

extern BOOL Is_Nested_Doacross(WN* wn_loop)
{ 
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
  return dli->Mp_Info != NULL && dli->Mp_Info->Nest_Total() > 1; 
} 

// Go through the code agin to set the depth field as this has been changed
// by dismantling
extern void Remark_Depth(WN *wn, mUINT8 depth)
{
  WN *kid;
  DO_LOOP_INFO *dli;

  Is_True(wn,("Null wn in Remark_Depth"));

  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      Remark_Depth(kid,depth);
      kid = WN_next(kid);
    }
    return;
  } 

  if (opcode == OPC_DO_LOOP) {
    dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    Is_True(dli,("no mapping in Remark_Depth"));
    dli->Depth = depth;
    depth++;
  } 

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    Remark_Depth(kid,depth);
  }

}

WN* UBexp(WN* end, BOOL* ne)
{
  OPERATOR opr = WN_operator(end);

  switch (opr) {
   case OPR_GE:
    if (ne) *ne = FALSE;
    return WN_kid0(end);
   case OPR_GT:
    if (ne) *ne = TRUE;
    return WN_kid0(end);
   case OPR_LE:
    if (ne) *ne = FALSE;
    return WN_kid1(end);
   case OPR_LT:
    if (ne) *ne = TRUE;
    return WN_kid1(end);
   default: 
    return NULL; 
  }
}

WN* UBvar(WN* end)
{
  WN* wn_index = NULL; 
  OPERATOR opr = WN_operator(end);
  switch (opr) {
   case OPR_LE:
   case OPR_LT:
    wn_index = WN_kid0(end);
    break;
   case OPR_GE:
   case OPR_GT:
    wn_index = WN_kid1(end);
    break;
   default: 
    return NULL; 
  }
  if (WN_operator(wn_index) != OPR_LDID) 
    return NULL; 
  WN *wn;
  for (wn = end; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      break; 
  if (wn == NULL) 
    return NULL;  
  if (SYMBOL(WN_index(wn)) != SYMBOL(wn_index))
    return NULL;  
  return wn_index; 
}

INT Loop_Depth(WN* stat)
{
  for (WN* wn = stat; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return Do_Loop_Depth(wn);
  return -1;
}

INT Block_Loop_Depth(WN* stat)
{
  BOOL found_block = FALSE; 
  for (WN* wn = stat; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) == OPC_BLOCK)
      found_block = TRUE; 
    if (found_block && WN_opcode(wn) == OPC_DO_LOOP)
      return Do_Loop_Depth(wn);
  }
  return -1;
}

/***********************************************************************
 *
 * Find the closest MP region enclosing wn (if any).
 * If one is found, add a pragma of the specified type
 * to its pragma list with the given ST/offset.
 *
 ***********************************************************************/
void Add_Pragma_To_MP_Region (const WN* cwn,
                              ST* st,
                              WN_OFFSET offset,
                              WN_PRAGMA_ID pragma_id,
                              BOOL make_compiler_generated) {
  Is_True (pragma_id == WN_PRAGMA_LOCAL ||
           pragma_id == WN_PRAGMA_SHARED ||
           pragma_id == WN_PRAGMA_LASTLOCAL ||
           pragma_id == WN_PRAGMA_FIRSTPRIVATE ||
           pragma_id == WN_PRAGMA_LASTTHREAD, 
           ("Add_Pragma_To_MP_Region: Unknown pragma_id"));
  /* It's too cumbersome to handle the const case, so just force a cast here */
  WN* wn = (WN*) cwn;
  while (wn) {
    if (Is_Mp_Region(wn)) break;
    wn = LWN_Get_Parent (wn);
  }
  if (wn == NULL) return;   /* no MP region */
#ifdef Is_True_On
  {
    /* Sanity check that pragmas continue to be consistent */
    BOOL new_is_local = (pragma_id == WN_PRAGMA_LOCAL ||
                         pragma_id == WN_PRAGMA_FIRSTPRIVATE ||
                         pragma_id == WN_PRAGMA_LASTLOCAL);
    BOOL new_is_shared = (pragma_id == WN_PRAGMA_SHARED);
    if (new_is_local || new_is_shared) {
      WN* orig_wn = WN_first(WN_region_pragmas(wn));
      while (orig_wn) {
        Is_True (WN_operator(orig_wn) == OPR_PRAGMA ||
                 WN_operator(orig_wn) == OPR_XPRAGMA,
                 ("Node in MP-region pragma list not pragma or xpragma"));

        WN_PRAGMA_ID orig_pragma_id = (WN_PRAGMA_ID) WN_pragma(orig_wn);
        BOOL orig_is_local = (orig_pragma_id == WN_PRAGMA_LOCAL ||
                              orig_pragma_id == WN_PRAGMA_FIRSTPRIVATE ||
                              orig_pragma_id == WN_PRAGMA_LASTLOCAL);
        BOOL orig_is_shared = (orig_pragma_id == WN_PRAGMA_SHARED);
        if ((orig_is_local || orig_is_shared) &&
            (WN_st(orig_wn) == st) && (WN_pragma_arg1(orig_wn) == offset)) {
          /* this pragma is for the same ST */

          Is_True ((new_is_local && orig_is_local) ||
                   (new_is_shared && orig_is_shared),
                   ("Add_Pragma_To_MP_Region: adding %s (%s,%s), already in %s list\n",
                    WN_pragmas[pragma_id].name,
                    ST_name(st),
                    (ST_class(st) == CLASS_PREG ? Preg_Name(offset) : ""),
                    WN_pragmas[orig_pragma_id].name));
        }
        orig_wn = WN_next(orig_wn);
      }
    }
  }
#endif /* Is_True_On */

  if (pragma_id == WN_PRAGMA_LOCAL) {
    // use the fancy algorithm

    //
    // Build a vector of enclosing parallel regions
    //
    WN_VECTOR wnv(Malloc_Mem_Pool);
 
    WN *tmp = wn;
    while (tmp) {
      if (WN_opcode(tmp) == OPC_REGION &&
          RID_TYPE_mp(REGION_get_rid(tmp))) {
        wnv.push_back (tmp);
      }
      tmp = LWN_Get_Parent(tmp);
    }
  
    Add_Pragma_To_MP_Regions (&wnv, WN_PRAGMA_LOCAL,
                              st, offset, Parent_Map,
                              make_compiler_generated);
    return;
  }


  // Else just add to the closest enclosing region
  // avoid duplicates
  {
    WN* curr_wn = WN_first(WN_region_pragmas(wn));
    while (curr_wn) {
      WN_PRAGMA_ID curr_pragma_id = (WN_PRAGMA_ID) WN_pragma(curr_wn);
      if (WN_operator(curr_wn) == OPR_PRAGMA &&
          curr_pragma_id == pragma_id &&
          WN_st(curr_wn) == st &&
          WN_pragma_arg1(curr_wn) == offset) {
        // the pragma already exists
        if (make_compiler_generated) {
          WN_set_pragma_compiler_generated(curr_wn); 
        } 
        return;
      }
      curr_wn = WN_next(curr_wn);
    }
  }

  WN* pwn = WN_CreatePragma (pragma_id, st, offset, 0);
  WN_Set_Linenum(pwn, WN_Get_Linenum(wn));
  LWN_Insert_Block_Before (WN_region_pragmas(wn), NULL, pwn);
  if (make_compiler_generated) {
    WN_set_pragma_compiler_generated(pwn); 
  } 
} /* Add_Pragma_To_MP_Region */

extern void Update_MP_Local_Var(ST* st, WN_OFFSET offset, WN *wn)
{

  //
  // Build a vector of enclosing parallel regions
  //
  WN_VECTOR wnv(Malloc_Mem_Pool);
 
  WN *tmp = wn;
  while (tmp) {
    if (WN_opcode(tmp) == OPC_REGION &&
        RID_TYPE_mp(REGION_get_rid(tmp))) {
      wnv.push_back (tmp);
    }
    tmp = LWN_Get_Parent(tmp);
  }
  
  Add_Pragma_To_MP_Regions (&wnv, WN_PRAGMA_LOCAL,
                            st, offset, Parent_Map,
                            FALSE);
}

//-----------------------------------------------------------------------
// NAME: Unique_Stid_Definition
// FUNCTION: Returns the unique definition (which must be a STID) of the 
//  LDID 'wn_ldid'.  Returns NULL if there is no unique defintion which 
//  is an STID. 
//-----------------------------------------------------------------------

static WN* Unique_Stid_Definition(WN* wn_ldid, 
				  DU_MANAGER* du)
{
  FmtAssert(WN_operator(wn_ldid) == OPR_LDID, 
    ("Unique_Stid_Definition() should be called with LDID")); 
  DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid);
  DEF_LIST_ITER iter(def_list); 
  if (def_list == NULL || def_list->Incomplete())
    return NULL;
  WN* unique_def = NULL; 
  DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* def = node->Wn();
    if (unique_def == NULL) 
      unique_def = def; 
    else                     
      return NULL; 
  }
  return WN_operator(unique_def) == OPR_STID   
    ? unique_def : NULL;  
} 

//-----------------------------------------------------------------------
// NAME: Symbol_In_Expression
// FUNCTION: Returns TRUE if the expression 'wn_exp' contains an LDID with 
//   the symbol 'sym', FALSE otherwise. 
// NOTE: LDIDs with unique defintions which are STIDs are effectively 
//   forward substituted for the uses of this function. 
//-----------------------------------------------------------------------

static BOOL Symbol_In_Expression(WN* wn_exp, 
				 SYMBOL sym, 
				 DU_MANAGER* du) 
{
  if (WN_operator(wn_exp) == OPR_LDID) {
    if (SYMBOL(wn_exp) == sym) 
       return TRUE; 
    WN* wn_stid = Unique_Stid_Definition(wn_exp, du); 
    if (wn_stid != NULL) 
      return Symbol_In_Expression(WN_kid0(wn_stid), sym, du); 
  }
  for (INT i = 0; i < WN_kid_count(wn_exp); i++) 
    if (Symbol_In_Expression(WN_kid(wn_exp, i), sym, du))
       return TRUE; 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: Unique_Ldid_Symbol 
// FUNCTION: Returns a WN* to the unique LDID in the 'wn_exp' if there is 
//   one, returns FALSE otherwise.   
// NOTE: LDIDs with unique defintions which are STIDs are effectively 
//   forward substituted for the uses of this function. 
//-----------------------------------------------------------------------

static WN* Unique_Ldid_Symbol(WN* wn_exp, 
			      DU_MANAGER* du) 
{
  if (WN_operator(wn_exp) == OPR_LDID) {
    WN* wn_stid = Unique_Stid_Definition(wn_exp, du); 
    FmtAssert(!Wn_Is_Inside(wn_exp, wn_stid), ("Exp inside def")); 
    if (wn_stid != NULL) 
      return Unique_Ldid_Symbol(WN_kid0(wn_stid), du); 
    return wn_exp; 
  } 
  WN* wn_unique = NULL; 
  for (INT i = 0; i < WN_kid_count(wn_exp); i++) {
    WN* wn = Unique_Ldid_Symbol(WN_kid(wn_exp, i), du);
    if (wn_unique == NULL) 
      wn_unique = wn; 
    else if (wn != NULL) 
      return NULL; 
  }
  return wn_unique; 
}

//-----------------------------------------------------------------------
// NAME: Wind_Down_Parent 
// FUNCTION: Returns a WN* to the loop from which 'wn_loop' was peeled 
//   as a wind-down loop.  If this is not a wind-down loop, or we can't 
//   tell, return NULL.  
//-----------------------------------------------------------------------

static WN* Wind_Down_Parent(WN* wn_loop, 
			    DU_MANAGER* du)
{ 
  WN* wn_ldid = WN_kid0(WN_start(wn_loop)); 
  if (WN_operator(wn_ldid) != OPR_LDID)
    return NULL; 
  DEF_LIST *def_list = du->Ud_Get_Def(wn_ldid);
  DEF_LIST_ITER iter(def_list);
  if (def_list->Incomplete())
    return NULL;
  INT def_count = 0; 
  DU_NODE* node = NULL;
  WN* wn_loop_parent = NULL; 
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* def = node->Wn();
    if (++def_count > 2) 
      return NULL; 
    if (LWN_Get_Parent(def) == NULL)
      return NULL;  
    if (WN_opcode(LWN_Get_Parent(def)) != OPC_DO_LOOP) 
      return NULL; 
    if (Do_Loop_Depth(LWN_Get_Parent(def)) != Do_Loop_Depth(wn_loop))
      return NULL; 
    if (wn_loop_parent == NULL)
      wn_loop_parent = LWN_Get_Parent(def);
    else if (wn_loop_parent != LWN_Get_Parent(def))
      return NULL;  
  }
  return wn_loop_parent; 
}   

//-----------------------------------------------------------------------
// NAME: Normal_Outer_Tile
// FUNCTION: Returns the outer tile loop corresponding to the inner tile 
//   loop 'wn_loop'.  Returns NULL if this is not an inner tile loop or 
//   if we can't determine what the outer tile loop is.     
// NOTE: In this function, we define "outer tile" and "inner tile" loops 
//       as a pair of loops of the form:  
//             do ii = LBii, UBii, Sii 
//               do i = f1(ii), f2(ii), Si 
//-----------------------------------------------------------------------

static WN* Normal_Outer_Tile(WN* wn_loop, 
		             DU_MANAGER* du) 
{
  WN* wn_original = Wind_Down_Parent(wn_loop, du); 
  if (wn_original != NULL)
    return Outer_Tile(wn_original, du);  
  WN* wn_ldid = Unique_Ldid_Symbol(WN_start(wn_loop), du);
  if (wn_ldid == NULL) 
    return NULL; 
  if (!Symbol_In_Expression(WN_end(wn_loop), SYMBOL(wn_ldid), du))
    return NULL;  
  for (WN* wn = LWN_Get_Parent(wn_loop); wn != NULL; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP && SYMBOL(WN_index(wn)) == SYMBOL(wn_ldid))
      return wn;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Outer_Tile
// FUNCTION: Returns the outer tile loop corresponding to the inner tile 
//   loop 'wn_loop'.  Returns NULL if this is not an inner tile loop or 
//   if we can't determine what the outer tile loop is.     
// NOTES: Lego tiling and mp tiling done by the compiler are considered 
//   as well as user and compiler tiling as defined by Normal_Outer_Tile()
//-----------------------------------------------------------------------

extern WN* Outer_Tile(WN* wn_loop, 
		      DU_MANAGER* du) 
{
  WN* wn_normal_loop = Normal_Outer_Tile(wn_loop, du); 
  DO_LOOP_INFO *dli_loop = Get_Do_Loop_Info(wn_loop); 
  if (dli_loop->Lego_Mp_Key_Lower == 0)
    return wn_normal_loop;
  if (dli_loop->Lego_Mp_Key_Lower < dli_loop->Lego_Mp_Key_Upper)
    return wn_normal_loop;  
  WN *wn;
  for (wn = LWN_Get_Parent(wn_loop); wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_opcode(wn) != OPC_DO_LOOP)
      continue; 
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn); 
    if (dli->Lego_Mp_Key_Lower == 0)
      continue; 
    if (dli_loop->Lego_Mp_Key_Lower < dli->Lego_Mp_Key_Lower
      || dli_loop->Lego_Mp_Key_Lower > dli->Lego_Mp_Key_Upper)
      continue; 
    break;
  }
  if (wn == NULL) 
    return wn_normal_loop; 
  return Do_Depth(wn) < Do_Depth(wn_normal_loop) 
    ? wn_normal_loop : wn; 
}

//-----------------------------------------------------------------------
// NAME: Return_Node
// FUNCTION: Returns one of the function "wn_func_nd"'s return nodes.
//-----------------------------------------------------------------------

extern WN* Return_Node(WN* wn_func_nd)
{
  WN* wn_first = WN_first(WN_func_body(wn_func_nd));
  WN* wn_return = NULL;
  WN* wnn = NULL; 
  for (WN* wn = wn_first; wn != NULL; wnn = wn, wn = WN_next(wn))
    if (WN_opcode(wn) == OPC_RETURN)
      wn_return = wn;
  if (wn_return == NULL) {
    wn_return = LWN_CreateReturn(); 
    LWN_Insert_Block_After(LWN_Get_Parent(wnn), wnn, wn_return);  
  }
  return wn_return;
} 

//-----------------------------------------------------------------------
// NAME: Loop_Step
// FUNCTION: Returns a WN* to the increment of the loop 'wn_loop'
//-----------------------------------------------------------------------

extern WN* Loop_Step(WN* wn_loop)
{
  FmtAssert(WN_opcode(wn_loop) == OPC_DO_LOOP,
    ("Must be a do loop to find the step"));
  WN* wn_add = WN_kid0(WN_step(wn_loop));
  FmtAssert(WN_operator(wn_add) == OPR_ADD,
    ("Step must be incremented with OPR_ADD node."));
  return (WN_operator(WN_kid0(wn_add)) == OPR_LDID
    && SYMBOL(WN_kid0(wn_add)) == SYMBOL(WN_step(wn_loop)))
    ? WN_kid1(wn_add) : WN_kid0(wn_add);
}
 
//-----------------------------------------------------------------------
// NAME: Common_Loop_Ancestor 
// FUNCTION: Returns the loop with the greatest depth which is a common
//   ancestor of both 'wn1' and 'wn2'.  Returns NULL if there is no loop 
//   which is a common ancestor of both.
//-----------------------------------------------------------------------

extern WN* Common_Loop_Ancestor(WN* wn1, WN* wn2)
{
  DOLOOP_STACK stack1(&LNO_local_pool);
  DOLOOP_STACK stack2(&LNO_local_pool);
  Build_Doloop_Stack(wn1, &stack1);
  Build_Doloop_Stack(wn2, &stack2);
  if (stack1.Elements() == 0 || stack2.Elements() == 0)
    return NULL;
  WN* wn_deepest = NULL; 
  for (INT i = 0; i < stack1.Elements(); i++) {
    if (i == stack2.Elements())  
      return wn_deepest;
    WN* wn1 = stack1.Bottom_nth(i);
    WN* wn2 = stack2.Bottom_nth(i);
    FmtAssert(Do_Depth(wn1) == i && Do_Depth(wn2) == i, 
      ("Build_Loop_Stack() returned unexpected do depths"));
    if (wn1 != wn2) 
      return wn_deepest;
    wn_deepest = wn1;
  }
  return wn_deepest;
}

//-----------------------------------------------------------------------
// NAME: Build_Parent_Stack 
// FUNCTION: Push all of the ancestors of 'wn' (including 'wn' itself) onto
//   the 'stack'. 
//-----------------------------------------------------------------------

extern void Build_Parent_Stack(WN* wn, STACK<WN*>* stack)
{
  if (!wn) 
    return; 
  Build_Parent_Stack(LWN_Get_Parent(wn), stack);
  stack->Push(wn);
}

//-----------------------------------------------------------------------
// NAME: Common_Ancestor 
// FUNCTION: Returns the node furthest away from the FUNC_ENTRY node which  
//   is a common ancestor of both 'wn1' and 'wn2'.  Returns NULL if there 
//   is no loop which is a common ancestor of both.
//-----------------------------------------------------------------------

extern WN* Common_Ancestor(WN* wn1, WN* wn2)
{
  STACK<WN*> stack1(&LNO_local_pool);
  STACK<WN*> stack2(&LNO_local_pool);
  Build_Parent_Stack(wn1, &stack1);
  Build_Parent_Stack(wn2, &stack2);
  if (stack1.Elements() == 0 || stack2.Elements() == 0)
    return NULL;
  WN* wn_deepest = NULL; 
  for (INT i = 0; i < stack1.Elements(); i++) {
    if (i == stack2.Elements())  
      return wn_deepest;
    WN* wn1 = stack1.Bottom_nth(i);
    WN* wn2 = stack2.Bottom_nth(i);
    if (wn1 != wn2) 
      return wn_deepest;
    wn_deepest = wn1;
  }
  return wn_deepest;
}

//-----------------------------------------------------------------------
// NAME: Is_Lex_Before 
// FUNCTION: Returns TRUE if 'wn1' is lexically before 'wn2', FALSE
//   otherwise. 
//-----------------------------------------------------------------------

extern BOOL Is_Lex_Before(WN* wn1, WN* wn2)
{
  WN* wn_common = Common_Ancestor(wn1, wn2); 
  if (wn_common == wn1) 
    return FALSE; 
  if (wn_common == wn2)
    return TRUE; 
  WN* wn1_before = NULL; 
  for (WN* wnn1 = wn1; wnn1 != wn_common; wnn1 = LWN_Get_Parent(wnn1)) 
    wn1_before = wnn1;  
  WN* wn2_before = NULL; 
  for (WN* wnn2 = wn2; wnn2 != wn_common; wnn2 = LWN_Get_Parent(wnn2)) 
    wn2_before = wnn2;  
  if (WN_opcode(wn_common) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_common); wn != NULL; wn = WN_next(wn)) { 
      if (wn == wn1_before)
	return TRUE; 
      if (wn == wn2_before)
	return FALSE; 
    } 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_common); i++) {
      if (WN_kid(wn_common, i) == wn1_before)
	return TRUE; 
      if (WN_kid(wn_common, i) == wn2_before)
	return FALSE; 
    } 
  } 
  FmtAssert(FALSE, ("Is_Lex_Before: Should have found answer by now")); 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Find_Node
// FUNCTION: Find the first node the the tree rooted at 'wn_tree' with
//   symbol 'sym' and return it.
//-----------------------------------------------------------------------

extern WN* Find_Node(SYMBOL sym,
                     WN* wn_tree)
{
  if (OPCODE_has_sym(WN_opcode(wn_tree)) && SYMBOL(wn_tree) == sym)
    return wn_tree;
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) {
      WN* wn_result = Find_Node(sym, wn);
      if (wn_result != NULL)
        return wn_result;
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) {
      WN* wn_result = Find_Node(sym, WN_kid(wn_tree, i));
      if (wn_result != NULL)
        return wn_result;
    }
  }
  return NULL;
}
 
//-----------------------------------------------------------------------
// NAME: Identity_Permutation
// FUNCTION: Returns TRUE if the 'permutation' of length 'nloops' is the
//   identity permutation, FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL Identity_Permutation(INT permutation[],
                                 INT nloops)
{
  for (INT i = 0; i < nloops; i++)
    if (permutation[i] != i)
      return FALSE;
  return TRUE;
}

extern WN* Trip_Count(WN* wn_loop)
{
  DU_MANAGER* du = Du_Mgr;
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 

  Upper_Bound_Standardize(WN_end(wn_loop), FALSE);
  TYPE_ID index_type = Promote_Type(Do_Wtype((WN *) wn_loop)); 
  WN* wn_kid0 = WN_kid0(WN_end(wn_loop));
  BOOL was_kid0 = WN_operator(wn_kid0) == OPR_LDID
    && SYMBOL(wn_kid0) == SYMBOL(WN_index(wn_loop));

  WN* wn_trip = NULL;
  WN* wn_init = WN_kid0(WN_start(wn_loop));
  WN* wn_limit = was_kid0 ? WN_kid1(WN_end(wn_loop))
    : WN_kid0(WN_end(wn_loop));
  WN* wn_stride = Loop_Step(wn_loop);

  WN* wn_cp_init = LWN_Copy_Tree(wn_init);
  LWN_Copy_Def_Use(wn_init, wn_cp_init, du);
  dg->Add_Deps_To_Copy_Block(wn_init, wn_cp_init, FALSE); 

  WN* wn_cp_limit = LWN_Copy_Tree(wn_limit);
  LWN_Copy_Def_Use(wn_limit, wn_cp_limit, du);
  dg->Add_Deps_To_Copy_Block(wn_limit, wn_cp_limit, FALSE); 

  WN* wn_cp_stride = LWN_Copy_Tree(wn_stride);
  LWN_Copy_Def_Use(wn_stride, wn_cp_stride, du);
  dg->Add_Deps_To_Copy_Block(wn_stride, wn_cp_stride, FALSE); 

  WN* wn_cp_stride2 = LWN_Copy_Tree(wn_stride);
  LWN_Copy_Def_Use(wn_stride, wn_cp_stride2, du);
  dg->Add_Deps_To_Copy_Block(wn_stride, wn_cp_stride2, FALSE); 

  BOOL unity_stride 
    = WN_operator(wn_cp_stride2) == OPR_INTCONST 
    && (WN_const_val(wn_cp_stride2) == 1 || WN_const_val(wn_cp_stride2) == -1); 

  if (((WN_operator(WN_end(wn_loop)) == OPR_LT) && was_kid0) ||
      ((WN_operator(WN_end(wn_loop)) == OPR_GT) && !was_kid0)) {
    WN* wn_sub = AWN_Sub(index_type, wn_cp_limit, wn_cp_init);
    WN* wn_add = AWN_Add(index_type, wn_sub, wn_cp_stride);
    WN* wn_sub2 = AWN_Sub(index_type, wn_add, LWN_Make_Icon(index_type, 1));
    wn_trip = unity_stride 
     ? AWN_Mpy(index_type, wn_sub2, wn_cp_stride2) 
     : AWN_Div(index_type, wn_sub2, wn_cp_stride2);
  } else if (((WN_operator(WN_end(wn_loop)) == OPR_GT)
      && was_kid0) || ((WN_operator(WN_end(wn_loop)) == OPR_LT)
      && !was_kid0)) {
    WN* wn_sub = AWN_Sub(index_type, wn_cp_limit, wn_cp_init);
    WN* wn_add = AWN_Add(index_type, wn_sub, wn_cp_stride);
    WN* wn_add2 = AWN_Add(index_type, wn_add, LWN_Make_Icon(index_type, 1));
    wn_trip = unity_stride 
     ? AWN_Mpy(index_type, wn_add2, wn_cp_stride2) 
     : AWN_Div(index_type, wn_add2, wn_cp_stride2);
  } else {
    WN* wn_sub = AWN_Sub(index_type, wn_cp_limit, wn_cp_init);
    WN* wn_add = AWN_Add(index_type, wn_sub, wn_cp_stride);
    wn_trip = unity_stride 
     ? AWN_Mpy(index_type, wn_add, wn_cp_stride2) 
     : AWN_Div(index_type, wn_add, wn_cp_stride2);
  }
  return wn_trip;
}

//-----------------------------------------------------------------------
// NAME: Reduce_Permutation
// FUNCTION: Let M be the largest integer for which the first M components
//   of the 'permutation' of length 'nloops' is the identity permutation.
//   Let N = MIN(M, 'reduction_max').  Return nloops - N and set 'spermuta-
//   tion' to the reduced permutation consisting of the last 'nloops' - N
//   components of 'permutation', adjusted so that they are a permutation.
// EXAMPLE: Let max_reduction == 2, permutation == [0 1 2 3 6 5 4], and
//   nloops == 7.  Then N == 2, spermutation == [0 1 4 3 2], and return 5.
//-----------------------------------------------------------------------

extern INT Reduce_Permutation(INT permutation[],
                              INT nloops,
                              INT spermutation[],
                              INT max_reduction)
{
  INT i;
  for (i = 0; i < max_reduction; i++)
    if (permutation[i] != i)
      break;
  for (INT j = i; j < nloops; j++)
    spermutation[j - i] = permutation[j] - i;
  return nloops - i;
}
 
//-----------------------------------------------------------------------
// NAME: Index_Variable
// FUNCTION: Return TRUE if 'wn_index' is an STID or LDID of an index
//   variable for some loop enclosing it.  Returns FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL Index_Variable(WN* wn_index)
{
  OPERATOR opr = WN_operator(wn_index);
  if (opr != OPR_LDID && opr != OPR_STID)
    return FALSE;
  SYMBOL sym_index(wn_index);
  for (WN* wn = wn_index; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP && SYMBOL(WN_index(wn)) == sym_index)
       return TRUE;
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Contains_Dedicated_Preg
// FUNCTION: Returns TRUE when 'wn_tree' contains a reference to a dedicated
//   preg, FALSE otherwise.
//-----------------------------------------------------------------------

extern BOOL Contains_Dedicated_Preg(WN* wn_tree)
{
  if (OPCODE_has_sym(WN_opcode(wn_tree)) && WN_st(wn_tree) != NULL
    && ST_class(WN_st(wn_tree)) == CLASS_PREG
    && WN_offset(wn_tree) <= Last_Dedicated_Preg_Offset)
    return TRUE;

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Contains_Dedicated_Preg(wn))
        return TRUE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (Contains_Dedicated_Preg(WN_kid(wn_tree, i)))
        return TRUE;
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: Factorial
// FUNCTION: Returns the factorial of 'n'. The value 'n' must be a non-
//   negative integer.
//-----------------------------------------------------------------------

extern INT Factorial(INT n)
{
  FmtAssert(n >= 0, ("Factorial() takes a non-negative argument"));
  INT factorial = 1;
  for (INT i = 2; i <= n; i++)
    factorial = factorial * i;
  return factorial;
}

//-----------------------------------------------------------------------
// NAME: Permutation
// FUNCTION: Sets 'permutation' to the 'order'th permutation of length
//   'nloops'.
//-----------------------------------------------------------------------

extern void Permutation(INT order,
                        INT nloops,
                        INT permutation[])
{
  if (nloops == 0)
    return;
  INT* factorials = CXX_NEW_ARRAY(INT, nloops, &LNO_local_pool);
  factorials[0] = 1;
  INT i;
  for (i = 1; i < nloops; i++)
    factorials[i] = factorials[i-1] * i;
  for (i = 0; i < nloops; i++)
    permutation[i] = i;
  INT current_base = order;
  for (INT search_index = 0; search_index < nloops; search_index++) {
    INT current_index = current_base / factorials[nloops - 1 - search_index];
    INT next_index = permutation[search_index + current_index];
    for (i = search_index + current_index; i > search_index; i--)
     permutation[i] = permutation[i-1];
    permutation[search_index] = next_index;
    current_base -= current_index * factorials[nloops - 1 - search_index];
  }
  Is_True(Is_Permutation_Vector(permutation, nloops),
    ("Permutation: Not a permutation vector"));
}

//-----------------------------------------------------------------------
// NAME: WN_Whirl_Linenum
// FUNCTION: Returns the best guess at a line number for 'wn_ref'.  This
//   is the line number of the node itself, if it has one, or that of the
//   closest enclosing parent node that has one, or 0, if no enclosing
//   parent node has one.
//-----------------------------------------------------------------------

extern INT WN_Whirl_Linenum(WN* wn_ref)
{
  WN *wn;
  for (wn = wn_ref; wn != NULL; wn = LWN_Get_Parent(wn))
    if (LWN_Get_Linenum(wn) != 0)
      break;
  return wn == NULL ? 0 : LWN_Get_Linenum(wn);
}

// Given an stid of a constant, use the DU chains to perform
// constant propogation
extern void Constant_Propogate(WN *stid, INT64 const_val)
{
  MEM_POOL_Push(&LNO_local_pool);
  typedef STACK<WN *> STACK_OF_WN;
  STACK_OF_WN *new_statement_stack = CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool);
  STACK_OF_WN *use_stack =
	CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool);
  USE_LIST *uses = Du_Mgr->Du_Get_Use(stid);
  if (!uses) return;
  if (uses->Incomplete()) return;

  USE_LIST_ITER iter(uses);
  // walk all the uses
  // Put this in a stack since removing nodes can kill the iter
  for(const DU_NODE *node=iter.First();!iter.Is_Empty();node=iter.Next()){
    use_stack->Push((WN *)node->Wn());
  }
  INT i;
  for (i=0; i<use_stack->Elements(); i++) {
    WN *use = use_stack->Bottom_nth(i);
    if (WN_operator(use) == OPR_LDID &&
	  WN_st(use) == WN_st(stid) && WN_offset(use) == WN_offset(stid)) {
      // make sure that this is the only def for the use
      DEF_LIST *defs = Du_Mgr->Ud_Get_Def(use);
      if (defs && !defs->Incomplete()) {
	DEF_LIST_ITER iter2(defs);
	const DU_NODE *node2 = iter2.First();
	iter2.Next();
	if (iter2.Is_Empty() && ((WN *)node2->Wn() == stid)) {
	  // do the propogation
	  WN *parent = LWN_Get_Parent(use);
	  INT kidno=0;
	  while (WN_kid(parent,kidno) != use) kidno++;
	  TYPE_ID type = WN_rtype(use);
	  LWN_Delete_Tree(use);
	  WN_kid(parent,kidno) = LWN_Make_Icon(type,const_val);
	  LWN_Set_Parent(WN_kid(parent,kidno),parent);

	  // Find the statement enclosing this guy
	  WN *statement = parent;
	  while (OPCODE_is_expression(WN_opcode(statement))) {
	    statement = LWN_Get_Parent(statement);
          }
	  if (WN_opcode(statement) == OPC_BLOCK) {
	    statement = LWN_Get_Parent(statement);
          }
	  new_statement_stack->Push(statement);
	} 
      } 
    } 
  }
  for (i=0; i<new_statement_stack->Elements(); i++) {
    WN *statement = new_statement_stack->Bottom_nth(i);
    WN_Simplify_Tree(statement);

    // Rebuild the access vectors for this statement

    // if a lb or step rebuild vector for loop
    WN *tmp=statement;
    WN *parent = LWN_Get_Parent(tmp);
    if (WN_opcode(parent) == OPC_DO_LOOP) {
      tmp = parent;
      parent = LWN_Get_Parent(tmp);
    }

    DOLOOP_STACK *loop_stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
				       &LNO_local_pool);
    Build_Doloop_Stack(parent, loop_stack);
    LNO_Build_Access(tmp, loop_stack, &LNO_default_pool);

    if (WN_opcode(tmp) == OPC_DO_LOOP) {
      INT64 iter = Iterations(tmp,&LNO_local_pool);
      if (iter > 0) {
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(tmp);
	dli->Est_Num_Iterations = iter;
	dli->Num_Iterations_Symbolic = FALSE;
	dli->Num_Iterations_Profile = FALSE;
      }
    }

   // If this is a constant stid, recurse
   if (WN_operator(statement) == OPR_STID) {
     if (WN_operator(WN_kid0(statement))==OPR_INTCONST){
       Constant_Propogate(statement, WN_const_val(WN_kid0(statement)));
     }
   }

  }
  MEM_POOL_Pop(&LNO_local_pool);
}

//-----------------------------------------------------------------------
// NAME: Messy_Subscript
// FUNCTION: If 'wn_array' has an OPR_ARRAY ancestor with a Too_Messy sub-
//   script, return the WN* of that ancestor.  Otherwise, return NULL. 
//-----------------------------------------------------------------------

extern WN* Messy_Subscript(WN* wn_array)
{ 
  for (WN* wn = wn_array; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_operator(wn) == OPR_ARRAY) {
      ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn);
      if (aa == NULL || aa->Too_Messy)
        return wn;
      for (INT i = 0; i < aa->Num_Vec(); i++)
        if (aa->Dim(i)->Too_Messy)
          return wn; 
    }
  }
  return NULL;  
} 

//-----------------------------------------------------------------------
// NAME: Replace_Index_Variable
// FUNCTION: Create a new index variable for the loop 'cp_loop' so that
//   its index variable is not the same as that of 'loop'.  Give the new
//   index variable a name that starts with 'prefix[]'.
//-----------------------------------------------------------------------

extern void Replace_Index_Variable(WN* loop,
                                   WN* cp_loop,
                                   const char prefix[])
{
  const INT  bufsz=128;
  char       buf[bufsz];
  INT        bufcnt;

  ST* st = WN_st(WN_index(loop));
  WN_OFFSET offset = WN_offset(WN_index(loop));
  TYPE_ID wtype = WN_desc(WN_start(loop));
  bufcnt = sprintf(buf, prefix);
  (SYMBOL(WN_index(loop))).Name(buf+bufcnt, bufsz-bufcnt);
  Replace_Symbol(cp_loop, SYMBOL(st, offset, wtype),
  Create_Preg_Symbol(buf, wtype), NULL, cp_loop);
  // the loop variables are independent in the two bodies
  Fix_Do_Du_Info(cp_loop, NULL, TRUE, NULL, 1);
}

// Does 'wn1' dominate 'wn2', be conservative in that you can always say FALSE
// 'wn1' must be a statement.
BOOL Dominates(WN *wn1, WN *wn2)
{
  Is_True(!OPCODE_is_expression(WN_opcode(wn1)),
    ("Non statement 1 in Dominates"));

  // wn1's parent has to be an ancestor of wn2
  WN *parent1 = LWN_Get_Parent(wn1);
  WN *ancestor2 = LWN_Get_Parent(wn2);
  WN *kid2 = wn2;
  while (ancestor2 && (ancestor2 != parent1)) {
   kid2 = ancestor2;
   ancestor2 = LWN_Get_Parent(ancestor2);
  }
  if (!ancestor2) return FALSE;


  // at this point, wn1 is a sibling of kid2
  // return TRUE if wn1 comes before kid2
  wn1 = WN_next(wn1);
  while (wn1) {
    if (wn1 == kid2) {
      return TRUE;
    }
    wn1 = WN_next(wn1);
  }
  return FALSE;
}

