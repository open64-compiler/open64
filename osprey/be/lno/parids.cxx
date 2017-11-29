/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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

#include <sys/types.h>
#include <alloca.h>
#include "pu_info.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "ara.h"
#include "ara_loop.h"
#include "lego_util.h"
#include "config.h"
#include "fiz_fuse.h"
#include "debug.h"
#include "glob.h"

//-----------------------------------------------------------------------
// NAME: Has_Io
// FUNCTION: Returns TRUE if the tree rooted at 'wn_tree' has an IO
//   statement, FALSE otherwise.   All I/O whirl nodes are pushed on the
//   stack 'stk_io'.
//-----------------------------------------------------------------------

static BOOL Has_Io(WN* wn_tree, STACK<WN*>* stk_io)
{
  BOOL found_io = FALSE;

  if (WN_opcode(wn_tree) == OPC_IO) {
    stk_io->Push(wn_tree);
    found_io = TRUE;
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Has_Io(wn, stk_io))
        found_io = TRUE;
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (Has_Io(WN_kid(wn_tree, i), stk_io))
        found_io = TRUE;
  }
  return found_io;
}

//-----------------------------------------------------------------------
// NAME: First_Wn_With_Symbol 
// FUNCTION: Return the first WN* which has a symbol in the tree rooted 
//   at 'wn_base'. 
//-----------------------------------------------------------------------

static WN* First_Wn_With_Symbol(WN* wn_base)
{
  if (OPCODE_has_sym(WN_opcode(wn_base)))
    return wn_base; 
  for (INT i = 0; i < WN_kid_count(wn_base); i++) {
    WN* wn_kid_base = First_Wn_With_Symbol(WN_kid(wn_base, i)); 
    if (wn_kid_base != NULL)
      return wn_kid_base; 
  } 
  return NULL; 
} 

//-----------------------------------------------------------------------
// NAME: WN_Array_Symbol
// FUNCTION: For the ILOAD or ISTORE 'wn_ref', returns the node which
//   contains the symbol of 'wn_ref'.
//-----------------------------------------------------------------------

extern WN* WN_Array_Symbol(WN* wn_ref)
{
  OPERATOR opr = WN_operator(wn_ref);
  if (opr == OPR_LDID || opr == OPR_STID)
    return wn_ref;
  if (opr == OPR_ISTORE || opr == OPR_ILOAD) {
    WN* wn_array = (WN_operator(wn_ref) == OPR_ISTORE) ?
      WN_kid1(wn_ref) : WN_kid0(wn_ref);
    if (WN_operator(wn_array) == OPR_PARM)
      wn_array = WN_kid0(wn_array);
    if (WN_operator(wn_array) == OPR_ARRAY) { 
      WN* wn_base = WN_array_base(wn_array); 
      return First_Wn_With_Symbol(wn_base);
    } else { 
      return First_Wn_With_Symbol(wn_array);
    } 
  } else { 
    return First_Wn_With_Symbol(wn_ref);
  } 
}

//-----------------------------------------------------------------------
// NAME: Mp_Lower_Number
// FUNCTION: If it returns a positive number, 'wn_loop' is part of code which 
//   will be mp lowered as a doacross, and the number returned is the number 
//   of the doacross to which it will be lowered.  
//     If it returns a negative number, 'wn_loop' is part of code which will 
//   be lowered as a parallel region, and the number returned is the nega-
//   tive of the number of the parallel region to which it will be lowered.
//     If it returns 0, there is no number has been assigned for the code
//   of which 'wn_loop' is a part, or 'wn_loop' is in serial code.   
//-----------------------------------------------------------------------

static INT Mp_Lower_Number(WN* wn_loop)
{
  INT multiplier = 0; 
  for (WN* wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn)) { 
    if (WN_opcode(wn) != OPC_REGION)
      continue; 
    WN* wn_first = WN_first(WN_region_pragmas(wn));     
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA)
      multiplier = WN_pragma(wn_first) == WN_PRAGMA_DOACROSS 
	|| WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_DO ? 1 
	: WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN ? -1 : 0;
    WN* wnn = 0;
    for (wnn = wn_first; wnn != NULL; wnn = WN_next(wnn))  
      if (WN_opcode(wnn) == OPC_PRAGMA && WN_pragma(wnn) == WN_PRAGMA_MPNUM)
        break;  
    if (wnn != NULL) 
      return multiplier * WN_pragma_arg1(wnn); 
  }
  return 0; 
} 

//-----------------------------------------------------------------------
// NAME: Print_Mp_Lowerer_Name 
// FUNCTION: Prints to 'fp' the name of the function which will be 
//   created by the mp lowerer for the parallel loop 'wn_loop'. 
//-----------------------------------------------------------------------

extern void Print_Mp_Lowerer_Name(PU_Info* current_pu, 
				  WN* wn_loop, 
				  FILE* fp)
{
  INT mp_num = Mp_Lower_Number(wn_loop);
  if (mp_num == 0)
    return; 
  char* program_name = ST_name(PU_Info_proc_sym(current_pu)); 
  if (mp_num > 0)
    fprintf(fp, "__mpdo_%s%d", program_name, mp_num);  
  else 
    fprintf(fp, "__mpregion_%s%d", program_name, -mp_num);  
} 

//-----------------------------------------------------------------------
// NAME: Print_Prompl_Msgs
// FUNCTION: Print information for the .l file for 'wn_loop' which is part
//   of the subprogram 'current_pu'.  The file pointer for the .l file is 
//   'fp'.  The 'ffi' is a list of SNLs in the subprogram; it is used to 
//   group SNLs together in the output of the .list file. 
//-----------------------------------------------------------------------

static void Print_Prompl_Msgs(PU_Info* current_pu,
                              FILE *fp,
		              WN* wn_loop, 
                              FIZ_FUSE_INFO* ffi)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 

  // Print a blank line to separate SNLs
  INT i;
  for (i = 0; i < ffi->Num_Snl(); i++) {
    if (ffi->Get_Type(i) == Invalid || ffi->Get_Type(i) == Non_SNL)
      continue;
    if (ffi->Get_Wn(i) == wn_loop)
      break;
  }
  if (i < ffi->Num_Snl()) {
    fprintf(fp, "\n");
  }
  if (dli->Last_Value_Peeled) {
    fprintf(fp,
      "%5d: Created by peeling last iteration of parallel loop.\n",
        Srcpos_To_Line(WN_linenum(wn_loop)));
    return;
  }
  if (Do_Loop_Is_Mp(wn_loop) && !dli->Auto_Parallelized) {
    fprintf(fp, "%5d: PARALLEL (Manual) ", Srcpos_To_Line(WN_linenum(wn_loop)));
    Print_Mp_Lowerer_Name(current_pu, wn_loop, fp);
    fprintf(fp, "\n"); 
    return;
  }
  if (dli->ARA_Info == NULL)  
    return; 
  if (dli->ARA_Info->Is_Parallel() && dli->Auto_Parallelized) {
    if (dli->Is_Doacross)
      fprintf(fp, "%5d: PARALLEL (Auto Synchronized) ", 
        Srcpos_To_Line(WN_linenum(wn_loop)));
    else 
      fprintf(fp, "%5d: PARALLEL (Auto) ", Srcpos_To_Line(WN_linenum(wn_loop)));
    Print_Mp_Lowerer_Name(current_pu, wn_loop, fp);
    fprintf(fp, "\n");
    return;
  }
  INT found_problem = FALSE;
  fprintf(fp, "%5d: Not Parallel\n", Srcpos_To_Line(WN_linenum(wn_loop)));
  if (dli->ARA_Info->Is_Parallel() 
      && dli->ARA_Info->Not_Enough_Parallel_Work()) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop body does not contain enough work.\n");
    found_problem = TRUE;
  }
  if (dli->Inside_Critical_Section) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop is inside CRITICAL section.\n");
    found_problem = TRUE;
  }
  if (dli->Has_Threadprivate) { 
    fprintf(fp, "         ");
    fprintf(fp, "Loop has THREAD_PRIVATE variables.\n");
    found_problem = TRUE;
  }
  if (dli->Has_Exits) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop has early exits.\n");
    found_problem = TRUE;
  }
  if (dli->Pragma_Cannot_Concurrentize) {
    fprintf(fp, "         ");
    fprintf(fp, "Pragma on loop inhibits parallelization.\n");
    found_problem = TRUE;
  }
  if (dli->Serial_Version_of_Concurrent_Loop) { 
    fprintf(fp, "         ");
    fprintf(fp, "Loop in serial version of parallel loop.\n");
    found_problem = TRUE;
  }
  if (!Upper_Bound_Standardize(WN_end(wn_loop), TRUE)) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop bounds could not be put in standard form.\n");
    found_problem = TRUE;
  }
  if (Inside_Lego_Tiled_Loop(wn_loop)) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop refers to distributed or reshaped arrays.\n");
    found_problem = TRUE;
  }
  if (dli->ARA_Info->Is_OK_Parallel() 
      && dli->ARA_Info->Inner_Loop_Is_Suggested_Parallel()) {
    fprintf(fp, "         ");
    fprintf(fp, "Parallelizing innermore loop was deemed more profitable.\n");
    found_problem = TRUE;
  }
  if (Outermore_Parallel_Construct(wn_loop, FALSE)) {
    fprintf(fp, "         ");
    fprintf(fp, "Loop is contained within a parallel construct.\n");
    found_problem = TRUE;
  }
  if (dli->Has_Unsummarized_Calls || !dli->Is_Concurrent_Call) {
    for (i = 0; i < dli->ARA_Info->Call_No_Dep_Vars().Elements(); i++) {
      char* call_name = dli->ARA_Info->Call_No_Dep_Vars().Bottom_nth(i);
      INT ln_call = dli->ARA_Info->Ln_Call_No_Dep_Vars().Bottom_nth(i);
      fprintf(fp, "         ");
      found_problem = TRUE;
      if (strlen(call_name) == 0) 
	fprintf(fp, "Call on line %d.\n", ln_call);
      else 
	fprintf(fp, "Call %s on line %d.\n", call_name, ln_call);
    }
  }
  STACK<WN*> stk_io(&ARA_memory_pool);
  if (Has_Io(wn_loop, &stk_io)) {
    for (INT i = 0; i < stk_io.Elements(); i++) {
      fprintf(fp, "         ");
      found_problem = TRUE;
      INT ln_node = WN_Whirl_Linenum(stk_io.Bottom_nth(i));
      fprintf(fp, "Has IO statement on line %d.\n", ln_node);
    }
  }
  if (found_problem) {
    return;
  }
  for (i = 0; i < dli->ARA_Info->Scalar_Vars().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    fprintf(fp, "Scalar dependence on %s.",
      dli->ARA_Info->Scalar_Vars().Bottom_nth(i).Prompf_Name());
    fprintf(fp, "\n");
  }
  for (i = 0; i < dli->ARA_Info->Scalar_Alias().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    fprintf(fp, "Aliased scalar %s.",
      dli->ARA_Info->Scalar_Alias().Bottom_nth(i).Prompf_Name());
    fprintf(fp, "\n");
  }
  for (i = 0; i < dli->ARA_Info->Scalar_No_Final().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    fprintf(fp, "Scalar %s without unique last value.",
      dli->ARA_Info->Scalar_No_Final().Bottom_nth(i).Prompf_Name());
    fprintf(fp, "\n");
  }
  for (i =0; i < dli->ARA_Info->Scalar_Bad_Peel().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    SYMBOL sym_priv = dli->ARA_Info->Scalar_Bad_Peel().Bottom_nth(i);
    INT ln_priv = dli->ARA_Info->Ln_Scalar_Bad_Peel().Bottom_nth(i); 
    fprintf(fp, "Last iteration may not write value of scalar %s on line %d.",
      sym_priv.Prompf_Name(), ln_priv);
    fprintf(fp, "\n");
  }
  for (i = 0; i < dli->ARA_Info->Array_No_Dep_Vars().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    SYMBOL sym_node = dli->ARA_Info->Array_No_Dep_Vars().Bottom_nth(i);
    INT ln_node = dli->ARA_Info->Ln_Array_No_Dep_Vars().Bottom_nth(i);
    fprintf(fp, "Array %s without dependence information. ", 
      sym_node.Prompf_Name()); 
    fprintf(fp, "on line %d.", ln_node);
    fprintf(fp, "\n");
  }
  for (i = 0; i < dli->ARA_Info->Ln_Misc_No_Dep_Vars().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    INT ln_node = dli->ARA_Info->Ln_Misc_No_Dep_Vars().Bottom_nth(i);
    fprintf(fp, "Reference without dependence information ");
    fprintf(fp, "on line %d.", ln_node);
    fprintf(fp, "\n");
  }
  for (i = 0; i < dli->ARA_Info->Dep_Vars().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    SYMBOL sym_source = dli->ARA_Info->Dep_Source().Bottom_nth(i); 
    SYMBOL sym_sink = dli->ARA_Info->Dep_Sink().Bottom_nth(i); 
    INT ln_source = dli->ARA_Info->Ln_Dep_Source().Bottom_nth(i);
    INT ln_sink = dli->ARA_Info->Ln_Dep_Sink().Bottom_nth(i);
    fprintf(fp, "Array dependence from %s on line %d ",
      sym_source.Prompf_Name(), ln_source);
    fprintf(fp, "to %s on line %d.",
      sym_sink.Prompf_Name(), ln_sink);
    fprintf(fp, "\n");
  }
  for (i =0; i < dli->ARA_Info->Dep_Bad_Peel().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    SYMBOL sym_priv = dli->ARA_Info->Dep_Bad_Peel().Bottom_nth(i);
    INT ln_priv = dli->ARA_Info->Ln_Dep_Bad_Peel().Bottom_nth(i);
    fprintf(fp, "Last iteration may not write value of array %s on line %d.",
      sym_priv.Prompf_Name(), ln_priv);
    fprintf(fp, "\n");
  }
  for (i =0; i < dli->ARA_Info->Partial_Array_Sec().Elements(); i++) {
    fprintf(fp, "         ");
    found_problem = TRUE;
    SYMBOL sym_priv = dli->ARA_Info->Partial_Array_Sec().Bottom_nth(i);
    fprintf(fp, "Cannot privatize %s. Requires partial array section.",
      sym_priv.Prompf_Name());
    fprintf(fp, "\n");
  }
  if (!found_problem && dli->Has_Bad_Mem) {
    fprintf(fp, "         ");
    fprintf(fp, "Indirect reference.");
    fprintf(fp, "\n");
    found_problem = TRUE;
  }
  if (!found_problem) {
    fprintf(fp, "         ");
    fprintf(fp, "Reason unknown.");
    fprintf(fp, "\n");
  }
}

//-----------------------------------------------------------------------
// NAME: Print_Prompl_Msgs_Traverse
// FUNCTION: Traversal function for Print_Prompl_Msgs(). 
//-----------------------------------------------------------------------

static void Print_Prompl_Msgs_Traverse(PU_Info* current_pu,
				       FILE* fp, 
				       WN* wn_tree,
				       FIZ_FUSE_INFO* ffi)
{
  if (WN_opcode(wn_tree) == OPC_DO_LOOP) { 
    Print_Prompl_Msgs(current_pu, fp, wn_tree, ffi);
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Is_Inner)
      return; 
  } 

  if (WN_opcode(wn_tree) == OPC_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))  
      Print_Prompl_Msgs_Traverse(current_pu, fp, wn, ffi); 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      Print_Prompl_Msgs_Traverse(current_pu, fp, WN_kid(wn_tree, i), ffi); 
  } 
}

//-----------------------------------------------------------------------
// NAME: Print_Prompl_Msgs
// FUNCTION: Print messages for prompf .list file in human readable form.
//-----------------------------------------------------------------------

static BOOL opened_lfile = FALSE;

extern void Print_Prompl_Msgs(PU_Info* current_pu,
                              WN* func_nd)
{
  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&ARA_memory_pool), &ARA_memory_pool);
  ffi->Build(func_nd, TRUE);
  char* filename = (char *) alloca(strlen(Obj_File_Name) + strlen(".list"));
  strcpy(filename, Obj_File_Name);
  INT i;
  for (i = strlen(Obj_File_Name); i >= 0; i--)
    if (filename[i] == '.')
      break;
  FmtAssert(i >= 0,
    ("Print_Prompl_Parallelization_Msgs: could not find '.'"));
  strcpy(&filename[i], ".list");
  FILE* fp = NULL;
  if (!opened_lfile) {
    fprintf(stdout, 
      "The file %s gives the parallelization status of each loop.\n",
      filename);
    fp = fopen(filename, "w");
    opened_lfile = TRUE;
  } else {
    fp = fopen(filename, "a");
  }
  if (fp == NULL) { 
    fprintf(stderr, "Fatal: Unable to open file %s\n", filename); 
    exit(1); 
  } 
  fprintf(fp, "Parallelization Log for Subprogram %s\n",
    ST_name(PU_Info_proc_sym(current_pu)));

  Print_Prompl_Msgs_Traverse(current_pu, fp, func_nd, ffi); 
  fprintf(fp, "\n\n");
  fclose(fp);
}

//-----------------------------------------------------------------------
// NAME: Lowered_Doacross_Loops
// FUNCTION: Returns the number of doacross loops which will be lowered
//   by the mp lowerer in the tree rooted at 'wn_tree', a list of them
//   on the 'stk_wn'.
//-----------------------------------------------------------------------

static INT Lowered_Doacross_Loops(WN* wn_tree,
                                  STACK<WN*>* stk_wn)
{
  INT doacross_count = 0;

  // Do not descend into parallel regions or orphaned PDO's
  if (WN_opcode(wn_tree) == OPC_REGION) {
    WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA
        && (WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN ||
	    WN_pragma(wn_first) == WN_PRAGMA_PDO_BEGIN)) {
      return doacross_count;
    }
  }

  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree);
    if (dli->Mp_Info != NULL && dli->Mp_Info->Nest_Index() == 0) {
      stk_wn->Push(wn_tree);
      doacross_count++;
    }
    if (dli->Is_Inner)
      return doacross_count;
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      doacross_count += Lowered_Doacross_Loops(wn, stk_wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      doacross_count += Lowered_Doacross_Loops(WN_kid(wn_tree, i), stk_wn);
  }
  return doacross_count;
}

//-----------------------------------------------------------------------
// NAME: Do_Loops
// FUNCTION: Return the number of do loops in the tree rooted at 'wn_tree'
//   and push a list of them on the stack 'stk_wn'.
//-----------------------------------------------------------------------

static INT Do_Loops(WN* wn_tree, 
		    STACK<WN*>* stk_wn)
{
  INT do_count = 0; 

  if (WN_opcode(wn_tree) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_tree); 
    stk_wn->Push(wn_tree);
    do_count++;
    if (dli->Is_Inner)
      return do_count;
  }

  if (WN_opcode(wn_tree) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      do_count += Do_Loops(wn, stk_wn);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      do_count += Do_Loops(WN_kid(wn_tree, i), stk_wn);
  }
  return do_count;
}

//-----------------------------------------------------------------------
// NAME: Lowered_Parallel_Regions
// FUNCTION: Returns the number of parallel regions which will be lowered
//   by the mp lowerer in the tree rooted at 'wn_tree', a list of them
//   on the 'stk_wn'.
//-----------------------------------------------------------------------

static INT Lowered_Parallel_Regions(WN* wn_tree,
                                    STACK<WN*>* stk_wn)
{
  INT parallel_region_count = 0;
  if (WN_opcode(wn_tree) == OPC_REGION) {
    WN* wn_first = WN_first(WN_region_pragmas(wn_tree));
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA
        && WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN) {
      stk_wn->Push(wn_tree);
      parallel_region_count++;
    }
  }
  if (WN_opcode(wn_tree) == OPC_BLOCK) {
     for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
       parallel_region_count += Lowered_Parallel_Regions(wn, stk_wn);
  } else {
     for (INT i = 0; i < WN_kid_count(wn_tree); i++)
       parallel_region_count += Lowered_Parallel_Regions(WN_kid(wn_tree, i),
         stk_wn);
  }
  return parallel_region_count;
}

//-----------------------------------------------------------------------
// NAME: Assign_Doacross_Mpnums 
// FUNCTION: Assign numbers to be used by the MP lowerer for the doacross
//   loops listed on the stack 'stk_wn'. 
//-----------------------------------------------------------------------

static void Assign_Doacross_Mpnums(STACK<WN*>* stk_wn)
{
  INT doacross_count = 0;
  for (INT i = 0; i < stk_wn->Elements(); i++) {
    WN* wn_loop = stk_wn->Bottom_nth(i);
    WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
    WN* wn_first = WN_first(WN_region_pragmas(wn_region));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPNUM, (ST_IDX) NULL,
      ++doacross_count, 0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma);
  }
}

//-----------------------------------------------------------------------
// NAME: Assign_Parallel_Region_Mpnums 
// FUNCTION: Assign numbers to be used by the MP lowerer for the parallel
//   regions listed on the stack 'stk_wn'. 
//-----------------------------------------------------------------------

static void Assign_Parallel_Region_Mpnums(STACK<WN*>* stk_wn)
{
  INT parallel_region_count = 0;
  for (INT i = 0; i < stk_wn->Elements(); i++) {
    WN* wn_region = stk_wn->Bottom_nth(i);
    WN* wn_first = WN_first(WN_region_pragmas(wn_region));
    WN* wn_pragma = WN_CreatePragma(WN_PRAGMA_MPNUM, (ST_IDX) NULL,
      ++parallel_region_count, 0);
    LWN_Insert_Block_Before(WN_region_pragmas(wn_region), NULL, wn_pragma);
  }
}

//-----------------------------------------------------------------------
// NAME: Add_Doacross_Comments 
// FUNCTION: For each doacross loop on 'stk_wn' place a comment before the
//   loop indicating the name which will be used for the subroutine which 
//   is created by the mp lowerer for that loop. 
//-----------------------------------------------------------------------

static void Add_Doacross_Comments(PU_Info* current_pu,
                                  STACK<WN*>* stk_wn)
{
  INT doacross_count = 0;
  for (INT i = 0; i < stk_wn->Elements(); i++) {
    WN* wn_loop = stk_wn->Bottom_nth(i);
    WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn_loop));
    WN* wn_place_comment = wn_region;
    if (LWN_Get_Parent(wn_region) != NULL) {
       WN* wn_if = LWN_Get_Parent(LWN_Get_Parent(wn_region));
       if (wn_if != NULL && WN_opcode(wn_if) == OPC_IF
            && WN_Is_If_MpVersion(wn_if))
          wn_place_comment = wn_if;
    }
    char* comment = NULL; 
    WN* wn_first = WN_first(WN_region_pragmas(wn_region)); 
    FmtAssert(WN_opcode(wn_first) == OPC_PRAGMA, 
      ("Add_Doacross_Comments: Expected a pragma")); 
    if (WN_pragma(wn_first) == WN_PRAGMA_DOACROSS) { 
      comment = (char *) alloca(strlen(ST_name(PU_Info_proc_sym(current_pu)))
	+ strlen("__mpdo_") + 10 +
	+ strlen("DOACROSS will be converted to SUBROUTINE ") + 1);
      sprintf(comment, 
	"DOACROSS will be converted to SUBROUTINE __mpdo_%s%d",
	ST_name(PU_Info_proc_sym(current_pu)), i + 1);
    } else if (WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_DO) { 
      comment = (char *) alloca(strlen(ST_name(PU_Info_proc_sym(current_pu)))
	+ strlen("__mpdo_") + 10 +
	+ strlen("PARALLEL DO will be converted to SUBROUTINE ") + 1);
      sprintf(comment, 
	"PARALLEL DO will be converted to SUBROUTINE __mpdo_%s%d",
	ST_name(PU_Info_proc_sym(current_pu)), i + 1);
    } else { 
      FmtAssert(FALSE, 
	("Add_Doacross_Comments: Expected a DOACROSS or PARALLEL DO")); 
    }  
    WN* wn_comment = WN_CreateComment(comment);
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_place_comment),
      wn_place_comment, wn_comment);
  }
}

//-----------------------------------------------------------------------
// NAME: Add_Parallel_Region_Comments 
// FUNCTION: For each parallel region on 'stk_wn' place a comment before the
//   loop indicating the name which will be used for the subroutine which 
//   is created by the mp lowerer for that parallel region. 
//-----------------------------------------------------------------------

static void Add_Parallel_Region_Comments(PU_Info* current_pu,
                                         STACK<WN*>* stk_wn)
{
  INT doacross_count = 0;
  for (INT i = 0; i < stk_wn->Elements(); i++) {
    WN* wn_region = stk_wn->Bottom_nth(i);
    WN* wn_place_comment = wn_region;
    if (LWN_Get_Parent(wn_region) != NULL) {
       WN* wn_if = LWN_Get_Parent(LWN_Get_Parent(wn_region));
       if (wn_if != NULL && WN_opcode(wn_if) == OPC_IF
            && WN_Is_If_MpVersion(wn_if))
          wn_place_comment = wn_if;
    }
    char* comment =
        (char *) alloca(strlen(ST_name(PU_Info_proc_sym(current_pu)))
      + strlen("__mpregion_") + 10 +
      + strlen("PARALLEL REGION will be converted to SUBROUTINE ") + 1);
    sprintf(comment, 
      "PARALLEL REGION will be converted to SUBROUTINE __mpregion_%s%d",
      ST_name(PU_Info_proc_sym(current_pu)), i + 1);
    WN* wn_comment = WN_CreateComment(comment);
    LWN_Insert_Block_Before(LWN_Get_Parent(wn_place_comment),
      wn_place_comment, wn_comment);
  }
}

//-----------------------------------------------------------------------
// NAME: Annotate_For_Mp_Lowering
// FUNCTION: Annotate the function 'func_nd' with information about what 
//   functions will be created when the doacrosses and parallel regions 
//   are lowered.  
//-----------------------------------------------------------------------

extern void Annotate_For_Mp_Lowering(PU_Info* current_pu,
				     WN* func_nd)
{
  STACK<WN*> stk_wn_loop(&LNO_local_pool); 
  STACK<WN*> stk_wn_region(&LNO_local_pool); 
  Lowered_Doacross_Loops(func_nd, &stk_wn_loop); 
  Lowered_Parallel_Regions(func_nd, &stk_wn_region); 
  Assign_Doacross_Mpnums(&stk_wn_loop); 
  Assign_Parallel_Region_Mpnums(&stk_wn_region);
  Add_Doacross_Comments(current_pu, &stk_wn_loop);
  Add_Parallel_Region_Comments(current_pu, &stk_wn_region);
} 
