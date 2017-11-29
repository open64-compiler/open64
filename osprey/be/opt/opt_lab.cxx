/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include <sys/elf_whirl.h>	    /* for WHIRL_REVISION */

#include "defs.h"
#include "config.h"		/* for Run_wopt, etc.		*/
#include "config_debug.h"	/* for DEBUG_Ir_Version_Check	*/
#include "glob.h"		/* for Irb_File_Name, Ipa_File_Name */
#include "timing.h"		/* for T_Optimize_CU		*/
#include "tracing.h"		/* for Get_Trace, etc.		*/
#include "opt_base.h"

#include "wn.h"			/* WHIRL Node descriptor	*/
#include "wn_util.h"
#include "cxx_memory.h"
#include "stab.h"		/* WHIRL symtab global variables*/
#include "optimizer.h"		/* External interface for optimizer phase */

#include "opt_defs.h"
#include "opt_config.h"
#include "region_util.h"
#include "region_main.h"	/* for RBI and RAIL prototypes	*/
#include "be_version.h"         /* for Get_BE_Version */
#include "opt_lab.h"

#define MAP_HASH_SIZE 20

// set up the label map and the required memory pool
//
LabelOpt::LabelOpt()
{
  MEM_POOL_Initialize(&_label_mem_pool, "label_mem_pool", FALSE);
  MEM_POOL_Push(&_label_mem_pool);
    
  _label_map = CXX_NEW(MAP(MAP_HASH_SIZE, &_label_mem_pool), &_label_mem_pool);
}

// tear down the memory pool
//
LabelOpt::~LabelOpt()
{
  MEM_POOL_Pop(&_label_mem_pool);
  MEM_POOL_Delete(&_label_mem_pool);
}

// Enter/mark the argument, representing a label number, into the label map.  
//
void 
LabelOpt::Set_Mark(INT32 num) 
{
  _label_map->Add_map((POINTER)(INTPTR)num, (POINTER)TRUE);
}

// return true if the arugment, representing a label number, was
// entered into the label map.
//
BOOL LabelOpt::Get_Mark(INT32 num) 
{
  return (BOOL)((_label_map->Get_val((POINTER)(INTPTR)num)) != NULL);
}

//
// walk through the whirl nodes recursivevly and mark any labels referenced
// in control transfer instructions
//
void
LabelOpt:: Mark_Referenced_Labels(WN * wn)
{
  FmtAssert(wn != NULL, ("cannot mark a null label"));

  switch (WN_operator(wn)) {
  case OPR_FUNC_ENTRY:
    Mark_Referenced_Labels(WN_func_body(wn));
    break;
  case OPR_IF:
    Mark_Referenced_Labels(WN_kid(wn, 0));
    Mark_Referenced_Labels(WN_kid(wn, 1));
    Mark_Referenced_Labels(WN_kid(wn, 2));
    break;
  case OPR_DO_LOOP:
    Mark_Referenced_Labels(WN_kid(wn, 4));
    break;
  case OPR_WHILE_DO:
  case OPR_DO_WHILE:
    Mark_Referenced_Labels(WN_kid(wn, 1));
    break;
  case OPR_BLOCK:
    for (WN * stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
      Mark_Referenced_Labels(stmt);
    }
    break;
  case OPR_GOTO:
  case OPR_FALSEBR:
  case OPR_TRUEBR:
    Set_Mark(WN_label_number(wn));
    break;
  default:
    return;
  }
}

// walk the whirl nodes recursively, loooking for label nodes inside 
// blocks and deleteing them if not marked or targets of control transfer
// nodes
//
void
LabelOpt::Remove_Unmarked_Labels(WN * wn)
{
  FmtAssert(wn != NULL, ("can remove a null label"));
  WN * nextstmt;

  switch (WN_operator(wn)) {
  case OPR_FUNC_ENTRY:
    Remove_Unmarked_Labels(WN_func_body(wn));
    break;
  case OPR_IF:
    Remove_Unmarked_Labels(WN_kid(wn, 0));
    Remove_Unmarked_Labels(WN_kid(wn, 1));
    Remove_Unmarked_Labels(WN_kid(wn, 2));
    break;
  case OPR_DO_LOOP:
    Remove_Unmarked_Labels(WN_kid(wn, 4));
    break;
  case OPR_WHILE_DO:
  case OPR_DO_WHILE:
    Remove_Unmarked_Labels(WN_kid(wn, 1));
    break;
  case OPR_BLOCK:
    nextstmt = NULL;
    for (WN * stmt = WN_first(wn); stmt != NULL; stmt = nextstmt) {
      nextstmt = WN_next(stmt);
      if (WN_operator(stmt) == OPR_LABEL) {
	if (Get_Mark(WN_label_number(stmt)) == FALSE) {
	  WN_DELETE_FromBlock(wn, stmt);
	}
      } else {
	Remove_Unmarked_Labels(stmt);
      }
    }
    break;
  default:
    return;
  }
}

// The main driver for removing the labels that are not used.
//  
void 
LabelOpt::Remove_Unreferenced_Labels(WN * wn)
{
  LabelOpt labelOpt;
  labelOpt.Mark_Referenced_Labels(wn);
  labelOpt.Remove_Unmarked_Labels(wn);
}
