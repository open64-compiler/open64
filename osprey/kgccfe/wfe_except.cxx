/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


#include <vector>
#include <queue>
#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

#include "gnu/config.h"
#include "gnu/system.h"

#include "srcpos.h"
#include "gnu/machmode.h"
#include "gnu/tree.h"
#include "gnu/flags.h"
#include "ir_reader.h"
#include "tree_symtab.h"
#include "wfe_misc.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "rt_symtab.h"
#include "wfe_except.h"

using std::vector;
using std::queue;

inline LABEL_IDX Get_New_Label_Idx()
{
  LABEL_IDX result;
  New_LABEL(CURRENT_SYMTAB, result);
  return result;
}

struct EH_INFO {
  LABEL_IDX outer_context;
  LABEL_IDX handler;
  tree	    finalization;
  bool	    label_used;
  EH_INFO(): outer_context(Get_New_Label_Idx()),
	     handler      (Get_New_Label_Idx()),
	     finalization(NULL_TREE),
	     label_used(false) {}
};

static vector<EH_INFO> eh_info_stack;
static vector<EH_INFO> catch_stack;
static vector<LABEL_IDX> caught_return_label_stack;
static queue <EH_INFO> eh_info_queue;

void WFE_Expand_EH_Region_Start()
{
  eh_info_stack.push_back(EH_INFO());
}

WN * Make_Runtime_Call(const char * name, TY_IDX return_ty_idx)
{
  WN * result = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 0);
  WN_st_idx(result) =  Throw_Runtime_st_idx();
  return result;
}

static void WFE_Expand_Internal_Throw()
{
  WN * call_wn = Make_Runtime_Call("__throw",
				   Make_Function_Type(MTYPE_To_TY(MTYPE_V)));
  WFE_Stmt_Append(call_wn, Get_Srcpos());
}

void WFE_Expand_EH_Region_End(tree handler)
{
   EH_INFO eh_info = eh_info_stack.back();
   eh_info_stack.pop_back();
   LABEL_IDX skip_label_idx = Get_New_Label_Idx();
   WN * goto_wn = WN_CreateGoto(skip_label_idx);
   WFE_Stmt_Append(goto_wn, Get_Srcpos());
   WN * outer_context_wn = WN_CreateLabel((ST_IDX) 0, eh_info.outer_context,
				          0, NULL);
   WFE_Stmt_Append(outer_context_wn, Get_Srcpos());
   WFE_Expand_Internal_Throw();
   WN * skip_label_wn = WN_CreateLabel((ST_IDX) 0, skip_label_idx, 0, NULL);
   WFE_Stmt_Append(skip_label_wn, Get_Srcpos());
   eh_info.finalization = handler;
   eh_info_queue.push(eh_info);
}

void WFE_Expand_Leftover_Cleanups()
{
  while (!eh_info_queue.empty()) {
    EH_INFO eh_info = eh_info_queue.front();
    Is_True(eh_info.finalization != NULL_TREE,
	    ("WFE_Expand_Leftover_Cleanups: unexpected leftover try-block"));
    WFE_Stmt_Append(
      WN_CreateLabel((ST_IDX) 0, eh_info.handler, 0, NULL),
      Get_Srcpos());
    WFE_Expand_Expr(eh_info.finalization, false);
    WFE_Stmt_Append(
      WN_CreateGoto(eh_info.outer_context),
      Get_Srcpos());
    eh_info_queue.pop();
  }
}

void WFE_Expand_Start_All_Catch()
{
  caught_return_label_stack.push_back(Get_New_Label_Idx());
  WFE_Expand_EH_Region_End(NULL_TREE);
  
  LABEL_IDX outer_context = eh_info_stack.back().outer_context;

  Is_True(!eh_info_queue.empty(),
	 ("WFE_Expand_Start_all_Catch: \
           eh_info_queue unexpectedly empty: no try handler found"));
  while (eh_info_queue.front().finalization != NULL_TREE) {
    EH_INFO eh_info = eh_info_queue.front();
    WFE_Expand_Expr(eh_info.finalization);
    WFE_Stmt_Append(
      WN_CreateGoto(eh_info.outer_context),
      Get_Srcpos());
    eh_info_queue.pop();
    Is_True(!eh_info_queue.empty(),
	    ("WFE_Expand_Start_All_Catch: \
              eh_info_queue unexpectedly empty: no try handler found"));
  }

  EH_INFO eh_info = eh_info_queue.front();
  eh_info_queue.pop();
  catch_stack.push_back(eh_info);
  WFE_Expand_EH_Region_Start();
  eh_info_stack.back().outer_context = outer_context;

  WFE_Stmt_Append(
    WN_CreateLabel((ST_IDX) 0, catch_stack.back().handler, 0, NULL),
    Get_Srcpos());
}

void WFE_Expand_End_All_Catch()
{
  Is_True(!catch_stack.empty(),
  	  ("WFE_Expand_All_Catch: catch_stack unexpectedly empty"));
  EH_INFO eh_info = catch_stack.back();
  catch_stack.pop_back();
  Is_True(!eh_info_stack.empty(),
	  ("WFE_Expand_All_Catch: eh_info_stack unexpectedly empty"));
  LABEL_IDX outer_context = eh_info_stack.back().outer_context;
  WFE_Expand_EH_Region_End(NULL_TREE);

  WFE_Stmt_Append(
    WN_CreateGoto(outer_context),
    Get_Srcpos());

}  
