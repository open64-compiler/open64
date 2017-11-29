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


#include "lwn_util.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "symtab_utils.h"
#include "eliminate.h"

//-----------------------------------------------------------------------
// NAME: GOTO_LIST::GOTO_LIST
// FUNCTION: Constructor for GOTO_LIST
//-----------------------------------------------------------------------

GOTO_LIST::GOTO_LIST(MEM_POOL* mem_pool)
{
  _mem_pool = mem_pool;
  _label = NULL;
  _label_number = -1;
  _gotos = CXX_NEW(DYN_ARRAY<WN*>(mem_pool), mem_pool);
}

//-----------------------------------------------------------------------
// NAME: GOTO_LIST::Add_Goto_Unique
// FUNCTION: Add a conditional or unconditional goto on 'wn' to the 
//   GOTO_LIST if it is not already there. 
//-----------------------------------------------------------------------

void GOTO_LIST::Add_Goto_Unique(WN* wn)
{
  for (INT i = 0; i <= _gotos->Lastidx(); i++)
    if ((*_gotos)[i] == wn)
      return;
  _gotos->AddElement(wn);
}

//-----------------------------------------------------------------------
// NAME: GOTO_LIST::Print
// FUNCTION: Print the GOTO_LIST. 
//-----------------------------------------------------------------------

void GOTO_LIST::Print(FILE* fp, INT increment)
{
  fprintf(fp, "LABEL #%d: (0x%p): \n", _label_number, _label);
  for (INT i = 0; i < Elements(); i++) {
    for (INT j = 0; j < increment; j++)
      fprintf(fp, " ");
    WN* wn_goto = Goto(i);
    INT label_number = WN_label_number(wn_goto);
    const char* goto_type = WN_operator(wn_goto) == OPR_GOTO ? "GOTO"
      : WN_operator(wn_goto) == OPR_TRUEBR ? "TRUEBR"
      : WN_operator(wn_goto) == OPR_FALSEBR ? "FALSEBR"
      : "UNKNOWN";
    fprintf(fp, "[%d] %s #%d (0x%p)\n", i, goto_type, label_number, wn_goto);
  }
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::LABEL_LIST
// FUNCTION: Constructor for LABEL_LIST. 
//-----------------------------------------------------------------------

LABEL_LIST::LABEL_LIST(MEM_POOL* mem_pool)
{
  _mem_pool = mem_pool;
  _has_assigned_goto = FALSE;
  _labels = CXX_NEW(DYN_ARRAY<GOTO_LIST>(mem_pool), mem_pool);
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Label_List_Label_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree' adding labels to the
//   LABEL_LIST.  Use memory from 'mem_pool'. 
//-----------------------------------------------------------------------

void LABEL_LIST::Label_List_Label_Traverse(MEM_POOL* mem_pool,
                                           WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_LABEL)
    Add_Label_Unique(wn_tree);

  if (WN_operator(wn_tree) == OPR_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) {
      Label_List_Label_Traverse(mem_pool, wn);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Label_List_Label_Traverse(mem_pool, WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Label_List_Goto_Traverse
// FUNCTION: Traverse the tree rooted at 'wn_tree' adding conditional and
//   unconditional gotos to the LABEL_LIST.  Use memory from 'mem_pool'.
//-----------------------------------------------------------------------

void LABEL_LIST::Label_List_Goto_Traverse(MEM_POOL* mem_pool,
                                          WN* wn_tree)
{
  switch (WN_operator(wn_tree)) {
  case OPR_GOTO:
  case OPR_FALSEBR:
  case OPR_TRUEBR:
    Add_Goto_Unique(wn_tree);
    break;
  case OPR_AGOTO:
    _has_assigned_goto = TRUE;
    break;
  }

  if (WN_operator(wn_tree) == OPR_BLOCK) {
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn)) {
      Label_List_Goto_Traverse(mem_pool, wn);
    }
  } else {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Label_List_Goto_Traverse(mem_pool, WN_kid(wn_tree, i));
  }
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::LABEL_LIST
// FUNCTION: Constructor for LABEL_LIST.  This constructs a full label 
//   list for the function whose root is 'wn_func'. 
//-----------------------------------------------------------------------

LABEL_LIST::LABEL_LIST(MEM_POOL* mem_pool,
                       WN* wn_func)
{
  _mem_pool = mem_pool;
  _has_assigned_goto = FALSE;
  _labels = CXX_NEW(DYN_ARRAY<GOTO_LIST>(mem_pool), mem_pool);
  Label_List_Label_Traverse(mem_pool, wn_func);
  if (_labels->Elements() > 0)
    Label_List_Goto_Traverse(mem_pool, wn_func);
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Add_Label
// FUNCTION: Add the label at node 'wn_label' to the LABEL_LIST. 
//-----------------------------------------------------------------------

void LABEL_LIST::Add_Label(WN* wn_label)
{
  FmtAssert(WN_operator(wn_label) == OPR_LABEL,
    ("LABEL_LIST::Add_Label: Expecting a LABEL node"));
  GOTO_LIST* goto_list = CXX_NEW(GOTO_LIST(_mem_pool), _mem_pool);
  goto_list->Set_Label(wn_label);
  goto_list->Set_Label_Number(WN_label_number(wn_label));
  _labels->AddElement(*goto_list);
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Add_Label_Unique
// FUNCTION: Add the label at node 'wn_label' to the LABEL_LIST, if it
//   is not already on that list.
//-----------------------------------------------------------------------

void LABEL_LIST::Add_Label_Unique(WN* wn_label)
{
  FmtAssert(WN_operator(wn_label) == OPR_LABEL,
    ("LABEL_LIST::Add_Label_Unique: Expecting a LABEL node"));
  INT label_number = WN_label_number(wn_label);
  for (INT i = 0; i <= _labels->Lastidx(); i++)
    if ((*_labels)[i].Label_Number() == label_number) {
      if ((*_labels)[i].Label() == NULL)
        (*_labels)[i].Set_Label(wn_label);
      return;
    }
  Add_Label(wn_label);
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Add_Goto_Unique
// FUNCTION: Add a conditional or unconditional branch 'wn_goto' to the 
//   LABEL_LIST if it is not already there.  
//-----------------------------------------------------------------------

void LABEL_LIST::Add_Goto_Unique(WN* wn_goto)
{
  INT label_number = WN_label_number(wn_goto);
  INT i;
  for (i = 0; i <= _labels->Lastidx(); i++)
    if ((*_labels)[i].Label_Number() == label_number)
      break;
  if (i > _labels->Lastidx()) {
     GOTO_LIST* goto_list = CXX_NEW(GOTO_LIST(_mem_pool), _mem_pool);
     goto_list->Set_Label(NULL);
     goto_list->Set_Label_Number(label_number);
     _labels->AddElement(*goto_list);
  }
  (*_labels)[i].Add_Goto_Unique(wn_goto);
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Find_Label_Number
// FUNCTION: Return a pointer to a GOTO_LIST with for the label with the
//   given 'label_number'.
//-----------------------------------------------------------------------

GOTO_LIST* LABEL_LIST::Find_Label_Number(INT label_number)
{
  for (INT i = 0; i < Elements(); i++)
    if (Label(i)->Label_Number() == label_number)
      return Label(i);
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: Label_Used_In_InitV
// FUNCTION: Returns TRUE if the label with the given 'label_idx' is used
//   in an initializer with INITV_IDX 'inv', FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Label_Used_In_InitV(INITV_IDX inv,
				LABEL_IDX label_idx)
{
  INITV_IDX invv; 
  switch (INITV_kind(inv)) { 
  case INITVKIND_SYMDIFF:
  case INITVKIND_SYMDIFF16: {
    LABEL_IDX lbl_idx = INITV_lab1(inv);
    if (lbl_idx == label_idx)
      return TRUE; 
    } 
    break;
  case INITVKIND_LABEL: { 
    LABEL_IDX lbl_idx = INITV_lab(inv);
    if (lbl_idx == label_idx)
      return TRUE; 
    } 
    break;
  case INITVKIND_BLOCK: { 
    FOREACH_INITV(INITV_blk(inv), invv) 
      if (Label_Used_In_InitV(invv, label_idx))
	return TRUE; 
    } 
    break;
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: Label_Used_In_Init
// FUNCTION: Returns TRUE if the label with the given 'label_idx' is used
//   in an initializer in the current symtab, FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Label_Used_In_Init(LABEL_IDX label_idx)
{
  INT i; 
  INITO* ino;
  INITV_IDX inv; 
  FOREACH_INITO(CURRENT_SYMTAB, ino, i) 
    FOREACH_INITV(INITO_val(*ino), inv) 
      if (Label_Used_In_InitV(inv, label_idx))
	return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Label_Is_Targeted_Outside_Scope
// FUNCTION:  
//-----------------------------------------------------------------------

BOOL LABEL_LIST::Label_Is_Targeted_Outside_Scope(WN* wn_label)
{
  if (Label_Used_In_Init(WN_label_number(wn_label)))
    return TRUE; 
  WN* wnn = NULL; 
  WN* wn = 0;
  for (wn = wn_label; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (WN_operator(wn) == OPR_IF || WN_operator(wn) == OPR_DO_LOOP) 
       break;
    wnn = wn;
  }
  if (wn == NULL)
    return FALSE; 
  WN* wn_control_label = wnn;
  GOTO_LIST* goto_list = Find_Label_Number(WN_label_number(wn_label));
  if (goto_list != NULL) {
    for (INT i = 0; i < goto_list->Elements(); i++) {
      WN* wn_goto = goto_list->Goto(i);
      WN* wnn = NULL; 
      for (WN* wn = wn_goto; wn != NULL; wn = LWN_Get_Parent(wn)) {
	if (WN_operator(wn) == OPR_IF || WN_operator(wn) == OPR_DO_LOOP)
	  break;
	wnn = wn;
      } 
      if (wnn != wn_control_label) 
	return TRUE;
    }
  }
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Has_Targeted_Label
// FUNCTION: Returns TRUE if the tree rooted at 'wn_tree' has a LABEL 
//   which is targeted outside its scope.  Returns FALSE otherwise. 
//-----------------------------------------------------------------------

BOOL LABEL_LIST::Has_Targeted_Label(WN* wn_tree)
{
  if (WN_operator(wn_tree) == OPR_LABEL 
      && Label_Is_Targeted_Outside_Scope(wn_tree))
    return TRUE; 

  if (OPERATOR_is_expression(WN_operator(wn_tree)))
    return FALSE; 

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      if (Has_Targeted_Label(wn))
        return TRUE; 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      if (Has_Targeted_Label(WN_kid(wn_tree, i)))
	return TRUE; 
  } 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Print
// FUNCTION: Print the LABEL_LIST. 
//-----------------------------------------------------------------------

void LABEL_LIST::Print(FILE* fp, INT increment)
{
  if (Has_Assigned_Goto())
    fprintf(fp, "Has Assigned Goto\n");
  for (INT i = 0; i < Elements(); i++) {
    for (INT j = 0; j < increment; j++)
      fprintf(fp, " ");
    fprintf(fp, "[%d] ", i);
    Label(i)->Print(fp, 4 + increment);
  }
}

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Remove_Label
// FUNCTION: Remove the label 'wn_label' from the LABEL_LIST.
//-----------------------------------------------------------------------

void LABEL_LIST::Remove_Label(WN* wn_label)
{ 
  LABEL_LIST* new_list = 
    CXX_NEW(LABEL_LIST(Mem_Pool()), Mem_Pool());
  INT i;
  for (i = 0; i < Elements(); i++) {
    if (Label(i)->Label() != wn_label) { 
      new_list->Add_Label_Unique(Label(i)->Label());
      for (INT j = 0; j < Label(i)->Elements(); j++) 
	new_list->Add_Goto_Unique(Label(i)->Goto(j));
    } 
  } 
  _labels->Resetidx();
  for (i = 0; i < new_list->Elements(); i++) {
    Add_Label_Unique(new_list->Label(i)->Label());
    for (INT j = 0; j < new_list->Label(i)->Elements(); j++) 
      Add_Goto_Unique(new_list->Label(i)->Goto(j));
  } 
} 

//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Remove_Target
// FUNCTION: Remove the target 'wn_target' from the LABEL_LIST.
//-----------------------------------------------------------------------

void LABEL_LIST::Remove_Target(WN* wn_target)
{ 
  INT i;
  for (i = 0; i < Elements(); i++)
    if (Label(i)->Label_Number() == WN_label_number(wn_target))
      break; 
  if (i == Elements())
    return; 
  INT j;
  for (j = 0; j < Label(i)->Elements(); j++)  
    if (Label(i)->Goto(j) == wn_target) 
      break; 
  if (j == Label(i)->Elements())
    return; 
  INT bad_index = j; 
  GOTO_LIST* new_list = CXX_NEW(GOTO_LIST(Mem_Pool()), Mem_Pool());
  for (j = 0; j < Label(i)->Elements(); j++) 
    if (j != bad_index)  
      new_list->Add_Goto_Unique(Label(i)->Goto(j));
  Label(i)->Reset_Targets();
  for (j = 0; j < new_list->Elements(); j++)  
    Label(i)->Add_Goto_Unique(new_list->Goto(j));
} 
      
//-----------------------------------------------------------------------
// NAME: LABEL_LIST::Remove_Tree
// FUNCTION: Remove all references on the LABEL_LIST to nodes in the tree 
//   rooted at 'wn_tree'.
//-----------------------------------------------------------------------

void LABEL_LIST::Remove_Tree(WN* wn_tree)
{ 
  switch (WN_operator(wn_tree)) { 
  case OPR_LABEL: 
    Remove_Label(wn_tree);
    break;
  case OPR_GOTO:
  case OPR_FALSEBR:
  case OPR_TRUEBR: 
    Remove_Target(wn_tree);
    break;
  } 

  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Remove_Tree(wn);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)
      Remove_Tree(WN_kid(wn_tree, i));
  } 
} 

