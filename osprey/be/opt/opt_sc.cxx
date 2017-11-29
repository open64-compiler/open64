/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.

#ifndef opt_sc_INCLUDED
#include "opt_sc.h"
#endif

using std::insert_iterator;

// Reset/clear fields.
void 
SC_NODE::Clear()
{
  type = SC_NONE;
  _id = 0;
  _class_id = 0;
  _depth = 0;
  pool = NULL;
  u1.bb_rep = NULL;
  u1.bbs = NULL;
  parent = NULL;
  kids = NULL;
  _flag = 0;
  next = NULL;
}

// Unmask given value from this SC_NODE's flag.
// See SC_NODE_FLAG for values of bitmask.
void
SC_NODE::Remove_flag(int bitmask)
{
  if (Has_flag(bitmask))
    _flag -= bitmask;
}

// Append given sc as this SC_NODE's last kid.
void 
SC_NODE::Append_kid(SC_NODE *sc)
{
  FmtAssert(this->Type() != SC_BLOCK, ("Unexpect kid for SC_BLOCK"));

  if (kids == NULL)
    kids = (SC_LIST*)CXX_NEW(SC_LIST(sc), pool);
  else {
    FmtAssert(!kids->Contains(sc), ("Repeated kids"));
    kids = kids->Append(sc,pool);
  }
}

// Prepend given sc as this SC_NODE's first kid.
void
SC_NODE::Prepend_kid(SC_NODE *sc)
{
  FmtAssert(this->Type() != SC_BLOCK, ("Unexpect kid for SC_BLOCK"));

  if (kids == NULL)
    kids = (SC_LIST*)CXX_NEW(SC_LIST(sc), pool);
  else {
    FmtAssert(!kids->Contains(sc), ("Repeated kids"));
    kids = kids->Prepend(sc,pool);
  }
}

// Insert given node before this node.
void
SC_NODE::Insert_before(SC_NODE * sc)
{
  SC_NODE * sc_parent = this->Parent();
  SC_NODE * sc_prev = this->Prev_sibling();

  sc->Set_parent(sc_parent);

  if (sc_prev == NULL)
    sc_parent->Prepend_kid(sc);
  else {
    SC_LIST * sc_list = sc_parent->Kids();
    SC_LIST_ITER sc_list_iter;
    SC_NODE * sc_tmp;
    sc_parent->Set_kids(NULL);

    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_list)) {
      sc_parent->Append_kid(sc_tmp);
      if (sc_tmp == sc_prev)
	sc_parent->Append_kid(sc);
    }

    while (sc_list) {
      sc_tmp = sc_list->Node();
      sc_list = sc_list->Remove(sc_tmp, pool);
    }
  }
}

// Insert given node after this node.
void
SC_NODE::Insert_after(SC_NODE * sc)
{
  SC_NODE * sc_parent = this->Parent();
  
  sc->Set_parent(sc_parent);

  SC_LIST * sc_list = sc_parent->Kids();
  SC_LIST_ITER sc_list_iter;
  SC_NODE * sc_tmp;
  sc_parent->Set_kids(NULL);

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(sc_list)) {
    sc_parent->Append_kid(sc_tmp);
    if (sc_tmp == this)
      sc_parent->Append_kid(sc);
  }

  while (sc_list) {
    sc_tmp = sc_list->Node();
    sc_list = sc_list->Remove(sc_tmp, pool);
  }
}

// Remove given SC_NODE from this SC_NODE's kids.
void SC_NODE::Remove_kid(SC_NODE *sc)
{
  if (kids != NULL)
    kids = kids->Remove(sc, pool);
}

// Obtain last kid of this SC_NODE.
SC_NODE *
SC_NODE::Last_kid()
{
  if (kids == NULL)
    return NULL;

  return kids->Last_elem();
}

// Obtain last non-empty kid.
SC_NODE *
SC_NODE::Last_non_empty_kid()
{
  SC_NODE * sc_iter = Last_kid();
  while (sc_iter) {
    if (!sc_iter->Is_empty())
      return sc_iter;
    sc_iter = sc_iter->Prev_sibling();
  }
  return NULL;
}

// Obtain first non-empty kid.
SC_NODE *
SC_NODE::First_non_empty_kid()
{
  SC_NODE * sc_iter = First_kid();
  while (sc_iter) {
    if (!sc_iter->Is_empty())
      return sc_iter;
    sc_iter = sc_iter->Next_sibling();
  }
  return NULL;
}

// Find the last kid whose type is 'm_type'.
SC_NODE *
SC_NODE::Last_kid_of_type(SC_TYPE m_type)
{
  SC_NODE * sc_iter = Last_kid();
  while (sc_iter) {
    if (sc_iter->Type() == m_type)
      return sc_iter;
    sc_iter = sc_iter->Prev_sibling();
  }
  return NULL;
}

// Unlink this SC_NODE from the SC tree.
void
SC_NODE::Unlink()
{
  parent->Remove_kid(this);
  this->Set_parent(NULL);
}

// Convert type to new_type 
void
SC_NODE::Convert(SC_TYPE new_type)
{
  SC_TYPE old_type = type;

  if (old_type == new_type)
    return;

  if (SC_type_has_bbs(old_type) && SC_type_has_rep(new_type)) {
    BB_LIST * bb_list = Get_bbs();
    FmtAssert(((bb_list != NULL) && !bb_list->Multiple_bbs()), 
	      ("Expect a single block"));
    BB_NODE * bb = bb_list->Node();
    bb_list->Remove(bb, pool);
    Set_bbs(NULL);
    type = new_type;
    Set_bb_rep(bb);
  }
  else if (SC_type_has_rep(old_type) && SC_type_has_bbs(new_type)) {
    BB_NODE * bb = Get_bb_rep();
    Set_bb_rep(NULL);
    type = new_type;
    Append_bbs(bb);
  }
  else
    FmtAssert(FALSE, ("TODO"));
}

// Obtain first kid of this SC_NODE.
SC_NODE *
SC_NODE::First_kid()
{
  if (kids == NULL)
    return NULL;
  
  return kids->First_elem();

}

// Find the first kid that contains an executable block.
SC_NODE *
SC_NODE:: First_executable_kid()
{
  SC_LIST_ITER iter;
  SC_NODE * tmp;
  FOR_ALL_ELEM(tmp, iter, Init(kids)) {
    if (tmp->First_executable_blk() != NULL)
      return tmp;
  }
  return NULL;
}

// Return next sibling SC_NODE from the same parent.

SC_NODE *
SC_NODE::Next_sibling()
{
  if (parent == NULL)
    return NULL;
  
  SC_LIST_ITER sc_list_iter(parent->Kids());
  SC_NODE * tmp = NULL;
  BOOL found = FALSE;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (found)
      return tmp;
    else if (tmp == this) {
      found = TRUE;
    }
  }

  return NULL;
}

// Return next executable sibling.
SC_NODE *
SC_NODE::Next_executable_sibling()
{
  SC_NODE * sc_iter = Next_sibling();
  while (sc_iter) {
    if (sc_iter->First_executable_blk() != NULL)
      return sc_iter;
    sc_iter = sc_iter->Next_sibling();
  }
  return NULL;
}

// Return next SC_NODE in the SC tree that immediately succeeds this SC_NODE in source order.
SC_NODE *
SC_NODE::Next_in_tree()
{
  SC_NODE * cur = this;

  while (cur) {
    if (cur->Next_sibling())
      return cur->Next_sibling();
    cur = cur->Parent();
  }

  return NULL;
}

// Get this node's 'lever'th nesting SC_IFs from its parent tree,
SC_NODE *
SC_NODE::Get_nesting_if(INT level)
{
  SC_NODE * sc_tmp = this->Parent();
  INT count = 0;
  while (sc_tmp) {
    if (sc_tmp->Type() == SC_IF) {
      count++;
      if (count == level)
	return sc_tmp;
    }
    sc_tmp = sc_tmp->Parent();
  }
  return NULL;
}

// Get this node's outermost nesting SC_IF that is bounded by sc_bound
SC_NODE *
SC_NODE::Get_nesting_if(SC_NODE * sc_bound)
{
  SC_NODE * sc_tmp = this->Parent();
  SC_NODE * ret_val = NULL;

  if (sc_bound->Is_pred_in_tree(this)) {
    while (sc_tmp && (sc_tmp != sc_bound)) {
      if (sc_tmp->Type() == SC_IF)
	ret_val = sc_tmp;
      sc_tmp = sc_tmp->Parent();
    }
  }

  return ret_val;
}

// Get closest nesting SC_IF and query whether this node is in an else-path.
std::pair<SC_NODE *, bool>
SC_NODE::Get_nesting_if()
{
  SC_NODE * tmp = this->Parent();
  BOOL is_else = FALSE;

  while (tmp) {
    switch (tmp->Type()) {
    case SC_ELSE:
      is_else = TRUE;
      break;
    case SC_THEN:
      is_else = FALSE;
      break;
    default:
	;
    }

    if (tmp->Type() == SC_IF)
      return std::pair<SC_NODE *, bool>(tmp, is_else);
    tmp = tmp->Parent();
  }
  return std::pair<SC_NODE *, bool> (NULL, is_else);
}

// Get outermost nesting SC_IF and number of nesting if-conditions.
std::pair<SC_NODE *, int>
SC_NODE::Get_outermost_nesting_if()
{
  std::pair<SC_NODE *, bool> p_ret = Get_nesting_if();
  SC_NODE * sc_if = p_ret.first;
  SC_NODE * sc_out = NULL;
  int count = 0;

  while (sc_if) {
    sc_out = sc_if;
    p_ret = sc_if->Get_nesting_if();
    sc_if = p_ret.first;
    count++;
  }

  return std::pair<SC_NODE *, int> (sc_out, count);
}

// Get outermost nesting SC_LOOP and number of nesting loops.
std::pair<SC_NODE *, int>
SC_NODE::Get_outermost_nesting_loop()
{
  SC_NODE * sc_tmp = Parent();
  SC_NODE * outer_loop = NULL;
  int nest_level = 0;
  while (sc_tmp) {
    if (sc_tmp->Type() == SC_LOOP) {
      nest_level++;
      outer_loop = sc_tmp;
    }
    sc_tmp = sc_tmp->Parent();
  }

  return std::pair<SC_NODE *, int> (outer_loop, nest_level);
}

// Return closest next sibling SC_NODE of the given type
SC_NODE *
SC_NODE::Next_sibling_of_type(SC_TYPE match_type)
{
  if (parent == NULL)
    return NULL;
  
  SC_LIST_ITER sc_list_iter(parent->Kids());
  SC_NODE * tmp = NULL;
  BOOL found = FALSE;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (found) {
      if (tmp->Type() == match_type)
	return tmp;
    }
    else if (tmp == this) {
      found = TRUE;
    }
  }

  return NULL;
}

// Return closest previous sibling SC_NODE of the given type.
SC_NODE *
SC_NODE::Prev_sibling_of_type(SC_TYPE match_type)
{
  SC_NODE * sc_iter = this->Prev_sibling();
  while (sc_iter) {
    if (sc_iter->Type() == match_type)
      return sc_iter;
    sc_iter = sc_iter->Prev_sibling();
  }
  return NULL;
}

// Find the first kid that matches the given type.
SC_NODE  *
SC_NODE::First_kid_of_type(SC_TYPE match_type)
{
  SC_LIST_ITER kids_iter;
  SC_NODE * tmp;

  FOR_ALL_ELEM(tmp, kids_iter, Init(kids)) {
    if (tmp->Type() == match_type)
      return tmp;
  }

  return NULL;
}

// Return previous sibling of this SC_NODE.
SC_NODE *
SC_NODE::Prev_sibling()
{
  if (parent == NULL)
    return NULL;

  SC_LIST_ITER sc_list_iter(parent->Kids());
  SC_NODE * tmp = NULL;
  SC_NODE * prev = NULL;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (tmp == this)
      return prev;

    prev = tmp;
  }
  
  return NULL;
}

// Find the first kid that matches the given type
SC_NODE * 
SC_NODE::Find_kid_of_type(SC_TYPE kid_type)
{
  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * tmp;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (tmp->Type() == kid_type)
      return tmp;
  }
  
  return NULL;
}

// Find the first node that matches the given type in this node's parent sub-tree.
SC_NODE *
SC_NODE::Find_parent_of_type(SC_TYPE parent_type)
{
  SC_NODE * sc_iter = Parent();
  while (sc_iter) {
    if (sc_iter->Type() == parent_type)
      return sc_iter;
    sc_iter = sc_iter->Parent();
  }
  return NULL;
}

// Find a node that matches the given type in this node's offspring sub-tree.
SC_NODE * 
SC_NODE::Find_offspring_of_type(SC_TYPE match_type)
{
  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      if (tmp->Type() == match_type)
	return tmp;
      else {
	SC_NODE * sc= tmp->Find_offspring_of_type(match_type);
	if (sc != NULL)
	  return sc;
      }
    }
  }
  return NULL;
}

// Obtain the first node on a then-path.
BB_NODE *
SC_NODE::Then()
{
  FmtAssert((this->Type() == SC_IF), ("Expect a SC_IF"));
  BB_NODE * head = this->Get_bb_rep();
  BB_IFINFO * ifinfo = head->Ifinfo();
  return ifinfo->Then();
}

// Obtain the first node on a else-path.
BB_NODE *
SC_NODE::Else()
{
  FmtAssert((this->Type() == SC_IF), ("Expect a SC_IF"));
  BB_NODE * head = this->Get_bb_rep();
  BB_IFINFO * ifinfo = head->Ifinfo();
  return ifinfo->Else();
}

// Obtain the first BB_NODE in source order for the SC tree rooted at this SC_NODE.
BB_NODE *
SC_NODE::First_bb()
{
  BB_NODE * bb_tmp = Get_bb_rep();

  if (bb_tmp)
    return bb_tmp;

  BB_LIST * bb_list = Get_bbs();
  if (bb_list)
    return bb_list->Node();

  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    bb_tmp = sc_tmp->First_bb();
    if (bb_tmp)
      return bb_tmp;
  }
  
  return NULL;
}

// Return the first real statement in this SC_NODE.
WN *
SC_NODE::First_executable_stmt()
{
  BB_NODE * bb_tmp = Get_bb_rep();
  WN * wn = NULL;

  if (bb_tmp) {
    wn = bb_tmp->First_executable_stmt();
    if (wn)
      return wn;
  }

  BB_LIST * bb_list = Get_bbs();
  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      wn = bb_tmp->First_executable_stmt();
      if (wn)
	return wn;
    }
  }

  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    wn = sc_tmp->First_executable_stmt();
    if (wn)
      return wn;
  }
  
  return NULL;
}

// Return the first real statement's containing block in this SC_NODE.

BB_NODE *
SC_NODE::First_executable_blk()
{
  BB_NODE * bb_tmp = Get_bb_rep();
  if (bb_tmp && (bb_tmp->First_executable_stmt() != NULL))
    return bb_tmp;

  BB_LIST * bb_list = Get_bbs();
  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      if (bb_tmp->First_executable_stmt() != NULL)
	return bb_tmp;
    }
  }

  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;
  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    bb_tmp = sc_tmp->First_executable_blk();
    if (bb_tmp)
      return bb_tmp;
  }

  return NULL;
}

// Walk upward in the ancestor sub-tree of this node and look for real nodes that
// are not boundary delimiters.
SC_NODE *
SC_NODE::Get_real_parent()
{
  SC_NODE * ret_val = NULL;

  if (parent) {
    SC_NODE * c_node = parent;
    SC_TYPE c_type = c_node->Type();

    while (ret_val == NULL) {
      switch (c_type) {
      case SC_THEN:
      case SC_ELSE:
      case SC_LP_START:
      case SC_LP_COND:
      case SC_LP_STEP:
      case SC_LP_BACKEDGE:
      case SC_LP_BODY:
	c_node = c_node->Parent();
	c_type = c_node->Type();
	break;
      default:
	ret_val = c_node;
      }
    }
  }

  return ret_val;
}

// Find the real parent that is 'level' above this node.
SC_NODE *
SC_NODE::Get_real_parent(int level)
{
  int l = 1;
  SC_NODE * parent = Get_real_parent();

  while (parent) {
    if (l == level)
      return parent;
    l++;
    parent = parent->Get_real_parent();
  }

  return NULL;
}

// Obtain the last BB_NODE in source order for the SC tree rooted at this SC_NODE.
BB_NODE *
SC_NODE::Last_bb()
{
  SC_NODE * last_kid = Last_kid();
  BB_NODE * last_bb = NULL;
  
  while (last_kid) {
    last_bb = last_kid->Last_bb();

    if (last_bb)
      return last_bb;

    last_kid = last_kid->Prev_sibling();
  }

  last_bb = Get_bb_rep();

  if (last_bb)
    return last_bb;

  BB_LIST * bb_list = Get_bbs();
  BB_LIST_ITER bb_list_iter(bb_list);
  BB_NODE * tmp;

  FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
    last_bb = tmp;
  }

  return last_bb;
}

// If this SC_NODE is a SC_LOOP, obtain loop info.
BB_LOOP * 
SC_NODE::Loopinfo()
{
  FmtAssert((type == SC_LOOP), ("Expect a SC_LOOP"));
  
  SC_LIST_ITER sc_list_iter;
  SC_NODE * tmp;
  SC_NODE * sc_cond = NULL;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init(kids)) {
    if (tmp->Type() == SC_LP_COND) {
      sc_cond = tmp;
      break;
    }
  }

  FmtAssert(sc_cond, ("Loop cond not found"));
  BB_NODE * bb_cond = NULL;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init(sc_cond->Kids())) {
    if (tmp->Type() == SC_BLOCK) {
      bb_cond = tmp->Get_bbs()->Node();
      break;
    }
  }

  FmtAssert(bb_cond, ("BB cond not found"));
  BB_LOOP * loopinfo = bb_cond->Loop();
  FmtAssert(loopinfo, ("Loop info not found"));
  return loopinfo;
}

// Obtain the merge block of a if-region or a loop-region.
BB_NODE *
SC_NODE::Merge()
{
  if (type == SC_IF) {
    BB_NODE * head = this->Get_bb_rep();
    BB_IFINFO * ifinfo = head->Ifinfo();
    return ifinfo->Merge();
  }
  else if (type == SC_LOOP) {
    BB_LOOP * loopinfo = Loopinfo();
    return loopinfo->Merge();
  }
  else
    return NULL;
}

// Set merge for this SC_NODE.
void
SC_NODE::Set_merge(BB_NODE * bb)
{
  if (type == SC_IF) 
    this->Head()->Ifinfo()->Set_merge(bb);
  else if (type == SC_LOOP)
    this->Loopinfo()->Set_merge(bb);
}

// Find an exit of this SC_NODE
BB_NODE *
SC_NODE::Exit()
{
  BB_NODE * exit = NULL;
  BB_NODE * merge = NULL;
  BB_NODE * tmp;
  BB_LIST_ITER bb_list_iter;

  switch (type) {
  case SC_LOOP:
    merge = Merge();
    FOR_ALL_ELEM(tmp, bb_list_iter, Init(merge->Pred())) {
      if (Contains(tmp)) {
	exit = tmp;
	break;
      }
    }
    break;
  default:
    FmtAssert(FALSE, ("TODO: find exit"));
  }

  return exit;
}

// Get loop index if this node is a SC_LOOP.
WN *
SC_NODE::Index()
{
  if (type == SC_LOOP) {
    BB_LOOP * loop_info = Loopinfo();
    return loop_info->Index();
  }

  return NULL;
}

// Obtain the head block of a if-region or a loop-region.
BB_NODE *
SC_NODE::Head()
{
  if (type == SC_IF) {
    return Get_bb_rep();
  }
  else if (type == SC_LOOP) {
    BB_NODE * bb = First_bb();
    FmtAssert(bb, ("First BB not found"));
    return bb;
  }
  else {
    FmtAssert(FALSE, ("Expect a SC_IF or a SC_LOOP"));
  }
  return NULL;
}

// Query whether the SC tree rooted at this SC_NODE contains bb.

BOOL
SC_NODE::Contains(BB_NODE * bb)
{
  BB_NODE * tmp = this->Get_bb_rep();
  if ((tmp != NULL) && (tmp == bb))
    return TRUE;

  BB_LIST * bb_list = this->Get_bbs();

  if (bb_list != NULL) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
      if (tmp == bb)
	return TRUE;
    }
  }

  SC_LIST * kids = this->Kids();

  if (kids != NULL) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE *sc_tmp;
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
      if (sc_tmp->Contains(bb))
	return TRUE;
    }
  }

  return FALSE;
}

// Obtain the last block of a then-path.
BB_NODE *
SC_NODE::Then_end()
{
  FmtAssert((this->Type() == SC_IF), ("Expect a SC_IF"));
  SC_NODE * sc_then = Find_kid_of_type(SC_THEN);

  BB_NODE * merge = this->Merge();
  BB_LIST_ITER bb_list_iter(merge->Pred());
  BB_NODE * tmp;

  FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
    if (sc_then->Contains(tmp))
      return tmp;
  }
  return NULL; 
}

// Obtain the last block of a else-path.
BB_NODE *
SC_NODE::Else_end()
{
  FmtAssert((this->Type() == SC_IF), ("Expect a SC_IF"));
  SC_NODE * sc_else = Find_kid_of_type(SC_ELSE);

  BB_NODE * merge = this->Merge();
  BB_LIST_ITER bb_list_iter(merge->Pred());
  BB_NODE * tmp;

  FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
    if (sc_else->Contains(tmp))
      return tmp;
  }
  return NULL;
}

// Delete SC tree rooted at this SC_NODE.
void
SC_NODE::Delete()
{
  if (kids) {
    SC_LIST_ITER sc_list_iter(kids);
    SC_NODE * tmp;

    FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
      tmp->Delete();
    }
  }

  if (SC_type_has_bbs(type)) {
    BB_LIST * bbs = Get_bbs();
    while (bbs) {
      BB_NODE * bb = bbs->Node();
      bbs = bbs->Remove(bb, pool);
    }
  }

  SC_LIST * cur;
  SC_LIST * next;

  for (cur = kids; cur; cur = next) {
    next = cur->Next();
    CXX_DELETE(cur, pool);
  }
  
  CXX_DELETE(this, pool);
}

// Query whether this SC_NODE is well-behaved.
// A well-behaved if-region has a head block, a then-path, a else-path
// and a merge block. 
// The Next() of the head block is the first block on the then-path.
// The Next() of the last block on the then-path is the first block on the else-path.
// The Next() of the last block on the else-path is the merge block.
// The last block on the then-path is a predecessor of the merge block.
// The last block on the else-path is a predecessor of the merge block.

BOOL
SC_NODE::Is_well_behaved()
{
  BB_NODE * bb_head = Get_bb_rep();
  BB_NODE * bb_then = Then();
  BB_NODE * bb_then_end = Then_end();
  BB_NODE * bb_else = Else();
  BB_NODE * bb_else_end = Else_end();
  BB_NODE * bb_merge = Merge();

  if (!bb_head || !bb_then || !bb_then_end 
      || !bb_else || !bb_else_end || !bb_merge)
    return FALSE;

  if (bb_head->Next() != bb_then)
    return FALSE;
  
  if (bb_then_end->Next() != bb_else)
    return FALSE;

  if (bb_else_end->Next() != bb_merge)
    return FALSE;

  if (!bb_then_end->Succ()
      || !bb_then_end->Succ()->Contains(bb_merge))
    return FALSE;

  if (!bb_else_end->Succ() 
      || !bb_else_end->Succ()->Contains(bb_merge))
    return FALSE;

  return TRUE;
}

// Query whether given BB_NODE is a member of the SC tree rooted at this SC_NODE.
BOOL
SC_NODE::Is_member(BB_NODE * bb)
{
  if (bb == this->Get_bb_rep())
    return TRUE;

  BB_LIST_ITER bb_list_iter(Get_bbs());
  BB_NODE * bb_tmp;

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
    if (bb_tmp == bb)
      return TRUE;
  }
  
  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;
  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    if (sc_tmp->Is_member(bb))
      return TRUE;
  }

  return FALSE;
}

// Query whether this SC_NODE has a single-entry and a single-exit.
// Return the single-entry and single-exit in the given parameters.
BOOL
SC_NODE::Is_sese()
{
  BOOL ret_val = FALSE;
  BB_NODE * bb_head;
  BB_NODE * bb_merge;
  BB_LIST_ITER bb_list_iter;
  BB_NODE * bb_tmp;
  BB_NODE * bb_first;
  BB_NODE * bb_last;

  switch (type) {
  case SC_BLOCK:
    ret_val = TRUE;
    bb_first = First_bb();
    bb_last = Last_bb();

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(Get_bbs())) {
      BB_LIST_ITER iter;
      BB_NODE * tmp;

      if (bb_tmp != bb_last) {
	// Do not allow exits except for the last block.
	if (bb_tmp->Succ()) {
	  FOR_ALL_ELEM(tmp, iter, Init(bb_tmp->Succ())) {
	    if (!Contains(tmp)) {
	      ret_val = FALSE;
	      break;
	    }
	  }
	}
      }
      if (!ret_val)
	break;

      if (bb_tmp != bb_first) {
	if (!bb_first->Dominates(bb_tmp) || !bb_tmp->Postdominates(bb_first)) {
	  ret_val = FALSE;
	  break;
	}
	else if (bb_tmp->Pred()) {
	  // Do not allow entrances except for the first block.
	  FOR_ALL_ELEM(tmp, iter, Init(bb_tmp->Pred())) {
	    if (!Contains(tmp)) {
	      ret_val = FALSE;
	      break;
	    }
	  }
	}
      }
      else {
	// For the first block, do not allow more than 1 entrances from outside 
	// unless it is a merge block of a SC_IF.
	BB_LIST * pred = bb_tmp->Pred();
	if (pred && (pred->Len() > 1)) {
	  int count = 0;
	  FOR_ALL_ELEM(tmp, iter, Init(pred)) {
	    if (!Contains(tmp))
	      count++;
	  }
	  if (count > 1) {
	    // Check whether bb_tmp is a SC_IF's merge block.
	    SC_NODE * sc_tmp = Prev_sibling();
	    if (!sc_tmp || (sc_tmp->Type() != SC_IF)
		|| (sc_tmp->Merge() != bb_tmp)
		|| (count != 2)) {
	      ret_val = FALSE;
	      break;
	    }
	  }
	}
      }

      if (!ret_val)
	break;
    }

    break;

  case SC_IF:
    if (Is_well_behaved()) {
      bb_head = Head();
      bb_merge = Merge();

      if (bb_head->Is_dom(this)
	  && bb_merge->Is_postdom(this))
	ret_val= TRUE;
    }
    break;

  case SC_LOOP:
    bb_head = Head();
    bb_merge = Merge();
    
    if (bb_head->Is_dom(this)
	&& bb_merge->Is_postdom(this)) {
      BB_LIST * pred = bb_merge->Pred();

      if (pred->Len() == 1) {
	BB_NODE * tmp = pred->Node();
	if (this->Is_member(tmp))
	  ret_val = TRUE;
      }
    }

    break;

  case SC_LP_BODY:
    bb_first = this->First_bb();
    bb_tmp = this->Last_bb();

    if (bb_first->Is_dom(this)
	&& bb_tmp->Is_postdom(this))
      ret_val = TRUE;

    break;
    
  default:
    ;
  }

  return ret_val;
}

// Query whether this SC_NODE is a predessor of sc in the SC tree.

BOOL
SC_NODE::Is_pred_in_tree(SC_NODE * sc)
{
  SC_NODE * p_sc = sc->Parent();

  while (p_sc) {
    if (this == p_sc)
      return TRUE;
    p_sc = p_sc->Parent();
  }

  return FALSE;
}

// Find least common predecessor of this SC_NODE and sc in the SC tree.

SC_NODE *
SC_NODE::Find_lcp(SC_NODE * sc)
{
  if ((this->Parent() == NULL)
      || (sc->Parent() == NULL))
    return NULL;

  if (Is_pred_in_tree(sc))
    return this;
  else if (sc->Is_pred_in_tree(this))
    return sc;
  else {
    SC_NODE * p_sc = parent;
    
    while (p_sc) {
      if (p_sc->Is_pred_in_tree(sc))
	return p_sc;
      p_sc = p_sc->Parent();
    }
  }

  FmtAssert(FALSE, ("LCP not found"));
  return NULL;
}

// For every pair of WHILR nodes in the WHIRL tree rooted
// at wn1 and wn2, check whether operators are identical.
// If the node is a constant, check whether constant value
// is identical.

static BOOL
Has_same_shape(WN * wn1, WN * wn2)
{
  if (WN_operator(wn1) != WN_operator(wn2))
    return FALSE;

  switch (WN_operator(wn1)) {
  case OPR_INTCONST:
    if (WN_const_val(wn1) != WN_const_val(wn2))
      return FALSE;

    break;

  default:
    ;
  }

  if (WN_kid_count(wn1) != WN_kid_count(wn2))
    return FALSE;

  for (int i = 0; i < WN_kid_count(wn1); i++) {
    if (!Has_same_shape(WN_kid(wn1,i), WN_kid(wn2,i)))
      return FALSE;
  }

  return TRUE;
}

// Query whether this SC_NODE has the same loop structure as sc
BOOL
SC_NODE::Has_same_loop_struct(SC_NODE * sc)
{
  if ((type != SC_LOOP) || (type != sc->Type()))
    return FALSE;

  if (kids->Len() != sc->Kids()->Len())
    return FALSE;

  SC_NODE * sc1 = First_kid();
  SC_NODE * sc2 = sc->First_kid();
  BB_NODE * bb1;
  BB_NODE * bb2;
  WN * wn1;
  WN * wn2;
  
  while (sc1) {
    if (sc1->Type() != sc2->Type())
      return FALSE;

    bb1 = sc1->First_bb();
    bb2 = sc2->First_bb();

    switch (sc1->Type()) {
    case SC_LP_START:
    case SC_LP_COND:
    case SC_LP_STEP:

      wn1 = bb1->Laststmt();
      wn2 = bb2->Laststmt();
      
      if (WN_operator(wn1) == OPR_GOTO)
	wn1 = WN_prev(wn1);

      if (WN_operator(wn2) == OPR_GOTO)
	wn2 = WN_prev(wn2);

      if (!wn1 || !wn2 || !Has_same_shape(wn1, wn2))
	return FALSE;
      
    default:
      ;
    }

    sc1 = sc1->Next_sibling();
    sc2 = sc2->Next_sibling();
  }

  return TRUE;
}

// Query this SC_NODE and the sc have symmetric path.
// Find LCP, for every pair of noded on the path from LCP to this SC_NODE, and 
// on the path from LCP to the sc, the following condition must be satisfied:
// - Same type, and the type must be {SC_IF, SC_LOOP, SC_THEN, SC_ELSE}.
// - If the type is a SC_IF, condition expression should have the same shape.
// - If the type is a SC_LOOP, loop structure should be the same
// - Two pathes have the same length.
//
// If "check_buddy" is TRUE, 
// - allow type mismatch at lcp's immediate children under the condition that the lcp is a SC_IF.
// - disallow SC_LOOP on the path.
BOOL
SC_NODE::Has_symmetric_path(SC_NODE * sc, BOOL check_buddy)
{
  SC_NODE * sc1 = this;
  SC_NODE * sc2 = sc;
  SC_NODE * lcp = Find_lcp(sc);

  if (!lcp)
    return FALSE;

  while (sc1 && sc2) {
    if ((sc1 == lcp) && (sc2 != lcp))
      return FALSE;
    else if ((sc1 != lcp) && (sc2 == lcp))
      return FALSE;
    else if ((sc1 == lcp) && (sc2 == lcp))
      return TRUE;
    else {
      SC_TYPE type1 = sc1->Type();
      SC_TYPE type2 = sc2->Type();

      if (type1 != type2) {
	if (!check_buddy || (sc1->Parent() != lcp)
	    || (lcp->Type() != SC_IF))
	  return FALSE;
      }
      else if (check_buddy && (sc != sc2)
	       && (type1 == SC_LOOP))
	return FALSE;
      
      if ((type1 != SC_IF) && (type1 != SC_LOOP)
	  && (type1 != SC_THEN) && (type1 != SC_ELSE))
	return FALSE;
      
      if (type1 == SC_IF) {
	BB_NODE * bb1 = sc1->Get_bb_rep();
	BB_NODE * bb2 = sc2->Get_bb_rep();

	if (!bb1->Compare_Trees(bb2))
	  return FALSE;
      }

      if ((type1 == SC_LOOP)
	  && !sc1->Has_same_loop_struct(sc2))
	return FALSE;
    }

    sc1 = sc1->Parent();
    sc2 = sc2->Parent();
  }

  return FALSE;
}

// If this node and 'sc' have LCP, for every pair of nodes on the path from LCP to this node,
// and on the path from LCP to 'sc', check 
// - 1. The paths have the same length.
// - 2. The pair has the same type, and the type must be of {SC_IF, SC_THEN, SC_ELSE}.
// - 3. If the type is a SC_IF, check whether condition expressions are identical and encode the
//      result using a bit mask, one bit per SC_IF node on the path. Use '0' for identical
//      condition expressions and '1' for different condition expressions.  
// Return 0 if (1) or (2) are not satisfied. Otherwise return encoded result in (3).

UINT32 SC_NODE::Encode_path(SC_NODE * sc)
{
  SC_NODE * lcp = Find_lcp(sc);
  UINT32 val = 0;
  UINT32 count = 0;

  if (!lcp)
    return val;

  SC_NODE * sc_iter1 = this->Parent();
  SC_NODE * sc_iter2 = sc->Parent();

  while (sc_iter1 && sc_iter2) {
    BOOL end1 = (sc_iter1 == lcp) ? TRUE : FALSE;
    BOOL end2 = (sc_iter2 == lcp) ? TRUE : FALSE;

    if (end1 != end2)
      return 0;
    else if (end1)
      return val;

    if (count >= sizeof(UINT32) * 8)
      return 0;
    
    SC_TYPE type = sc_iter1->Type();
    if ((type != sc_iter2->Type())
	|| ((type != SC_THEN) && (type != SC_ELSE) && (type != SC_IF)))
      return 0;

    if (type == SC_IF) {
      WN * wn1 = sc_iter1->Get_cond();
      WN * wn2 = sc_iter2->Get_cond();
      if (!wn1 || !wn2)
	return 0;
      else if (WN_Simp_Compare_Trees(wn1, wn2) != 0) 
	val |= (1 << count);
      count++;
    }

    sc_iter1 = sc_iter1->Parent();
    sc_iter2 = sc_iter2->Parent();
  }

  return 0;
}

// Count number of loops on the path from this SC_NODE to given sc_root.
// this_is_exc indicates whether to exclude this SC_NODE.
// root_is_exc indicates whether to exclude sc_root.
int 
SC_NODE::Num_of_loops(SC_NODE * sc_root, BOOL this_is_exc, BOOL root_is_exc)
{
  FmtAssert(sc_root->Is_pred_in_tree(this), ("Expect a pred in the SC tree"));
  int count = 0;
  SC_NODE * sc_node;

  if (this_is_exc)
    sc_node = this->Parent();
  else
    sc_node = this;

  while (sc_node) {
    if (sc_node == sc_root) {
      if (root_is_exc)
	break;
    }
    
    if (sc_node->Type() == SC_LOOP)
      count++;

    if (sc_node == sc_root)
      break;

    sc_node = sc_node->Parent();
  }

  return count;
}

// Count number of statements for all BB_NODEs in the SC tree rooted at this SC_NODE.
int
SC_NODE::Executable_stmt_count()
{
  int count = 0;
  BB_NODE * bb = Get_bb_rep();
  
  if (bb)
    count += bb->Executable_stmt_count();

  BB_LIST * bb_list = Get_bbs();
  BB_LIST_ITER bb_list_iter(bb_list);
  BB_NODE * bb_tmp;

  FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
    count += (bb_tmp->Executable_stmt_count());
  }

  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    count += (sc_tmp->Executable_stmt_count());
  }

  return count;
}

// Query whether there exists a SC_LOOP in the SC-tree rooted at this SC_NODE.
BOOL
SC_NODE::Has_loop()
{
  if (type == SC_LOOP)
    return TRUE;
  
  SC_LIST_ITER sc_list_iter(kids);
  SC_NODE * sc_tmp;
  
  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init()) {
    if ((sc_tmp->Type() == SC_LOOP)
	|| sc_tmp->Has_loop())
      return TRUE;
  }

  return FALSE;
}

// Query whether this SC_NODE contains empty blocks.
BOOL
SC_NODE::Is_empty_block()
{
  if ((type != SC_BLOCK) || (Executable_stmt_count() > 0))
    return FALSE;

  BB_NODE * tmp;
  BB_LIST_ITER bb_list_iter(Get_bbs());

  FOR_ALL_ELEM(tmp, bb_list_iter, Init()) {
    if (tmp->Kind() != BB_GOTO)
      return FALSE;
  }

  return TRUE;
}

// Query whether the SC tree rooted at this node contain empty blocks.
BOOL
SC_NODE::Is_empty()
{
  switch (type) {
  case SC_THEN:
  case SC_ELSE:
  case SC_LP_START:
  case SC_LP_COND:
  case SC_LP_STEP:
  case SC_LP_BODY:
  case SC_LP_BACKEDGE:
    break;
  case SC_BLOCK:
    return Is_empty_block();
  default:
    return FALSE;
  }

  SC_LIST_ITER sc_list_iter;
  SC_NODE * sc_tmp;

  FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(kids)) {
    if (!sc_tmp->Is_empty())
      return FALSE;
  }

  return TRUE;
}

// Query whether this SC_NODE executes the same statements as 'sc'.
BOOL
SC_NODE::Is_same(SC_NODE * sc)
{
  FmtAssert((sc != this), ("Expect different SC_NODE."));

  if ((type != sc->Type())
      || (kids->Len() != sc->Kids()->Len()))
    return FALSE;

  BB_NODE * bb1 = Get_bb_rep();
  BB_NODE * bb2 = sc->Get_bb_rep();
  
  if (bb1 && bb2) {
    if (!bb1->Compare_Trees(bb2))
      return FALSE;
  }

  BB_LIST * bb_list1 = Get_bbs();
  BB_LIST * bb_list2 = sc->Get_bbs();

  if (bb_list1 && bb_list2) {
    STACK<BB_NODE *> * stk1 = CXX_NEW(STACK<BB_NODE *> (pool), pool);
    STACK<BB_NODE *> * stk2 = CXX_NEW(STACK<BB_NODE *> (pool), pool);
    BB_LIST_ITER bb_list_iter;
    BB_NODE * bb_tmp;

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_list1)) {
      if (bb_tmp->Executable_stmt_count() > 0)
	stk1->Push(bb_tmp);
    }

    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init(bb_list2)) {
      if (bb_tmp->Executable_stmt_count() > 0)
	stk2->Push(bb_tmp);
    }
    
    BOOL is_same = TRUE;
    if (stk1->Elements() != stk2->Elements()) 
      is_same = FALSE;
    else {
      for (int i = 0; i < stk1->Elements(); i++) {
	bb1 = stk1->Bottom_nth(i);
	bb2 = stk2->Bottom_nth(i);
	if (!bb1->Compare_Trees(bb2)) {
	  is_same = FALSE;
	  break;
	}
      }
    }

    CXX_DELETE(stk1, pool);
    CXX_DELETE(stk2, pool);

    if (!is_same)
      return FALSE;
  }
    
  SC_NODE * sc1 = First_kid();
  SC_NODE * sc2= sc->First_kid();

  while (sc1) {
    if (!sc1->Is_same(sc2))
      return FALSE;
    sc1 = sc1->Next_sibling();
    sc2 = sc2->Next_sibling();
  }

  return TRUE;
}

// Obtain condition-setting WN if this SC_NODE is a SC_IF.
WN *
SC_NODE::Get_cond()
{
  if (type == SC_IF) {
    BB_NODE * bb = u1.bb_rep;
    WN * last_wn = bb->Laststmt();
    OPERATOR opr = WN_operator(last_wn);
    
    if ((opr == OPR_FALSEBR) || (opr == OPR_TRUEBR)) {
      return WN_kid0(last_wn);
    }
  }

  return NULL;

}

// Get the last element in the list linked by the 'next' field.
SC_NODE *
SC_NODE::Last() {
  SC_NODE * sc_iter = this;
  SC_NODE * sc_last = NULL;

  while (sc_iter) {
    sc_last = sc_iter;
    sc_iter = sc_iter->Next();
  }
  return sc_last;
}

// Find init, end-test and step statement for this SC_LOOP node.
BOOL
SC_NODE::Get_bounds(WN ** p_start, WN ** p_end, WN ** p_step)
{
  FmtAssert(p_start && p_end && p_step, ("Unexpected NULL pointers."));
  *p_start = NULL;
  *p_end = NULL;
  *p_step = NULL;

  SC_NODE * sc = this;
  FmtAssert(sc->Type() == SC_LOOP, ("Expect a SC_LOOP."));

  SC_NODE * sc_start = sc->Find_kid_of_type(SC_LP_START);
  SC_NODE * sc_end = sc->Find_kid_of_type(SC_LP_COND);
  SC_NODE * sc_step = sc->Find_kid_of_type(SC_LP_STEP);
  
  if (!sc_start || !sc_end || !sc_step)
    return FALSE;
  
  BB_NODE * bb_start = sc_start->First_bb();
  BB_NODE * bb_end = sc_end->First_bb();
  BB_NODE * bb_step = sc_step->First_bb();

  if (!bb_start || !bb_end || !bb_step)
    return FALSE;

  WN * wn_start = bb_start->Laststmt();  
  WN * wn_end = bb_end->Laststmt();
  WN * wn_step = bb_step->Laststmt();

  if (wn_step && (WN_operator(wn_step) == OPR_GOTO))
    wn_step = WN_prev(wn_step);

  if (!wn_start || !wn_end || !wn_step)
    return FALSE;

  *p_start = wn_start;
  *p_end = wn_end;
  *p_step = wn_step;

  return TRUE;
}

// Dump a SC_NODE.  If dump_tree is TRUE, dump the SC tree
// rooetd at this SC_NODE.
void 
SC_NODE::Print(FILE *fp, BOOL dump_tree) const
{
  fprintf(fp, "\n--- SC:%d %s ---\n", _id, this->Type_name());

  if (SC_type_has_rep(type)) {
    BB_NODE * bb = Get_bb_rep();
    if (bb) 
      fprintf(fp, " rep BB:%d", bb->Id());
  }
  else if (SC_type_has_bbs(type)) {
    BB_LIST  * bbs = Get_bbs();
    if (bbs) {
      fprintf(fp, " component BBs:");
      bbs->Print(fp);
    }
  }

  if (parent)
    fprintf(fp, " parent:%d", parent->Id());

  if (kids) {
    fprintf(fp, " kids:");
    kids->Print(fp);
    
    if (dump_tree) {
      SC_LIST_ITER sc_list_iter(kids);
      SC_NODE *tmp = NULL;

      FOR_ALL_ELEM(tmp, sc_list_iter, Init()) 
	tmp->Print(fp, TRUE);
    }
  }

  fprintf(fp, "\n");
}

SC_LIST*
SC_LIST::Append(SC_NODE *sc, MEM_POOL *pool)
{
  SLIST sc_list_container(this);
  SC_LIST *new_sclst = (SC_LIST*)CXX_NEW(SC_LIST(sc), pool);
  if (new_sclst == NULL) ErrMsg ( EC_No_Mem, "SC_LIST::Append" );
  sc_list_container.Append(new_sclst);
  return (SC_LIST*)sc_list_container.Head();
}

SC_LIST *
SC_LIST::Remove(SC_NODE *sc, MEM_POOL *pool)
{
  SC_LIST *prev, *cur, *retval = this;
  
  if (sc == NULL) return this;

  for (prev=NULL,cur=this; cur && cur->node != sc; cur = cur->Next()) {
    prev = cur;
  }

  if (cur == NULL)
    return this;

  if (cur == this)
    retval = Next();

  cur->SLIST_NODE::Remove(prev);
  CXX_DELETE(cur, pool);
  return retval;
}

BOOL
SC_LIST::Contains(SC_NODE *sc) const
{
  SC_LIST_ITER sc_list_iter(this);
  SC_NODE *tmp;
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (tmp == sc)
      return TRUE;
  }
  return FALSE;
}

SC_NODE *
SC_LIST::Last_elem()
{
  SC_LIST_ITER sc_list_iter(this);
  SC_NODE *tmp = NULL;
  SC_NODE * last = NULL;
  
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    last = tmp;
  }

  return last;
}

SC_NODE *
SC_LIST::First_elem()
{
  SC_LIST_ITER sc_list_iter(this);
  SC_NODE *tmp = NULL;

  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    return tmp;
  }

  return NULL;
}

void
SC_LIST::Print(FILE *fp) const
{
  SC_LIST_ITER sc_list_iter(this);
  SC_NODE * tmp;
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (tmp) {
      fprintf(fp, "%d(%s) ",tmp->Id(), tmp->Type_name_abbr());
    }
  }
  fprintf(fp, "\n ");
}

void
SC_LIST_ITER::Validate_unique(FILE *fp)
{
  for (First(); !Is_Empty(); Next()) {
    SC_NODE *tmp = Cur()->Node();
    if (tmp == NULL) {
      fprintf(fp, "Empty Node in the sc_list!!!\n");
      break;
    }
    if (Peek_Next()) {
      if (Peek_Next()->Contains(tmp)) {
	fprintf(fp, "The sc_list has redundant sc_node");
	this->Head()->Print(fp);
      }
    }
  }  
}

void
SC_LIST_CONTAINER::Append(SC_NODE *sc, MEM_POOL *pool)
{
  SC_LIST * new_sclst = (SC_LIST*)CXX_NEW(SC_LIST(sc), pool);
  if (new_sclst == NULL) ErrMsg ( EC_No_Mem, "SC_LIST::Append" );
  Append(new_sclst);
}


void 
SC_LIST_CONTAINER::Prepend(SC_NODE *sc, MEM_POOL *pool)
{
  SC_LIST *new_sclst = (SC_LIST*)CXX_NEW( SC_LIST(sc), pool );
  if ( new_sclst == NULL ) ErrMsg ( EC_No_Mem, "SC_LIST::Prepend" );
  Prepend(new_sclst);
}

void
SC_LIST_CONTAINER::Remove  (SC_NODE *sc, MEM_POOL *pool)
{
  Warn_todo("SC_LIST_CONTAINER::Remove: remove this call");
  SC_LIST *prev, *cur;

  if (sc == NULL) return;
  for (prev=NULL,cur=Head(); cur && cur->Node() != sc; cur = cur->Next())
    prev = cur;

  CXX_DELETE(cur->Remove(prev), pool);
}

SC_NODE *
SC_LIST_CONTAINER::Remove_head(MEM_POOL *pool)
{
  Warn_todo("SC_LIST_CONTAINER::Remove_head: remove this call");
  SC_NODE *sc;
  SC_LIST *head;

  head = Head();
  if (head == NULL)
    return NULL;
  sc = head->Node();
  CXX_DELETE(Remove_Headnode(), pool);
  return sc;
}

BOOL
SC_LIST_CONTAINER::Contains(SC_NODE *sc) const
{
  SC_LIST_ITER sc_list_iter(this);
  SC_NODE* tmp;
  FOR_ALL_ELEM(tmp, sc_list_iter, Init()) {
    if (tmp == sc)
      return TRUE;
  }
  return FALSE;
}

// Find a pair of proactive loop fusion candidates for the SC_tree rooted at sc_root.
// a non-NULL sc_begin gives the initial search point in _loop_list.
// This routine can only be called from PRO_LOOP_FUSION_TRANS::Top_down_trans.
// In the SC tree between this node and 'sc', find a SC_ELSE or a SC_THEN whose parent is
// 'dist' away from 'sc'.
SC_NODE *
SC_NODE::Get_node_at_dist(SC_NODE * sc, int dist)
{
  if (dist > 0) {
    SC_NODE * sc_iter = parent;

    while (sc_iter && (sc_iter != sc)) {
      if ((sc_iter->Type() == SC_THEN) || (sc_iter->Type() == SC_ELSE)) {
	SC_NODE * sc_if = sc_iter->Parent();
	if (sc_if->Get_real_parent(dist) == sc)
	  return sc_iter;
      }
      sc_iter = sc_iter->Parent();
    }
  }
  return NULL;
}

// Query whether this node is control equivalent to 'sc' and
// comes before 'sc' in source order.
BOOL
SC_NODE::Is_ctrl_equiv(SC_NODE * sc)
{
  SC_NODE * sc1 = this;
  SC_NODE * sc2 = sc;

  if (sc1->Parent() != sc2->Parent())
    return FALSE;

  SC_NODE * sc_iter = sc1;
  BOOL in_order = FALSE;
  
  while (sc_iter) {
    if (!sc_iter->Is_sese())
      return FALSE;

    SC_NODE * sc_next = sc_iter->Next_sibling();

    if (sc_iter == sc2) {
      in_order = TRUE;
      break;
    }
    
    if (sc_next) {
      BB_NODE * bb_first = sc_next->First_bb();
      if (!bb_first->Is_postdom(sc_iter))
	return FALSE;
    }

    sc_iter = sc_next;
  }

  return in_order;
}

// Compare this SC_NODE to 'sc', return TRUE if they are identical, return FALSE otherwise.
BOOL
SC_NODE::Compare_Trees(SC_NODE * sc)
{
  if (this == sc)
    return TRUE;

  if (Type() != sc->Type())
    return FALSE;
  
  BB_NODE * bb_tmp1 = Get_bb_rep();
  BB_NODE * bb_tmp2 = sc->Get_bb_rep();

  if (bb_tmp1 && bb_tmp2 && 
      !bb_tmp1->Compare_Trees(bb_tmp2))
    return FALSE;
  
  BB_LIST * bb_list1 = Get_bbs();
  BB_LIST * bb_list2 = sc->Get_bbs();

  if (bb_list1 && bb_list2) {
    BB_LIST_ITER bb_iter1(bb_list1);
    BB_LIST_ITER bb_iter2(bb_list2);
    bb_tmp1 = bb_iter1.First_elem();
    bb_tmp2 = bb_iter2.First_elem();
    
    while ((bb_tmp1 != NULL) 
	   && (bb_tmp2 != NULL)) {
      if (!bb_tmp1->Compare_Trees(bb_tmp2))
	return FALSE;

      bb_tmp1 =(!bb_iter1.Is_Empty()) ? bb_iter1.Next_elem() : NULL;
      bb_tmp2 =(!bb_iter2.Is_Empty()) ? bb_iter2.Next_elem() : NULL;
    }
  }

  SC_NODE * tmp1;
  SC_NODE * tmp2;
  tmp1 = First_executable_kid();
  tmp2 = sc->First_executable_kid();

  while (tmp1 && tmp2) {
    if (!tmp1->Compare_Trees(tmp2))
      return FALSE;
    tmp1 = tmp1->Next_executable_sibling();
    tmp2 = tmp2->Next_executable_sibling();
  }

  return ((tmp1 == NULL) && (tmp2 == NULL));
}

// Query whether the SC tree rooted at this node is clonable. 
BOOL
SC_NODE::Clonable(BOOL seen_loop)
{
  BB_NODE * bb_tmp = Get_bb_rep();
  BB_LIST * bb_list = Get_bbs();

  if ((type == SC_BLOCK) 
      || (type == SC_IF)
      || (type == SC_LOOP)) {
    if (!Is_sese())
      return FALSE;
  
    // Do not allow cloning of nested loops or loops that are
    // not "LOOP_PRE_DO".
    if (type == SC_LOOP) {
      if  (!Loopinfo()->Is_flag_set(LOOP_PRE_DO)
	   || seen_loop)
	return FALSE;
    }
    else if (type == SC_BLOCK) {
      // Do not allow cloning of SC_BLOCKs having branches.
      BB_LIST_ITER bb_list_iter(bb_list);
      FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
	if (bb_tmp->Branch_wn())
	  return FALSE;
      }
    }
  }
  else if (type == SC_COMPGOTO)
    return FALSE;
  
  if (bb_tmp && !bb_tmp->Clonable(TRUE, NULL, FALSE)) {
    return FALSE;
  }

  if (bb_list) {
    BB_LIST_ITER bb_list_iter(bb_list);
    FOR_ALL_ELEM(bb_tmp, bb_list_iter, Init()) {
      if (!bb_tmp->Clonable(TRUE, NULL, FALSE)) {
	return FALSE;
      }
    }
  }

  if (type == SC_LOOP) {
    SC_NODE * sc_body = Find_kid_of_type(SC_LP_BODY);
    if (!sc_body || !sc_body->Clonable(TRUE))
      return FALSE;
  }
  else if (kids) {
    SC_LIST_ITER sc_list_iter;
    SC_NODE * sc_tmp;
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(kids)) {
      if (!sc_tmp->Clonable(seen_loop))
	return FALSE;
    }
  }

  return TRUE;
}

// Check whether all kids of this node are clonable.
// Exclude 'sc' if it is not NULL.
BOOL
SC_NODE::All_kids_clonable(SC_NODE * sc)
{
  if (kids) {
    SC_LIST_ITER sc_list_iter;
    SC_NODE * sc_tmp;
    FOR_ALL_ELEM(sc_tmp, sc_list_iter, Init(kids)) {
      if (sc_tmp == sc)
	continue;
      if (!sc_tmp->Clonable(FALSE))
	return FALSE;
    }
  }
  return TRUE;
}

