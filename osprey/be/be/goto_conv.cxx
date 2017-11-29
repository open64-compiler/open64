/*
 * Copyright 2007 PathScale, LLC.  All Rights Reserved.
 */
/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright (C) 2001-2004 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
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


//-*-c++-*-
//                     TCF
//                     ---
//
// Description:
//
//	Convert gotos into WHILEs and IFs.  This is loosely based on
//  the paper "Taming Control Flow: A Structured Approach to Eliminating
//  Goto Statements" by Ana Erosa and Laurie Hendren.  As a first cut,
//  and probably as a last cut, we limit ourselves to removing sibling
//  gotos and gotos going out of an 'IF'.  
//  Our requirements are different than Erosa's.  We don't need
//  to get rid of every goto.  For most of the other gotos, although 
//  not all, we don't benefit by getting rid of them.  Limiting ourselves
//  simplifies the algorithm and prevents bloat (Granted Erosa claims the 
//  bloat is small).
//
//  Additional limitations. We limit the number of gotos out of IFs that we
//  remove to MAX_GOTO_EXPANDING_TRANSFORMATIONS = 20.  This prevents code
//  bloat in the bad cases.  We don't touch gotos to "break" labels.  These
//  transformations unnecessarily mess up case statements.
//  We don't create IFs out of unconditional gotos.  The optimizer promises to
//  erase these gotos.
//
//      WHIRL doesn't allow gotos into structured control flow.  Goto
//  conversion creates such gotos.  So TCF also dismantles such control
//  flow nodes (whether or not TCF created them)
//
//  *** The last statement above is no longer true. This pass of TCF
//  only dismantles invalid control flow nodes that it itself creates. ***
//
//

/* ====================================================================
 * ====================================================================
 *
 * Description: Build up do loops and ifs
 *
 * Code taken from be/com/opt_goto.{cxx,h}
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "goto_conv.h"
#include "wn_util.h"
#include "config_targ.h"
#include "strtab.h"
#include "stab.h"
#include "wn_lower.h"
#include "targ_sim.h"
#include "fb_whirl.h"


// Utility function to check for structured-control-flow nodes that
// we are interested in.
static inline BOOL opcode_is_scf (OPCODE opcode)
{
  return (/*(opcode == OPC_DO_LOOP) ||*/ (opcode == OPC_DO_WHILE) ||
            (opcode == OPC_WHILE_DO) /*|| (opcode == OPC_IF)*/);
}

// Build up a table of goto descriptors
// Go through the code, push each goto onto the stack
// Use a hash table to map each label_number to a label descriptor
// Go back and backpatch the label descriptors into the goto descriptor
//
// Also create parent pointers for every goto, label and hcf visited
// This routine is called by preopt, so we don't have a full set of
// parent pointers.  We don't need parents for regular statements.
void GTABLE::Build()
{
  _parent_map = WN_MAP_Create(_pool);
  Is_True(_parent_map != -1, ("Ran out of mappings in GTABLE::Build"));
  if (_parent_map == -1) { // conservatively don't do anything
    return;
  }

  // use hash table to map label number to descriptor
  typedef HASH_TABLE<INT, LDESCRIPTOR *> THIS_HASH_TABLE;
  _label_table = 
    CXX_NEW(THIS_HASH_TABLE(200, _pool), _pool);  

  Build_Rec(WN_func_body(_func_nd), _func_nd, FALSE);
  Backpatch();
}

void GTABLE::Fixup_Parents(WN *wn, WN *parent) 
{
  const OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    Set_Parent(wn, parent);
    WN *kid = WN_first(wn);
    while (kid) {
      Fixup_Parents(kid, wn);
      kid = WN_next(kid);
    }
    return;
  }
  if (opcode == OPC_ALTENTRY) {
    Set_Parent(wn, parent);
  }


  // recurse down the hierarchical control flow, except we don't
  if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) ||
      (opcode == OPC_REGION) ||
      (opcode == OPC_WHILE_DO) || (opcode == OPC_IF)) {
    Set_Parent(wn, parent);
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Fixup_Parents(WN_kid(wn, kidno), wn);
    }
    return;
  }
  if (opcode == OPC_COMPGOTO) {
    Set_Parent(wn, parent);
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Fixup_Parents(WN_kid(wn, kidno), wn);
    }
    return;
  }

  if ((opcode ==  OPC_GOTO) || (opcode == OPC_TRUEBR) ||
      (opcode == OPC_FALSEBR)) {
    Set_Parent(wn, parent);
  } else if (opcode == OPC_LABEL) {
    Set_Parent(wn, parent);
  } 

  // Always set parent pointers.
  // TODO: Since we are always setting pointers, a lot of cleanup can be
  // done.
  Set_Parent(wn, parent);
  if (WN_operator(wn) == OPR_COMMA) {
    Fixup_Parents(WN_kid(wn, 0), wn);
  }
  else
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Fixup_Parents(WN_kid(wn, kidno), wn);
    }
}

void GTABLE::Build_Rec(WN *wn, WN *parent, BOOL inside_compgoto)
{
  const OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    Set_Parent(wn, parent);
    WN *kid = WN_first(wn);
    while (kid) {
      Build_Rec(kid, wn, inside_compgoto);
      kid = WN_next(kid);
    }
    return;
  }
  if (opcode == OPC_ALTENTRY) {
    Set_Parent(wn, parent);
  }

  // recurse down the hierarchical control flow, except we don't
  if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) ||
      (opcode == OPC_REGION) ||
      (opcode == OPC_WHILE_DO) || (opcode == OPC_IF)) {
    Set_Parent(wn, parent);
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Build_Rec(WN_kid(wn, kidno), wn, inside_compgoto);
    }
    return;
  }
  if (opcode == OPC_COMPGOTO) {
    Set_Parent(wn, parent);
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Build_Rec(WN_kid(wn, kidno), wn, TRUE);
    }
    return;
  }

  if ((opcode ==  OPC_GOTO) || (opcode == OPC_TRUEBR) ||
      (opcode == OPC_FALSEBR)) {
    Set_Parent(wn, parent);
    _gd.Push(GDESCRIPTOR(wn, NULL, _offset, 0, inside_compgoto));
    _offset++;
  } else if (opcode == OPC_LABEL) {
    Set_Parent(wn, parent);
    LDESCRIPTOR *ld = CXX_NEW(LDESCRIPTOR(wn, _offset), _pool);
    if (LABEL_kind (Label_Table[WN_label_number(wn)]) == LKIND_ASSIGNED) {
      _bad_label.Push(*ld);
    }
    _label_table->Enter(WN_label_number(wn), ld);
    _offset++;
  } else if (opcode == OPC_ALTENTRY) {
    _contains_altentry = TRUE;
    _altentry.Push(wn);
  }

  // Always set the parent (bug 13208)
  Set_Parent(wn, parent);
  if (WN_operator(wn) == OPR_COMMA) {
    Build_Rec(WN_kid(wn, 0), wn, inside_compgoto);
  }
  else
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
      Build_Rec(WN_kid(wn, kidno), wn, inside_compgoto);
    }
}



// backpatch the label info into the stack
void GTABLE::Backpatch()
{
  for (INT i = 0; i<_gd.Elements(); i++) {
    GDESCRIPTOR *gd = &_gd.Bottom_nth(i);
    INT label_num = WN_label_number(gd->Goto_Wn);
    LDESCRIPTOR *ld = _label_table->Find(label_num);
    if (ld != NULL) {
      gd->Label_Wn = ld->Label_Wn;
      gd->Label_Offset = ld->Offset;
    }
  }
}

// Get rid of the relevant gotos.
// Currently we only remove gotos that can be replaced with loops.
// TODO: Consider enabling of converting gotos to IFs.
void GTABLE::Remove_Gotos()
{
  INT goto_expanding_transformations = 0;
  // go backwards, seems to be cleaner
  INT i;
  BOOL created_whiles=FALSE;
  for (i = _gd.Elements() - 1; i >= 0; i--) {
    GDESCRIPTOR *gd = &_gd.Bottom_nth(i);
    // don't touch breaks or compgotos
    if (gd->Label_Wn && ! WN_Label_Is_Break(gd->Label_Wn)
	&& ! gd->Is_Compgoto) {
      if (Goto_Is_Noop(gd)) {
        WN_DELETE_FromBlock(Get_Parent(gd->Goto_Wn), gd->Goto_Wn);
	gd->Is_Dismantled = TRUE;
      } else {
        if (Is_Truebr(gd)) {
          Create_Truebr(gd);
        }
        if (Sibling(gd)) {
          if (gd->Goto_Offset < gd->Label_Offset) {
	    //Replace_Goto_With_If(gd); /* Disabled for now. */
          } else {
	    BOOL converted_goto = Replace_Goto_With_While(gd);
	    if (converted_goto)
	      created_whiles = TRUE;
          }
        }
      }
    }
  }

  if (created_whiles) {
    Promote_Do_While(Func_Nd());
  }

  // Dismantle bad control flow nodes
  for (i = _gd.Elements() - 1; i >= 0; i--) { 
    GDESCRIPTOR *gd = &_gd.Bottom_nth(i);
    if (gd->Label_Wn && !gd->Is_Dismantled) {
      WN *label_wn = gd->Label_Wn;
      WN *goto_wn =  gd->Goto_Wn;
      WN *ancestor = Find_Common_Ancestor(label_wn, goto_wn);
      WN *tmp = Get_Parent(label_wn);
      while (tmp != ancestor) {
        WN *next_tmp = Get_Parent(tmp);
        OPCODE opcode = WN_opcode(tmp);
        if (opcode_is_scf(opcode)) {
          Dismantle(tmp, next_tmp);
        }
        tmp = next_tmp;
      }
    }
  }
  for (i = _bad_label.Elements() - 1; i >= 0; i--) {
    WN *label_wn = _bad_label.Bottom_nth(i).Label_Wn;
    if (label_wn) {
      WN *tmp = Get_Parent(label_wn);
      while (tmp) {
	WN *next_tmp = Get_Parent(tmp);
	OPCODE opcode = WN_opcode(tmp);
	if (opcode_is_scf(opcode)) {
	  Dismantle(tmp, next_tmp);
	}
	tmp = next_tmp;
      }
    }
  }
  for (i = _altentry.Elements() - 1; i >= 0; i--) {
    WN *tmp = _altentry.Bottom_nth(i);
    while (tmp) {
      WN *next_tmp = Get_Parent(tmp);
      OPCODE opcode = WN_opcode(tmp);
      if (opcode_is_scf(opcode)) {
        Dismantle(tmp, next_tmp);
      }
      tmp = next_tmp;
    }
  }
}

// find the common ancestor of wn1 and wn2
WN *GTABLE::Find_Common_Ancestor(WN *wn1, WN *wn2)
{
  INT l1 = Find_Level(wn1);
  INT l2 = Find_Level(wn2);

  if (l1 > l2) {
    for (INT i = 0; i < l1 - l2; i++) {
      wn1 = Get_Parent(wn1);
    }
  } else if (l2 > l1) {
    for (INT i = 0; i < l2 - l1; i++) {
      wn2 = Get_Parent(wn2);
    }
  }
  while (wn1 != wn2) {
    wn1 = Get_Parent(wn1);
    wn2 = Get_Parent(wn2);
  }
  return wn1;
}

INT GTABLE::Find_Level(WN *wn)
{
  INT result = 0;
  while ((wn = Get_Parent(wn)) != 0) {
    result++;
  }
  return result;
}

// This routine is not from the McGill paper
// is the goto meaningless, are we jumping to the same
// location we were going to go to anyway
// we catch two cases
//   1. The label immediately follows the goto
//   2. The goto is the last thing in an if, and the label
//      immediately follows the if
BOOL GTABLE::Goto_Is_Noop(const GDESCRIPTOR *gd) const
{
  WN *goto_wn = gd->Goto_Wn;

  if (WN_next(goto_wn) == gd->Label_Wn) {
    return TRUE;
  } 

  WN *tmp = goto_wn;
  WN *grand_parent = goto_wn;
  while (!WN_next(tmp)) {
    tmp = grand_parent;
    WN *parent = Get_Parent(tmp);
    if (WN_opcode(parent) != OPC_BLOCK) {
      return FALSE;
    }
    grand_parent = Get_Parent(parent);
    if (WN_opcode(grand_parent) != OPC_IF) {
      return FALSE;
    }
  }
  return (WN_next(tmp) == gd->Label_Wn);
}

// is the goto a sibling of its label
BOOL GTABLE::Sibling(const GDESCRIPTOR *gd) const
{
  return (Get_Parent(gd->Label_Wn) == Get_Parent(gd->Goto_Wn));
}

// This routine is not from the McGill paper
// Replace if (cond) goto with a single TRUEBR
BOOL GTABLE::Is_Truebr(const GDESCRIPTOR *gd) const
{
  WN *wn = gd->Goto_Wn;
  if (WN_opcode(wn) != OPC_GOTO) {
    return FALSE;
  }
  WN *parent = Get_Parent(wn);
  if (WN_opcode(parent) != OPC_BLOCK) {
    return FALSE;
  }
  WN *grand_parent = Get_Parent(parent);
  if (WN_opcode(grand_parent) != OPC_IF) {
    return FALSE;
  }

  WN *wn_else = WN_else(grand_parent);
  if (WN_first(wn_else)) {
     WN * stmt = WN_first(wn_else);
     if (stmt == WN_last(wn_else) &&
         wn != stmt &&
         WN_operator(stmt) == OPR_GOTO &&
         WN_next(grand_parent) &&
         WN_operator(WN_next(grand_parent)) == OPR_LABEL &&
         WN_label_number(stmt) == WN_label_number(WN_next(grand_parent)))
       ;
     else
     return FALSE;
  }

  if (wn != WN_last(parent) || wn != WN_first(parent)) {
    return FALSE;
  }
  return TRUE;
}

void GTABLE::Create_Truebr(GDESCRIPTOR *gd)
{
  WN *block_wn = Get_Parent(gd->Goto_Wn);
  WN *if_wn = Get_Parent(block_wn);
  WN *if_parent = Get_Parent(if_wn);
  WN *if_test = WN_if_test(if_wn);

  WN *truebr = WN_CreateTruebr(WN_label_number(gd->Goto_Wn),if_test);
  Set_Parent(if_test, truebr);
  WN_Set_Linenum(truebr, WN_Get_Linenum(gd->Goto_Wn));
  WN_INSERT_BlockBefore(if_parent,if_wn,truebr);
  gd->Goto_Wn = truebr;
  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_lower_branch( if_wn, truebr );
  }
  Set_Parent(truebr, if_parent);
  WN_if_test(if_wn) = NULL;
  if (WN_first(WN_else(if_wn))) //
  {
    WN * stmt = WN_EXTRACT_FromBlock(WN_else(if_wn), WN_first(WN_else(if_wn)));
    WN_INSERT_BlockBefore(if_parent, if_wn, stmt);
    if (Get_Parent(stmt)) Set_Parent(stmt, if_parent);
  }
  WN_DELETE_FromBlock(if_parent,if_wn);
}

// Given a forward, sibling, goto, replace it with an if statement.  Eg:
// TRUEBR <cond> <label>                    IF
// <statements>                               <! cond>
// LABEL <label>                ===>        THEN
//                                            <statements>
//                                          END_IF
void GTABLE::Replace_Goto_With_If(GDESCRIPTOR *gd)
{
  WN *goto_wn = gd->Goto_Wn;
  WN *label_wn = gd->Label_Wn;
  WN *parent = Get_Parent(goto_wn);

  if (WN_next(goto_wn) == label_wn) {  // not really jumping anywhere
    WN_DELETE_FromBlock(parent, goto_wn);
    gd->Is_Dismantled = TRUE;
    return;
  }

  const OPCODE goto_opcode = WN_opcode(goto_wn);
  WN *cond;
  if (goto_opcode == OPC_GOTO) { // an unconditional branch, we don't get rid 
				 // of these as preopt promises it will 
				 // remove them
    return;
    //cond = WN_CreateIntconst(
   //	OPCODE_make_op(OPR_INTCONST,Boolean_type, MTYPE_V), 0);
  } else if (goto_opcode == OPC_TRUEBR) {
    cond = WN_CreateExp1(OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V),
			 WN_kid0(goto_wn));
  } else {
    Is_True(goto_opcode == OPC_FALSEBR,
	("Unknown goto in Replace_Goto_With_If"));
    cond = WN_kid0(goto_wn);
  }

  WN *then_block = WN_CreateBlock();
  WN *else_block = WN_CreateBlock();
  WN_Set_Linenum(then_block, WN_Get_Linenum(goto_wn));
  WN_Set_Linenum(else_block, WN_Get_Linenum(goto_wn));

  // move all the staments into the then_block
  WN *next_wn = NULL;
  for (WN *wn = WN_next(goto_wn); wn != label_wn; wn = next_wn) {
    next_wn = WN_next(wn);
    wn = WN_EXTRACT_FromBlock(parent, wn);
    WN_INSERT_BlockBefore(then_block, NULL, wn);
    if (Get_Parent(wn)) Set_Parent(wn, then_block);
  }

  WN *if_wn = WN_CreateIf(cond, then_block, else_block);
  Set_Parent(then_block, if_wn);
  Set_Parent(else_block, if_wn);
  Set_Parent(if_wn, parent);
  WN_Set_Linenum(if_wn, WN_Get_Linenum(goto_wn));

  WN_INSERT_BlockAfter(parent, goto_wn, if_wn);
  WN_EXTRACT_FromBlock(parent, goto_wn);
  gd->Is_Dismantled = TRUE;

  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_convert_goto_to_if(goto_wn, if_wn);
  }
  WN_Delete(goto_wn);
}

// Are two expressions equivalent, assumes that there are no intervening writes
static BOOL Equivalent(WN *wn1, WN *wn2)
{
  if (!WN_Equiv(wn1, wn2)) return FALSE;
  for (INT i=0; i<WN_kid_count(wn1); i++) {
    if (!Equivalent(WN_kid(wn1,i), WN_kid(wn2,i))) return FALSE;
  }
  return TRUE;
}


// replace 
// if (cond) {
// do {
//    ...
// while(cond) 
// with 
// while (cond) do
void GTABLE::Promote_Do_While(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      WN *next_kid = WN_next(kid);
      Promote_Do_While(kid);
      kid = next_kid;
    }
  } else {
    if (OPCODE_is_scf(opcode)) {
      if (opcode == OPC_IF) {
        if (WN_first(WN_else(wn)) == NULL) {
          if (WN_first(WN_then(wn)) && (WN_first(WN_then(wn)) == WN_last(WN_then(wn)))) {
            if (WN_opcode(WN_first(WN_then(wn))) == OPC_DO_WHILE) {
	      WN *while_wn = WN_first(WN_then(wn));
              if (Equivalent(WN_if_test(wn), WN_while_test(while_wn))) {
                WN_set_opcode(while_wn, OPC_WHILE_DO);
  		WN *parent = Get_Parent(wn);
  		WN_INSERT_BlockBefore(parent, wn, while_wn);
		Set_Parent(while_wn, parent);
                WN_first(WN_then(wn)) = NULL;
                WN_DELETE_FromBlock(parent, wn);
		Promote_Do_While(while_wn);
		return;
              }
            }
          }
        }
      }
    } 
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
       Promote_Do_While(WN_kid(wn,kidno));
    }
  }
}

// Convert 
//
// goto L
// do_while                                   while_do
//                                                   <S1-Sn>
//                                                 BLOCK
//                                                 <test>
//    <test>                                     COMMA
//  <statements begin>               ===>      <statements begin>
//    <optional statements>                      <optional statements>
//    L:                                         L:
//    <optional statements S1-Sn>              <statements end>
//  <statements end>
//
// Note: The input while_wn in the above example has invalid control flow.
//
// As a special case, S1-Sn may be empty, in which case only the goto is
// removed, and the loop converted to a while_do.
//
void GTABLE::Patch_Do_While (WN * while_wn, WN * parent)
{
  BOOL patch = FALSE;
  Is_True (Get_Parent(while_wn) == parent, ("Invalid parent of while node"));

  // goto node before the loop
  WN * goto_wn = WN_prev(while_wn);

  if (!goto_wn || WN_operator(goto_wn) != OPR_GOTO) return;

  // last statement in loop
  WN * last_stmt = WN_last(WN_while_body(while_wn));

  // early determination of special case if loop can be modified
  //
  // Bug 13181 has a case with an empty loop body, so the above goto
  // must be to a target outside of the loop.
  if (last_stmt &&
      WN_operator(last_stmt) == OPR_LABEL &&
      WN_label_number(goto_wn) == WN_label_number(last_stmt))
    patch = TRUE;

  // find the target label node
  WN * label_wn = NULL;
  GDESCRIPTOR * label_gd = NULL;
  for (INT i = _gd.Elements() - 1; i >= 0; i--) {
    GDESCRIPTOR *gd = &_gd.Bottom_nth(i);
    if (goto_wn == gd->Goto_Wn) {
      label_wn = gd->Label_Wn;
      label_gd = gd;
      break;
    }
  }

  if (label_wn == NULL) return;

  // is the target label inside the loop?
  WN * block = Get_Parent(label_wn);

  if (!block || block != WN_while_body(while_wn)) return;

  if (WN_next(label_wn))
  { // there are statements after the label inside the loop, so
    // create a new comma node.
    WN * comma_block = WN_CreateBlock();
    WN * wn = WN_next(label_wn);
    while (wn)
    {
      // move statements into comma block
      WN * next_wn = WN_next(wn);
      wn = WN_EXTRACT_FromBlock(block, wn);
      WN_INSERT_BlockLast (comma_block, wn);
      // update parent
      Fixup_Parents (wn, comma_block);
      wn = next_wn;
    }
    // insert comma node into loop condition
    WN * test = WN_while_test(while_wn);
    WN_while_test(while_wn) = WN_CreateComma(OPR_COMMA, WN_rtype(test),
                                             MTYPE_V, comma_block, test);
    // update parent
    Fixup_Parents(comma_block, WN_while_test(while_wn));
    Fixup_Parents(WN_kid1(WN_while_test(while_wn)), WN_while_test(while_wn));
    patch = TRUE;
  }

  if (patch)
  {
    label_gd->Label_Wn = NULL;

    // remove goto
    WN_DELETE_FromBlock(parent, WN_prev(while_wn));
    // change loop to while-do
    WN_set_opcode(while_wn, OPC_WHILE_DO);
  }
}

// Do sanity checks on the statements about to form the loop body.
// Return TRUE if OK, else return FALSE.
static BOOL sanity_check_loop_body (WN * label_wn, WN * goto_wn)
{
  BOOL found_target_label = FALSE;
  WN * prev = WN_prev(label_wn);
  if (!prev || WN_operator(prev) != OPR_GOTO)
    return TRUE;

  // For now, we need the check only for fortran. ** Note: remove this check
  // when we apply this to other languages.
  if (!PU_ftn_lang(Get_Current_PU()))
    return TRUE;

  // Early goto-conversion is now OFF by default for fortran, so the above
  // should always return true by default (bug 14188).
  INT label_num = WN_label_number(prev);
  WN * next_wn = NULL;
  for (WN *wn = label_wn; wn != goto_wn; wn = next_wn) {
    next_wn = WN_next(wn);
    if (WN_operator(wn) == OPR_LABEL && WN_label_number(wn) == label_num)
      found_target_label = TRUE;
    // Check for scenario documented in comments for Replace_Goto_With_While:
    // there are statements after the target-label (bug 13084)
    else if (found_target_label && PU_ftn_lang(Get_Current_PU()))
      return FALSE;
    // There probably are more such scenarios, like statements with control
    // flow when found_target_label is true, when we should prevent the
    // transformation, but let me wait for some such examples.
  }

  return TRUE;
}

// Given a backward, sibling, goto, replace it with a DO_WHILE.  For example:
// LABEL <label>                            DO_WHILE
// <statements>                               <cond>
// TRUEBR <cond> <label>        ===>        BODY
//                                            <statements>
//                                          END_DO_WHILE
//
// The above conversion can sometimes generate invalid control flow, as in:
//   GOTO L
//   DO_WHILE
//     <cond>
//   BODY
//     <statements>
//     L:
//     <statements S>
//   END_DO_WHILE
//
// Patch_Do_While() attempts to fix the above control flow. If S is non-empty,
// a COMMA node needs to be used. F90 lowerer does not handle COMMA nodes (as
// seen in bug 13084), so in such a case, the above transformation should not
// be done.
//
// Return TRUE if we really generated the loop, else return FALSE;
BOOL GTABLE::Replace_Goto_With_While(GDESCRIPTOR *gd)
{
  WN *goto_wn = gd->Goto_Wn;
  WN *label_wn = gd->Label_Wn;
  WN *parent = Get_Parent(goto_wn);

  if (!sanity_check_loop_body(label_wn, goto_wn))
    return FALSE;

  const OPCODE goto_opcode = WN_opcode(goto_wn);
  WN *cond;
  if (goto_opcode == OPC_GOTO) { // an unconditional branch
    cond = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, Boolean_type,
					    MTYPE_V), 1);
  } else if (goto_opcode == OPC_FALSEBR) {
    cond = WN_CreateExp1(OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V),
				WN_kid0(goto_wn));
  } else {
    Is_True(goto_opcode == OPC_TRUEBR,
	("Unknown goto in Replace_Goto_With_While"));
    cond = WN_kid0(goto_wn);
  }

  WN *block = WN_CreateBlock();
  WN_Set_Linenum(block, WN_Get_Linenum(goto_wn));

  // move all the statements into the block
  WN *next_wn = NULL;
  for (WN *wn = label_wn; wn != goto_wn; wn = next_wn) {
    next_wn = WN_next(wn);
    wn = WN_EXTRACT_FromBlock(parent, wn);
    WN_INSERT_BlockBefore(block, NULL, wn);
    if (Get_Parent(wn)) Set_Parent(wn, block);
  }


  WN *while_wn = WN_CreateDoWhile(cond, block);
  Set_Parent(cond, while_wn);
  Set_Parent(block, while_wn);
  Set_Parent(while_wn, parent);
  WN_Set_Linenum(while_wn, WN_Get_Linenum(goto_wn));

  WN_INSERT_BlockAfter(parent, goto_wn, while_wn);
  WN_EXTRACT_FromBlock(parent, goto_wn);
  gd->Is_Dismantled = TRUE;

  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_convert_goto_to_loop(goto_wn, while_wn);
  }

  Patch_Do_While (while_wn, parent);

  WN_Delete(goto_wn);

  return TRUE;
}

void GTABLE::Print(FILE *fp)
{
  fprintf(fp, "Printing a GTABLE\n");
  for (INT i = 0; i < _gd.Elements(); i++) {
    GDESCRIPTOR *gd = &_gd.Bottom_nth(i);
    fprintf(fp, "A goto with: \n");
    fprintf(fp, "  offset = %d \n", gd->Goto_Offset);
    fprintf(fp, "to label = %d  with \n", WN_label_number(gd->Goto_Wn));
    fprintf(fp, "  offset = %d \n", gd->Label_Offset);
    fprintf(fp, "\n");
  }
}

/* Convert WHILE_DO to gotos.
   Generate:
 *      (GOTO <br_lbl>)
 *      (LABEL <top_lbl>)
 *      <body>
 *      (LABEL <br_lbl>)
 *      (TRUEBR <test> <top_lbl>)
 */
static void convert_while_do (WN * block, WN * tree)
{
  Is_True (WN_operator(tree) == OPR_WHILE_DO,
           ("convert_while_do() can only accept OPR_WHILE_DO!"));
  LABEL_IDX top_lbl;
  New_LABEL (CURRENT_SYMTAB, top_lbl);

  LABEL_IDX br_lbl;
  New_LABEL (CURRENT_SYMTAB, br_lbl);

  // goto
  WN_INSERT_BlockLast (block, WN_CreateGoto (br_lbl));
  // top label
  WN_INSERT_BlockLast (block, WN_CreateLabel ((ST_IDX) 0, top_lbl, 0, NULL));
  // body
  WN_INSERT_BlockLast (block, WN_while_body(tree));
  // br label
  WN_INSERT_BlockLast (block, WN_CreateLabel((ST_IDX)0, br_lbl, 0, NULL));
  // truebr
  WN_INSERT_BlockLast (block,
                       WN_CreateTruebr (top_lbl, WN_while_test(tree)));

  WN_Delete(tree);
}

/* Convert DO_WHILE to gotos.
 * Generate:
 *   (LABEL <top_lbl>)
 *   <body>
 *   (TRUEBR <test> <top_lbl>)
 */
static void convert_do_while (WN * block, WN * tree)
{
  Is_True (WN_operator(tree) == OPR_DO_WHILE,
           ("convert_do_while() can only accept OPR_DO_WHILE!"));
  LABEL_IDX top_lbl;
  New_LABEL (CURRENT_SYMTAB, top_lbl);
  // top label
  WN_INSERT_BlockLast (block, WN_CreateLabel ((ST_IDX) 0, top_lbl, 0, NULL));
  // body
  WN_INSERT_BlockLast (block, WN_while_body(tree));
  // truebr
  WN_INSERT_BlockLast (block,
                       WN_CreateTruebr (top_lbl, WN_while_test(tree)));

  WN_Delete(tree);
}

// Dismantle a bad hierarchical control flow node
void GTABLE::Dismantle(WN *bad, WN *parent)
{
  WN *prev = WN_prev(bad);
  WN *block = WN_CreateBlock(); 
  WN_EXTRACT_FromBlock(parent, bad);

  switch (WN_operator(bad))
  {
    case OPR_WHILE_DO:
      convert_while_do (block, bad);
      break;
    case OPR_DO_WHILE:
      convert_do_while (block, bad);
      break;
    default:
      Is_True (FALSE, ("GTABLE::Dismantle does not dismantle %s",
                       OPERATOR_name(WN_operator(bad))));
  }

  // move everything in block to be after prev
  while (WN_last(block)) {
    WN *element = WN_EXTRACT_FromBlock(block, WN_last(block));
    WN_INSERT_BlockAfter(parent, prev, element);
    Fixup_Parents(element, parent);
  }
  WN_Delete(block);
}

