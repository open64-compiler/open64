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
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: opt_goto.cxx  
 * $Revision: 1.21 $
 * $Date: 2000/04/06 01:58:39 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/com/RCS/opt_goto.cxx,v $
 *
 * Revision history:
 *  1-31-95 - Original Version
 *
 * Description: Build up do loops and ifs
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
//#include <sys/types.h>
//#include <elf.h>
#include "opt_goto.h"
#include "wn_util.h"
#include "config_targ.h"
#include "strtab.h"
#include "stab.h"
#include "wn_lower.h"
#include "targ_sim.h"
#include "fb_whirl.h"

#ifdef _KEEP_RCS_ID
#define opt_goto_CXX "opt_goto.cxx"
static char *rcs_id = opt_goto_CXX" $Revision: 1.21 $";
#endif /* _KEEP_RCS_ID */


// Build up a table of goto descriptors
// Go through the code, push each goto onto the stack
// Use a hash table to map each label_number to a label descriptor
// Go back and backpatch the label descriptors into the goto descriptor
//
// Also create parent pointers for every goto, label and hcf visited
// This routine is called by preopt, so we don't have a full set of
// parent pointers.  We don't need parents for regular statements.
void GOTO_TABLE::Build()
{
  _parent_map = WN_MAP_Create(_pool);
  Is_True(_parent_map != -1, ("Ran out of mappings in GOTO_TABLE::Build"));
  if (_parent_map == -1) { // conservatively don't do anything
    return;
  }

  // use hash table to map label number to descriptor
  typedef HASH_TABLE<INT, LABEL_DESCRIPTOR *> THIS_HASH_TABLE;
  _label_table = 
    CXX_NEW(THIS_HASH_TABLE(200, _pool), _pool);  

  Build_Rec(WN_func_body(_func_nd), _func_nd, FALSE);
  Backpatch();
}

void GOTO_TABLE::Fixup_Parents(WN *wn, WN *parent) 
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
}

void GOTO_TABLE::Build_Rec(WN *wn, WN *parent, BOOL inside_compgoto)
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
    _gd.Push(GOTO_DESCRIPTOR(wn, NULL, _offset, 0, inside_compgoto));
    _offset++;
  } else if (opcode == OPC_LABEL) {
    Set_Parent(wn, parent);
    LABEL_DESCRIPTOR *ld = CXX_NEW(LABEL_DESCRIPTOR(wn, _offset), _pool);
    if (LABEL_kind (Label_Table[WN_label_number(wn)]) == LKIND_ASSIGNED) {
      _bad_label.Push(*ld);
    }
    _label_table->Enter(WN_label_number(wn), ld);
    _offset++;
  } else if (opcode == OPC_ALTENTRY) {
    _contains_altentry = TRUE;
    _altentry.Push(wn);
  }
}



// backpatch the label info into the stack
void GOTO_TABLE::Backpatch()
{
  for (INT i = 0; i<_gd.Elements(); i++) {
    GOTO_DESCRIPTOR *gd = &_gd.Bottom_nth(i);
    INT label_num = WN_label_number(gd->Goto_Wn);
    LABEL_DESCRIPTOR *ld = _label_table->Find(label_num);
    if (ld != NULL) {
      gd->Label_Wn = ld->Label_Wn;
      gd->Label_Offset = ld->Offset;
    }
  }
}

// Get rid of the relevant gotos
void GOTO_TABLE::Remove_Gotos()
{
  INT goto_expanding_transformations = 0;
  // go backwards, seems to be cleaner
  INT i;
  BOOL created_whiles=FALSE;
  for (i = _gd.Elements() - 1; i >= 0; i--) {
    GOTO_DESCRIPTOR *gd = &_gd.Bottom_nth(i);
    // don't touch breaks or compgotos
    if (gd->Label_Wn && ! WN_Label_Is_Break(gd->Label_Wn)
	&& ! gd->Is_Compgoto) {
      if (Is_Truebr(gd)) {
	Create_Truebr(gd);
      }
      if (Can_Move_Into_Else(gd)) {
        Move_Into_Else(gd);
      } 
      if (Goto_Is_Noop(gd)) {
        WN_DELETE_FromBlock(Get_Parent(gd->Goto_Wn), gd->Goto_Wn);
	gd->Is_Dismantled = TRUE;
      } else {
        //while (Ancestor_Through_If(gd) && 
	//     !_contains_altentry &&
	//    (goto_expanding_transformations < 
	//	   MAX_GOTO_EXPANDING_TRANSFORMATIONS)) {
        if (Parent_Through_If(gd) && !_contains_altentry) { 
          Move_Goto_Out(gd);
	  goto_expanding_transformations++;
        }
        if (Sibling(gd)) {
          if (gd->Goto_Offset < gd->Label_Offset) {
	    Replace_Goto_With_If(gd);
          } else {
	    Replace_Goto_With_While(gd);
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
    GOTO_DESCRIPTOR *gd = &_gd.Bottom_nth(i);
    if (gd->Label_Wn && !gd->Is_Dismantled) {
      WN *label_wn = gd->Label_Wn;
      WN *goto_wn =  gd->Goto_Wn;
      WN *ancestor = Find_Common_Ancestor(label_wn, goto_wn);
      WN *tmp = Get_Parent(label_wn);
      while (tmp != ancestor) {
        WN *next_tmp = Get_Parent(tmp);
        OPCODE opcode = WN_opcode(tmp);
        if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) ||
            (opcode == OPC_WHILE_DO) || (opcode == OPC_IF)) {
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
	if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) ||
            (opcode == OPC_WHILE_DO) || (opcode == OPC_IF)) {
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
      if ((opcode == OPC_DO_LOOP) || (opcode == OPC_DO_WHILE) ||
            (opcode == OPC_WHILE_DO) || (opcode == OPC_IF)) {
        Dismantle(tmp, next_tmp);
      }
      tmp = next_tmp;
    }
  }
}

// find the common ancestor of wn1 and wn2
WN *GOTO_TABLE::Find_Common_Ancestor(WN *wn1, WN *wn2)
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

INT GOTO_TABLE::Find_Level(WN *wn)
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
BOOL GOTO_TABLE::Goto_Is_Noop(GOTO_DESCRIPTOR *gd) const
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

// is the goto a descendant of its label, with only BLOCKs and IFs in between
BOOL GOTO_TABLE::Ancestor_Through_If(GOTO_DESCRIPTOR *gd)
{
  INT goto_level = Find_Level(gd->Goto_Wn);
  INT label_level = Find_Level(gd->Label_Wn);
  if (goto_level <= label_level) {
    return(FALSE);
  }

  WN *tmp = gd->Goto_Wn;
  for (INT i=goto_level; i>label_level; i--) { 
    tmp = Get_Parent(tmp);
    const OPCODE opcode = WN_opcode(tmp);
    if ((opcode != OPC_BLOCK) && (opcode != OPC_IF)) {
      return(FALSE);
    }
  }

  return (Get_Parent(tmp) == Get_Parent(gd->Label_Wn));
}
  
// is the goto a descendant of its label, with only one BLOCK and one IF in between
BOOL GOTO_TABLE::Parent_Through_If(GOTO_DESCRIPTOR *gd)
{
  INT goto_level = Find_Level(gd->Goto_Wn);
  INT label_level = Find_Level(gd->Label_Wn);
  if (goto_level != label_level+2) {
    return(FALSE);
  }

  WN *tmp = Get_Parent(gd->Goto_Wn);
  OPCODE opcode = WN_opcode(tmp);
  if (opcode != OPC_BLOCK) return FALSE;
  tmp = Get_Parent(tmp);
  opcode = WN_opcode(tmp);
  if (opcode != OPC_IF) return FALSE;
  return (Get_Parent(tmp) == Get_Parent(gd->Label_Wn));
}

// is the goto a sibling of its label
BOOL GOTO_TABLE::Sibling(GOTO_DESCRIPTOR *gd) 
{
  return (Get_Parent(gd->Label_Wn) == Get_Parent(gd->Goto_Wn));
}

// This routine is not from the McGill paper
// Replace if (cond) goto with a single TRUEBR
BOOL GOTO_TABLE::Is_Truebr(GOTO_DESCRIPTOR *gd)
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
     return FALSE;
  }

  if (wn != WN_last(parent) || wn != WN_first(parent)) {
    return FALSE;
  }
  return TRUE;
}

void GOTO_TABLE::Create_Truebr(GOTO_DESCRIPTOR *gd)
{
  WN *block_wn = Get_Parent(gd->Goto_Wn);
  WN *if_wn = Get_Parent(block_wn);
  WN *if_parent = Get_Parent(if_wn);
  WN *if_test = WN_if_test(if_wn);

  WN *truebr = WN_CreateTruebr(WN_label_number(gd->Goto_Wn),if_test);
  WN_Set_Linenum(truebr, WN_Get_Linenum(gd->Goto_Wn));
  WN_INSERT_BlockBefore(if_parent,if_wn,truebr);
  gd->Goto_Wn = truebr;
  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_lower_branch( if_wn, truebr );
  }
  Set_Parent(truebr, if_parent);
  WN_if_test(if_wn) = NULL;
  WN_DELETE_FromBlock(if_parent,if_wn);
}

// This routine is not from the McGill paper
// If a goto is the last goto in the then/else clause, and if it's an
// unconditional goto rather than use the regular algorithm, we put all
// the statements following the if (but before the label) at the end of
// the else/then clause of the if
//
// While all these cases could be handled by the regular algorithm,
// this special case avoids creating new conditional variables and is
// easier for the optimizer to deal with.
BOOL GOTO_TABLE::Can_Move_Into_Else(GOTO_DESCRIPTOR *gd)
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
  if (wn != WN_last(parent)) {
    return FALSE;
  }
  return TRUE;
}

void GOTO_TABLE::Move_Into_Else(GOTO_DESCRIPTOR *gd)
{
  WN *block_wn = Get_Parent(gd->Goto_Wn);
  WN *if_wn = Get_Parent(block_wn);
  WN *if_parent = Get_Parent(if_wn);

  WN *insert_block = NULL;
  if (block_wn == WN_then(if_wn)) {
    insert_block = WN_else(if_wn);
  } else {
    insert_block = WN_then(if_wn);
  }

  WN *label_wn = gd->Label_Wn;
  while (WN_next(if_wn) && (WN_next(if_wn) != label_wn)) {
    WN *tmp = WN_EXTRACT_FromBlock(if_parent, WN_next(if_wn));
    WN_INSERT_BlockBefore(insert_block, NULL, tmp);
    Set_Parent(tmp, insert_block);
  }
}


//
// Find the last formal argument store in a block.  This is
// assumed to be the WN_func_body() block of a function.
// 
// We find it so we can insert stuff after those stores, and keep
// them at the beginning of the block, if they're there at all.
//
static WN *
Find_last_arg_copy( WN *func_body )
{
  Is_True( WN_opcode(func_body) == OPC_BLOCK,
    ("Find_last_arg_copy: function body is not block") );

  WN *last_arg_copy = NULL;
  for ( WN *stmt = WN_first(func_body); stmt; stmt = WN_next(stmt) ) {
    const OPERATOR opr = WN_operator(stmt);
    if ( opr != OPR_STID )
      break;

    WN *rhs = WN_kid0(stmt);
    OPERATOR rhs_opr = WN_operator(rhs);

    // allow dedicated register as kid of convert operation
    if ( rhs_opr == OPR_CVTL || rhs_opr == OPR_CVT ) {
      rhs = WN_kid0(rhs);
      rhs_opr = WN_operator(rhs);
    }

    if ( rhs_opr != OPR_LDID )
      break;

    if ( ST_class(WN_st(rhs)) != CLASS_PREG ||
	!Preg_Is_Dedicated(WN_load_offset(rhs)) )
      break;

    // at this point, we know we have a store of one of the dedicated
    // registers to its home location, so save a pointer to the last
    // one seen
    last_arg_copy = stmt;
  }

  return last_arg_copy;
}

// Given a goto which is jumping out of a series of IF statements, move it up
// one level
//
// IF                                       IF
//   <expr>                                   <expr>
// THEN/ELSE                                THEN/ELSE
//   <statements1>                            <statements1>
//   TRUEBR <cond> <label>                    <goto_label> = <cond>
//   <statements2>              ===>          IF
// END_IF                                       <! goto_label>
//                                            THEN
//                                              <statements2>
//                                            END_IF
//                                          END_IF
//                                          TRUEBR <goto_label> <label>
void GOTO_TABLE::Move_Goto_Out(GOTO_DESCRIPTOR *gd)
{
  WN *goto_wn = gd->Goto_Wn;
  WN *parent = Get_Parent(goto_wn);
  INT64 goto_line = WN_Get_Linenum(goto_wn);

  // Find the surrounding if
  WN *if_wn = parent;
  while (WN_opcode(if_wn) != OPC_IF) {
    if_wn = Get_Parent(if_wn);
  }

  // Create the new control variable, give it the name goto_Ln, where
  // n is the label number truncated to 8 digits
  // Seems to me that if truncation to 8 digits is really wanted,
  // there are two problems: the constant modulus given below has only
  // seven 9's in it (instead of eight), and we really want one with
  // eight zeros after it, not eight nines. I think we should have "%
  // 100000000" in place of "% 9999999". Of course none of this really
  // matters. -- rkennedy
  char new_var[15];
  INT label_name_num = WN_label_number(goto_wn) % 9999999;
  sprintf(new_var, ".goto_L%d", label_name_num);
  ST *st = MTYPE_To_PREG(Boolean_type);
  WN_OFFSET offset = Create_Preg(Boolean_type, new_var);
  
  // initialize the control variable to FALSE at the start of the function
  WN *func_body = WN_func_body(_func_nd);
  WN *init_val = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, Boolean_type,
						  MTYPE_V), 0);
  WN *stid = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, Boolean_type),
			   offset, st, Be_Type_Tbl(Boolean_type), init_val);
  WN_Set_Linenum(stid, WN_Get_Linenum(func_body));
  // insert after any argument copies (from formal registers to their
  // home locations)
  WN *last_arg_copy = Find_last_arg_copy( func_body );
  WN_INSERT_BlockAfter(func_body, last_arg_copy, stid);

  // reset the control variable to FALSE right after the label
  init_val = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, Boolean_type,
					      MTYPE_V), 0);
  stid = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, Boolean_type),
		       offset, st, Be_Type_Tbl(Boolean_type), init_val);
  WN_Set_Linenum(stid, WN_Get_Linenum(gd->Label_Wn));
  WN_INSERT_BlockAfter(Get_Parent(gd->Label_Wn), gd->Label_Wn, stid);

  // If inside a region, reset the control variable to FALSE
  // at the start of the region.  This is necessary for mp
  // where each processor might get its own variable
  WN *tmp = Get_Parent(if_wn);
  while (tmp && (WN_opcode(tmp) != OPC_REGION)) {
    tmp = Get_Parent(tmp);
  }
  if (tmp) {
    init_val = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, Boolean_type,
						MTYPE_V), 0);
    stid = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, Boolean_type),
			 offset, st, Be_Type_Tbl(Boolean_type), init_val);
    WN_Set_Linenum(stid, WN_Get_Linenum(gd->Label_Wn));
    WN_INSERT_BlockAfter(WN_region_body(tmp), NULL, stid);
  }

  // Set the control value to the condition of the goto
  const OPCODE opcode = WN_opcode(goto_wn);
  WN *val;
  if (opcode == OPC_GOTO) {
     val = WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, Boolean_type,
					    MTYPE_V), 1);
  } else if (opcode == OPC_TRUEBR) {
     val = WN_kid0(goto_wn);
  } else {
     val = WN_CreateExp1(OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V),
	WN_kid0(goto_wn));
  }
  stid = WN_CreateStid(OPCODE_make_op(OPR_STID, MTYPE_V, Boolean_type),
			offset, st, Be_Type_Tbl(Boolean_type), val);
  WN_INSERT_BlockBefore(parent, goto_wn, stid);
  WN_Set_Linenum(stid, goto_line);

  // put the rest of the if into a deeper if guarded by if (!goto_...)
  // (only do this if there are more statments in the if)
  // The frequency of the new if is the same as the frequency of the
  // goto being eliminated.
  WN *new_if = NULL;
  if (WN_next(goto_wn)) {
    WN *then_block = WN_CreateBlock();
    WN *else_block = WN_CreateBlock();
    WN_Set_Linenum(then_block, goto_line);
    WN_Set_Linenum(else_block, goto_line);
    WN *next_wn=NULL;
    for (WN *wn = WN_next(goto_wn); wn; wn = next_wn) {
      next_wn = WN_next(wn);
      wn = WN_EXTRACT_FromBlock(parent, wn);
      WN_INSERT_BlockBefore(then_block, NULL, wn);
      if (Get_Parent(wn)) Set_Parent(wn, then_block);
    }
    WN *ldid = WN_CreateLdid(OPCODE_make_op(OPR_LDID, Boolean_type,
					    Boolean_type),
			     offset, st, Be_Type_Tbl(Boolean_type));
    WN *cond = WN_CreateExp1(OPCODE_make_op(OPR_LNOT, Boolean_type, MTYPE_V),
			     ldid);
    new_if = WN_CreateIf(cond, then_block, else_block);
    Set_Parent(then_block, new_if);
    Set_Parent(else_block, new_if);
    Set_Parent(new_if, parent);
    WN_INSERT_BlockAfter(parent, goto_wn, new_if);
    WN_Set_Linenum(new_if, goto_line);
  }
  WN_EXTRACT_FromBlock(parent, goto_wn);

  // Create the conditional branch outside of the if.
  // Frequency of the new conditional branch is once for each
  // execution of the original if.
  // I don't know what the source position of the new conditional
  // branch should be. -- rkennedy
  WN *ldid = WN_CreateLdid(OPCODE_make_op(OPR_LDID, Boolean_type,
					  Boolean_type),
			   offset, st, Be_Type_Tbl(Boolean_type));
  WN *condbr = WN_CreateTruebr(WN_label_number(goto_wn), ldid);
  WN_INSERT_BlockAfter(Get_Parent(if_wn), if_wn, condbr);
  Set_Parent(condbr, Get_Parent(if_wn));
  gd->Goto_Wn = condbr;
  WN_Set_Linenum(condbr, goto_line);
  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_move_goto_out(goto_wn, new_if, condbr);
  }
  WN_Delete(goto_wn);
}

// Given a forward, sibling, goto, replace it with an if statement.  Eg:
// TRUEBR <cond> <label>                    IF
// <statements>                               <! cond>
// LABEL <label>                ===>        THEN
//                                            <statements>
//                                          END_IF
void GOTO_TABLE::Replace_Goto_With_If(GOTO_DESCRIPTOR *gd)
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
void GOTO_TABLE::Promote_Do_While(WN *wn)
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

// Given a backward, sibling, goto, replace it with a DO_WHILE.  For example:
// LABEL <label>                            DO_WHILE
// <statements>                               <cond>
// TRUEBR <cond> <label>        ===>        BODY
//                                            <statements>
//                                          END_DO_WHILE
void GOTO_TABLE::Replace_Goto_With_While(GOTO_DESCRIPTOR *gd)
{
  WN *goto_wn = gd->Goto_Wn;
  WN *label_wn = gd->Label_Wn;
  WN *parent = Get_Parent(goto_wn);


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

  // move all the staments into the block
  WN *next_wn = NULL;
  for (WN *wn = label_wn; wn != goto_wn; wn = next_wn) {
    next_wn = WN_next(wn);
    wn = WN_EXTRACT_FromBlock(parent, wn);
    WN_INSERT_BlockBefore(block, NULL, wn);
    if (Get_Parent(wn)) Set_Parent(wn, block);
  }


  WN *while_wn = WN_CreateDoWhile(cond, block);
  Set_Parent(block, while_wn);
  Set_Parent(while_wn, parent);
  WN_Set_Linenum(while_wn, WN_Get_Linenum(goto_wn));

  WN_INSERT_BlockAfter(parent, goto_wn, while_wn);
  WN_EXTRACT_FromBlock(parent, goto_wn);
  gd->Is_Dismantled = TRUE;

  if (Cur_PU_Feedback) {
    Cur_PU_Feedback->FB_convert_goto_to_loop(goto_wn, while_wn);
  }
  WN_Delete(goto_wn);
}

void GOTO_TABLE::Print(FILE *fp)
{
  fprintf(fp, "Printing a GOTO_TABLE\n");
  for (INT i = 0; i < _gd.Elements(); i++) {
    GOTO_DESCRIPTOR *gd = &_gd.Bottom_nth(i);
    fprintf(fp, "A goto with: \n");
    fprintf(fp, "  offset = %d \n", gd->Goto_Offset);
    fprintf(fp, "to label = %d  with \n", WN_label_number(gd->Goto_Wn));
    fprintf(fp, "  offset = %d \n", gd->Label_Offset);
    fprintf(fp, "\n");
  }
}


// Dismantle a bad hierarchical control flow node
void GOTO_TABLE::Dismantle(WN *bad, WN *parent)
{
  WN *prev = WN_prev(bad);
  WN *block = WN_CreateBlock(); 
  WN_EXTRACT_FromBlock(parent, bad);
  WN *scf_wn = lower_scf_non_recursive(block, bad, 
				       LOWER_SCF | LOWER_FREQUENCY_MAPS);
  WN_INSERT_BlockLast(block, scf_wn);

  // move everything in block to be after prev
  while (WN_last(block)) {
    WN *element = WN_EXTRACT_FromBlock(block, WN_last(block));
    WN_INSERT_BlockAfter(parent, prev, element);
    Fixup_Parents(element, parent);
  }
  WN_Delete(block);
}

