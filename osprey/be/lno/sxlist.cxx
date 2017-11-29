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


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include "defs.h"
#include "lnopt_main.h"
#include "mat.h"
#include "cxx_base.h"
#include "access_vector.h"
#include "snl.h"
#include "opt_du.h" 
#include "lwn_util.h"
#include "lnoutils.h"
#include "scalar_expand.h"
#include "sxlist.h"

//---------------------------------------------------------------------------
// SX_INFO::Make_Sx_Info() constructs a list of privatizable
// variables in the SNL, such that no transformation will occur outside
// an outer loop (call it depth s1) or inside an inner loop (call it s2).
// A loop nest is transformable out to a certain depth (_outermost_no_se_reqd)
// if from that depth in, there are no defs at all, or every def and use
// within the nest is part of the same kind of reduction.  Note that in
// both cases, no dependences are introduced by scalars, even distribution
// will not be impeded by scalars.  Loops further out can be transformed with
// scalar expansion, if there is a privatizable def.  The rule we use, so
// that we can ensure no dependences (so transformable and distributable)
// is that there must either be no defs further in, or all uses/defs further
// in must be part of reductions.  The only special case is that if a
// scalar is privatizable, and it's in s2 or inside, transformation legal
// without scalar expansion.  And if privatizable in loop l and all defs and
// uses are in that same loop and all either above or below the enclosed loop
// e.g.
//      for i
//         s =
//           = s
//         for j
//           (no def or use of s)
// then again the loop is fully transformable without scalar expansion.
//
// THESE ROUTINES ASSUME THAT RENAMING HAS OCCURRED, because it looks
// up names in privatizability info, and assumes the same answer for the
// same name.
// There are no dependences if there are no defs inside nested more deeply
// then ref.  Or if there are, all defs and uses inside are part of the
// same reduction.
//---------------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: Which_Loop_Inside 
// FUNCTION: Returns the largest index into 'stack' (less than or equal to
//   'first') such that WN_Is_Inside(ref, stack.Bottom_nth(index)) is 
//   TRUE. Returns -1 if not nested in any element of stack.
//-----------------------------------------------------------------------

static INT Which_Loop_Inside(WN* ref, DOLOOP_STACK* stack, INT first)
{
  INT i;
  for (i = first; i >= 0; i--) 
    if (Wn_Is_Inside(ref, stack->Bottom_nth(i)))
      break;
  return i;
}

//-----------------------------------------------------------------------
// NAME: Which_Loop_Inside 
// FUNCTION: Equivalent to Which_Loop_Inside(ref, stack, stack.Elements()-1)
//-----------------------------------------------------------------------

static INT Which_Loop_Inside(WN* ref, DOLOOP_STACK* stack)
{
  return Which_Loop_Inside(ref, stack, stack->Elements() - 1);
}

//======================================================================= 
// CLASS: SX_PNODE (Single variable which must be scalar expanded)  
// MEMBER FUNCTIONS: SX_PNODE, Print 
//======================================================================= 

SX_PNODE::SX_PNODE(WN* wn_symbol, 
		   const SYMBOL& symbol, 
	           WN* reduction_carried_by,
                   INT outer_se_reqd, 
		   INT outer_se_not_reqd, 
	           INT defining_def_depth,
		   INT lcd_depth,
		   BOOL finalize,
		   INT non_red_depth ):
 _wn_symbol(wn_symbol), _symbol(symbol), 
 _reduction_carried_by(reduction_carried_by),
 _outer_se_reqd(outer_se_reqd), _outer_se_not_reqd(outer_se_not_reqd),
 _defining_def_depth(defining_def_depth), _finalize(finalize),
 _lcd_depth(lcd_depth), _non_red_depth(non_red_depth) {
 FmtAssert(_outer_se_reqd <= _outer_se_not_reqd,
   ("how can scalar expansion disable transformation? %d %d",
   _outer_se_reqd, _outer_se_not_reqd));
}

void SX_PNODE::Print(FILE* fp) const
{
  fprintf(fp, "<0x%p %s:se=%d,exdp=%d,nose=%d,lcd=%d",
    _wn_symbol, _symbol.Name(), _outer_se_reqd, _defining_def_depth, 
    _outer_se_not_reqd, _lcd_depth);
  if (_reduction_carried_by)
    fprintf(fp, ",red_carried_by=%s(0x%p)",
      SYMBOL(WN_index(_reduction_carried_by)).Name(), _reduction_carried_by);
  fprintf(fp, ">");
}

SX_PNODE::STATUS SX_PNODE::Transformable(INT depth, INT permutation[], 
  INT nloops) const
{
  if (depth >= _outer_se_not_reqd)
    return SX_PNODE::SE_NOT_REQD;
  if (permutation != NULL) { 
    INT i;
    for (i = _defining_def_depth - depth + 1; i < nloops; i++) 
      if (permutation[i] != i) 
	break; 
    if (i == nloops)
      return SX_PNODE::SE_NOT_REQD;
  } 
  if (depth >= _outer_se_reqd)
    return SX_PNODE::SE_REQD; 
  return SX_PNODE::ILLEGAL; 
}

//======================================================================= 
// CLASS: SX_PLIST (List of variables to be scalar expanded) 
// MEMBER FUNCTIONS: ~SX_PLIST, Print, Transformable
//======================================================================= 

SX_PLIST::~SX_PLIST() {
  while (!Is_Empty())
    CXX_DELETE(Remove_Head(), _pool);
}

void SX_PLIST::Print(FILE* fp) const
{
  SX_CONST_PITER i(this);
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    n->Print(fp);
  fprintf(stdout, "\n"); 
}

//======================================================================= 
// CLASS: SX_INFO (List of variables to be scalar expanded with member 
//   functions)  
// MEMBER FUNCTIONS: SX_INFO, Find, Enter, Remove, Print, 
//   First_Transformable_Depth, Make_Sx_Info, Handle_Use, Handle_Def, Walk.   
//======================================================================= 

SX_INFO::SX_INFO(const SX_INFO& pinfo,
                 WN* wn_orig,
                 WN* wn_copy,
                 MEM_POOL* pool) : Plist(pool)
{
  HASH_TABLE<WN*,WN*>* ht = Make_Loop_Mapping(wn_orig, wn_copy, pool);
  for (WN* wn = LWN_Get_Parent(wn_orig); wn; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      ht->Enter(wn, wn);
  SX_CONST_PITER ii(&pinfo.Plist);
  for (const SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    WN* red = NULL;
    if (n->Reduction_Carried_By()) {
      red = ht->Find(n->Reduction_Carried_By());
      if (red == NULL && LNO_Verbose) {
        fprintf(stdout, "pnode: ");
        n->Print(stdout);
        fprintf(stdout, " is broken\n");
      }
      FmtAssert(red, ("Loop 0x%p(%s) had bad pnode reduction",
        wn_orig, SYMBOL(WN_index(wn_orig)).Name()));
    }
    Enter(n->Wn_Symbol(), n->Symbol(), red, n->Outer_Se_Reqd(), 
	  n->Outer_Se_Not_Reqd(),
          n->Expansion_Depth(), n->Lcd_Depth(), n->Finalize());
  }
}

SX_INFO::SX_INFO(const SX_INFO& pinfo,
                 WN* wn_orig,
                 HASH_TABLE<WN*,WN*>* ht,
                 MEM_POOL* pool) : Plist(pool)
{
  for (WN* wn = LWN_Get_Parent(wn_orig); wn; wn = LWN_Get_Parent(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      ht->Enter(wn, wn);
  SX_CONST_PITER ii(&pinfo.Plist);
  for (const SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    WN* red = NULL;
    if (n->Reduction_Carried_By()) {
      red = ht->Find(n->Reduction_Carried_By());
      if (red == NULL && LNO_Verbose) {
        fprintf(stdout, "pnode: ");
        n->Print(stdout);
        fprintf(stdout, " is broken\n");
      }
      FmtAssert(red, ("Loop 0x%p(%s) had bad pnode reduction",
        wn_orig, SYMBOL(WN_index(wn_orig)).Name()));
    }
    Enter(n->Wn_Symbol(), n->Symbol(), red, n->Outer_Se_Reqd(), 
          n->Outer_Se_Not_Reqd(), n->Expansion_Depth(), n->Finalize(), 
	  n->Lcd_Depth());
  }
}

void SX_INFO::Print(FILE* f) const
{
  fprintf(f, "SX_INFO: (ftd=%d): ", First_Transformable_Depth());
  Plist.Print(f);
  fprintf(f, "\n");
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Enter 
// FUNCTION: Enter the symbol 'sym' into the scalar expansion list.  The 
//   symbol is contained in the whirl node 'wn_rd'.  We must scalar expand 
//   this symbol if we are transforming loops with depths satisfying: 
//  	se_depth <= depth < no_se_depth 
//   The symbol is defined at loop depth 'defining_def_depth'. 
//-----------------------------------------------------------------------

void SX_INFO::Enter(WN* wn_symbol, 
		    const SYMBOL& sym, 
		    WN* wn_rd,
                    INT se_depth, 
	            INT no_se_depth,
                    INT defining_def_depth,
		    INT lcd_depth,
		    BOOL finalize,
		    INT non_red_depth)
{
  Is_True(Find(sym) == NULL,
    ("Entering %s twice into SX_INFO", sym.Name()));
  SX_PNODE* n = CXX_NEW(SX_PNODE(wn_symbol, sym, wn_rd, se_depth,
				 no_se_depth, defining_def_depth, 
				 lcd_depth, finalize, non_red_depth), 
			Plist._pool);
  Plist.Append(n);
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Remove 
// FUNCTION: Remove the SX_PNODE 'n' from the SX_INFO. 
//-----------------------------------------------------------------------

void SX_INFO::Remove(SX_PNODE* n)
{
  Plist.Remove(n);
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Find 
// FUNCTION: Returns the first node with symbol 'sym' on the SX_INFO.  
//-----------------------------------------------------------------------

SX_PNODE* SX_INFO::Find(const SYMBOL& sym)
{
  SX_PITER i(&Plist);
  for (SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next()) 
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Find 
// FUNCTION: Returns the first node with symbol 'sym' on the SX_INFO.  
//-----------------------------------------------------------------------

const SX_PNODE* SX_INFO::Find(const SYMBOL& sym) const
{
  SX_CONST_PITER i(&Plist);
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next()) 
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::First_Transformable_Depth 
// FUNCTION: Returns the depth of the loop with the lowest depth number 
//   which can be transformed by scalar expansion according to the symbols
//   on SX_INFO.  If there is st least once scalar which must be expanded,
//   SX_PNODE** 'p' will point to a SX_PNODE* containing the innermost 
//   scalar which must be expanded.   
//-----------------------------------------------------------------------

INT SX_INFO::First_Transformable_Depth(const SX_PNODE** p) const
{
  INT ftd = 0;
  SX_CONST_PITER i(&Plist);
  if (p)
    *p = NULL;
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next()) {
    if (ftd < n->_outer_se_reqd) {
      ftd = n->_outer_se_reqd;
      if (p)
        *p = n;
    }
  }
  return ftd;
}
//-----------------------------------------------------------------------
// NAME: SX_INFO::First_Transformable_Depth 
// FUNCTION: Returns the depth of the loop with the lowest depth number 
//   which can be transformed by considering the reduction, 
//   SX_PNODE** 'p' will point to a SX_PNODE* containing the innermost 
//   scalar which must be expanded or the scalar reduction that can not be
//   expanded   
//-----------------------------------------------------------------------

INT SX_INFO::First_Transformable_Depth_Reduction(const SX_PNODE** p) const
{
  INT ftd = 0;
  SX_CONST_PITER i(&Plist);
  if (p)
    *p = NULL;
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next()) {
    if (ftd < n->_outer_se_reqd) {
      if ( n->_non_red_depth == -1)
	ftd = n->_outer_se_reqd;
      else if ( ftd < n->_defining_def_depth )
	ftd = n->_defining_def_depth;
      if (p)
        *p = n;
    }
  }
  return ftd;
}

//-----------------------------------------------------------------------
// NAME: Index_Variable_Outside_Loop 
// FUNCTION: Returns TRUE if 'wn_use', which is defined at 'wn_def', 
//   is the use of an index variable of one of the loops in the hash table 
//   'loops' but is outside that particular loop.
// NOTE: wn_parent != NULL below guards against 'wn_def' being an ENTRY 
//   node. (MJW)   
//-----------------------------------------------------------------------

static BOOL Index_Variable_Outside_Loop(WN* wn_use, 
					WN* wn_def, 
					HASH_TABLE<WN*,BOOL>* loops) 
{
  WN* wn_parent = LWN_Get_Parent(wn_def);
  return wn_parent != NULL && WN_opcode(wn_parent) == OPC_DO_LOOP 
    && loops->Find(wn_parent) && !Wn_Is_Inside(wn_use, wn_parent);
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Handle_Use 
// FUNCTION:  Enter a symbol in the SX_INFO for the use 'wn_use' if it 
//   is an index variable which is used outside of the loop which it 
//   indexes.  This is a troublesome case that we can't scalar expand. 
//   We are analyzing the loop at depth 'depth'.  The loops in the SNL
//   with depths greater than 'depth' are contained in the hash table 
//   'loops'.  
// NOTE: Check out deflist.  If any defs could possibly depend upon
//   an induction variable inside, and if this load is not inside,
//   then not scalar expandable.  The algorithm implemented here only
//   checks scalars written that are not induction variables, and
//   this fixes things conservative for induction variables. (MJW)
//-----------------------------------------------------------------------

void SX_INFO::Handle_Use(WN* wn_use, 
		         INT depth,
                         HASH_TABLE<WN*,BOOL>* loops)
{
  DEF_LIST* dl = Du_Mgr->Ud_Get_Def(wn_use);
  if (dl == NULL) {
    DevWarn("missing def list for use (%s) while gathering sx info",
      SYMBOL(wn_use).Name());
    return;
  }
  DEF_LIST_ITER iter(dl);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_def = node->Wn();
    if (Index_Variable_Outside_Loop(wn_use, wn_def, loops)) {
      Enter(wn_use, SYMBOL(wn_use), NULL, depth + 1, depth + 1, 0, -1, FALSE);
      if (LNO_Verbose)
        fprintf(stdout, "ivar %s used outside loop makes untransformable\n",
                (SYMBOL(wn_use)).Name());
      SNL_DEBUG1(2, "ivar %s used outside loop makes untransformable\n",
                 (SYMBOL(wn_use)).Name());
      return;
    }
  }
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Handle_Index_Variable_Def
// FUNCTION:  If the index variable definition 'wn_def' is aliased to some 
//   symbol other than itself, this is an unexpandable scalar, so add or 
//   update an entry in the SX_INFO indicating this.  We are analyzing the 
//   loop at 'depth' for scalar expandable variables.  Here 'wn_rep_def' 
//   is the definition which has already been entered into the SX_INFO, 
//   if there already is one. 
// NOTE: So long as all uses on the def/use list point to this symbol, then
//   Handle_Use() is doing the right thing looking only at induction 
//   variables. (MJW) 
//-----------------------------------------------------------------------

void SX_INFO::Handle_Index_Variable_Def(WN* wn_def, 
					WN* wn_rep_def, 
					INT depth) 
{
  if (wn_rep_def != NULL) 
    return;
  SYMBOL sym(wn_def);
  USE_LIST* ul = Du_Mgr->Du_Get_Use(wn_def);
  USE_LIST_ITER iter(ul);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (!OPCODE_has_sym(WN_opcode(wn_use)) || sym != SYMBOL(wn_use)) {
      // Enter the induction variable.  In theory, it could already be
      // there, in which case just update it. (MJW)
      SX_PNODE* pnode = Find(SYMBOL(wn_def));
      if (pnode == NULL) {
	Enter(wn_def, SYMBOL(wn_def), NULL, depth + 1, depth + 1, 0, -1, FALSE);
      } else {
	pnode->_outer_se_reqd = depth + 1;
	pnode->_outer_se_not_reqd = depth + 1;
	pnode->_reduction_carried_by = NULL;
	pnode->_defining_def_depth = 0;
      }
      const char* name = OPCODE_has_sym(WN_opcode(wn_use)) 
	? (SYMBOL(wn_use)).Name() : "<NONAME>";
      if (LNO_Verbose)
	fprintf(stdout, "ivar %s has link to use %s\n", sym.Name(), name);
      SNL_DEBUG2(2, "ivar %s has link to use %s\n", sym.Name(), name); 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Check_Loop_Statement
// FUNCTION: Assert if the 'wn_loop_stmt' is not a loop enclosing 'wn'.  
//-----------------------------------------------------------------------

static void Check_Loop_Statement(WN* wn, 
			         WN* wn_loop_stmt) 
{
#ifdef Is_True_On
  if (wn_loop_stmt == NULL) 
    return; 

  WN *parent;
  for (parent = wn; parent; parent = LWN_Get_Parent(parent)) 
    if (parent == wn_loop_stmt)
      break;
  const INT bufsz = 64;
  char buf[bufsz];
  FmtAssert(parent, ("Bad loop_stmt %s(0x%p) for %s(0x%p)",
    SYMBOL(WN_index(wn_loop_stmt)).Name(), wn_loop_stmt, 
    SYMBOL(wn).Name(buf,bufsz), wn));
#endif
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Analyze_Reduction 
// FUNCTION: For the STID 'wn_def' inside the SNL whose outermost loop is 
//   of depth 'outer' and which is part of the 'equivalence_class' return 
//   TRUE if the normally non-expandable parts are part of a reduction. 
//   In this case, three values are returned: 
//     WN** wn_non_red_def_ptr: Pointer to a WN* which is the unique 
//      definition of SYMBOL(wn_def) which is not part of the reduction. 
//     INT* non_red_depth_ptr: Pointer to the loop depth of all of the 
//      SYMBOL(wn_def) references which are not part of the reduction. 
//      These must all be the same.
//     WN** wn_red_loop_stmt_ptr: The loop statement of the reduction.  
//   As above 'dostack' is the stack of all loops in the SNL and all 
//   loops enclosing it, with dostack->Bottom_nth(i) the loop at depth 'i'. 
//  EXAMPLE:  
//     do i = 1, n		 
//       s = a(i) 		! This "s" is 'wn_non_red_def'
//       do j = 1, n		! Hence 'non_red_depth' == 0 
//         s = s + b(i,j)	! 'wn_red_loop_stmt' is the "j" loop  
//       end do 
//     end do 
//-----------------------------------------------------------------------

BOOL SX_INFO::Analyze_Reduction(WN* wn_def, 
				INT outer, 
				STACK<WN*>* equivalence_class, 
  				DOLOOP_STACK* dostack, 
				WN** wn_non_red_def_ptr, 
				INT* non_red_depth_ptr,
				WN** wn_red_loop_stmt_ptr)
{
  REDUCTION_TYPE rt_old = RED_NONE;
  BOOL bad = red_manager == NULL ? TRUE : FALSE;
  INT non_red_depth = -1;
  WN* wn_non_red_def = NULL;
  WN* wn_loop_stmt = NULL;
  WN* wn_red_loop_stmt = NULL;
  if (bad) {
    *wn_non_red_def_ptr = wn_non_red_def; 
    *non_red_depth_ptr = non_red_depth; 
    *wn_red_loop_stmt_ptr = wn_red_loop_stmt; 
    return TRUE; 
  }

  INT elts = equivalence_class->Elements();
  INT i;
  for (i = 0; i < elts; i++) {

    WN* wn = equivalence_class->Bottom_nth(i);
    OPERATOR opr = WN_operator(wn);
    if (opr != OPR_LDID && opr != OPR_STID || SYMBOL(wn_def) != SYMBOL(wn)) { 
      DevWarn("Bad equivalence class");
      break;
    }

    INT dwn = Which_Loop_Inside(wn, dostack);
    if (dwn < outer)
      continue;

    REDUCTION_TYPE rt_new = red_manager->Which_Reduction(wn);
    if (rt_new == RED_NONE) {
      // Need unique non_red_depth 
      if (non_red_depth != -1 && non_red_depth != dwn) 
        break;
      non_red_depth = dwn;
      if (opr == OPR_STID) {
	// Need unique wn_non_red_def 
	if (wn_non_red_def != NULL)
	  break;
	wn_non_red_def = wn;
      }
    } else {
      // Need everything inside to be the same reduction
      if (rt_old != RED_NONE && rt_old != rt_new) 
	break;
      rt_old = rt_new;
      if (opr == OPR_LDID) {
	WN* wn_loop_stmt = Du_Mgr->Ud_Get_Def(wn)->Loop_stmt();
	Check_Loop_Statement(wn, wn_loop_stmt); 
        // Need red_manager to process reductions. 
	if (wn_loop_stmt != NULL && red_manager == NULL) 
	  break;
	if (wn_red_loop_stmt == NULL)
	  wn_red_loop_stmt = wn_loop_stmt;
	// Need unique reduction loop statement. 
	if (wn_red_loop_stmt != wn_loop_stmt) 
	  break;
      }
    }
  }
  *wn_non_red_def_ptr = wn_non_red_def; 
  *non_red_depth_ptr = non_red_depth; 
  *wn_red_loop_stmt_ptr = wn_red_loop_stmt; 
  return i < elts; 
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Handle_Other_Def
// FUNCTION: For 'wn_def' which is not an index variable, add an entry to 
//   the SX_INFO which describes the conditions under which it is scalar 
//   expandable.  The 'dostack' contains the SNL of which 'wn_def' is a 
//   part and all enclosing loops.  The SNL has outer loop of depth 'outer'
//   and inner loop of depth 'inner'.  Here 'wn_rep_def' is the definition 
//   which has already been entered into the SX_INFO, if there already is 
//   one.
//-----------------------------------------------------------------------

void SX_INFO::Handle_Other_Def(WN* wn_def, 
			       WN* wn_rep_def, 
			       INT outer, 
			       INT inner, 
	                       INT depth, 
			       DOLOOP_STACK* dostack) 
{
  WN* wn_outer_loop = dostack->Bottom_nth(outer); 
  WN* wn_loop = dostack->Bottom_nth(depth);
  WN* wn_eq_loop = NULL; 
  STACK<WN*>* equivalence_class = Scalar_Equivalence_Class(wn_def, 
     Du_Mgr, &LNO_local_pool, TRUE, &wn_eq_loop);

  // Worst case defaults
  INT se_reqd = depth + 1;
  INT se_not_reqd = depth + 1;
  INT defining_def_depth = 0; // value doesn't matter yet
  WN* wn_red_loop_stmt = NULL;

  if (Get_Trace(TP_LNOPT2, TT_LNO_DISABLE_SEFIN)) {
    CXX_DELETE(equivalence_class, &LNO_local_pool); 
    equivalence_class = NULL; 
  }
 
  // Another def is OK as long as it is in the same equivalence class
  // and is not at a different depth. 
  if (wn_rep_def != NULL) { 
    BOOL in_eq_class = FALSE; 
    if (equivalence_class != NULL) {
      INT i;
      for (i = 0; i < equivalence_class->Elements(); i++) 
	if (equivalence_class->Bottom_nth(i) == wn_rep_def)
	  break; 
      if (i < equivalence_class->Elements())
        in_eq_class = TRUE; 
    } 
    SX_PNODE* sxp = Find(SYMBOL(wn_rep_def));

    // a reduction definition need not be at the same depth
    // as the defining def depth.
    //  example:
    //           I:
    //            J: 
    //              x = S()  (1)
    //              L:
    //                x = x +  (2)
    //                M:
    //                  x = x +  (3)
    //              END
    //              S() = x (4)
    //            END
    //           END
    // In this example defining depth is (1) and we have 
    // reduction defs at (2) and (3). 

    if (sxp->_reduction_carried_by != NULL)
      return; 

    if (!in_eq_class || depth != sxp->_defining_def_depth) { 
      sxp->_outer_se_reqd = depth + 1;
      sxp->_outer_se_not_reqd = depth + 1;
      sxp->_defining_def_depth = 0; 
      sxp->_reduction_carried_by = NULL; 
    }  
    CXX_DELETE(equivalence_class, &LNO_local_pool);
    return; 
  }

  if (equivalence_class == NULL 
      || wn_eq_loop != NULL && Do_Loop_Depth(wn_eq_loop) > outer) {
    // Worst case.  Could not even build a proper equivalence class. 
    Enter(wn_def, SYMBOL(wn_def), wn_red_loop_stmt, se_reqd, se_not_reqd, 
      defining_def_depth, -1, FALSE);
    return; 
  }

  SE_RESULT se_result = Scalar_Expandable(equivalence_class, wn_def, 
    wn_loop, Du_Mgr, wn_outer_loop, wn_eq_loop);
  INT lcd_depth = -1; 
  if (se_result == SE_EASY_LCD || se_result == SE_HARD_LCD) { 
    for (INT i = 0; i < equivalence_class->Elements(); i++) {
      WN* wn = equivalence_class->Bottom_nth(i);
      if (WN_operator(wn) == OPR_LDID) {
	DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn);
	if (def_list->Loop_stmt() && (lcd_depth == -1 
	    || Do_Loop_Depth(def_list->Loop_stmt()) < lcd_depth))
	  lcd_depth = Do_Loop_Depth(def_list->Loop_stmt());
      }
    }
  } 
  if (se_result != SE_NONE) { 
    // We traverse loops from innermost to outermost, but for a given 
    // subnest, we traverse outermost in.  This is the first def seen 
    // within this subnest.  There are no reductions inside, so only  
    // uses if anything.  The ideal case.
    se_reqd = outer;
    se_not_reqd = depth + 1;
    defining_def_depth = depth;
    if (depth >= inner || Scalar_Expansion_Not_Necessary(wn_def, Du_Mgr)) {
      // Private in the inner body, so no need to expand at all.
      // Or local to an imperfect nest, where we also don't need to expand.
      se_not_reqd = outer;
    }
    Enter(wn_def, SYMBOL(wn_def), wn_red_loop_stmt, se_reqd, se_not_reqd, 
      defining_def_depth, lcd_depth, se_result == SE_HARD || se_result 
      == SE_HARD_LCD); 
    CXX_DELETE(equivalence_class, &LNO_local_pool);
    return; 
  }

  // This is a def, but it's not privatizable at this level.  We'd tend
  // to give up, but there is one case we'd like to catch.  If this def
  // is part of a reduction, then let's try.  Specifically, find the
  // outermost def.  If everything else is part of a reduction, then
  // it's scalar expandable with no dependences.  Otherwise, die.

  WN* wn_non_red_def = NULL;
  INT non_red_depth = -1; 
  BOOL bad = Analyze_Reduction(wn_def, outer, equivalence_class, dostack, 
    &wn_non_red_def, &non_red_depth, &wn_red_loop_stmt);  
  if (bad) {
    // Could not analyze this as a special case. Use worst case defaults.
    Enter(wn_def, SYMBOL(wn_def), wn_red_loop_stmt, se_reqd, se_not_reqd, 
      defining_def_depth, -1, FALSE);
    CXX_DELETE(equivalence_class, &LNO_local_pool);
    return; 
  }
  if (non_red_depth < outer) {
    // Everything's a reduction, so no problems at all
    se_reqd = outer;
    se_not_reqd = outer;
  } else if (wn_non_red_def) {
     se_result = Scalar_Expandable(equivalence_class, 
       wn_non_red_def, dostack->Bottom_nth(non_red_depth), Du_Mgr, 
       wn_outer_loop, wn_eq_loop);
     if (se_result != SE_NONE) { 
       se_reqd = outer;
       se_not_reqd = non_red_depth + 1; // Just inside the definition
       defining_def_depth = non_red_depth;
     }
  }
  Enter(wn_def, SYMBOL(wn_def), wn_red_loop_stmt, se_reqd, se_not_reqd, 
    defining_def_depth, lcd_depth, se_result == SE_HARD 
	|| se_result == SE_HARD_LCD, non_red_depth); 
  CXX_DELETE(equivalence_class, &LNO_local_pool);
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Handle_Def
// FUNCTION: Add an entry to the SX_INFO for this STID 'wn_def' which is  
//   defined on the SNL with outer loop of depth 'outer' and inner loop of
//   depth 'inner'.  All of the loops of the SNL and the loops containing 
//   the SNL are given in 'dostack'.  We are analyzing the loop of depth 
//   'depth'.  
//-----------------------------------------------------------------------

void SX_INFO::Handle_Def(WN* wn_def, 
			 WN* wn_rep_def, 
			 INT outer, 
			 INT inner, 
	                 INT depth, 
			 DOLOOP_STACK* dostack) 
{
  if (WN_opcode(LWN_Get_Parent(wn_def)) == OPC_DO_LOOP) {
    Handle_Index_Variable_Def(wn_def, wn_rep_def, depth); 
  } else {
    Handle_Other_Def(wn_def, wn_rep_def, outer, inner, depth, dostack); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: SX_INFO::Walk 
// FUNCTION: Walk the tree rooted at 'wn', adding scalar expansion symbols 
//   for the SNL containing 'wn' whose outer loop has depth 'outer' and 
//   whose inner loop has depth 'inner' and which is being analyzed at  
//   'depth'.  The hash table 'loops' contains all of the loops in the 
//   SNL with depths greater than 'depth'.  The stack 'dostack' contains 
//   a list of all loops in the SNL and contained loops, such that 
//   'dostack->Bottom_nth(i)' is the loop in or contained by the SNL at 
//   depth 'i'. 
//-----------------------------------------------------------------------

void SX_INFO::Walk(WN* wn, 
		   INT outer, 
		   INT inner, 
	           INT depth,
                   HASH_TABLE<WN*,BOOL>* loops, 
		   DOLOOP_STACK* dostack)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_DO_LOOP) {
    // don't enter inside do loops, except when _depth_inner == depth,
    // because we've already explored that in previous invocations
    // of this routine.
    if (depth != inner)
      return;
  }

  if (opc == OPC_BLOCK) {
    for (WN* wnn = WN_first(wn); wnn; wnn = WN_next(wnn))
      Walk(wnn, outer, inner, depth, loops, dostack);
  } else {
    OPERATOR opr = OPCODE_operator(opc);
    if (opr == OPR_STID) {
      SX_PNODE* sxp = Find(SYMBOL(wn)); 
      WN* wn_rep_def = sxp != NULL ? sxp->Wn_Symbol() : NULL; 
      Handle_Def(wn, wn_rep_def, outer, inner, depth, dostack);
    } else if (opr == OPR_LDID) {
      if (Find(SYMBOL(wn)) == NULL)
        Handle_Use(wn, depth, loops);
    }
    for (INT k = 0; k < WN_kid_count(wn); k++)
      Walk(WN_kid(wn, k), outer, inner, depth, loops, dostack);
  }
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Make_Sx_Info 
// FUNCTION: Add symbols to the SX_INFO for the SNL whose outermost loop 
//   is 'wn_outer' and whose innermost loop is 'wn_inner'.  If 'ignore_
//   illegal' is TRUE, we continue processing even when we have found a 
//   scalar that can't be expanded.  
//-----------------------------------------------------------------------

void SX_INFO::Make_Sx_Info(WN* wn_outer,
			   INT nloops,
			   BOOL ignore_illegal)
{
  INT td = 0;
  const SX_PNODE* p = NULL;
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  INT outer = Do_Loop_Depth(wn_outer); 
  INT inner = Do_Loop_Depth(wn_inner); 
  DOLOOP_STACK dostack(&LNO_local_pool); 
  Build_Doloop_Stack(wn_inner, &dostack); 
  for (INT d = inner; d >= outer; d--) {
    WN* wn_body = WN_do_body(dostack.Bottom_nth(d));
    HASH_TABLE<WN*,BOOL>* ht = Find_Loops_Within(wn_body, &LNO_local_pool);
    Walk(wn_body, outer, inner, d, ht, &dostack);
    td = First_Transformable_Depth(&p);
    CXX_DELETE(ht, &LNO_local_pool);
    if (!ignore_illegal && td >= d)
      break;
  }

  if (td > outer) {
    FmtAssert(p != NULL, ("Bug return val of First_Transformable_Depth()"));
    SNL_DEBUG3(1, "Loop %s (depth %d) has priv problems from sym %s",
               (SYMBOL(WN_index(dostack.Bottom_nth(td-1)))).Name(), td-1,
               p->Symbol().Name());
  }
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Lcd_Depth()
// FUNCTION: Returns TRUE if some variable on the list requires scalar 
//   expansion with LCDs.  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

INT SX_INFO::Lcd_Depth()
{
  INT lcd_depth = -1; 
  SX_CONST_PITER i(&Plist);
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Lcd_Depth() >= 0 && (lcd_depth == -1 || n->Lcd_Depth() < lcd_depth))
      lcd_depth = n->Lcd_Depth(); 
  return lcd_depth;
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Must_Finalize()
// FUNCTION: Returns TRUE if some variable on the list requires finaliza-
//   tion.  Returns FALSE otherwise.  
//-----------------------------------------------------------------------

INT SX_INFO::Must_Finalize()
{
  SX_CONST_PITER i(&Plist);
  for (const SX_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Finalize())
      return TRUE; 
  return FALSE; 
}

//-----------------------------------------------------------------------
// NAME: SX_INFO::Update_Reduction_Loop_Stmts()
// FUNCTION: Update the reduction loop statements stored in the SX_INFO.
//-----------------------------------------------------------------------

void SX_INFO::Update_Reduction_Loop_Stmts(WN* wn_inner) 
{
  DOLOOP_STACK loop_stack(&LNO_local_pool);
  Build_Doloop_Stack(wn_inner, &loop_stack);
  SX_PITER ix(&Plist);
  SX_PNODE* nn = NULL;
  for (SX_PNODE* n = ix.First(); !ix.Is_Empty(); n = nn) {
    nn = ix.Next(); 
    if (n->Reduction_Carried_By() != NULL) {
      // Remove items which are no longer in the SNL
      WN* wn_def = n->Wn_Symbol();
      WN* wn_enclosing_loop = Enclosing_Loop(wn_def);
      INT i;
      for (i = 0; i < loop_stack.Elements(); i++) { 
	WN* wn_loop = loop_stack.Bottom_nth(i); 
	if (wn_loop == wn_enclosing_loop) 
          break; 
      }
      if (i == loop_stack.Elements()) { 
        Remove(n);
        continue; 
      } 
      REDUCTION_TYPE rt = red_manager->Which_Reduction(wn_def);
#ifdef KEY // Bug 6288 - if not part of a reduction, continue.
      if (rt == RED_NONE) 
	continue;
#else
      FmtAssert(rt != RED_NONE, ("Should be part of a reduction"));
#endif
      USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn_def);
      FmtAssert(use_list != NULL, ("Expected a use list"));
      USE_LIST_ITER iter(use_list);
      const DU_NODE* node = NULL;
      WN* wn_use = NULL;
      for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
        wn_use = node->Wn();
	if (!Wn_Is_Inside(wn_use, wn_def))
          continue;
	REDUCTION_TYPE rt_use = red_manager->Which_Reduction(wn_use);
        if (rt_use != RED_NONE)
          break;
      }
      FmtAssert(wn_use != NULL, ("Need to find at least one use"));
      WN* wn_loop_stmt = Du_Mgr->Ud_Get_Def(wn_use)->Loop_stmt();
      n->Set_Reduction_Carried_By(wn_loop_stmt);
    }
  }
}

