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


// -*-C++-*-

/**
*** This file implements finding information about singly nested loop nests
*** and finding those eligible for transformation.
***
*** The bounds information in SNL_NEST_INFO is canonicized so
*** that the first variable corresponds to the outermost found loop, etc.
*** The conditionals use the same variables as are used in Bi.
***/

/** $Revision: 1.5 $
*** $Date: 04/12/21 14:57:16-08:00 $
*** $Author: bos@eng-25.internal.keyresearch.com $
*** $Source: /home/bos/bk/kpro64-pending/be/lno/SCCS/s.snl_xbounds.cxx $
**/

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define snl_nest_CXX      "snl_nest.cxx"
const static char *rcs_id =   snl_nest_CXX "$Revision: 1.5 $";

#include <sys/types.h>
#include "snl.h"
#include "snl_xbounds.h"

#include "soe.h"
#include "lwn_util.h"
#include "lnopt_main.h"
#include "opt_du.h"

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// SNL_BOUNDS_SYMBOL_...
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

void SNL_BOUNDS_SYMBOL_LIST::Print(FILE *fp) const
{
  SNL_BOUNDS_SYMBOL_CONST_ITER iter(this);
  for (const SNL_BOUNDS_SYMBOL_NODE *node = iter.First(); !iter.Is_Empty(); 
	node=iter.Next()) {
    node->Print(fp);
    if (iter.Peek_Next() != NULL)
      fprintf(fp, ",");
  }
} 

void SNL_BOUNDS_SYMBOL_LIST::Init(const SNL_BOUNDS_SYMBOL_LIST *sl)
{
  SNL_BOUNDS_SYMBOL_CONST_ITER iter(sl);
  for (const SNL_BOUNDS_SYMBOL_NODE *node = iter.First(); !iter.Is_Empty(); 
	node = iter.Next()) {
	Append(CXX_NEW(SNL_BOUNDS_SYMBOL_NODE(node), _pool));
  }
}

SNL_BOUNDS_SYMBOL_LIST::~SNL_BOUNDS_SYMBOL_LIST()
{
  while (!Is_Empty())
    CXX_DELETE(Remove_Headnode(), _pool);
}

void SNL_BOUNDS_SYMBOL_NODE::Print(FILE *fp) const
{
  Symbol.Print(fp);
}


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// SNL_BOUNDS_INFO
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// Constructors and initialization
//-----------------------------------------------------------------------

SNL_BOUNDS_INFO::SNL_BOUNDS_INFO(MEM_POOL* mp)
  : _pool(mp),
    _outermost_depth(0),
    _bounds(0,0,0,mp),
    _conditionals(0,0,0,mp),
    _var_info(mp)
{
}

SNL_BOUNDS_INFO::SNL_BOUNDS_INFO(const SNL_BOUNDS_INFO* bi, MEM_POOL* mp)
  : _pool(mp ? mp : bi->_pool),
    _outermost_depth(bi->_outermost_depth),
    _bounds(&bi->_bounds, mp ? mp : bi->_pool),
    _conditionals(&bi->_conditionals, mp ? mp : bi->_pool),
    _var_info(mp ? mp : bi->_pool)
{
  Is_True(Pool() != &LNO_local_pool,
	  ("SNL_BOUNDS_INFO cannot use LNO_local_pool"));
  _var_info.Init(&bi->_var_info);
}

SNL_BOUNDS_INFO::~SNL_BOUNDS_INFO()
{
}

//-----------------------------------------------------------------------
// Adding an equation
//-----------------------------------------------------------------------

INT SNL_BOUNDS_INFO::Lookup_Entry(SYMBOL s, WN* alias_wn)
{
  SNL_BOUNDS_SYMBOL_ITER iter(&Var_Info());
  SNL_BOUNDS_SYMBOL_NODE* n;
  INT entry = 0;

  for (n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
    if (n->Symbol == s)
      return entry;
    entry++;
  }

  // previously unseen
  _var_info.Append(CXX_NEW(SNL_BOUNDS_SYMBOL_NODE(s, alias_wn), _pool));
  _bounds.Add_Vars(1);
  _conditionals.Add_Vars(1);
  return entry;
}

// Add this equation to the soe, possibly adding additional symbols to 
// the Var_Info().  An access vector expresses ai+bj <= const_offset,
// which is exactly what an soe wants.  But the soe expects the variables
// to come in a certain order, so look up variables in the list.  Note
// that, for now, loop indices are represented by the symbol (0,0,i), so
// that they are easy for the canonicize routine to recognize.  This may
// be stupid, but its never visible after canonicization, and nothing should
// be visible before anyway.

INT SNL_BOUNDS_INFO::Add_Access(ACCESS_VECTOR* av, BOOL is_cond)
{
  if (av->Too_Messy || av->Contains_Non_Lin_Symb())
    return 0;

  INT		vsz = (av->Lin_Symb ? av->Lin_Symb->Len() : 0) +
			Var_Info().Len() + av->Nest_Depth() + 1;
  mINT32*	v = CXX_NEW_ARRAY(mINT32, vsz, &LNO_local_pool);

  for (INT vszx = 0; vszx < vsz; vszx++)
    v[vszx] = 0;

  // put loop indices into v.

  for (INT i = 0; i <= av->Nest_Depth(); i++) {
    INT coeff = av->Loop_Coeff(i);
    if (coeff) {
      INT entry = Lookup_Entry(SYMBOL((ST*)NULL, i, MTYPE_V), NULL);
      Is_True(entry < vsz,("Overflow1 in Add_Access\n"));
      v[entry] = coeff;
    }
  }

  // add symbolic constants

  if (av->Contains_Lin_Symb()) {
    INTSYMB_ITER ii(av->Lin_Symb);
    for (INTSYMB_NODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
      INT entry = Lookup_Entry(n->Symbol, NULL);
      Is_True(entry < vsz,("Overflow2 in Add_Access\n"));
      v[entry] = n->Coeff;
    }
  }

  _bounds.Add_Le(v, av->Const_Offset);
  if (is_cond)
    _conditionals.Add_Le(v, av->Const_Offset);

  CXX_DELETE_ARRAY(v, &LNO_local_pool);
  return 1;
}

INT SNL_BOUNDS_INFO::Add_Access(ACCESS_ARRAY* aa, BOOL is_cond)
{
  INT added_cnt = 0;

  for (INT i = 0; i < aa->Num_Vec(); i++)
    added_cnt += Add_Access(aa->Dim(i), is_cond);

  return added_cnt;
}

//-----------------------------------------------------------------------
// Collecting equations from structured control flow
//-----------------------------------------------------------------------

void SNL_BOUNDS_INFO::Collect_If_Info(WN* wn, BOOL in_then_part)
{
  Is_True(WN_opcode(wn) == OPC_IF,
	  ("bad opcode %d for Collect_If_Info()", WN_opcode(wn)));

  MEM_POOL_Push(&LNO_local_pool);
  IF_INFO* ii = Get_If_Info(wn);

  if (ii == NULL) {
    fprintf(stderr, "Missing IF annotation!");
  }
  else if (!in_then_part == !ii->Condition_On_Then) {
    // the access vector applies to us
    Add_Access(ii->Condition, FALSE);
  }
  else {
    // If more than one condition, a disjunction and we are hosed.
    // But if exactly one, then just reverse the condition
    if (ii->Condition->Num_Vec() == 1) {
      ACCESS_VECTOR av(ii->Condition->Dim(0), Pool());
      av.Negate_Me();
      av.Const_Offset--;
      Add_Access(&av, FALSE);
    }
  }
  MEM_POOL_Pop(&LNO_local_pool);
}

void SNL_BOUNDS_INFO::Collect_Do_Info(WN* wn)
{
  Is_True(WN_opcode(wn) == OPC_DO_LOOP,
	  ("bad opcode %d for Collect_Do_Info()", WN_opcode(wn)));

  if (Step_Size(wn) != 1)
    return;

  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
  Add_Access(dli->LB, FALSE);
  Add_Access(dli->UB, FALSE);
}

void SNL_BOUNDS_INFO::Collect_DoWhile_Info(WN* wn)
{
  FmtAssert(WN_opcode(wn) == OPC_DO_WHILE,
	    ("bad opcode %d for Collect_DoWhile_Info()", WN_opcode(wn)));
}

void SNL_BOUNDS_INFO::Collect_WhileDo_Info(WN* wn)
{
  FmtAssert(WN_opcode(wn) == OPC_WHILE_DO,
	    ("bad opcode %d for Collect_WhileDo_Info()", WN_opcode(wn)));
}

// Examine all parents, looking for conditions that we can add to the
// system of equations.

void SNL_BOUNDS_INFO::Collect_Outer_Info(WN* wn)
{
  WN* pwn = wn;

  for (wn = LWN_Get_Parent(pwn); wn; pwn = wn, wn = LWN_Get_Parent(pwn)) {
    switch (WN_opcode(wn)) {
     case OPC_IF:
      BOOL in_then_part;
      if (pwn == WN_then(wn))
	in_then_part = TRUE;
      else if (pwn == WN_else(wn))
	in_then_part = FALSE;
      else
	FmtAssert(0, ("Bad if/then/else prev condition"));
      Collect_If_Info(wn, in_then_part);
      break;
     case OPC_DO_LOOP:
      Collect_Do_Info(wn);
      break;
     case OPC_DO_WHILE:
      Collect_DoWhile_Info(wn);
      break;
     case OPC_WHILE_DO:
      Collect_WhileDo_Info(wn);
      break;
    }
  }
}

void SNL_BOUNDS_INFO::Reset_Varcount_To(INT cols)
{
  INT len = _var_info.Len();

  Is_True(cols <= len, ("Reset_Varcount_To() len=%d, cols=%d", len, cols));
  if (len >= cols)
    return;

  // something to remove

  SNL_BOUNDS_SYMBOL_NODE* prev = NULL;
  SNL_BOUNDS_SYMBOL_NODE* node = _var_info.Head();
  for (INT cnt = 0; cnt < cols; cnt++) {
    prev = node;
    node = node->Next();
  }

  while (node) {
    SNL_BOUNDS_SYMBOL_NODE* next = node->Next();
    _var_info.Remove(prev, node);
    CXX_DELETE(node, Pool());
    node = next;
  }
}

void SNL_BOUNDS_INFO::Reset_Conditionals_To(INT rows_le, INT rows_eq, INT cols)
{
  Reset_Varcount_To(cols);
  Conditionals().Reset_To(rows_le, rows_eq, cols);
  Bounds().Reset_To(Bounds().Num_Le_Constraints(),
		    Bounds().Num_Eq_Constraints(),
		    cols);
}

void SNL_BOUNDS_INFO::Reset_Bounds_To(INT rows_le, INT rows_eq, INT cols)
{
  Reset_Varcount_To(cols);
  Bounds().Reset_To(rows_le, rows_eq, cols);
  Conditionals().Reset_To(Conditionals().Num_Le_Constraints(),
			  Conditionals().Num_Eq_Constraints(),
			  cols);
}

// Swap the variables in the system of equations so that the 0-th loop
// index (in the nest!) is the 0th variable in the system of equations,
// and so on.  Also, index variables need to be given a real symbol.

void SNL_BOUNDS_INFO::Canonicize(INT nloops, DOLOOP_STACK* stack, INT stk_first)
{
  SYSTEM_OF_EQUATIONS*		soeb = &Bounds();
  SYSTEM_OF_EQUATIONS*		soec = &Conditionals();

  SNL_BOUNDS_SYMBOL_LIST*	sl = &Var_Info();
  SNL_BOUNDS_SYMBOL_NODE*	sld = sl->Head();

  for (INT d = 0; d < nloops; d++, sld = sld->Next()) {
    SNL_BOUNDS_SYMBOL_NODE*	sldd = sld;

    for (INT dd = d; sldd; dd++, sldd = sldd->Next()) {
      if (sldd->Symbol == SYMBOL((ST*)NULL, _outermost_depth+d, MTYPE_V)) {
	if (sld->Symbol != sldd->Symbol) {
	  SYMBOL		tmps = sld->Symbol;
	  WN*			tmpw = sld->Alias_Wn;

	  sld->Symbol = sldd->Symbol;
	  sld->Alias_Wn = sldd->Alias_Wn;

	  sldd->Symbol = tmps;
	  sldd->Alias_Wn = tmpw;

	  soeb->Ale().D_Swap_Cols(d,dd);
	  soeb->Aeq().D_Swap_Cols(d,dd);
	  soec->Ale().D_Swap_Cols(d,dd);
	  soec->Aeq().D_Swap_Cols(d,dd);
	}
	break;
      }
    }
    FmtAssert(sldd, ("Couldn't find loop %d", Outermost_Depth() + d));
  }

  for (sld = sl->Head(); sld; sld = sld->Next()) {
    if (sld->Symbol.St() == NULL) {
      WN* doloop = stack->Bottom_nth(sld->Symbol.WN_Offset());
      sld->Symbol = SYMBOL(WN_index(doloop));
      sld->Symbol.Type = Do_Wtype(doloop);
      sld->Alias_Wn = Find_Use_In_Exp(WN_step(doloop), sld->Symbol);
    }
    else {
      // this symbol must appear in a loop bound or conditional somewhere
      WN* wn = stack->Bottom_nth(stk_first+nloops-1);
      for ( ; wn; wn = LWN_Get_Parent(wn)) {
	if (WN_opcode(wn) == OPC_IF) {
	  sld->Alias_Wn = Find_Use_In_Exp(WN_if_test(wn), sld->Symbol);
	  if (sld->Alias_Wn)
	    break;
	}
	else if (WN_opcode(wn) == OPC_DO_LOOP) {
	  sld->Alias_Wn = Find_Use_In_Exp(WN_start(wn), sld->Symbol);
          if (sld->Alias_Wn)
            break;
          sld->Alias_Wn = Find_Use_In_Exp(WN_end(wn), sld->Symbol);
          if (sld->Alias_Wn)
            break;
          sld->Alias_Wn = Find_Use_In_Exp(WN_step(wn), sld->Symbol);
          if (sld->Alias_Wn)
            break;
	}
      }
    }
    FmtAssert(sld->Alias_Wn,
	      ("Missing alias for %s\n", SYMBOL(sld->Symbol).Name()));
  }
}

static void cshift_left(mINT32* row, INT rowsize, INT shft)
{
  INT i;

  MEM_POOL_Push(&LNO_local_pool);

  mINT32* hold = CXX_NEW_ARRAY(mINT32, shft, &LNO_local_pool);

  for (i = 0; i < shft; i++)
    hold[i] = row[i];
  for (i = shft; i < rowsize; i++)
    row[i-shft] = row[i];
  for (i = 0; i < shft; i++)
    row[rowsize-shft+i] = hold[i];

  CXX_DELETE_ARRAY(hold, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
}

void SNL_BOUNDS_INFO::Exclude_Outer_Loops(INT how_many)
{
  _outermost_depth += how_many;

  // recanonicize, so the first how_many columns of the system of equations
  // become the last.

  INT cols = _var_info.Len();

  IMAT& m1 = _bounds.Aeq();
  IMAT& m2 = _bounds.Ale();
  IMAT& m3 = _conditionals.Aeq();
  IMAT& m4 = _conditionals.Ale();

  BOOL m1_valid = m1.Rows() > 0;
  BOOL m2_valid = m2.Rows() > 0;
  BOOL m3_valid = m3.Rows() > 0;
  BOOL m4_valid = m4.Rows() > 0;

  FmtAssert((!m1_valid || cols == m1.Cols()) &&
	    (!m2_valid || cols == m2.Cols()) &&
	    (!m3_valid || cols == m3.Cols()) &&
	    (!m4_valid || cols == m4.Cols()),
	    ("Bad number of cols in Exclude_Outer_Loops"));

  for (INT i = 0; i < how_many; i++) {
    SNL_BOUNDS_SYMBOL_NODE* n = _var_info.Remove_Headnode();
    _var_info.Append(n);
  
    // a very inefficient way to do a circular rotation, but unlikely to matter
    for (INT c = 0; c < cols-1; c++) {
      if (m1_valid) m1.D_Swap_Cols(c,c+1);
      if (m2_valid) m2.D_Swap_Cols(c,c+1);
      if (m3_valid) m3.D_Swap_Cols(c,c+1);
      if (m4_valid) m4.D_Swap_Cols(c,c+1);
    }
  }
}

void SNL_BOUNDS_INFO::Print(FILE* f) const
{
  fprintf(f, "Bounds Info: Outermost Depth = %d\n", _outermost_depth);

  fprintf(f, "Variables: ");
  _var_info.Print(f);

  fprintf(f, "\nBounds:\n");
  _bounds.Print(f);

  fprintf(f, "Conditionals:\n");
  _conditionals.Print(f);

  fprintf(f, "End of Bounds Info\n");
}

