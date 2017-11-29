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
#include "sdlist.h"

//=======================================================================
// CLASS: SD_PNODE (Single scalar variable which must be distributed out)
// MEMBER FUNCTIONS: SD_PNODE, Print
//=======================================================================

//-----------------------------------------------------------------------
// NAME: SD_PNODE::SD_PNODE
// FUNCTION: Constructor for SD_PNODE. 
//-----------------------------------------------------------------------

SD_PNODE::SD_PNODE(const SYMBOL& symbol, 
 		   INT innermost_depth,
		   BOOL above):
  _symbol(symbol), _innermost_depth(innermost_depth), _above(above),
  _in_closure(SD_HASH_SIZE, &LNO_local_pool) 
{
}

//-----------------------------------------------------------------------
// NAME: SD_PNODE::Print 
// FUNCTION: Print important fields in SD_PNODE on the file 'fp'.  
//-----------------------------------------------------------------------

void SD_PNODE::Print(FILE* fp) const
{
  fprintf(fp, "<%s:[%d]>", _symbol.Name(), _innermost_depth); 
}

//=======================================================================
// CLASS: SD_PLIST (List of scalar variables to be distributed out)
// MEMBER FUNCTIONS: ~SD_PLIST, Print
//=======================================================================

//-----------------------------------------------------------------------
// NAME: SD_PLIST::~SD_PLIST 
// FUNCTION: Destructor for SD_PLIST.  
//-----------------------------------------------------------------------

SD_PLIST::~SD_PLIST() {
  while (!Is_Empty())
    CXX_DELETE(Remove_Head(), _pool);
}

//-----------------------------------------------------------------------
// NAME: SD_PLIST::Find 
// FUNCTION: Returns a pointer to the SD_PNODE corresponding to the symbol
//   'sym'.  Returns NULL if there is no node corresponding to this 'sym'. 
//-----------------------------------------------------------------------

const SD_PNODE* SD_PLIST::Find(const SYMBOL& sym) const
{
  SD_CONST_PITER i(this);
  for (const SD_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SD_PLIST::Find 
// FUNCTION: Returns a pointer to the SD_PNODE corresponding to the symbol
//   'sym'.  Returns NULL if there is no node corresponding to this 'sym'. 
//-----------------------------------------------------------------------

SD_PNODE* SD_PLIST::Find(const SYMBOL& sym)
{
  SD_PITER i(this);
  for (SD_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SD_PLIST::Print 
// FUNCTION: Print important fields in SD_PLIST on the file 'fp'. 
//-----------------------------------------------------------------------

void SD_PLIST::Print(FILE* fp) const
{
  SD_CONST_PITER i(this);
  for (const SD_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    n->Print(fp);
  fprintf(stdout, "\n");
}

//=======================================================================
// CLASS: SD_INFO (List of variables to be scalar expanded with member
//   functions)
// MEMBER FUNCTIONS: Update, Set_Worst_Case, Push_Memory_Nodes, 
//   Closure_Ldid, Closure_Stid, Closure_ILoad, Closure_IStore, 
//   Closure, Handle_Def, SD_INFO, Find, Enter, Update, Remove, 
//   Print, Make_Sd_Info, and Distribution_Range
//=======================================================================

//-----------------------------------------------------------------------
// NAME: Is_Above 
// FUNCTION: Returns TRUE if 'wn_ref' is not inside a loop nest, is in 
//   the innermost loop of a loop nest, or is in the sandwiched code of 
//   a loop nest but above another at the same level.  Returns FALSE 
//   otherwise.  
//-----------------------------------------------------------------------

BOOL Is_Above(WN* wn_ref)
{
  WN* wnn = NULL; 
  WN* wn = NULL;
  for (wn = wn_ref; wn != NULL; wn = wnn) {
    wnn = LWN_Get_Parent(wn); 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return TRUE; 
    if (wnn != NULL && WN_opcode(wnn) == OPC_BLOCK)
      break; 
  }
  if (wn == NULL)
    return TRUE; 
  WN* wn_first = wn; 
  for (wn = wn_first; wn != NULL; wn = WN_next(wn)) 
    if (WN_opcode(wn) == OPC_DO_LOOP)
      return TRUE; 
  return FALSE; 
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Update 
// FUNCTION: Add 'wn' to the closure list for 'sd_ref'.  
//-----------------------------------------------------------------------

BOOL SD_INFO::Update(SD_PNODE* sd_ref, 
		     WN* wn)
{
  sd_ref->Add_Closure(wn); 
  INT depth = Loop_Depth(wn);
  BOOL above = Is_Above(wn); 
  if (sd_ref->Innermost_Depth() == depth 
      && (sd_ref->Above() && !above || !sd_ref->Above() && above)) {
    Set_Worst_Case(sd_ref);    
    return FALSE;
  }
  if (sd_ref->Innermost_Depth() < depth) {
    sd_ref->Set_Innermost_Depth(depth);
    sd_ref->Set_Above(above); 
  } 
  if (Is_Worst_Case(sd_ref))
    return FALSE; 
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Set_Worst_Case 
// FUNCTION: Indicate that the node 'sd_ref' has worst case behavior, i.e.
//   the '_innermost_depth' is the depth of the innermost loop in the SNL. 
//-----------------------------------------------------------------------

void SD_INFO::Set_Worst_Case(SD_PNODE* sd_ref)
{
  if (sd_ref != NULL) {
    sd_ref->Set_Innermost_Depth(_max_inner_depth);
  } else {
    SD_PITER ii(&Plist);
    for (SD_PNODE* sdn = ii.First(); !ii.Is_Empty(); sdn = ii.Next()) 
      sdn->Set_Innermost_Depth(_max_inner_depth);
  } 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Push_Memory_Nodes
// FUNCTION: If necessary, push all of the memory reference nodes in the 
//   statement for 'wn_orig' onto the stack 'st_closure', in anticipation
//   of entering them into the closure hash table 'sd_ref'.  Return TRUE 
//   if we should continue computing the closure, FALSE if there is no 
//   reason to continue. 
//-----------------------------------------------------------------------

BOOL SD_INFO::Push_Memory_Nodes(WN* wn_orig, 
				SD_PNODE* sd_ref, 
			        STACK<WN*>* st_closure)
{
  if (sd_ref->In_Closure(wn_orig))
    return TRUE; 
  if (!Wn_Is_Inside(wn_orig, _wn_outer))
    return TRUE;  
  WN* wnn = NULL; 
  for (WN* wn = wn_orig; wn != NULL; wnn = wn, wn = LWN_Get_Parent(wn)) {
    OPCODE op = WN_opcode(wn); 
    if (op == OPC_BLOCK || op == OPC_DO_LOOP)
      break;
   } 
  WN* wn_stat = wnn; 
  if (wn_stat == NULL) 
    return TRUE; 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_stat);  
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    switch (WN_operator(wn)) {
    case OPR_STID:
      if (!Index_Variable(wn) && !sd_ref->In_Closure(wn)) {
        BOOL ok = Register_Stid(wn, sd_ref);
        if (ok) { 
	  st_closure->Push(wn); 
        } else { 
	  st_closure->Clear();
	  return FALSE; 
	} 
      } 
      break; 
    case OPR_LDID:
      if (!Index_Variable(wn) && !sd_ref->In_Closure(wn)) { 
        BOOL ok = Register_Ldid(wn, sd_ref);
        if (ok) { 
	  st_closure->Push(wn); 
        } else { 
	  st_closure->Clear();
	  return FALSE; 
	} 
      } 
      break; 
    case OPR_ILOAD:
      if (!Index_Variable(wn) && !sd_ref->In_Closure(wn)) { 
	BOOL ok = Register_ILoad(wn, sd_ref);
        if (ok) { 
	  st_closure->Push(wn); 
        } else { 
	  st_closure->Clear();
	  return FALSE; 
	} 
      } 
      break; 
    case OPR_ISTORE:
      if (!Index_Variable(wn) && !sd_ref->In_Closure(wn)) {   
 	BOOL ok = Register_IStore(wn, sd_ref);
        if (ok) { 
	  st_closure->Push(wn); 
        } else { 
	  st_closure->Clear();
	  return FALSE; 
	} 
      } 
      break;
    case OPR_MLOAD:
    case OPR_MSTORE:
    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
    case OPR_IO:
      Set_Worst_Case(NULL); 
      st_closure->Clear(); 
      return FALSE; 
    } 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Register_Ldid
// FUNCTION: Add the LDID 'wn_ldid' to the closure list of 'sd_ref'.  
//   Return TRUE if the worst case behavior is not yet seen, FALSE if 
//   it is. 
//-----------------------------------------------------------------------

BOOL SD_INFO::Register_Ldid(WN* wn_ldid,
			    SD_PNODE* sd_ref)
{ 
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (dg->Get_Vertex(wn_ldid) != 0) {
    Set_Worst_Case(sd_ref); 
    return FALSE; 
  } 
  DEF_LIST* def_list = du->Ud_Get_Def(wn_ldid);
  if (def_list == NULL || def_list->Incomplete()) {
    Set_Worst_Case(sd_ref);
    return FALSE; 
  }
  if (!Update(sd_ref, wn_ldid))
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Closure_Ldid 
// FUNCTION: Enter the node of OPR_LDID 'wn_ldid' into the closure hash 
//   table for 'sd_ref', and push other nodes within the immediately 
//   closure of 'wn_ldid' onto the stack 'st_closure'.  Return TRUE if 
//   it makes sense to continue computing the closure of 'sd_ref', return
//   FALSE otherwise.
//-----------------------------------------------------------------------

BOOL SD_INFO::Closure_Ldid(WN* wn_ldid,
			   SD_PNODE* sd_ref, 
			   STACK<WN*>* st_closure)
{
  DU_MANAGER* du = Du_Mgr; 
  DEF_LIST* def_list = du->Ud_Get_Def(wn_ldid);
  DEF_LIST_ITER iter(def_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_def = node->Wn();
    if (!Push_Memory_Nodes(wn_def, sd_ref, st_closure)) 
      return FALSE; 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Register_Stid
// FUNCTION: Add the STID 'wn_ldid' to the closure list of 'sd_ref'.  
//   Return TRUE if the worst case behavior is not yet seen, FALSE if 
//   it is. 
//-----------------------------------------------------------------------

BOOL SD_INFO::Register_Stid(WN* wn_stid, 
			    SD_PNODE* sd_ref)
{ 
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (dg->Get_Vertex(wn_stid) != 0) {
    Set_Worst_Case(sd_ref); 
    return FALSE; 
  } 
  USE_LIST *use_list = du->Du_Get_Use(wn_stid);
  if (use_list == NULL || use_list->Incomplete()) {
    Set_Worst_Case(sd_ref); 
    return FALSE; 
  }
  if (!Update(sd_ref, wn_stid)) 
    return FALSE; 
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Closure_Stid 
// FUNCTION: Enter the node of OPR_STID 'wn_stid' into the closure hash 
//   table for 'sd_ref', and push other nodes within the immediately 
//   closure of 'wn_stid' onto the stack 'st_closure'.  Return TRUE if 
//   it makes sense to continue computing the closure of 'sd_ref', return
//   FALSE otherwise.
//-----------------------------------------------------------------------

BOOL SD_INFO::Closure_Stid(WN* wn_stid,
                           SD_PNODE* sd_ref, 
                           STACK<WN*>* st_closure)
{
  DU_MANAGER* du = Du_Mgr; 
  USE_LIST *use_list = du->Du_Get_Use(wn_stid);
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = iter.Next()) {
    WN* wn_use = node->Wn();
    if (!Push_Memory_Nodes(wn_use, sd_ref, st_closure)) 
      return FALSE; 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Register_ILoad
// FUNCTION: Add the ILOAD 'wn_ldid' to the closure list of 'sd_ref'.  
//   Return TRUE if the worst case behavior is not yet seen, FALSE if 
//   it is. 
//-----------------------------------------------------------------------

BOOL SD_INFO::Register_ILoad(WN* wn_iload, 
			     SD_PNODE* sd_ref)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  if (!Update(sd_ref, wn_iload))
    return FALSE;
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Closure_ILoad 
// FUNCTION: Enter the node of OPR_STID 'wn_iload' into the closure hash 
//   table for 'sd_ref', and push other nodes within the immediately 
//   closure of 'wn_iload' onto the stack 'st_closure'.  Return TRUE if 
//   it makes sense to continue computing the closure of 'sd_ref', return
//   FALSE otherwise.
//-----------------------------------------------------------------------

BOOL SD_INFO::Closure_ILoad(WN* wn_iload, 
                            SD_PNODE* sd_ref,
                            STACK<WN*>* st_closure)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  EINDEX16 e = 0; 
  VINDEX16 v_iload = dg->Get_Vertex(wn_iload);
  for (e = dg->Get_In_Edge(v_iload); e != 0; e = dg->Get_Next_In_Edge(e)) {
    WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
    if (!Push_Memory_Nodes(wn_source, sd_ref, st_closure))
      return FALSE; 
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Register_IStore
// FUNCTION: Add the ISTORE 'wn_ldid' to the closure list of 'sd_ref'.  
//   Return TRUE if the worst case behavior is not yet seen, FALSE if 
//   it is. 
//-----------------------------------------------------------------------

BOOL SD_INFO::Register_IStore(WN* wn_istore, 
			      SD_PNODE* sd_ref)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  if (!Update(sd_ref, wn_istore))
    return FALSE;
  return TRUE; 
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Closure_IStore 
// FUNCTION: Enter the node of OPR_STID 'wn_istore' into the closure hash 
//   table for 'sd_ref', and push other nodes within the immediately 
//   closure of 'wn_istore' onto the stack 'st_closure'.  Return TRUE if 
//   it makes sense to continue computing the closure of 'sd_ref', return
//   FALSE otherwise.
//-----------------------------------------------------------------------

BOOL SD_INFO::Closure_IStore(WN* wn_istore, 
                             SD_PNODE* sd_ref,
                             STACK<WN*>* st_closure)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph;
  EINDEX16 e = 0; 
  VINDEX16 v_istore = dg->Get_Vertex(wn_istore);
  for (e = dg->Get_Out_Edge(v_istore); e != 0; e = dg->Get_Next_Out_Edge(e)) {
    WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
    if (WN_operator(wn_sink) != OPR_ILOAD)
      continue; 
    if (!Push_Memory_Nodes(wn_sink, sd_ref, st_closure))
      return FALSE;
  }
  return TRUE; 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Closure 
// FUNCTION: Compute the depth of the closure of 'wn_ref' and place it in
//   the _innermost_depth field of this node's SD_PNODE. 
//-----------------------------------------------------------------------

void SD_INFO::Closure(WN* wn_ref)
{
  DU_MANAGER* du = Du_Mgr; 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 

  SYMBOL sym_ref(wn_ref); 
  SD_PNODE* sd_ref = Find(sym_ref); 
  FmtAssert(sd_ref != NULL, ("Computing closure on non-entered symbol")); 
  if (sd_ref->In_Closure(wn_ref) || Is_Worst_Case(sd_ref))
    return; 

  STACK<WN*> st_closure(&LNO_local_pool); 
  if (!Push_Memory_Nodes(wn_ref, sd_ref, &st_closure)) 
    return; 

  while (st_closure.Elements() > 0) {
    WN* wn = st_closure.Pop(); 
    switch (WN_operator(wn)) {
    case OPR_LDID:
      if (!Closure_Ldid(wn, sd_ref, &st_closure))
        return; 
      break;
    case OPR_STID:
      if (!Closure_Stid(wn, sd_ref, &st_closure)) 
        return; 
      break;
    case OPR_ILOAD:
      if (!Closure_ILoad(wn, sd_ref, &st_closure))
        return; 
      break;
    case OPR_ISTORE:
      if (!Closure_IStore(wn, sd_ref, &st_closure))
        return; 
      break;
    default:
      FmtAssert(TRUE, ("Bad node type on closure stack")); 
      break;
    }
  }
} 

//-----------------------------------------------------------------------
// NAME: SD_INFO::Handle_Def 
// FUNCTION: Add information relevant to the OPR_STID node 'wn' to
//   the Plist of the SD_INFO. 
//-----------------------------------------------------------------------

void SD_INFO::Handle_Def(WN* wn)
{
  SYMBOL sym(wn); 
  Create(sym, wn); 
  Closure(wn); 
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Find 
// FUNCTION: Returns a pointer to the SD_PNODE on the Plist corresponding
//   to the symbol 'sym'.  Returns NULL if there is no node on the Plist
//   orresponding to this 'sym'. 
//-----------------------------------------------------------------------

const SD_PNODE* SD_INFO::Find(const SYMBOL& sym) const 
{
  SD_CONST_PITER i(&Plist);
  for (const SD_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Find 
// FUNCTION: Returns a pointer to the SD_PNODE on the Plist corresponding
//   to the symbol 'sym'.  Returns NULL if there is no node on the Plist
//   orresponding to this 'sym'. 
//-----------------------------------------------------------------------

SD_PNODE* SD_INFO::Find(const SYMBOL& sym) 
{
  SD_PITER i(&Plist);
  for (SD_PNODE* n = i.First(); !i.Is_Empty(); n = i.Next())
    if (n->Symbol() == sym)
      return n;
  return NULL;
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Enter 
// FUNCTION: Enter a node on the Plist of the SD_INFO which has a symbol
//   'sym' and innermost depth 'innermost_depth' and has Is_Above() 
//   value of 'above'. 
//-----------------------------------------------------------------------

void SD_INFO::Enter(const SYMBOL& sym,
		    INT innermost_depth,
		    BOOL above)
{
  Is_True(Find(sym) == NULL,
    ("Entering %s twice into SD_INFO", sym.Name()));
  SD_PNODE* n = CXX_NEW(SD_PNODE(sym, innermost_depth, above),  
    Plist.Pool());
  Plist.Append(n);
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Create 
// FUNCTION: Create a node for the symbol 'sym' with innermost depth and
//   above values calculated from 'wn', if one does not already exist.  
//   If there is already such a node, update it with the innermost depth 
//   and above value corresponding to 'wn'.  
//-----------------------------------------------------------------------

void SD_INFO::Create(const SYMBOL& sym,
		     WN* wn)
{
  SD_PNODE* sd_node = Find(sym); 
  INT innermost_depth = Loop_Depth(wn); 
  BOOL above = Is_Above(wn); 
  if (sd_node == NULL) {
    sd_node = CXX_NEW(SD_PNODE(sym, innermost_depth, above),
      Plist.Pool());  
    Plist.Append(sd_node);
  } else {
    Update(sd_node, wn); 
  }
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Remove 
// FUNCTION: Remove the node 'sdn' from the Plist of the SD_INFO.
//-----------------------------------------------------------------------

void SD_INFO::Remove(SD_PNODE* sdn)
{
  Plist.Remove(sdn);
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Print 
// FUNCTION: Print function for objects of class SD_INFO. 
//-----------------------------------------------------------------------

void SD_INFO::Print(FILE* f) const
{
  Plist.Print(f);
  fprintf(f, "\n");
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Make_Sd_Info 
// FUNCTION: Construct an SD_INFO for the SNL with outermost loop 'wn_outer'
//   containing 'nloops' loops.
//-----------------------------------------------------------------------

void SD_INFO::Make_Sd_Info(WN* wn_outer,  
			   INT nloops)
{
  _wn_outer = wn_outer; 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_outer, nloops); 
  _max_inner_depth = Do_Loop_Depth(wn_inner); 
  LWN_ITER* itr = LWN_WALK_TreeIter(wn_outer); 
  for (; itr != NULL; itr = LWN_WALK_TreeNext(itr)) {
    WN* wn = itr->wn; 
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_STID && !Index_Variable(wn))
      Handle_Def(wn); 
  }
}

//-----------------------------------------------------------------------
// NAME: SD_INFO::Distribution_Range
// FUNCTION: For the SNL whose outermost transformable depth is 'depth'
//   and whose expandable scalars are summarized in 'sx_info', set the
//   'lower_range' and 'upper_range' of loops containing unexpandable 
//   scalars.
//-----------------------------------------------------------------------

BOOL SD_INFO::Distribution_Range(INT depth, 
				 SX_INFO* sx_info) 
{
  INT best_upper_range = -1; 
  SD_CONST_PITER ii(&Plist);
  for (const SD_PNODE* sdn = ii.First(); !ii.Is_Empty(); sdn = ii.Next()) {
    const SX_PNODE* sxn = sx_info->Find(sdn->Symbol()); 
    if (sxn != NULL && sxn->Transformable(depth) == SX_PNODE::ILLEGAL) {
      if (best_upper_range == -1 || best_upper_range < sdn->Innermost_Depth())
        best_upper_range = sdn->Innermost_Depth(); 
    }
  }
  return best_upper_range; 
}
