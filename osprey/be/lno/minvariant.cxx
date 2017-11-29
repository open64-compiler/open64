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


//-*-C++-*-

/**
***                     Memory Invariant Removal
***                     ------------------------
***
*** Algorithm Description:
***
*** The goal is to move memory invariant out of loops, e.g.
*** 
***         for i
***           for j
***                  = a(k)
***             a(k) = 
***           end for
***         end for
*** 
*** is transformed to 
*** 
***         t = a(k)
***         for i
***           for j
***             t = 
***               = t
***           end for
***         end for
***         a(k) = t
*** 
*** We cannot do this unless we know that this memory location
*** would have been referenced without the transformation (TODO:
*** can speculate loads if Eager_Level >= 4, but we're not taking
*** advantage of that right now).  In the above example, we know
*** loops i and j both must go at least once because we require this
*** code execute after the phase that insures each DO loop go
*** at least once and pulls the IFs protecting the DO loops out
*** as far as possible.  So, an elaboration of the above example:
*** 
***         if (N >= 1)
***           for i = 1 to n
***             for j = 1 to n
***                    = a(k)
***               a(k) = 
***             end for
***           end for
***         end if
*** 
*** is transformed to 
*** 
***         if (N >= 1)
***           t = a(k)
***           for i = 1 to n
***             for j = 1 to n
***                 = t
***               t = 
***             end for
***           end for
***           a(k) = t
***         end if
*** 
*** Notice that we cannot pull the 't = a(k)' outside the IF, since we
*** don't know whether it executes.
*** 
*** Likewise, within the FOR loops may be IFs.  If references only
*** occur within such IFs, we don't know if they execute.  E.g.
*** 
***           for i = 1 to n
***             for j = 1 to n
***               if (...)
***                    = a(k)
***               endif
***               //a(k) = ..
***             end for
***           end for
*** 
*** In this case, the read may or may not execute.  It cannot be pulled
*** out.  However, if the commented line is uncommented, then we know
*** that a(k) is referenced within the loop, and so the a(k) can be
*** pulled out everywhere, including within the IF.
*** 
*** The following is the proposed algorithm for moving out memory
*** references.  TODO: This algorithm does not look at the expressions inside
*** ifs and determine whether the statements will execute.  It just
*** assumes that code inside an IF may or may not execute.  One
*** improvement would be to recognize that if a reference occurs within
*** both an IF and the corresponding ELSE, then it must execute -- is that
*** important, and how could that be smoothly integrated into this
*** algorithm?
*** 
*** Algorithm:
*** 
***   For each outermost DO loop
*** 
***     1. Gather lists of identical loop references (references with
***        identical bases and access vectors).
*** 
***     2. For each list, see if each reference is invariant in this loop
***        and on in -- if so, it qualifies for step 3.  Only one
***        reference needs to be checked, and the
***        check is that if loop j is of depth 2, then the Loop_Coeff()s
***        of depth 2 or more are all zero and the access vector coeffs
***        are constant in this loops.  Also, check that at least one reference
***        has no ancestors that are ifs out to the current loop (the only
***        IF check) -- if not, it also doesn't qualify.  
***	   Similarly, check that no ancestors out to the current loop are
***        MP loops.  We don't guard MP loops so can't move invariants outside
***        of them.  As opposed to IFs, we don't move if any reference is
***        inside an MP loop for now.  Lists that don't
***        qualify for step 3 still qualify for step 4.
***      
***
***     3. For each list that qualifies from step 2, we must look at the
***        dependences to see if transformation is legal.  If all dependence
***        arcs to and from each entry on the list go only to other
***        elements of the list and/or to references outside the current
***        loop of interest, then:
*** 
***         a) if the variable is ever written, put a
***             arrayref(..) = scalar
***            after the loop; if the variable is ever read, put a
***             scalar = arrayref(..)
***            before the loop.
***         b) all dependences to/from outside the loop to/from any
***            read inside the loop should go to the hoisted read from
***            step a.  Likewise for the write dependences.  Otherwise,
***            remove all the dependence to the references.  If there was
***            both a read and a write, update add a dependence arc
***            between those two as well.
***         c) replace all references with a scalar.  Conservatively update
***            the DU chain.
*** 
***        Finally, remove this list from this consideration for step 4.
***        Note that if the enclosing loop for either reference has Has_Gotos
***        or Has_Bad_Mem set, then we can't do anything.  
*** 
***     4. For each loop inside, make new lists consisting only of
***        references within that loop and GOTO step 2.
***
*** ----------------------------
***
*** What if we have a(i) and b(i)?  It could be that b = &a, so that they
*** actually refer to the same thing.  So what should we do about this?
*** One approach is to use Get_ST_Base(WN_array_base(array)).  It returns the
*** base st if the array base is an lda.  If it's an ldid, it follows the
*** du chain back, if possible, looking for an lda.  It returns 0 if something
*** went wrong.  For all pairs in the same partition, call
*** DEPV_COMPUTE::Base_Test(array1,array2).  Two array bases are
*** exactly equivlant if this routine returns DEP_CONTINUE.
***
*** So here's how we deal with bases.  We have a dynamic array of bases
*** from Get_St_Base().  Each of these contains a list of equivalence classes,
*** equivalenced by the function Base_Test() == DEP_CONTINUE, which Dror
*** promises is transitive.  If there is a dependence that occurs inside a
*** loop of interest but has one end of the arc on this list and the other
*** not, then that's a bad dependence.
***
*** TODO: If we have a(b(i)), inner loop j, then in theory we might be able
*** to hoist not just b(i), but a(b(i)).  In practice, we don't try.  That's
*** because our algorithm depends upon access vectors, which get stale
*** very quickly when doing this kind of thing.  Redoing promotion without
*** access vectors (and with careful handling of MIR_REFLISTs to make sure
*** that indirect arrays don't result in deleted nodes being referenced)
*** might be interesting.
**/

/* ====================================================================
 * $Revision: 1.17 $
 * $Date: 05/08/31 17:40:36-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: be/lno/SCCS/s.minvariant.cxx $
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: be/lno/SCCS/s.minvariant.cxx $ $Revision: 1.17 $";
#endif

#include <sys/types.h>
#include <alloca.h>
#include "defs.h"
#include "wn.h"
#include "cxx_memory.h"
#include "dep_graph.h"
#include "lnopt_main.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "minvariant.h"
#include "optimizer.h"
#include "opt_du.h"
#include "wn_simp.h"
#include "snl_utils.h" 

#ifdef TARG_X8664
BOOL Minvariant_Removal_For_Simd = FALSE;
#endif

static MEM_POOL MIR_local_pool;
static BOOL mir_local_pool_initialized = FALSE;

static BOOL Minv_Debug = FALSE;

//------------------------------------------------------------------------

// find a load/store parent for the node.  

static WN* Find_Ls(WN *wn, BOOL okay_to_not_find_one = FALSE)
{
  for (WN* p = wn; p; p = LWN_Get_Parent(p)) {
    OPCODE opc = WN_opcode(p);
    if (OPCODE_is_load(opc) || OPCODE_is_store(opc))
      return p;
  }

  FmtAssert(okay_to_not_find_one,
	    ("Couldn't find a load or store parent for wn=0x%lx", wn));
  return NULL;
}

//------------------------------------------------------------------------

typedef DYN_ARRAY<WN*>              WN_ARRAY;

void Unique_AddElement(WN_ARRAY* array, WN* wn, BOOL duplicates_okay)
{
  for (INT i = array->Elements() - 1; i >= 0; i--) {
    if (wn == (*array)[i]) {
      Is_True(duplicates_okay, ("Duplicate wn 0x%lx added in minvariant", wn));
      return;
    }
  }
  array->AddElement(wn);
}

// each MIR_REFLIST holds a Base.  Call Get_ST_Base to fill this field.
// To see if another array reference has the same base, call Get_ST_Base
// on that to see if it's the same.  If it is, then call Base_Test() and
// to see if it really is the same base.  The access vectors and the type
// of the memory operations also have to
// be identical to go on this list.  The Wnlist is a list of all references
// with the same base (by this definition) and access vector.
// Also note that the Wnlist is a list of addresses, ie OPR_ARRAY nodes.

struct MIR_REFLIST {
  ST*           Base;                 // from Get_St_Base(WN_array_base(array)
  ACCESS_ARRAY* Aa;
  WN*           Array;
  WN_ARRAY      Wnlist;

  MIR_REFLIST(ST* base, ACCESS_ARRAY* aa, WN* wn, MEM_POOL* pool) :
  Wnlist(pool), Aa(aa), Base(base), Array(wn) {
    Unique_AddElement(&Wnlist, wn, FALSE);
  }
  ~MIR_REFLIST() {}
  char*         Basename() const {
    return SYMBOL(WN_array_base(Wnlist[0])).Name();
  }
  char*         Basename(char* buf, INT bufsz) const {
    return SYMBOL(WN_array_base(Wnlist[0])).Name(buf, bufsz);
  }
  BOOL          Same(ST* base, ACCESS_ARRAY* aa, WN* array);
  BOOL          operator == (const MIR_REFLIST&);
  void          Print(FILE* f) const;

 private:

  static BOOL Same_Base(const ST* st1, const ST* st2) {
    return (st1 == NULL || st2 == NULL) ? st1 == st2 :
      (ST_base(st1) == ST_base(st2) && ST_ofst(st1) == ST_ofst(st2));
  }
};


BOOL MIR_REFLIST::Same(ST* base, ACCESS_ARRAY* aa, WN* array)
{
  return Same_Base(base, Base) &&
	 *aa == *Aa &&
	 DEPV_COMPUTE::Base_Test(Find_Ls(array), NULL, Find_Ls(Array),NULL ) == 
					      DEP_CONTINUE &&
	WN_desc(LWN_Get_Parent(Wnlist[0])) ==
	WN_desc(LWN_Get_Parent(array));
}

BOOL MIR_REFLIST::operator == (const MIR_REFLIST& mir)
{
  if (Base != mir.Base)
    return FALSE;
  if (!(*Aa == *mir.Aa))
    return FALSE;

  INT elts = Wnlist.Elements();
  if (elts != mir.Wnlist.Elements())
    return FALSE;

  mBOOL* used = (mBOOL*) alloca(sizeof(mBOOL)*elts);

  INT i;
  for (i = 0; i < elts; i++)
    used[i] = FALSE;

  for (i = 0; i < elts; i++) {
    INT j;
    for (j = 0; j < elts; j++) {
      if (used[j] == FALSE && Wnlist[i] == mir.Wnlist[j]) {
        used[j] = TRUE;
        break;
      }
    }
    if (j == elts)
      return FALSE;
  }

  return TRUE;
}

void MIR_REFLIST::Print(FILE* f) const
{
  INT reads = 0;
  INT writes = 0;

  for (INT j = 0; j < Wnlist.Elements(); j++) {
    WN*         wn = Wnlist[j];
    WN*         parent = LWN_Get_Parent(wn);
    OPCODE      op = WN_opcode(parent);

    if (OPCODE_is_load(op))
      reads++;
    else if (OPCODE_is_store(op))
      writes++;
    else
      FmtAssert(0, ("Bad parent for array--can't happen (op=%d)", op));
    fprintf(f, " <0x%p,0x%p>", parent, wn);
  }

  fprintf(f, " [r=%d,w=%d]", reads, writes);
  fprintf(f, " %s", Basename());
  Aa->Print(f);
}

static BOOL MIR_Messy_Subscript(MIR_REFLIST* mr);

static void Print_Lists(FILE* f, const DYN_ARRAY<MIR_REFLIST*>& lists)
{
  for (INT i = 0; i < lists.Elements(); i++) 
    lists[i]->Print(f);
}

#ifdef Is_True_On
static BOOL Compare_Lists(const DYN_ARRAY<MIR_REFLIST*>& lists1,
                          const DYN_ARRAY<MIR_REFLIST*>& lists2)
{
  // Kind of tricky.  This is very slow.

  INT elts = lists1.Elements();

  if (lists2.Elements() != elts)
    return FALSE;

  mBOOL* used = (mBOOL*) alloca(sizeof(mBOOL)*elts);

  INT i;
  for (i = 0; i < elts; i++)
    used[i] = FALSE;

  for (i = 0; i < elts; i++) {
    INT j;
    for (j = 0; j < elts; j++) {
      if (used[j] == FALSE && *lists1[i] == *lists2[j]) {
        used[j] = TRUE;
        break;
      }
    }
    if (j == elts)
      return FALSE;
  }
  return TRUE;
}
#endif

//------------------------------------------------------------------------

static BOOL MIR_Dep_Subset(DEP d1, DEP d2)
{
  if (DEP_IsDistance(d2)) {
    if (DEP_IsDistance(d1)) {
      if (DEP_Distance(d1) == DEP_Distance(d2)) {
        return TRUE;
      } else {
        return FALSE;
      }
    } else {
      return FALSE;
    }
  } else {
    INT id1 = (INT) DEP_Direction(d1);
    INT id2 = (INT) DEP_Direction(d2);
    if ((id1 | id2) == id2) {
      return TRUE;
    } else {
      return FALSE;
    }
  }
}

// TRUE if d2 encompasses d1
static BOOL MIR_Depv_Subset(const DEPV* d1, const DEPV* d2, INT dims)
{
  for (INT i = 0; i < dims; i++)
    if (MIR_Dep_Subset(DEPV_Dep(d1,i), DEPV_Dep(d2,i)) == FALSE)
      return FALSE;
  return TRUE;
}

// if returns FALSE, a disaster.  Someone else must clean up

static BOOL MIR_Add_Edge(ARRAY_DIRECTED_GRAPH16*        dg,
                         VINDEX16                       vsource,
                         VINDEX16                       vsink,
                         const DEPV_ARRAY*              depv_array)
{
  MEM_POOL* pool = dg->Pool();

  // first, get the edge from the source to the sink

  EINDEX16 e;
  for (e = dg->Get_Out_Edge(vsource); e;
       e = dg->Get_Next_Out_Edge(e)) {
    FmtAssert(vsource == dg->Get_Source(e), ("Bad source"));
    if (vsink == dg->Get_Sink(e))
      break;
  }

  // if there is none, then just add a copy of this depv_array

  if (e == 0) {
    DEPV_ARRAY* new_depv_array = Create_DEPV_ARRAY(depv_array, pool);
    EINDEX16 e2 = dg->Add_Edge(vsource, vsink, new_depv_array);
    Is_True(e2, ("Graph ran out of space"));
    return e2 != 0;
  }

  // we have to union the depv arrays.  That's kind of a pain.  We use the
  // following strategy.  Hopefully, each dependence we are adding in is
  // redundant with an existant one.  If so, we are done.  If not, we
  // throw away the depv_array associated with this edge and create a new one
  // that's the union.

  INT ndim = dg->Depv_Array(e)->Num_Dim();
  FmtAssert(ndim == depv_array->Num_Dim(),
            ("Bad number of dimensions"));
  FmtAssert(dg->Depv_Array(e)->Num_Unused_Dim() == depv_array->Num_Unused_Dim(),
            ("Bad number of unused dimensions"));

  BOOL   need_to_add_total = 0;
  mBOOL* need_to_add = (mBOOL*) alloca(sizeof(mBOOL) * depv_array->Num_Vec());
  for (INT i = 0; i < depv_array->Num_Vec(); i++) {
    const DEPV* dv = depv_array->Depv(i);
    need_to_add[i] = TRUE;
    for (INT j = 0; j < dg->Depv_Array(e)->Num_Vec(); j++) {
      if (MIR_Depv_Subset(dv, dg->Depv_Array(e)->Depv(j), ndim)) {
        need_to_add[i] = FALSE;
        break;
      }
    }
    if (need_to_add[i])
      need_to_add_total++;
  }
  if (need_to_add_total) {
    INT nvec = need_to_add_total + dg->Depv_Array(e)->Num_Vec();
    if (nvec >= 255) return FALSE;
    INT nunused_dim = dg->Depv_Array(e)->Num_Unused_Dim();
    DEPV_ARRAY* newdva = Create_DEPV_ARRAY(nvec, ndim, nunused_dim, pool);

    INT j;
    for (j = 0; j < dg->Depv_Array(e)->Num_Vec(); j++) {
      DEPV* newdv = newdva->Depv(j);
      DEPV* dv = dg->Depv_Array(e)->Depv(j);
      for (INT k = 0; k < ndim; k++)
        DEPV_Dep(newdv, k) = DEPV_Dep(dv, k);
    }

    for (INT jj = 0; jj < depv_array->Num_Vec(); jj++) {
      if (need_to_add[jj]) {
        DEPV* newdv = newdva->Depv(j++);
        const DEPV* dv = depv_array->Depv(jj);
        for (INT k = 0; k < ndim; k++)
          DEPV_Dep(newdv, k) = DEPV_Dep(dv, k);
      }
    }

    dg->Delete_Array_Edge(e);
    dg->Add_Edge(vsource, vsink, newdva);
  }

  return TRUE;
}

static BOOL MIR_Has_Array_Kid(WN* wn)
{
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    WN* w = WN_kid(wn,i);
    if (WN_operator(w) == OPR_ARRAY)
      return TRUE;
    if (MIR_Has_Array_Kid(w))
      return TRUE;
  }
  return FALSE;
}

static void MIR_Build_Loop_List_Array(WN*                     wn,
                                      DYN_ARRAY<MIR_REFLIST*>&lists,
                                      MEM_POOL*               pool,
                                      BOOL                    duplicates_okay);

static void MIR_Maybe_Add_To_List(WN*                      wn,
                                  DYN_ARRAY<MIR_REFLIST*>& lists,
                                  MEM_POOL*                pool,
                                  BOOL                     duplicates_okay)
{
  // if there is indirection, then different MIR_REFLISTs may contain
  // each other and it's very tricky.

  if (!MIR_Has_Array_Kid(wn)) {
    WN*      parent = LWN_Get_Parent(wn);
    OPERATOR parent_oper = WN_operator(parent);
    if (((parent_oper == OPR_ISTORE) && (wn == WN_kid1(parent))) ||
        (parent_oper == OPR_ILOAD)){
#ifdef KEY //12404: don't "minvar" for istore/iload of volatiles
      if (!WN_Is_Volatile_Mem(parent))
#endif
      MIR_Build_Loop_List_Array(wn, lists, pool, duplicates_okay);
    }
  }
}

//-----------------------------------------------------------------------
// NAME: Already_On_Stack
// FUNCTION: Returns -1 if 'wn_node' is not aready on the stack, returns
//   the position of the 'wn_node' from the bottom of the stack if it is
//   on the 'stack'. 
//-----------------------------------------------------------------------

static INT Already_On_Stack(STACK<WN*>* stack, 
			    WN* wn_node)
{
  INT i;
  for (i = 0; i < stack->Elements(); i++)  
    if (stack->Bottom_nth(i) == wn_node)
      break;
  return (i < stack->Elements()) ? i : -1; 
} 

//-----------------------------------------------------------------------
// NAME: Build_Ordered_Stack_Traverse
// FUNCTION: Traverse the 'wn_tree' in postorder, adding the nodes listed
//   on 'stk_deps' to 'stk_ordered' in the order that they are encountered.
//-----------------------------------------------------------------------

static void Build_Ordered_Stack_Traverse(WN* wn_tree,
					 STACK<WN*>* stk_deps, 
					 STACK<WN*>* stk_ordered)
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (WN_operator(wn_tree) == OPR_BLOCK) { 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = WN_next(wn))
      Build_Ordered_Stack_Traverse(wn, stk_deps, stk_ordered);
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++)  
      Build_Ordered_Stack_Traverse(WN_kid(wn_tree, i), stk_deps,
	stk_ordered);
    if (dg->Get_Vertex(wn_tree)) {
      INT stk_count = Already_On_Stack(stk_deps, wn_tree);
      if (stk_count >= 0) 
        stk_ordered->Push(stk_deps->Bottom_nth(stk_count)); 
    } 
  } 
} 

//-----------------------------------------------------------------------
// NAME: Build_Ordered_Stack
// FUNCTION: Traverse the 'wn_tree' in postorder, adding the nodes listed
//   on 'stk_deps' to 'stk_ordered' in the order that they are encountered.
//-----------------------------------------------------------------------

static void Build_Ordered_Stack(WN* wn_loop,
				STACK<WN*>* stk_deps, 
				STACK<WN*>* stk_ordered)
{
  Build_Ordered_Stack_Traverse(wn_loop, stk_deps, stk_ordered);
} 

//-----------------------------------------------------------------------
// NAME: MIR_Update_Dependences
// FUNCTION: Update the dependences inside 'wn_loop' on nodes which are
//   descendants of 'new_uses'. 
//-----------------------------------------------------------------------

extern void MIR_Update_Dependences(WN* wn_loop, 
		                   DYN_ARRAY<WN*>* new_uses)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  STACK<WN*>* stk_deps = CXX_NEW(STACK<WN*>(&LNO_local_pool), 
    &LNO_local_pool);   

  if (mir_local_pool_initialized == FALSE) {
    mir_local_pool_initialized = TRUE;
    MEM_POOL_Initialize(&MIR_local_pool, "MIR_local_pool", FALSE);
  }

  INT i;
  for (i = 0; i < new_uses->Elements(); i++) {
    for (WN* wn = (*new_uses)[i]; wn != NULL; wn = LWN_Get_Parent(wn)) 
      if (dg->Get_Vertex(wn) && Already_On_Stack(stk_deps, wn) == -1)
	stk_deps->Push(wn);
    WN* wn_array = Messy_Subscript((*new_uses)[i]); 
    if (wn_array == NULL)
      continue; 
    WN* wn_dep = LWN_Get_Parent(wn_array); 
    if (dg->Get_Vertex(wn_dep))
      stk_deps->Push(wn_dep); 
  }
  INT original_count = stk_deps->Elements(); 
  for (i = 0; i < original_count; i++) { 
    WN* wn_dep = stk_deps->Bottom_nth(i); 
    VINDEX16 v = dg->Get_Vertex(wn_dep);
    EINDEX16 e = 0;
    for (e = dg->Get_In_Edge(v); e != 0; e = dg->Get_Next_In_Edge(e)) {
      WN* wn_source = dg->Get_Wn(dg->Get_Source(e));
      if (Already_On_Stack(stk_deps, wn_source) == -1)
	stk_deps->Push(wn_source); 
    }
    for (e = dg->Get_Out_Edge(v); e != 0; e = dg->Get_Next_Out_Edge(e)) {
      WN* wn_sink = dg->Get_Wn(dg->Get_Sink(e));
      if (Already_On_Stack(stk_deps, wn_sink) == -1)
	stk_deps->Push(wn_sink); 
    }
  }
  WN* wn = 0;
  for (wn = wn_loop; wn != NULL; wn = LWN_Get_Parent(wn))
    if (WN_opcode(wn) == OPC_DO_LOOP && Do_Depth(wn) == 0)
      break; 
  WN* wn_outer_loop = wn; 
  STACK<WN*>* stk_ordered = CXX_NEW(STACK<WN*>(&LNO_local_pool), 
    &LNO_local_pool); 
  Build_Ordered_Stack(wn_loop, stk_deps, stk_ordered);
  for (i = 0; i < stk_ordered->Elements(); i++) { 
    WN* wn_one = stk_ordered->Bottom_nth(i); 
    VINDEX16 v1 = dg->Get_Vertex(wn_one); 
    DOLOOP_STACK stk_one(&LNO_local_pool); 
    Build_Doloop_Stack(wn_one, &stk_one); 
    for (INT j = i; j < stk_ordered->Elements(); j++) { 
      WN* wn_two = stk_ordered->Bottom_nth(j); 
      DOLOOP_STACK stk_two(&LNO_local_pool); 
      Build_Doloop_Stack(wn_two, &stk_two); 
      VINDEX16 v2 = dg->Get_Vertex(wn_two); 
      if (OPCODE_is_load(WN_opcode(wn_one)) 
	  && OPCODE_is_load(WN_opcode(wn_two)))
	continue; 
      BOOL found_edge = FALSE; 
      EINDEX16 e1 = dg->Get_Edge(v1, v2);
      if (e1 != 0) {
	found_edge = TRUE; 
	dg->Delete_Edge(e1);
      } 
      EINDEX16 e2 = dg->Get_Edge(v2, v1); 
      if (e2 != 0) { 
        found_edge = TRUE; 
        dg->Delete_Edge(e2); 
      } 
      if (found_edge) {
	if (!dg->Add_Edge(wn_one, &stk_one, wn_two, &stk_two, TRUE)) {
	  LNO_Erase_Dg_From_Here_In(wn_one, dg);     
	  LNO_Erase_Dg_From_Here_In(wn_two, dg); 
	}     
      } 
    }
  }
}

//-----------------------------------------------------------------------
// NAME: MIR_Patch_Loop_Stmt
// FUNCTION: PV 651045: A weird case.  An array reference 'new_load' was 
//   hoisted, but the loop stmt which was too conservative now becomes
//   incorrect (since it refers to an inner loop) after the hoisting. 
//   Patch it up with a valid value if this happens. 
//-----------------------------------------------------------------------

static void MIR_Patch_Loop_Stmt(WN* new_load)
{ 
  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(new_load);
  if (def_list != NULL && def_list->Loop_stmt() != NULL) {
    WN* wn_start = Enclosing_Proper_Do_Loop(new_load);
    WN* wn = 0;
    for (wn = wn_start; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_operator(wn) == OPR_DO_LOOP && wn == def_list->Loop_stmt())
	break;
    if (wn == NULL) { 
      Du_Mgr->Ud_Get_Def(new_load)->Set_loop_stmt(wn_start);
    } 
  }
}  

static void MIR_Replace(WN*                      loop,
                        const MIR_REFLIST*       p,
                        ARRAY_DIRECTED_GRAPH16*  dg,
                        DYN_ARRAY<MIR_REFLIST*>& newly_direct_lists,
                        MEM_POOL*                pool,
			BOOL 			 messy_only)
{
  /*
  ***        From the comments at the top of the file:
  ***
  ***         a) if the variable is ever written, put a
  ***             arrayref(..) = scalar
  ***            after the loop; if the variable is ever read, put a
  ***             scalar = arrayref(..)
  ***            before the loop.
  ***         b) all dependences to/from outside the loop to/from any
  ***            read inside the loop should go to the hoisted read from
  ***            step a.  Likewise for the write dependences.  Otherwise,
  ***            remove all the dependence to the references.  If there was
  ***            both a read and a write, update add a dependence arc
  ***            between those two as well.
  ***         c) replace all references with a scalar.  Conservatively update
  ***            the DU chain.
  *** 
  ***        Finally, remove this list from this consideration for step 4.
  */

  static INT    unique_mi_num = 0;

  char          newname[64];
  WN*           read_tree = NULL;
  WN*           write_tree = NULL;
  VINDEX16      vr = 0;
  VINDEX16      vw = 0;
  INT           gdepth = Good_Do_Depth(loop);
  INT           j;
  BOOL          dependence_disaster = FALSE;


  TYPE_ID       type = WN_desc(LWN_Get_Parent(p->Wnlist[0]));
  TY_IDX        ty = Be_Type_Tbl(type);
  TY_IDX        pty = Make_Pointer_Type(ty);

#ifdef TARG_X8664 //bug 11472: memory chunk should be assigned to a temporary instead of a preg
  if (Minvariant_Removal_For_Simd || type == MTYPE_M) pty = ty;
#endif
#ifdef KEY  
  // Bug 3072 - If parent type is MTYPE_BS then, we can not create OPC_BSBSLDID 
  // - triggered by fix to this bug in model.cxx (adjust esz to 1-byte in 
  // Build_Aarray).
  if (type == MTYPE_BS) return;
#endif

  if (Minv_Debug) {
    fprintf(TFile, "In loop %s, hoisting", SYMBOL(WN_index(loop)).Name());
    p->Print(TFile);
  }

  sprintf(newname, "$mi%d", unique_mi_num++);
#ifdef TARG_X8664
  SYMBOL newsym;
  if (Minvariant_Removal_For_Simd || type == MTYPE_M) {
    ST * st = Gen_Temp_Symbol (MTYPE_TO_TY_array[type], "_misym");
    newsym = SYMBOL(st, 0, type);
    // bug 7446
    for (WN* wn = loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (Is_Mp_Region(wn)) {
	WN *prag = WN_CreatePragma(WN_PRAGMA_LOCAL, newsym.St(), 
				   newsym.WN_Offset(), 0);
	WN_set_pragma_compiler_generated(prag);
	LWN_Insert_Block_Before(WN_region_pragmas(wn), NULL, prag);
	break;
      } 
    } 
  } else
    newsym = Create_Preg_Symbol(newname, type);
#else
  SYMBOL newsym = Create_Preg_Symbol(newname, type);
#endif

  // Are there any reads?  If so, read before.

  for (j = 0; j < p->Wnlist.Elements(); j++) {
    WN* wn = p->Wnlist[j];
    if (OPCODE_is_load(WN_opcode(LWN_Get_Parent(wn))))
      break;
  }

  if (j < p->Wnlist.Elements()) {
    WN*         the_load = LWN_Get_Parent(p->Wnlist[j]);
    WN*         new_load = LWN_Copy_Tree(the_load, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(the_load, new_load, Du_Mgr);
    OPCODE      op = OPCODE_make_op(OPR_STID, MTYPE_V, type);
    read_tree = LWN_CreateStid(op, newsym.WN_Offset(), 
				newsym.St(), pty, new_load);
#ifdef TARG_X8664
    if (Minvariant_Removal_For_Simd || type == MTYPE_M)
      Create_alias(Alias_Mgr,read_tree);
#endif
    LWN_Copy_Linenumber(LWN_Get_Statement(the_load),read_tree);
    LWN_Copy_Frequency_Tree(read_tree,loop);
    LWN_Insert_Block_Before(LWN_Get_Parent(loop), loop, read_tree);
    if (gdepth > 0) {
      vr = dg->Add_Vertex(WN_kid0(read_tree));
      if (vr == 0)
        dependence_disaster = TRUE;
    }
    DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
      &LNO_local_pool);
    Build_Doloop_Stack(read_tree, stack); 
    LNO_Build_Access(read_tree, stack, &LNO_default_pool);   
    MIR_Patch_Loop_Stmt(new_load);
    CXX_DELETE(stack, &LNO_local_pool);
  }

  // Are there any writes?  If so, write after.

  for (j = 0; j < p->Wnlist.Elements(); j++) {
    WN* wn = p->Wnlist[j];
    if (OPCODE_is_store(WN_opcode(LWN_Get_Parent(wn))))
      break;
  }

  if (j < p->Wnlist.Elements()) {
    WN*         naddr = LWN_Copy_Tree(p->Wnlist[j], TRUE, LNO_Info_Map);
    INT         naddr_wnoffset = WN_offset(Find_Ls(p->Wnlist[j]));

    LWN_Copy_Def_Use(p->Wnlist[j], naddr, Du_Mgr);
    OPCODE      op = OPCODE_make_op(OPR_LDID, Promote_Type(type), type);
    WN*         ldid = WN_CreateLdid(op, newsym.WN_Offset(), 
				newsym.St(), ty);
#ifdef TARG_X8664
    if (Minvariant_Removal_For_Simd || type == MTYPE_M)
      Create_alias(Alias_Mgr,ldid);
#endif
    OPCODE      storeop = WN_opcode(LWN_Get_Parent(p->Wnlist[j]));
#ifdef TARG_X8664
    pty = Make_Pointer_Type(ty);
#endif
    write_tree = LWN_CreateIstore(storeop, naddr_wnoffset, pty, ldid, naddr);
#ifdef TARG_X8664
    pty = ty;
#endif
    LWN_Insert_Block_After(LWN_Get_Parent(loop), loop, write_tree);
    Copy_alias_info(Alias_Mgr, LWN_Get_Parent(p->Wnlist[j]), write_tree);
    LWN_Copy_Linenumber(LWN_Get_Parent(p->Wnlist[j]),write_tree);
    LWN_Copy_Frequency_Tree(write_tree,loop);
    if (gdepth > 0) {
      vw = dg->Add_Vertex(write_tree);
      if (vw == 0)
        dependence_disaster = TRUE;
    }
    DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
      &LNO_local_pool);
    Build_Doloop_Stack(write_tree, stack); 
    LNO_Build_Access(write_tree, stack, &LNO_default_pool); 
    CXX_DELETE(stack, &LNO_local_pool);
  }

  if (read_tree && write_tree) {
    Du_Mgr->Ud_Add_Def(WN_kid0(write_tree), read_tree);
    Du_Mgr->Du_Add_Use(read_tree, WN_kid0(write_tree));

    // I'm willing to pay the price to recompute this edge. :-)

    if (dependence_disaster == FALSE && vr && vw) {
      DOLOOP_STACK stack(&MIR_local_pool);
      Build_Doloop_Stack(LWN_Get_Parent(loop), &stack);
      if (!dg->Add_Edge(WN_kid0(read_tree), &stack,
                        write_tree, &stack, TRUE, FALSE))
        dependence_disaster = TRUE;
    }
  }

  // Now go through every reference in the loop.  Remove array refs
  // (including the various dg and du edges) and replace with ldids and
  // stids.  There are edges to add to the dependence graph, because what
  // used to point to the interior node now points to the read or write
  // outside.

  DYN_ARRAY<WN*> new_uses(&MIR_local_pool);
  DYN_ARRAY<WN*> new_defs(&MIR_local_pool);

  if (read_tree)
    Unique_AddElement(&new_defs, read_tree, FALSE);
  if (write_tree)
    Unique_AddElement(&new_uses, WN_kid0(write_tree), FALSE);

  for (j = 0; j < p->Wnlist.Elements(); j++) {
    WN*         wn = p->Wnlist[j];
    BOOL        is_write = OPCODE_is_store(WN_opcode(LWN_Get_Parent(wn)));
    VINDEX16    v = dg->Get_Vertex(LWN_Get_Parent(wn));
    EINDEX16    e;

    // For this wn, reset every guard corresponding to all the do loops
    // between this reference and loop, inclusive

    BOOL done = FALSE;
    for (WN* pwn = wn; !done; pwn = LWN_Get_Parent(pwn)) {
      if (WN_opcode(pwn) == OPC_DO_LOOP) {
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(pwn);
	WN *guard = dli->Guard;
	if (guard) {
          WN_Reset_If_Guard(guard);
        }
	if (pwn == loop) {
	  done = TRUE;
        }
      }
    }

    // Now fix dependences

    for (e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
      VINDEX16    v2 = dg->Get_Sink(e);
      INT components = dg->Depv_Array(e)->Num_Dim();
#ifdef Is_True_On
      WN* prnt = dg->Get_Wn(v2);
      while (prnt && prnt != loop)
        prnt = LWN_Get_Parent(prnt);
      if ((components <= gdepth) != (prnt == NULL)) {
        fprintf(TFile, "Disaster: %d %d 0x%p:", components, gdepth, prnt);
        fprintf(TFile, " v=%d 0x%p v2=%d 0x%p e=%d\n",
                v, dg->Get_Wn(v), v2, dg->Get_Wn(v2), e);
        FmtAssert(0,
                  ("Inconsistent component count/parent stuff"));
      }
#endif
      if (components > gdepth)
        continue;

      // add this identical edge from v2 over to vw/vr

      VINDEX16 v = is_write ? vw : vr;
      if (dependence_disaster == FALSE && v &&
          !MIR_Add_Edge(dg, v, v2, dg->Depv_Array(e))) {
        dependence_disaster = TRUE;
        break;
      }
    }

    for (e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) {
      VINDEX16    v2 = dg->Get_Source(e);
      INT components = dg->Depv_Array(e)->Num_Dim();
#ifdef Is_True_On
      WN* prnt = dg->Get_Wn(v2);
      while (prnt && prnt != loop)
        prnt = LWN_Get_Parent(prnt);
      FmtAssert((components <= gdepth) == (prnt == NULL),
                ("Inconsistent component count/parent stuff"));
#endif
      if (components > gdepth)
        continue;

      // add this identical edge from vw/vr over to v2

      VINDEX16 v = is_write ? vw : vr;
      if (dependence_disaster == FALSE && v &&
          !MIR_Add_Edge(dg, v2, v, dg->Depv_Array(e))) {
        dependence_disaster = TRUE;
        break;
      }
    }

    // now change the code and delete old code/DU/dg info

    if (is_write) {
      // replace the parent of wn, which is an istore, with an stid.

      OPCODE    op = OPCODE_make_op(OPR_STID, MTYPE_V, type);
      WN*       oldistore = LWN_Get_Parent(wn);
      FmtAssert(WN_kid1(oldistore) == wn, ("Bad ISTORE child!"));
      WN* newstid = LWN_CreateStid(op, newsym.WN_Offset(), 
				   newsym.St(), pty, WN_kid0(oldistore));
#ifdef TARG_X8664
      if (Minvariant_Removal_For_Simd || type == MTYPE_M)
	Create_alias(Alias_Mgr, newstid);
#endif
      LWN_Copy_Linenumber(oldistore,newstid);
      LWN_Copy_Frequency_Tree(newstid,oldistore);

      // store a bogus value, so delete functions work ok
      WN_kid0(oldistore) =
        WN_CreateIntconst(OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V), 0);
      LWN_Set_Parent(WN_kid0(oldistore), oldistore);

      LWN_Insert_Block_Before(LWN_Get_Parent(oldistore), oldistore, newstid);
      LWN_Extract_From_Block(oldistore);
      LWN_Delete_Tree(oldistore);
      Unique_AddElement(&new_defs, newstid, FALSE);
    }
    else {
      // replace the parent of wn, which is an iload, with an ldid

      OPCODE  op = OPCODE_make_op(OPR_LDID, Promote_Type(type), type);
      WN*     oldiload = LWN_Get_Parent(wn);
      FmtAssert(WN_kid0(oldiload) == wn, ("Bad iload child!"));
      WN*     newldid = WN_CreateLdid(op, newsym.WN_Offset(), 
			   newsym.St(), ty);
#ifdef TARG_X8664
      if (Minvariant_Removal_For_Simd || type == MTYPE_M)
	Create_alias(Alias_Mgr, newldid);
#endif
      WN*     p_oldiload = LWN_Get_Parent(oldiload);
      LWN_Copy_Frequency_Tree(newldid,oldiload);

      INT ikid;
      for (ikid = 0; ikid < WN_kid_count(p_oldiload); ikid++)
        if (WN_kid(p_oldiload,ikid) == oldiload)
          break;
      FmtAssert(ikid < WN_kid_count(p_oldiload), ("Missing kid!"));

      WN_kid(p_oldiload, ikid) = newldid;
      LWN_Set_Parent(newldid, p_oldiload);
#ifdef KEY
      // Bug 1277 - generate a CVT here.
      if ( MTYPE_bit_size( WN_rtype(newldid ) ) == 32 &&
	   MTYPE_bit_size( WN_rtype(p_oldiload ) ) == 64 &&
	   !MTYPE_is_float( WN_rtype( newldid ) ) &&
	   !MTYPE_is_float( WN_rtype( p_oldiload ) ) &&
	   WN_st( newldid ) ) {
	OPCODE cvt_o = OPCODE_make_op(OPR_CVT,WN_rtype(p_oldiload),
				    WN_rtype(newldid));
        WN *cvt = LWN_CreateExp1(cvt_o, newldid);
        LWN_Set_Parent(cvt,p_oldiload);
	WN_kid(p_oldiload, ikid) = cvt;
      }
#endif
      LWN_Delete_Tree(oldiload);
      Unique_AddElement(&new_uses, newldid, FALSE);
    }
  }

  if (dependence_disaster) {
    Is_True(0, ("Dependence graph ran out of space?"));
    LNO_Erase_Dg_From_Here_In(LWN_Get_Parent(loop), dg);
  }

  // conservatively update du chain.  TODO: If anyone seriously needs this
  // down the line, this is ridiculously bad.

  INT ii = 0;
  for (ii = 0; ii < new_defs.Elements(); ii++) {
    for (INT jj = 0; jj < new_uses.Elements(); jj++) {
      Du_Mgr->Ud_Add_Def(new_uses[jj], new_defs[ii]);
      Du_Mgr->Du_Add_Use(new_defs[ii], new_uses[jj]);
    }
  }
#ifdef TARG_X8664
  if (Minvariant_Removal_For_Simd || type == MTYPE_M) {
    for (INT jj = 0; jj < new_uses.Elements(); jj++)
      if (Wn_Is_Inside(new_uses[jj], loop))
        Du_Mgr->Ud_Get_Def(new_uses[jj])->Set_loop_stmt(loop);
  }
#endif

  // Indirect array refs can only become direct when *using* the inner array.
  // So only look at new_uses: see if any array refs become direct as a
  // result of this.  Note that if we have a(b(i),b(i)), b(i) are both hoisted.
  // Then each is on new_uses, and when we climb up the tree, we may add
  // a(mi2,mi3) to newly_direct_lists twice.  That's why we put in the flag
  // saying adding duplicates is okay.

  for (ii = 0; ii < new_uses.Elements(); ii++) {
    WN* parent = LWN_Get_Parent(new_uses[ii]);
    for ( ; parent; parent = LWN_Get_Parent(parent)) {
      OPCODE   opc = WN_opcode(parent);
      if (!OPCODE_is_expression(opc))
        break;
      if (OPCODE_operator(opc) == OPR_ARRAY) {
        DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                      &LNO_local_pool);
        Build_Doloop_Stack(parent, stack);
        LNO_Build_Access(parent, stack, &LNO_default_pool);
        MIR_Maybe_Add_To_List(parent, newly_direct_lists, pool, TRUE);
        CXX_DELETE(stack, &LNO_local_pool);
        break;
      }
    }
  }

  // In the normal minvariant algorithm, we may not care about improving  
  // the dependences, but in Hoist_Messy_Bounds(), this is one of the main
  // reasons for doing this optimization. 

  if (messy_only) 
    MIR_Update_Dependences(loop, &new_uses); 

}

static BOOL MIR_Ok_Hoist_Wn_Dependence(WN* other_wn,
                                       const MIR_REFLIST* p,
                                       WN* loop,
                                       ARRAY_DIRECTED_GRAPH16* dg,
                                       EINDEX16 e)
{
  INT elts = p->Wnlist.Elements();

  // Same list?  That's fine.

  for (INT j = 0; j < elts; j++)
    if (other_wn == LWN_Get_Parent(p->Wnlist[j]))
      return TRUE;

  // Otherwise, other_wn outside the loop?  That's fine too.

  WN* prnt = 0;
  for (prnt = other_wn; prnt; prnt = LWN_Get_Parent(prnt))
    if (prnt == loop)
      break;
  if (prnt == NULL)
    return TRUE;

  // Dependence carried outside the loop? 

  DEPV_ARRAY* dv = dg->Depv_Array(e);
  INT         outer_depth = Do_Depth(loop);
  INT         unused = dv->Num_Unused_Dim();
  INT         dim = dv->Num_Dim();

  for (INT v = dv->Num_Vec() - 1; v >= 0; v--) {
    DEPV* depv = dv->Depv(v);
    BOOL  carried = FALSE;
    for (INT i = 0; i < outer_depth - unused; i++) {
      if (DEP_Direction(DEPV_Dep(depv,i)) == DIR_POS) {
        carried = TRUE;
        break;
      }
    }
    if (carried == FALSE)
      return FALSE;
  }

  return TRUE;
}

static BOOL MIR_Hoistable_Ref(WN* loop,
                              MIR_REFLIST* p,
                              ARRAY_DIRECTED_GRAPH16* dg)
{
  if (Minv_Debug) {
    fprintf(TFile, "In loop %s, trying to hoist", SYMBOL(WN_index(loop)).Name());
    p->Print(TFile);
  }

  // Exclude symbols whose base are in MP reduction statements 
  for (WN* wn = loop; wn != NULL; wn = LWN_Get_Parent(wn)) {
    if (Is_Mp_Region(wn)) {
      WN* wn_first = WN_first(WN_region_pragmas(wn));
      for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
        if (WN_opcode(wn) == OPC_XPRAGMA 
	  && WN_pragma(wn) == WN_PRAGMA_REDUCTION
         && WN_operator(WN_kid0(wn)) == OPR_ARRAY ) {
          if (OPCODE_has_sym(WN_opcode(WN_array_base(WN_kid0(wn))))) {
           if( SYMBOL(WN_array_base(WN_kid0(wn)))
		== SYMBOL(WN_array_base(p->Array))) {
               return FALSE;
            }
          } else {
              return FALSE;
          }
        }
      }
    } 
  } 

  // Step one.  Is this thing invariant in this loop?
  // Use access vectors only for grouping, not for invariance test
  //   because of PV 574185. (RJC)

  INT depth = Do_Loop_Depth(loop);

  // Step one.  Is this thing invariant in this loop?

  for (INT nv = 0; nv < p->Aa->Num_Vec(); nv++) {
    ACCESS_VECTOR*      av = p->Aa->Dim(nv);
    INT                 nest_depth = av->Nest_Depth();

#ifdef Is_True_On
    for (INT ii = nest_depth+1; ii <= depth; ii++) {
      Is_True(av->Loop_Coeff(ii) == 0,
              ("reference in loop of depth=%d, nest_depth=%d, coeff=%d",
               ii, nest_depth, av->Loop_Coeff(ii)));
    }
#endif
    if (av->Non_Const_Loops() > depth) {
      if (Minv_Debug)
        fprintf(TFile, "   ... failed: non_const_loops problem\n");
      return FALSE;
    }
  }

  INT i;
  for (i = 0; i < p->Wnlist.Elements(); i++) {
    WN* wn_ref = p->Wnlist[i];
#ifdef TARG_X8664
    if (WN_operator(wn_ref) == OPR_ARRAY) {
      WN *parent = LWN_Get_Parent(wn_ref);
      if (WN_operator(parent) == OPR_ILOAD &&
	  (WN_desc(parent) == MTYPE_V16F4 ||
	   WN_desc(parent) == MTYPE_V16F8 ||
	   WN_desc(parent) == MTYPE_V16I1 ||
	   WN_desc(parent) == MTYPE_V16I2 ||
	   WN_desc(parent) == MTYPE_V16I4 ||
	   WN_desc(parent) == MTYPE_V16I8))
	return FALSE;	    
    }
#endif /* TARG_X8664 */
    for (WN* wn = wn_ref; wn != NULL; wn = LWN_Get_Parent(wn)) {
      if (WN_opcode(wn) == OPC_DO_LOOP) {
        for (INT j = 0; j < WN_num_dim(wn_ref); j++)
          if (!Is_Loop_Invariant_Exp(WN_array_index(wn_ref, j), wn))
             return FALSE;
      }
      if (wn == loop)
        break;
    }
  }

  // Step two.  See if it's got reads, writes, and executes for sure.
  INT reads = 0;
  INT writes = 0;
  INT for_sure = p->Wnlist.Elements();
  STACK<WN*> stk_good(&MIR_local_pool);

  for (INT j = 0; j < p->Wnlist.Elements(); j++) {
    WN*         wn = p->Wnlist[j];
    OPCODE      op = WN_opcode(LWN_Get_Parent(wn));

    if (OPCODE_is_load(op))
      reads++;
    else if (OPCODE_is_store(op))
      writes++;
    else
      FmtAssert(0, ("Bad parent for array--can't happen op=%d", op));

    WN* pp = NULL;
    WN* np = wn;
    BOOL for_sure_not = FALSE;
    BOOL for_sure_decr = FALSE;
    while (np && np != loop && !for_sure_not) {
      pp = np, np = LWN_Get_Parent(np);
      if (WN_opcode(np) == OPC_DO_LOOP && Do_Loop_Is_Mp(np)) {
        for_sure_not = TRUE;
      } else if (Is_Mp_Region(np)) {
        for_sure_not = TRUE;
      }
      if (!for_sure_decr && WN_opcode(np) == OPC_IF && WN_if_test(np) != pp) {
        for_sure_decr = TRUE;
        for_sure--;
      }
    }

    FmtAssert(np, ("Couldn't find parent of 0x%p(opc=%d)!", pp, WN_opcode(pp)));    if (!for_sure_not) {
      stk_good.Push(wn);
    }
  }

  if (for_sure == 0) {
    if (Minv_Debug)
      fprintf(TFile, "   ... failed: not sure it executes\n");
    return FALSE;
  }

  if (stk_good.Elements() < p->Wnlist.Elements()) {
    p->Wnlist.Resetidx();
    for (INT k = 0; k < stk_good.Elements(); k++)
      p->Wnlist.AddElement(stk_good.Bottom_nth(k));
  }

  if (stk_good.Elements() == 0) {
    if (Minv_Debug)
      fprintf(TFile, "   ... failed: not sure it executes\n");
    return FALSE;
  }

  // Step three.  Does it have dependences that disqualify it?
  // Go through each reference.  If all dependences carried outside
  // the loop, then fine.

  INT elts = p->Wnlist.Elements();

  for (i = 0; i < elts; i++) {
    VINDEX16 v = dg->Get_Vertex(LWN_Get_Parent(p->Wnlist[i]));
    if (v == 0) {
      if (Minv_Debug)
        fprintf(TFile, "   ... failed: dependence graph incomplete\n");
      return FALSE;           // if missing dependences, can't hoist
    }

    EINDEX16 e;
    for (e = dg->Get_Out_Edge(v); e; e = dg->Get_Next_Out_Edge(e)) {
      WN* other_wn = dg->Get_Wn(dg->Get_Sink(e));
      FmtAssert(other_wn && dg->Get_Source(e) == v, ("Broken graph"));
      if (MIR_Ok_Hoist_Wn_Dependence(other_wn, p, loop, dg, e) == FALSE) {
        if (Minv_Debug)
          fprintf(TFile, "   ... failed: dependence conflict edge=%d\n", e);
        return FALSE;
      }
    }

    for (e = dg->Get_In_Edge(v); e; e = dg->Get_Next_In_Edge(e)) {
      WN* other_wn = dg->Get_Wn(dg->Get_Source(e));
      FmtAssert(other_wn && dg->Get_Sink(e) == v, ("Broken graph"));
      if (MIR_Ok_Hoist_Wn_Dependence(other_wn, p, loop, dg, e) == FALSE) {
        if (Minv_Debug)
          fprintf(TFile, "   ... failed: dependence conflict edge=%d\n", e);
        return FALSE;
      }
    }
  }

  // Passed all of the tests. 
  return TRUE; 
} 

static BOOL MIR_Hoist_Ref_If_Sensible(WN* loop,
                                  MIR_REFLIST* p,
                                  ARRAY_DIRECTED_GRAPH16* dg,
                                  DYN_ARRAY<MIR_REFLIST*>& newly_direct_lists,
                                  MEM_POOL* pool, 
				  BOOL messy_only) 
{
  if (messy_only && !MIR_Messy_Subscript(p))
    return FALSE; 

  if (!MIR_Hoistable_Ref(loop, p, dg))
    return FALSE; 

  // Step four.  Do the replacement

  MIR_Replace(loop, p, dg, newly_direct_lists, pool, messy_only);

  if (Minv_Debug)
    fprintf(TFile, "   ... hoisted!\n");

  return TRUE;
}

//------------------------------------------------------------------------

// MIR_Build_Loop_List
//
// Put every reference with a good access array on a list.  For each entry,
// mark if it's invariant in one of its loop.

static void MIR_Build_Loop_List_Array(WN* wn,
                                      DYN_ARRAY<MIR_REFLIST*>& lists,
                                      MEM_POOL* pool,
                                      BOOL      duplicates_okay)
{
  // examine array nodes only.  That's ok -- if something's not on the
  // list, it's still conservative.
  //
  // only interesting if its child is an LDA/LDID.
  // only interesting if its access vector is well behaved.

  WN *parent = LWN_Get_Parent(wn);
  OPCODE parent_op = WN_opcode(parent);
  if (OPCODE_operator(parent_op) == OPR_ADD) {
    OPCODE gparent_op = WN_opcode(LWN_Get_Parent(parent));
    if (!OPCODE_is_load(gparent_op) && !OPCODE_is_store(gparent_op)) {
      return;
    }
  } else if (!OPCODE_is_load(parent_op) && !OPCODE_is_store(parent_op))
    return;

  WN* wnbase = WN_array_base(wn);
  switch (WN_operator(wnbase)) {
   case OPR_LDA:
   case OPR_LDID:
    break;
   default:
    return;
  }

  ACCESS_ARRAY*    aa((ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map, wn));

  if (aa == NULL || aa->Too_Messy)
    return;
  INT i;
  for (i = 0; i < aa->Num_Vec(); i++)
    if (aa->Dim(i)->Too_Messy)
      return;

  ST*              base(Get_ST_Base(wnbase));

  // the reference is good, so add it to one of our lists, if match exists

  for (i = 0; i < lists.Elements(); i++) {
    MIR_REFLIST* p = lists[i];
    if (p->Same(base, aa, wn)) {
      Unique_AddElement(&p->Wnlist, wn, duplicates_okay);
      return;
    }
  }

  // if no match exists, the access vector isn't on any of our lists.  Push it.

  lists.AddElement(CXX_NEW(MIR_REFLIST(base, aa, wn, pool), pool));
}

static void MIR_Build_Loop_List_Walk(WN* wn,
                                     DYN_ARRAY<MIR_REFLIST*>& lists,
                                     MEM_POOL* pool)
{
  OPERATOR opr = WN_operator(wn);

  if (opr == OPR_ARRAY) {
    MIR_Maybe_Add_To_List(wn, lists, pool, FALSE);
  }

  // iterate through the children

  if (opr == OPR_BLOCK) {
    for (WN* w = WN_first(wn); w; w = WN_next(w))
      MIR_Build_Loop_List_Walk(w, lists, pool);
  }
  else if (opr == OPR_IO) {
    // Don't go inside IO for now.  The loops are marked as bad, so no
    // hoisting of these will ever be tried.  Going inside results in
    // problems when Base_Test is called on these nodes.
#ifdef Is_True_On
    while (WN_opcode(wn) != OPC_DO_LOOP)
      wn = LWN_Get_Parent(wn);
    FmtAssert(Get_Do_Loop_Info(wn)->Has_Calls, ("IO error in minvariant"));
#endif
  }
  else {
    for (INT i = 0; i < WN_kid_count(wn); i++)
      MIR_Build_Loop_List_Walk(WN_kid(wn,i), lists, pool);
  }
}

static DYN_ARRAY<MIR_REFLIST*>* MIR_Build_Loop_List(WN* wn, MEM_POOL* pool)
{
  DYN_ARRAY<MIR_REFLIST*>* lists =
    CXX_NEW(DYN_ARRAY<MIR_REFLIST*>(pool), pool);
  MIR_Build_Loop_List_Walk(wn, *lists, pool);
  return lists;
}

static BOOL MIR_Try_Hoist(WN* loop,
                          DYN_ARRAY<MIR_REFLIST*>& lists,
                          ARRAY_DIRECTED_GRAPH16* dg,
                          MEM_POOL* pool,
			  BOOL messy_only)
{
#ifdef KEY // bug 8076: do not hoist if index variable is marked addr_saved
  if (ST_addr_saved(WN_st(WN_kid0(loop))))
    return FALSE;
#endif
  BOOL did_hoisting = FALSE;
  INT  i = 0;

  DYN_ARRAY<MIR_REFLIST*>* newly_direct_lists =
    CXX_NEW(DYN_ARRAY<MIR_REFLIST*>(pool), pool);

  while (i < lists.Elements()) {
    BOOL hoisted = MIR_Hoist_Ref_If_Sensible(loop, lists[i], dg,
      *newly_direct_lists, pool, messy_only);
#if Is_True_On
    LWN_Check_Parentize(loop);
#endif
    if (hoisted) {
      // take it off the list
      did_hoisting = TRUE;
      if (i < lists.Elements() - 1)
        lists[i] = lists[lists.Elements() - 1];
      lists.Decidx();

      for (INT ii = newly_direct_lists->Elements() - 1; ii >= 0; ii--) {
        INT newx = lists.Newidx();
        lists[newx] = (*newly_direct_lists)[ii];
        (*newly_direct_lists).Decidx();
      }
    }
    else {
      i++;
    }
  }

  CXX_DELETE(newly_direct_lists, pool);

  return did_hoisting;
}

static DYN_ARRAY<MIR_REFLIST*>*
MIR_Partition_Lists(WN* loop,
                    MEM_POOL* pool)
{
  if (Minv_Debug) {
    fprintf(TFile, "MIR_Partition_Lists for loop %s, depth %d:\n",
            SYMBOL(WN_index(loop)).Name(), Do_Depth(loop));
  }

  DYN_ARRAY<MIR_REFLIST*>* rval = MIR_Build_Loop_List(loop, pool);

  if (Minv_Debug) {
    fprintf(TFile, "new lists:\n");
    Print_Lists(TFile, *rval);
  }

  return rval;
}


static void MIR_Go_Inside(WN* body,
                          const DYN_ARRAY<MIR_REFLIST*>& lists,
                          ARRAY_DIRECTED_GRAPH16* dg,
			  BOOL messy_only)
{
  FmtAssert(WN_opcode(body) == OPC_BLOCK,
            ("Bad block for MIR_Iterate_Outer_Loops()"));

  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_DO_LOOP:
      {
        DYN_ARRAY<MIR_REFLIST*>* sublists =
          MIR_Partition_Lists(wn, &MIR_local_pool);
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        if (!dli->Has_Bad_Mem && !dli->Has_Gotos)
          MIR_Try_Hoist(wn, *sublists, dg, &MIR_local_pool, messy_only);
        MIR_Go_Inside(WN_do_body(wn), *sublists, dg, messy_only);
        while (sublists->Elements() > 0)
          sublists->Decidx();
        CXX_DELETE(sublists, &MIR_local_pool);
      }
      break;
     case OPC_IF:
      MIR_Go_Inside(WN_then(wn), lists, dg, messy_only);
      MIR_Go_Inside(WN_else(wn), lists, dg, messy_only);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      MIR_Go_Inside(WN_while_body(wn), lists, dg, messy_only);
      break;
     case OPC_REGION:
      MIR_Go_Inside(WN_region_body(wn), lists, dg, messy_only);
      break;
    }
  }
}

static void MIR_Iterate_Outer_Loops(WN* body, 
				    ARRAY_DIRECTED_GRAPH16* dg,
				    BOOL messy_only)
{
  FmtAssert(WN_opcode(body) == OPC_BLOCK,
            ("Bad block for MIR_Iterate_Outer_Loops()"));

  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_DO_LOOP:
      {
        MEM_POOL_Push_Freeze(&MIR_local_pool);
        DYN_ARRAY<MIR_REFLIST*>* lists =
          MIR_Build_Loop_List(wn, &MIR_local_pool);
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        if (!dli->Has_Bad_Mem && !dli->Has_Gotos)
          MIR_Try_Hoist(wn, *lists, dg, &MIR_local_pool, messy_only);
        MIR_Go_Inside(WN_do_body(wn), *lists, dg, messy_only);
        while (lists->Elements() > 0)
          lists->Decidx();
        CXX_DELETE(lists, &MIR_local_pool);
        MEM_POOL_Pop_Unfreeze(&MIR_local_pool);
      }
      break;
     case OPC_IF:
      MIR_Iterate_Outer_Loops(WN_then(wn), dg, messy_only);
      MIR_Iterate_Outer_Loops(WN_else(wn), dg, messy_only);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      MIR_Iterate_Outer_Loops(WN_while_body(wn), dg, messy_only);
      break;
     case OPC_REGION:
      MIR_Iterate_Outer_Loops(WN_region_body(wn), dg, messy_only); 
      break; 
    }
  }
}

extern void Minvariant_Removal(WN *func_nd, ARRAY_DIRECTED_GRAPH16 *dg)
{
  if (mir_local_pool_initialized == FALSE) {
    mir_local_pool_initialized = TRUE;
    MEM_POOL_Initialize(&MIR_local_pool, "MIR_local_pool", FALSE);
  }

  Minv_Debug = Get_Trace(TP_LNOPT, TT_LNO_MINVARIANT_DEBUG);

#ifdef Is_True_On
  LWN_Check_Parentize(func_nd);
#endif
  MIR_Iterate_Outer_Loops(WN_func_body(func_nd), dg, FALSE);
#ifdef Is_True_On
  LWN_Check_Parentize(func_nd);
#endif
}

//-----------------------------------------------------------------------
// NAME: MIR_Messy_Subscript
// FUNCTION: For the OPR_ARRAY node 'wn_array' return TRUE if it has a 
//   parent which is an OPR_ARRAY node that is marked Too_Messy.  Return 
//   FALSE otherwise.  
//-----------------------------------------------------------------------

static BOOL MIR_Messy_Subscript(MIR_REFLIST* mr)
{ 
  for (INT i = 0; i < mr->Wnlist.Elements(); i++) { 
    WN* wn_array = mr->Wnlist[i]; 
    if (Messy_Subscript(wn_array)) 
      return TRUE; 
  } 
  return FALSE; 
}  

//-----------------------------------------------------------------------
// NAME: MIR_Test_Hoist
// FUNCTION: For the 'loop' containing the 'lists' of potential hoistable
//   array elements, set 'can_hoist[i]' if there is a hoistable messy 
//   subscript at the loop at depth 'i'.  If 'can_hoist[i] > 0', set 
//   'lowest_depth[i]' to the depth of the outermost loop enclosing a 
//   reference hoisted at loop depth 'i'. 
//-----------------------------------------------------------------------

static void MIR_Test_Hoist(WN* loop,
                           DYN_ARRAY<MIR_REFLIST*>& lists,
                           ARRAY_DIRECTED_GRAPH16* dg,
			   INT can_hoist[],
			   INT lowest_depth[])
{
  for (INT i = 0; i < lists.Elements(); i++) {
    if (MIR_Messy_Subscript(lists[i]) 
        && MIR_Hoistable_Ref(loop, lists[i], dg)) {
      INT index = Do_Loop_Depth(Enclosing_Do_Loop(lists[i]->Array)); 
      can_hoist[index]++; 
      if (Do_Loop_Depth(loop) < lowest_depth[index])
	lowest_depth[index] = Do_Loop_Depth(loop); 
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: MIR_Test_Outer_Loops
// FUNCTION: For the nodes inside OPC_BLOCK node 'body', set 'can_hoist[i]'
//   if there is a hoistable messy subscript at the loop at depth 'i'. 
//   If 'can_hoist[i] > 0', set 'lowest_depth[i]' to the depth of the 
//   outermost loop enclosing a reference hoisted at loop depth 'i'. 
//-----------------------------------------------------------------------

static void MIR_Test_Outer_Loops(WN* body, 
		                 ARRAY_DIRECTED_GRAPH16* dg,
				 INT can_hoist[],
				 INT lowest_depth[])
{
  FmtAssert(WN_opcode(body) == OPC_BLOCK,
    ("MIR_Test_Outer_Loops(): Bad block"));

  for (WN* wn = WN_first(body); wn; wn = WN_next(wn)) {
    switch (WN_opcode(wn)) {
     case OPC_DO_LOOP:
      {
        MEM_POOL_Push(&LNO_local_pool);
        DYN_ARRAY<MIR_REFLIST*>* lists =
          MIR_Build_Loop_List(wn, &LNO_local_pool);
        DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
        if (!dli->Has_Bad_Mem && !dli->Has_Gotos) 
          MIR_Test_Hoist(wn, *lists, dg, can_hoist, lowest_depth);
        MIR_Test_Outer_Loops(WN_do_body(wn), dg, can_hoist, lowest_depth);
        while (lists->Elements() > 0)
          lists->Decidx();
        CXX_DELETE(lists, &LNO_local_pool);
        MEM_POOL_Pop(&LNO_local_pool);
      }	
      break;
     case OPC_IF:
      MIR_Test_Outer_Loops(WN_then(wn), dg, can_hoist, lowest_depth);
      MIR_Test_Outer_Loops(WN_else(wn), dg, can_hoist, lowest_depth);
      break;
     case OPC_DO_WHILE:
     case OPC_WHILE_DO:
      MIR_Test_Outer_Loops(WN_while_body(wn), dg, can_hoist, lowest_depth); 
      break;
     case OPC_REGION:
      MIR_Test_Outer_Loops(WN_region_body(wn), dg, can_hoist, lowest_depth);
      break; 
    } 
  }
}

//-----------------------------------------------------------------------
// NAME: MIR_Test_SNL
// FUNCTION: For the nodes inside loop 'wn_loop', set 'can_hoist[i]'
//   if there is a hoistable messy subscript at the loop at depth 'i'. 
//   If 'can_hoist[i] > 0', set 'lowest_depth[i]' to the depth of the 
//   outermost loop enclosing a reference hoisted at loop depth 'i'. 
//-----------------------------------------------------------------------

static void MIR_Test_SNL(WN* wn_loop,
                         ARRAY_DIRECTED_GRAPH16* dg,
		         INT can_hoist[],
			 INT lowest_depth[])
{ 
  MEM_POOL_Push(&LNO_local_pool);
  DYN_ARRAY<MIR_REFLIST*>* lists =
    MIR_Build_Loop_List(wn_loop, &LNO_local_pool);
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop);
  if (!dli->Has_Bad_Mem && !dli->Has_Gotos)
    MIR_Test_Hoist(wn_loop, *lists, dg, can_hoist, lowest_depth);
  MIR_Test_Outer_Loops(WN_do_body(wn_loop), dg, can_hoist, lowest_depth);
  while (lists->Elements() > 0)
    lists->Decidx();
  CXX_DELETE(lists, &LNO_local_pool);
  MEM_POOL_Pop(&LNO_local_pool);
} 

//-----------------------------------------------------------------------
// NAME: MIR_Has_Messy_Subscript
// FUNCTION: For all non-bound references in the loop 'wn_loop', set 
//   'can_hoist[i]' if there is a hoistable messy subscript at the loop 
//   at depth 'i'. If 'can_hoist[i] > 0', set 'lowest_depth[i]' to the 
//   depth of the outermost loop enclosing a reference hoisted at loop 
//   depth 'i'.  If 'initialize' is TRUE, initialize 'can_hoist[]' and 
//   'lowest_depth[]'. 
// NOTE: Expects 'wn_loop' to be an SNL. 
//-----------------------------------------------------------------------

extern void MIR_Has_Messy_Subscript(WN* wn_loop, 
				    INT can_hoist[],
				    INT lowest_depth[],
				    BOOL initialize) 
{
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  INT count = SNL_Loop_Count(wn_loop); 
  WN* wn_inner = SNL_Get_Inner_Snl_Loop(wn_loop, count); 
  INT inner_depth = Do_Loop_Depth(wn_inner); 

  if (mir_local_pool_initialized == FALSE) {
    mir_local_pool_initialized = TRUE;
    MEM_POOL_Initialize(&MIR_local_pool, "MIR_local_pool", FALSE);
  }

  if (initialize) { 
    for (INT i = 0; i <= inner_depth; i++) {
      can_hoist[i] = 0; 
      lowest_depth[i] = i;
    }
  } 
  MIR_Test_SNL(wn_loop, dg, can_hoist, lowest_depth); 
} 
  
//-----------------------------------------------------------------------
// NAME: MIR_Hoist_Messy_Subscripts
// FUNCTION: For the loop nest 'wn_loop', hoist all hoistable messy 
//   array subscripts inside loops (not in the bounds.)  
//-----------------------------------------------------------------------

extern void MIR_Hoist_Messy_Subscripts(WN* wn_loop)
{ 
  ARRAY_DIRECTED_GRAPH16* dg = Array_Dependence_Graph; 
  if (mir_local_pool_initialized == FALSE) {
    mir_local_pool_initialized = TRUE;
    MEM_POOL_Initialize(&MIR_local_pool, "MIR_local_pool", FALSE);
  }
  MIR_Iterate_Outer_Loops(LWN_Get_Parent(wn_loop), dg, TRUE); 
} 
