/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


// c-or-c++

/*
 * Volume computation
 * ------------------
 *
 * The hard part of volume computation is in computing the volume for each LG,
 * described in pf_ref.cxx. At the loop level, we go depth first, i.e. 
 * innermost loop outwards. 
 *  - For each loop we first compute the volume of data in a single
 *    iteration, to see if it is localized. This includes the date in inner 
 *    loops as well as the references immediately within the loop.
 *  - If a single_iter of a loop is localized, then we compute the amount of
 *    data in total_iterations, by calling Volume_For_Outer on the inner loops 
 *    and giving them my depth. This value is then returned to the caller as 
 *    the amount of data referenced in a single iteration of the outer loop.
 * At the end of the volume computation, the volume for each loop, both single 
 * and total iter is stored within each loopnode.
 *
 *
 * Determining localized loops
 * -----------------------------
 * 
 * This is basically implemented through the subroutine Find_Loc_Loops on a 
 * loopnode.
 * This routine does the following:
 *  - it walks the loops checking to see if the volume is localized.
 *  - once it finds the localized loops, it stores that information within each
 *    loopnode (_locloop).
 *  - it computes the prefetch vectors for each UGS in each base array that are
 *    within the localized space.
 *  - Finally, the outermost localized loop walks all the prefetch vectors for 
 *    each UGS, looks at the number of cache lines they constitute, and chooses
 *    a versioning/split (Find_Split_Vector ()).
 *  - After choosing the versioning, again the outermost localized loop 
 *    initiates
 *    the actual loop versioning (Split_Loops ()).
 *
 * Determining how to version loops
 * --------------------------------
 * 
 * Find_Loc_Loops has done the work of determining the various prefetch vectors,
 * and the counts for each. The outermost localized loop then calls 
 * Find_Split_Vector.
 * This routine currently walks all the prefetch vectors and simply picks the 
 * one which is both deepest, and then has the highest count. However, this 
 * heuristic could be made smarter, and in particular augmented to perhaps 
 * choose a union of non-overlapping prefetch vectors. In any case, 
 * Find_Split_Vector can be made arbitrarily smart so long as it computes a 
 * version vector.
 *
 * The chosen split is stored in the loopnode for the outermost localized loop,
 * and is used when generating prefetches, next.
 *
 * Actually Versioning the Loops
 * -----------------------------
 *
 * If the outermost localized loop finds the Find_Split_Vector returns a 
 * non-empty versioning, then it calls Split_Loops with the desired split_vec. 
 * Split_Loops does the actual versioning of the loops. 
 * Amongst other things, it must be careful to
 *  - walk up the leaf loopnode and make sure that the current loopnode is on a
 *    path from the leaf to the root and needs versioning, since there may be 
 *    multiple loops at the same level in the nest, and a simple split_vec 
 *    would not identify the loops uniquely. Hence we need to store the leaf 
 *    loopnode in the PF_SPLIT_VECTOR.
 *  - Keep track of the references in the new versions of the loop.
 *    This is implemented by creating a map - version_map - that is attached 
 *    to each array node and points to the next instance of that reference in 
 *    the next version, in a left-to-right fashion. So all the versions of a 
 *    reference are strung together through version_map pointers.
 *    This map is currently also used in updating the dependence graph.
 *  - while splitting the loops, care must be taken to preserve
 *      - alias information
 *      - dependence graph
 *  - in the current code each loop is versioned just once, in the original 
 *    instance of the code. Another way to interpret this is that we currently 
 *    support only a single split_vector. We may further wish to extend this to
 *    support multiple, non-conflicting split-vectors, in which case more than 
 *    one version of inner loops may themselves get versioned.
 *
 * Generating prefetches
 * ---------------------
 * Initiated by calling Gen_Prefetch with a NULL split vector. 
 * This basically calls Gen_Prefetch on each base array with the split vector.
 * The base array calls Gen_Prefetch on each UGS, which in turn calls 
 * Gen_Prefetch on the locality group at the depth of the outermost localized 
 * loop. Gen_Prefetch on an LG calls Gen_PrefNode for each set of
 * references in the same cache line, which does the main work.
 *  - for each prefetch, generate a prefetch node with stride information
 *  - for each load/store miss, annotate with a pointer to an intermediate node
 *    that stores 
 *      a. a pointer to the prefetch node
 *      b. separation in trips along innermost loop from leading reference.
 * Prefetches are generated for each level of the cache.
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>
#include <limits.h>
#include "defs.h"
#include "pf_loop.h"
#include "pf_ref.h"
#include "pf_manual.h"
#include "lwn_util.h"
#include "lnopt_main.h"
#include "pf_cg.h"
#include "tlog.h"

#include "targ_sim.h"
#include "opt_du.h"
#include "lnoutils.h"
#include "wn_tree_util.h"               // for tree iterators

extern WN* Find_SCF_Inside(WN* parent_wn, OPCODE opc); // in ff_utils.cxx
#define minof(x, y) (((x)<(y)) ? (x) : (y))

extern INT64 Get_Good_Num_Iters (DO_LOOP_INFO *dli);

PF_LOOPNODE::~PF_LOOPNODE () {
  while (_child.Elements()) CXX_DELETE (_child.Pop(), PF_mpool);
  while (_bases.Elements()) CXX_DELETE (_bases.Pop(), PF_mpool);
}

static BOOL Contains_Array(WN *exp)
{
  if (WN_operator(exp) == OPR_ARRAY) {
    return TRUE;
  }
  for (INT kid=0; kid < WN_kid_count(exp); kid++) {
    if (Contains_Array(WN_kid(exp,kid))) {
      return TRUE;
    }
  }
  return FALSE;
}

/***********************************************************************
 *
 * Given a whirl node, walk up the tree:
 *  - return TRUE  if there is a DO LOOP before an MP region
 *  - return FALSE if there is an MP region before a DO LOOP
 *  - return FALSE if neither is found.
 *
 ***********************************************************************/
static BOOL Loop_Before_MP_Region (WN* wn) {
  while (wn) {
    if (WN_operator(wn) == OPR_DO_LOOP) return TRUE;
    if (Is_Mp_Region(wn)) return FALSE;
    wn = LWN_Get_Parent(wn);
  }
  DevWarn ("Reference not contained within a DO-LOOP");
  return FALSE;
}

#ifdef KEY
static BOOL Has_Indirect_Ref (WN* wn)
{
  if (WN_operator(wn) == OPR_ILOAD) return TRUE;
  for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
    if (Has_Indirect_Ref(WN_kid(wn, kid)))
      return TRUE;
  return FALSE;
}
#endif 
#if defined(TARG_X8664) || defined(TARG_IA64)
// i = i + inv -- stride is just inv (inv - non const)
static WN *Obtain_Stride_From_Loop_Step(WN *loop)
{
  WN *add = WN_kid0(WN_step(loop));
  if(WN_operator(add) != OPR_ADD)
    return NULL;

 if(WN_operator(WN_kid0(add))==OPR_LDID &&
     SYMBOL(WN_kid0(add)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid1(add),loop) &&
     WN_operator(WN_kid1(add)) != OPR_INTCONST)
     return WN_kid1(add); //i = i + inv

 if(WN_operator(WN_kid1(add))==OPR_LDID &&
     SYMBOL(WN_kid1(add)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid0(add),loop) &&
     WN_operator(WN_kid0(add)) != OPR_INTCONST)
    return WN_kid0(add); //i = inv + i

  return NULL;
}

static BOOL Is_Step_One_Loop(WN *loop)
{
  WN *add = WN_kid0(WN_step(loop));
  if(WN_operator(add) != OPR_ADD)
    return FALSE;

 if(WN_operator(WN_kid0(add))==OPR_LDID &&
     SYMBOL(WN_kid0(add)) == SYMBOL(WN_index(loop)) &&
    WN_operator(WN_kid1(add))== OPR_INTCONST &&
    WN_const_val(WN_kid1(add))==1)
     return TRUE; //i = i + 1

 if(WN_operator(WN_kid1(add))==OPR_LDID &&
     SYMBOL(WN_kid1(add)) == SYMBOL(WN_index(loop)) &&
    WN_operator(WN_kid0(add))== OPR_INTCONST &&
    WN_const_val(WN_kid0(add))==1)
     return TRUE; //i = 1 + i

 return FALSE;

}

//inv*i -- inv is actual stride
static WN *Stride_From_MPY(WN *mpy, WN *loop)
{
  if(!mpy || WN_operator(mpy) != OPR_MPY)
     return NULL;

  if(WN_operator(WN_kid0(mpy))==OPR_LDID &&
     SYMBOL(WN_kid0(mpy)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid1(mpy),loop) &&
     WN_operator(WN_kid1(mpy)) != OPR_INTCONST)//don't handle constant
     return WN_kid1(mpy);

  if (WN_operator(WN_kid1(mpy))==OPR_LDID &&
     SYMBOL(WN_kid1(mpy)) == SYMBOL(WN_index(loop)) &&
     Is_Loop_Invariant_Exp(WN_kid0(mpy),loop) &&
      WN_operator(WN_kid0(mpy)) != OPR_INTCONST)//don't handle constant
     return WN_kid0(mpy);

  return NULL;
}

//a[inv*i+b] and stride one loop, actual stride is "inv" between two iters
//TODO: right now we only handle a*i+b, we may able to find more cases
static WN *Obtain_Stride_From_Array_Index(WN *array_index, WN *loop)
{
  if(WN_operator(array_index)==OPR_MPY) //a*i
   return Stride_From_MPY(array_index, loop);

  if(WN_operator(array_index)==OPR_ADD){ // a*i+b or b + a*i
     WN *stride = Stride_From_MPY(WN_kid0(array_index), loop);
    if(stride && Is_Loop_Invariant_Exp(WN_kid1(array_index),loop))
      return stride;
     stride = Stride_From_MPY(WN_kid1(array_index), loop);
    if(stride && Is_Loop_Invariant_Exp(WN_kid0(array_index),loop))
      return stride;
  }
  return NULL;
}

/*
   This function analyzes an OPR_ARRAY whether it has an inductive base
   address. An "inductive base aadress ARRAY" is defined as the following. 
   (1) the all dimensions have constant strides, 
   (2) the the base address of the ARRAY is inductive (a linear expression 
       of the innermost loop index var), and
   (3) the stride of the inductive expression is a compile-time constant.

   Following is an example.

   {int loopend; loopend=sites_on_node; 
    site *lattice, *s;

     for( i=0, s=lattice ; i<loopend; i++,s++ ){
       register double  ar,br;

       ar = s->tempmat1->e[0][0]; 
       ...
       br = s->link[dir[j]]->e[2][2]; 
     }
   }

   Both s->tempmat1->e[0][0] and s->link[dir[j]]->e[2][2] meet the above
   definition and are treated as ARRAYs with inductive base addresses.

   If an ARRAY has an (linear) inductive base address expression, this 
   function returns the WN contains the (constant) stride.

   We currently exclude the cases with small strides even if they meet the
   inductive base address definition. This is for a profitability
   consideration. 

   A variant is called indirect inductive base address, which follows
   the above definition except of replacing (2) with the following.
   - the base address of the ARRAY contains one indirect load whose address 
     is inductive and the rest of the base address is loop invariant.

   The *inductive_base and *indirect_base are marked accordingly.

   **base returns a load which has a symbol for PF_BASE_ARRAY to track.
   Since PF_BASE_ARRAY typically tracks the symbol of the load at the kid0
   of OPR_ARRAY, but an inductive base address case does not necessarily
   have a load, we try to deterministically get a load for this purpose.
   But don't count what exactly this load is within the entire base address
   expression.
 
 */
WN *Inductive_Base_Addr_Const_Stride(WN *array, WN *loop, WN **base, 
                          BOOL *inductive_base, BOOL *indirect_base, mINT32 *stride_val)
{
  int kid;
  *inductive_base = FALSE;
  *indirect_base = FALSE;
  *stride_val = 0;

  // Prefetching the memory accesses of inductive base addresses is enabled
  // only at AGGRESSIVE_PREFETCH.
  if (!LNO_Prefetch_Inductive ||
      LNO_Run_Prefetch < AGGRESSIVE_PREFETCH) {
    return NULL;
  }

  // Check if the subscript address expression in each dimension is a
  // constant.
  for(kid = WN_num_dim(array)+1; kid < WN_kid_count(array); kid++) {
    if(WN_operator(WN_kid(array, kid)) != OPR_INTCONST) {
      return NULL;
    }
  }

  WN *array_base_addr = WN_kid0(array);
 
  if (WN_operator(array_base_addr) == OPR_LDID) {
    // Get its def
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(array_base_addr);
    WN* def = NULL;
    DEF_LIST_ITER d_iter(def_list);
    /* Not considering the incomplete cases here. Be aggressive.
       If in rare cases an aliasing does appear, we may prefetch
       undesirable cache lines but won't produce wrong code.
     */
    for (DU_NODE* dnode=d_iter.First(); !d_iter.Is_Empty();
                  dnode=d_iter.Next()) { 
      if (def != NULL) {
        return NULL;  // Encounter multiple defs
      }
      def=dnode->Wn();
    }

    // Make sure the def is in the same loop.
    WN *wn_loop = LWN_Get_Parent(def);
    while (wn_loop && (WN_opcode(wn_loop)!=OPC_DO_LOOP)) {
      wn_loop = LWN_Get_Parent(wn_loop);
    }

    if (WN_operator(def) == OPR_STID &&
        wn_loop == loop) {
      array_base_addr = WN_kid0(def);
      PF_PRINT( fprintf(TFile, "Base address defined by a previous WN. \n");
                fdump_tree(TFile, def); );
    } else {
      return NULL;  
    }
  }

  // Search for an loop index load in the tree.
  WN *loop_index_ld = NULL;
  for (WN_TREE_ITER<PRE_ORDER, WN*> iter (array_base_addr);
       iter.Wn () != NULL; ++iter) {
    WN *wn = iter.Wn ();

    if (WN_operator(wn) == OPR_LDID &&
        SYMBOL(wn) == SYMBOL(WN_index(loop))) {
      // The candidate tree should have no more than one loop index load.
      if (loop_index_ld == NULL) {
        loop_index_ld = wn;
      } else {
        loop_index_ld = NULL;
        break;
      }
    }
  }

  if (loop_index_ld == NULL) {
    return NULL;
  }

  /* Check if the ancestor chain of the loo_index_ld forms a linear inductive
     expression (a * i + b).
   */
  WN *ancestor = LWN_Get_Parent(loop_index_ld);
  WN *cur_wn = loop_index_ld;
  WN *const_stride_wn = NULL;
  WN *first_add = NULL;
  *stride_val = 1;

  // Traverse all ADDs.
  while (ancestor != NULL && 
         (WN_operator(ancestor) == OPR_ADD ||
          WN_operator(ancestor) == OPR_CVT ||
          WN_operator(ancestor) == OPR_MPY) ) {
    if (WN_operator(ancestor) == OPR_ADD &&
        first_add == NULL) {
      first_add = ancestor;
    } else if (WN_operator(ancestor) == OPR_MPY) {
      WN *sibling = NULL;
      if (WN_kid0(ancestor) == cur_wn) {
        sibling = WN_kid1(ancestor);
      } else {
        sibling = WN_kid0(ancestor);
      }
      if (WN_operator(sibling) == OPR_INTCONST) {
        const_stride_wn = sibling;
        *stride_val *= WN_const_val(sibling);
      } else {
        return NULL;
      }
    }

    cur_wn = ancestor;
    ancestor = LWN_Get_Parent(ancestor);
  }

  if (cur_wn == array_base_addr) {
    // Found a linear inductive expression.
    if (*stride_val < (Cache.LineSize(1)/2)) {
      // If the stride is too small (for now less than half of a cache line),
      // don't consider it for this prefetching. This exclusion is for a
      // profitability consideration.
      return NULL;
    } 
    PF_PRINT( fprintf(TFile, "Found an inductive base addr \n"); );
    *inductive_base = TRUE;
  } else if (WN_operator(ancestor) == OPR_ILOAD &&
             ancestor == array_base_addr &&
             LNO_Prefetch_Induc_Indir) {
    // Found an indirect load through a linear inductive expression.
    PF_PRINT( fprintf(TFile, "Found an indirect inductive base addr \n"); );
    *inductive_base = TRUE;
    *indirect_base = TRUE;
  } else {
    PF_PRINT( fprintf(TFile, "Possibly missing a prefetching candidate with inductive base address \n"); );
  }

  if (*inductive_base) {
    FmtAssert(first_add, ("Unable to locate ADD in an linear inductive expr."));
    for (WN_TREE_ITER<PRE_ORDER, WN*> iter (first_add);
       iter.Wn () != NULL; ++iter) {
      WN *wn = iter.Wn ();
      if ((WN_operator(wn) == OPR_LDID ||
           WN_operator(wn) == OPR_ILOAD ||
           WN_operator(wn) == OPR_LDA) &&
          wn != loop_index_ld) {
        *base = wn;
        break;
      }
    }
    FmtAssert(*base, ("Unable to locate a base load."));

    /* Return a constant stride. */
    return const_stride_wn;
  }

  return NULL;
}

//-------------------------------------------------------------------
//Return a stride for the array if we can find a "loop" invariant one
//
//Right now we handle two simple cases:
//case 1: array base is loop induction variable
//      for(a=...; ...; a=a+inv)
//          ...a[5]...
//case 2: array index implies invariant stride
//      for(i=0; ...; i++)
//           ...a[inv*i+5]...
//
//TODO: find more cases and release the singly-nested loop constraint
//-------------------------------------------------------------------
WN *Simple_Invariant_Stride_Access(WN *array, WN *loop, BOOL ck_induc_base,
                                   BOOL *inductive_base, BOOL *indirect_base)
{

  INT kid;
  if(!LNO_Prefetch_Invariant_Stride) //users don't want
    return NULL;

  ACCESS_ARRAY *aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,array);

  if (!aa || aa->Too_Messy || WN_element_size(array)<0)
   return NULL;

  if (!LNO_Prefetch_Inductive ||
      LNO_Run_Prefetch < AGGRESSIVE_PREFETCH) {
    // Don't deal with inductive base address cases.
    //we only handle one dimensional array now
    if(WN_num_dim(array) != 1) return NULL;
  } else {
    /* The dim could be 0, but the base could be inductive. */
    if(WN_num_dim(array) > 1) {
      // We also want to deal with the case where a multi-dimenional
      // array with constant indexing but the base is inductive.
      for(kid = WN_num_dim(array)+1; kid < WN_kid_count(array); kid++) {
        if(WN_operator(WN_kid(array, kid)) != OPR_INTCONST) {
          return NULL;
        }
      }
    }
  }

  //we only process singly-nested loop now
  DO_LOOP_INFO *dli = (DO_LOOP_INFO *)WN_MAP_Get(LNO_Info_Map,loop);
  if(!dli || !dli->Is_Inner)
     return NULL;

  WN *base;
  mINT32 stride_val = 0;
  if (ck_induc_base) {
    WN *stride = Inductive_Base_Addr_Const_Stride(array, loop, &base, 
                        inductive_base, indirect_base, &stride_val);
    if (stride != NULL) {
      return stride;
    }
  } else {
    *inductive_base = FALSE;
    *indirect_base = FALSE;
  }

  // For an inductive base addr, we will deal with it even there are outer
  // loops, though the prefetches will be generated w.r.t. the innermost
  // loops.

  WN *up_nest = LWN_Get_Parent(loop);
  while(up_nest != NULL){ //we only consider singly-nested
    if(WN_opcode(up_nest) == OPC_DO_LOOP)
      return NULL;
    up_nest = LWN_Get_Parent(up_nest);
  }

  if(!Is_Loop_Invariant_Exp(WN_kid0(array), loop)){//base is not loop invariant

  for(kid = WN_kid_count(array)-1; kid>0; kid--){
    if(!Is_Loop_Invariant_Exp(WN_kid(array, kid),loop)) {
      return NULL; //could not tolerate other invariants
    }
  }

   if(WN_operator(WN_kid0(array)) != OPR_LDID || //base must be the loop index
      SYMBOL(WN_kid0(array)) != SYMBOL(WN_index(loop))) {
     return NULL;
   }

   return Obtain_Stride_From_Loop_Step(loop);
  }//end if

  //case2:
  if(!Is_Step_One_Loop(loop))
   return NULL;

  //----------------------------------------------------------------
  //if the loop index is i=i+1, and array index expression is: a*x+b
  //where a and b are loop invariant, then we can still figure out
  //the in variant stride, and gen prefetches.
  //----------------------------------------------------------------
  for(kid = 0; kid < WN_kid_count(array)-1; kid++)
   if(!Is_Loop_Invariant_Exp(WN_kid(array, kid),loop))
     return NULL;

  return Obtain_Stride_From_Array_Index(WN_kid(array,WN_num_dim(array)<<1), loop);
}

#endif

/***********************************************************************
 *
 * Add_Ref - If reference is reasonable, 
 * then add it to the appropriate base array.
 * (create a new base array if necessary)
 *
 ***********************************************************************/
void PF_LOOPNODE::Add_Ref (WN* wn_array) {
  BOOL messy = FALSE;
  BOOL inductive_base = FALSE;
  BOOL indirect_base = FALSE;

  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn_array);

  if (!array || array->Too_Messy) {
    messy = TRUE;
  }

  // if the access-array (index expression for array)
  // contains a variable that varies in the loop containing the
  // array reference ("innermost" loop), then don't analyze the reference.
  // TODO: currently we do analyse references that have non-const-loops,
  // just not innermost. Although we won't get incorrect code, 
  // we could get erroneous analysis and poor prefetch generation.
  if (array && array->Non_Const_Loops() > 0) {
    // Non_Const_Loops is one greater than our _depth.
    if (array->Non_Const_Loops() == (_depth+1)) {
      messy = TRUE;
    }
  }

  if (array) {

#ifdef KEY 
    // Bug 1656, 2945 - filter out indirect references from the messy vectors
    // to enable aggressive prefetching under AGGRESSIVE_PREFETCH.
    // TODO: If this is always safe, then we could make it default.
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
#if defined(TARG_IA64)
      // For IA64, prefetch can be more aggressive.
      if (av->Contains_Non_Lin_Symb() || av->Too_Messy) {
	  if (Has_Indirect_Ref(wn_array))
	    messy = TRUE;
      }
#else
      if (av->Contains_Non_Lin_Symb()) {
        messy = TRUE;
      } else if (av->Too_Messy) {
	if (LNO_Run_Prefetch == AGGRESSIVE_PREFETCH) {
	  if (Has_Indirect_Ref(wn_array))
	    messy = TRUE;
	} else
	  messy = TRUE;
      }
#endif
    }
#else
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if (av->Too_Messy || av->Contains_Non_Lin_Symb()) {
        messy = TRUE;
      }
    }
#endif
  }

  if (WN_element_size(wn_array) < 0) {
#ifdef TARG_X8664
      //bug 5945: if the loop was vectorized
      //the array is guaranteed to be contiguous, and we can prefetch
      WN *parent = LWN_Get_Parent(wn_array);
     if((WN_desc(parent) != MTYPE_V && MTYPE_is_vector(WN_desc(parent)))||
       (WN_rtype(parent) != MTYPE_V && MTYPE_is_vector(WN_rtype(parent))));
     else
#endif
       // Funny F90 strided array; pass for now (Guarding Loop for prefetch is expensive?)
        messy = TRUE;
  }

//bug 14291: It is aggressive to prefetch for a field of a Large structure, where the structure
//is the array element. The case is similar to prefetch for non-continuous array, we can not
//make sure whether the prefetched cache line will ever be used.
#ifdef KEY
 if(LNO_Run_Prefetch < AGGRESSIVE_PREFETCH &&
    WN_element_size(wn_array) > 16 && 
    WN_element_size(wn_array)%64 != 0){ //64 should be the cache line size in byte.
     messy = TRUE;
  }
#endif

  if (LNO_Prefetch_Indirect && messy) {
    BOOL is_indirect = FALSE;
    for (INT kid=0; !is_indirect && kid<WN_kid_count(wn_array); kid++) {
      if (Contains_Array(WN_kid(wn_array,kid))) {
        is_indirect = TRUE;
      }
    }

    if (is_indirect) {
      UINT32 flag=0;
      WN *parent = LWN_Get_Parent(wn_array);
      WN_OFFSET offset = WN_offset(parent);
      WN *wn_block = parent;
      while (wn_block && (WN_opcode(wn_block)!=OPC_DO_LOOP)) {
        wn_block = LWN_Get_Parent(wn_block);
      }
      if (!wn_block) {
        _num_bad++;
        return;
      } else {
        wn_block = WN_do_body(wn_block);
      }
       
      if (OPCODE_is_load(WN_opcode(parent))) {
        PF_SET_READ(flag);
      } else {
        PF_SET_WRITE(flag);
      }
      PF_SET_CONFIDENCE(flag,0);
      UINT32 save_flag = flag;

      PF_SET_STRIDE_1L(flag,1);
      WN *arraynode = LWN_Copy_Tree(wn_array,TRUE,LNO_Info_Map);
      LWN_Copy_Def_Use(wn_array, arraynode, Du_Mgr);
      WN* pfnode = LWN_CreatePrefetch (offset, flag, arraynode);
      LWN_Insert_Block_Before (wn_block, WN_first(wn_block), pfnode);
      LWN_Copy_Frequency_Tree (pfnode, WN_first(wn_block));
      extern MEM_POOL PF_CG_mpool;
      PF_POINTER *tmp = CXX_NEW (PF_POINTER, &PF_CG_mpool);
      WN_MAP_Set (WN_MAP_PREFETCH, parent, tmp);
      PF_PTR_flag(tmp) = 0;
      SET_AUTO(tmp);
      PF_PTR_wn_pref_1L(tmp) = pfnode;
      PF_PTR_lrnum_1L(tmp) = 0;
      PF_PTR_distance_1L(tmp) = offset;
      PF_PTR_set_conf_1L(tmp, 0);

      flag = save_flag;
      PF_SET_STRIDE_2L(flag,1);
      arraynode = LWN_Copy_Tree(wn_array,TRUE,LNO_Info_Map);
      LWN_Copy_Def_Use(wn_array, arraynode, Du_Mgr);
      pfnode = LWN_CreatePrefetch (offset, flag, arraynode);
      LWN_Insert_Block_Before (wn_block, WN_first(wn_block), pfnode);
      LWN_Copy_Frequency_Tree (pfnode, WN_first(wn_block));
      PF_PTR_wn_pref_2L(tmp) = pfnode;
      PF_PTR_lrnum_2L(tmp) = 0;
      PF_PTR_distance_2L(tmp) = offset;
      PF_PTR_set_conf_2L(tmp, 0);
    }
    return;
  } else if (messy) {
#if defined(TARG_X8664) || defined(TARG_IA64) //bug 10953: for cases may not be "messy"
     if(!Simple_Invariant_Stride_Access(wn_array, _code, TRUE,
                                        &inductive_base, &indirect_base)){
       PF_PRINT( fprintf(TFile, "failing Simple_Invariant_Stride_Access(), _num_bad= %d\n", _num_bad); );
#endif
    _num_bad++;
    return;
#if defined(TARG_X8664) || defined(TARG_IA64) //bug 10953
   }
#endif
  }

  // Find which element in the stack contains our base array
  WN *base = WN_array_base(wn_array);

  if ((WN_operator(base) != OPR_LDA) &&
      (WN_operator(base) != OPR_LDID)) {

     WN *stride = NULL;

#if defined(TARG_X8664) || defined(TARG_IA64) //bug 10953
     mINT32 stride_val = 0;
     stride = Inductive_Base_Addr_Const_Stride(wn_array, _code, &base, 
                                &inductive_base, &indirect_base, &stride_val);
#endif

     if (stride != NULL) {
       inductive_base = TRUE;
     } else {
       _num_bad++;
       return;
     }
  }

  SYMBOL symb (base);
  if (mpf_syms->In_Manual (&symb)) {
    VB_PRINT (printf ("symbol prefetched manually: ");
              symb.Print(stdout);
              printf ("\n"));
    return;
  }

  WN* parent_ref = LWN_Get_Parent (wn_array);
  if (WN_MAP_Get(WN_MAP_PREFETCH, parent_ref)) {
    VB_PRINT (printf ("reference prefetched manually, ignoring\n"));
    return;
  }

  if (!Steady_Base(wn_array) && !inductive_base) {
    // base of array seems to vary in loop.
    _num_bad++;
    return;
  }

  if (!Loop_Before_MP_Region(wn_array)) {
    _num_bad++;
    return;
  }

  for (INT i=0; i<_bases.Elements(); i++) {
    if (_bases.Bottom_nth(i)->Add_Ref (wn_array, TRUE, inductive_base)) return;
  }

  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(&symb), PF_mpool);
  _bases.Push (CXX_NEW (PF_BASE_ARRAY (tmp_symb, wn_array, array->Num_Vec(), 
                                       this, inductive_base, indirect_base),
                        PF_mpool));
  
  BOOL tmp = _bases.Bottom_nth(_bases.Elements()-1)->Add_Ref (wn_array, FALSE,
                                                              inductive_base);
  Is_True (tmp, ("Strange -- ref doesn't match sample ref"));
}

/***********************************************************************
 *
 * Given an Istore to an array, return TRUE iff the array base
 * is an LDA of an ST marked not-used.
 *
 ***********************************************************************/
static BOOL Store_Is_Useless (const WN* istore_wn) {
  Is_True (istore_wn && WN_operator(istore_wn) == OPR_ISTORE &&
           WN_operator(WN_kid1(istore_wn)) == OPR_ARRAY,
           ("Store_Is_Useless called incorrectly\n"));
  WN* array_wn = WN_kid1(istore_wn);
  WN* base_wn = WN_array_base(array_wn);
  if (WN_operator(base_wn) == OPR_LDA &&
      ST_is_not_used(WN_st(base_wn))) return TRUE;
  return FALSE;
}

#ifdef KEY
// Return 1 if there is an 'if' stmt immediately inside the loop
static BOOL Is_Multi_BB (const WN* loop)
{
  FmtAssert (LNO_Run_Prefetch == SOME_PREFETCH, ("Should not have reached here"));
  OPCODE opcode = WN_opcode(loop);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (loop);
    while (kid) {
      if (WN_opcode(kid) == OPC_IF)
	return TRUE;
      kid = WN_next(kid);
    }
    return FALSE;
  }

  for (INT kidno=0; kidno<WN_kid_count(loop); kidno++) {
    WN *kid = WN_kid(loop,kidno);
    if (WN_opcode(kid) == OPC_IF)
      return TRUE;
  }
  return FALSE;
}
#endif

/***********************************************************************
 *
 * Walk the whirl code looking for array references.
 * Pass the array references to Add_Ref to add to the
 * references within _bases.
 * Walk the whirl tree recursively.
 * Stack the do-loops within this node in the loopnode (in _child)
 * the references within those are processed later.
 *
 ***********************************************************************/
void PF_LOOPNODE::Process_Refs (WN* wn) {
  if (!wn) return;

  OPCODE opcode = WN_opcode(wn);

#ifdef KEY
  if (LNO_Run_Prefetch == SOME_PREFETCH && OPCODE_operator(opcode) == OPR_IF) {
  // Don't prefetch references within an 'if' stmt
    Process_Refs (WN_if_test (wn));
    return;
  }
#endif

  if (OPCODE_operator(opcode) == OPR_PREFETCH) {
    // don't do auto-prefetch analysis on references within a manual
    // prefetch, such as #pragma prefetch_ref=a[b[i]], don't analyze
    // b[i] in auto-analysis.
    return;
  }

  if ((OPCODE_operator(opcode) == OPR_PRAGMA) &&
      (WN_pragma(wn) == WN_PRAGMA_PREFETCH_REF)) {
    // manual prefetch. Store the size ..
    if (LNO_Run_Prefetch_Manual) _manual_volume += WN_pragma_arg2(wn);
    return;
  }

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (WN_opcode(kid) == OPC_DO_LOOP) {
        PF_LOOPNODE* childnode =
          CXX_NEW (PF_LOOPNODE (this, kid, _depth+1), PF_mpool);
        _child.Push (childnode);
        Process_Refs (WN_start(kid));
        Process_Refs (WN_end(kid));
        Process_Refs (WN_step(kid));
      }
      else Process_Refs (kid);
      kid = WN_next(kid);
    }
    return;
  } 

  if (OPCODE_operator(opcode) == OPR_ILOAD) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
      PF_PRINT( fprintf(TFile, "\nProcess_Refs() triggers a new pref candidate.\n");
                fdump_tree(TFile, wn););
      Add_Ref (WN_kid0(wn));
    } else {
      _num_bad++;
    }
  } else if (OPCODE_operator(opcode) == OPR_ISTORE) {
#ifdef KEY
    if (LNO_Prefetch_Stores)
    {
#endif
    if (WN_operator(WN_kid1(wn)) == OPR_ARRAY &&
        !Store_Is_Useless(wn)) {
      PF_PRINT( fprintf(TFile, "\nProcess_Refs() triggers a new pref candidate.\n");
                fdump_tree(TFile, wn); );
      Add_Ref (WN_kid1(wn));
    } else {
      _num_bad++;
    }
#ifdef KEY
    } else _num_bad++;	// Don't prefetch store array accesses
#endif
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    if (WN_opcode(kid) == OPC_DO_LOOP) {
      PF_LOOPNODE* childnode =
        CXX_NEW (PF_LOOPNODE (this, kid, _depth+1), PF_mpool);
      _child.Push (childnode);
      Process_Refs (WN_start(kid));
      Process_Refs (WN_end(kid));
      Process_Refs (WN_step(kid));
    }
    else Process_Refs (kid);
  }
}

/***********************************************************************
 *
 * Process all the references within this loop,
 * and within all the nested children loops.
 * At the end of this procedure, all the "good" array references 
 * within this loop (and nested loops) will be organized
 * into uniformly generated sets.
 *
 ***********************************************************************/
void PF_LOOPNODE::Process_Loop () {
  // FIRST process the references within this loop, 
  // which will only walk the references immediately 
  // within this loop, and will (as a by-product) put all the
  // immediately nested loops in _child
  WN * w = WN_do_body(_code);
#ifdef KEY
  DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map, _code);
  BOOL single_small_trip_loop = FALSE; 
 // BOOL simple_copy_loop = FALSE; //bug 8560 disable this

  if (LNO_Run_Prefetch != AGGRESSIVE_PREFETCH && dli->Is_Inner) {
    // Check if loop is not inside a nested loop (outermost loop) and if the
    // trip count is small then avoid inserting prefetches - bug 2958
    WN* parent = LWN_Get_Parent(_code);
    while (parent && WN_opcode(parent) != OPC_DO_LOOP)
      parent = LWN_Get_Parent(parent);
    if (!parent && 
	((!dli->Num_Iterations_Symbolic && 
	  dli->Est_Num_Iterations < 100) ||
	 (dli->Num_Iterations_Symbolic &&
	  LNO_Num_Iters < 100)))
      single_small_trip_loop = TRUE;
  }
  if (dli->Loop_Align_Peeled && !dli->Loop_Vectorized)
    single_small_trip_loop = TRUE;

  if ((LNO_Run_Prefetch > SOME_PREFETCH || 
       (LNO_Run_Prefetch == SOME_PREFETCH && !Is_Multi_BB (w))) &&
//      !simple_copy_loop && // bug 8560 disable this
      !single_small_trip_loop)
#endif
    Process_Refs (w);

  // now process nested inner loops
  for (INT i=0; i<_child.Elements(); i++) {
    _child.Bottom_nth(i)->Process_Loop ();
  }
}

/***********************************************************************
 *
 * Now that lists of references have been sorted into
 * their appropriate uniformly generated sets, process each
 * UGS into locality groups.
 * Only the *base* locality groups are built by this routine
 * i.e. assuming no loops are localized.
 * Other locality groups are built on-demand during the volume computation.
 *
 ***********************************************************************/
void PF_LOOPNODE::Build_Base_LGs () {
  
  // Do children (nested loops) first
  INT i;
  for (i=0; i<_child.Elements(); i++) {
    _child.Bottom_nth(i)->Build_Base_LGs ();
  }

  for (i=0; i<_bases.Elements(); i++) {
    _bases.Bottom_nth (i)->Build_Base_LGs ();
  }
}

/***********************************************************************
 *
 * Find the volume (in bytes) of this loop for a loop
 * "depth" levels from outermost loop in the loop nest.
 * Outermost loop is depth 0, and so on.
 *
 ***********************************************************************/
PF_VOLUME PF_LOOPNODE::Volume_For_Outer (mINT16 depth) {
  // since this is for outer, start off with the manual volume.
  PF_VOLUME myvol (_manual_volume, _manual_volume);
  INT i;

  Is_True ((depth>=0), ("Volume_For_Outer: depth is negative (%d)\n", depth));
  PF_PRINT(fprintf (TFile, "Vol-FO[0x%p] depth (%d)\n",
                    this, depth));

  for (i=0; i<_child.Elements(); i++) {
    myvol += _child.Bottom_nth(i)->Volume_For_Outer(depth);
    PF_PRINT(fprintf (TFile, "      [0x%p]: nest (%d)", this, i);
             myvol.Print (TFile));
    if (!myvol.Localized()) {
      // no longer localized - abort and return
      PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this));
      return myvol;
    }
  }

  for (i=0; i<_bases.Elements(); i++) {
    myvol += _bases.Bottom_nth(i)->Volume (depth);
    PF_PRINT(
      fprintf (TFile, "      [0x%p]: nest (%d)", this, i);
      myvol.Print (TFile);
    );
    if (!myvol.Localized()) {
      // no longer localized - abort and return
      PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this);)
      return myvol;
    }
  }
  return myvol;
}

/***********************************************************************
 *
 * Return confidence based on the given loop-info.
 *
 ***********************************************************************/
extern mINT16 Loop_Confidence (DO_LOOP_INFO* dli) {
  mINT16 conf;
  if (!dli->Num_Iterations_Symbolic)
    conf = 3;
  else 
    if (dli->Est_Max_Iterations_Index >= 0 
        // &&
        // dli->Est_Max_Iterations_Index <= 2*dli->Est_Num_Iterations
        )
      conf = 2;
    else 
      conf = 1;
  return conf;
}

/**************************************************************************
 *
 * Compute volume in a single iteration of a loop.
 * Stop when volume is no longer localized.
 * Return the total volume in all iters of this loop.
 *
 **************************************************************************/
PF_VOLUME PF_LOOPNODE::Volume () {
  INT i;

  _single_iter *= 0;    // initialize to 0

  _volume_confidence = Loop_Confidence(Get_LoopInfo());

  PF_PRINT(fprintf (TFile, "Volume[0x%p]\n", this);)

  // First find volume in one iter to see if this loop is localized.
  // Do the nested loops first
  for (i=0; i<_child.Elements(); i++) {
    _single_iter += _child.Bottom_nth (i)->Volume ();


    DO_LOOP_INFO* dli = _child.Bottom_nth(i)->Get_LoopInfo ();
    _volume_confidence = minof (_volume_confidence, Loop_Confidence(dli));

    PF_PRINT(
      fprintf (TFile, "      [0x%p]: nest (%d)", this, i);
      _single_iter.Print (TFile);
    );
  }

  // Note that this test is done after processing all the children loops.
  // This is necessary so that if the inner loops have any reuse within,
  // we do detect that, even if this outer loop is not localized.
  if (!_single_iter.Localized()) {
    // no longer localized.
    // store the fact, and just return the volume so far -- 
    // since it's larger than 2nd-level cache, it needn't be accurate
    // don't bother setting _total_iter -- it shouldn't be used
    // if _single_iter is not localized
    PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this));
    return _single_iter;
  }

  PF_PRINT(fprintf (TFile, "      [0x%p]: now count local refs\n", this));

  // If we reached here, volume is still within 2nd-level cache.
  // Do the immediate references within this loop next.
  // Note: we are still computing single-iter volume, so
  // this call to Volume should literally only calculate the number
  // of locality groups.
  // Since we just want their volume in a single-iter (i.e. not in any loop)
  // we must pass _depth+1.
  for (i=0; i<_bases.Elements(); i++) {
    _single_iter += _bases.Bottom_nth (i)->Volume (_depth+1);
    PF_PRINT(fprintf (TFile, "      [0x%p]: ", this);
             _single_iter.Print (TFile));
    // This check is done within the loop, since these references get
    // no locality whatsoever if this loop is not localized.
    if (!_single_iter.Localized()) {
      // No longer localized. Again, doesn't matter what we return.
      PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this));
      return (_single_iter);
    }
  }

  PF_PRINT(fprintf (TFile, "      [0x%p]: now compute total iter\n", this));

  // Now find the volume (in bytes) for all iterations of this loop.
  _total_iter *= 0; // initialize to 0
  // Add in the volume of manual prefetches to total iter,
  // with all the volume showing up in total-iter of innermost enclosing loop.
  // Assume they have no effect on single iter.
  _total_iter.vol_1L += _manual_volume;
  _total_iter.vol_2L += _manual_volume;

  // First call the nested loops to compute volume for appropriate depth
  for (i=0; i<_child.Elements(); i++) {
    _total_iter += _child.Bottom_nth (i)->Volume_For_Outer (_depth);
    PF_PRINT(fprintf (TFile, "      [0x%p]: ", this);
             _total_iter.Print (TFile));
    if (!_total_iter.Localized()) {
      // no longer localized.
      // just return _total_iter -- since it's larger
      // than 2nd-level cache, it needn't be accurate
      PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this));
      return _total_iter;
    }
  }

  PF_PRINT(fprintf (TFile, "      [0x%p]: now count local refs\n", this));

  // Compute the volume of the immediate references for all iters of this loop
  for (i=0; i<_bases.Elements(); i++) {
    _total_iter += _bases.Bottom_nth (i)->Volume (_depth);
    PF_PRINT(fprintf (TFile, "      [0x%p]: ", this);
             _total_iter.Print (TFile));
    if (!_total_iter.Localized()) {
      // no longer localized. again, doesn't matter what we return
      PF_PRINT(fprintf (TFile, "      [0x%p]: not localized, return\n", this));
      return _total_iter;
    }
  }
  return _total_iter;
}


/***********************************************************************
 *
 * Called with a split_vec, which may be NULL since that is stored
 * only for the outermost localized loop.
 * Call Gen_Prefetch on each of the child loops, and on each base array.
 *
 ***********************************************************************/
void PF_LOOPNODE::Gen_Prefetch (PF_SPLIT_VECTOR* split_vec) {
  INT i;

  // If incoming split_vec is NULL, then use my _split_vec.
  // If I am outermost localized loop then _split_vec will be
  // non-NULL.

  if (LNO_Analysis) {
    WN* index_wn = WN_index(_code);
    const char* var_name = ((ST_class(WN_st(index_wn)) != CLASS_PREG) ?
                      ST_name(WN_st(index_wn)) :
                      (WN_offset(index_wn) > Last_Dedicated_Preg_Offset ?
                       Preg_Name(WN_offset(index_wn)) : "DEDICATED PREG"));
    
    ls_print_indent; fprintf (LNO_Analysis, "(PREFETCH-LOOP \"%s\"\n",
                              var_name);
//                              ST_name(WN_st(WN_kid0(_code))));
    ls_print_indent; fprintf (LNO_Analysis,
                              "  (PREF-VOLUME (SINGLE %d %d) (TOTAL %d %d))\n",
                              _single_iter.vol_1L, _single_iter.vol_2L,
                              _total_iter.vol_1L, _total_iter.vol_2L);
    ls_print_indent; fprintf (LNO_Analysis, "  (PREF-SPLIT ");
  }
  VB_PRINT (vb_print_indent;
            printf ("Loop: \"%s\" depth (%d) (single %d%s %d%s) (total %d %d)\n",
                    ST_name(WN_st(WN_kid0(_code))),
                    _depth,
                    _single_iter.vol_1L, (_single_iter.Localized_1L()?"*":""),
                    _single_iter.vol_2L, (_single_iter.Localized_2L()?"*":""),
                    _total_iter.vol_1L, _total_iter.vol_2L));

  if (split_vec == NULL) split_vec = _split_vec;
  BOOL versions = FALSE;
  INT i_tmp=0;
  if (split_vec) {
    // determine if this loop is part of a versioned loop
    PF_LOOPNODE* loopnode = split_vec->Get_Loop ();
    while (loopnode->Get_Depth() > _depth) loopnode = loopnode->Get_Parent ();
    if (loopnode == this) {
      // this loop is in the versioned nest.
      // now, is there more than one copy of this loop?
      mINT16* vec = split_vec->Get_Vector ();
      for (i=0; i<= _depth; i++)
        if (vec[i] != 0) {
          versions = TRUE;
          break;
        }
      if (LNO_Analysis) fprintf (LNO_Analysis, "%d)\n", vec[_depth]);
      if (LNO_Tlog) i_tmp=vec[_depth];
    }
    else if (LNO_Analysis) fprintf (LNO_Analysis, "0)\n");
  }
  else if (LNO_Analysis) fprintf (LNO_Analysis, "0)\n");

  if (LNO_Tlog) {
    WN *loop=_code;
    char out_string[8];
    sprintf(out_string,"%d", i_tmp);
    Generate_Tlog("LNO","prefetching", Srcpos_To_Line(WN_Get_Linenum(loop)),
                ST_name(WN_st(WN_index(loop))), "", out_string, "PREF-SPLIT");
  }

  // prefetch immediate references in this loop
  if (LNO_Analysis) {
    ls_print_indent; fprintf (LNO_Analysis, "  (PREFETCHES\n");
    ls_num_indent += 4;
  }

  for (i=0; i<_bases.Elements(); i++) {
    if (versions) _bases.Bottom_nth(i)->Gen_Prefetch (split_vec);
    else          _bases.Bottom_nth(i)->Gen_Prefetch (NULL);
  }
  if (LNO_Analysis) {
    ls_num_indent -= 4;
    ls_print_indent; fprintf (LNO_Analysis, "  )\n");
  }

  // do the children loops
  VB_PRINT (if (_child.Elements()) {
    vb_print_indent;
    printf ("Inner loops (%d):\n", _child.Elements());
    vb_num_indent += 2;
  });
  if (LNO_Analysis) ls_num_indent += 2;
  for (i=0; i<_child.Elements(); i++)
      _child.Bottom_nth(i)->Gen_Prefetch (split_vec);
  VB_PRINT (if (_child.Elements()) vb_num_indent -= 2;);
  if (LNO_Analysis) {
    ls_num_indent -= 2;
    ls_print_indent; fprintf (LNO_Analysis, ")\n");
  }
}

/***********************************************************************
 *
 * Verify that all array references and all nodes that have a 
 * vertex in the dependence graph in body_orig point to
 * their matching instances in body_new.
 *
 ***********************************************************************/
BOOL Check_Version_Map (WN* body_orig, WN* body_new) {
  extern WN_MAP version_map;
  if (body_orig == NULL) {
    Is_True (body_new == NULL,
             ("Check version map: things didn't get copied right\n"));
    return TRUE;
  }

  OPCODE opcode = WN_opcode(body_orig);
  if ((OPCODE_operator(opcode) == OPR_ILOAD) &&
      (WN_operator(WN_kid0(body_orig)) == OPR_ARRAY))
    Is_True (WN_MAP_Get(version_map, WN_kid0(body_orig)) == WN_kid0(body_new),
             ("Check version map: error in array load\n"));
  if ((OPCODE_operator(opcode) == OPR_ISTORE) &&
      (WN_operator(WN_kid1(body_orig)) == OPR_ARRAY)) 
    Is_True (WN_MAP_Get(version_map, WN_kid1(body_orig)) == WN_kid1(body_new),
             ("Check version map: error in array store\n"));
  extern ARRAY_DIRECTED_GRAPH16 *pf_array_dep_graph;
  if (pf_array_dep_graph->Get_Vertex(body_orig))
    Is_True (WN_MAP_Get(version_map, body_orig) == body_new,
             ("Check version map: error in node-with-vertex\n"));
  
  if (opcode == OPC_BLOCK) {
    WN* kid_orig = WN_first (body_orig);
    WN* kid_new = WN_first (body_new);
    while (kid_orig) {
      Is_True (kid_new, ("check version map: kid_new is missing\n"));
      Check_Version_Map (kid_orig, kid_new);
      kid_orig = WN_next (kid_orig);
      kid_new = WN_next (kid_new);
    }
  }
  else {
    INT kidno;
    for (kidno=0; kidno<WN_kid_count(body_orig); kidno++) 
      Check_Version_Map (WN_kid(body_orig, kidno), WN_kid(body_new, kidno));
  }
  return TRUE;
}

/***********************************************************************
 *
 * Collect all the labels and gotos in the tree newbody into the stacks
 * gotos and labels.
 *
 ***********************************************************************/
static void Collect_Labels_Gotos (WN* newbody, WN_DA* gotos, WN_DA* labels) {
  if (newbody == NULL) return;
  OPCODE op = WN_opcode(newbody);
  if (OPCODE_has_label(op)) {
    if (op == OPC_LABEL) labels->Push (newbody);
    else gotos->Push (newbody);
  }
  else {
    if (WN_opcode(newbody) == OPC_BLOCK) {
      WN* kid = WN_first (newbody);
      while (kid) {
        Collect_Labels_Gotos (kid, gotos, labels);
        kid = WN_next (kid);
      }
      return;
    }
    else if ((OPCODE_is_stmt(WN_opcode(newbody))) ||
             (OPCODE_is_scf (WN_opcode(newbody)))) {
      for (INT kidno=0; kidno<WN_kid_count(newbody); kidno++)
        Collect_Labels_Gotos (WN_kid (newbody,kidno), gotos, labels);
      return;
    }
    else return;
  }
}

/***********************************************************************
 *
 * Called on the newbody of a versioned loop if it has gotos.
 * This code renames all labels and gotos in some unique
 * fashion to avoid conflicts with the original body.
 *
 ***********************************************************************/
static void Rename_Labels_Gotos (WN* newbody) {
  WN_DA gotos(PF_mpool);
  WN_DA labels(PF_mpool);
  STACK<INT32> old_label(PF_mpool);
    
  Collect_Labels_Gotos (newbody, &gotos, &labels);

  INT i;
  for (i=0; i<labels.Elements (); i++) {
    // Create a new label
    // Store the label for labels(i) in old_label(i),
    // so that next pass can find the new label easily in labels(i)
    WN* label_wn = labels.Bottom_nth(i);
#ifdef _NEW_SYMTAB
    LABEL_IDX new_label;
    (void) New_LABEL (CURRENT_SYMTAB, new_label);
    old_label.Push (WN_label_number(label_wn));
    WN_label_number(label_wn) = new_label;
#else
    INT32 new_label = ++SYMTAB_last_label(Current_Symtab);
    old_label.Push (WN_label_number(label_wn));
    WN_label_number(label_wn) = new_label;
    WN_st(label_wn) = NULL;     // not a user created label
#endif
  }
  Is_True (old_label.Elements() == labels.Elements(),
           ("Mismatch while walking labels"));
  for (i=0; i<gotos.Elements(); i++) {
    WN* goto_wn = gotos.Bottom_nth(i);
    INT32 cur_label = WN_label_number(goto_wn);
    INT j;
    for (j=0; j<old_label.Elements(); j++)
      if (old_label.Bottom_nth(j) == cur_label) break;
    // if we didn't find the label, jump must be outside the loop,
    // so don't touch it.
    if (j == old_label.Elements()) continue;
    // Else change to the new label
    WN_label_number(goto_wn) = WN_label_number(labels.Bottom_nth(j));
#ifndef _NEW_SYMTAB
    WN_st(goto_wn) = NULL;
#endif
  }
}

/***********************************************************************
 *
 * Called with a split_vec chosen by the outermost localized loop
 * through a call to Find_Split_Vector.
 * This function performs the actual split based on the split vector.
 * The different versions of each reference can be located through the
 * version_map.
 *
 ***********************************************************************/
void PF_LOOPNODE::Split_Loops (PF_SPLIT_VECTOR *split_vec) {
  INT i;

  Is_True (!split_vec->Empty(), ("Split_Loops: Empty split vector\n"));
  // Do I need to split? See if this loop contains the winning pfvector
  PF_LOOPNODE* loopnode = split_vec->Get_Loop ();
  Is_True (loopnode->Get_Depth() >= _depth,
           ("Split_Loops: splitting an inner loop, split_vec is outside\n"));
  while (loopnode->Get_Depth() > _depth) loopnode = loopnode->Get_Parent ();
  if (loopnode == this) {
    // I contain the winning split vector, so I might need to split
    mINT16* vec = split_vec->Get_Vector ();
    DO_LOOP_INFO* dli = loopnode->Get_LoopInfo();

    // But this is a little more involved --- if we disable a split
    // then later we go searching for an instance that isn't there.
    // 

    if ((vec[_depth] > 1) && 
        // Check that if the estimate is non-symbolic
        // (i.e. high confidence) then the estimate must be larger than
        // the split factor
        // Any changes in this logic must be replicated in 
        // PF_LG::Get_Bit_Vec
        (dli->Num_Iterations_Symbolic ||
         (vec[_depth] < Get_Good_Num_Iters(dli)))) {

      _split_num = vec[_depth]; // store the split
      extern WN_MAP version_map;
      PF_PRINT(fprintf (TFile, "Split loop [0x%p] with stride %d\n",
                        this, vec[_depth]));
      // split this loop...
      WN* body = WN_do_body(_code);
      WN* newbody = LWN_Copy_Tree (body,TRUE,LNO_Info_Map,TRUE,version_map);
      LWN_Copy_Frequency_Tree(newbody, WN_step(_code));
      Is_True (Check_Version_Map (body, newbody),
               ("Check_Version_Map failed"));
      if (Debug_Prefetch) Check_Version_Map (body, newbody);
      DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
      if (dli->Has_Gotos) Rename_Labels_Gotos (newbody);

      // update DU information for versioned body
      WN* wn_array[2];
      wn_array[0] = body;
      wn_array[1] = newbody;
      Unrolled_DU_Update (wn_array, 2, dli->Depth);

      // construct condition
      WN* expr;
      TYPE_ID index_type = WN_desc(WN_step(_code));
      WN* index_var = LWN_CreateLdid 
        (OPCODE_make_op(OPR_LDID, Promote_Type(index_type), index_type),
         WN_step(_code));
         
      switch (vec[_depth]) {
      case 2:
      case 4:
      case 8:
      case 16:
      case 32: {
        // power of two, simpler test:  ((i & 0x111) == 0x100)
        WN* intconst = WN_CreateIntconst (
          OPCODE_make_op(OPR_INTCONST, Promote_Type(index_type), MTYPE_V),
          (INT64) (vec[_depth]+vec[_depth]-1));
        expr = LWN_CreateExp2 (
          OPCODE_make_op(OPR_BAND, Promote_Type(index_type), MTYPE_V),
          index_var,
          intconst);
        WN* intconst2 = WN_CreateIntconst (
          OPCODE_make_op(OPR_INTCONST, Promote_Type(index_type), MTYPE_V),
          (INT64) vec[_depth]);

        expr = LWN_CreateExp2 (
          OPCODE_make_op(OPR_EQ, Boolean_type, Promote_Type(index_type)),
          expr,
          intconst2);
	}
        break;
      default: {
        // not so simple test: ((i % s) == 0)
        WN* intconst = WN_CreateIntconst (
          OPCODE_make_op(OPR_INTCONST, Promote_Type(index_type), MTYPE_V),
          (INT64) vec[_depth]);
        expr = LWN_CreateExp2 (
          OPCODE_make_op(OPR_MOD, Promote_Type(index_type), MTYPE_V),
          index_var,
          intconst);
        WN* intconst0 = WN_CreateIntconst (
          OPCODE_make_op(OPR_INTCONST, Promote_Type(index_type), MTYPE_V),
          (INT64) 0);
        expr = LWN_CreateExp2 (
          OPCODE_make_op (OPR_EQ, Boolean_type, Promote_Type(index_type)),
          expr,
          intconst0);
        } 
        break;
      }
      
      LWN_Copy_Frequency_Tree(expr, WN_step(_code));
      WN* ifnode = LWN_CreateIf (expr, body, newbody);
      LWN_Copy_Frequency(ifnode, expr);
      LWN_Scale_Frequency_Tree(body, 1.0/vec[_depth]);
      LWN_Scale_Frequency_Tree(body, 1.0-1.0/vec[_depth]);

      WN* block = WN_CreateBlock ();
      LWN_Insert_Block_Before (block, NULL, ifnode);
      WN_do_body(_code) = block;
      LWN_Set_Parent(block, _code);

      // Update DU info for index_var
      Du_Mgr->Du_Add_Use (WN_step(_code), index_var); 
      Du_Mgr->Du_Add_Use (WN_start(_code), index_var);
      Du_Mgr->Ud_Add_Def (index_var, WN_step(_code));
      Du_Mgr->Ud_Add_Def (index_var, WN_start(_code));

      // set index var to point to enclosing loop
      // this has some semantics that i don't fully remember.
      DEF_LIST *def_list_copy = Du_Mgr->Ud_Get_Def(index_var);
      def_list_copy->Set_loop_stmt(_code);

      // Update access vectors for expr in the ifnode
      IF_INFO *ii=CXX_NEW (IF_INFO(&LNO_default_pool,TRUE,
                           Find_SCF_Inside(ifnode,OPC_REGION)!=NULL),
                           &LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map,ifnode,(void *)ii);
      DOLOOP_STACK* stack = CXX_NEW(DOLOOP_STACK(&LNO_local_pool),
                                    &LNO_local_pool);
      Build_Doloop_Stack(ifnode, stack);
      LNO_Build_If_Access(ifnode, stack);
      CXX_DELETE(stack, &LNO_local_pool);

      // Update the dependence graph
      extern ARRAY_DIRECTED_GRAPH16 *pf_array_dep_graph;
      PF_PRINT(fprintf (TFile, "dep-graph: Before updating\n");
               pf_array_dep_graph->Print (TFile));
      // Pass Depth+1 since dep-graph entries are 1-based, 
      // while loop depths are 0-based.
      pf_array_dep_graph->Versioned_Dependences_Update (body, newbody, 
                                                        dli->Depth+1, 
							version_map);
//      pf_array_dep_graph->Versioned_Dependences_Update(body,
//                                                       newbody,
// 							 Good_Do_Depth(_code),
//							 version_map);
      PF_PRINT(fprintf (TFile, "dep-graph: After updating\n");
               pf_array_dep_graph->Print (TFile));


      Is_True (LWN_Check_Parentize(_code),
               ("CheckParentize failed after loop split\n"));
    }
    // whether this loop was actually split or not, it is on the 
    // split path. therefore one of the children may need splitting.
    if (split_vec->Get_Loop()->Get_Depth() > _depth) {
      for (i=0; i<_child.Elements(); i++)
        _child.Bottom_nth(i)->Split_Loops (split_vec);
    }
    // otherwise all the loops that needed to be split have been split up.
  }
}

/***********************************************************************
 *
 * Find the best split vector, based on:
 *  - deepest vector requiring a split (most inner-most loop)
 *  - with the highest count
 *
 ***********************************************************************/
PF_SPLIT_VECTOR* PF_LOOPNODE::Find_Split_Vector () {
  PF_SPLIT_VECTOR* split_vec = NULL;
  INT i;
  for (i=0; i<_child.Elements(); i++) {
    PF_SPLIT_VECTOR* tmp = _child.Bottom_nth(i)->Find_Split_Vector ();
    if (tmp) {
      if (split_vec) split_vec->Update (tmp);
      else split_vec = tmp;
    }
  }
  if (!split_vec) {
    // if didn't find a deeper one, look in current base arrays etc
    for (i=0; i<_bases.Elements(); i++) {
      PF_SPLIT_VECTOR* tmp = _bases.Bottom_nth(i)->Find_Split_Vector();
      if (tmp) {
        if (split_vec) split_vec->Update (tmp);
        else split_vec = tmp;
      }
    }
  }
  return split_vec;
}

/***********************************************************************
 *
 * Given a do-loop WN*, return the while_wn if it is contained within 
 * a while-loop before a do-loop. NULL otherwise.
 *
 * (useful to determine if locality is actually temporal).
 *
 ***********************************************************************/
WN* While_Before_Do (WN* do_wn) {
  FmtAssert (do_wn && WN_opcode(do_wn) == OPC_DO_LOOP,
             ("While_Before_Do: Expected a DO loop"));
  do_wn = LWN_Get_Parent(do_wn);
  while (do_wn) {
    OPERATOR opr = WN_operator(do_wn);

    switch (opr) {
    case OPR_WHILE_DO:
    case OPR_DO_WHILE:
      return do_wn;
      
    case OPR_DO_LOOP:
      return NULL;
      
    default:
      do_wn = LWN_Get_Parent(do_wn);
      break;
    }
  }
  return NULL;
}

/***********************************************************************
 *
 * Called on a do-loop contained within a while-loop (with no intervening
 * do-loops), return the volume of data referenced in each iteration
 * of the while loop.
 * 
 * This determines whether the do-loop benefits from while-temporal
 * locality or not.
 *
 ***********************************************************************/
PF_VOLUME PF_LOOPNODE::Volume_Within_While (WN* while_wn) {

  Is_True (while_wn && (WN_opcode(while_wn) == OPC_WHILE_DO ||
                        WN_opcode(while_wn) == OPC_DO_WHILE),
           ("While_Is_Localized: expected a WHILE loop"));

  /* to determine the volume of the WHILE loop,
   * find all the do-loops immediately within the while
   * and add up their volume.
   * Note: These do-loops must be siblings of the current do-loop,
   * since this do-loop is immediately contained within the while
   * without any intervening do-loops.
   */
  PF_VOLUME while_volume;
  PF_LOOPNODE* parent_loop = Get_Parent();
  INT i;
  INT myidx = INT_MAX;

  // walk all the siblings
  for (i=0; i<parent_loop->_child.Elements(); i++) {

    PF_LOOPNODE* sibling_loop = parent_loop->_child.Bottom_nth(i);
    WN* sibling_wn = sibling_loop->Get_Code();

    if (sibling_loop == this) myidx = i;

    if (Is_Descendent(sibling_wn, while_wn)) {
      while_volume += sibling_loop->_total_iter;
      if (i > myidx) {
        // there are sibling loops in the while that haven't been
        // processed yet, so we just assume that the volume is infinity.
        // actually twice eff-cache size should be plenty.
        PF_VOLUME tmp (2*Cache.EffSize(1), (Cache.Levels() > 1 ?
                                            2*Cache.EffSize(2) :
                                            0));
        while_volume += tmp;
        break;
      }
    }
  }

  return while_volume;
}

/***********************************************************************
 *
 * Walk this loop nest, find the localized loops, and compute
 * the desired stride to split in each loop.
 * Do the splitting after all the votes have been taken, so that
 * a more global decision can be taken. Thus the splitting, although
 * done inside out, is initiated after all votes have been tallied.
 *
 * locloop is the incoming localized status (of the outer loops).
 *
 ***********************************************************************/
void PF_LOOPNODE::Find_Loc_Loops (PF_LOCLOOP locloop) {
  INT i;

  // If outer are localized, that is the final localized status, 
  // otherwise volume of this loop is examined.
  // The final localized status is stored locally in _locloop in PF_LOOPNODE.
  BOOL idosplit = locloop.Update (_depth, _single_iter, _volume_confidence);

  if (idosplit) {
    // this loops is an outermost localized loop at some cache level
    WN* while_wn = While_Before_Do(Get_Code());

    if (while_wn) {

      PF_VOLUME while_volume = Volume_Within_While (while_wn);

      if (locloop.Loop_1L() == _depth) {
        // This loop is localized for L1 cache.
        // If total_iter is also localized and loop is contained in a while
        // then we are getting temporal locality.
        if (while_volume.Localized_1L()) {
          locloop.Set_While_Temporal_1L();
        }
      }
      if ((Cache.Levels() > 1) && (locloop.Loop_2L() == _depth)) {
        // localized for L2 cache; check for while-temporal
        if (while_volume.Localized_2L()) {
          locloop.Set_While_Temporal_2L();
        }
      }
    }
  }
  
  _locloop = locloop;

  // so now locloop is either -1 (not localized) or depth (if localized).
  // For all loops within, process strides
  for (i=0; i<_child.Elements(); i++)
    _child.Bottom_nth(i)->Find_Loc_Loops (locloop);

  // Process local references only if localized
  if (locloop.Localized())
    for (i=0; i<_bases.Elements(); i++)
      _bases.Bottom_nth(i)->Find_Loc_Space (locloop);
  
  // Now all references within this loop have been processed
  // If I am the outermost localized, then I can start the splits.
  if (idosplit) {
    // First determine the split vector
    PF_SPLIT_VECTOR* split_vec = Find_Split_Vector ();
    PF_PRINT(fprintf (TFile, "After Find_Split_Vector: ");
             if (split_vec) split_vec->Print (TFile);
             else fprintf (TFile, "split_vec is NULL\n"));
    if ((split_vec) && (!split_vec->Empty())) {
      _split_vec = split_vec;
      Split_Loops (split_vec);
    }
  }
}

void PF_LOOPNODE::Print (FILE *fp) {
  INT i;
  fprintf (fp, "Do loop: node 0x%p\n    depth %d\n    parent 0x%p\n    code   0x%p\n    refs   0x%p\n",
           this, _depth, _parent, _code, &_bases);
  fprintf (fp, "  single iter: ");    _single_iter.Print (fp);
  fprintf (fp, "  total  iter: ");    _total_iter.Print (fp);
  if (_bases.Elements()) {
    fprintf (fp, "  Printing the references in each base array (of %d)\n",
             _bases.Elements());
    for (i=0; i<_bases.Elements(); i++) {
      fprintf (fp, "\n  Base array %d -> ", i);
      _bases.Bottom_nth(i)->Print (fp);
    }
  }
  else fprintf (fp, "  No references, no base arrays\n");
  fprintf (fp, "  %d children: ", _child.Elements());
  for (i=0; i<_child.Elements(); i++) {
    fprintf (fp, "    0x%p ", _child.Bottom_nth (i));
  }
  if (_child.Elements()) {
    fprintf (fp, "\n  Now printing the children\n\n");
    for (i=0; i<_child.Elements(); i++) {
      fprintf (fp, "[");
      for (INT j=0; j<(_depth+1); j++) fprintf (fp, " *");
      fprintf (fp, " %3d ] ", i);
      _child.Bottom_nth (i)->Print (fp);
    }
  }
  else fprintf (fp, "\n");
}

void PF_LOOPNODE::Print_Structure () {
  INT i;

  vb_print_indent;
  printf ("Loop: \"%s\" depth (%d)\n", 
          ST_name(WN_st(WN_kid0(_code))), _depth);
  if (_bases.Elements ()) {
    vb_print_indent;
    printf ("Base arrays (%d): ", _bases.Elements ());
    for (i=0; i<_bases.Elements(); i++) {
      _bases.Bottom_nth(i)->Get_Symbol()->Print (stdout);
      if (i == (_bases.Elements()-1)) printf (".\n");
      else printf (", ");
    }
  }
  if (_child.Elements()) {
    vb_print_indent;
    printf ("Inner loops (%d):\n", _child.Elements());
    vb_num_indent += 2;
    for (i=0; i<_child.Elements(); i++) {
/*
      printf ("[");
      for (INT j=0; j<(_depth+1); j++) printf (" *");
      printf (" %3d ]\n", i);
      */
      _child.Bottom_nth (i)->Print_Structure ();
    }
    vb_num_indent -= 2;
  }
}

void PF_LOOPNODE::Print_Volume () {
  INT i;
  vb_print_indent;
  printf ("Loop: \"%s\" depth (%d)\n", 
          ST_name(WN_st(WN_kid0(_code))), _depth);
  vb_print_indent;
  printf ("  single_iter: "); _single_iter.Print (stdout);
  vb_print_indent;
  printf ("  total_iter:  "); _total_iter.Print (stdout);
  if (_child.Elements()) {
    vb_print_indent;
    printf ("Inner loops (%d):\n", _child.Elements());
    vb_num_indent += 2;
    for (i=0; i<_child.Elements(); i++) {
      _child.Bottom_nth (i)->Print_Volume ();
    }
    vb_num_indent -= 2;
  }
}

void PF_LOOPNODE::Print_Splits () {
  INT i;
  vb_print_indent;
  printf ("Loop: depth (%d), index ", _depth); dump_wn (WN_index(_code));
  if ((_split_vec) && (!_split_vec->Empty()) && vb_print_split) {
    vb_print_indent;
    _split_vec->Print (stdout);
  }
  if ((_split_num > 1) && (vb_print_split)) {
    vb_print_indent;
    printf (">> split: %d\n", _split_num);
  }
  if (_child.Elements()) {
    if ((_split_num > 1) && (vb_print_split)) {
      vb_print_indent;
      printf ("Inner loops (%d), prefetch version (stride = %d)\n",
              _child.Elements(), _split_num);
      vb_num_indent += 2;
      for (i=0; i<_child.Elements(); i++) {
        _child.Bottom_nth (i)->Print_Splits ();
      }
      vb_num_indent -= 2;

      vb_print_indent;
      printf ("Inner loops (%d), non-pref version\n", _child.Elements());
      vb_print_split = FALSE;
      vb_num_indent += 2;
      for (i=0; i<_child.Elements(); i++) {
        _child.Bottom_nth (i)->Print_Splits ();
      }
      vb_num_indent -= 2;
      vb_print_split = TRUE;
    }
    else {
      vb_print_indent;
      printf ("Inner loops (%d):\n", _child.Elements());
      vb_num_indent += 2;
      for (i=0; i<_child.Elements(); i++) {
        _child.Bottom_nth (i)->Print_Splits ();
      }
      vb_num_indent -= 2;
    }
  }
}
