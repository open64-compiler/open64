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


// c-or-c++

/*
 *
 * Finding the stride and the spatial locality vector:
 * Two possible situations:
 *  For volume computation - getting an exact answer in this case is tricky.
 *   Currently we do the following:
 *      - see if there is spatial locality within the loop subnest (depth)
 *      - if so, we look at the smallest coefficient in the stride-one dim
 *        of a loop within subnest. An improvement would be to actually
 *        find the GCD of the coefficients of loops within subnest. However, 
 *        that would not be perfectly precise either. 
 *        If Michael can solve this problem, great, otherwise a TODO item is
 *        to come up with an algorithm that fast-paths some common cases,
 *        and does a brute-force search otherwise (see next item).
 * For determining prefetching in the localized loops
 *  As described later, we prefetch only if there is no temporal locality
 *  within the localized loops. We try to catch a common case as follows:
 *      - Does the stride-one dimension have a loop var that doesn't occur
 *        in any other dimension? If so, there must be only one such variable,
 *        otherwise there would be temporal locality.
 *        If there is such a variable, then the coefficient of that variable
 *        gives the stride, and the basis vector in that loop-var gives a
 *        vector.
 *        (this will miss a possible common case of a(i, i+j) - fortran).
 *      - If there is no such variable, resort to the following hack.
 *        Run the loops in the localized loop nest during compilation, 
 *        generate the values for each dimension, and deduce a stride.
 *
 *
 * Volume computation (for a locality group in a loop subnest given by depth):
 *  - First find the number of distinct elements being referenced
 *    This is done by finding the temporal locality (if any)
 *    which is basically (KerH * Lsn) - * is intersection, Lsn is loop subnest
 *    Call this sKerH. 
 *    For each basis vector bv in sKerH, contribution to vol is
 *      - if single element in bv, 1
 *      - if multiple elements in bv, 
 *        vol = Sum (P/ck * Nk) - P,   where P = Prod (ck)
 *             ck!=0                             ck!=0
 *        Heuristics in above equation: 
 *          - if abs(ck) > 20, no locality
 *          - if (abs(ck) > Nk, no locality
 *    Multiply out the contributions of each basis vector in sKerH
 *  - For all loops in subnest that have no locality, simply multiply by Nk
 *  - Next compensate for spatial reuse, if any
 *    Find sKerHs = KerHs * Lsn. If sKerHs > sKerH, then spatial locality
 *    Find the stride one loop -- 
 *      - loop within loop-subnest  
 *      - smallest coefficient in stride-one dimension
 *    Find the stride of the stride one loop
 *    Using that stride, convert the number of elements from above into
 *     number of cache lines for each level of the cache.
 *  - Convert number of cache lines into bytes
 *  - Multiply by the number of effective lines within each locality group
 *
 *
 * Having determined the volume of each loop, identify the localized loops 
 * for each level of the cache hierarchy:
 *  loop i_f s.t. volume-in-single-iter(i_f) < Cache.EffSize_1L(), and
 *  loop i_s s.t. volume-in-single-iter(i_s) < Cache.EffSize_2L().
 *
 *
 * Given the localized loops, determine what to prefetch:
 *  - for all references that occur outside localized loops, always prefetch
 *  - for each reference within localized loops
 *      - find sKerH (= KerH * Lsn) - subset of KerH that is localized
 *      - if sKerH is non-empty, i.e. has temporal locality along *any* bv, 
 *        don't prefetch. Rationale being that there is high cache reuse
 *        due to temporal locality.
 *        A refinement would be to count the tripcount of temporal reuse,
 *        and if that is small then turn on prefetching, perhaps incurring
 *        some unnecessary prefetches.
 *      - if dim(sKerH) < dim(sKerHs) then there is spatial locality.
 *        Furthermore, we come here only if dim(sHerH) == 0 (no temporal
 *        locality)
 *        so dim(sKerH) == 1.
 *        - Find the stride and the spatial locality vector (see above)
 *        - Based on the spatial locality vector, determine the desired stride
 *          on each of the loops in the localized nest
 *      - else prefetch each instance of that reference
 *  - having done this for each reference, decide the actual stride for each
 *    loop
 *
 */

#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#define CACHE_LINE_SIZE 128

#include "defs.h"
#include "config_asm.h"         // Temp_Symbol_Prefix
#include "prefetch.h"
#include "access_vector.h"
#include "pf_ref.h"
#include "pf_loop.h"
#include "wn_map.h"
#include "stab.h"
#include "lnopt_main.h"
#include "pf_cache.h"
#include "lwn_util.h"
#include "lnoutils.h"
#include "pf_cg.h"
#include "whirl2src.h"
#include "dep.h"
#include "tlog.h"
#include "alloca.h"
#include "targ_sim.h"

#include "w2c_weak.h"
#include "w2f_weak.h"
#include "ir_reader.h"

#include "opt_du.h"
#include "wn_tree_util.h"               // for tree iterators

#define INT_INFINITY 9999
#define absof(x) (((x)>0) ? (x) : (0-(x)))
#define maxof(x, y) (((x)>(y)) ? (x) : (y))
#define minof(x, y) (((x)<(y)) ? (x) : (y))
#define minmaxof(mn, mx, item) (((item) < (min)) ? ((min) = (item)) : (((item) > (max)) ? ((max) = (item)) : 0))

#ifdef KEY
#define ABS(a) ((a<0)?-(a):(a))
#endif

extern WN_MAP LNO_Info_Map;
extern void LWN_Parentize_One_Level(const WN* wn);

inline mINT16 PF_LG::Get_Dim ()     {
  return _myugs->Get_BA()->Get_Dim ();
}
inline mINT16 PF_LG::Get_Depth ()   {
  return _myugs->Get_BA()->Get_Loop()->Get_Depth();
}
inline LU_FMAT* PF_LG::Get_Hslu ()   {
  return _myugs->Get_Hslu ();
}
inline VECTOR_SPACE<FRAC>* PF_LG::Get_KerHs ()   {
  return _myugs->Get_KerHs ();
}
inline VECTOR_SPACE<FRAC>* PF_LG::Get_KerH  ()   {
  return _myugs->Get_KerH ();
}
inline PF_LOOPNODE* PF_LG::Get_Loop () {
  return _myugs->Get_BA()->Get_Loop ();
}
inline WN* PF_LG::Get_Ref (INT num) {
  return _myugs->Get_Ref (num);
}
// inline mINT16* PF_LG::Get_Stride () { return _myugs->Get_Stride (); }
inline mINT16  PF_LG::Get_Stride_One_Loop () {
  return _myugs->Get_Stride_One_Loop ();
}

inline mINT16 PF_LG::Stride_Forward () {
  return _myugs->Stride_Forward ();
}

inline mINT16  PF_LG::Get_Stride_One_Size () {
  return _myugs->Get_Stride_One_Size ();
}

inline mINT32  PF_LG::Get_Stride_In_Enclosing_Loop () {
  return _myugs->Get_Stride_In_Enclosing_Loop ();
}

#if defined(TARG_IA64)
inline BOOL  PF_LG::Get_Stride_Accurate() {
  return _myugs->Get_Stride_Accurate();
}
#endif

inline PF_LOOPNODE* PF_UGS::Get_Loop ()  {
  return _myba->Get_Loop ();
}
inline mINT16 PF_UGS::Get_Depth ()       {
  return _myba->Get_Loop()->Get_Depth ();
}
inline mINT16 PF_BASE_ARRAY::Get_Depth () {
  return _myloopnode->Get_Depth();
}

extern INT64 Get_Good_Num_Iters (DO_LOOP_INFO *dli) {
  INT64 real_numiters = dli->Est_Num_Iterations;
  if (dli->Num_Iterations_Symbolic &&
      dli->Est_Max_Iterations_Index != -1) {
    real_numiters = dli->Est_Max_Iterations_Index;
  }
  return real_numiters;
}

static VECTOR_SPACE<FRAC>* global_lvs[LNO_MAX_DO_LOOP_DEPTH+1][LNO_MAX_DO_LOOP_DEPTH+1];

/***********************************************************************
 *
 * Initialize the global variable global_lvs, which is a 2-D array
 * of vector spaces. Each vector space consists of unit basis vectors.
 * The two dimensions of global_lvs help choose the size of the vector-space,
 * and the depth at which it is localized.
 *
 * Called before any prefetching is performed.
 * Initialize the lvs to be NULL initially, and allocate them on demand.
 *
 ***********************************************************************/
void Initialize_Lvs () {
  INT i, j;
  // to keep indexing of global_lvs simple, declare them of an extra size
  // global_lvs[i][j]:
  //    i == size of space
  //    j == the "number" of the outermost loop that is localized (its depth+1)

  for (i=0; i<LNO_MAX_DO_LOOP_DEPTH+1; i++)
    for (j=0; j<LNO_MAX_DO_LOOP_DEPTH+1; j++)
      global_lvs[i][j] = NULL;

}

/***********************************************************************
 *
 * Allocate and initialize a localized-vector-space
 * of size "i" localized at depth "j".
 *
 ***********************************************************************/
void Allocate_Lvs (INT i, INT j) {
  static FRAC tmp[LNO_MAX_DO_LOOP_DEPTH];
  Is_True (global_lvs[i][j] == NULL,
           ("Allocate_Lvs called twice on the same vector space (%d, %d)\n",
            i, j));
  global_lvs[i][j] = CXX_NEW (VECTOR_SPACE<FRAC>(i,PF_mpool,FALSE),
                              PF_mpool);
  // This one has "i-j+1" vectors, insert them
  for (INT k=j; k<=i; k++) {
    // create the vector of size "i" with a 1 in the "k-1th" place
    for (INT m=0; m<i; m++)
      if (m == (k-1)) tmp[m] = 1; else tmp[m] = 0;
    global_lvs[i][j]->Insert (tmp);
  }
}

/***********************************************************************
 *
 * Deallocate the storage allocated for global_lvs.
 * Called at the end of prefetching.
 *
 ***********************************************************************/
void Cleanup_Lvs () {
  for (INT i=1; i<=LNO_MAX_DO_LOOP_DEPTH; i++)
    for (INT j=1; j<=i; j++) {
      if (global_lvs[i][j])
        CXX_DELETE (global_lvs[i][j], PF_mpool);
    }
}

PF_LG::PF_LG (PF_LG* lg) : _refvecs(PF_mpool) {
  INT dim = lg->Get_Dim();
  INT i;

  for (i=0; i<lg->_refvecs.Elements(); i++) {
    _refvecs.Push (CXX_NEW (PF_REFVEC(lg->_refvecs.Bottom_nth(i)), PF_mpool));
  }
  _depth = lg->_depth - 1;
  _myugs = lg->_myugs;
  _leading_ref = lg->_leading_ref;
  _c = CXX_NEW_ARRAY (INT64, dim, PF_mpool);
  for (i=0; i<dim; i++)
    _c[i] = lg->_c[i];
  for (i=0; i<LNO_MAX_DO_LOOP_DEPTH; i++) {
    _min_iter[i] = lg->_min_iter[i];
    _max_iter[i] = lg->_max_iter[i];
  }
  _min_dist = lg->_min_dist;
  _max_dist = lg->_max_dist;
  
  _numlines_1L = lg->_numlines_1L;
  _numlines_2L = lg->_numlines_2L;
}

/***********************************************************************
 *
 * Given a whirl node, print out its contents in the given FILE*
 * within double-quotes.
 *
 ***********************************************************************/
extern void Listing_Emit_WN (FILE* fp, WN* wn) {
  fprintf (fp, " \"");
  extern WN* pf_func_nd;

  if (WN_operator(wn) != OPR_ISTORE) {
    Whirl2Src_Emit (fp, wn);
    fprintf (fp, "\" ");
    return;
  }

  // it's an ISTORE, more painful
  switch (PU_src_lang(Get_Current_PU())) {
  case PU_C_LANG:
  case PU_CXX_LANG:
  {
    W2C_Push_PU (pf_func_nd, wn);
    // lhs, so extract that
    char buf[100];
    W2C_Translate_Istore_Lhs (buf, 100, WN_kid1(wn), WN_offset(wn),
                              WN_ty(wn), WN_desc(wn));
    fprintf (fp, "%s", buf);
    W2C_Pop_PU();
    break;
  }
  case PU_F90_LANG:
  case PU_F77_LANG:
  {
    W2F_Push_PU(pf_func_nd, wn);
    // lhs, so extract that
    char buf[100];
    W2F_Translate_Istore_Lhs (buf, 100, WN_kid1(wn), WN_offset(wn),
                              WN_ty(wn), WN_desc(wn));
    fprintf (fp, "%s", buf);
    W2F_Pop_PU();
    break;
  }
  default:
    fprintf (fp, "var-unknown-src-lang");
    break;
  }
  fprintf (fp, "\" ");
  return;
} /* Listing_Emit_WN */

/***********************************************************************
 *
 * Called with reference, bit position corresponding to the reference,
 * depth of the locality group in the loop nest, and a pointer to the UGS.
 *
 ***********************************************************************/
PF_LG::PF_LG (WN* ref,
              mINT16 bitpos,
              mINT16 depth,
              PF_UGS* myugs) : _refvecs(PF_mpool) {

  // don't add anything to list of refvec, since just one reference so far
  _depth = depth;
  _myugs = myugs;
  _leading_ref = bitpos;
  INT i;
  for (i=0; i<LNO_MAX_DO_LOOP_DEPTH; i++) {
    _max_iter[i] = _min_iter[i] = 0;
  }
  _min_dist = _max_dist = 0;

  ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
  _c = CXX_NEW_ARRAY (INT64, aa->Num_Vec (), PF_mpool);
  for (i=0; i<aa->Num_Vec(); i++) _c[i] = aa->Dim(i)->Const_Offset;
  _numlines_1L = _numlines_2L = 0;
}

PF_LG::~PF_LG () {
  while (_refvecs.Elements()) CXX_DELETE (_refvecs.Pop (), PF_mpool);
  CXX_DELETE_ARRAY (_c, PF_mpool);
}

/***********************************************************************
 *
 * Check that all the references in this locality group are distinct.
 * Used for debugging.
 *
 ***********************************************************************/
BOOL PF_LG::Check () {
  // make sure all references are distinct
  INT num = _refvecs.Elements();
  INT i;
  for (i=0; i<num; i++)
    Is_True (_leading_ref != _refvecs.Bottom_nth(i)->Refnum(),
             ("oops -- duplicate in LG, with leading ref\n"));
  for (i=0; i<num; i++) {
    INT cur = _refvecs.Bottom_nth(i)->Refnum();
    for (INT j=i+1; j<num; j++)
      Is_True (cur != _refvecs.Bottom_nth(j)->Refnum(),
               ("oops -- duplicate in LG, between refs\n"));
  }
  return TRUE;
}

/***********************************************************************
 *
 * Check that the given reference does not occur in this locality group.
 * Used for debugging.
 *
 ***********************************************************************/
BOOL PF_LG::Check_Ref (mINT16 refnum) { 
  INT num = _refvecs.Elements();
  Is_True (refnum != _leading_ref, ("Check_Ref: ref same as leading ref\n"));
  for (INT i=0; i<num; i++)
    Is_True (refnum != _refvecs.Bottom_nth(i)->Refnum(),
             ("Check_Ref: ref (%d) is a duplicate\n", refnum));
  return TRUE;
}

/***********************************************************************
 *
 * Check to see if the given reference belongs to this locality group.
 * Return the dvector (array of FRACS) if yes, NULL if no.
 * Doesn't care about fitting within the same cache line.
 *
 ***********************************************************************/
FRAC* PF_LG::Ref_In_LG (WN* ref) {
  ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
  INT indxs = aa->Num_Vec ();
  mINT16 maxdepth = Get_Depth()+1;   // maxdepth is now the number of loops
  FRAC* dvec = CXX_NEW_ARRAY (FRAC, maxdepth, PF_mpool);
  LU_FMAT* Hslu = Get_Hslu();
  
  INT i;
  for (i=0; i<indxs; i++)
    if (aa->Dim(i)->Const_Offset != _c[i]) 
      break;

  if (i == indxs) {
    // all elements were identical, so definitely in this group
    // FRAC constructor initialized values to 0
    return dvec;
  }

  // construct a delta_c vector = C_leadref - C_newref
  FRAC *deltac = CXX_NEW_ARRAY (FRAC, indxs, PF_mpool);
  for (i=0; i<indxs; i++)
    deltac[i] = FRAC (_c[i] - aa->Dim(i)->Const_Offset);

  BOOL soln = Hslu->Particular_Solution (deltac, dvec);
  CXX_DELETE_ARRAY (deltac, PF_mpool);

  if (!soln) {
    // no solution exists
    CXX_DELETE_ARRAY (dvec, PF_mpool);
    return NULL;
  }

  // solution exists
  // TODO what about last dimension of array?
  PF_PRINT (
    fprintf (TFile, "Found a particular solution: "); 
    dvec->Print (TFile);
    fprintf (TFile, "\n");
  );
  // is distance vector contained within this loop?
  // _depth is the level of this loop, so ensure that elements
  // upto (_depth-1) are zero.
  for (i=0; i<_depth; i++) {
    if (dvec[i].N()) {
      // not in LG for this loop
      PF_PRINT(fprintf (TFile, "solution not within desired loop-nest\n"););
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return NULL;
    }
  }

  // ensure that subsequent elements are integers    
  for (i=_depth; i<maxdepth; i++) {
    if (dvec[i].D() != 1) {
      // this particular solution is not integral
      // just abort
      PF_PRINT(fprintf (TFile,
                        "solution is not integral\n"););
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return NULL;
    }
    // TODO: Heuristic Number Alert
    if (dvec[i].N() > 20) {
      // if reuse is across 20 iterations of any loop, forGet_ it
      PF_PRINT(fprintf (TFile, "reuse across more than 20 iters\n"););
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return NULL;
    }
  }

  // dvec is pure integers at this point, no fractions.
  // Check stepsize in each of the loops
  // to make sure that this trip is possible
  // go innermost loop outwards
  PF_LOOPNODE* loopnode = Get_Loop();
    
  for (i=(maxdepth-1); i>=_depth; i--) {
    DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();

    if ((dvec[i].N() == 0) ||
        ((dli->Step->Is_Const()) &&
         ((dli->Step->Const_Offset == 1) ||
          (dli->Step->Const_Offset == -1) ||
          ((dvec[i].N() % (dli->Step)->Const_Offset) == 0)))) {
      loopnode = loopnode->Get_Parent ();
    }
    else {
      // either not a constant step size,
      // or the step size doesn't divide the distance vector
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return NULL;
    }
  }

  // Phew! OK, so it's (almost certainly) in this locality group.
  // note: didn't do testing in the stride-one dimension, 
  // but that is now done in the caller (Add_Ref, or Add_Group).
  return dvec;
}

/***********************************************************************
 *
 * Given a reference and its dvec, compute its distance (in bytes) from 
 * the leading reference.
 *
 ***********************************************************************/
INT64 PF_LG::Distance_LR (WN* ref, FRAC* dvec) {
  // compute distance in bytes from leading reference
  ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
  INT dims = aa->Num_Vec();
  ACCESS_VECTOR* av = aa->Dim(dims-1);
  INT64 step = _c[dims-1]-av->Const_Offset;
  mINT16 maxdepth = Get_Depth()+1;

  for (INT i=_depth; i<maxdepth; i++) 
    step += dvec[i].N() * av->Loop_Coeff(i);

  // now examine the offset in the iload (if any),
  // and add it to the distance
  WN* my_iload_wn = LWN_Get_Parent(ref);
  WN* lr_iload_wn = LWN_Get_Parent(Get_Ref(Get_LeadingRef()));

  return ((INT64) ((ABS(WN_element_size (ref))*step)+
                   (WN_offset(my_iload_wn)-WN_offset(lr_iload_wn))));
}

/***********************************************************************
 *
 * Given references refvecnum1 and refvecnum2 in the refvecs list of an LG,
 * Return   0 if references are identical,
 *          1 if ref1 leads ref2
 *         -1 if ref2 leads ref1
 *
 ***********************************************************************/
INT PF_LG::LR_Compare (mINT16 refvecnum1, mINT16 refvecnum2) {
  INT i;
  mINT16 maxdepth = Get_Depth()+1;
  WN* ref1 = ((refvecnum1 == -1) ?
              Get_Ref (_leading_ref) :
              Get_Ref (_refvecs.Bottom_nth(refvecnum1)->Refnum ()));
  WN* ref2 = ((refvecnum2 == -1) ?
              Get_Ref (_leading_ref) : 
              Get_Ref (_refvecs.Bottom_nth(refvecnum2)->Refnum ()));

  Is_True ((refvecnum1 != -1) || (refvecnum2 != -1),
           ("LR_Compare: both refs are the same (leading ref)"));
  // Compute dvec s.t.   dvec = A_inv (C_1 - C_2)
  FRAC* dvec = CXX_NEW_ARRAY (FRAC, maxdepth+1, PF_mpool);

  if (refvecnum1 == -1) {
    // just copy dvec for ref2
    FRAC* orig_dvec = _refvecs.Bottom_nth(refvecnum2)->Dvec ();
    for (i=0; i<maxdepth; i++) dvec[i] = orig_dvec[i];
  }
  else if (refvecnum2 == -1) {
    // use dvec for ref1, but negate it
    FRAC* orig_dvec = _refvecs.Bottom_nth(refvecnum1)->Dvec ();
    for (i=0; i<maxdepth; i++) dvec[i] = ((FRAC) 0) - orig_dvec[i];
  }
  else {
    // neither is a leading reference
    FRAC* orig_dvec1 = _refvecs.Bottom_nth(refvecnum1)->Dvec ();
    FRAC* orig_dvec2 = _refvecs.Bottom_nth(refvecnum2)->Dvec ();
    for (i=0; i<maxdepth; i++) dvec[i] = orig_dvec2[i]-orig_dvec1[i];
  }

  // OK - now we have the references, and the distance vector

  ACCESS_ARRAY* aa1 = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref1);
  ACCESS_ARRAY* aa2 = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref2);
  INT dims = aa1->Num_Vec ();
  ACCESS_VECTOR* av1 = aa1->Dim(dims-1);
  ACCESS_VECTOR* av2 = aa2->Dim(dims-1);

  for (i=_depth; i<maxdepth; i++) 
    if (dvec[i].N() != 0) break;
  
  if (i == maxdepth) {
    // distance vector is all zeros. ref is almost same as leading ref,
    // except perhaps in stride-one dimension

    CXX_DELETE_ARRAY (dvec, PF_mpool);
    // identical references? 
    if (av1->Const_Offset == av2->Const_Offset) return 0;

    // differ in stride-one dimension
    INT stride_one_loop = Get_Stride_One_Loop ();
    // no stride one loop? -- lref don't matter. 
    if (stride_one_loop < 0) return 0;

    mINT16 stride_forward = Stride_Forward ();
    Is_True (stride_forward, ("stride one loop exists, but no direction\n"));
    if (((stride_forward > 0) && (av1->Const_Offset > av2->Const_Offset)) ||
        ((stride_forward < 0) && (av1->Const_Offset < av2->Const_Offset)))
      // ref1 leads ref2
      return 1;
    else return -1;
  }

  // dvec not all zeros
  INT nz = i;
  PF_LOOPNODE* loopnode = Get_Loop();
  for (i=maxdepth-1; i!=nz; i--) 
    loopnode = loopnode->Get_Parent ();
  DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();

  // is the step for this loop positive or negative?
  // if non-constant, assume positive
  BOOL steppos = TRUE;
  if ((dli->Step->Is_Const()) &&
      (dli->Step->Const_Offset < 0)) steppos = FALSE;

  if (((dvec[nz].N() < 0) && steppos) || ((dvec[nz].N() > 0) && !steppos)) {
    // ref2 is leading reference
    CXX_DELETE_ARRAY (dvec, PF_mpool);
    PF_PRINT (fprintf (TFile, "LR_Compare: ");
              aa2->Print (TFile);
              fprintf (TFile, "     leads  ");
              aa1->Print (TFile));
    return -1;
  }
  // ref1 is leading reference
  CXX_DELETE_ARRAY (dvec, PF_mpool);
  PF_PRINT (fprintf (TFile, "LR_Compare: ");
            aa1->Print (TFile);
            fprintf (TFile, "     leads  ");
            aa2->Print (TFile));
  return 1;
}

/***********************************************************************
 *
 * Given a reference "ref" and it's position in the list of references
 * in the UGS, this function either adds the reference to the locality 
 * group and returns TRUE, else it returns FALSE.
 *
 * This doesn't care about number of caches, their size, or linesize.
 * Just adds references if they can overlap along the stride-one dimension.
 * 
 ***********************************************************************/
BOOL PF_LG::Add_Ref (WN* ref, mINT16 bitpos) {
  Is_True (Check_Ref (bitpos), ("oops - error\n"));
  FRAC* dvec = Ref_In_LG (ref);

  if (!dvec) return FALSE;

  // reference is in this locality group, add it.
  //  - update leading reference
  //  - update min/max values relative to leading ref
  //  - update refvec
  /* scubadoo: perhaps insert this reference in "sorted"
   * order into the stack of refereces, _refvecs.
   */

  mINT16 maxdepth = Get_Depth()+1;
  INT i;
  INT64 distance = Distance_LR (ref, dvec); // distance from leading reference
  ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
  INT dims = aa->Num_Vec();
  ACCESS_VECTOR* av = aa->Dim(dims-1);

  /* First: update leading reference
   *  examine the dvec we got, which is of the current leading reference
   *  relative to the new reference
   * The possible cases are:
   *    - identical to incoming reference
   *    - same dvec, but differ in constant along stride-one dimension
   *    - different dvec
   */

  for (i=_depth; i<maxdepth; i++) 
    if (dvec[i].N() != 0) break;

  if (i == maxdepth) {
    // the distance vector is all zeros - this ref same as leading ref
    if (aa->Dim(dims-1)->Const_Offset == _c[dims-1]) {
      // identical references

      // the distance should nearly always be 0, except if the parent 
      // iload/istore has an offset, which it does for structure references.
      if (distance >= 0) {
        // leading reference didn't change
        _refvecs.Push (CXX_NEW(PF_REFVEC(bitpos, maxdepth, dvec, distance),
                               PF_mpool));
      }
      else {
        // although the references are identical, they must be differing
        // in the offset in the iload/istore. That makes the new reference
        // the new leading reference. So update.
        
        // _c vector remains the same
        // dvecs     remain the same
        // min/max are in terms of elements, not bytes, and remain the same

        // update the distance of the stored references
        for (i=0; i<_refvecs.Elements(); i++) {
          _refvecs.Bottom_nth(i)->Update_Distance(distance);
        }

        // update distance
        if (distance > _max_dist) _max_dist = distance;
        else if (distance < _min_dist) _min_dist = distance;
        // now scale to new leading reference
        _max_dist -= distance;
        _min_dist -= distance;

        // update the leading reference
        // and add a refvec for the old leading reference
        _refvecs.Push (CXX_NEW
                       (PF_REFVEC(_leading_ref, maxdepth, dvec, -distance),
                        PF_mpool));
        _leading_ref = bitpos;
      }

      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return TRUE;
    }

    // the references differ in constant along stride-one dimension
    // The incoming reference may be the new leading reference 
    // if it is "ahead" along the stride-one dimension in the direction 
    // that the stride-one loop (innermost loop also in stride-one dim) 
    // is travelling. 
    // Specifically, +ve step and +ve constant, or -ve step
    // and -ve constant difference
    // E.g. 
    // do i = 1,100     a[i], a[i+1] then a[i+1] is leading reference
    // do i = 100,1,-1  a[i], a[i+1] then a[i] is leading reference
    INT stride_one_loop = Get_Stride_One_Loop ();
    if (stride_one_loop < 0) {
      // didn't find an innermost loop
      // therefore leading reference doesn't matter
      // if leading reference didn't change, then just add it and return
      if (distance > _max_dist) _max_dist = distance;
      else if (distance < _min_dist) _min_dist = distance;

      _refvecs.Push (CXX_NEW(PF_REFVEC(bitpos, maxdepth, dvec, distance),
                             PF_mpool));
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return TRUE;
    }

    // are we travelling forwards or backwards in memory
    mINT16 stride_forward = Stride_Forward ();
    Is_True (stride_forward, ("Stride-one loop but no direction!"));

    // now find the constant offset in the last (stride-one) dimension
    if (((stride_forward > 0) &&
         (aa->Dim(dims-1)->Const_Offset > _c[dims-1])) ||
        ((stride_forward < 0) &&
         (aa->Dim(dims-1)->Const_Offset < _c[dims-1]))) {
      // new leading reference
      INT64 const_diff = aa->Dim(dims-1)->Const_Offset-_c[dims-1];

      // update _c vector of constants
      for (i=0; i<dims; i++) _c[i] = aa->Dim(i)->Const_Offset;

      // Stored dvecs do not change, since this reference
      // has the same dvec as the previous leading reference.
      // However, min/max may change for the stride-one loop.
      _min_iter[stride_one_loop] =
        minof(_min_iter[stride_one_loop], const_diff);
      _max_iter[stride_one_loop] =
        maxof(_max_iter[stride_one_loop], const_diff);

      // update the distance of the stored references
      for (i=0; i<_refvecs.Elements(); i++)
        _refvecs.Bottom_nth(i)->Update_Distance(distance);

      // update distance
      if (distance > _max_dist) _max_dist = distance;
      else if (distance < _min_dist) _min_dist = distance;
      // now scale to new leading reference
      _max_dist -= distance;
      _min_dist -= distance;

      // update the leading reference
      // and add a refvec for the old leading reference
      _refvecs.Push (CXX_NEW
                     (PF_REFVEC(_leading_ref, maxdepth, dvec, -distance),
                      PF_mpool));
      _leading_ref = bitpos;
      CXX_DELETE_ARRAY (dvec, PF_mpool);
      return TRUE;
    }

    // leading reference didn't change:
    // update min/max for stride-one loop, and add the reference
    INT64 const_diff = _c[dims-1]-aa->Dim(dims-1)->Const_Offset;
    _min_iter[stride_one_loop] = minof(_min_iter[stride_one_loop], const_diff);
    _max_iter[stride_one_loop] = maxof(_max_iter[stride_one_loop], const_diff);

    if (distance > _max_dist) _max_dist = distance;
    else if (distance < _min_dist) _min_dist = distance;

    _refvecs.Push (CXX_NEW (PF_REFVEC(bitpos, maxdepth, dvec, distance),
                            PF_mpool));
    CXX_DELETE_ARRAY (dvec, PF_mpool);
    return TRUE;
  }

  INT nz = i;   // free up 'i' for local usage
  // ah - found the first non-zero
  // dvec is not identical. i is the outermost loop index that differs.

  // Now go up from current loop till we find this loop 
  // (where the dvec is non-zero)
  PF_LOOPNODE* loopnode = Get_Loop();
  for (i=maxdepth-1; i!=nz; i--) 
    loopnode = loopnode->Get_Parent ();
  DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();

  // is the step for this loop positive or negative?
  // if non-constant, assume positive
  BOOL steppos = TRUE;
  if ((dli->Step->Is_Const()) &&
      (dli->Step->Const_Offset < 0)) steppos = FALSE;

  if (((dvec[nz].N() < 0) && steppos) ||
      ((dvec[nz].N() > 0) && !steppos)) {

    // we have a new leading ref:
    //  dvec is negative, step is positive, or 
    //  dvec is positive, step is negative

    // update _c vector of constants
    for (i=0; i<aa->Num_Vec(); i++) _c[i] = aa->Dim(i)->Const_Offset;

    // update the dvecs of each stored reference to be relative 
    // to the leading reference
    // and the distances
    for (i=0; i<_refvecs.Elements(); i++) {
      _refvecs.Bottom_nth(i)->Update_dvec (dvec);
      _refvecs.Bottom_nth(i)->Update_Distance (distance);
    }

    // scale min/max to be relative to new leading reference
    for (i=0; i<maxdepth; i++) {
      mINT32 val = dvec[i].N();
      _max_iter[i] = _max_iter[i] - val;
      _min_iter[i] = _min_iter[i] - val;
      // the new distance vector relative to new leading ref is -dvec
      // so flip dvec
      dvec[i] = -dvec[i];
      val = dvec[i].N();
      if (val > _max_iter[i]) _max_iter[i] = val;
      else if (val < _min_iter[i]) _min_iter[i] = val;
    }
    
    // update distance
    if (distance > _max_dist) _max_dist = distance;
    else if (distance < _min_dist) _min_dist = distance;
    // now scale to new leading reference
    _max_dist -= distance;
    _min_dist -= distance;

    // now add a refvec for the old leading reference
    _refvecs.Push (CXX_NEW
                   (PF_REFVEC(_leading_ref, maxdepth, dvec, -distance),
                    PF_mpool));
    _leading_ref = bitpos;
  }
  else {
    // else no change in leading reference
    // push the incoming reference
    _refvecs.Push (CXX_NEW (PF_REFVEC(bitpos, maxdepth, dvec, distance),
                            PF_mpool));

    // update min/max
    for (INT i=_depth; i<maxdepth; i++) {
      mINT32 val = dvec[i].N();
      if (val > _max_iter[i]) _max_iter[i] = val;
      else if (val < _min_iter[i]) _min_iter[i] = val;
    }
    if (distance > _max_dist) _max_dist = distance;
    else if (distance < _min_dist) _min_dist = distance;
  }
  CXX_DELETE_ARRAY (dvec, PF_mpool);
  return TRUE;
} /* PF_LG::Add_Ref () */

/* Compute the constant offset of ref from the base address for an
   inductive base addr cases.
 */
INT64 PF_LG::Offset_to_Base_Addr (WN* array) 
{
  int kid;
  INT64 offset = 0;

  FmtAssert (_myugs->Get_BA()->Get_Inductive_Base(),
             ("Expect an inductive base address. \n"));
  FmtAssert (WN_operator(array) == OPR_ARRAY, ("Expect an array op.\n"));

  int n = WN_num_dim(array);
  for(kid = 1; kid < n+1; kid++) {
      FmtAssert (WN_operator(WN_kid(array, kid)) == OPR_INTCONST,
                 ("Expect a constant dimension.\n"));
      FmtAssert (WN_operator(WN_kid(array, kid + n)) == OPR_INTCONST,
                 ("Expect a constant subscript.\n"));

      offset *= WN_const_val(WN_kid(array, kid));
      offset += WN_const_val(WN_kid(array, kid + n));
  }
  offset *= WN_element_size(array);
  WN *parent = LWN_Get_Parent(array);
  FmtAssert (WN_operator(parent) == OPR_ILOAD || 
             WN_operator(parent) == OPR_ISTORE,
             ("Expect an ILOAD or ISTORE.\n"));
  // TODO: to be precie, we should take into account the field id in the parent ref 
  // (iload or istore).
  return offset;
}

/* This is similar to PF_LG::Add_Ref except that it adds a reference
   to PF_REFLIST for an inductive base addr case.
 */
BOOL PF_LG::Add_Induc_Base_Ref (WN* ref, mINT16 bitpos) {
  INT64 offset = Offset_to_Base_Addr(ref);

  // Instead of distance to the leading ref, we use the distance field 
  // to store the offset to the (inductive) base address.
  _refvecs.Push (CXX_NEW(PF_REFVEC(bitpos, 0, NULL, offset),
                 PF_mpool));

  // The one with the smallest offset will be the _leading_ref.
  if (_myugs->Stride_Forward () >= 0) {
    if (offset < _refvecs.Bottom_nth(_leading_ref)->Distance()) {
      _leading_ref = bitpos;
    }
  } else {
    if (offset > _refvecs.Bottom_nth(_leading_ref)->Distance()) {
      _leading_ref = bitpos;
    }
  }
}

    
/***********************************************************************
 *
 * add a locality group (union lg with "this")
 * lref is leading reference of the incoming group.
 *
 * This doesn't care about number of caches, their size, or linesize.
 * Just adds the group if its leading reference can overlap along the 
 * stride-one dimension.
 *
 ***********************************************************************/
BOOL PF_LG::Add_Group (PF_LG* lg, WN* lref) {
#ifdef Is_True_On
  if (Debug_Prefetch) {
    Check ();
    lg->Check ();
    Check_Ref (lg->_leading_ref);
    for (INT i=0; i<lg->_refvecs.Elements(); i++)
      Check_Ref (lg->_refvecs.Bottom_nth(i)->Refnum());
  }
#endif
  FRAC* dvec = Ref_In_LG (lref);
  
  if (!dvec) return FALSE;

  mINT16 maxdepth = Get_Depth ()+1;
  INT i;

  _numlines_1L = _numlines_2L = 0;  // this forces recomputation in Split_LG

  // compute distance in bytes from leading reference
  INT64 distance = Distance_LR (lref, dvec);

  // TODO: for now assume that it's in the same LG, irrespective of distance
  // leading ref of lg is in this LG, so merge the two LGs

  /* scubadoo: assume that the _refvecs within each group is sorted,
   * and merge the two (sorted) groups using mergesort so that the 
   * resultant _refvecs is also sorted.
   */

  // First: update leading reference
  //  compare the dvec we got, which is of the current leading reference
  //  relative to the leading reference of the incoming group
  
  for (i=_depth; i<maxdepth; i++) 
    if (dvec[i].N() != 0) break;

  if (i == maxdepth) {
    // the two leading refs are the same - Error!
    CXX_DELETE_ARRAY (dvec, PF_mpool);
    FmtAssert (FALSE,
               ("Two identical refs went into different locality groups\n"));
    return FALSE;
  }

  INT nz = i;
  // ah - found the first non-zero
  // dvec is not identical. i is the outermost loop index that differs.

  // Now go up from current loop till we find this loop 
  // (where the dvec is non-zero)
  PF_LOOPNODE* loopnode = Get_Loop();
  for (i=maxdepth-1; i!=nz; i--) 
    loopnode = loopnode->Get_Parent ();
  DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();

  // is the step for this loop positive or negative?
  // if non-constant, assume positive
  BOOL steppos = TRUE;
  if ((dli->Step->Is_Const()) &&
      (dli->Step->Const_Offset < 0)) steppos = FALSE;

  if (((dvec[nz].N() < 0) && steppos) ||
      ((dvec[nz].N() > 0) && !steppos)) {
    // we have a new leading ref:
    //  dvec is negative, step is positive, or 
    //  dvec is positive, step is negative

    // update _c vector of constants
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, lref);
    for (i=0; i<aa->Num_Vec(); i++) _c[i] = aa->Dim(i)->Const_Offset;

    // update the dvecs of each stored reference to be relative 
    // to the leading reference
    for (i=0; i<_refvecs.Elements(); i++) {
      _refvecs.Bottom_nth(i)->Update_dvec (dvec);
      _refvecs.Bottom_nth(i)->Update_Distance (distance);
    }

    // scale min/max to be relative to new leading reference
    for (i=0; i<maxdepth; i++) {
      mINT32 val = dvec[i].N();
      INT64 oldmin = _min_iter[i];
      INT64 oldmax = _max_iter[i];
      _min_iter[i] = lg->_min_iter[i];
      _max_iter[i] = lg->_max_iter[i];


      // now take union of this and old min/max
      _max_iter[i] = maxof(_max_iter[i], (oldmax-val));
      _min_iter[i] = minof(_min_iter[i], (oldmax-val));
      
      // the new distance vector relative to new leading ref is -dvec
      // so flip dvec
      dvec[i] = -dvec[i];

      // see if new dvec changes anything
      val = dvec[i].N();
      if (val > _max_iter[i]) _max_iter[i] = val;
      else if (val < _min_iter[i]) _min_iter[i] = val;
    }

    // scale min/max distance
    INT64 oldmin = _min_dist;
    INT64 oldmax = _max_dist;
    _min_dist = lg->_min_dist;
    _max_dist = lg->_max_dist;
    if ((oldmin-distance) < _min_dist) _min_dist = (oldmin-distance);
    if ((oldmax-distance) > _max_dist) _max_dist = (oldmax-distance);

    // add a refvec for the old leading reference
    Is_True (Check (), ("oops - error\n"));
    _refvecs.Push (CXX_NEW
                   (PF_REFVEC(_leading_ref, maxdepth, dvec, -distance),
                    PF_mpool));
    // add a refvec for each reference in incoming group
    for (i=0; i<lg->_refvecs.Elements(); i++) {
      _refvecs.Push (CXX_NEW (PF_REFVEC(lg->_refvecs.Bottom_nth(i)),
                              PF_mpool));
    }
    _leading_ref = lg->_leading_ref;
    Is_True (Check (), ("oops - error\n"));
  }
  else {
    // no change in leading reference
    // add a refvec for the incoming leading reference
    Is_True (Check (), ("oops - error\n"));
    _refvecs.Push (CXX_NEW
                   (PF_REFVEC(lg->_leading_ref, maxdepth, dvec, distance),
                    PF_mpool));
    Is_True (Check (), ("oops - error\n"));

    INT oldmax = _refvecs.Elements ();
    // add a refvec for each reference in incoming group
    for (i=0; i<lg->_refvecs.Elements(); i++) {
      _refvecs.Push (CXX_NEW (PF_REFVEC(lg->_refvecs.Bottom_nth(i)),
                              PF_mpool));
    }
    Is_True (Check (), ("oops - error\n"));

    // update the dvecs of each new reference to be relative 
    // to the original leading reference
    // but first need to negate dvec, since it's opposite of what
    // Update_dvec wants
    for (i=0; i<maxdepth; i++) dvec[i] = -dvec[i];
    for (i=oldmax; i<_refvecs.Elements(); i++) {
      _refvecs.Bottom_nth(i)->Update_dvec (dvec);
      _refvecs.Bottom_nth(i)->Update_Distance (-distance);
    }
        
    // scale incoming min/max and update
    for (i=0; i<maxdepth; i++) {
      INT inc_min_iter = lg->_min_iter[i] - dvec[i].N();
      INT inc_max_iter = lg->_max_iter[i] - dvec[i].N();
      _max_iter[i] = maxof (_max_iter[i], inc_max_iter);
      _min_iter[i] = minof (_min_iter[i], inc_min_iter);

      // see if new dvec changes anything
      // since dvec was negated above, flip it again
      mINT32 val = -dvec[i].N();
      if (val > _max_iter[i]) _max_iter[i] = val;
      else if (val < _min_iter[i]) _min_iter[i] = val;
    }
    if ((lg->_min_dist+distance) < _min_dist)
      _min_dist = lg->_min_dist+distance;
    if ((lg->_max_dist+distance) > _max_dist)
      _max_dist = lg->_max_dist+distance;
  }
  CXX_DELETE_ARRAY (dvec, PF_mpool);
  return TRUE;
} /* PF_LG::Add_Group () */



/***********************************************************************
 *
 * Given an array "dist" with "num" elements that are already sorted
 * in increasing order, insert the given "elem" into the list in place.
 *
 ***********************************************************************/
static void Insert (INT64* dist, INT num, INT64 elem) {
  INT i, j;
  for (i=0; i<num; i++) if (dist[i] >= elem) break;
  if (i == num) 
    // all are smaller
    dist[num] = elem;
  else {
    for (j=num; j>i; j--) dist[j] = dist[j-1];
    dist[i] = elem;
  }
}


/***********************************************************************
 *
 * Split LG into one (or more) pseudo-LGs.
 * This code becomes specific to number of caches, cache lines, etc.
 * Currently handles upto two levels of caches.
 *
 ***********************************************************************/
void PF_LG::Split_LG () {

  if (_numlines_1L) return;     // don't need recomputing - already computed

  INT i;
  mINT16 stride_one_loop = Get_Stride_One_Loop ();

  // TODO: the following pre-sorted scheme.
  /* scubadoo: assume that the _refvecs are sorted. walk them to break them
   * into chunks. Each chunk is such that the "spread" -- distance between
   * min_dist and max_dist -- is small compared to the number of iterations
   * in the stride-one loop. This reuse is likely to be exploited, therefore
   * this spread contributes to the min_iter/max_iter of the stride-one loop.
   *
   * Then there will be gaps in the sorted list that are much larger than
   * the number of iterations in the stride-one loop, likely leading to no
   * reuse. These gaps will cause the creation of another chunk which will
   * be treated like a new locality group with the same min_iter/max_iter 
   * vectors except for the values in the stride-one loop which may be 
   * different. The volume calculation for this new chunk will be identical to 
   * that of the others.
   */

  // For now just copy the numbers,
  // doing insertion sort while copying them

  // watch-out: must add the leading reference too, 
  // which is *not* in _refvecs
  INT64* dist = CXX_NEW_ARRAY (INT64, _refvecs.Elements()+1, PF_mpool);
  Insert (dist, 0, 0);  // distance of leading reference is zero
  for (i=0; i<_refvecs.Elements(); i++)
    Insert (dist, i+1, _refvecs.Bottom_nth(i)->Distance());

  // now walk the sorted list, and construct the chunks

  // initially assume one line at each level
  _numlines_1L = 1;
  _numlines_2L = 1;

  if (stride_one_loop == -1) {
    // no stride-one loop. Just count the number of references, 
    // since there will be no spatial locality anymore,
    // any spread (maxdist) beyond a cache line will basically translate into
    // another cache line for all purposes.
    INT64 gap;
    INT64 curdist_1L = 0;
    INT64 curdist_2L = 0;
    for (i=1; i<(_refvecs.Elements()+1); i++) {
      gap = dist[i] - dist[i-1];
      Is_True ((gap >= 0), ("Split_LG: some error in sorting distances\n"));
      curdist_1L += gap;
      if (curdist_1L > Cache.LineSize(1)) {
        _numlines_1L++;
        curdist_1L = 0;
      }
      if (Cache.Levels() > 1) {
        curdist_2L += gap;
        if (curdist_2L > Cache.LineSize(2)) {
          _numlines_2L++;
          curdist_2L = 0;
        }
      }
    }
  }
  else {
    INT64 curdist_1L = 0;      // current spread in bytes of current pseudo-LG
    INT64 curdist_2L = 0;
    INT64 maxdist_1L = 0;      // maximum spread across all pseudo-LGs
    INT64 maxdist_2L = 0;
    INT64 gap = 0;
    
    for (i=1; i<(_refvecs.Elements()+1); i++) {
      gap = dist[i] - dist[i-1];
      Is_True ((gap >= 0), ("Split_LG: some error in sorting distances\n"));
      if (gap > Cache.LineSize(1)) {
        // new cache line
        _numlines_1L++;
        maxdist_1L = maxof (maxdist_1L, curdist_1L);
        curdist_1L = 0;
        if (Cache.Levels() > 1) {
          if (gap > Cache.LineSize(2)) {
            _numlines_2L++;
            maxdist_2L = maxof (maxdist_2L, curdist_2L);
            curdist_2L = 0;
          }
          else curdist_2L += gap;
        }
      }
      else {
        curdist_1L += gap;
        curdist_2L += gap;
      }
    }
    maxdist_1L = maxof (maxdist_1L, curdist_1L);
    maxdist_2L = maxof (maxdist_2L, curdist_2L);

    // TODO: Ideally I should store the maxdist (spread) for both 1L and 2L
    // and then use them appropriately. But that would complicate the min/max 
    // calculations. Instead, just conservatively use the larger of these two
    // numbers, which should be maxdist_2L (if two-level), else maxdist_1L.

    INT64 maxdist;
    if (Cache.Levels() == 1) maxdist = maxdist_1L;
    else maxdist = maxdist_2L;

    // First convert maxdist to iters rather than bytes
    INT64 sz = (INT) ABS(WN_element_size(Get_Ref(_leading_ref)));
    maxdist = (maxdist+sz-1)/sz;
    if ((_max_iter[stride_one_loop]-_min_iter[stride_one_loop]) < maxdist) {
      // update the spread
      _min_iter[stride_one_loop] = 0;
      _max_iter[stride_one_loop] = maxdist;
    }
  }

  Is_True ((_numlines_1L>0),
           ("Split_LG returned 0 (or less) lines in lev-1 cache\n"));
  Is_True ((_numlines_2L>0),
           ("Split_LG returned 0 (or less) lines in lev-2 cache\n"));
  CXX_DELETE_ARRAY (dist, PF_mpool);
}

/***********************************************************************
 *
 * Return TRUE if the loopnode for this locality group is an outer tile
 * for this reference, FALSE otherwise.
 *
 * Conditions for outer tile:
 *  - this index variable must appear in both the lower-bound and
 *    the upper bound expressions for an inner loop containing this
 *    reference, and
 *  - the reference must use the index variable of that inner loop.
 *
 ***********************************************************************/
PF_LOOPNODE* Is_Outer_Tile (PF_LOOPNODE* inner_ln, PF_LOOPNODE* outer_ln,
                    ACCESS_ARRAY* aa) {
  INT i, j;
  PF_LOOPNODE* ln;
  WN* outer_wn;
  INT ref_depth = inner_ln->Get_Depth();
  INT loop_depth = outer_ln->Get_Depth();
  
  // Let's find the code for the loopnode for the
  // current loop we're computing volume for
  ln = inner_ln;     // loopnode immediately containing these references
  ref_depth = ln->Get_Depth();
  for (i=ref_depth; i!=loop_depth; i--) {
    ln = ln->Get_Parent ();
  }
  outer_wn = ln->Get_Code();
  
  {
    // some debugging stuff
    WN* index_wn = WN_index(outer_wn);
    const char* name = ((ST_class(WN_st(index_wn)) != CLASS_PREG) ?
                  ST_name(WN_st(index_wn)) :
                  (WN_offset(index_wn) > Last_Dedicated_Preg_Offset ?
                   Preg_Name(WN_offset(index_wn)) : "DEDICATED PREG"));
    // printf ("Is_Outer_Tile: query for %s: ", name);
  }
  // Now find the reference
  // ACCESS_ARRAY* aa = _myugs->Get_AA();
  ACCESS_VECTOR* av;
  for (i=0; i<aa->Num_Vec(); i++) {
    av = aa->Dim(i);
    ln = inner_ln;
    for (j=ref_depth; j>loop_depth; j--) {
      if (av->Loop_Coeff(j)) {
        // access depends on loop j. Is "j" an inner tile of outer_loop?
        WN* cur_wn = ln->Get_Code();
        Is_True (cur_wn != outer_wn,
                 ("Temporal reuse, but loop var used in index expr"));
        // This while loop is needed to handle multiple levels of tiling
        while (1) {
          cur_wn = Outer_Tile (cur_wn, Du_Mgr);
          if (cur_wn == NULL) break;
          // is current loop is an outer tile?
          if (cur_wn == outer_wn) {
            // printf ("is an outer tile\n");
            return ln;
          }
        }
      }
      ln=ln->Get_Parent();
    }
  }      
  // printf ("is NOT an outer tile, so presumable really temporal\n");
  return NULL;
}

/***********************************************************************
 *
 * Compute the volume of the references in this locality group,
 * for a loop at the given depth. depth must be the same as the
 * depth of this locality group, since we create a new LG for each
 * loop in the nest. Hence the depth argument appears to be redundant.
 *
 * Most of the computation is independent of the number of caches
 * (at least while computing the number of distinct elements referenced.
 * At the end of the routine we convert that to bytes based on the number
 * of caches and the line size in each. That portion assumes upto two-levels 
 * of caches.
 *
 ***********************************************************************/
PF_VOLUME PF_LG::Volume () {
  PF_VOLUME myvol (1, 1);       // since vol is a product of terms
  INT i, j;
  VECTOR_SPACE<FRAC>* KerH  = Get_KerH  ();
  VECTOR_SPACE<FRAC>* KerHs = Get_KerHs ();
  // const MAT<FRAC>& Hsbasis = KerHs->Basis ();
  INT num_loops = KerHs->N();
  INT num_bvs   = KerHs->D();
  BOOL* done_loop = CXX_NEW_ARRAY (BOOL, num_loops, PF_mpool);

  Is_True (Check (), ("oops - error\n"));
  // split LGs first if necessary
  Split_LG ();

  for (i=0; i<num_loops; i++) done_loop[i] = FALSE;

  // copy KerH and KerHs into temporary storage that can be modified
  VECTOR_SPACE<FRAC> sKerH  (KerH, PF_mpool);
  VECTOR_SPACE<FRAC> sKerHs (KerHs, PF_mpool);

  // Construct a vector space for current loop-nest (depth to num_loops)
  VECTOR_SPACE<FRAC> thisnest (num_loops, PF_mpool, FALSE);
  FRAC ubv[LNO_MAX_DO_LOOP_DEPTH];   // ubv == unit basis vector
  for (i=_depth; i<num_loops; i++) {
    // insert another unit basis vector
    for (j=0; j<num_loops; j++) {
      if (j == i) ubv[j] = 1;
      else ubv[j] = 0;
    }
    thisnest.Insert (ubv);
  }

  BOOL kerh_overflow = FALSE, kerhs_overflow = FALSE;
  
  FRAC::Exception = FALSE; 
  sKerH *= thisnest;    kerh_overflow |= (FRAC::Exception);
  FRAC::Exception = FALSE; 
  sKerHs *= thisnest;   kerhs_overflow |= (FRAC::Exception);
  FRAC::Exception = FALSE;

  if (kerh_overflow) {
    // vectors in sKerH contain very large numbers, 
    // and basically there is no reuse. 
    DevWarn ("Vector contains large numbers -- assuming no reuse");
  }
  else {
    const MAT<FRAC>& Hbasis = sKerH.Basis (); // get basis vectors
    num_bvs = sKerH.D();

    // now go through each basis vector at a time. 
    // Note - these are all temporal locality vectors
    for (i=0; i<num_bvs; i++) {

#   ifdef Is_True_On
      // this vector better be within the depth i'm considering (within sKer)
      for (j=0; j<_depth; j++)
        if (Hbasis(i,j).N() != 0)
          Is_True (0, ("Intersection of KerH and thisnest is wrong\n"));
#   endif

      PF_LOOPNODE* tmp_loopnode = Get_Loop ();
      for (j=(num_loops-1); j>=_depth; j--) {
        // check that all elements in the vector are less than 20 (heuristic)
        if (abs(Hbasis(i,j).N()) >= 20) break;
        // check that num_iters is greater than constant term (heuristic)
        DO_LOOP_INFO* dli = tmp_loopnode->Get_LoopInfo ();
        if (abs(Hbasis(i,j).N()) > Get_Good_Num_Iters(dli)) break;
        tmp_loopnode = tmp_loopnode->Get_Parent();
      }
      if (j != (_depth-1)) continue;
    
      // let's compute the number of distinct elements
      INT product_coeff = 1;
    
      // count the number of non-zeros in basis vector
      INT num_nz = 0;

      for (j=_depth; j<num_loops; j++) {
        if (Hbasis(i,j).N() != 0) {
          num_nz++;
          product_coeff = product_coeff * Hbasis(i,j).N();
          done_loop[j] = TRUE;
        
          // TODO: what if a loop index appears in more than one basis vector?
          // The volume expressions become imprecise.
          // and going to orthogonal basis doesn't help either...
        }
      }
      product_coeff = abs (product_coeff);

      INT num_vectors = 0;
      // number of vectors along basis vector that exist in the space
      
      PF_LOOPNODE* loopnode = Get_Loop ();
      // do this loop backwards -- easier to get the loopnodes at the same time
      for (j=(num_loops-1); j>=_depth; j--) {
        if (Hbasis(i,j).N() != 0) {
          // non-zero c_j
          DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
          UINT64 num_iters = Get_Good_Num_Iters(dli);
          INT64 spread = (_max_iter[j]-_min_iter[j]);
          spread = abs(spread);
          num_iters += spread;
          num_vectors += (product_coeff/(abs(Hbasis(i,j).N()))) * num_iters;
        }
        loopnode = loopnode->Get_Parent ();
      }
      if (num_nz == 1) {
        // Find current loop
        PF_LOOPNODE* loopnode = Get_Loop ();
        INT dp = loopnode->Get_Depth (); /* depth of innermost do 
                                          * containing the reference.
                                          * _depth == depth of current loop.
                                          */
        while (dp != _depth) {
          dp--;
          loopnode = loopnode->Get_Parent();
        }
        DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
        if ((!dli->Is_Inner_Tile) &&
            (Is_Outer_Tile(Get_Loop(), loopnode, _myugs->Get_AA()))) {
          // no locality at all -- multiply out by loop trip count.
          UINT64 num_iters = Get_Good_Num_Iters(dli);
          INT64 spread = (_max_iter[dp]-_min_iter[dp]);
          spread = abs(spread);
          num_iters += spread;
          myvol *= num_iters;
        }
        // otherwise really temporal, no contribution to volume
        continue;
      }
      num_vectors -= product_coeff-1;
      FmtAssert (num_vectors >= 0,
                 ("Computing volume: Num_vectors should be positive, is %d\n",
                  num_vectors));
      myvol *= num_vectors;
    }
  }
  // done with all basis vectors
  // now multiply by non-locality loops

  PF_LOOPNODE* loopnode = Get_Loop ();
  // do this loop backwards -- easier to get the loopnodes at the same time
  for (j=(num_loops-1); j>=_depth; j--) {
    if (done_loop[j]) continue;
    DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
    UINT64 num_iters = Get_Good_Num_Iters(dli);
    INT64 spread = (_max_iter[j]-_min_iter[j]);
    spread = abs(spread);
    num_iters += spread;
    myvol *= num_iters;
    loopnode = loopnode->Get_Parent ();
  }

  // volume thus far is in number of distinct elements, 
  // presumed one cache line per element. 
  // now consider spatial aspect. 
  // (if kerhs_overflow occured, just be pessimistic, and bypass this portion).
  if (!(kerhs_overflow || kerh_overflow) &&
      (sKerHs.D() > sKerH.D())) {
    ACCESS_ARRAY* aa =  (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map,
                                                    Get_Ref(_leading_ref));
    ACCESS_VECTOR* av = aa->Dim(aa->Num_Vec()-1);
    INT stride = INT_INFINITY;
    // find stride, based on loop within thisnest that has the smallest coeff
    for (j=_depth; j<num_loops; j++) {
      INT64 cur = av->Loop_Coeff(j);
      if (cur == 0) continue;
      cur = abs(cur);
      stride = minof (stride, cur);
    }
    if (stride < INT_INFINITY) {
      // "stride" elements
      stride = stride * (INT) ABS(WN_element_size(Get_Ref(_leading_ref)));
      // now stride is in bytes. Is it within a cache line?
      if (stride < Cache.LineSize(1)) {
        INT vec = Cache.LineSize(1)/stride;
        myvol.vol_1L = (myvol.vol_1L+vec-1)/vec;    // number of 1L cache lines
        if (Cache.Levels()>1) {
          vec = Cache.LineSize(2)/stride;
          myvol.vol_2L = (myvol.vol_2L+vec-1)/vec;  // number of 2L cache lines
        }
      }
      else if (stride < Cache.LineSize(2)) {
        INT vec = Cache.LineSize(2)/stride;
        myvol.vol_2L = (myvol.vol_2L+vec-1)/vec;    // number of 2L cache lines
      }
      else {
        // no real spatial reuse
      }
    }
  }

  // convert lines to bytes
  myvol.vol_1L = myvol.vol_1L * Cache.LineSize(1);
  myvol.vol_2L = myvol.vol_2L * Cache.LineSize(2);

  // multiply by the number of pseudo-LGs (num_lines)
  myvol.vol_1L = myvol.vol_1L * _numlines_1L;
  myvol.vol_2L = myvol.vol_2L * _numlines_2L;

  PF_PRINT (
    fprintf (TFile, "vol lg[0x%lx]: symbol: ", this);
    _myugs->Get_BA()->Get_Symbol()->Print (TFile);
    myvol.Print (TFile);
  );
              
  return myvol;
}

struct PF_SORTED_REFS {
  INT64     dist;       // distance (in bytes) from leading reference
  mINT16    refnum;     // reference number (in reflist in UGS)
  mINT16    refvecnum;  // number (in refvecs in LG)
  mINT16    lrnum;      // leading reference number
};

/***********************************************************************
 *
 * Given the array of refvecs and the leading referece,
 * construct an array of PF_SORTED_REFS in which the elements
 * occur in sorted order of their distances.
 *
 ***********************************************************************/
static PF_SORTED_REFS* Sort_Refvecs (PF_REFVEC_DA* refvecs, mINT16 leadingref){
  // One extra since refvecs doesn't have the leading reference.
  PF_SORTED_REFS* srefs =
    CXX_NEW_ARRAY (PF_SORTED_REFS, refvecs->Elements()+1, PF_mpool);
  srefs[0].dist = 0;
  srefs[0].refnum = leadingref;
  srefs[0].refvecnum = -1;   // -1 for leading reference
  INT num = 1;

  for (INT i=0; i<refvecs->Elements(); i++) {
    // scan the current list (has i+1 elements)
    PF_REFVEC* refvec = refvecs->Bottom_nth(i);
    INT j;
    for (j=0; j<(i+1); j++)
#ifdef KEY //preventing leader being swept away
      if (srefs[j].dist > refvec->Distance()) break;
#else
      if (srefs[j].dist >= refvec->Distance()) break;
#endif
    if (j == (i+1)) {
      // all are smaller or equal
      srefs[i+1].dist  = refvec->Distance();
      srefs[i+1].refnum = refvec->Refnum ();
      srefs[i+1].refvecnum = i;
    }
    else {
      for (INT k=(i+1); k>j; k--) srefs[k] = srefs[k-1];
      srefs[j].dist  = refvec->Distance();
      srefs[j].refnum = refvec->Refnum ();
      srefs[j].refvecnum = i;
    }
  }

  PF_PRINT( fprintf(TFile,"Sort_Refvecs:\n");
            for (INT i=0; i<(refvecs->Elements()+1); i++) {
              fprintf(TFile, "PF_SORTED_REFS: dist %d, refnum %d, refvecnum %d, lrnum %d\n",
                      srefs[i].dist, srefs[i].refnum, srefs[i].refvecnum, srefs[i].lrnum);
            } );

#ifdef Is_True_On
  {
    for (INT i=0; i<(refvecs->Elements()-1); i++) {
      Is_True (srefs[i].dist <= srefs[i+1].dist,
               ("Sort_Refvecs: sorting error during prefetching"));
    }
  }
#endif

  return srefs;
}

struct SORT_STR {
  mINT16 num;
  mINT16 eq;      /* 1 if LR_Compare/equal to previous ref */
};

/***********************************************************************
 *
 * Given the srefs, compute and fill in leading reference order
 * for references start through stop-1.
 *
 ***********************************************************************/
void PF_LG::LR_Ordering (PF_SORTED_REFS* srefs, INT start, INT stop) {
  INT i, j;

  /* Now compute and store a sorted leading-ref number */

  SORT_STR* lr_num = CXX_NEW_ARRAY (SORT_STR, stop-start, PF_mpool);
  lr_num[0].num = start;
  lr_num[0].eq  = 0;
  for (INT cur=start+1; cur<stop; cur++) {
    // insert element 'cur'. 
    // Walk the previous elements backwards, looking for a place to insert
    INT order;
    for (i=cur-start-1; i>=0; i--) {
      // compare elements "cur" and lr_num[i]
      order = LR_Compare (srefs[cur].refvecnum,
                              srefs[lr_num[i].num].refvecnum);
      // stop if lr_num[i] same as cur, or it leads cur
      if (order <= 0) break;
    }
    // insert to the right of "i" in sorted list
    for (j=cur-start; j>(i+1); j--)
      lr_num[j] = lr_num[j-1];
    lr_num[j].num = cur;
    lr_num[j].eq  = (order == 0 ? 1 : 0);
  }

  // now fill in lrnum in srefs
  j = 1;
  for (i=start; i<stop; i++) {
    srefs[lr_num[i-start].num].lrnum = j;
    if (i<(stop-1)) {
      if (lr_num[i-start+1].eq == 0) j++;
    }
  }

#ifdef Is_True_On
  for (i=start; i<stop; i++) {
    if ((srefs[i].lrnum <= 0) || (srefs[i].lrnum > (stop-start)))
      FmtAssert (FALSE, ("sorting error\n"));
  }
#endif
  
  CXX_DELETE_ARRAY (lr_num, PF_mpool);
}

/***********************************************************************
 *
 * Given the original reference, the desired locality vector,
 * and the split vector, locate the real reference to which the prefetch
 * should be attached, based on the loop split that has been done.
 *
 * The bitvec returned has a 1 for each version of the reference
 * that needs to be prefetched, and a 0 if it doesn't need prefetching.
 * The least-significant bit (position 0) corresponds to the initial
 * (non-versioned) reference, and is always prefetched. The bits then
 * go towards higher-order bits for each successive version.
 *
 ***********************************************************************/
INT PF_LG::Get_Bit_Vec (PF_DESC* pfdesc,
                        PF_LEVEL level,
                        PF_SPLIT_VECTOR *split_vec) {
  INT bitvec = 0x1; // presumably the initial reference is always included
  INT i;

  if (split_vec == NULL) {
    // no splits were performed; just return 0
    return bitvec;
  }
  mINT16 split_depth = split_vec->Get_Depth();
  mINT16* orig_split_vector = split_vec->Get_Vector ();

  // Copy the original split vector into a local copy,
  // and then update it based on the logic for actual splitting
  // i.e. don't split if the split-factor is greater than the loop bound.
  mINT16* split_vector = (mINT16*) alloca(split_depth*sizeof(mINT16));
  PF_LOOPNODE* loopnode = split_vec->Get_Loop();
  for (i=split_depth-1; i>=0; i--) {
    DO_LOOP_INFO* dli = loopnode->Get_LoopInfo();
    split_vector[i] = orig_split_vector[i];
    if (split_vector[i] > 1) {
      if (!dli->Num_Iterations_Symbolic &&
          (split_vector[i] >= Get_Good_Num_Iters(dli)))
        split_vector[i] = 1;
    }
    loopnode = loopnode->Get_Parent();
  }

  // if split_vec is non-NULL, then we're guaranteed that we're on
  // the split path. However, only the innermost references undergo full
  // versioning based on the complete split_vector. Non-innermost refs
  // may have partial versioning.

#ifdef Is_True_On
  {
    PF_LOOPNODE* leaf = split_vec->Get_Loop ();
    PF_LOOPNODE* myloop = Get_Loop ();
    while (leaf) {
      if (leaf == myloop) break;
      leaf = leaf->Get_Parent ();
    }
    if (leaf == NULL) 
	  FmtAssert (FALSE, ("Get_Bit_Vec - this loop hasn't been versioned\n"));
  }
#endif

  // Just check the split_vec depth upto my loop level
  // since these references only get versioned upto that level
  mINT16 myloopdepth = Get_Depth ()+1;
  mINT16 count = 1;
  for (i=0; i<myloopdepth; i++) {
    // if (split_vector[i] > 1) count *= 2;
    // Since we continue splitting on only one side, 
    // we actually just ADD an extra reference each time we split.
    if (split_vector[i] > 1) count++;
  }
  // count is now the total number of references 

  Is_True (pfdesc->Kind(level) != none,
           ("Get_Bit_Vec: prefetch kind is none\n"));
  // Handle the "all" case first.
  if (pfdesc->Kind(level) == all) {
    for (i=0; i<count-1; i++)
      bitvec = ((bitvec << 1) | 0x1);
    return bitvec;
  }

  // "vec". see if this vec has a chance.
  mINT16 depth = Get_Depth ()+1;
  mINT16* vec = pfdesc->Vec (level);
  mINT16 version_number = 0;
  for (i=0; i<depth && i<split_depth; i++) {
    if (split_vector[i] < 2) continue;
    
    // this loop has been split. to prefetch or not to prefetch?
    version_number++;

    if ((vec[i] == 0) || ((vec[i] % split_vector[i]) == 0)) {
      // don't need to prefetch, or by a perfect multiple of split
      // so don't prefetch. i.e. leave bitvec unchanged.
    }
    else {
      // prefetch
      bitvec = bitvec | (1 << version_number);
      // bitvec = ((bitvec << 1) | 0x1);
    }
  }
  return bitvec;
}

/***********************************************************************
 *
 * Given a reference and the bitposition in the versions,
 * follow the version_map pointers and locate the appropriate version
 * of the reference.
 *
 ***********************************************************************/
WN* PF_LG::Get_Ref_Version (WN* ref, INT bitpos) {
  extern WN_MAP version_map;
  while (bitpos > 0) {
    ref = (WN*) WN_MAP_Get (version_map, ref);
    Is_True (ref, ("Get_Ref_Version: couldn't find a map\n"));
    bitpos--;
  }
  return ref;
}

#if defined(TARG_IA64)
BOOL Contain_Induction_Variable (WN* wn, ST_IDX idx)
{
  if (WN_st_idx(wn) == idx) return TRUE;
  for (INT kid = 0; kid < WN_kid_count(wn); kid ++)
    if (Contain_Induction_Variable(WN_kid(wn, kid), idx))
      return TRUE;
  return FALSE;
}

void Update_Array_Index (WN* wn, WN* wn_incr, WN* wn_induc)
{
  for (INT kid = 0; kid < WN_kid_count(wn); kid ++) {
    WN* wn_kid = WN_kid(wn, kid);
    if (WN_st_idx(wn_kid) == WN_st_idx(wn_induc)
         && SYMBOL(wn_kid) == SYMBOL(wn_induc)) {
      TYPE_ID desc = Promote_Type(WN_rtype(wn_kid));
      WN* wn_ahead = LWN_Make_Icon(desc, LNO_Prefetch_Iters_Ahead);
      wn_ahead = LWN_CreateExp2(OPCODE_make_op(OPR_MPY, desc, MTYPE_V), 
                                                  WN_CopyNode(wn_incr), wn_ahead);
      WN_kid(wn, kid) = LWN_CreateExp2(OPCODE_make_op(OPR_ADD, desc, MTYPE_V), wn_kid, wn_ahead);
      LWN_Set_Parent(WN_kid(wn, kid), wn);
    }
    // bug fix for OSP_348
    //
    else if ((WN_operator(wn_kid) == OPR_CVT || WN_operator(wn_kid) == OPR_CVTL || WN_operator(wn_kid) == OPR_TRUNC)
	     && (WN_st_idx(WN_kid(wn_kid, 0)) == WN_st_idx(wn_induc) && SYMBOL(WN_kid(wn_kid, 0)) == SYMBOL(wn_induc)))
    {
      TYPE_ID desc = Promote_Type(WN_rtype(wn_kid));
      WN* wn_ahead = LWN_Make_Icon(desc, LNO_Prefetch_Iters_Ahead);
      wn_ahead = LWN_CreateExp2(OPCODE_make_op(OPR_MPY, desc, MTYPE_V),
		                WN_CopyNode(wn_incr), wn_ahead);
      WN_kid(wn, kid) = LWN_CreateExp2(OPCODE_make_op(OPR_ADD, desc, MTYPE_V), wn_kid, wn_ahead);
      LWN_Set_Parent(WN_kid(wn, kid), wn);
    }
    else {
      Update_Array_Index(wn_kid, wn_incr, wn_induc);
    }
  }
}
#endif

#ifdef KEY //bug 10953 -- to generate prefetch address
static WN *Gen_Pf_Addr_Node(WN *invariant_stride, WN *array, WN *loop)
{
   OPCODE mpy_opc = OPCODE_make_op(OPR_MPY,WN_rtype(array), MTYPE_V);
   OPCODE add_opc= OPCODE_make_op(OPR_ADD,WN_rtype(array), MTYPE_V);
   OPCODE intconst_opc= OPCODE_make_op(OPR_INTCONST,WN_rtype(array), MTYPE_V);

   WN *stridenon = LWN_Copy_Tree(invariant_stride, TRUE, LNO_Info_Map);
   LWN_Copy_Def_Use(invariant_stride, stridenon, Du_Mgr);

   INT const_val;
   WN *array_index = WN_kid(array, WN_num_dim(array)<<1);
   if(Is_Loop_Invariant_Exp(array_index, loop))
     const_val = LNO_Prefetch_Stride_Ahead;
   else const_val = LNO_Prefetch_Stride_Ahead*ABS(WN_element_size(array));

   WN *stride_node = LWN_CreateExp2(add_opc, array,
           LWN_CreateExp2(mpy_opc,stridenon,
              WN_CreateIntconst(intconst_opc, const_val)));

   return stride_node;
}

static BOOL Is_Other_Array_Bad(WN *wn, INT dim)
{
  if (WN_operator(wn) == OPR_BLOCK){
    for (WN* kid=WN_first(wn); kid; kid=WN_next(kid)){ 
      if(Is_Other_Array_Bad(kid, dim))
        return TRUE;
    }
    return FALSE;
 }else if(WN_operator(wn) == OPR_ARRAY){
   if(WN_element_size(wn) < 0){ //bug 14169: may not be contiguous
    WN *array_parent = LWN_Get_Parent(wn);
    if(WN_operator(array_parent)!=OPR_ILOAD &&
       WN_operator(array_parent)!=OPR_ISTORE)
      return TRUE;
     if(!MTYPE_is_vector(WN_desc(array_parent)))
      return TRUE;
    }
    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,wn);
    if(aa==NULL || aa->Num_Vec() > dim) 
      return TRUE;
 }

  for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno ++){
    if (Is_Other_Array_Bad(WN_kid(wn, kidno), dim))
      return TRUE;
  }
 return FALSE;
}

//expression contains no other array reference other than "array"
static BOOL Well_Formed_Expr(WN *expr, WN *array)
{
  if(WN_operator(expr) == OPR_BLOCK)
    return FALSE;
  else if(WN_operator(expr)==OPR_ARRAY){
   if(!Tree_Equiv(expr, array))
    return FALSE;
  }
  for(INT ii = 0; ii< WN_kid_count(expr); ii++)
   if(!Well_Formed_Expr(WN_kid(expr, ii), array))
      return FALSE;
 return TRUE;
}

static BOOL Good_Stmt_To_Adjust_Offset(WN *stmt, WN *array)
{
  if(!stmt) return FALSE;
  OPERATOR opr= WN_operator(stmt);
  switch(opr){
   case OPR_BLOCK:
      if(WN_first(stmt)==WN_last(stmt))
        return Good_Stmt_To_Adjust_Offset(WN_first(stmt), array);
      else return FALSE;
      break;
   case OPR_IF:{
       WN *then_part = WN_then(stmt);
       WN *else_part = WN_else(stmt);
       if(else_part && WN_first(else_part) != NULL)
        return FALSE;
       if(!then_part || !WN_first(then_part))
        return FALSE;
       if(WN_first(then_part) != WN_last(then_part))
        return FALSE;
       return Good_Stmt_To_Adjust_Offset(WN_first(then_part), array);
      }
     break;
   case OPR_ISTORE:{
      WN *kid1 = WN_kid1(stmt);
      WN *kid0 = WN_kid0(stmt);
      if(WN_operator(kid1) != OPR_ARRAY || !Tree_Equiv(kid1, array))
       return FALSE;
      if(!Well_Formed_Expr(kid0, array) || WN_operator(kid0) != OPR_BXOR)
          return FALSE;
       return TRUE;
    }
    break;
    default:
        return FALSE;
     break;
   }      
}  


static BOOL Good_Loop_To_Adjust_Offset(WN *loop, WN *array)
{
   WN *body = WN_do_body(loop);
   if(!body || WN_first(body)==NULL )
      return FALSE;
   if(WN_first(body)==WN_last(body))
     return Good_Stmt_To_Adjust_Offset(WN_first(body), array);
   return FALSE;
}  


static BOOL Larger_Dimension_Arrays_In(WN *wn, INT dim)
{
  if (WN_operator(wn) == OPR_BLOCK){
    for (WN* kid=WN_first(wn); kid; kid=WN_next(kid)){
      if(Larger_Dimension_Arrays_In(kid, dim))
	return TRUE;
    }
    return FALSE;
  }else if(WN_operator(wn) == OPR_ARRAY){
    ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,wn);
    if(aa==NULL || aa->Num_Vec() > dim)
      return TRUE;
  }

  for (UINT kidno = 0; kidno < WN_kid_count(wn); kidno ++){
    if (Larger_Dimension_Arrays_In(WN_kid(wn, kidno), dim))
      return TRUE;
  }
  return FALSE;
}
#endif

/***********************************************************************
 *
 * Given the array of sorted references (refs in sorted order of distance)
 * generate a prefetch node to cover the reference nos: start .. stop-1
 * Insert the generated prefetch node into the whirl code, and
 * add mapping from the loads/stores to this prefetch node.
 *
 ***********************************************************************/
void PF_LG::Gen_Pref_Node (PF_SORTED_REFS* srefs, mINT16 start, mINT16 stop,
                           PF_LEVEL level, PF_DESC* pfdesc,
                           PF_SPLIT_VECTOR* split_vec) {

  // BOOL innermost_loop = Get_Loop()->Innermost_Loop ();
  // Because of loop splitting (versioning) this process may need to be done
  // for multiple instances of exactly the same set of references.
  // Find the bitvector for the set of references.
  INT bitvec = Get_Bit_Vec (pfdesc, level, split_vec);
  INT curbitpos = 0;

  // if user has disabled L1 cache, then we actually want to 
  // generate L2 prefetches, not L1, although our analysis can
  // be for an L1 cache. So this flag is used for CG-related things
  // in this routine.
  PF_LEVEL level_for_cg = level;
  if ((level == level_1) &&
      (Cache.Levels() == 1) && Cache.Level1_Really_Level2()) {
    // actually an L2
    level_for_cg = level_2;
  }

  // For all the relevant versions of this set of references, do
  while (bitvec) {

    // OK - now prefetch references [start through stop-1], inclusive
    BOOL spatial_in_loop = FALSE;
    // 1. Create prefetch node
    // Build flag
    // Determine Read or Write prefetch
    UINT32 flag = 0;
    BOOL readpf = TRUE;
    INT j;
    for (j=start; j<stop; j++) {
      WN* ref = Get_Ref (srefs[j].refnum);
      Is_True (WN_operator(LWN_Get_Parent(ref)) == OPR_ILOAD || 
               WN_operator(LWN_Get_Parent(ref)) == OPR_ISTORE,
               ("Gen_Pref_Node: Parent of array ref is not a iload/istore\n"));
      if (WN_operator(LWN_Get_Parent(ref)) == OPR_ISTORE)
        readpf = FALSE;
    }
    if (readpf) PF_SET_READ(flag); else PF_SET_WRITE(flag);

    // Now determine the confidence
    PF_LOOPNODE* loopnode = Get_Loop ();
    PF_LOCLOOP locloop = loopnode->Get_locloop ();
    mINT16 confidence;
    // Look at the confidence of the localized loops
    switch (level) {
    case level_1:
      confidence = locloop.Confidence_1L ();
      if (confidence == 3) {
        if (locloop.Loop_1L() == 0) {
          // localized is outermost. What about its total_iter?
          INT d=Get_Depth(); 
          PF_LOOPNODE* oloop = loopnode;
          while (d>0) {
            oloop = oloop->Get_Parent ();
            d--;
          }
          switch (Loop_Confidence(oloop->Get_LoopInfo())) {
          case 3: 
          {
            // confident of that one too
            PF_VOLUME vol = oloop->Get_Total ();
            if (vol.Localized_1L()) {
#ifdef KEY
              confidence = (LNO_Run_Prefetch > SOME_PREFETCH) ? (LNO_Run_Prefetch - 1) : LNO_Run_Prefetch;
#else
              confidence = LNO_Run_Prefetch;
#endif
            }
            break;
          }
          case 2:
          case 1:
            confidence = 2;
            break;
          }
        }
      }
      else {
        // if confidence in loop is 2 (high), that means that max-iter is not
        // too far off from est-iter. So make prefetch less important (conf=1).
        // And vice versa.
        confidence = 2;
        // should we make confidence 1? If outermost, and localized in total
        INT d = Get_Depth();
        if ((locloop.Confidence_1L() == 2) && (locloop.Loop_1L() <= d)) {
          PF_LOOPNODE* oloop = loopnode;
          while (d > locloop.Loop_1L()) {
            oloop = oloop->Get_Parent();
            d--;
          }
          PF_VOLUME vol = oloop->Get_Total();
          if (vol.Localized_1L()) confidence = 1;
        }
        // should we prefetch more aggressively?
        if (confidence == 1) {
#ifdef KEY
              confidence = (LNO_Run_Prefetch > SOME_PREFETCH) ? (LNO_Run_Prefetch - 1) : LNO_Run_Prefetch;
#else
          confidence = LNO_Run_Prefetch;
#endif
        }
      }
      break;
    case level_2:
      confidence = locloop.Confidence_2L ();
      if (confidence == 3) {
        if (locloop.Loop_2L() == 0) {
          // localized is outermost. What about its total_iter?
          INT d=Get_Depth(); 
          PF_LOOPNODE* oloop = loopnode;
          while (d>0) {
            oloop = oloop->Get_Parent ();
            d--;
          }
          switch (Loop_Confidence(oloop->Get_LoopInfo())) {
          case 3:
          {
            // confident of that one too
            PF_VOLUME vol = oloop->Get_Total ();
            if (vol.Localized_2L()) {
#ifdef KEY
              confidence = (LNO_Run_Prefetch > SOME_PREFETCH) ? (LNO_Run_Prefetch - 1) : LNO_Run_Prefetch;
#else
              confidence = LNO_Run_Prefetch;
#endif
            }
            break;
          }
          case 2:
          case 1:
            confidence = 2;
            break;
          }
        }
      }
      else {
        // if confidence in loop is 2 (high), that means that max-iter is not
        // too far off from est-iter. So make prefetch less important (conf=1).
        // And vice versa.
        confidence = 2;
        // should we make confidence 1? If outermost, and localized in total
        INT d = Get_Depth();
        if ((locloop.Confidence_2L() == 2) && (locloop.Loop_2L() <= d)) {
          PF_LOOPNODE* oloop = loopnode;
          while (d > locloop.Loop_2L()) {
            oloop = oloop->Get_Parent();
            d--;
          }
          PF_VOLUME vol = oloop->Get_Total();
          if (vol.Localized_2L()) confidence = 1;
        }
        // should we prefetch more aggressively?
        if (confidence == 1) {
#ifdef KEY
              confidence = (LNO_Run_Prefetch > SOME_PREFETCH) ? (LNO_Run_Prefetch - 1) : LNO_Run_Prefetch;
#else
          confidence = LNO_Run_Prefetch;
#endif
        }
      }
      break;
    case level_1and2:
      confidence = minof(locloop.Confidence_1L(), locloop.Confidence_2L());
      break;
    }
    PF_SET_CONFIDENCE (flag, confidence);

    // Compute stride. Important if innermost loop. 
    // Probably ignored if non-innermost loop, 
    // where we may generate a conditional.
    if ((level == level_1) || (level == level_1and2)) {
      if (pfdesc->Kind(level_1) == all){
        if (level_for_cg == level_2) PF_SET_STRIDE_2L (flag, 1);
        else PF_SET_STRIDE_1L (flag, 1);
      }
      else {
        Is_True (pfdesc->Kind(level_1) == vec,
                 ("Gen_Pref_Node: prefetch when kind is none\n"));
        Is_True (pfdesc->Vec(level_1),
                 ("Gen_Pref_Node: kind is vec, but vector is NULL\n"));
        mINT16* prefetch_vec = pfdesc->Vec(level_1); 
        mINT16 depth = Get_Depth();
        if (prefetch_vec[depth]) {
	  INT dp = depth-1;
	  while (dp >= 0) {
	    if (prefetch_vec[dp]) break;
	    dp--;
	  }
	  if(dp<0) spatial_in_loop = TRUE;

          if (level_for_cg == level_2)
            PF_SET_STRIDE_2L (flag, prefetch_vec[depth]);
          else PF_SET_STRIDE_1L (flag, prefetch_vec[depth]);
        }
        else {
          // we come here if the locality is spatial but in another loop
          // (not this one). Verify that first, and then simply prefetch
          // in every iter (doing some redundant prefetches).
#ifdef Is_True_On
          INT dp = depth-1;
          while (dp >= 0) {
            if (prefetch_vec[dp]) break;
            dp--;
          }
          Is_True (dp >= 0, ("Error in computing spatial locality"));
#endif
          PF_SET_STRIDE_1L (flag, 1);
        }
      }
    }
    if ((level == level_2) || (level == level_1and2)) {
      if (pfdesc->Kind(level_2) == all)
        PF_SET_STRIDE_2L (flag, 1);
      else {
        Is_True (pfdesc->Kind(level_2) == vec,
                 ("Gen_Pref_Node: prefetch when kind is none\n"));
        Is_True (pfdesc->Vec(level_2),
                 ("Gen_Pref_Node: kind is vec, but vector is NULL\n"));
        mINT16* prefetch_vec = pfdesc->Vec(level_2); 
        mINT16 depth = Get_Depth();
        if (prefetch_vec[depth]) {
	  INT dp = depth-1;
	  while (dp >= 0) {
	    if (prefetch_vec[dp]) break;
	    dp--;
	  }
	  if(dp<0) spatial_in_loop = TRUE;

          PF_SET_STRIDE_2L (flag, prefetch_vec[depth]);
        }
        else {
          // we come here if the locality is spatial but in another loop
          // (not this one). Verify that first, and then simply prefetch
          // in every iter (doing some redundant prefetches).
#ifdef Is_True_On
          INT dp = depth-1;
          while (dp >= 0) {
            if (prefetch_vec[dp]) break;
            dp--;
          }
          Is_True (dp >= 0, ("Error in computing spatial locality"));
#endif
          PF_SET_STRIDE_2L (flag, 1);
        }
      }
    }

    WN* ref = Get_Ref (srefs[start].refnum);
    ref = Get_Ref_Version (ref, curbitpos);
    WN* parent_ref = LWN_Get_Parent (ref);

    // Build array address node
    WN* arraynode = LWN_Copy_Tree (ref, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(ref, arraynode, Du_Mgr);

    OPCODE opcode = WN_opcode (parent_ref);
    WN_OFFSET offset;
    switch (OPCODE_operator(opcode)) {
    case OPR_ILOAD: 
      offset = WN_load_offset (parent_ref);
      break;
    case OPR_ISTORE:
      offset = WN_store_offset (parent_ref);
      break;
    default:
      FmtAssert (false, ("Parent of array ref not a load/store\n"));
      break;
    }

#if defined(TARG_IA64)
    {
      // Go some cache lines ahead

      if ( LNO_Prefetch_Ahead || LNO_Prefetch_Iters_Ahead) {
        INT increment;
        if ((level == level_1) || (level == level_1and2))
          increment =  LNO_Prefetch_Ahead *Cache.LineSize(1);
        else increment =  LNO_Prefetch_Ahead *Cache.LineSize(2);

        if(Stride_Forward())
          increment = Stride_Forward()*increment;
        else
          increment = (Get_Stride_In_Enclosing_Loop()>0) ? increment : (-increment) ;

        // what is the size in bytes that the reference is jumping?
        INT stride_size = ABS(Get_Stride_In_Enclosing_Loop());

        if ((((level == level_1) || (level == level_1and2)) &&
             (stride_size >= Cache.LineSize(1))) ||
            ((level == level_2) && stride_size >= Cache.LineSize(2))) {
          if( Stride_Forward() && Get_Stride_Accurate() ) {
            // we're exceeding a cache line in each iteration,
            // so prefetch some ITERATIONS ahead instead.
            increment = Get_Stride_In_Enclosing_Loop() * LNO_Prefetch_Iters_Ahead;
          }
          else {
            //OSP_233 & OSP_240
            //  DO I = 1, N
            //    DO J = 1, N
            //      SUM = SUM + A(J, I)*B(I, J)
            //    END DO
            //  END DO
            //
            //For this situation, we can not figure out to prefetch how  
            //many cache lines ahead for B(I,J). Instead, we should generate 
            //prefetch for B(I, J + LNO_Prefetch_Iters_Ahead).

            //We don't use offset to prefetch some element ahead
            increment = 0;

            ACCESS_ARRAY *aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
            ACCESS_VECTOR *av;

            // find the first array dimension (going from stride-one outwards)
            // that uses the index of the loop immediately enclosing the reference. 
            // (Ignore coupled subscripts)
            INT i, curr_depth=Get_Depth();
            for (i=aa->Num_Vec()-1; i>=0; i--) {
              av = aa->Dim(i);
              if (av->Loop_Coeff(curr_depth)) break;
            }

            if(i<0) {
              WN* wn_loop = Get_Loop()->Get_Code();        
              WN* wn_induction = WN_index(wn_loop);
              WN* wn_step = WN_step(wn_loop);
              WN* wn_incr = WN_kid(wn_step, 0);
              Is_True(WN_operator(wn_step) == OPR_STID && 
                           WN_operator(wn_incr) == OPR_ADD, ("Incorrect loop index"));

              for (i = 0; i < WN_kid_count(wn_incr); ++i) {
                if( WN_st_idx(WN_kid(wn_incr, i)) != WN_st_idx(wn_induction) ) {
                  wn_incr = WN_kid(wn_incr, i);
                  break;
                }
              }              

              for(i=0; i<WN_num_dim(arraynode); ++i) {
                Update_Array_Index(WN_array_index(arraynode, i), wn_incr, wn_induction);
              }

            } else {
              //In some cases, i could be larger than the maxmium array dimension
              //e.g. U(2*I+J), the array is one-dimension, but Num_Vec()==2.
              //So we just use the innermost dim to perform address computation.
              if(i >= WN_num_dim(arraynode))
                i = WN_num_dim(arraynode) - 1;

              DO_LOOP_INFO* dli = Get_Loop()->Get_LoopInfo();
              INT step;
              if (dli->Step->Is_Const()) {
                step = LNO_Prefetch_Iters_Ahead * av->Loop_Coeff(curr_depth) * dli->Step->Const_Offset;
              }
              else {
                // assume step is 1
                step = LNO_Prefetch_Iters_Ahead * av->Loop_Coeff(curr_depth);
              }
  
              //Update array index
              WN* wn_index = WN_array_index(arraynode, i);
              TYPE_ID desc = Promote_Type(WN_rtype(wn_index));
              OPCODE addop = OPCODE_make_op(OPR_ADD, desc, MTYPE_V);
              WN* wn_ahead = LWN_Make_Icon(desc, step);
              WN_array_index(arraynode, i) = LWN_CreateExp2(addop, wn_index, wn_ahead); 
              LWN_Set_Parent(WN_array_index(arraynode, i), arraynode);
            }
          }

        }

        offset += increment;
      }
    }

#else
    {
      // Go some cache lines ahead

      if ( LNO_Prefetch_Ahead || LNO_Prefetch_Iters_Ahead) {
        INT increment;
        if ((level == level_1) || (level == level_1and2))
         increment =  LNO_Prefetch_Ahead *Cache.LineSize(1);
         else increment =  LNO_Prefetch_Ahead*Cache.LineSize(2);

        increment = Stride_Forward()*increment;

        // what is the size in bytes that the reference is jumping?
        INT stride_size = Get_Stride_In_Enclosing_Loop();
        if ((((level == level_1) || (level == level_1and2)) &&
             (stride_size >= Cache.LineSize(1))) ||
            ((level == level_2) && stride_size >= Cache.LineSize(2))) {

            // we're exceeding a cache line in each iteration,
            // so prefetch some ITERATIONS ahead instead.
            stride_size = stride_size * LNO_Prefetch_Iters_Ahead;
            increment = stride_size;
        }

        offset += increment;
      }
    }
#endif
#if defined(TARG_X8664) || defined(TARG_IA64)

//------------------------------------------------------------------------
//bug 5945: CG ebo will drop some prefetches according to address patterns
//However, for dope vector, it is difficult for CG to figure out. We know
//that the array access is contiguous though simd in LNO, so don't drop it
//bug 11546 : CG ebo should not drop prefetches for vectorized loads or stores
if(LNO_Run_Prefetch > SOME_PREFETCH && 
      (WN_element_size(arraynode) < 0 ||
      (Get_Dim()==1 &&( confidence ==3 || WN_element_size(arraynode) > 8))||
      MTYPE_is_vector(WN_desc(parent_ref)) ||
      MTYPE_is_vector(WN_rtype(parent_ref)))){
  PF_SET_KEEP_ANYWAY(flag);
}
 
//bug 14144: It is difficult for CG to figure out the address patterns for
//indirect array access even though the base is a constant array reference 
if(LNO_Run_Prefetch > SOME_PREFETCH && offset != 0 &&
   Get_Dim() == 1 && confidence >=2 &&
   WN_operator(WN_array_base(arraynode))==OPR_LDID &&
   strncmp(SYMBOL(WN_array_base(arraynode)).Name(),
           Temp_Symbol_Prefix "_misym",
           sizeof(Temp_Symbol_Prefix "_misym") - 1)==0){
   PF_SET_KEEP_ANYWAY(flag);
   WN *loop = Enclosing_Do_Loop(parent_ref);
   if(loop && Good_Loop_To_Adjust_Offset(loop,ref)){
     INT fancy_offset_incr=0;
#ifdef TARG_X8664
     if(Is_Target_Core() || Is_Target_EM64T())
       fancy_offset_incr=8;
     else if(Is_Target_Barcelona() || Is_Target_Orochi())
       fancy_offset_incr=28;
     else
#endif
     {
#if defined(TARG_IA64)
        fancy_offset_incr=28;
#else
        fancy_offset_incr=8;
#endif
        if(LNO_Run_Stream_Prefetch && spatial_in_loop)
        PF_SET_NON_TEMPORAL(flag);
     }
    if(LNO_Prefetch_Ahead==2){
      if((level == level_1) || (level == level_1and2))
        offset += fancy_offset_incr*Cache.LineSize(1);
      else
        offset += fancy_offset_incr*Cache.LineSize(2);
    }
   }
  }


//--------------------------------------------------------------------------
//Bug 13609: to make a decision for streaming prefetch
//(1) only one array reference in this locality group
//(2) the spatial locality only in the innermost loop
//(3) we only consider loads here. Stores may use non-temporal stores in CG
//(4) the array must be in good shape, and we only consider the largest dimensional
//    arrays in a loop.
//---------------------------------------------------------------------------
 if(LNO_Run_Stream_Prefetch  && //depth starts from 0 from outmost
    _refvecs.Elements() == 0 && //only one reference(leader) in this locality group
    spatial_in_loop          && //spatial locality not across loops
    WN_operator(parent_ref)==OPR_ILOAD){  //only consider load

   BOOL stream_pf = TRUE;
   ACCESS_ARRAY* aa=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,arraynode);
   INT loopdepth = Get_Depth(); //make sure whether get depth is the depth
   ACCESS_VECTOR* av1 = aa->Dim(aa->Num_Vec()-1); //first dimention access vector

   if(av1->Non_Const_Loops() != loopdepth)
     stream_pf = FALSE;

   if(av1->Loop_Coeff(loopdepth) != 1)
     stream_pf = FALSE;
   for(INT ii=0; ii<loopdepth; ii++)
     if(av1->Loop_Coeff(ii)!=0){
       stream_pf = FALSE;
       break;
     }
   for(INT ii=0; ii < aa->Num_Vec(); ii++){
     ACCESS_VECTOR* av = aa->Dim(ii);
     if (av->Contains_Lin_Symb() || av->Contains_Non_Lin_Symb()){
       stream_pf = FALSE;
       break;
     }
   }

   if(stream_pf){
     WN *doloop = Enclosing_Do_Loop(parent_ref);
#if defined(TARG_IA64)
     if(Larger_Dimension_Arrays_In(doloop, Get_Dim()))
#elif defined(TARG_X8664)
     if(Is_Other_Array_Bad(doloop, Get_Dim()))
#else  
       // take your pick here
     if(Larger_Dimension_Arrays_In(doloop, Get_Dim()))
#endif
       stream_pf = FALSE;
   }

   if(stream_pf)
     PF_SET_NON_TEMPORAL(flag); //prefetchnta
 }
#endif


#if !(defined(TARG_X8664) || defined(TARG_IA64)) //bug 10953
   WN* pfnode = LWN_CreatePrefetch (offset, flag, arraynode);
#else //bug 10953
   WN* pfnode=NULL;
   WN *do_loop = ref;
   while(do_loop && WN_operator(do_loop) != OPR_DO_LOOP)
     do_loop = LWN_Get_Parent(do_loop); //stop at current

   BOOL inductive_use = FALSE;
   BOOL indirect_use = FALSE;
   WN *invariant_stride=Simple_Invariant_Stride_Access(ref, do_loop, FALSE,
                                      &inductive_use, &indirect_use);
   if(NULL == invariant_stride) //all good
      pfnode = LWN_CreatePrefetch (offset, flag, arraynode);
   else{
      WN *pf_addr_node = Gen_Pf_Addr_Node(invariant_stride, arraynode, do_loop);
      PF_SET_STRIDE_2L(flag, 0); //reset flag
      PF_SET_STRIDE_1L(flag, 0); //reset flag
      if(level_for_cg==level_2)  //no prefetchnta for L2
         PF_SET_STRIDE_2L (flag, 1);
      else{ //cg stuffs
           PF_SET_STRIDE_1L (flag, 1);
           PF_SET_NON_TEMPORAL(flag); //prefetchnta
           PF_SET_KEEP_ANYWAY(flag);  //don't drop
      }
      pfnode = LWN_CreatePrefetch (offset, flag, pf_addr_node);
   }
#endif //bug 10953

    WN_linenum(pfnode) = LWN_Get_Linenum(ref);
    VB_PRINT (vb_print_indent;
              printf (">> pref ");
              Listing_Emit_WN (stdout, parent_ref);
              printf (" %c (L1 %3d) (L2 %3d)\n",
                      (WN_pf_read(pfnode) ? 'R' : 'W'),
                      WN_pf_stride_1L(pfnode),
                      WN_pf_stride_2L(pfnode)));
    if (LNO_Analysis) {
      ls_print_indent; fprintf (LNO_Analysis,
                                "(PREF-NODE %c (CONF %3d) (L1 %3d) (L2 %3d) ",
                                (WN_pf_read(pfnode) ? 'R' : 'W'),
                                WN_pf_confidence(pfnode),
                                WN_pf_stride_1L(pfnode),
                                WN_pf_stride_2L(pfnode));
      Listing_Emit_WN (LNO_Analysis, parent_ref);
      fprintf (LNO_Analysis, "\n");
      ls_num_indent += 2;
      ls_print_indent; fprintf (LNO_Analysis, "(PREF-REFS\n");
      ls_num_indent += 2;
    }
#define MAX_LEN 1024
    char out_string[MAX_LEN];
    char aux_string[MAX_LEN];
    INT out_string_count=0;
    if (LNO_Tlog) {
      extern WN* pf_func_nd;
      Whirl2Src_Init(pf_func_nd);

      char str_buf[64];
      for (INT i=0; i<MAX_LEN; i++)
        out_string[i]='\0';
      sprintf (aux_string, "PREF-NODE %c (CONF %3d) (L1 %3d) (L2 %3d)",
                                (WN_pf_read(pfnode) ? 'R' : 'W'),
                                WN_pf_confidence(pfnode),
                                WN_pf_stride_1L(pfnode),
                                WN_pf_stride_2L(pfnode));

      /* Just print the array base */
      OPERATOR opr = WN_operator(parent_ref);
      char* name;
      switch (opr) {
      case OPR_ILOAD:
        name = ST_name(WN_st(WN_array_base(WN_kid0(parent_ref))));
        break;
      case OPR_ISTORE:
        name = ST_name(WN_st(WN_array_base(WN_kid1(parent_ref))));
        break;
      }
      if (out_string_count+strlen(name)+1 < MAX_LEN) {
        sprintf (&out_string[out_string_count], "%s ", name);
        out_string_count += (strlen(name)+1);
      }
      else out_string_count += strlen(name)+1;
    }
    Is_True(WN_opcode(pfnode) == OPC_PREFETCH,
            ("oops - LWN_CreatePrefetch returned a non-prefetch node\n"));


    // 2. now insert pfnode somewhere
    // Walk up from the reference till we find a statement or scf
    /*
     * WN* wn_stmt = ref;
     * while (!OPCODE_is_stmt(WN_opcode(wn_stmt))) {
     *   wn_stmt = LWN_Get_Parent(wn_stmt);
     *   Is_True (wn_stmt, ("Error trying to find stmt: parent is NULL\n"));
     * }
     *
     *    // Now insert the prefetch just before the stmt
     * WN* wn_block = LWN_Get_Parent (wn_stmt);
     *
     *    Is_True (wn_block, ("Parent of loop is NULL\n"));
     *    Is_True (WN_opcode(LWN_Get_Parent(wn_loop)) == OPC_BLOCK,
     *             ("Parent of stmt not a block\n"));
     *    LWN_Insert_Block_Before (wn_block, wn_loop, pfnode);
     *    Is_True (LWN_Get_Parent(pfnode) == wn_block,
     *             ("Inserted pfnode, but parent got screwed up\n"));
     *
     */

    WN *wn_loop = NULL;

    // we drop a prefetch if it is an F90 optional argument
    // that cannot be speculated 
    // e.g. earliest reference is underneath a CAND or a COR
    BOOL drop_prefetch = FALSE;

    if ((WN_operator(WN_array_base(ref)) == OPR_LDID ||
         WN_operator(WN_array_base(ref)) == OPR_LDA) &&
        (ST_sclass(WN_st(WN_array_base(ref))) == SCLASS_FORMAL_REF  ||
         ST_sclass(WN_st(WN_array_base(ref))) == SCLASS_FORMAL)     &&
        ST_is_optional_argument(WN_st(WN_array_base(ref)))) {

      // for optional arguments, cannot speculate. 
      // So we need to find the earliest of all the references (in the CFG)
      // and insert the prefetch before that one.
      WN *earliest_ref = ref;
      for (INT er=start; er<stop; er++) {
        WN *tmp_ref = Get_Ref (srefs[er].refnum);
        tmp_ref = Get_Ref_Version(tmp_ref, curbitpos);
        if (Is_Lex_Before (tmp_ref, earliest_ref)) earliest_ref = tmp_ref;
      }
      while (!OPCODE_is_stmt(WN_opcode(earliest_ref)) &&
             !OPCODE_is_scf(WN_opcode(earliest_ref))) {
        earliest_ref = LWN_Get_Parent(earliest_ref);
        if (earliest_ref == NULL) {
          DevWarn ("Where did the stmt/scf parent go?");
        }
        if (earliest_ref == NULL ||
            WN_operator(earliest_ref) == OPR_CAND ||
            WN_operator(earliest_ref) == OPR_CIOR) {
          drop_prefetch = TRUE;
          LWN_Delete_Tree (pfnode);
          pfnode = NULL;
          break;
        }
      }
      if (!drop_prefetch) {
        LWN_Insert_Block_Before (NULL, earliest_ref, pfnode);
        wn_loop = earliest_ref;   // for use in Tlogs later
      }
    }
    else {
      // change: insert prefetch at the top of the loop!
      wn_loop = ref;
      while (WN_opcode(wn_loop) != OPC_DO_LOOP) {

        if (WN_opcode(LWN_Get_Parent(wn_loop)) == OPC_DO_LOOP &&
            wn_loop != WN_do_body (LWN_Get_Parent(wn_loop))) {
          // this node hangs off a non-body node of a do-loop.
          // so generate the prefetch before the do, rather than 
          // as the first thing in the body.
          break;
        }

        wn_loop = LWN_Get_Parent(wn_loop);
        if (Is_Mp_Region(wn_loop)) break;
        Is_True (wn_loop, ("Error trying to find loop: parent is NULL\n"));
      }

      WN* wn_block;
      WN* wn_first;
      if (WN_opcode(wn_loop) == OPC_DO_LOOP) {
        // Now insert the prefetch just before the first stmt in loop
        wn_block = WN_do_body(wn_loop);
        wn_first = WN_first(wn_block);
      }
      else if (Is_Mp_Region(wn_loop)) {
        // don't cross an MP region -- insert as the first node in the region
        wn_block = WN_region_body(wn_loop);
        wn_first = WN_first(wn_block);
      }
      else {
        // node was under a non-body kid of a do-loop.
        wn_first = LWN_Get_Parent(wn_loop);
        Is_True (WN_opcode(wn_first) == OPC_DO_LOOP,
                 ("pref-ref under do-loop, but where is the do?"));
        wn_block = LWN_Get_Parent(wn_first);
        wn_first = WN_first(wn_block);
      }

      Is_True (wn_block, ("Body of loop/region is NULL\n"));
      Is_True (WN_opcode(wn_block) == OPC_BLOCK,
               ("Body of do-loop/region not a block\n"));
    
      LWN_Copy_Frequency_Tree(pfnode, WN_first(wn_block));
      LWN_Insert_Block_Before (wn_block, wn_first, pfnode);
      Is_True (LWN_Get_Parent(pfnode) == wn_block,
               ("Inserted pfnode, but parent got screwed up\n"));
    }

    // 3. for refs start..stop-1, add a mapping to this node, with distances
    // First fill in the lrnums
    LR_Ordering (srefs, start, stop);
    for (j=start; j<stop && !drop_prefetch; j++) {
      WN* ref = Get_Ref (srefs[j].refnum);
      ref = Get_Ref_Version (ref, curbitpos);
      WN* parent_ref = LWN_Get_Parent (ref);
      if (LNO_Analysis) {
        ls_print_indent; fprintf (LNO_Analysis, "(");
        Listing_Emit_WN (LNO_Analysis, parent_ref);
        fprintf (LNO_Analysis, "%d)\n", srefs[j].lrnum);
      }
      if (LNO_Tlog) {

        /* Just print the array base */
        OPERATOR opr = WN_operator(parent_ref);
        char* name;
        switch (opr) {
        case OPR_ILOAD:
          name = ST_name(WN_st(WN_array_base(WN_kid0(parent_ref))));
          break;
        case OPR_ISTORE:
          name = ST_name(WN_st(WN_array_base(WN_kid1(parent_ref))));
          break;
        }
        char str_buf[64];
        sprintf(str_buf,"%d", srefs[j].lrnum);

        if (out_string_count+strlen(name)+3+strlen(str_buf)+3 < MAX_LEN) {
          sprintf (&out_string[out_string_count], "( %s ", name);
          out_string_count += (strlen(name)+3);
          sprintf(&out_string[out_string_count],"%s ) ", str_buf);
          out_string_count += (strlen(str_buf)+3);
        }
        else out_string_count += strlen(name)+3+strlen(str_buf)+3;
      }
      VB_PRINT (vb_print_indent;
                printf ("ref: ");
                Listing_Emit_WN (stdout, parent_ref);
                printf ("\n");
        );
      BOOL newnode = FALSE;
      // this guy (array reference), or its parent (load/store)??
      // map(ref) = pfnode;
      PF_POINTER* tmp = (PF_POINTER*) WN_MAP_Get (WN_MAP_PREFETCH, parent_ref);
      if (tmp == NULL) {
        extern MEM_POOL PF_CG_mpool;
        tmp = CXX_NEW (PF_POINTER, &PF_CG_mpool);
        WN_MAP_Set (WN_MAP_PREFETCH, parent_ref, tmp);
        PF_PTR_flag(tmp) = 0;
        SET_AUTO(tmp);
        newnode = TRUE;
      }
      Is_True (WN_MAP_Get (WN_MAP_PREFETCH, parent_ref),
               ("Oops - where did the prefetch map go?\n"));

      switch (level_for_cg) {
      case level_1:
        if (newnode) {
          PF_PTR_wn_pref_2L(tmp) = NULL;
          PF_PTR_lrnum_2L(tmp) = 0;
          PF_PTR_distance_2L(tmp) = 0;
          PF_PTR_set_conf_2L(tmp, 0);
        }
        else {
#ifdef Is_True_On
          Is_True (PF_PTR_wn_pref_1L(tmp) == NULL,
                   ("Error in old node, wn_pref_1L\n"));
          Is_True (PF_PTR_wn_pref_2L(tmp) != NULL,
                   ("Error in old node, wn_pref_2L\n"));
#endif
        }
        PF_PTR_wn_pref_1L(tmp) = pfnode;
        PF_PTR_lrnum_1L(tmp) = srefs[j].lrnum;
        PF_PTR_distance_1L(tmp) = offset;
        PF_PTR_set_conf_1L(tmp, confidence);
        break;
      case level_2:
        if (newnode) {
          PF_PTR_wn_pref_1L(tmp) = NULL;
          PF_PTR_lrnum_1L(tmp) = 0;
          PF_PTR_distance_1L(tmp) = 0;
          PF_PTR_set_conf_1L(tmp, 0);
        }
        else {
#ifdef Is_True_On
          Is_True (PF_PTR_wn_pref_1L(tmp) != NULL,
                   ("Error in old node, wn_pref_1L\n"));
          Is_True (PF_PTR_wn_pref_2L(tmp) == NULL,
                   ("Error in old node, wn_pref_2L\n"));
#endif
        }
        PF_PTR_wn_pref_2L(tmp) = pfnode;
        PF_PTR_lrnum_2L(tmp) = srefs[j].lrnum;
        PF_PTR_distance_2L(tmp) = offset;
        PF_PTR_set_conf_2L(tmp, confidence);
        break;
      case level_1and2:
        Is_True (newnode, ("Duplicate pfnode\n"));
        PF_PTR_wn_pref_1L(tmp) = pfnode;
        PF_PTR_lrnum_1L(tmp) = srefs[j].lrnum;
        PF_PTR_distance_1L(tmp) = offset;
        PF_PTR_set_conf_1L(tmp, confidence);
        PF_PTR_wn_pref_2L(tmp) = pfnode;
        PF_PTR_lrnum_2L(tmp) = srefs[j].lrnum;
        PF_PTR_distance_2L(tmp) = offset;
        PF_PTR_set_conf_2L(tmp, confidence);
        break;
      default:
        Is_True (FALSE, ("Error in level type\n"));
        break;
      }
    }

    if (LNO_Analysis) {
      ls_num_indent -= 2;
      ls_print_indent; fprintf (LNO_Analysis, ")\n");
    }

    // go to the next version
    bitvec = bitvec >> 1;
    curbitpos++;
    while (bitvec && ((bitvec & 0x1) == 0)) {
      bitvec = bitvec >> 1;
      curbitpos++;
    }
    if (LNO_Analysis) {
      ls_num_indent -= 2;
      ls_print_indent; fprintf (LNO_Analysis, ")\n");
    }
    if (LNO_Tlog) {
      if (out_string_count>=MAX_LEN)
        sprintf(out_string,"ref-list too long");
      
      // possible that we stopped before the do coz of an MP-region 
      // or non-body kid
      
      if (WN_opcode(wn_loop) != OPC_DO_LOOP) {
        if (!Is_Mp_Region(wn_loop)) {
          wn_loop = LWN_Get_Parent(wn_loop);
          // wn_loop is now an OPC_DO_LOOP
          wn_loop = LWN_Get_Parent(wn_loop);
        }
        while (WN_opcode(wn_loop) != OPC_DO_LOOP) {
          wn_loop = LWN_Get_Parent(wn_loop);
        }
      }
      Generate_Tlog("LNO","prefetching",
                Srcpos_To_Line(WN_Get_Linenum(wn_loop)),
                ST_name(WN_st(WN_index(wn_loop))), "", out_string, aux_string);
    }
  }
}

/***********************************************************************
 *
 * Given a prefetch descriptor (none, all, or dvec) and the vector
 * for this UGS, generate a prefnode for each cache line in this LG,
 * for each level of the cache.
 * Called when prefetch_p not equal to none, so prefetches are required
 *
 ***********************************************************************/
void PF_LG::Gen_Prefetch (PF_DESC *pfdesc,
                         PF_SPLIT_VECTOR* split_vec,
                         PF_LEVEL level) {

  PF_SORTED_REFS* srefs = Sort_Refvecs (&_refvecs, _leading_ref);
  INT num = _refvecs.Elements()+1;
  // TODO: for now do a simple split, but improve this based on alignment etc

  switch (level) {
  case level_1:
  {
    INT64 curdist = 0;
    mINT16 start = 0; // starting index
    for (mINT16 i=1; i<num; i++) {
      INT64 gap = (srefs[i].dist-srefs[i-1].dist);
      Is_True ( gap >= 0,
                ("Error in Sort_Refvecs - returned non-sorted list\n"));
      curdist += gap;

      if (curdist > Cache.LineSize(1)) {
        // OK - now prefetch references [start through (i-1)], inclusive
        Gen_Pref_Node (srefs, start, i, level_1, pfdesc, split_vec);
        curdist = 0;
        start = i;
      }
    }
    // The last reference(s) is now left, in start..(num-1)
    Gen_Pref_Node (srefs, start, num, level_1, pfdesc, split_vec);
    break;
  }

  case level_2:
  {
    INT64 curdist = 0;
    mINT16 start = 0; // starting index
    for (mINT16 i=1; i<num; i++) {
      INT64 gap = (srefs[i].dist-srefs[i-1].dist);
      Is_True ( gap >= 0,
                ("Error in Sort_Refvecs - returned non-sorted list\n"));
      curdist += gap;

      if (curdist > Cache.LineSize(2)) {
        // OK - now prefetch references [start through (i-1)], inclusive
        Gen_Pref_Node (srefs, start, i, level_2, pfdesc, split_vec);
        curdist = 0;
        start = i;
      }
    }
    // The last reference(s) is now left, in start..(num-1)
    Gen_Pref_Node (srefs, start, num, level_2, pfdesc, split_vec);
    break;
  }

  case level_1and2:
  {
    // Currently this does not get called.
    // But this code is here for when we get smarter and 
    // merge redundant prefetch nodes for first and second level.
    Is_True (FALSE, ("Gen_Pref_Node: one level at a time...\n"));
    INT64 curdist_1L = 0;
    INT64 curdist_2L = 0; 
    mINT16 start_1L = 0; // starting index
    mINT16 start_2L = 0;    
    for (mINT16 i=1; i<num; i++) {
      INT64 gap = (srefs[i].dist-srefs[i-1].dist);
      Is_True ( gap >= 0,
                ("Error in Sort_Refvecs - returned non-sorted list\n"));
      curdist_1L += gap;
      curdist_2L += gap;

      if ((Cache.Levels () > 1) &&
          (curdist_1L > Cache.LineSize(1)) &&
          (curdist_2L > Cache.LineSize(2)) &&
          (start_1L == start_2L)) {
        // ok -- one prefetch node will suffice for 
        // both 1st-lev and 2nd-lev pref
        Gen_Pref_Node (srefs, start_1L, i, level_1and2, pfdesc, split_vec);
        curdist_1L = curdist_2L = 0;
        start_1L = start_2L = i;
        continue;
      }

      // need a separate node for 1st-level and 2nd-level
      if (curdist_1L > Cache.LineSize(1)) {
        // OK - now prefetch references [start through (i-1)], inclusive
        Gen_Pref_Node (srefs, start_1L, i, level_1, pfdesc, split_vec);
        curdist_1L = 0;
        start_1L = i;
      }
      if ((Cache.Levels() > 1) && curdist_2L > Cache.LineSize(2)) {
        // OK - now prefetch references [start through (i-1)], inclusive
        Gen_Pref_Node (srefs, start_2L, i, level_2, pfdesc, split_vec);
        curdist_2L = 0;
        start_2L = i;
      }
    }
    // The last reference(s) is now left, in start..(num-1)
    if (Cache.Levels() > 1) {
      if (start_1L == start_2L) 
        Gen_Pref_Node (srefs, start_2L, num, level_1and2, pfdesc, split_vec);
      else {
        Gen_Pref_Node (srefs, start_1L, num, level_1, pfdesc, split_vec);
        Gen_Pref_Node (srefs, start_2L, num, level_2, pfdesc, split_vec);
      }
    }
    else {
      // one-level
      Gen_Pref_Node (srefs, start_1L, num, level_1, pfdesc, split_vec);
    }
    break;
  }

  default:
    Is_True (FALSE, ("Illegal case in switch for level\n"));
    break;
  }
  
  CXX_DELETE_ARRAY (srefs, PF_mpool);
}

/* This function is similar to Gen_Pref_Node(), but this deals with
   the cases where the base address of an ARRAY being inductive.
 */
void PF_LG::Gen_Induc_Base_Pref_Node (PF_SORTED_REFS* srefs, mINT16 srefnum)
{
  UINT32 flag = 0;
  INT j, increment = 0;
  WN* wn_array = Get_Ref (srefs[srefnum].refnum);

  WN *parent = LWN_Get_Parent(wn_array);
  WN_OFFSET offset = WN_offset(parent);
  WN *wn_loop = parent;
  /* Find the enclosing loop. */
  while (wn_loop && (WN_opcode(wn_loop)!=OPC_DO_LOOP)) {
    wn_loop = LWN_Get_Parent(wn_loop);
  }
  WN *wn_block = NULL;
  if (!wn_loop) {
    FmtAssert(FALSE, ("Expect DO_LOOP!\n"));
  } else {
    wn_block = WN_do_body(wn_loop);
  }

  // Since there are special heuristics downstream to remove prefetches 
  // for stores, we mark the prefetches inserted for inductive base
  // address cases as READ.
  PF_SET_READ(flag);
  PF_SET_CONFIDENCE(flag, AGGRESSIVE_PREFETCH);
  PF_SET_NON_TEMPORAL(flag); //prefetchnta
  PF_SET_KEEP_ANYWAY(flag);  // Tell CG not to remove this prefetch.
  UINT32 save_flag = flag;

  PF_SET_STRIDE_1L(flag,1);
  WN *arraynode = LWN_Copy_Tree(wn_array,TRUE,LNO_Info_Map);
  LWN_Copy_Def_Use(wn_array, arraynode, Du_Mgr);
  WN *base;
  BOOL indirect_base = FALSE;
  BOOL inductive_base = FALSE;
  mINT32 stride_val = 0;

#if (defined(TARG_X8664) || defined(TARG_IA64))
  Inductive_Base_Addr_Const_Stride(wn_array, wn_loop, &base,
                     &inductive_base, &indirect_base, &stride_val);
#endif

  UINT32 local_prefetch_iters_ahead = LNO_Prefetch_Iters_Ahead;

  if (!indirect_base) {
    /* Determine how far down the distance should be for the given
       prefetch.
     */
    if (stride_val < Cache.LineSize(1)) {
      if (LNO_Prefetch_Ahead != 0) {
        increment =  LNO_Prefetch_Ahead * Cache.LineSize(1);
      } else if (LNO_Prefetch_Iters_Ahead != 0) {
        increment =  LNO_Prefetch_Iters_Ahead * stride_val;
      } else {
        increment =  2 * Cache.LineSize(1);
      }
    } else if (stride_val >= 8 * Cache.LineSize(1)) {
      // stride >= 512B
      local_prefetch_iters_ahead = LNO_Prefetch_Iters_Ahead * 3;
      increment = stride_val * local_prefetch_iters_ahead;
    } else {
      // 64B <= stride < 512B
      local_prefetch_iters_ahead = LNO_Prefetch_Iters_Ahead;
      increment = stride_val * local_prefetch_iters_ahead;
    }
    offset += increment;
  }

  WN* pfnode = LWN_CreatePrefetch (offset, flag, arraynode);
  WN *pf_array_base = NULL;
  BOOL base_tree_prev_def = FALSE;
  WN* prior_def = NULL;

  if (WN_operator(WN_kid0(pfnode)) == OPR_ARRAY &&
      WN_operator(WN_kid0(WN_kid0(pfnode))) == OPR_LDID) {
    pf_array_base = WN_kid0(WN_kid0(pfnode));
    // Get its def 
    DEF_LIST* def_list=Du_Mgr->Ud_Get_Def(pf_array_base);
    DEF_LIST_ITER d_iter(def_list);
    /* Since we did not consider the incomplete cases during the
       candidate selection in Inductive_Base_Addr_Const_Stride(),
       no need to consider them here.
     */
    for (DU_NODE* dnode=d_iter.First(); !d_iter.Is_Empty();
                  dnode=d_iter.Next()) {
      FmtAssert(prior_def == NULL, ("Encounter multiple defs"));
      prior_def=dnode->Wn();
      base_tree_prev_def = TRUE;
    }
  }

  // If the base addr tree has a def in a preceding tree, insert
  // the prefetch after the def to ensure all uses after their defs.
  // Otherwise, we can insert the prefetch at the beginning of the
  // block.
  if (!base_tree_prev_def ) {
    LWN_Insert_Block_Before (wn_block, WN_first(wn_block), pfnode);
  } else {
    WN *wn_parent = prior_def;
    while (wn_parent && LWN_Get_Parent(wn_parent) != wn_block) {
      wn_parent = LWN_Get_Parent(wn_parent);
    }
    FmtAssert(wn_parent, ("prior_def not in this loop!"));

    LWN_Insert_Block_After (wn_block, wn_parent, pfnode);
  }

  LWN_Copy_Frequency_Tree (pfnode, WN_first(wn_block));

  PF_PRINT( fprintf(TFile, "Gen_Induc_Base_Pref_Node: offset %d, increment %d, stride_val %d, LNO_Prefetch_Ahead %d, LNO_Prefetch_Iters_Ahead %d\n", 
            offset, increment, stride_val, LNO_Prefetch_Ahead, LNO_Prefetch_Iters_Ahead);
            fdump_tree(TFile, arraynode);
            fdump_tree(TFile, pfnode); );

  // Establish the prefetch map with its associated memory op. Note that
  // we generate only one prefetch and link to both L1 and L2 prefetches.
  PF_POINTER* tmp = (PF_POINTER*) WN_MAP_Get (WN_MAP_PREFETCH, parent);
  if (tmp == NULL) {
    extern MEM_POOL PF_CG_mpool;
    tmp = CXX_NEW (PF_POINTER, &PF_CG_mpool);
    WN_MAP_Set (WN_MAP_PREFETCH, parent, tmp);
    PF_PTR_flag(tmp) = 0;
    SET_AUTO(tmp);
    PF_PTR_lrnum_1L(tmp) = 0;
  }
  PF_PTR_wn_pref_1L(tmp) = pfnode;
  PF_PTR_distance_1L(tmp) = offset;
  PF_PTR_set_conf_1L(tmp, AGGRESSIVE_PREFETCH);

  PF_PTR_wn_pref_2L(tmp) = NULL;
  PF_PTR_lrnum_2L(tmp) = 0; 
  PF_PTR_distance_2L(tmp) = 0;
  PF_PTR_set_conf_2L(tmp, 0);

  if (indirect_base) {
    /* Since this is an indirect inductive base address case, we need to
       modify the tree by incrementing some iterations ahead.
       If the base address has a load defined in a prior tree, we want to
       merge the def (a duplicate) to the WN tree of the base address.
     */
    if (base_tree_prev_def) {
      if (WN_operator(prior_def) == OPR_STID) {
        WN *new_def = LWN_Copy_Tree(prior_def,TRUE,LNO_Info_Map);
        // Replace the base LDID with a copy of the kid of STID.
        WN_kid0(WN_kid0(pfnode)) = WN_kid0(new_def);
        LWN_Parentize_One_Level(WN_kid0(pfnode));
        pf_array_base = WN_kid0(WN_kid0(pfnode));
      }
    }
    FmtAssert(pf_array_base, ("Unable to locate the array base for a prefetch!\n"));

    // Search for an loop index load in the tree.
    WN *loop_index_ld = NULL; 
    for (WN_TREE_ITER<PRE_ORDER, WN*> iter (pf_array_base);
         iter.Wn () != NULL; ++iter) {
      WN *wn = iter.Wn ();

      if (WN_operator(wn) == OPR_LDID &&
          SYMBOL(wn) == SYMBOL(WN_index(wn_loop))) {
        // This tree should have no more than one loop index load.
        FmtAssert(loop_index_ld == NULL, ("Have more than one inductive var!\n"));
          loop_index_ld = wn;
      }
    }

    FmtAssert(loop_index_ld, ("Unable to locate an induction var!\n"));
    WN *ld_parent = LWN_Get_Parent(loop_index_ld);

    FmtAssert(WN_operator(ld_parent) == OPR_CVT ||
              WN_operator(ld_parent) == OPR_ADD ||
              WN_operator(ld_parent) == OPR_MPY,
              ("Unexpected operator for an inductive load."));

    /* Generate the expression to increment the iteration indexing. */
    WN* iconst_wn = LWN_Make_Icon(MTYPE_I4, local_prefetch_iters_ahead );
    WN* add_wn = LWN_CreateExp2(OPCODE_make_op(OPR_ADD, MTYPE_I4, 
                                MTYPE_V), iconst_wn, loop_index_ld);
    if (WN_operator(ld_parent) == OPR_CVT) {
      WN_kid0(ld_parent) = add_wn;
    } else if (WN_operator(ld_parent) == OPR_MPY ||
               WN_operator(ld_parent) == OPR_ADD) {
      if( WN_kid0(ld_parent) == loop_index_ld) {
        WN_kid0(ld_parent) = add_wn;
      } else {
        WN_kid1(ld_parent) = add_wn;
      }
    }
    LWN_Parentize_One_Level(ld_parent);

    PF_PRINT( fprintf(TFile, "A new inductive and indirect base prefetch node:\n"); 
              fdump_tree(TFile, pfnode); );
  } else {
    PF_PRINT( fprintf(TFile, "A new inductive base prefetch node:\n"); 
              fdump_tree(TFile, pfnode); );
  }
}

/* Note that the current volume and locality analyses are too inaccurate
   to the inductive base address cases. Hence, while we are reusing the
   data structures, such as PF_BASE_ARRAY and PF_UGS, which were constructed
   at PF_LOOPNODE::Process_Refs(), we discard PF_LG constructed earlier and
   the analysis from the current framework by Mowry for the inductive base
   address cases.
     Instead we are using the following framework for the inductive base
   address cases.

   Santhanam, Gornish, & Hsu, Data Prefetching on the HP PA-8000, ISCA 97.

   This is a simple and effective framework, in particular sufficient for
   less complicated and overlapped access patterns.

   The part that we leverage is where we represent the same partition of
   linear inductive address expressions (a1*i + b1 and a2*i + b2, where 
   a1 = a2) as one PF_LG. We sort refs in the PF_LG based on the 
   constant offset terms. We then select leaders, where any ref whose
   offset within 1 cache line of the previous leader will not be a leader.
   We will then insert prefetches only for the leaders.

   Sharing the same data structures makes it easier to integrate the two
   approach under one framework in the future. However, it's unclear what
   would be the best way to integrate them yet given the strengths and 
   weaknesses of the two approaches.
 */ 
void PF_LG::Gen_Induc_Base_Prefetch ()
{
  PF_SORTED_REFS* srefs = Sort_Refvecs (&_refvecs, _leading_ref);
  INT num = _refvecs.Elements()+1;

  mINT16 leader = 0; // starting index
  Gen_Induc_Base_Pref_Node (srefs, leader);

  for (mINT16 i=1; i<num; i++) {
    INT64 gap = (srefs[i].dist-srefs[leader].dist);

    PF_PRINT( fprintf(TFile, "Gen_Induc_Base_Prefetch PF_SORTED_REFS: dist %d, refnum %d, refvecnum %d, lrnum %d, gap %d, leader %d\n", 
              srefs[i].dist, srefs[i].refnum, srefs[i].refvecnum, 
              srefs[i].lrnum, gap, leader); );

    // Find the next leader which is equal to or more than one cache line
    // away from the previous leader.
    if (gap >= Cache.LineSize(1) ||
        gap <= (-1) * Cache.LineSize(1)) {
      leader = i;
      Gen_Induc_Base_Pref_Node (srefs, leader);
    }
  }

  CXX_DELETE_ARRAY (srefs, PF_mpool);
}

void PF_LG::Print (FILE *fp) {
  fprintf (fp, "        Locality group: (0x%p)\n", this);
  fprintf (fp, "          depth       : %d\n", _depth);
  fprintf (fp, "          leading ref : %d\n", _leading_ref);
  fprintf (fp, "          numlines: 1L %d, 2L %d\n",
           _numlines_1L, _numlines_2L);
  fprintf (fp, "          C.   Min. Max.\n");
  INT i;
  for (i=0; i<Get_Dim(); i++)
    fprintf (fp, "          %4lld %3lld %3lld\n", 
	_c[i], _min_iter[i], _max_iter[i]);
  fprintf (fp, "          Distance (bytes): Min %lld, Max %lld\n",
           _min_dist, _max_dist);
  fprintf (fp, "          References in this LG (%d) and their vecs\n",
           _refvecs.Elements()+1);
  for (i=0; i<_refvecs.Elements(); i++) 
    _refvecs.Bottom_nth(i)->Print (fp);
  fprintf (fp, "          Done with Locality group (0x%p)\n", this);
}

PF_UGS::PF_UGS (WN* wn_array, PF_BASE_ARRAY* myba) : _refs (PF_mpool) {
  INT num_loops = myba->Get_Depth()+1;
  INT i, j;
  FMAT *H;
  LU_FMAT *Hlu;
  
  _aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, wn_array);
  _refs.Push (wn_array);

  // construct the two access matrices
  H  = CXX_NEW (FMAT(_aa->Num_Vec(), num_loops, PF_mpool), PF_mpool);
  _Hs = CXX_NEW (FMAT(_aa->Num_Vec()-1, num_loops, PF_mpool), PF_mpool);

  INT indxs = _aa->Num_Vec ();
  for (i=0; i<indxs; i++) {
    for (j=0; j<num_loops; j++) {
      (*H)(i,j) = _aa->Dim(i)->Loop_Coeff(j);
      if (i != (indxs-1))
        (*_Hs)(i,j) = _aa->Dim(i)->Loop_Coeff(j);
    }
  }

  // now solve for locality in the two matrices
  Hlu = CXX_NEW (LU_FMAT(*H, PF_mpool), PF_mpool);
  _Hslu = CXX_NEW (LU_FMAT(*_Hs, PF_mpool), PF_mpool);
  
  _KerH = CXX_NEW(VECTOR_SPACE<FRAC>(*Hlu, PF_mpool), PF_mpool);
  _KerH->Beautify ();
  _KerHs = CXX_NEW(VECTOR_SPACE<FRAC>(*_Hslu, PF_mpool), PF_mpool);
  _KerHs->Beautify ();

  CXX_DELETE (H, PF_mpool); 
  CXX_DELETE (Hlu, PF_mpool);


  // now calculate stride-one loop
  PF_LOOPNODE* loopnode = myba->Get_Loop();
  INT maxdepth = loopnode->Get_Depth() + 1;
  ACCESS_ARRAY *aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, wn_array);
  ACCESS_VECTOR *av = aa->Dim(aa->Num_Vec()-1);

  // find the stride-one loop - innermost loop that is also in stride-one dim
  for (i=(maxdepth-1); i>=0; i--) {
    if (av->Loop_Coeff(i)) break;
    loopnode = loopnode->Get_Parent ();
  }
  _stride_one_loop = i; // -1 if no stride-one loop

  // Stride positive:
  // TRUE: if s-o-loop is forward and s-o-loop index has a +ve coeff in so-dim
  //   or  if s-o-loop is backward and s-o-loop index has a -ve coeff in so-dim
  // FALSE: if s-o-loop is forward and s-o-loop index has a -ve coeff in so-dim
  //     or if s-o-loop is backward and s-o-loop index has a +ve coeff
  
  _stride_forward = 1;  // default
  _stride_one_size = (mINT16) ABS(WN_element_size(wn_array)); // default
  if (i == -1) {
    // no stride one loop
    _stride_forward = 0;
    _stride_one_size = 0;
  }
  else { 
    DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
    if (!(dli->Step->Is_Const())) {
      // step is not a constant, so assume that step is +ve
      if (av->Loop_Coeff(i) < 0) _stride_forward = -1;
      _stride_one_size *= absof(av->Loop_Coeff(i));
    }
    else {
      // step is constant
      if (dli->Step->Const_Offset > 0) {
        // step is +ve
        if (av->Loop_Coeff(i) < 0) _stride_forward = -1;
      }
      else {
        // step is -ve
        if (av->Loop_Coeff(i) > 0) _stride_forward = -1;
      }
      _stride_one_size *= (absof(av->Loop_Coeff(i)*dli->Step->Const_Offset));
    }
  }

  // get stride in enclosing loop
  {
    _stride_in_enclosing_loop = 0;
#if defined(TARG_IA64)
    _stride_accurate = TRUE;
#endif

    ACCESS_ARRAY *aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, wn_array);

    // find the first array dimension (going from stride-one outwards)
    // that uses the index of the loop immediately enclosing the reference. 
    // (Ignore coupled subscripts)
    for (i=aa->Num_Vec()-1; i>=0; i--) {
      ACCESS_VECTOR *av = aa->Dim(i);
      if (av->Loop_Coeff(maxdepth-1)) break;
    }

    if (i >= 0) {
      // add up the stride in the dimensions inner to "i"
      _stride_in_enclosing_loop = (mINT16) ABS(WN_element_size(wn_array)); // default
      for (INT j=aa->Num_Vec()-1; j>i; j--) {
        WN* dim_wn = NULL;
        if (j < WN_num_dim(wn_array)) dim_wn = WN_array_dim(wn_array, j);
        if (dim_wn && WN_operator(dim_wn) == OPR_INTCONST) {
          // use array dimension size
          _stride_in_enclosing_loop *= WN_const_val(dim_wn);
        }
        else {
#if defined(TARG_IA64)
          //OSP_233 & OSP_240 
          //
          //  DO I = 1, N
          //    DO J = 1, N
          //      SUM = SUM + A(J, I)*B(I, J)
          //    END DO
          //  END DO
          //
          // Fortran's evil. If bound of a dimension, e.g. N, is not a constant,  
          // we can not compute how many bytes B(I,J) will go through in a J iteration. 
          // Assuming 100 as array dimension size will lead to inaccurate prefetch.
          // 
          _stride_accurate = FALSE;
#endif
          // randomly assume arraydimension size is 100
          _stride_in_enclosing_loop *= 100;
        }
      }
      // now handle the "i" dimension
      ACCESS_VECTOR *av = aa->Dim(i);
      PF_LOOPNODE* loopnode = myba->Get_Loop ();
      DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
      if (dli->Step->Is_Const()) {
        _stride_in_enclosing_loop *= (av->Loop_Coeff(maxdepth-1)*dli->Step->Const_Offset);
      }
      else {
        // assume step is 1
        _stride_in_enclosing_loop *= (av->Loop_Coeff(maxdepth-1));
      }
    }
    else {
#if defined(TARG_IA64)
      BOOL messy=FALSE;
    
      for (i=aa->Num_Vec()-1; i>=0; i--) {
        ACCESS_VECTOR *av = aa->Dim(i);
        if (av->Too_Messy || av->Contains_Non_Lin_Symb()) {
          messy = TRUE;
          break;
        }    
      }

      WN* wn_loop = LWN_Get_Parent(wn_array);
      while (wn_loop && WN_opcode(wn_loop) != OPC_DO_LOOP)
        wn_loop = LWN_Get_Parent(wn_loop);

      WN* wn_induction = WN_index(wn_loop);
      if(messy) {
        for(i=0; i<WN_num_dim(wn_array); ++i) {
          if( Contain_Induction_Variable(WN_array_index(wn_array, i), WN_st_idx(wn_induction)) ) {
            _stride_in_enclosing_loop = 100*ABS(WN_element_size(wn_array));
            _stride_accurate = FALSE;
            break;
          }
        }
      }
#endif

      // innermost loop doesn't appear at all, so probably temporal locality
      // do nothing
    }
  }


#ifdef PF_PrintDebugging
  if (Debug_Prefetch) {
    fprintf (TFile, "Just constructed a new UGS: 0x%p. symbol = ", this);
    myba->Get_Symbol()->Print (TFile); fprintf (TFile, "\n");
    fprintf (TFile, "_Hs is:    "); _Hs->Print (TFile);
    fprintf (TFile, "_Hslu is:  "); _Hslu->Print (TFile);
    fprintf (TFile, "_KerHs is: "); _KerHs->Print (TFile);
    fprintf (TFile, "Hsbasis is:"); _KerHs->Basis().Print (TFile);
    fprintf (TFile, "Basis vectors: %d, with locality/stride as follows:\n",
             _KerHs->D());
    /* for (i=0; i<_KerHs->D(); i++) 
     *      fprintf (TFile, "[%d] = %d, ", i, _stride[i]);
     */
    fprintf (TFile, "\n");
  }
#endif

  // one more than num of loops. Last one (num_loops+1) is
  // for base case when none of the loops in the nest is localized
  _lg = CXX_NEW_ARRAY (PF_LG_DA*, num_loops+1, PF_mpool);
  for (i=0; i<=num_loops; i++) {
    _lg[i] = NULL;
    // allocate lazily
    // CXX_NEW(PF_LG_DA(PF_mpool), PF_mpool);
  }
  _myba = myba;
}

PF_UGS::~PF_UGS () {
  INT depth = Get_Depth();
  
  for (INT i=0; i<=depth; i++) {
    if (_lg[i]) {
      while (_lg[i]->Elements()) CXX_DELETE (_lg[i]->Pop(), PF_mpool);
      CXX_DELETE (_lg[i], PF_mpool);
    }
  }
  CXX_DELETE_ARRAY (_lg, PF_mpool);
  
  // CXX_DELETE_ARRAY (_stride, PF_mpool);
  // CXX_DELETE (_H, PF_mpool);
  // CXX_DELETE (_Hlu, PF_mpool);
  CXX_DELETE (_Hs, PF_mpool);
  CXX_DELETE (_Hslu, PF_mpool);
  CXX_DELETE (_KerH, PF_mpool);
  CXX_DELETE (_KerHs, PF_mpool);
}

/***********************************************************************
 *
 * Build base locality group.
 *
 ***********************************************************************/
void PF_UGS::Build_Base_LGs () {
  WN* tref;
  PF_LG_DA* lglist;
  INT depth = Get_Depth()+1;
  
  Is_True ((_lg[depth] == NULL),
           ("Already processed this LG at depth %d\n", depth));

  _lg[depth] = lglist = CXX_NEW (PF_LG_DA(PF_mpool), PF_mpool);
  for (INT i=0; i<_refs.Elements(); i++) {
    tref = _refs.Bottom_nth(i);
    INT j;
    for (j=0; j<lglist->Elements(); j++) {
      if (lglist->Bottom_nth(j)->Add_Ref (tref, i))
        break;
    }
    if (j == lglist->Elements()) {
      // didn't find a match, create new LG
      // lglist->Push (CXX_NEW(PF_LG(tref, i, depth, this), PF_mpool));
      PF_LG* tmp_lg = CXX_NEW(PF_LG(tref, i, depth, this), PF_mpool);
      lglist->Push (tmp_lg);
    }
  }
}

void PF_UGS::Build_Induc_Base_LG () {
  WN* tref;
  PF_LG_DA* lglist;
  INT depth = Get_Depth()+1;  // innermost loop
  
  lglist = _lg[depth];
  tref = _refs.Bottom_nth(0);
  PF_LG* tmp_lg = CXX_NEW(PF_LG(tref, 0, depth, this), PF_mpool);
  lglist->Push (tmp_lg);

  for (INT i=0; i<_refs.Elements(); i++) {
    tref = _refs.Bottom_nth(i);
    tmp_lg->Add_Induc_Base_Ref (tref, i);
  }

  
}

/***********************************************************************
 *
 * Make non-base locality group, at given depth.
 * 
 * Since localized loops are based on single_iter, it is possible
 * that a loop be localized, yet lg's for that depth not be computed
 * since those are required only for total iter. This allows the lg's to
 * be computed later for prefetching (in Gen_Pref_Node).
 *
 ***********************************************************************/
void PF_UGS::BuildLG (mINT16 depth) {
  if (_lg[depth] == NULL) {
    // first make the locality groups for that loop level
    FmtAssert((_lg[depth+1] != NULL),
              ("Build LG: somehow previous LG missing!\n"));
    PF_LG_DA* cur_lglist = _lg[depth] = CXX_NEW (PF_LG_DA(PF_mpool), PF_mpool);
    // now start with the locality groups of the previous level
    // and create the new locality groups, perhaps merging
    for (INT i=0; i<_lg[depth+1]->Elements(); i++) {
      PF_LG* cur_lg = _lg[depth+1]->Bottom_nth(i);
      INT j;
      for (j=0; j<cur_lglist->Elements(); j++) {
        if (cur_lglist->Bottom_nth(j)->Add_Group
            (cur_lg, _refs.Bottom_nth(cur_lg->Get_LeadingRef()))) {
          Is_True (cur_lglist->Bottom_nth(j)->Check (), ("oops - error\n"));
          break;
        }
      }
      if (j == cur_lglist->Elements()) {
        // didn't find a merge, so create a new locality group
        cur_lglist->Push (CXX_NEW (PF_LG(cur_lg), PF_mpool));
      }
    }
  }
}

/***********************************************************************
 *
 * Compute the volume of this UGS for loop at given depth.
 * Basically calls each locality group at that depth.
 *
 ***********************************************************************/
PF_VOLUME PF_UGS::Volume (mINT16 depth) {

  PF_PRINT(fprintf (TFile, "Volugs[0x%p] depth (%d)\n",
                    this, depth););
  if (_lg[depth] == NULL) BuildLG (depth);
  PF_VOLUME myvol (0,0);
  
  for (INT i=0; i<_lg[depth]->Elements(); i++) {
    myvol += _lg[depth]->Bottom_nth(i)->Volume ();
  }
  PF_PRINT(fprintf (TFile, "Volugs[0x%p] depth (%d) vol ",
                    this, depth);
           myvol.Print (TFile);
  );
  return myvol;
}

/***********************************************************************
 *
 * if reference belongs to this UGS, 
 * then add it to _refs and return true.
 * if not, return false
 *
 ***********************************************************************/
BOOL PF_UGS::Add_Ref (WN* ref) {
  INT i, j;
  ACCESS_ARRAY *new_aa = (ACCESS_ARRAY*) WN_MAP_Get (LNO_Info_Map, ref);
  ACCESS_VECTOR *new_av, *myav;

  if (new_aa->Num_Vec() != _aa->Num_Vec()) return FALSE;
    
  for (i=0; i<new_aa->Num_Vec(); i++) {
    myav = _aa->Dim(i);
    new_av = new_aa->Dim(i);
    if (new_av->Nest_Depth() != myav->Nest_Depth()) return FALSE;
    for (j=0; j<new_av->Nest_Depth(); j++) {
      if (new_av->Loop_Coeff(j) != myav->Loop_Coeff(j)) return FALSE;
      /* array references may get delinearized, so that different references 
       * may have different factors in each dimension.
       * So, in addition to checking that the number of dimensions
       * match, check that they are to the same delinearized symbols,
       * if any.
       */
      if (new_av->Delinearized_Symbol != myav->Delinearized_Symbol)
        return FALSE;
    }
  }

  // if we reach here then the reference is in this UGS
  _refs.Push (ref);
  return TRUE;
}

#ifdef KEY //bug 10953
static BOOL Pseudo_Temporal_Locality(WN *array)
{
  WN *loop = LWN_Get_Parent(array);
  while(loop && WN_opcode(loop) != OPC_DO_LOOP)
     loop = LWN_Get_Parent(loop);
  if(loop == NULL)
    return FALSE;
  //temporalal locality is questionable for single loop
  //and stride(non-constant) may varies between executions
  //of the loop(NOT different iters!!!)
  //TODO: ...
#if (defined(TARG_X8664) || defined(TARG_IA64)) //bug 10953
  BOOL inductive_use = FALSE;
  BOOL indirect_use = FALSE;
  if(Simple_Invariant_Stride_Access(array, loop, TRUE,
                                    &inductive_use, &indirect_use))
    return TRUE;
#endif

  return FALSE;
}
#endif

/***********************************************************************
 *
 * Given the level, compute locality in the localized space,
 * and see what prefetches (if any) are required.
 * Compute the prefetch vector if spatial locality, into pfdesc for 
 * this UGS.
 *
 ***********************************************************************/
void PF_UGS::ComputePFVec (PF_LEVEL level, PF_LOCLOOP locloop) {
#if defined(TARG_IA64)
  if (!Get_Stride_Accurate())
    return;
#endif

  INT depth = Get_Depth ();
  INT linesize;
  mINT16 loopdepth;
  switch (level) {
  case level_1:
    loopdepth = locloop.Loop_1L ();
    linesize = Cache.LineSize(1);
    break;
  case level_2:
    loopdepth = locloop.Loop_2L ();
    linesize = Cache.LineSize(2);
    break;
  default:
    Is_True (FALSE, ("ComputePFVec for level one OR two\n"));
    break;
  }

  if (global_lvs[depth+1][loopdepth+1] == NULL)
    Allocate_Lvs (depth+1, loopdepth+1);
  Is_True (global_lvs[depth+1][loopdepth+1], ("global_lvs not initialized\n"));
  Is_True (global_lvs[depth+1][loopdepth+1]->N() == (depth+1),
           ("global_lvs has size %d, should be %d\n",
            global_lvs[depth+1][loopdepth+1]->N(), (depth+1)));
  Is_True (global_lvs[depth+1][loopdepth+1]->D() == (depth-loopdepth+1),
           ("global_lvs has %d vecs, should have %d vecs\n",
            global_lvs[depth+1][loopdepth+1]->D(), (loopdepth+1)));

  VECTOR_SPACE<FRAC>& lvs = *global_lvs[depth+1][loopdepth+1];
  // lvs is of size Get_Depth(), with locality starting at loop loopdepth

  VECTOR_SPACE<FRAC> sKerH (_KerH, PF_mpool);
  sKerH  *= lvs;

  PF_PRINT ({
    fprintf (TFile, "ugs    [0x%p]. Ref: ", this);
    Get_BA()->Get_Symbol()->Print (TFile);
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*)
      WN_MAP_Get (LNO_Info_Map,_refs.Bottom_nth(0));
    aa->Print (TFile);
  });

#ifdef KEY //bug 10953: temporal locality estimation may not be
           // due to various reasons(pointer access, lower-bound and
           // step changes, etc
  BOOL pseudo_temporal = TRUE;
  if(!LNO_Prefetch_Invariant_Stride)
      pseudo_temporal = FALSE;
  else
    for(INT kk=0; kk<_refs.Elements(); kk++){
      if(!Pseudo_Temporal_Locality(_refs.Bottom_nth(kk))){
         pseudo_temporal = FALSE;
         break;
      }
    }
#endif

  if ((level == level_1) ?
      locloop.While_Temporal_1L() : locloop.While_Temporal_2L()) {
    // there is while-temporal locality in localized loop, so don't prefetch
#ifdef KEY //bug 10953
   if(!pseudo_temporal){
#endif
     _pfdesc.Turn_Off (level);
     PF_PRINT(fprintf (TFile, "    while temporal locality (no prefetch)\n"));
#ifdef KEY
    }
#endif
    return;
  }

  if (sKerH.D()) {
    // there is temporal locality, so don't prefetch at all
    // before we do that, let's see if this is an outer tile loop
    PF_LOOPNODE* loopnode = Get_Loop ();
    INT dp = loopnode->Get_Depth (); /* depth of innermost do 
                                      * containing the reference.
                                      * _depth == depth of current loop.
                                      */
    INT not_loopdepth = loopdepth;  

    /* Not_Outermost_Tile_loopdepth.
     * serves same function as loopdepth (i.e. where we want locality)
     * but excludes outer-most outer-tile loops.
     *
     * A little complicated, but the basic idea is to exclude 
     * outer-tile loops since they mess up the matrix algebra and
     * show temporal locality.
     */

    PF_LOOPNODE* ln = NULL;
    while (1) {
      // start from current loop node
      loopnode = Get_Loop ();
      dp = loopnode->Get_Depth();

      // go outwards till the loop at not_loopdepth
      // (initiallly the loopdepth at which localized, but 
      // decreasing in subsequent iterations as we find outermost
      // outer-tile loops.
      while (dp != not_loopdepth) {
        dp--;
        loopnode = loopnode->Get_Parent();
      }
      DO_LOOP_INFO* dli = loopnode->Get_LoopInfo ();
      if (!dli->Is_Inner_Tile) {
        ln = Is_Outer_Tile (Get_Loop(), loopnode, Get_AA());
        if (ln) {
          // yes, it is an outer-tile loop.
          // so go to one-inner outer-loop, and continue.
          not_loopdepth++;
          continue; // the while (1) loop.
        }
        else break; // from the while (1) loop.
      }
      else break; // from the while (1) loop.
    }
    
    // at this point loopnode is the loop we want to consider for locality
    // and its depth is not_loopdepth.

    if (not_loopdepth > loopdepth) {
      // we did find an outer tile loop.

      // Compute the stride and spatial locality vector
      // but use not_loopdepth

      VECTOR_SPACE<FRAC> msKerHs (_KerHs, PF_mpool);
      if (global_lvs[depth+1][not_loopdepth+1] == NULL)
        Allocate_Lvs (depth+1, not_loopdepth+1);
      VECTOR_SPACE<FRAC>& mlvs = *global_lvs[depth+1][not_loopdepth+1];
      msKerHs *= mlvs;
      if (msKerHs.D() == 0) {
        _pfdesc.Turn_Off (level);
        return;
      }
      INT64 stride = 0;
      const MAT<FRAC>& msHsbasis = msKerHs.Basis ();
      ACCESS_VECTOR *av = _aa->Dim(_aa->Num_Vec()-1);
  
      INT i;
      for (i=0; i<=depth; i++)
        stride += msHsbasis(0, i).N() * av->Loop_Coeff(i);
      stride = abs(stride);
      if (stride == 0) {
        _pfdesc.Turn_Off (level);
        return;
      }
      // now stride is the increment (in elements) in stride-one dimension
      // find element size, are that many elements still within cache line?
      stride = stride * ABS(WN_element_size(_refs.Bottom_nth(0)));

        // check that it doesn't exceed cache line size
      if (stride > linesize) return;

      mINT16 trips = (mINT16) linesize / stride;
      // compute the prefetch vector -- stride times spatial locality vector
      mINT16* prefetch_vec = CXX_NEW_ARRAY (mINT16, depth+1, PF_mpool);
      for (i=0; i<=depth; i++) {
        Is_True (((i>=loopdepth) || (msHsbasis(0,i).N() == 0)),
                 ("Spatial locality vector in subnest spilt over\n"));
        prefetch_vec[i] = abs(msHsbasis(0,i).N()) * trips;
      }
      PF_PRINT(fprintf (TFile, "     level %d, spatial locality with vector: ",
                        level);
               for (INT i=0; i<depth+1; i++)
               fprintf (TFile, " %4d ", prefetch_vec[i]);
               fprintf (TFile, "\n"));
      _pfdesc.Turn_On (level, prefetch_vec, depth+1);
      return;
    }
#ifdef KEY //bug 10953
    if(!pseudo_temporal){
#endif
      _pfdesc.Turn_Off (level);
      PF_PRINT(fprintf (TFile, "    temporal locality with basis (no prefetch)\n");
             sKerH.Print (TFile));
#ifdef KEY
    }
#endif
    return;
  }
  // Else no temporal locality. Any spatial?
  VECTOR_SPACE<FRAC> sKerHs (_KerHs, PF_mpool);
  sKerHs *= lvs;
  if (sKerHs.D() == 0) {
    // no spatial locality either
    // Default value of _prefetch_p is already all
    PF_PRINT(fprintf (TFile, "    no spatial locality (prefetch all)\n"));
    return;
  }

  PF_PRINT(fprintf (TFile, "    spatial locality, basis is 0x%p\n", this);
           sKerHs.Print (TFile));
  // ahha -- there is spatial locality
  Is_True (sKerHs.D() == 1,
           ("Error in dimension of spatial locality subspace\n"));

  // Compute the stride and spatial locality vector
  INT64 stride = 0;
  VB_PRINT(printf ("** compute spatial locality vector and stride correctly! **\n"));
  const MAT<FRAC>& sHsbasis = sKerHs.Basis ();

  ACCESS_VECTOR *av = _aa->Dim(_aa->Num_Vec()-1);
  
  INT i;
  for (i=0; i<=depth; i++)
    stride += sHsbasis(0, i).N() * av->Loop_Coeff(i);
  stride = abs(stride);
  // now stride is the increment (in elements) in the stride-one dimension
  // find element size, see if that many elements are still within a cache line
  stride = stride * ABS(WN_element_size(_refs.Bottom_nth(0)));

  // check that it doesn't exceed cache line size
  if (stride > linesize) return;

  mINT16 trips = (mINT16) (linesize / stride);
  // compute the prefetch vector -- stride times spatial locality vector
  mINT16* prefetch_vec = CXX_NEW_ARRAY (mINT16, depth+1, PF_mpool);
  for (i=0; i<=depth; i++) {
    Is_True (((i>=loopdepth) || (sHsbasis(0,i).N() == 0)),
             ("Spatial locality vector in subnest spilt over\n"));
    prefetch_vec[i] = abs(sHsbasis(0,i).N()) * trips;
  }
  PF_PRINT(fprintf (TFile, "     level %d, spatial locality with vector: ",
                    level);
           for (INT i=0; i<depth+1; i++)
             fprintf (TFile, " %4d ", prefetch_vec[i]);
           fprintf (TFile, "\n"));
  _pfdesc.Turn_On (level, prefetch_vec, depth+1);
  PF_PRINT (_pfdesc.Print (TFile));
} /* ComputePFVec */

/***********************************************************************
 *
 * Return how this UGS would like the loops to be versioned.
 * If the prefetches desired in pfdesc are on some stride, then 
 * this UGS does desire some versioning.
 * Otherwise if the prefetches are all or none, then return an empty
 * split.
 *
 ***********************************************************************/
PF_SPLIT_VECTOR* PF_UGS::Find_Split_Vector () {
  mINT16* prefetch_vec;
  if (((Cache.Levels() > 1) && (prefetch_vec = _pfdesc.Vec(level_2))) ||
      ((Cache.Levels() == 1) && (prefetch_vec = _pfdesc.Vec(level_1)))) {

    // make sure it is a real split vector
    INT i;
    for (i=0; i<Get_Depth(); i++)
      if (prefetch_vec[i] > 1) break;
    if (i == Get_Depth()) {
      // all elements were 1 or 0. No split required.
      return NULL;
    }
    PF_SPLIT_VECTOR* split_vec = CXX_NEW (
      PF_SPLIT_VECTOR(Get_Depth()+1,
                      _pfdesc.Num_Lines (),
                      prefetch_vec,
                      Get_Loop()),
      PF_mpool);
    Is_True (!split_vec->Empty (), ("Just created an empty split_vec\n"));
    return split_vec;
  }
  return NULL;
}


/***********************************************************************
 *
 * Called to vote on loop splits once the localized loops have been identified.
 * It computes the locality vectors within the localized space,
 * And if there is spatial locality, then compute the count 
 * (in # of cache lines) for the locality vector.
 * This count is used later in choosing a loop-split vector.
 *
 ***********************************************************************/
void PF_UGS::Find_Loc_Space (PF_LOCLOOP locloop) {

  if (Cache.Levels() > 1) {
    if (locloop.Localized_1L()) ComputePFVec (level_1, locloop);
    if (locloop.Localized_2L()) ComputePFVec (level_2, locloop);
  }
  else {
    Is_True (locloop.Localized_1L(), ("Find_Loc_Space, but not localized\n"));
    ComputePFVec (level_1, locloop);
  }

  // Now do the voting -- how many references (misses, really) are 
  // there in this UGS.  count the refs and bump up the histograms.
  // Currently done based on 2L.
  mINT16 loopdepth;
  mINT16* prefetch_vec = NULL;
  mINT16 count = 0;

  // Do the voting only if spatial locality
  if ((Cache.Levels() > 1) && (prefetch_vec = _pfdesc.Vec(level_2))) {
    loopdepth = locloop.Loop_2L();
    // walk the prefetch vector, updating the counts for the desired strides
    // in each loop
    if (_lg[loopdepth] == NULL) BuildLG (loopdepth);
    for (INT i=0; i<_lg[loopdepth]->Elements(); i++)
      count += _lg[loopdepth]->Bottom_nth(i)->Lines (level_2);
  }
  else if ((Cache.Levels() == 1) && (prefetch_vec = _pfdesc.Vec(level_1))) {
    loopdepth = locloop.Loop_1L();
    // walk the prefetch vector, updating the counts for the desired strides
    // in each loop
    if (_lg[loopdepth] == NULL) BuildLG (loopdepth);
    for (INT i=0; i<_lg[loopdepth]->Elements(); i++)
      count += _lg[loopdepth]->Bottom_nth(i)->Lines (level_1);
  }

  if (prefetch_vec) _pfdesc.Set_Num_Lines (count);
}

/***********************************************************************
 *
 * Generate the prefetch instructions for the misses within this UGS. 
 *
 ***********************************************************************/
void PF_UGS::Gen_Prefetch (PF_SPLIT_VECTOR* split_vec) {
  if (!_pfdesc.Is_On()) return;

  INT i;
  PF_LG_DA* curlg;
  PF_LOCLOOP locloop = Get_Loop()->Get_locloop ();
  mINT16 loopdepth = Get_Depth() + 1;

  /* Generate prefetches for the cases of inductive base addresses only on 
     TARG_X8664 and TARG_IA64.
   */
#if (defined(TARG_X8664) || defined(TARG_IA64))
  if (_myba->Get_Inductive_Base()) {
    Build_Induc_Base_LG();
    PF_PRINT( fprintf(TFile, "Inductive base address case: printing UGS and LG\n");
              Print(TFile); );

    curlg = _lg[loopdepth];
    curlg->Bottom_nth(curlg->Elements() - 1)->Gen_Induc_Base_Prefetch ();
    return;
  }
#endif

  /* OK: We now have the way the loops were split (in split_vec),
   * and we have the desired prefetch vector (in _pfdesc).
   * We can now get arbitrarily smart in deciding how to prefetch.
   * (be careful -- the split_vec might be in another loop subnest,
   *  so find the common loops first)
   */

  /* Should at least try to do the right thing in the common case,
   * where it depends on only 1 loop variable, either innermost or
   * not.
   */

  /* TODO: Which locality groups do we use?
   * As such, different ones for first and second level caches.
   * In which case it becomes difficult to merge common prefetches
   * i.e. having just one prefetch node specifying both first and second
   * level prefetch.
   * Quite grungy to do this. For now just generate a separate prefetch
   * for first and second level. But, this can and should be improved.
   */
  // Do first then second level cache
  if (_pfdesc.Kind(level_1) != none) {
    if (locloop.Localized_1L ()) 
      loopdepth = locloop.Loop_1L();
    if (_lg[loopdepth] == NULL) BuildLG (loopdepth);
    curlg = _lg[loopdepth];
    for (i=0; i<curlg->Elements(); i++)
      curlg->Bottom_nth(i)->Gen_Prefetch (&_pfdesc, split_vec, level_1);
  }
  
  if (Cache.Levels() == 1) return;
  // Now do second-level
  if (_pfdesc.Kind(level_2) != none) {
    loopdepth = Get_Depth() + 1;
    if (locloop.Localized_2L())
      loopdepth = locloop.Loop_2L ();
    if (_lg[loopdepth] == NULL) BuildLG (loopdepth);
    curlg = _lg[loopdepth];
    for (i=0; i<curlg->Elements(); i++)
      curlg->Bottom_nth(i)->Gen_Prefetch (&_pfdesc, split_vec, level_2);
  }
}

void PF_UGS::Print (FILE* fp) {
  fprintf (fp, "      UGS/Access array: ");
  _aa->Print (fp);
  fprintf (fp, "        FMAT is: \n");      _Hs->Print (fp);
  fprintf (fp, "        LU_FMAT is: \n");   _Hslu->Print (fp);
  fprintf (fp, "        KerHsis: \n");      _KerHs->Print (fp);
  
  fprintf (fp, "        The references are (%d):\n", _refs.Elements());
  INT i;
  for (i=0; i<_refs.Elements(); i++) {
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*)
      WN_MAP_Get (LNO_Info_Map,_refs.Bottom_nth(i));
    fprintf (fp, "          [%d] 0x%p ", i, _refs.Bottom_nth (i));
    aa->Print (fp);
    fprintf (fp, "\n");
  }
  fprintf (fp, "        The locality groups are:\n");
  for (i=0; i<=(Get_Depth()+1); i++) {
    if (_lg[i] == NULL) continue;
    fprintf (fp, "          Group# %d\n", i);
    for (INT j=0; j<_lg[i]->Elements(); j++) {
      _lg[i]->Bottom_nth(j)->Print (fp);
    }
  }
}

PF_BASE_ARRAY::~PF_BASE_ARRAY () {
  CXX_DELETE (_array_base, PF_mpool);
  while (_ugs.Elements())
    CXX_DELETE (_ugs.Pop(), PF_mpool);
}

void PF_BASE_ARRAY::Build_Base_LGs () {
  for (INT i=0; i<_ugs.Elements (); i++) {
    _ugs.Bottom_nth(i)->Build_Base_LGs ();
  }
}

/***********************************************************************
 *
 * Return TRUE if the base of the array reference can be determined to
 * be steady within the current loop nest, FALSE otherwise.
 *
 ***********************************************************************/
extern BOOL Steady_Base (WN* wn_array) {
  Is_True (WN_operator(wn_array) == OPR_ARRAY,
           ("Steady_base must be called with an array node"));
  WN* load_wn = LWN_Get_Parent(wn_array);
  return (DEPV_COMPUTE::Base_Test(load_wn,NULL, load_wn,NULL) == DEP_CONTINUE);
}

/***********************************************************************
 *
 * Given a reference, add it to this base array into the appropriate UGS
 * (if it belongs here) and return TRUE, otherwise return FALSE. 
 * If do_check is FALSE then the check for "belonging" is NOT
 * performed. This is because the test can sometimes fail for 
 * a reference known to belong because of incomplete DU-chains.
 *
 ***********************************************************************/
BOOL PF_BASE_ARRAY::Add_Ref (WN* wn_array, BOOL do_check, BOOL induc_base) {
  if (do_check) {
    ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_array);
    // number of dimensions must be the same
    if (aa->Num_Vec() != _dim) return FALSE;

    ACCESS_ARRAY* sample_aa =
      (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, _sample_wn_array);
    for (INT i=0; i<_dim ; i++) {
      ACCESS_VECTOR *av = aa->Dim(i);
      ACCESS_VECTOR *sample_av = sample_aa->Dim(i);
      if (av->Delinearized_Symbol && sample_av->Delinearized_Symbol) {
        if (!(*av->Delinearized_Symbol == *sample_av->Delinearized_Symbol)) {
          return FALSE;
        }
      } else if (av->Delinearized_Symbol || sample_av->Delinearized_Symbol) {
        return FALSE;
      }
    }

    /* If they don't match, this ref cannot be in this PF_BASE_ARRAY. */
    if (induc_base != _inductive_base)  return FALSE;
      
    if (!induc_base) {
      switch (DEPV_COMPUTE::Base_Test(LWN_Get_Parent(wn_array),NULL,
                                    LWN_Get_Parent(_sample_wn_array),NULL)) {
      case DEP_CONTINUE:
        // add the reference
        break;
      case DEP_INDEPENDENT:
        // might be references to the same struct
      {
        if (Tree_Equiv(wn_array, _sample_wn_array)) {
          // yes, these are struct references
          break;
        }
        else {
          return FALSE;
        }
      }
      default:
        return FALSE;
      }
    } else {
      /* The dependence test does not give the precise info for the cases
         with inductive base addresses to accurately classify UGS.
       */
      WN *this_base_addr = WN_kid0(_sample_wn_array);
      WN *new_base_addr = WN_kid0(wn_array);
      /* If the base address sub-trees are identical, it is enough to put
         them under the same UGS since the index var and stride match already.
         we know the subscript expressions in all dim are constants 
         (possibly with different values. We don't care whether the field id 
         on the parent memory ops are the same or not. Those affect only the
         constant offsets, and they won't affect the classification of UGS
         though they will affect the partitioning of LGs.
       */
      if (!Tree_Equiv(new_base_addr, this_base_addr)) {
        return FALSE;
      }
    }
  }

#ifdef Is_True_On
  ACCESS_ARRAY* aa = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, wn_array);
  FmtAssert (aa->Num_Vec() == _dim, 
             ("mismatch -- %d != %d\n", aa->Num_Vec(), _dim));
#endif
  INT i;
  for (i=0; i<_ugs.Elements (); i++) {
    if (_ugs.Bottom_nth(i)->Add_Ref (wn_array)) break;
  }
  if (i == _ugs.Elements ()) {
    // didn't find a ugs for it, create new one
    PF_PRINT( fprintf(TFile,"creating a new UGS\n"); );
    _ugs.Push (CXX_NEW (PF_UGS(wn_array, this), PF_mpool));
  } else {
    PF_PRINT( fprintf(TFile,"adding into an existing UGS\n"); );
  }
  return TRUE;
}

PF_VOLUME PF_BASE_ARRAY::Volume (mINT16 depth) {
  PF_VOLUME myvol (0, 0);
  for (INT i=0; i<_ugs.Elements(); i++) {
    myvol += _ugs.Bottom_nth(i)->Volume(depth);
  }
  return myvol;
}

/***********************************************************************
 *
 * Called to process impact on strides of these base references
 * Called only when a localized loop has been identified,
 *
 ***********************************************************************/
void PF_BASE_ARRAY::Find_Loc_Space (PF_LOCLOOP locloop) {
  for (INT i=0; i<_ugs.Elements(); i++)
    _ugs.Bottom_nth(i)->Find_Loc_Space (locloop);
}

PF_SPLIT_VECTOR* PF_BASE_ARRAY::Find_Split_Vector () {
  PF_SPLIT_VECTOR* split_vec = NULL;
  for (INT i=0; i<_ugs.Elements(); i++) {
    PF_SPLIT_VECTOR* tmp = _ugs.Bottom_nth(i)->Find_Split_Vector  ();
    if (tmp) {
      if (split_vec) split_vec->Update(tmp);
      else split_vec = tmp;
    }
  }
  return split_vec;
}

void PF_BASE_ARRAY::Gen_Prefetch (PF_SPLIT_VECTOR* split_vec) {
  for (INT i=0; i<_ugs.Elements(); i++)
    _ugs.Bottom_nth(i)->Gen_Prefetch (split_vec);
}

void PF_BASE_ARRAY::Print (FILE* fp) {
  fprintf (fp, "Symbol : "); _array_base->Print (fp);
  if (_ugs.Elements() == 0)
    fprintf (fp, "    No uniformly generated sets\n");
  else {
    if (_inductive_base) {
      fprintf (fp, ", -induc_base,");
    }
    if (_indirect_base) {
      fprintf (fp, ", -indir_base,");
    }
    fprintf (fp, "    %d uniformly generated sets\n", _ugs.Elements());
    for (INT i=0; i<_ugs.Elements(); i++) {
      _ugs.Bottom_nth(i)->Print (fp);
    } 
  }
}
