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

/* This file includes the data structures corresponding to array references
 * within a loop. These data structures are organized hierarchically --
 *  - references in a loop are partitioned across different base-arrays.
 *  - Multiple references in a base-array are partitioned into their individual
 *    uniformly generated sets.
 *  - references within a UGS are partitioned into locality groups,
 *    with a set of locality groups for each loop in the loop-nest.
 *
 * The data structures are documented here, the main algorithms are described
 * at the beginning of pf_ref.cxx and within the code.
 *
 * Exported Types:
 *
 *  PF_REFVEC
 *
 *      Multiple references within a locality group are stored in a stack of
 *      PF_REFVECs. Each refvec stores the information required to relate a
 *      reference to the other references (specifically to the leading 
 *      reference) of the locality group.
 *      The fields that are stored within a PF_REFVEC are documented in the 
 *      class description.
 *
 * Exported Functions
 *
 *  PF_REFVEC (mINT16 refnum, mINT16 depth, FRAC* dvec, INT distance)
 *
 *      Constructor for a PF_REFVEC that takes a value for each of the 
 *      four fields.
 *
 *  PF_REFVEC (PF_REFVEC* refvec)
 *
 *      Constructor that takes a refvec, and makes a copy into the new refvec.
 *
 *  ~PF_REFVEC ()
 *
 *      Destructor for a refvec. Deallocated the _dvec.
 *
 *  void Update_dvec (FRAC* lr_dvec)
 *
 *      Basically subtracts the given dvec from stored dvec.
 *      Update the stored dvec relative to the supplied dvec of the new 
 *      leading reference relative to the old leading reference. 
 *      Called when the leading reference changes.
 *
 *  INT64 Distance ()
 * 
 *      Return the distance of this reference from the leading reference.
 *
 *  mINT16 Refnum ()
 *
 *      Return the reference number of this reference in the reflist in  UGS.
 *
 *  mINT16 Trips ()
 *
 *      Returns the trip separation from the leading reference in the 
 *      inner-most loop.
 *
 *  void Update_Distance (INT64 dist)
 *
 *      Subtract the supplied distance from the stored distance.
 *      Again useful when the leading reference changes, and the distance 
 *      is now relative to the new leading reference.
 *
 *  void Print (FILE* fp)
 *
 *      Print the contents of a refvec.
 *
 *
 * Exported Type
 *
 *  enum PF_KIND { all, none, vec}
 *
 *      Useful to store the kind of prefetching required for a reference.
 *      All means all reference should be prefetched,
 *      none means there is temporal locality and no reference should be 
 *      prefetched, vec means that there is spatial locality, and prefetches 
 *      should be done based on the vector in PF_DESC.
 *
 *  enum PF_LEVEL { level_1, level_2, level_1and2 }
 *
 *      Describes the cache level.
 *
 *  PF_DESC
 *
 *      This object describes the prefetch desired for each level of the cache.
 *      For each cache level, it stores the kind of prefetch and the prefetch 
 *      vector, if any. It also stores the number of cache lines that are 
 *      contained within this locality group (for the outer-level cache).
 *
 * Exported Functions
 *
 *  PF_DESC ()
 *
 *      Constructor. Initializes fields to NULL, initialize kind to "all".
 *
 *  ~PF_DESC ()
 *
 *  Destructor - deletes the two prefetch vectors, if any.
 *
 *  void Turn_Off (PF_LEVEL level)
 *
 *      Disable prefetching in the specified level of the cache.
 *
 *  void Turn_On (PF_LEVEL level, mINT16* pf_vec)
 *
 *      Enable prefetching with stride along supplied prefetch vector
 *
 *  void Set_Num_Lines (mINT16 numlines)
 *
 *      Set the number of lines in this prefetchi descriptor.
 *
 *  mINT16 Num_Lines ()
 *
 *      Read the number of lines in this descriptor.
 *
 *  PF_KIND Kind (PF_LEVEL level)
 *
 *      Return the kind of prefetching for the specified cache level.
 *
 *  mINT16* Vec (PF_LEVEL level)
 *
 *      Return the prefetch vector for the specified cache level.
 *
 *  BOOL Is_On ()
 *
 *      Return true if any prefetching for any level of the cache is turned on,
 *      either all or vec.
 *
 *
 * Exported Types
 *
 *  PF_LG
 *
 *      Locality group: bunch of references that could get spatical locality
 *      i.e. there is a solution to Ax = c1-c2, and the solution is contained 
 *      within the appropriate depth. Separation between references of greater 
 *      than a cache line doesn't matter --- that separation is made 
 *      individually for the two cache levels (with their difference 
 *      line-sizes) later.
 *
 * Private Functions
 *
 *  void  Split_LG ()
 *  
 *      Take the references in the locality group, sort them based on their 
 *      distance from the leading reference, and try to get a count of the 
 *      number of distinct lines in each level of the cache.
 *
 *  void  Gen_Pref_Node (PF_SORTED_REFS* srefs, mINT16 start, mINT16 stop,
 *                     PF_LEVEL level, PF_DESC*, PF_SPLIT_VECTOR*)
 *
 *      Actually generate the prefetch node for the references start through 
 *      stop-1 in the array srefs of sorted refs. level gives the level of 
 *      the cache to prefetch for. pfdesc gives the desired prefetch vector, 
 *      and split-vec gives the loop versioning that has happened.
 *
 *  INT Get_Bit_Vec (PF_DESC* pfdesc, 
 *                   PF_LEVEL level, 
 *                   PF_SPLIT_VECTOR* split_vec)
 *
 *      Given a desired prefetch vector in pfdesc, and the versioning vector in
 *      split_vec, this returns a bitvector that specifies which of the 
 *      strung-together references (through version_map) need be prefetched. 
 *      This is done based on a (currently) dumb comparison of pfdesc and 
 *      split_vec, and can be improved.
 *
 *  WN* Get_Ref_Version (WN* ref, INT bitpos)
 *
 *      Given the bitvector returned by Get_Bit_Vec, and given the first 
 *      reference, this routing returns the version of the reference that 
 *      should be prefetched based on bitpos.
 *
 *  INT64 Distance_LR (WN* ref, FRAC* dvec)
 *
 *      Given a ref and its dvec relative to leading reference, compute its
 *      distance in bytes from the leading reference.
 *
 * Exported Functions
 *
 *  PF_LG (PF_LG* lg)
 *
 *      Construct a locality group initialized to the supplied locality group.
 *
 *  PF_LG (WN* ref, mINT16 bitpos, mINT16 depth, PF_UGS* myugs)
 *
 *      Construct a locality group from scratch. Initialize it to the one 
 *      single reference, that is in the given bitposition in the UGS, the 
 *      locality group is for a loop at the given depth, and it belongs to the 
 *      given UGS.
 *
 *  ~PF_LG ()
 *
 *      Destructor for a locality group. Deallocates all the pf_refvecs 
 *      allocated within this locality group, as well as the _c (constant 
 *      vector for leading ref) vector.
 *
 *  FRAC* Ref_In_LG (WN* ref)
 *
 *      Given a ref, determine if this reference belongs to this locality 
 *      group or not. This basically involves finding a solution to Ax = c1-c2,
 *      seeing if a solution exists, and whether the solution vector is 
 *      contained within the desired loop depth.
 *      Returns a pointer to the dvec if true, otherwise it returns NULL.
 *
 *  BOOL Add_Ref (WN* ref, mINT16 bitpos)
 *
 *      Given a reference and its bitpos in the reflist of the UGS, determine 
 *      if the reference belongs to this locality group. If yes, then add it, 
 *      including updating data structures such as the spread (min/max iter), 
 *      distance (min/max bytes), etc. If this reference is not in this LG, 
 *      return FALSE.
 *
 *  BOOL Add_Group (PF_LG* lg, WN* lref)
 *
 *      Given a group and it's leading reference, determine if the incoming 
 *      group and the this group should be merged together. This is done based 
 *      on the dvec of the two leading references relative to each other. 
 *      If yes, then merge them together, otherwise return NULL>
 *
 *  BOOL Check ()
 *
 *      Check that refvecs within this LG has no duplicate references. 
 *      For debugging.
 *
 *  BOOL Check_Ref (mINT16 refnum)
 *
 *      Check that the given ref doesn't already exist in the refvecs for this 
 *      LG. For debugging.
 *
 *  PF_VOLUME Volume ()
 *
 *      Compute the volume of this locality group for the loop at depth _depth 
 *      that the LG is for. Volume is computed in bytes.
 *
 *  mINT16 Lines (PF_LEVEL level)
 *
 *      Return the number of cache lines in this LG at the given cache level.
 *
 *  mINT16 Get_Dim ()
 *
 *      Return the dimensionality of this array (stored in base array node).
 *
 *  mINT16 Get_Depth ()
 *
 *      Get the depth of the loop (outermost loop is 0).
 *
 *  LU_FMAT Get_Hslu ()
 *  
 *      Get the Hs in LU form for the UGS of this LG.
 *
 *  VECTOR_SPACE<FRAC>* Get_KerH ()
 *
 *      Get the kernel for H for the UGS of this LG.
 *
 *  VECTOR_SPACE<FRAC>* Get_KerHs ()
 * 
 *      Get the kernel for Hs for the UGS of this LG.
 *
 *  PF_LOOPNODE* Get_Loop ()
 *
 *      Get the loopnode for the references in this LG.
 *
 *  WN* Get_Ref (INT num)
 *
 *      Get this reference num from the reflist in the UGS.
 *
 *  mINT16 Get_LeadingRef ()
 *
 *      Return the refnum of the leading ref of the LG>
 *
 *  mINT16 Get_Stride_One_Loop ()
 *
 *      Find the inner-most loop that is also in the stride-one dimension 
 *      (in this UGS).
 *
 *  mINT16 Stride_Forward ()
 *
 *      Return 1 if this array reference is travelling forwards in memory
 *      based on stride-one loop, -1 if travelling backwards, 0 otherwise.
 *
 *  mINT16 Get_Stride_One_Size ()
 *
 *      Return the size in bytes travelled in one iteration of the 
 *      stride-one loop along the stride-one dimension.
 *
 *  mINT16 Get_Stride_In_Enclosing_Loop ()
 *
 *      Return the size in bytes travelled in one iteration of the 
 *      immediately enclosing loop. Useful for prefetch_ahead.
 *
 *  void Gen_Prefetch (PF_DESC* pfdesc, 
 *                    PF_SPLIT_VECTOR* split_vec, 
 *                    PF_LEVEL level);
 *
 *      Given the desired prefetch in pfdesc, and the way the loops are 
 *      versioned in split_vec, generate the prefetches for the appropriate 
 *      cache-level.
 *
 *  void Print (FILE* fp)
 *
 *      Print the locality group.
 *
 * 
 * Exported Types
 *
 *  PF_UGS
 *
 *      An object corresponding to a uniformly generated set --- i.e. all 
 *      references with the same index expression that differ only in the 
 *      constant terms in any dimension of the array.
 *
 * Exported Functions
 *
 *  PF_UGS (WN* wn_array, PF_BASE_ARRAY* myba)
 *
 *      Constructor for a UGS. Given an array node and the base array, 
 *      initialize the UGS.
 *
 *  ~PF_UGS ()
 *
 *      Destructor for a UGS. Deallocate locality group, H, Hs, KerH, KerHs.
 *
 *  BOOL Add_Ref (WN* ref)
 *
 *      Given a reference, add the reference if it belongs to this UGS,
 *      otherwise return FALSE.
 *
 *  void Build_Base_LGs ()
 *
 *      Build the base locality groups for a depth of (loopdepth+1), i.e. the 
 *      innermost level.
 *
 *  void Build_Induc_Base_LG ()
 *
 *      Build the locality group for the UGS with the base address of the
 *      associated ARRAY being inductive in the innermost level.
 *
 *  PF_VOLUME Volume (mINT16 depth)
 *
 *      Compute the volume (in bytes) for this UGS at the supplied loopdepth.
 *
 *  void Find_Loc_Space (PF_LOCLOOP locloop)
 *
 *      Given the localized loops in each cache level, find the desired 
 *      prefetch, and the prefetch vector (if spatial) for the references in 
 *      this UGS, into pfdesc. Also count the number of lines that are 
 *      contained within this UGS, store in pfdesc. Used later to determine 
 *      how to version loops.
 *
 *  PF_SPLIT_VECTOR* Find_Split_Vector ()
 *
 *      If this UGS has spatial locality prefetch vectors in its pfdesc, then
 *      return how this UGS would like the loops to be versioned.
 *
 *  void Gen_Prefetch (PF_SPLIT_VECTOR* split_vec)
 *
 *      Given how the loops were versioned, generate prefetches for this UGS.
 *
 *  LU_FMAT       *Get_Hslu ()
 *
 *      Return Hs in LU form.
 *
 *  VECTOR_SPACE<FRAC> *Get_KerH ()
 *
 *      Return the kernel of H
 *
 *  VECTOR_SPACE<FRAC> *Get_KerHs ()
 *
 *      Return the Kernel of Hs
 *
 *  PF_BASE_ARRAY *Get_BA ()
 *
 *      Return the base array of this UGS
 *
 *  ACCESS_ARRAY  *Get_AA ()
 *
 *      Return the access array of this UGS
 *
 *  mINT16    Get_Stride_One_Loop ()
 *
 *      Return the depth of the stride-one loop for this UGS
 *      (innermost loop that occurs in stride-one dimension of the array).
 *
 *  mINT16  Stride_Forward ()
 *
 *      Return 1 if this array reference is travelling forwards in memory
 *      based on stride-one loop, -1 if it is travelling backwards, 0 if not 
 *      going anywhere in stride one dimension.
 *
 *  mINT16 Get_Stride_One_Size ()
 *
 *      Return the size in bytes travelled in one iteration of the 
 *      stride-one loop along the stride-one dimension.
 *
 *  mINT16 Get_Stride_In_Enclosing_Loop ()
 *
 *      Return the size in bytes travelled in one iteration of the 
 *      immediately enclosing loop. Useful for prefetch_ahead.
 *
 *  WN  *Get_Ref (INT num)
 *
 *      Return the reference "num" in the list of references in this UGS
 *
 *  mINT16    Get_Depth ()
 *
 *      Return depth of the loop for this UGS.
 *
 *  PF_LOOPNODE*  Get_Loop ()
 *
 *      Return the loopnode for this UGS.
 *
 *  void  Print (FILE*)
 *
 *      Print this UGS.
 *
 *
 * Exported Type
 *
 *  PF_BASE_ARRAY
 *
 * Exported Functions
 *
 *  PF_BASE_ARRAY (SYMBOL* symb, mINT16 dim, PF_LOOPNODE* loopnode)
 *
 *      Constructor for a base array - copy the supplied fields.
 *
 *  ~PF_BASE_ARRAY ()
 *
 *      Destructor for the base array. Delete the array base, and all the UGSs.
 *
 *  SYMBOL    *Get_Symbol ()
 *
 *      Return the base symbol for this PF_BASE_ARRAY
 *
 *  BOOL      Add_Ref (WN* wn_array, BOOL do_check = TRUE)
 *
 *      Given a reference, add it to this base array into the appropriate UGS
 *      (if it belongs here) and return TRUE, otherwise return FALSE. 
 *      If do_check is FALSE then the check for "belonging" is NOT
 *      performed. This is because the test can sometimes fail for 
 *      a reference known to belong because of incomplete DU-chains.
 *      Default do_check is TRUE.
 *
 *  PF_VOLUME Volume (mINT16 depth)
 *
 *      Compute the volume of this base array (in bytes) for a loop at given 
 *      depth.
 *
 *  void      Build_Base_LGs ()
 *
 *      Build the base locality groups for all the UGSs in this base array.
 *
 *  void      Find_Loc_Space (PF_LOCLOOP locloop)
 *
 *  Given the localized loops, for each UGS compute the desired prefetch vec
 *      and the number of lines in each UGS. This is used to determine how to 
 *      version loops.
 *
 *  PF_SPLIT_VECTOR*   Find_Split_Vector ()
 *
 *      Having voted how to split, compute the best split of all UGSs 
 *
 *  void      Gen_Prefetch (PF_SPLIT_VECTOR*)
 *
 *      Having versioned the loops, generate prefetches for the UGSs in this 
 *      basearray.
 *
 *  mINT16    Get_Dim()
 *
 *      Return the dimensionality of this base array.
 *
 *  mINT16    Get_Depth ()
 *
 *      Return the loopdepth of these references to the base array.
 *
 *  PF_LOOPNODE* Get_Loop ()
 *
 *      Return the loopnode for these references.
 *
 *  void      Print (FILE*)
 *
 *      Print the base array.
 *
 * 
 * Exported Function
 *
 *  BOOL Steady_Base (WN* wn_array)
 *      Called with an array reference. Return TRUE if the base of the 
 *      array is known to be invariant within the current loop nest, 
 *      FALSE otherwise.
 *  
 */

#ifndef pf_ref_INCLUDED
#define pf_ref_INCLUDED

#include "defs.h"
#include "cxx_base.h"
#include "cxx_template.h"
#include "wn.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "pf_common.h"
#include "pf_cache.h"
#include "lu_mat.h"
#include "vs.h"

class SYMBOL;
class PF_REFVEC;
class PF_LG;
class PF_UGS;
class PF_BASE_ARRAY;
class PF_LOOPNODE;

typedef STACK<PF_REFVEC*> PF_REFVEC_DA;
typedef STACK<WN*> WN_DA;
typedef STACK<PF_LG*> PF_LG_DA;

class PF_REFVEC {
  // TODO: the following types can be unsigned
  mINT16    _refnum;     // number of this reference in the reflist of a UGS
  mINT16    _depth;     /* number of elements in _dvec,
                         * same as maxdepth of parent
                         */
  INT64     _distance;  // distance in bytes from leading reference
  FRAC      *_dvec;     /* distance vector of this ref,
                         * relative to leading ref of LG
                         */

  PF_REFVEC (void);
  PF_REFVEC (const PF_REFVEC&);
  PF_REFVEC* operator= (const PF_REFVEC&);
public:
  PF_REFVEC (mINT16 refnum, mINT16 depth, FRAC* dvec, INT64 distance) {
    _refnum = refnum;
    _depth = depth;
    _dvec = CXX_NEW_ARRAY (FRAC, _depth, PF_mpool);
    for (INT i=0; i<_depth; i++) {
      _dvec[i] = dvec[i];
    }
    _distance = distance;
  }
  PF_REFVEC (PF_REFVEC* refvec) {
    _refnum = refvec->_refnum;
    _depth = refvec->_depth;
    _dvec = CXX_NEW_ARRAY (FRAC, _depth, PF_mpool);
    for (INT i=0; i<_depth; i++) {
      _dvec[i] = refvec->_dvec[i];
    }
    _distance = refvec->_distance;
  }
  ~PF_REFVEC () {
    CXX_DELETE_ARRAY (_dvec, PF_mpool);
  }

  // update dvec relative to dvec of new leading ref
  void Update_dvec (FRAC* lr_dvec) {
    // subtract given lr_dvec from stored vec
    // Stored: d_lm, s.t. Ad_lm = C_l - C_m (l == leading ref, m == me)
    // want to store: d_l'm, s.t. Ad_l'm = C_l' - C_m
    // Ad_l'm = (C_l - C_m) - (C_l - C_l')
    // or Ad_l'm = Ad_lm - Ad_ll'
    // or d_l'm = d_lm - d_ll'
    // d_lm is stored here, d_ll' is given as input.
    // so subtract incoming vector from stored vector
    for (INT i=0; i<_depth; i++) {
      _dvec[i] = _dvec[i] - lr_dvec[i];
    }
  }
  INT64     Distance () const { return _distance; }
  mINT16    Refnum ()    const { return _refnum; }         
  FRAC*     Dvec ()     const { return _dvec; }
  // the following returns the trip separation in the 
  // innermost loop from leadingref
  mINT16    Trips ()    const { return _dvec[_depth-1].N(); }
  void  Update_Distance (INT64 dist) { _distance -= dist; }
  void  Print (FILE* fp) {
    fprintf (fp, "         Reference number: %hd. Distance %lld. Vector is\n",
             _refnum, _distance);
    for (INT i=0; i<_depth; i++) 
      _dvec[i].Print (fp);
    fprintf (fp, "\n");
  }
};

enum PF_KIND { all, none, vec };
enum PF_LEVEL { level_1, level_2, level_1and2 };

class PF_DESC {
  PF_KIND   _pf_p_1L;      // prefetch or not, what stride
  PF_KIND   _pf_p_2L;
  mINT16    *_pf_vec_1L;    // vector for prefetch, if any
  mINT16    *_pf_vec_2L;
  mINT16    _numlines;      /* number of lines that would miss if no prefetch.
                             * Based on the outermost cache level.
                             * Used for voting on loop splitting.
                             */
  mINT16    _depth;         // size of prefetch vectors
  // PF_DESC (void);
  PF_DESC (const PF_DESC&);
  PF_DESC* operator= (const PF_DESC&);
public:
  PF_DESC () {
    _pf_p_1L = all;
    _pf_p_2L = all;
    _pf_vec_1L = NULL;
    _pf_vec_2L = NULL;
    _numlines = 0;
    _depth = 0;
  }
  ~PF_DESC () {
    if (_pf_vec_1L) CXX_DELETE_ARRAY (_pf_vec_1L, PF_mpool);
    if (_pf_vec_2L) CXX_DELETE_ARRAY (_pf_vec_2L, PF_mpool);
  }
  void Turn_Off (PF_LEVEL level) { 
    switch (level) {
    case level_1:
      _pf_p_1L = none; 
      break;
    case level_2:
      _pf_p_2L = none;
      break;
    default:
      Is_True (FALSE, ("Turn_Off: broken level\n"));
    }
  }
  void Turn_On (PF_LEVEL level, mINT16* pf_vec, mINT16 depth) {
    switch (level) {
    case level_1:
      _pf_p_1L = vec; 
      _pf_vec_1L = pf_vec;
      break;
    case level_2:
      _pf_p_2L = vec;
      _pf_vec_2L = pf_vec;
      break;
    default:
      Is_True (FALSE, ("Turn_On: broken level\n"));
    }
    _depth = depth;
  }
  void Set_Num_Lines (mINT16 numlines) {  _numlines = numlines;  }
  mINT16  Num_Lines () const { return _numlines; }
  PF_KIND Kind (PF_LEVEL level) { 
    switch (level) {
    case level_1:
      return _pf_p_1L;
    case level_2:
      return _pf_p_2L;
    default:
      Is_True (FALSE, ("Kind: broken level\n"));
      return none;
    }
  }
  mINT16* Vec (PF_LEVEL level) const { 
    switch (level) {
    case level_1:
      return _pf_vec_1L;
    case level_2:
      return _pf_vec_2L;
    default:
      Is_True (FALSE, ("Vec: broken level\n"));
      return NULL;
    }
  }
  BOOL Is_On () {
    if (Cache.Levels() == 1) return (_pf_p_1L != none);
    return ((_pf_p_1L != none) || (_pf_p_2L != none));
  }
  void Print (FILE* fp) {
    INT i;
    fprintf (fp, "Printing pf descriptor (0 == all, 1 == none, 2 == vec)\n");
    fprintf (fp, "  1st level: kind %2d. ", _pf_p_1L);
    if (_pf_p_1L == vec)
      for (i=0; i<_depth; i++) fprintf (fp, " %3d ", _pf_vec_1L[i]);
    fprintf (fp, "\n");
    if (Cache.Levels() > 1) {
      fprintf (fp, "  2nd level: kind %2d. ", _pf_p_2L);
      if (_pf_p_2L == vec)
        for (i=0; i<_depth; i++) fprintf (fp, " %3d ", _pf_vec_2L[i]);
      fprintf (fp, "\n");
    }
    fprintf (fp, "  numlines = %d, _depth = %d\n", _numlines, _depth);
  }
};

struct PF_SORTED_REFS;
class PF_SPLIT_VECTOR;

class PF_LG {
  PF_REFVEC_DA  _refvecs;       // list of references and their dvecs
  mINT16        _depth;         // loop nesting level that this LG is for

  mINT16    _leading_ref;       // number, not bitvector
  INT64     *_c;                // constant vector for representative ref
  INT64     _min_iter[LNO_MAX_DO_LOOP_DEPTH];  // spread in the iteration space
  INT64     _max_iter[LNO_MAX_DO_LOOP_DEPTH]; 
  INT64     _min_dist;          // spread in bytes for this locality group
  INT64     _max_dist;          // (distance from leading reference)
  PF_UGS    *_myugs;            // pointer to my UGS
  mINT16    _numlines_1L;       // for Split_LG
  mINT16    _numlines_2L;

  void      Split_LG ();     // split if large spread
  void      Gen_Pref_Node (PF_SORTED_REFS* srefs, mINT16 start, mINT16 stop,
                         PF_LEVEL, PF_DESC*, PF_SPLIT_VECTOR*);
            /* Similar to the purpose of Gen_Pref_Node, but this deals with
               the cases where the base address of an ARRAY being inductive.
             */
  void      Gen_Induc_Base_Pref_Node (PF_SORTED_REFS* srefs, mINT16 srefnum);
  INT Get_Bit_Vec (PF_DESC* pfdesc,
                   PF_LEVEL level,
                   PF_SPLIT_VECTOR* split_vec);
  WN* Get_Ref_Version (WN* ref, INT bitpos);
  INT64 Distance_LR (WN* ref, FRAC* dvec);    /* Compute distance of ref
                                               * from leading ref.
                                               */
  INT64     Offset_to_Base_Addr(WN* ref); /* Compute the constant offset of
                                             ref from the base address for an
                                             inductive base addr cases.
                                          */
  INT       LR_Compare (mINT16 refvecnum1, mINT16 refvecnum2);
  void      LR_Ordering (PF_SORTED_REFS* srefs, INT start, INT stop);

  PF_LG (void);
  PF_LG (const PF_LG&);
  PF_LG* operator= (const PF_LG&);
public:
  // create a new locality group by unioning a given group with this
  PF_LG (PF_LG* lg);

  // create a new locality group with the given reference
  PF_LG (WN* ref, mINT16 bitpos, mINT16 depth, PF_UGS* myugs);

  ~PF_LG ();

  
  FRAC*     Ref_In_LG (WN* ref);      // check if reference is in this LG
  BOOL      Add_Ref (WN* ref, mINT16 bitpos);  // add a reference to PF_REFLIST
            /* This is similar to Add_Ref except that it adds a reference 
               to PF_REFLIST for an inductive base addr case.
             */
  BOOL      Add_Induc_Base_Ref (WN* ref, mINT16 bitpos);  
  BOOL      Add_Group (PF_LG* lg, WN* lref);   // add a locality group
  BOOL Check ();    // just check that there are no duplicates. For debugging.
  BOOL Check_Ref (mINT16 refnum);   /* just check that there are no duplicates.
                                    * For debugging.
                                    */

  // Given the UGS and the loop number,
  // compute the volume of this locality group
  // try not to do this repeatedly
  // need to redo this only when a new locality vecter 
  // gets added to the sKer in the loop subnest.
  PF_VOLUME     Volume ();
  mINT16        Lines (PF_LEVEL level) const { 
    switch (level) {
    case level_1:
      return _numlines_1L;
    case level_2:
      return _numlines_2L;
    default:
      Is_True (FALSE, ("Lines in PF_LG: pick a level\n"));
      return 0;
    }
  }
  mINT16        Get_Dim ();
  mINT16        Get_Depth ();
  LU_FMAT       *Get_Hslu ();
  VECTOR_SPACE<FRAC> *Get_KerH  ();
  VECTOR_SPACE<FRAC> *Get_KerHs ();
  // mINT16        *Get_Stride ();
  PF_LOOPNODE   *Get_Loop ();
  WN            *Get_Ref (INT num);
  mINT16        Get_LeadingRef () const { return _leading_ref; }
  mINT16        Get_Stride_One_Loop ();
  mINT16        Get_Stride_One_Size ();
  mINT32        Get_Stride_In_Enclosing_Loop ();
#if defined(TARG_IA64)
  BOOL           Get_Stride_Accurate();
#endif
  mINT16        Stride_Forward ();
  void          Gen_Prefetch (PF_DESC* pfdesc,
                             PF_SPLIT_VECTOR* split_vec,
                             PF_LEVEL level);
                /* This is similar to Gen_Prefetch() but it deals with the
                   inductive base addr cases.
                 */
  void		Gen_Induc_Base_Prefetch();
  void Print (FILE*);
};

class PF_BASE_ARRAY;

class PF_UGS {
  ACCESS_ARRAY*     _aa;    // access array for this ugs. 
                            // pointer to first ref that comes along
  // FMAT              *_H;
  // LU_FMAT           *_Hlu;
  FMAT              *_Hs;
  LU_FMAT           *_Hslu;

  VECTOR_SPACE<FRAC> *_KerH;
  VECTOR_SPACE<FRAC> *_KerHs;
  // mINT16            *_stride;

  // innermost loop that occurs in stride-one dimension. 
  // -1 if none.
  mINT16            _stride_one_loop;   
  // 1 if stride direction is +ve, -1 if -ve, 0 otherwise
  // i.e. travelling forward or backwards in memory
  mINT16            _stride_forward;
  // size in bytes travelled per iteration of stride-one loop
  // do i a[i]          --> (elem_size)
  // do i a[2*i]        --> (2*elem_size)
  // do i=..,..,2 a[i]  --> (2*elem_size)
  mINT16            _stride_one_size;
  // size in bytes travelled per iteration of immediately enclosing loop
  mINT32            _stride_in_enclosing_loop;

#if defined(TARG_IA64)
  // this variable is used to indicate whether the computation of 
  // _stride_in_enclosing_loop is accurate or is just a kind of estimation. 
  BOOL               _stride_accurate;
#endif
  
  WN_DA             _refs;      // list of references in this UGS
  PF_LG_DA          **_lg;      // list of lgs, one for each loop
  PF_BASE_ARRAY     *_myba;     // my base array
  PF_DESC           _pfdesc;

  void ComputePFVec (PF_LEVEL level, PF_LOCLOOP locloop);
  void BuildLG      (mINT16 depth);

  PF_UGS (void);
  PF_UGS (const PF_UGS&);
  PF_UGS* operator= (const PF_UGS&);
public:
  PF_UGS (WN* wn_array, PF_BASE_ARRAY* myba);
  ~PF_UGS ();
  BOOL                  Add_Ref (WN* ref);
  void                  Build_Base_LGs ();
                        /* Similar to Build_Base_LGs(), but this builds the 
                           locality group for the UGS with an inductive base 
                           address case.
                         */
  void                  Build_Induc_Base_LG ();
  PF_VOLUME             Volume (mINT16 depth);

  void                  Find_Loc_Space (PF_LOCLOOP locloop);
  PF_SPLIT_VECTOR*      Find_Split_Vector ();
  void                  Gen_Prefetch (PF_SPLIT_VECTOR*);

  LU_FMAT               *Get_Hslu () const   { return _Hslu; }
  VECTOR_SPACE<FRAC>    *Get_KerH ()    { return _KerH; }
  VECTOR_SPACE<FRAC>    *Get_KerHs ()   { return _KerHs; }
  // mINT16             *Get_Stride () { return _stride; }
  PF_BASE_ARRAY         *Get_BA ()   const { return _myba; }
  ACCESS_ARRAY          *Get_AA ()   const { return _aa;   }
  mINT16                Get_Stride_One_Loop () const { return _stride_one_loop; }
  mINT16                Get_Stride_One_Size () const { return _stride_one_size; }
  mINT32                Get_Stride_In_Enclosing_Loop () const { return _stride_in_enclosing_loop; }
#if defined(TARG_IA64)
  BOOL                   Get_Stride_Accurate ()    const { return _stride_accurate; }
#endif
  mINT16                Stride_Forward ()    const { return _stride_forward; }
  WN                    *Get_Ref (INT num)  const { return _refs.Bottom_nth(num); }
  mINT16                Get_Depth ();
  PF_LOOPNODE*          Get_Loop ();
  void      Print (FILE*);
};

typedef STACK<PF_UGS*> PF_UGS_DA;

class PF_LOOPNODE;

class PF_BASE_ARRAY {
  SYMBOL        *_array_base;
  WN            *_sample_wn_array; /* symbols aren't good enough, need 
                                    * a sample reference
                                    */
  PF_UGS_DA     _ugs;
  mINT16        _dim;       // dimension of this array
  PF_LOOPNODE   *_myloopnode;
  
  BOOL          _inductive_base;  // Is this a case where the index
                                  // expression in each dim is a 
                                  // constant but the base address
                                  // is inductive?
  BOOL          _indirect_base;   // Is this a case where the index
                                  // expression in each dim is a 
                                  // constant but the base address
                                  // contains an indirect but otherwise 
                                  // is inductive?

  PF_BASE_ARRAY (void);
  PF_BASE_ARRAY (const PF_BASE_ARRAY&);
  PF_BASE_ARRAY* operator= (const PF_BASE_ARRAY&);
public:
  PF_BASE_ARRAY (SYMBOL* symb, WN* wn_array, mINT16 dim, PF_LOOPNODE* loopnode,
                 BOOL inductive_base = FALSE, BOOL indirect_base = FALSE)
    : _ugs (PF_mpool) {
    _array_base = symb;
    _sample_wn_array = wn_array;
    _dim        = dim;
    _myloopnode = loopnode;
    _inductive_base = inductive_base;
    _indirect_base = indirect_base;
  }
  ~PF_BASE_ARRAY ();
  SYMBOL    *Get_Symbol () { return _array_base; }
  BOOL      Add_Ref (WN* wn_array, BOOL do_check = TRUE, 
                     BOOL induc_base = FALSE);
  PF_VOLUME Volume (mINT16 depth);
  void      Build_Base_LGs ();
  void      Find_Loc_Space (PF_LOCLOOP locloop);
  PF_SPLIT_VECTOR*   Find_Split_Vector ();
  void      Gen_Prefetch (PF_SPLIT_VECTOR*);

  mINT16    Get_Dim() const { return _dim; }
  BOOL      Get_Inductive_Base() const { return _inductive_base; }
  BOOL      Get_Indirect_Base() const  { return _indirect_base; }
  mINT16    Get_Depth ();
  PF_LOOPNODE* Get_Loop () const { return _myloopnode; }
  void      Print (FILE*);
};

extern BOOL Steady_Base (WN* wn_array);

#endif // pf_ref_INCLUDED
