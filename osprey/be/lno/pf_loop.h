/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
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
 *      Data structures for a do loop node
 *      ----------------------------------
 *
 * Exported Type:
 *
 *  PF_SPLIT_VECTOR
 *
 *      Store how loops got versioned.
 *
 * Exported Functions
 *
 *  PF_SPLIT_VECTOR ()
 *  
 *      Constructor. Initialize to NULL.
 *
 *  PF_SPLIT_VECTOR (mINT16 depth, 
 *                   mINT16 count, 
 *                   mINT16* vec, 
 *                   PF_LOOPNODE* loopnode)
 *
 *      Constructor given the various values.
 *
 *  BOOL Empty ()
 *
 *      Return whether splits are empty or not.
 *
 *  void Update (PF_SPLIT_VECTOR split_vec)
 *
 *      Given another split_vector, update this to be current best
 *      based on the one that is deepest, and with highest count.
 *      CXX_DELETE the incoming split_vec. ALWAYS.
 *
 *  PF_LOOPNODE* Get_Loop ()
 *
 *      Return the lowest (deepest) loop in the chosen version vector.
 *      This allows us to uniquely identify the loops to be versioned by 
 *      walking up the tree of loops from this leaf loop.
 *
 *  mINT16       Get_Depth ()
 *
 *      Return the depth of the split vector.
 *
 *  mINT16       *Get_Vector ()
 *
 *      Return the split/version vector.
 *
 *  void Print (FILE* fp)
 *
 *      Print the split vector.
 *
 *
 * Exported Type
 *
 *  PF_LOOPNODE
 *      A node corresponding to a do loop in the original code
 *      Contains a parent pointer to the closent enclosing do loop,
 *      and contains children pointers to the immediately nested 
 *      do loops within this do loop.
 *
 * Private Functions
 *
 *  void   Add_Ref (WN* wn_array)
 *
 *      Add a reference to this loop, if it is well-behaved.
 *
 *  void  Process_Refs (const WN* wn)
 *
 *      Walk the whirl code looking for array references,
 *      adding them to the references in this loop.
 *
 *  PF_VOLUME Volume ()
 *
 *      Compute the volume of data referenced in this loop.
 *
 *  PF_VOLUME Volume_For_Outer (mINT16 depth)
 *
 *      Find the volume of the references in this loop, but for an outer
 *      loop at the given depth.
 *
 *  void      Find_Loc_Loops (PF_LOCLOOP locloop)
 *
 *      locloop is the localized status of the outer loops. This routine 
 *      updates the localized status including this loop, and then 
 *      walks the localized loops  to compute the prefetch vectors within 
 *      the localized space. Also count the number of lines for the various 
 *      UGSs, that are then used to determine ultimate split.
 *
 *  PF_SPLIT_VECTOR*   Find_Split_Vector ()
 *
 *      Find the best split vector, based on deepest, with highest count.
 *
 * Exported Functions
 *
 *  PF_LOOPNODE(PF_LOOPNODE* parent, WN* code, mINT16 depth)
 *
 *      Constructor, given parent and actual do loop whirl tree
 *
 *  ~PF_LOOPNODE ()
 *
 *      Destructor -- delete all the children loops and the bases.
 *
 *  void Add_Child (PF_LOOPNODE *childnode)
 *
 *      add childnode as a child do loop
 *
 *  void Process_Refs ()
 *
 *      Process all the references within this do loop
 *
 *  void Process_Loop ()
 *
 *      First process all the references in this loop,
 *      then process the children (nested) loops
 *
 *  void  Build_Base_LGs ()
 *
 *      Build the base LGs for all base arrays, all UGSs
 *
 *  void  Process_PU_Volume ()
 *
 *      Do volume computation for all the nested loops.
 *
 *  DO_LOOP_INFO* Get_LoopInfo ()
 *
 *      get the do loop info
 *
 *  void  Gen_Prefetch (PF_SPLIT_VECTOR*)
 *
 *      Given how the loops were versioned, generate prefetches for all refs
 *      in this loop.
 *
 *  void  Process_Prefetch ()
 *
 *      Call Gen_Prefetch for all the children nodes in this PU.
 *
 *  void  Split_Loops  (PF_SPLIT_VECTOR* split_vec)
 *
 *      Given how loops should be versioned, actually do the versioning.
 *
 *  void  Process_Loc_Loops ()
 *  
 *      Given localized loops, for each loop in PU calculate the desired split.
 *
 *  mINT16   Get_Depth ()
 *
 *      Return the depth of this loop, outermost loop 0, PU is -1.
 *
 *  PF_LOOPNODE* Get_Parent ()
 *
 *      Return the parent of this loopnode
 *
 *  PF_LOCLOOP    Get_locloop ()
 *
 *      Return the info for localized loops stored in each loop.
 *
 *  void Print (FILE*)
 *
 *      standard print routine
 *
 *
 *
 *  mINT16 Loop_Confidence (DO_LOOP_INFO* dli)
 *
 *      Return confidence based on the given loop-info.
 *
 ***********************************************************************/


#ifndef pf_loop_INCLUDED
#define pf_loop_INCLUDED

#include "defs.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "wn.h"
#include "lnopt_main.h"
#include "pf_common.h"
#include "pf_cache.h"

class PF_BASE_ARRAY;
class PF_LOOPNODE;


class PF_LOOPNODE;

class PF_SPLIT_VECTOR {
  mINT16    _depth;         // depth of the split vector
  mINT16    _count;         // number of lines
  mINT16    *_vec;          // the split vector itself, of depth _depth
  PF_LOOPNODE* _loopnode;   /* the lowest level loopnode in the chosen split
                             * This is useful since there may be many loops
                             * at the same level. Storing the "leaf" loopnode 
                             * allows us to walk up the tree of loops, 
                             * identifying the ones that should get versioned.
                             */

  // PF_SPLIT_VECTOR (void);
  PF_SPLIT_VECTOR (const PF_SPLIT_VECTOR&);
  PF_SPLIT_VECTOR* operator= (const PF_SPLIT_VECTOR&);
public:
  PF_SPLIT_VECTOR () {
    _depth = _count = 0;
    _vec = NULL;
    _loopnode = NULL;
  }
  PF_SPLIT_VECTOR (mINT16 depth,
                   mINT16 count,
                   mINT16* vec,
                   PF_LOOPNODE* loopnode) {
    _depth = depth;
    _count = count; 
    _vec = vec;
    _loopnode = loopnode;
  }
  void Copy (PF_SPLIT_VECTOR* split_vec) {
    _depth = split_vec->_depth;
    _count = split_vec->_count;
    _vec = split_vec->_vec;
    _loopnode = split_vec->_loopnode;
  }
    
  BOOL Empty () {
    if (_vec == NULL) return TRUE;
    for (INT i=0; i<_depth; i++)
      if (_vec[i] > 1) return FALSE;
    return TRUE;
  }
  void Update (PF_SPLIT_VECTOR* split_vec) {
    if (split_vec == NULL) return;
    Is_True (split_vec->_vec,
             ("Split_vec: Update - got an empty split_vec\n"));
    // make sure that some split is required
    INT i;
    for (i=0; i<split_vec->_depth-1; i++)
      if (split_vec->_vec[i] !=  0) break;
    if (i == (split_vec->_depth-1)) {
      Is_True (FALSE, ("split_vec:Update - got an empty vector\n"));
      CXX_DELETE (split_vec, PF_mpool);
      return;
    }
    // ok, so this is a real split vector
    // if I am empty, update
    if (Empty()) {
      Is_True (FALSE, ("split_vec: update - why am i empty?\n"));
      return;
    }
    // these are the only real cases
    if (split_vec->_depth < _depth) {
      CXX_DELETE (split_vec, PF_mpool);
      return;
    }
    if ((split_vec->_depth > _depth) ||
        (split_vec->_count > _count)) {
      // use the new split vector
      Copy (split_vec);
      CXX_DELETE (split_vec, PF_mpool);
      return;
    }
  }
  PF_LOOPNODE* Get_Loop () const { return _loopnode; }
  mINT16       Get_Depth () const { return _depth; }
  mINT16       *Get_Vector () const { return _vec; }
  void Print (FILE* fp) {
    if (Empty()) 
      fprintf (fp, "Split vector is Empty\n");
    else {
      fprintf (fp, "Split Vector: depth - %d, count - %d, loopnode - 0x%p, Vector - ",
               _depth, _count, _loopnode);
      for (INT i=0; i<_depth; i++) fprintf (fp, " %3d ", _vec[i]);
      fprintf (fp, "\n");
    }
  }
};


typedef STACK<PF_LOOPNODE*> PF_LOOPNODE_DA;
typedef STACK<PF_BASE_ARRAY*> PF_BASE_ARRAY_DA;

class PF_LOOPNODE {
  PF_LOOPNODE       *_parent;       // pointer to parent loop in this graph
  PF_LOOPNODE_DA    _child;         // stack of pointers to children
  PF_BASE_ARRAY_DA  _bases;         // references directly within this loop
  WN                *_code;         // pointer to whirl node for this loop
  INT               _num_bad;
  mINT16            _depth;         // depth in loop nest (outermost loop is 0)
  mINT16            _volume_confidence;
                         // confidence in vol computation
                         // 3 == very high
                         // 2 == symbolic, but maxiters not too diff
                         // 1 == symbolic
                         // This value is basically minimum of values for
                         // each nested loop, and does NOT include "this" loop.
                         // This is coz localization depends on single-iter vol

  PF_VOLUME         _single_iter;   // volume (in bytes) in a single iteration
  PF_VOLUME         _total_iter;    // volume (in bytes) of all iterations
  INT               _manual_volume; /* volume (bytes) of manually prefetched
                                     * data
                                     */

  PF_LOCLOOP        _locloop;       // the localized loops
  mINT16            _split_num;     // splitting factor for this loop, if any
                                    // used just for verbose printing
  PF_SPLIT_VECTOR*  _split_vec;     // store the chosen versioning
  // private methods
  void              Add_Ref (WN* wn_array);
  void              Process_Refs (WN* wn);
  PF_VOLUME         Volume_For_Outer (mINT16 depth);
  PF_SPLIT_VECTOR*   Find_Split_Vector ();

  PF_LOOPNODE (void);
  PF_LOOPNODE (const PF_LOOPNODE&);
  PF_LOOPNODE* operator= (const PF_LOOPNODE&);
public:
  PF_LOOPNODE (PF_LOOPNODE *parent, WN *code, mINT16 depth) :
    _child(PF_mpool), _bases(PF_mpool) {
    _parent     = parent;
    _code       = code;
    _num_bad    = 0;
    _depth      = depth;
    _volume_confidence = 3;
    _manual_volume = 0;
    _split_vec  = NULL;
    _split_num  = 0;
  }
  ~PF_LOOPNODE ();
  void Add_Child (PF_LOOPNODE *childnode) { 
    _child.Push (childnode);
  }
  INT       Num_Children () { return _child.Elements(); }
  PF_LOOPNODE *Get_Child (INT i) { return _child.Bottom_nth(i); }
  void      Process_Loop ();
  void      Build_Base_LGs ();
  PF_VOLUME Volume ();
  PF_VOLUME Volume_Within_While (WN* while_wn);
  void      Find_Loc_Loops (PF_LOCLOOP locloop);
  void      Process_PU_Volume () {
    for (INT i=0; i<_child.Elements(); i++) _child.Bottom_nth(i)->Volume ();
  }

  DO_LOOP_INFO* Get_LoopInfo () {
    DO_LOOP_INFO* dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map, _code);
    FmtAssert(dli, ("Get_LoopInfo(): Unmarked do loop\n"));
    return dli;
  }

  void  Gen_Prefetch (PF_SPLIT_VECTOR*);
  void  Process_Prefetch () {
    for (INT i=0; i<_child.Elements(); i++)
      _child.Bottom_nth(i)->Gen_Prefetch(NULL);
  }
  void  Split_Loops  (PF_SPLIT_VECTOR* split_vec);
  void  Process_Loc_Loops () {
    PF_LOCLOOP tmp;
    for (INT i=0; i<_child.Elements(); i++) {
      _child.Bottom_nth(i)->Find_Loc_Loops (tmp);
    }
  }
  mINT16        Get_Depth ()  const { return _depth; }
  PF_LOOPNODE   *Get_Parent () const { return _parent; }
  PF_LOCLOOP    Get_locloop () const { return _locloop; }
  mINT16        Get_Confidence () const { return _volume_confidence; }
  PF_VOLUME     Get_Total () const { return _total_iter; }
  WN            *Get_Code () const { return _code; }
  void Print (FILE *fp);
  void Print_Structure ();
  void Print_Volume ();
  void Print_Splits ();
};

extern mINT16 Loop_Confidence (DO_LOOP_INFO* dli);
#if defined(TARG_X8664) || defined(TARG_IA64) //introduced by bug 10953
extern WN *Simple_Invariant_Stride_Access(WN *array, WN *loop, BOOL ck_induc_base,
           BOOL *inductive_use, BOOL *indirect_use);
#endif

#endif // pf_loop_INCLUDED
