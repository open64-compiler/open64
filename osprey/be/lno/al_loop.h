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
/* ====================================================================
 * ====================================================================
 *
 * Module: al_loop.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description:
 *
 *   Representation of loops containing references to reshaped arrays.
 *
 *  Exported Types:
 *
 *      ARRAY_LOWER_LOOP
 *
 *        Represents a loop that possibly contains references to reshaped arrays.
 *
 *
 *  Exported Functions:
 *
 *    ARRAY_LOWER_LOOP(ARRAY_LOWER_LOOP *parent, WN *doloop, mINT16 depth) 
 *     
 *      Constructor for ARRAY_LOWER_LOOP.  The "doloop" must be either a OPC_DO_LOOP
 *      or the OPR_FUNC_ENTRY.
 * 
 *   ~ARRAY_LOWER_LOOP(void)
 *
 *      Destructor for ARRAY_LOWER_LOOP.
 * 
 *    ARRAY_LOWER_LOOP *Parent(void) const 
 *
 *      Return the ARRAY_LOWER_LOOP enclosing this loop.  Returns NULL if this is
 *      the top level. 
 *      
 *    ARRAY_LOWER_LOOP *Get_Ancestor(INT n)
 * 
 *      Return the nth Parent of this loop.  If n <= 0, returns this loop.  If n
 *      is greater than the number of ancestors, returns the top level loop. 
 *
 *    WN *Doloop(void) const 
 *
 *      Return the do loop this represents.
 *
 *    mINT16 Depth(void) const 
 *      
 *      Return the nesting depth of this loop, numbered from outermost loops.  
 *      Note that the OPR_FUNC_ENTRY
 *
 *    LEGO_INFO *Get_Lego_Info(void);
 *
 *      Return the lego_info associated with this loop.
 *
 *    void Build_Loop(WN *curr_nd)
 *
 *      Walk the whirl tree rooted at curr_nd looking for uses of reshaped 
 *      arrays.  Builds up the ARRAY_LOWER_LOOPs and ARRAY_LOWER_REFs
 *
 *    void Process_Loop(void)
 *
 *       Lower all the references rooted at this loop.
 * 
 * ====================================================================
 * ====================================================================
 */
#ifndef al_loop_INCLUDED
#define al_loop_INCLUDED

#ifdef _KEEP_RCS_ID
static char *al_loop_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "wn.h"
#include "lego.h"
#include "lego_opts.h"
#include "al_ref.h"

class ARRAY_LOWER_LOOP;  // Forward reference

typedef STACK<ARRAY_LOWER_LOOP*> ARRAY_LOWER_LOOP_STACK;
typedef STACK<ARRAY_LOWER_REF *> ARRAY_LOWER_REF_STACK;

class ARRAY_LOWER_LOOP {
private:
  ARRAY_LOWER_LOOP *_parent;         // pointer to parent node
  ARRAY_LOWER_LOOP_STACK _children;  // pointers to children nodes
  WN *_doloop;                       // pointer to whirl node for the loop 
  mINT16 _depth;                     // depth in loop nest (outermost is 0)

  ARRAY_LOWER_REF_STACK _array_refs; // array references within this node

  ARRAY_LOWER_LOOP(void);
  ARRAY_LOWER_LOOP(const ARRAY_LOWER_LOOP&);
  ARRAY_LOWER_LOOP& operator=(const ARRAY_LOWER_LOOP&);

  void Add_Child(ARRAY_LOWER_LOOP *child) {
    _children.Push(child);
  }
  void Add_Ref(ARRAY_LOWER_REF *ref) {
    _array_refs.Push(ref);
  }

  void Build_Refs(WN *curr_nd);
  void Process_Refs(void);

public: 
  ARRAY_LOWER_LOOP(ARRAY_LOWER_LOOP *parent, WN *doloop, mINT16 depth) :
    _children(LEGO_pool), _array_refs(LEGO_pool) 
  {
    _parent = parent;
    _doloop = doloop;
    _depth = depth;
  }
  ~ARRAY_LOWER_LOOP(void);

  ARRAY_LOWER_LOOP *Parent(void) const { return _parent; }
  ARRAY_LOWER_LOOP *Get_Ancestor(INT n);

  WN *Doloop(void) const { return _doloop; }
  mINT16 Depth(void) const { return _depth; }

  LEGO_INFO *Get_Lego_Info(void);

  void Build_Loop(WN *curr_nd);
  void Process_Loop(void);
};

#endif /* al_loop_INCLUDED */

