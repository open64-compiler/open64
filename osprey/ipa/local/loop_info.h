/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
//---------------------------------------------------------------------------
// DO_LOOP_INFO_BASE
//
//      Loop representation, hold information about DO loops
//
// ACCESS_ARRAY _lb
//  
//           the lower bound is the max of all the access vectors in _lb
//
// ACCESS_ARRAY _ub
//
//           the upper bound is the min of all the access vectors in _ub
//
// mUINT8 Depth
//
//	What is the "depth" of this DO loop.  The outer DO loop
//	(good or bad) is numbered 0.  The next inner 1, etc.
//
// WN *Guard
//
//	Points to the if statement that "guards" this loop.
//	By guard we guarantee that the do loop is not
//	zero trip count.  If this is NULL, then the loop
//	is non-zero trip count without any special guard.
//
// IF_INFO
//	Contains information about block structured IFs
//	
//---------------------------------------------------------------------------

#ifndef loop_info_INCLUDED
#define loop_info_INCLUDED

#ifndef defs_INCLUDED
#include "defs.h"
#endif

#ifndef wn_INCLUDED
#include "wn.h"
#endif

#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif

#ifndef cxx_memory_INCLUDED
#include "cxx_memory.h"
#endif

#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif

#ifndef if_info_INCLUDED
#include "if_info.h"
#endif

#ifndef reduction_INCLUDED
#include "reduction.h"
#endif

extern WN_MAP IPL_info_map;
extern WN_MAP IPL_reduc_map;

class DO_LOOP_INFO_BASE;

typedef STACK<WN* > STACK_OF_WN;
typedef STACK<DO_LOOP_INFO_BASE *> DLI_BASE_STACK;
typedef STACK<IF_INFO *> IF_STACK;

#define IPL_LOOP_HAS_CALLS  1
#define IPL_LOOP_HAS_GOTOS  2
#define IPL_LOOP_IS_INNER  4

class DO_LOOP_INFO_BASE {
    
private:
  MEM_POOL *_pool;
  ACCESS_ARRAY *_lb;
  ACCESS_ARRAY *_ub;
  ACCESS_VECTOR *_step;
  INT32 _state;
  mUINT8 _depth;
  mINT16 _max_non_const_loop;
public:
  MEM_POOL *Pool() { return _pool; };

  DO_LOOP_INFO_BASE(MEM_POOL *pool) { 
    _lb = _ub =  NULL;
    _step = NULL;
    _pool = pool;
    _state = 0;
    _depth = 0;
    _max_non_const_loop = 0;
  };

  DO_LOOP_INFO_BASE(DO_LOOP_INFO_BASE *dli, MEM_POOL *pool);

  void Set_depth (mUINT8 d) { _depth = d;};
  mUINT8 Get_depth () const { return _depth;};

  void Set_has_calls() { _state = _state | IPL_LOOP_HAS_CALLS;};
  BOOL Has_calls() const { return _state & IPL_LOOP_HAS_CALLS;};
    
  void Set_has_gotos() { _state = _state | IPL_LOOP_HAS_GOTOS;};
  BOOL Has_gotos() const { return _state & IPL_LOOP_HAS_GOTOS;};
    
  void Set_is_inner_loop() { _state = _state | IPL_LOOP_IS_INNER;};
  BOOL Is_inner_loop() const { return _state & IPL_LOOP_IS_INNER;};
  void Reset_is_inner_loop() { _state = _state & ~IPL_LOOP_IS_INNER;};

  mINT16 Get_max_non_const_loop() const { return _max_non_const_loop;};
  void Set_max_non_const_loop(mINT16 l) { _max_non_const_loop = l;};

  void Set_lb(ACCESS_ARRAY *lb) { _lb = lb; };
  ACCESS_ARRAY* Get_lb() const { return _lb;};

  void Set_ub(ACCESS_ARRAY *ub) { _ub = ub;};
  ACCESS_ARRAY* Get_ub() const { return _ub;};
    
  void Set_step(ACCESS_VECTOR *step) { _step = step;};
  ACCESS_VECTOR* Get_step() const { return _step;};

#if defined(KEY) && defined(SHARED_BUILD)
  void Print(FILE *fp, INT = 0) __attribute__((weak));
#else
  void Print(FILE *fp, INT = 0);
#endif

  ~DO_LOOP_INFO_BASE() {
    CXX_DELETE(_lb,_pool);
    CXX_DELETE(_ub,_pool);
    CXX_DELETE(_step,_pool);
  }
    
};

// do we need to store control flow for this symbol?
extern
BOOL Record_scalar_flow(WN* stid);

extern void Mark_formal_summary_symbol(ST* s);
extern "C" void Print_DO_LOOP_INFO_BASE (FILE *fp, DO_LOOP_INFO_BASE *b);

#endif
