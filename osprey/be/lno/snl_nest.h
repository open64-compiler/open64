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


// -*-C++-*-

/**
**/

/** $Revision$
*** $Date$
*** $Author$
*** $Source$
**/

#ifndef snl_nest_INCLUDED
#define snl_nest_INCLUDED "snl_nest.h"

#ifdef _KEEP_RCS_ID
static char *snl_nest_rcs_id = snl_nest_INCLUDED "$Revision$";
#endif /* _KEEP_RCS_ID */

class ACCESS_VECTOR;
class ACCESS_ARRAY;

#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef mat_INCLUDED
#include "mat.h"
#endif
#ifndef CXX_BASE_INCLUDED
#include "cxx_base.h"
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif
#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef lnopt_main_INCLUDED
#include "snl.h"
#endif
#include "sxlist.h"

//---------------------------------------------------------------------------
// class SNL_NEST_INFO
//---------------------------------------------------------------------------

enum SNL_LOOP_PROBLEM {
  SNL_LOOP_PROBLEM_NONE = 747,
  SNL_LOOP_PROBLEM_LOOP,               // e.g. bad upper or lower bound
  SNL_LOOP_PROBLEM_DISTRIBUTION,       // distribution illegal
  SNL_LOOP_PROBLEM_SCALAR,             // e.g. can't privatize
  SNL_LOOP_PROBLEM_INNER_MIGHT_NOT_GO, // e.g. general i = 1,N; where N unknown
  SNL_LOOP_PROBLEM_INNER_DOES_NOT_GO   // e.g. general i = 1,N; where N <= 0
};

struct SNL_LOOP_PROBLEM_INFO {
  SNL_LOOP_PROBLEM     Problem;
  WN*                  Wn;             // LOOP only
  SYMBOL               Var;            // SCALAR only
  SNL_LOOP_PROBLEM_INFO() : Problem(SNL_LOOP_PROBLEM_NONE) {}
  SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM prob) : Problem(prob) {}
};

class SNL_BOUNDS_INFO;

class SNL_NEST_INFO {

 public:

  SNL_NEST_INFO(WN* outer, INT nloops, MEM_POOL* pool, BOOL inner_only);
  SNL_NEST_INFO(WN* outer, WN* inner, INT nloops, MEM_POOL* pool);
  ~SNL_NEST_INFO();

  DOLOOP_STACK&		Dostack() {return _dostack;}
  const DOLOOP_STACK&	Dostack() const {return _dostack;}
  INT			Nloops() const {return _nloops;}
  INT			Nloops_General() const {return _nloops_general;}
  INT			Nloops_Invariant() const {return _nloops_invariant;}
  INT&			Nloops_Invariant() {return _nloops_invariant;}
  INT			Nloops_Transformable() const {return _nloops_transformable;}
  INT		       	Depth_Inner() const {return _depth_inner;}
  INT			Num_Bad() const {return _num_bad;}
  SNL_BOUNDS_INFO*	Bi() {return _bi;}
  const SNL_BOUNDS_INFO*Bi() const {return _bi;}
  BOOL			Above_Is_Distributable() const
    {return _above_is_distributable;}
  BOOL			Below_Is_Distributable() const
    {return _below_is_distributable;}
  MEM_POOL*		Pool() const {return _pool;}
  BOOL			Innermost() const {return _innermost;}
  void			Print(FILE*) const;
  void			Exclude_Outer_Loops(INT how_many);
  BOOL                  All_Var_Expandable(int nloops);

  SX_INFO& Privatizability_Info()
    {return _privatizability_info;}
  const SX_INFO& Privatizability_Info() const
    {return _privatizability_info;}

  SNL_LOOP_PROBLEM_INFO Problem(INT depth) const {
    return _problem == NULL ? SNL_LOOP_PROBLEM_INFO(SNL_LOOP_PROBLEM_NONE)
                            : _problem[depth];
  }

 private:

  void                  Make_Privatizability_Info_Handle_Def(WN*, INT);
  void                  Make_Privatizability_Info_Handle_Use(WN*, INT,
                                                       HASH_TABLE<WN*,BOOL>*);
  void                  Make_Privatizability_Info_Walk(WN*, INT,
                                                       HASH_TABLE<WN*,BOOL>*);
  void                  Make_Privatizability_Info();

  // undefined
  SNL_NEST_INFO(const SNL_NEST_INFO& i);

  INT			    _nloops;
  INT			    _num_bad;
  INT			    _depth_inner;
  MEM_POOL*		    _pool;
  DOLOOP_STACK		    _dostack;
  SX_INFO  		    _privatizability_info;

  // if constructor was inner_only and this is not true, then nothing
  // else in this data structure defined, except the _dostack and _pool.
  BOOL			    _innermost;

  // this is only interesting for invariant transformations
  INT			    _nloops_invariant;

  // these are only interesting for general transformations
  SNL_BOUNDS_INFO*	    _bi;
  INT			    _nloops_general;
  BOOL			    _above_is_distributable;
  BOOL			    _below_is_distributable;
  SNL_LOOP_PROBLEM_INFO*    _problem;
  
  // event aggretive transformations
  INT			     _nloops_transformable;
};

//---------------------------------------------------------------------------
// exported functions
//---------------------------------------------------------------------------

class FIZ_FUSE_INFO;

extern FIZ_FUSE_INFO* Emulate_Fiz_Fuse(WN* body, MEM_POOL* pool);

#endif
