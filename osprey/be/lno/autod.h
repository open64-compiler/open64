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
//                     Automatic Data Distribution
//                     ---------------------------
// Exported functions:
//
// void Automatic_Data_Distribute(WN *wn)
//
//
//


/**
*** $Source$
**/


#ifndef AUTOD_DECLARE

#define AUTOD_DECLARE


#include <sys/types.h>
#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif
#ifndef lwn_util_INCLUDED
#include "lwn_util.h"
#endif
#ifndef _lno_bv_INCLUDED
#include "lno_bv.h"
#endif
#ifndef bin_tree_INCLUDED
#include "btree.h"
#endif
#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif


void Automatic_Data_Distribute(WN *wn);
void Transpose_For_MP(WN *wn);

// the desired distribution for an array
class ARRAY_DESCRIPTOR
{
  mBOOL _is_bad;  // contradictory data distribution
  BIT_VECTOR *_parallel_dims; // which dims go in parallel
  ST *_st;

public:
  BOOL Is_Bad() const { return _is_bad; };
  BOOL operator <(const ARRAY_DESCRIPTOR &array) const {
    return (_st < array._st); 
  }
  BOOL operator >(const ARRAY_DESCRIPTOR &array) const {
    return (_st > array._st);
  }
  BOOL operator ==(const ARRAY_DESCRIPTOR &array) const {
    return (_st == array._st);
  }
  ARRAY_DESCRIPTOR &operator=(const ARRAY_DESCRIPTOR &array) {
    _st = array._st;
    _is_bad = array._is_bad;
    _parallel_dims = array._parallel_dims;
    return *this;
  }
  ARRAY_DESCRIPTOR() {;};
  ARRAY_DESCRIPTOR(ST *st, BIT_VECTOR *parallel_dims, BOOL is_bad) {
    _st = st;
    _parallel_dims = parallel_dims;
    _is_bad = is_bad;
  }
  void Union(ARRAY_DESCRIPTOR *ad)  {
    if (_is_bad) return;
    if (ad->_is_bad || !(*_parallel_dims == *ad->_parallel_dims)) {
      _is_bad = TRUE;
    }
  }
  void Set_Bad() { _is_bad=TRUE; };
  void Distribute_Array(WN *insertion_point);
};

typedef BINARY_TREE<ARRAY_DESCRIPTOR> ARRAY_DESCR_TREE;
typedef STACK<ST *> ARRAY_DESCR_STACK;

class DISTRIBUTION
{
  ARRAY_DESCR_TREE *_locals;
  ARRAY_DESCR_TREE *_globals;
  ARRAY_DESCR_STACK *_locals_stack;
  ARRAY_DESCR_STACK *_globals_stack;
  MEM_POOL *_pool;
  WN *_preamble;
  enum {epsilon = 5};
  void Gather_Arrays(WN *wn, BOOL seen_mp);
  void Process_Memory(WN *wn);
  void Distribute_Arrays();
  DOLOOP_STACK *_do_stack;
public:
  DISTRIBUTION(WN *wn, MEM_POOL *pool);
};


// Classes to do a transpose for mp
#define TRANSPOSE_MAX_SIZE 16

// a binary tree of local arrays along with indices into the graph
// and with the array dimension we want non-stride1
class ARRAY_TRANSPOSE_DESCRIPTOR
{
  ST *_st;
  INT _dimension;
  VINDEX16 _vertex;
  mBOOL _transposable;
public:
 
  BOOL operator <(const ARRAY_TRANSPOSE_DESCRIPTOR &array) const {
    return (_st < array._st); 
  }
  BOOL operator >(const ARRAY_TRANSPOSE_DESCRIPTOR &array) const {
    return (_st > array._st);
  }
  BOOL operator ==(const ARRAY_TRANSPOSE_DESCRIPTOR &array) const {
    return (_st == array._st);
  }
  ARRAY_TRANSPOSE_DESCRIPTOR &operator=(const ARRAY_TRANSPOSE_DESCRIPTOR &array) {
    _st = array._st;
    _vertex = array._vertex;
    _transposable = array._transposable;
    _dimension = array._dimension;
    return *this;
  }
  ARRAY_TRANSPOSE_DESCRIPTOR(ST *st) {
    _st = st;
    _transposable = TRUE;
    _vertex = 0;
    _dimension = -1;
  }
  ARRAY_TRANSPOSE_DESCRIPTOR(ST *st, BOOL transposable) {
    _st = st;
    _transposable = transposable;
    _vertex = 0;
    _dimension = -1;
  }
  ARRAY_TRANSPOSE_DESCRIPTOR() {;};
  VINDEX16 Get_Vertex() { return _vertex; };
  void Set_Vertex(INT i) { _vertex=i; };
  INT Get_Dimension() { return _dimension; };
  void Set_Dimension(INT value) { _dimension=value; };
  void Set_Transposable() { _transposable = TRUE; };
  void Reset_Transposable() { _transposable = FALSE; };
  BOOL Transposable() { return _transposable; };
};

typedef BINARY_TREE<ARRAY_TRANSPOSE_DESCRIPTOR> ARRAY_TRANSPOSE_TREE;

// A vertex represents either an array or an SNL
class TVERTEX : public VERTEX16 
{
public:
  INT size; // how many dims to the array, how many loops in the snl
  INT value; // our choice for which loop to go parallel, or which
	     // dim to go parallel
  BOOL is_loop;
  union {
    WN *inner_loop;
    ST *st;
  } tvertex_union;
  TVERTEX(WN *wn, INT size) { tvertex_union.inner_loop = wn; size = size; is_loop = TRUE; }
  TVERTEX(ST *st, INT size) { tvertex_union.st = st; size = size; is_loop = FALSE; }
  mBOOL can_be_parallel[TRANSPOSE_MAX_SIZE];  // can the loop be parallel
  friend class TRANSPOSE_DIRECTED_GRAPH16;
};

// an edge from an SNL to an array represents the contraints
// "if loop i of the SNL is parallel, then dim j of the array should be"
// "if dim i of the array is parallel then loop j of the SNL should be"
// -1 implies no constraint
class TEDGE : public EDGE16
{
public:
  INT  constraint[TRANSPOSE_MAX_SIZE];
  TEDGE() { ; };
  friend class TRANSPOSE_DIRECTED_GRAPH16;
};

class TRANSPOSE_DIRECTED_GRAPH16: public DIRECTED_GRAPH16<TEDGE,TVERTEX> {
  BOOL Outermore_Parallelizable(WN *wn);
  BOOL Contains_Parallelizable(WN *wn,INT nloops);
  BOOL _is_bad;  // give up
  void Gather_Arrays(WN *wn,ARRAY_TRANSPOSE_TREE *arrays);
  BOOL IO_element_read(WN *item);
  void Build_Snl(WN *inner, INT nloops, ARRAY_TRANSPOSE_TREE *arrays);
  void Build_Snl_Arrays(WN *outer,  ARRAY_TRANSPOSE_TREE *arrays,
			INT outer_depth,INT inner_depth,VINDEX16 snl_v);
  void Build_Snl_Array(WN *array, ARRAY_TRANSPOSE_TREE *arrays,
			INT outer_depth,INT inner_depth,VINDEX16 snl_v);
  BOOL Local_Array(ST *st);
  BOOL Propogate_V(VINDEX16 v);
  void Clear_Values();
  VINDEX16 Get_Loop_Vertex();
  BOOL _did_transpose;
  void Record(ARRAY_TRANSPOSE_TREE *arrays);
public:
  TRANSPOSE_DIRECTED_GRAPH16( mUINT16 num_v, mUINT16 num_e) :
    	DIRECTED_GRAPH16<TEDGE,TVERTEX>(num_v,num_e) {
    _is_bad = FALSE;
    _did_transpose = FALSE;
  }
  BOOL Did_Transpose() { return _did_transpose; };
  void Transpose(WN *wn,ARRAY_TRANSPOSE_TREE *arrays);
  void Transpose_Array(WN *array, INT dim); 
  void Transpose_Array(ST *st, INT dim); 
  void Build(WN *wn,ARRAY_TRANSPOSE_TREE *arrays);
  void Solve(ARRAY_TRANSPOSE_TREE *arrays);

  EINDEX16 Add_Edge(VINDEX16 from, VINDEX16 to, INT size) {
    EINDEX16 result =  DIRECTED_GRAPH16<TEDGE,TVERTEX>::Add_Edge(from,to);
    for (INT i=0; i<size; i++) {
      _e[result].constraint[i] = -1;
    }
    return result;
  }
  void Set_Constraint(EINDEX16 edge,INT index, INT val) {
    _e[edge].constraint[index] = val;
  }
  INT Get_Constraint(EINDEX16 edge,INT index) {
    return _e[edge].constraint[index];
  }
  VINDEX16 Add_Vertex(INT size, WN *loop) {
    VINDEX16 result = DIRECTED_GRAPH16<TEDGE,TVERTEX>::Add_Vertex();
    _v[result].tvertex_union.inner_loop = loop;
    _v[result].size = size;
    _v[result].is_loop = TRUE;
    return result;
  }
  VINDEX16 Add_Vertex(INT size, ST *st) {
    VINDEX16 result = DIRECTED_GRAPH16<TEDGE,TVERTEX>::Add_Vertex();
    _v[result].tvertex_union.st = st;
    _v[result].size = size;
    _v[result].is_loop = FALSE;
    return result;
  }
  mBOOL Can_Be_Parallel(VINDEX16 v, INT i) {
    return _v[v].can_be_parallel[i];
  }
  void Set_Can_Be_Parallel(VINDEX16 v, INT i) {
    _v[v].can_be_parallel[i]=TRUE;
  }
  void Reset_Can_Be_Parallel(VINDEX16 v, INT i) {
    _v[v].can_be_parallel[i]=FALSE;
  }
  void Print(FILE *fp);

};


#endif  // AUTOD_DECLARE

