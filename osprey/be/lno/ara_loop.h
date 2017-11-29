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


// This may look like C code, but it is really -*- C++ -*-
//
// ========================================================================
// .NAME ARA_LOOP_INFO - a class for array def-use info of each loop nest
// .INCLUDE ara_loop_info.h
// .FILE ara_loop_info.h
// .FILE ara_loop_info.h ara_loop_info.inline.h ara_loop_info.cxx
//
// .SECTION Description
//
// This class contains array data flow information for a loop:
//      (1) Upwardly-exposed array use of the loop
//      (2) Kill (region must be defined by the loop)
//      (3) Private array regions of the loop
//
// Exported Type:
//
//    ARA_LOOP_INFO
//
// Exported Functions:
//
// ========================================================================

#ifndef _ara_loop_info_INCLUDED
#define _ara_loop_info_INCLUDED

#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef dep_graph_INCLUDED
#include "dep_graph.h"
#endif
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif
#ifndef _ara_region_INCLUDED
#include "ara_region.h"
#endif
#ifndef snl_INCLUDED
#include "snl.h"
#endif
#ifndef parallel_INCLUDED
#include "parallel.h" 
#endif
#ifndef wnmp_INCLUDED
#include "wn_mp.h"        // for REDUCTION_LIST
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif

extern BOOL Loop_Invariant_Access(WN* wn_array, const WN* loop);
extern BOOL Inside_Lego_Tiled_Loop(WN* wn_loop);

extern MEM_POOL ARA_memory_pool;     // mem pool
// class REGION;                        // forward declaration
// class REGION_UN;                     // forward declaration
class ARA_LOOP_INFO;                 // forward declaration

// A class to hold kernel matrices and their images
class KERNEL_IMAGE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(KERNEL_IMAGE);

  ACCESS_ARRAY * _kernel;
  REGION       * _region;
  INT16          _depth;
  INT16          _projected_level;
  BOOL           _decoupled;
  BOOL         * _is_independent;
  BOOL         * _changed;

public:
 
  KERNEL_IMAGE(KERNEL_IMAGE* k); 
  KERNEL_IMAGE(const ACCESS_ARRAY * a, ARA_LOOP_INFO *ara_info);
  KERNEL_IMAGE(const ACCESS_ARRAY * a);
  ~KERNEL_IMAGE();
  
//  void           Build_Region(ARA_LOOP_INFO &loop_info);
  REGION       * Region(){ return _region; }
  void		 Set_Region(REGION* r){ _region=r; }
  ACCESS_ARRAY * Get_Kernel(){ return _kernel; }
  INT16          Depth(){ return _depth; }
  INT16          Projected_Level() { return _projected_level; }
  void           Project(const INT i, const ARA_LOOP_INFO &ara_info);
  BOOL         * Get_Independent_Loops() { return _is_independent; }
//  BOOL         * Is_Changed() { return _changed; }

};

class KERNEL_LIST : public SLIST {
  DECLARE_SLIST_CLASS(KERNEL_LIST, KERNEL_IMAGE);

public:
  ~KERNEL_LIST(){
    while (!Is_Empty()) CXX_DELETE(Remove_Headnode(),&ARA_memory_pool);
  }

};

class KERNEL_SLIST_CONST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS(KERNEL_SLIST_CONST_ITER, KERNEL_IMAGE, KERNEL_LIST);
};

class KERNEL_SLIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(KERNEL_SLIST_ITER, KERNEL_IMAGE, KERNEL_LIST);
};

// ARA_REF contains a list of REGION_UN for a particular array

class ARA_REF {

  SYMBOL          *_array;      // symbol of the base array
  INT32           _offset;      // ILOAD/ISTORE offset
  REGION_UN       _image;       // accessed regions
  mBOOL           _has_bad_alias; // not privatizable because of aliasing
  mBOOL           _need_last_value; // live after the loop.
  mBOOL           _is_loop_invariant; // Is loop invariant.
  mBOOL           _donot_care_invariant; // 
  mBOOL           _is_unknown_size; 
  mBOOL		  _is_too_messy; // don't know anything about dims

public:

  ARA_REF():_image()
    {
      _array = NULL;
      _offset = 0;
      _has_bad_alias = FALSE;
      _need_last_value = TRUE;
      _is_loop_invariant = FALSE;
      _donot_care_invariant = FALSE;
      _is_unknown_size = FALSE;
      _is_too_messy = FALSE; 
    }

  ARA_REF(ARA_REF &a):_image()
    {
      _array = CXX_NEW(SYMBOL(a._array),&ARA_memory_pool);
      _offset = a._offset;
      _has_bad_alias = a.Has_Bad_Alias();
      _need_last_value = a.Need_Last_Value();
      _is_loop_invariant = a.Is_Loop_Invariant();
      _donot_care_invariant = a.Donot_Care_Invariant();
      _is_unknown_size = a.Is_Unknown_Size();
      _is_too_messy = a.Is_Too_Messy(); 
      REGION_CONST_ITER a_iter(&a._image);
      for (const REGION *cur = a_iter.First(); 
	   !a_iter.Is_Empty(); cur = a_iter.Next())
	_image.Append(CXX_NEW(REGION(*cur), &ARA_memory_pool));
    }

  ARA_REF(WN *array_wn, INT32 offset, ARA_LOOP_INFO *ali);
  ARA_REF(SYMBOL *array_sym, REGION* new_region,
		ARA_LOOP_INFO  *ali, BOOL is_invariant);

  ~ARA_REF()
    {
      if (_array) CXX_DELETE(_array, &ARA_memory_pool);
    }
  
  const SYMBOL & Array() { return *_array; }
  INT32      & Offset() { return _offset; }
  BOOL Has_Bad_Alias() { return _has_bad_alias; }
  BOOL Has_Formal_Parameter();
  BOOL Need_Last_Value() { return _need_last_value; }
  BOOL Is_Loop_Invariant() { return _donot_care_invariant 
    || _is_loop_invariant; }
  void Assign_Loop_Invariant(BOOL b) { _is_loop_invariant=b; }
  BOOL Donot_Care_Invariant() { return _donot_care_invariant; }
  void Set_Donot_Care_Invariant() { _donot_care_invariant=TRUE; }
  void Set_Whole_Array(BOOL set_invariant=TRUE);
  BOOL Is_Whole_Array() { return (_image.Head() && _image.Head()->Is_All()); }
  void Set_Bad_Alias() { _has_bad_alias = TRUE; }
  void Set_No_Last_Value() { _need_last_value = FALSE; }
  void Set_Need_Last_Value() { _need_last_value = TRUE; }
  void Set_Loop_Invariant(WN* loop);
  BOOL Is_Unknown_Size() { return _is_unknown_size; }
  void Set_Unknown_Size() { _is_unknown_size = TRUE; }
  BOOL Is_Too_Messy() { return _is_too_messy; }
  BOOL Is_Messy(); 
  void Set_Too_Messy() { _is_too_messy = TRUE; }
  REGION_UN & Image() { return _image; }
  void Add_Ref(ARA_REF *a, const ARA_LOOP_INFO &ali);
  void Print(FILE *fp) const;
  void WB_Print(FILE *fp) const;
  INT WB_Print(char* bf, INT ccount) const;
  void Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack);
};

// A list of ARA_REF, each element of the list is a pointer to
// an ARA_REF_ST.  Different element of the list may have different
// base arrays. 
// TODO: Make this a MAP indexed by St of the base array.
typedef STACK<ARA_REF*> ARA_REF_ST;

// A class to hold the array data flow information of a loop.
//   (1) the set of killed array regions
//   (2) the set of upwardly exposed use of array regions
//   (3) the set of private array regions

typedef STACK<ARA_LOOP_INFO*> ARA_LOOP_INFO_ST;
typedef HASH_TABLE<ST*,BOOL> S_HTABLE;

class ARA_LOOP_INFO {

  BOOL               _invariant;// current loop bounds and all its outer
                                // loop bounds are invariant
  ARA_LOOP_INFO_ST   _children; // list of inner loops
  ARA_LOOP_INFO     *_parent;   // pointer to outer loop info
  WN                *_loop;     // pointer to the current loop
  DO_LOOP_INFO      *_info;     // pointer to its info
  KERNEL_LIST       *_kernels;  // a list of kernels for WNs inside the loop
  DOLOOP_STACK      *_do_stack; // DOLOOP_STACK of enclosing loops
  STACK<WN*>        *_invariant_symbols; // a list of symbols invariant inside
                                        // the loop
  STACK<WN*>        *_processed; // temporaray list of symbols processed
  STACK<WN*>        _reduction; // reduction WN's
  BOOL 		    _inner_loop_is_suggested_parallel; 
				// TRUE if loop inside has DO_LOOP_INFO 
				// with Suggested_Parallel set.

  // Array region info in the loop
  BOOL              _has_bad_region; // for bad array references
  ARA_REF_ST        _def;      // kill set of array regions
  ARA_REF_ST	    _may_def;  // may def set of array regions 
  ARA_REF_ST        _use;      // upwardly-exposed use set of array regions
  ARA_REF_ST        _pri;      // private set of array regions

  // Scalar info in the loop
  SCALAR_STACK      _scalar_def;      // kill set of scalars
  SCALAR_STACK      _scalar_use;      // exposed use set of scalars
  SCALAR_STACK      _scalar_pri;      // private set of scalars
  SCALAR_STACK      _scalar_may_def;  // may modify set of scalars
  STACK<BOOL>       _scalar_last_value;   // private scalar needing last value;
  STACK<BOOL>       _bad_alias; // private scalar having bad alias;
  STACK<BOOL>       _scalar_always_defined; // private scalar always defined;

  // Dependence info for parallelization
  INT               _dep_dist; // dependence distance if it is good
  BOOL              _is_good; // dependence is good (+,- _dep_dist)
  BOOL              _has_last_value_array; // has at least one array needs last
                                           // value
  INT 		    _peel_value; // How much to peel to expose last iteration

  STACK<SYMBOL>     _scalar_vars;     // scalar variables having LCDs
  STACK<SYMBOL>     _scalar_alias;    // scalar variables having aliases 
  STACK<SYMBOL>     _scalar_no_final; // scalar variables having aliases 
  STACK<SYMBOL>	    _scalar_bad_peel; // could not get last value by peeling
  STACK<INT>	    _ln_scalar_bad_peel; // line number for the above 
  STACK<SYMBOL>     _dep_vars; // array variables having dependences
  STACK<SYMBOL>     _dep_source; // array variables having dependences
  STACK<SYMBOL>     _dep_sink; // array variables having dependences
  STACK<INT> 	    _ln_dep_source; // line number for the above 
  STACK<INT>	    _ln_dep_sink; // line number for the above 
  STACK<SYMBOL>     _dep_bad_peel; // could not get last value by peeling
  STACK<INT>        _ln_dep_bad_peel; // line number for the above 
  STACK<SYMBOL>     _partial_array_sec; // cannot privatize array section

  STACK<char*>	    _call_no_dep_vars; // calls without dependence info 
  STACK<INT>	    _ln_call_no_dep_vars; // line number for the above
  STACK<SYMBOL>	    _array_no_dep_vars; // calls without dependence info 
  STACK<INT>	    _ln_array_no_dep_vars; // line number for the above
  STACK<INT>	    _ln_misc_no_dep_vars; // line number for the above

  // Hash table for exposed uses
  S_HTABLE * _live_use; // uses that are exposed

  // Create the IF clause condition based on fp count and trip count
  WN* Create_Old_IF_Clause();
  BOOL Always_Enough_Parallel_Work(BOOL* has_left_right, INT* left,
    INT* right);
  float Tc_Parallel_Cost();
  float Tp_Parallel_Cost();
  WN* Create_New_IF_Clause(BOOL is_pdo);
  WN* Create_IF_Clause(BOOL is_pdo);
  float Const_Work_Estimate(WN* wn_loop, BOOL* minimum_only);

  // return list of store nodes for reductions
  void Reduction_List(REDUCTION_LIST *rlist);

public:

  ARA_LOOP_INFO();
  ARA_LOOP_INFO(ARA_LOOP_INFO* p);
  ARA_LOOP_INFO(WN* wn, ARA_LOOP_INFO *p, const BOOL inv);
  void Copy_Some_Values(ARA_LOOP_INFO *p);
  ~ARA_LOOP_INFO();
  BOOL     Inner_Loop_Is_Suggested_Parallel() 
    { return _inner_loop_is_suggested_parallel; }
  BOOL     Has_Bad_Region() { return _has_bad_region; }
  void     Set_Bad_Region() { _has_bad_region = TRUE; }
  BOOL     Has_Last_Value_Array() { return _has_last_value_array; }
  BOOL     Invariant_Bounds() const { return _invariant; }
  void     Add_Reduction(WN* wn) { _reduction.Push(wn); }
  STACK<WN*> & Reduction() { return _reduction; }
  ARA_LOOP_INFO_ST & Children() { return _children; }
  void     Add_Child(ARA_LOOP_INFO *child){ _children.Push(child); }
  void     Screen_Scalar_Conditional();
  const DO_LOOP_INFO * Info(){ return _info; }
  const WN     * Loop() const { return _loop; }
  DOLOOP_STACK & Do_Stack() { return *_do_stack; }
  ARA_LOOP_INFO * Parent() const { return _parent; }
  mUINT8  Depth() const { return (_info) ? _info->Depth: 0; }
  ARA_REF_ST & DEF() { return _def; }
  ARA_REF_ST & MAY_DEF() { return _may_def; }
  ARA_REF_ST & USE() { return _use; }
  ARA_REF_ST & PRI() { return _pri; }
  SCALAR_STACK & SCALAR_MAY_DEF() { return _scalar_may_def; }
  SCALAR_STACK & SCALAR_DEF() { return _scalar_def; }
  SCALAR_STACK & SCALAR_USE() { return _scalar_use; }
  SCALAR_STACK & SCALAR_PRI() { return _scalar_pri; }
  void Add_Def(ARA_REF *ara_ref);
  void Add_May_Def(ARA_REF *ara_ref);
  void Add_Use(ARA_REF *ara_ref);
  void Add_Pri(ARA_REF *ara_ref);
  void Remove_Array_Info();
  void Annotate_Invariant_Pri();
  void Annotate_Invariant_Def();
  void Projection();
  BOOL Variable_Load();
  BOOL Bounds_Depend_On_Index(INT depth);
  void Default_For_Bad_Loop(); // compute all the uses and considered them
                               // as exposed

  KERNEL_LIST & Kernels() { return *_kernels; }
  REGION & Iteration_Space();
  void     Set_Whole_Array();
  void     Add_Invariant(WN* wn) { _invariant_symbols->Push(wn);  }
  BOOL     Is_Invariant(const SYMBOL &sym) const 
    {
      for (INT i=0; i<_invariant_symbols->Elements(); ++i) 
	if (SYMBOL(_invariant_symbols->Bottom_nth(i))==sym)
	  return TRUE;

      return FALSE;
    }
  void     Add_Processed(WN* wn) { _processed->Push(wn);  }
  BOOL     Processed(const WN* wn)
    {
      for (INT i=0; i<_processed->Elements(); ++i) 
	if (SYMBOL(_processed->Bottom_nth(i))==SYMBOL(wn))
	  return TRUE;

      return FALSE;
    }
  void     Walk_Loop();
  BOOL     Is_Covered(WN *wn);
  BOOL     Is_Exposed(WN *wn)
    {
      return !Is_Covered(wn);
    }
  BOOL     Is_Covered(ARA_REF* ref);
  BOOL     Is_Exposed(ARA_REF* ref) 
    {
      return !Is_Covered(ref);
    }
  void Print(FILE *fp, BOOL terse = FALSE) const;
  void Print_Analysis_Info();
  void WB_Print(FILE *fp, BOOL terse = FALSE) const; 
  void CI_Print(FILE* fp);
  void Tlog_CI_Print();
  BOOL Is_Privatizable(WN* wn, BOOL definitely = TRUE);
  BOOL Dep_Is_Good() const { return _is_good; }
  INT  Dep_Dist() const { return _dep_dist; }
  void Add_Dependence(INT dist) 
    { 
      if (!_is_good) return;
      if (_dep_dist==0||_dep_dist==dist) {
	_dep_dist = dist;
      } else {
	_dep_dist = 0;
	_is_good = FALSE;
      }
    }
  BOOL Need_Copyin()
    {
      for (INT i = 0; i < _use.Elements(); ++i) {
	if (Overlap_Local_Array(_use.Bottom_nth(i)->Array()))
	  return TRUE;
      }
      return FALSE;
    } 
  BOOL Is_OK_Parallel() 
    {
      return (_info && !_info->Has_Exits && !_info->Has_Bad_Mem 
	      && (_is_good && _dep_dist==0 || _info->Is_Doacross) && 
	      Upper_Bound_Standardize(WN_end(_loop),TRUE)
	      && !_info->Pragma_Cannot_Concurrentize
	      && !_info->Serial_Version_of_Concurrent_Loop
	      && !_info->Inside_Critical_Section
	      && !_info->Has_Threadprivate
	      && !Inside_Lego_Tiled_Loop(_loop)
	      && !Need_Copyin() && _peel_value >= 0);
    }
  BOOL Is_Parallel() 
    {
      return (_info && !_info->Has_Exits && !_info->Has_Bad_Mem 
	      && (_is_good && _dep_dist==0 || _info->Is_Doacross)  
	      && Upper_Bound_Standardize(WN_end(_loop),TRUE)
	      && !Outermore_Parallel_Construct_Or_Lego_Loop(_loop)
	      && !(Innermore_Parallel_Or_Lego_Loop(_loop)
		&& !_info->Auto_Parallelized)
	      && !_info->Pragma_Cannot_Concurrentize
 	      && !_info->Serial_Version_of_Concurrent_Loop
	      && !_info->Inside_Critical_Section
	      && !_info->Has_Threadprivate
	      && !_inner_loop_is_suggested_parallel
	      && !Inside_Lego_Tiled_Loop(_loop)
              && !Need_Copyin() && _peel_value >= 0); 
    }
  void Set_To_Sequential()
    {
      _is_good = FALSE;
      _dep_dist = 0;
    }
  void Test_Alias();
  void Print_Loop_Property();
  S_HTABLE * Live_Use() { return _live_use; }
  void Create_Live_Use();
  void Delete_Live_Use() 
    { 
      if (_live_use!=NULL) CXX_DELETE(_live_use,&ARA_memory_pool);
      _live_use = NULL;
    }
  void Determine_Last_Value();
  void Determine_Peel(); 
  INT Peel_Value(); 
  void Generate_Parallel_Pragma();
  void Generate_Copyout_Loop();
  void Merge_then_else(ARA_LOOP_INFO *ara_then, ARA_LOOP_INFO *ara_else);
  void Merge_Info(ARA_LOOP_INFO *ali, BOOL seen_non_scf);
  void Walk_Block(WN *block_stmt);
  void Walk_Rhs(WN *wn, WN *skip_store_id = NULL);
  ARA_REF * Has_Matching(ARA_REF_ST &ara_s, ARA_REF *a);
  SCALAR_NODE * Has_Matching(SCALAR_STACK &st, SCALAR_NODE *sn);
  IF_INFO * Walk_If(WN *if_stmt);
  
  BOOL Def_Is_Whole_Array(const SYMBOL &sym, INT32 offset = 0)
    { 
      for (INT i = 0; i < _def.Elements(); ++ i)
	if (_def.Bottom_nth(i)->Array() == sym &&
	    _def.Bottom_nth(i)->Offset() == offset) {
	  return _def.Bottom_nth(i)->Is_Whole_Array();
	}
      return FALSE;
    }
  BOOL Overlap_Local_Array(const SYMBOL &sym, INT32 offset = 0)
    {
      for (INT i = 0; i < _pri.Elements(); ++ i)
	if (_pri.Bottom_nth(i)->Is_Loop_Invariant() &&
	    _pri.Bottom_nth(i)->Array() == sym &&
	    _pri.Bottom_nth(i)->Offset() == offset)
	  return TRUE;
      return FALSE;
    }

  BOOL Overlap_Local_Scalar(const SYMBOL &sym)
    {
      for (INT i = 0; i < _scalar_pri.Elements(); ++ i )
	if (_scalar_pri.Bottom_nth(i)->_scalar == sym)
	  return TRUE;
      return FALSE;
    }

  BOOL Overlap_Exposed_Array(const SYMBOL &sym, INT32 offset = 0)
    {
      for (INT i = 0; i < _use.Elements(); ++ i)
	if (_use.Bottom_nth(i)->Array() == sym &&
	    _use.Bottom_nth(i)->Offset() == offset)
	  return TRUE;
      return FALSE;
    }

  BOOL Overlap_Exposed_Scalar(const SYMBOL &sym)
    {
      for (INT i = 0; i < _scalar_use.Elements(); ++ i )
	if (_scalar_use.Bottom_nth(i)->_scalar == sym)
	  return TRUE;
      return FALSE;
    }

  BOOL Overlap_Kill_Scalar(const SYMBOL &sym)
    {
      for (INT i = 0; i < _scalar_def.Elements(); ++ i )
	if (_scalar_def.Bottom_nth(i)->_scalar == sym)
	  return TRUE;
      return FALSE;
    }

  BOOL Overlap_Reduction_Scalar(const SYMBOL &sym)
    {
      for (INT i = 0; i < _reduction.Elements(); ++ i ) {
	WN * red = _reduction.Bottom_nth(i);
	if (WN_operator(red) == OPR_STID || WN_operator(red) == OPR_LDID) {
	  SYMBOL symbol(red);
	  if (symbol == sym) return TRUE;
	}
      }
      return FALSE;
    }

  STACK<SYMBOL>& Scalar_Vars() { return _scalar_vars; }
  STACK<SYMBOL>& Scalar_Alias() { return _scalar_alias; }
  STACK<SYMBOL>& Scalar_No_Final() { return _scalar_no_final; }
  STACK<SYMBOL>& Dep_Vars() { return _dep_vars; }
  STACK<SYMBOL>& Dep_Source() { return _dep_source; }
  STACK<SYMBOL>& Dep_Sink() { return _dep_sink; }
  STACK<INT>& Ln_Dep_Source() { return _ln_dep_source; }
  STACK<INT>& Ln_Dep_Sink() { return _ln_dep_sink; }
  STACK<SYMBOL>& Scalar_Bad_Peel() { return _scalar_bad_peel; }
  STACK<INT>& Ln_Scalar_Bad_Peel() { return _ln_scalar_bad_peel; }
  STACK<SYMBOL>& Dep_Bad_Peel() { return _dep_bad_peel; }
  STACK<INT>& Ln_Dep_Bad_Peel() { return _ln_dep_bad_peel; }
  STACK<SYMBOL>& Partial_Array_Sec() { return _partial_array_sec; }
  STACK<char*>& Call_No_Dep_Vars() { return _call_no_dep_vars; }
  STACK<INT>& Ln_Call_No_Dep_Vars() { return _ln_call_no_dep_vars; }
  STACK<SYMBOL>& Array_No_Dep_Vars() { return _array_no_dep_vars; }
  STACK<INT>& Ln_Array_No_Dep_Vars() { return _ln_misc_no_dep_vars; }
  STACK<INT>& Ln_Misc_No_Dep_Vars() { return _ln_array_no_dep_vars; }
  BOOL Is_Problem_Scalar(WN* wn); 
  void Bad_Array_Dependence(WN* wn_source, WN* wn_sink); 
  BOOL Not_Enough_Parallel_Work();
};

extern void Walk_Loop_Dependence(WN * func_nd);

#endif
