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
// .NAME REGION - a class to hold array regions
// .INCLUDE ara_region.h
// .FILE ara_region.h
// .FILE ara_region.inline.h ara_region.cxx
//
// .SECTION Description
//
// Region is a simple class for holding a bounded convex region (polytope).
// It contains two parts (dual representations of polytope):
//
//     (1) Constraints:
//             [lower1:upper1:stride1]  --- constraint for dimension 1
//             [lower2:upper2:stride2]  --- constraint for dimension 2
//                 ...
//             [lowern:uppern:striden]  --- constraint for dimension n
//    
//         representing the inequality constraints for each dimensions.
//
//     (2) Vertices:
//             {(x1,x2, ..., xn)| all the vertices of the polytope}
//
//     (3) Conditions:
//             conditions for the region to be valid.
//
//     (4) Kernel:
//             kernel matrix of array occurrence for efficiency purpose.
//
// Exported Type:
//
//    REGION
//
// Exported Functions
//    
//    BOOL Is_Empty()
//          ------ Return TRUE if the region is empty.
//
//    BOOL Is_Included(const REGION &a)
//          ------ Test if 'a' is a subregion.
//
//    REGION * Region_Intersect(const REGION &a, const REGION &b)
//    REGION * Region_Intersect(const SLIST_REGION &list_of_regions)
//          ------ Return the intersection of regions.
//
//    REGION * Region_Union(const REGION &a, const REGION &b)
//    REGION * Region_Union(const SLIST_REGION &list_of_regions)
//          ------ If the union of regions is convex
//                    return the union
//                 else
//                    return NULL
//
//    REGION * Region_Projection(const WN &loop)
//          ------ Project away the loop index of &loop from the region.
//    
// =======================================================================

#ifndef _ara_region_INCLUDED
#define _ara_region_INCLUDED

#include <stdlib.h>
#ifndef defs_INCLUDED
#include "defs.h"
#endif
#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif
#ifndef soe_INCLUDED
#include "soe.h"
#endif
// #include "ara_loop.h"

extern MEM_POOL ARA_memory_pool;     // default mem pool
class KERNEL_IMAGE;                  // forward declaration
class KERNEL_LIST;                   // forward declaration
class ARA_LOOP_INFO;                 // forward declaration

typedef STACK<INT> INT_ST;

enum ARA_REGION_TYPE {
  ARA_TOP, ARA_BOTTOM, ARA_TOO_MESSY, ARA_NORMAL
};

//======================================================================
// This data structure hold one inequality constraint:
// (refer to AXLE_NODE for how various constraints are represented.)
// 
//  _coeff * X + _ac_v * Y  <= 0
//  ----- X is a vector representing the axles of a region
//  ----- Y is a vector representing the enclosing loop indices
//  ----- _coeff is a vector representing the relations among the
//        axles.  
//             If (_coeff == NULL), the region is decoupled.
//  ----- _ac_v is an access vector representing the coefficients
//        for loop indices, linear terms, and constants.
//======================================================================
class CON_PAIR {

public:
  
  ACCESS_VECTOR *_ac_v;
  INT32         *_coeff;

  CON_PAIR() { 
    _ac_v = NULL;
    _coeff = NULL;
  }

  CON_PAIR(CON_PAIR* c, INT dim);

  CON_PAIR(ACCESS_VECTOR *av) {
    _coeff = NULL;
    _ac_v = CXX_NEW(ACCESS_VECTOR(av,&ARA_memory_pool), &ARA_memory_pool);
  }

  CON_PAIR(ACCESS_VECTOR *av, INT* coeff, INT dim, MEM_POOL* mem_pool) { 
    if (coeff != NULL) {
      _coeff = CXX_NEW_ARRAY(INT, dim, mem_pool);
      for (INT i = 0; i < dim; i++) 
	_coeff[i] = coeff[i];
    } else {
      _coeff = NULL; 
    }
    _ac_v = CXX_NEW(ACCESS_VECTOR(av, mem_pool), mem_pool);
  }

  // create a CON_PAIR from inequality 'i' of 'soe' using symbols in 'syms'
  CON_PAIR(SYSTEM_OF_EQUATIONS *soe, const INT i, const SYMBOL_LIST *syms);

  CON_PAIR(const CON_PAIR &a, const INT dim);
    
  ~CON_PAIR() {
    if (_ac_v) {
      CXX_DELETE(_ac_v, &ARA_memory_pool);
      _ac_v = NULL;
    }
    if (_coeff) {
      CXX_DELETE_ARRAY(_coeff, &ARA_memory_pool);
      _coeff = NULL;
    }
  }

  BOOL Has_Formal_Parameter();
  ACCESS_VECTOR * Access_Vector(){ return _ac_v; }
  INT32         * Coeff() { return _coeff; }
  friend BOOL Equivalent(const CON_PAIR &a, const CON_PAIR &b, const INT dim);
  void Print(FILE *fp, INT dim) const;
  void WB_Print(FILE *fp, INT dim) const;
  INT WB_Print(char* bf, INT ccount, INT dim) const;
  void Print_Analysis_Info(FILE *fp, INT dim, DOLOOP_STACK &do_stack);

};

//============================================================================
// 
// A class to hold the constraints for one axle of an array.
// Representing 
//    lower <= axle <= upper, using:
//
//               -axle + lower <= 0
//                axle - upper <= 0
//
// If (up==NULL) 'lo' represents an equality: 
//
//                axle = lower
//
//============================================================================
class AXLE_NODE {

public:

                           // lower <= axle <= upper
  CON_PAIR      *lo;       // lower bound  [-axle + lower <= 0]
                           //                    ax1 ax2
                           // axle2 <= axle1    [ -1  1 ]

  CON_PAIR      *up;       // upper bound  [ axle - upper <= 0]
                           // coefficients between other axles
                           //                    ax1 ax2
                           // axle1 <= axle2    [  1 -1 ]

  INT16          step;     // step

  AXLE_NODE() {
    lo = up = NULL;
    step = 1;
  }

  AXLE_NODE(AXLE_NODE* a, INT dim);

  AXLE_NODE(const AXLE_NODE &a, const INT dim) {
    step = a.step;
    if (a.lo) lo = CXX_NEW(CON_PAIR(*a.lo,dim),&ARA_memory_pool);
    if (a.up) up = CXX_NEW(CON_PAIR(*a.up,dim),&ARA_memory_pool);
  }

  ~AXLE_NODE() {
    if (lo) CXX_DELETE(lo, &ARA_memory_pool);
    if (up) CXX_DELETE(up, &ARA_memory_pool);
  }

  BOOL Has_Formal_Parameter(); 
  void Clear();

  // Set axle according to system of equations and symbols
  void Set_Axle(const SYSTEM_OF_EQUATIONS *soe,
		const INT i, const INT j, const SYMBOL_LIST *syms,
		const INT depth, const INT dim,
		const INT_ST & non_const_loops,
		const INT stride);

  void Set_Axle_Eq(const SYSTEM_OF_EQUATIONS *soe,
		   const INT i, const INT j, const SYMBOL_LIST *syms,
		   const INT depth, const INT dim,
		   const INT_ST & non_const_loops,
		   const INT stride);

  void Set_Axle_Le(const SYSTEM_OF_EQUATIONS *soe,
		   const INT i, const INT j, const SYMBOL_LIST *syms,
		   const INT depth, const INT dim,
		   const INT_ST & non_const_loops,
		   const INT stride);

  void Set_Axle(const CON_PAIR *lo_new, const CON_PAIR *up_new, 
		const INT16 step_new, const INT dim);
  void Set_To_Kernel_Image(const AXLE_NODE &a, const INT dim, const INT kernel_offset);
  void Init_To_Access(ACCESS_VECTOR *av);
  
  friend BOOL Equivalent(const AXLE_NODE &a, 
			 const AXLE_NODE &b, const INT dim);

  void Print(FILE *fp, INT dim) const;
  void WB_Print(FILE *fp, INT dim) const;
  INT WB_Print(char* bf, INT ccount, INT dim) const;
  void Print_Analysis_Info(FILE *fp, INT dim, INT indent, DOLOOP_STACK &do_stack);

};

//======================================================================
// 
// A class to hold a convex region.  It is an array of AXLE_NODE where
// each AXLE_NODE representing a convex region for each axle. 
// 
//======================================================================
class REGION : public SLIST_NODE {

public:

  INT32                 _dim;         // Number of dimensions of the region
  AXLE_NODE            *_axle;        // Each axle's constraint and stride
  mUINT16               _depth;       // Nesting depth.
  ARA_REGION_TYPE       _type;        // Special region type
  BOOL                  _coupled;     // 
  ACCESS_ARRAY         *_conditions;  
  KERNEL_IMAGE         *_kernel;
  STACK<WN*>            _wn_list;     // list of WNs associated to the region

  // Create a region for array access node wn
  REGION(WN* wn, ARA_LOOP_INFO *ara_loop_info);
  REGION(WN* wn, ACCESS_ARRAY* array);
  REGION(const REGION &a);
  REGION(REGION* r);
  REGION(const INT depth, const INT dim):_wn_list(&ARA_memory_pool)
    {

      _type         = ARA_TOO_MESSY;
      _axle         = NULL;
      _conditions   = NULL;
      _kernel       = NULL;
      _depth        = depth;
      _dim          = dim;
      
    }  

  ~REGION() {

    if (_axle != NULL) CXX_DELETE_ARRAY(_axle, &ARA_memory_pool);
    if (_conditions != NULL) CXX_DELETE(_conditions, &ARA_memory_pool);

  }

  BOOL Has_Formal_Parameter();
  INT16 Num_Dim() { return _dim; }
  AXLE_NODE & Dim(INT32 i) { return _axle[i]; }
  void Set_Too_Messy() { _type = ARA_TOO_MESSY; }
  const KERNEL_IMAGE * Kernel(){ return _kernel; }
  BOOL Is_Too_Messy() const { return _type==ARA_TOO_MESSY; }
  BOOL Is_Empty() const { return _type==ARA_BOTTOM; }
  BOOL Is_All() const { return _type==ARA_TOP; }
  BOOL Is_Included(const REGION &a, const ARA_LOOP_INFO &ara_info);
  BOOL Is_Coupled() const { return _coupled; }
  BOOL Contains(WN* array_wn);
  BOOL Is_Loop_Invariant(WN *loop);
  REGION & Region_Projection(const INT pos, const ARA_LOOP_INFO &ara_info);
  void  Set_Region(const SYSTEM_OF_EQUATIONS * soe, 
		   const SYMBOL_LIST * syms,
		   const INT_ST & non_const_loops,
		   const INT strides[]);

  void Set_Region(const SYSTEM_OF_EQUATIONS * soe,
		  const SYMBOL_LIST * syms,
		  const INT_ST & non_const_loops,
		  INT strides[],
		  const INT pivot_row,
		  const INT pos, const INT step, const INT projected_axle);

  void Print(FILE *fp) const;
  void WB_Print(FILE *fp) const;
  INT WB_Print(char* bf, INT ccount) const;
  void Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack);

friend  REGION * Region_Intersect(const REGION &a, const REGION &b, const ARA_LOOP_INFO &ara_info);
friend  REGION * Region_Union(const REGION &a, const REGION &b, const ARA_LOOP_INFO &ara_info);
friend  INT Region_Compare(const REGION &a, const REGION &b, const ARA_LOOP_INFO &ara_info);
friend  void Add_To_SOE(const REGION &a, const INT pos, 
			SYSTEM_OF_EQUATIONS *soe, 
			SYMBOL_LIST *syms, INT_ST & non_const_loops,
			const BOOL convert_equation,
			const ARA_LOOP_INFO &ara_info);

};

// A class to hold a list of convex regions
class REGION_UN : public SLIST {
  DECLARE_SLIST_CLASS(REGION_UN, REGION);
  
public:

  ~REGION_UN(){
    while (!Is_Empty()) CXX_DELETE(Remove_Headnode(), &ARA_memory_pool);
  }

  BOOL Has_Formal_Parameter();
  BOOL Is_Bottom() { return (Is_Empty() || Head()->Is_Empty()); }
  BOOL Is_All() { return (Head() && Head()->Is_All()); }
  BOOL Is_Included(const REGION &a, const ARA_LOOP_INFO &ara_info);
  BOOL Is_Included(const REGION_UN &a, const ARA_LOOP_INFO &ara_info);
  BOOL Contains(WN *array_wn);
  WN * Any_Wn();

  REGION_UN & Add_Region(REGION *a, const ARA_LOOP_INFO &ara_info);
  REGION_UN & RegionUN_Projection(const INT depth, ARA_LOOP_INFO &ara_info);
  void Print(FILE *fp) const;
  void WB_Print(FILE *fp) const; 
  INT WB_Print(char* bf, INT ccount) const; 
  void Print_Analysis_Info(FILE *fp, INT indent, DOLOOP_STACK &do_stack);
  
friend  REGION_UN * RegionUN_Intersect(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info);
friend  REGION_UN * RegionUN_Union(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info);
friend  BOOL RegionUN_LE(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info);
friend  BOOL RegionUN_EQ(const REGION_UN &a, const REGION_UN &b, const ARA_LOOP_INFO &ara_info);

};

class REGION_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS(REGION_ITER, REGION, REGION_UN);
};

class REGION_CONST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS(REGION_CONST_ITER, REGION, REGION_UN);
};

#endif

