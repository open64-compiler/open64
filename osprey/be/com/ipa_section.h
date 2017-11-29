/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ipa_section_INCLUDED
#define ipa_section_INCLUDED

#ifdef IPA_SUMMARY
#ifndef loop_info_INCLUDED
#include "loop_info.h"
#endif
#ifndef cxx_hash_INCLUDED
#include "cxx_hash.h"
#endif
#else

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

#ifndef opt_du_INCLUDED
#include "optimizer.h"
#endif

#endif


class IPA_LNO_READ_FILE; 

extern BOOL Trace_Sections;
// forward declarations:
typedef STACK<INT> INT_ST;

#define CONST_DESC 255

// Sets that are being computed
enum IPA_SECTION_TYPE {
  IPA_DEF = 1,
  IPA_USE = 2,
  IPA_REDUC = 3,
  IPA_PASS = 4,
  IPA_UNKNOWN = 5
};

// A term's variable, in a linear expression, can be one of:
enum LTKIND {
  LTKIND_NONE,		// No term
  LTKIND_CONST,		// Constant
  LTKIND_LINDEX,	// One of the loop index variables
  LTKIND_SUBSCR,	// One of the array subscripts
  LTKIND_IV		// One of the independent variables
};

// when we set up the system of equations we need to specify the
// action type
enum ACTION_TYPE {
  ACTION_EQ,
  ACTION_LO,
  ACTION_UP
};

typedef mINT32 COEFF;	    // A coefficient
typedef mUINT16 DESCR;    // A coefficient descriptor


//------------------------------------------------------------------------
// node containing information about scalar symbols occuring in loops
//------------------------------------------------------------------------
class LOOP_SYMBOL
{
private:
  mUINT32 _ivar_index;

public:
  LOOP_SYMBOL (UINT32 index) : 
    _ivar_index (index)
  {}
  
  BOOL operator== (const LOOP_SYMBOL& other) 
  {
    return (_ivar_index == other._ivar_index);
  }
  
  UINT32 Ivar_Index () const { return _ivar_index; }
  
  void Print (FILE* fp) { fprintf(fp, "IVAR[%d]\n", _ivar_index); }
};

typedef DYN_ARRAY<LOOP_SYMBOL> LOOP_SYMBOL_ARRAY;



//===================================================================
// In IPL and IPA, an IVAR represents either a formal parameter
// (specified by its position) or a global variable (ST_IDX).
// In LNO, an IVAR may also represent a local ST_IDX, when 
// mapping a formal parameter into the actual argument
//===================================================================
class IVAR
{
private:
  union {
    mUINT32 _formal_position;
    ST_IDX  _st_idx;
  } u;
  
  WN_OFFSET _offset;
  mTYPE_ID  _mtype:8;
  mBOOL     _is_formal;
  
public:
  IVAR (UINT32 position, WN_OFFSET offset, TYPE_ID mtype) 
  {
    u._formal_position = position;
    _offset = offset;
    _mtype = mtype;
    _is_formal = TRUE;
  }
  
  IVAR (const ST* st, WN_OFFSET offset, TYPE_ID mtype) 
  {
    u._st_idx = ST_st_idx(st);
    _offset = offset;
    _mtype = mtype;
    _is_formal = FALSE;
  }

  IVAR ()
  {
    u._formal_position = 0;
    u._st_idx = ST_IDX_ZERO;
    _offset = 0;
    _mtype = 0;
    _is_formal = FALSE;
  }

  BOOL Is_Formal () const { return _is_formal; }

  UINT32 Formal_Position () const 
  { 
    FmtAssert(_is_formal, ("IVAR::Formal_Position(): IVAR is not a formal"));
    return u._formal_position;
  }

  void Set_Formal_Position(UINT32 formal_position) 
  { 
    _is_formal = TRUE; 
    u._formal_position = formal_position; 
  }

  ST_IDX St_Idx () const 
  { 
    FmtAssert(!_is_formal, ("IVAR::St_Idx(): IVAR is a formal"));
    return u._st_idx; 
  }

  WN_OFFSET Offset () const { return _offset; }

  void Set_Offset(WN_OFFSET offset) { _offset = offset; }

  TYPE_ID Mtype () const { return _mtype; }

  void Set_Mtype (TYPE_ID mtype) { _mtype = mtype; }

  BOOL operator== (const IVAR& other) const 
  { 
    return (_is_formal == other._is_formal &&
            _offset == other._offset &&
            _mtype == other._mtype &&
            ((_is_formal && u._formal_position == other.u._formal_position) ||
             (!_is_formal && u._st_idx == other.u._st_idx)));
  }

  void Set_St_Idx (ST_IDX new_st_idx) 
  { 
    _is_formal = FALSE; 
    u._st_idx = new_st_idx; 
  }

  void Print (FILE* fp = stderr);

  void WB_Print (FILE* fp, INT ivar_index);

  void IPA_LNO_Print_File (FILE* fp = stderr, INT ivar_index = -1);

  void IPA_LNO_Print (FILE* fp = stderr, 
                      IPA_LNO_READ_FILE* IPA_LNO_File = NULL);

};

typedef DYN_ARRAY<IVAR> IVAR_ARRAY;

//===========================================================
// Define a term of a linear expression
//===========================================================
class TERM
{
private:
  COEFF _coeff;
  DESCR _desc;
  LTKIND _kind : 8;
  mUINT8 _projected_level;

public:
  // constructors
  TERM (LTKIND kind, COEFF coeff, DESCR descr, mUINT8 level) :
    _coeff (coeff),
    _desc (descr),
    _kind (kind),
    _projected_level (level)
  {}
  
  TERM (const TERM* term) :
    _coeff (term->_coeff),
    _desc (term->_desc),
    _kind (term->_kind),
    _projected_level (term->_projected_level)
  {}
  
  void Set_coeff (COEFF w) { _coeff = w; }
  COEFF Get_coeff () const { return _coeff; }

  void Set_desc (DESCR b) { _desc = b; }
  DESCR Get_desc () const { return _desc; }

  void Set_type (LTKIND kind) { _kind = kind; }
  LTKIND Get_type () const    { return _kind; }
  
  void Set_projected_level (mUINT8 level) { _projected_level = level; }
  mUINT8 Get_projected_level () const     { return _projected_level; }

  BOOL Equivalent(TERM& t);

  BOOL Is_equal(TERM* t, INT count);

  void Print(FILE* fp = stderr, BOOL newline = TRUE);

  void Print_file(FILE* fp = stderr);

  void IPA_LNO_Print(FILE* fp = stderr, IPA_LNO_READ_FILE* ilr_file = NULL);

  void IPA_LNO_Print_File(FILE* fp = stderr, INT term_index = -1);

  void WB_Print(FILE* fp, INT term_index);
};

typedef DYN_ARRAY<TERM> TERM_ARRAY;

// forward declaration
class LOOPINFO;

//===================================================================
// an array of TERMS in a linear expression 
//===================================================================
class LINEX 
{
private:
  TERM_ARRAY _larray;

public:
  // constructor
  LINEX (MEM_POOL* m) { new (&_larray) TERM_ARRAY(m); }

  // destructor
  ~LINEX () { _larray.Free_array(); }
  

  mINT32 Num_terms () const { return _larray.Lastidx(); }

  TERM* Get_term(INT32 idx) const { return &(_larray)[idx]; }

  void Set_term(const TERM* term) { _larray.AddElement(TERM(term)); }
  
  void Set_term(LTKIND kind, COEFF coeff, DESCR descr, mUINT8 level) 
  {
    _larray.AddElement(TERM(kind, coeff, descr, level));
  }

  void Set_linex_terms(INT start_index, INT end_index, TERM* term);

  // return the constant term 
  INT Get_constant_term();
  
  // copy a linex class
  void Copy(LINEX *to);

  // check if the coeff of the loop terms are equal
  BOOL Loop_coeff_terms_equal(LINEX* l);

  // check the number of loop coefficient terms
  INT Num_loop_coeff_terms();

  // check if the linear expression contains a term with
  // the loop coefficient "i" in it
  BOOL Has_loop_coeff(INT i);
  
  // map a linex expression to a vector in the systems of equations
  void Add_access(SYSTEM_OF_EQUATIONS *soe,
		  mUINT8 depth, 
                  INT num_dim, 
                  INT axle,
		  INT num_syms, 
                  ACTION_TYPE act, 
                  LOOP_SYMBOL_ARRAY* sym, 
                  BOOL trace);
  
  void Print(FILE* fp = stderr);
  void Print_file(FILE* fp = stderr);

  void Map_access_vector(ACCESS_VECTOR* av, BOOL Is_LNO, 
    IPA_LNO_READ_FILE* IPA_LNO_File);

  // map a particular line of an SOE to a linex
  void Map_from_SOE(const SYSTEM_OF_EQUATIONS* soe, 
                    INT i, 
		    const LOOP_SYMBOL_ARRAY* syms, 
		    INT depth, 
                    INT dim, 
		    INT which_array, 
                    BOOL is_lower_bound);


  // clean up the terms in the linex
  void Free_terms();

  BOOL Equivalent(LINEX &b);

  // compute the max, note, this is only valid if 
  INT Max(LINEX*l);

  // check if the linex  is a constant term only 
  // (used for step calculation)
  BOOL Is_const();

  // Merge/Add 2 linex terms
  LINEX* Merge(LINEX* l);

  // Subtract 2 linex terms
  LINEX* Subtract(LINEX* l);

  void Init(MEM_POOL* m);

  void Add_coupled_terms(LINEX* from);

  void Simplify();
  void LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File, WN* wn_call); 
  void Substitute_Lindex(INT lindex, LINEX* lx_substitute);
  void Remove_Zero_Terms();
  BOOL Has_Local_Symbol();
};

typedef DYN_ARRAY<LINEX> LINEX_ARRAY;

class PROJECTED_KERNEL;
typedef DYN_ARRAY<PROJECTED_KERNEL> PROJECTED_KERNEL_ARRAY;
#ifdef IPA_SUMMARY
extern IVAR_ARRAY *Ivar;

// Mapping from LOOPINFO (ipl) to DO_LOOP_INFO_BASE (lno)
typedef 
HASH_TABLE<LOOPINFO*,DO_LOOP_INFO_BASE*> LOOPINFO_TO_DLI_MAP;
extern LOOPINFO_TO_DLI_MAP* IPL_Loopinfo_Map;

// Mapping from PROJECTED_REGION (ipl) to ACCESS_ARRAY (lno)
class PROJECTED_REGION;
typedef 
HASH_TABLE<PROJECTED_REGION*,ACCESS_ARRAY*> PROJ_REGION_TO_ACCESS_ARRAY_MAP;
extern PROJ_REGION_TO_ACCESS_ARRAY_MAP* IPL_Access_Array_Map;

#endif 
// ====================================================================
//
// LOOPINFO
//
// A loop nest, containing information about the nest itself, plus
// common data structure elements for the array sections which occur
// in the nest.
//
//
// ====================================================================
class LOOPINFO
{
private:
#define MESSY_UPPER_BOUND 0x01
#define MESSY_LOWER_BOUND 0x02
#define MESSY_STRIDE      0x04
#define MESSY_BOUNDS      0x08
#define MESSY_ANY_BOUNDS  0x0f

  mINT8 _nest_level;
  mINT8 _flags;

  MEM_POOL* _mem_pool;

  // kernels for this loop (only used when in memory)
  PROJECTED_KERNEL_ARRAY* _kernel;

  union {
    struct {
      LINEX* _upper_linex;
      LINEX* _lower_linex;
      LINEX* _step_linex;
      LOOP_SYMBOL_ARRAY* _symbols;    
      mINT16 _cd_idx;      // id into the control dependence structure
    } u3;
    struct {
      mINT16 _upper_index; 
      mINT16 _lower_index; 
      mINT16 _step_index;
      mUINT8 _upper_count;
      mUINT8 _lower_count;
      mUINT8 _step_count;
    } u2;
  } u1;

public:
  // constructors
  LOOPINFO(MEM_POOL* m) { BZERO(this, sizeof(LOOPINFO)); _mem_pool = m; }

  LOOPINFO(MEM_POOL* m, INT32 cd_idx);

  MEM_POOL* Mem_Pool() const     { return _mem_pool; }
  void Set_Mem_Pool(MEM_POOL* m) { _mem_pool = m; }

  mINT8 Get_nest_level()  const    { return _nest_level; }
  void Set_nest_level(mINT8 level) { _nest_level = level; }

  mINT8 Get_flags() const { return _flags; }
  void Set_flags(mINT8 f) { _flags = f; }

  mINT16 Get_cd_idx() const { return u1.u3._cd_idx; };
  void Set_cd_idx(mINT16 cd_idx) { u1.u3._cd_idx = cd_idx;};

  LINEX* Get_upper_linex() const { return u1.u3._upper_linex; }
  LINEX* Get_lower_linex() const { return u1.u3._lower_linex; }
  LINEX* Get_step_linex()  const { return u1.u3._step_linex;  }

  mINT32 Get_ub_term_index()   const { return u1.u2._upper_index; }
  mINT32 Get_lb_term_index()   const { return u1.u2._lower_index; }
  mINT32 Get_step_term_index() const { return u1.u2._step_index;  }
  mUINT8 Get_ub_term_count()   const { return u1.u2._upper_count; }
  mUINT8 Get_lb_term_count()   const { return u1.u2._lower_count; }
  mUINT8 Get_step_term_count() const { return u1.u2._step_count;  }

  void Set_ub_term_index(mINT16 i)   { u1.u2._upper_index = i; }
  void Set_lb_term_index(mINT16 i)   { u1.u2._lower_index = i; }
  void Set_step_term_index(mINT16 i) { u1.u2._step_index  = i; }
  void Set_ub_term_count(mUINT8 i)   { u1.u2._upper_count = i; }
  void Set_lb_term_count(mUINT8 i)   { u1.u2._lower_count = i; }
  void Set_step_term_count(mUINT8 i) { u1.u2._step_count  = i; }

  BOOL Is_messy_ub()         const { return _flags & MESSY_UPPER_BOUND; }
  BOOL Is_messy_lb()         const { return _flags & MESSY_LOWER_BOUND; }
  BOOL Is_messy_step()       const { return _flags & MESSY_STRIDE; }
  BOOL Is_messy_bounds()     const { return _flags & MESSY_BOUNDS; }
  BOOL Is_messy_any_bounds() const { return _flags & MESSY_ANY_BOUNDS; }

  void Set_messy_ub ()    { _flags |= MESSY_UPPER_BOUND; }
  void Set_messy_lb()     { _flags |= MESSY_LOWER_BOUND; }
  void Set_messy_step()   { _flags |= MESSY_STRIDE; }
  void Set_messy_bounds() { _flags |= MESSY_ANY_BOUNDS; }

#ifdef IPA_SUMMARY
  void Map_do_loop_info(DO_LOOP_INFO_BASE* dli);
#endif

  void Create_linex(TERM* term);

  LINEX* Build_linex(ACCESS_VECTOR* av);

  PROJECTED_KERNEL_ARRAY* Get_kernels() const { return _kernel; }

  void Add_bound(LINEX* l, 
                 SYSTEM_OF_EQUATIONS* soe, 
                 mUINT8 depth, 
		 INT num_dim, 
                 INT num_syms, 
		 LOOP_SYMBOL_ARRAY* sym);



  void Print(FILE* fp = stderr);
  void Print_file(FILE* fp = stderr);
  void WB_Print(FILE* fp, INT loop_info_index);

  LOOP_SYMBOL_ARRAY*  Get_symbol_array() const { return u1.u3._symbols;};

  LINEX* Min_value();
  LINEX* Max_value();
};

typedef DYN_ARRAY<LOOPINFO> LOOPINFO_ARRAY;

//------------------------------------------------------------------------
// after a linex has been projected, we obtain upper, lower and step 
// linex constraints
// this is equivalent to the AXLE_NODE structure
//------------------------------------------------------------------------
class PROJECTED_NODE
{
private:
#define MESSY_UPPER_BOUND 0x01
#define MESSY_LOWER_BOUND 0x02
#define MESSY_STRIDE      0x04
#define UNPROJECTED       0x08
#define ASSUMED_SHAPE     0x10

  union {
    struct {
      LINEX* _lb_linex;
      LINEX* _ub_linex;
      LINEX* _step_linex;
      LINEX* _segment_length_linex;
      LINEX* _segment_stride_linex;
    } u0;
    struct {
      mINT32  _lb_term_index : 24;
      mUINT32 _lb_term_count : 8;
      mINT32  _ub_term_index : 24;
      mUINT32 _ub_term_count : 8;
      mINT32  _step_term_index : 24;
      mUINT32 _step_term_count : 8;
      mINT32  _segment_length_term_index : 24;
      mUINT32 _segment_length_term_count : 8;
      mINT32  _segment_stride_term_index : 24;
      mUINT32 _segment_stride_term_count : 8;
    } u1;
  } u2;

  mUINT32 _flags;
  MEM_POOL* _mem_pool;

public:
  void Init(MEM_POOL* m);

  void Set_Mem_Pool(MEM_POOL* mem_pool) { _mem_pool = mem_pool; }
  MEM_POOL* Mem_Pool() { return _mem_pool; }

  LINEX* Get_lower_linex() const { return (u2.u0._lb_linex); }
  void Set_lower_linex(LINEX* l) { u2.u0._lb_linex = l; }

  LINEX* Get_upper_linex()  const { return (u2.u0._ub_linex); }
  void Set_upper_linex(LINEX* l)  { u2.u0._ub_linex = l; }

  LINEX* Get_step_linex() const { return (u2.u0._step_linex); }
  void Set_step_linex(LINEX* l) { u2.u0._step_linex = l; }

  LINEX* Get_segment_length_linex() const { 
    return (u2.u0._segment_length_linex);
  }
  void Set_segment_length_linex(LINEX* l) { 
    u2.u0._segment_length_linex = l; 
  }

  LINEX* Get_segment_stride_linex() const { 
    return (u2.u0._segment_stride_linex);
  }
  void Set_segment_stride_linex(LINEX* l) { 
    u2.u0._segment_stride_linex = l; 
  }

  mINT32 Get_lb_term_index() const { return u2.u1._lb_term_index; }
  void Set_lb_term_index(mINT32 i) { u2.u1._lb_term_index = i; }

  mUINT32 Get_lb_term_count() const { return u2.u1._lb_term_count; }
  void Set_lb_term_count(mUINT32 i) { u2.u1._lb_term_count = i; }

  mINT32 Get_ub_term_index() const { return u2.u1._ub_term_index; }
  void Set_ub_term_index(mINT32 i) { u2.u1._ub_term_index = i; }

  mUINT32 Get_ub_term_count() const { return u2.u1._ub_term_count; }
  void Set_ub_term_count(mUINT32 i) { u2.u1._ub_term_count =i; }

  mINT32 Get_step_term_index() const { return u2.u1._step_term_index; }
  void Set_step_term_index(mINT32 i) { u2.u1._step_term_index=i; }

  mUINT32 Get_step_term_count() const { return u2.u1._step_term_count; }
  void Set_step_term_count(mUINT32 i) { u2.u1._step_term_count = i; }

  mINT32 Get_segment_length_term_index() const { 
    return u2.u1._segment_length_term_index; 
  }
  void Set_segment_length_term_index(mINT32 i) { 
    u2.u1._segment_length_term_index=i; 
  }

  mINT32 Get_segment_length_term_count() const { 
    return u2.u1._segment_length_term_count; 
  }
  void Set_segment_length_term_count(mINT32 i) { 
    u2.u1._segment_length_term_count=i; 
  }

  mINT32 Get_segment_stride_term_index() const { 
    return u2.u1._segment_stride_term_index; 
  }
  void Set_segment_stride_term_index(mINT32 i) { 
    u2.u1._segment_stride_term_index=i; 
  }

  mINT32 Get_segment_stride_term_count() const { 
    return u2.u1._segment_stride_term_count; 
  }
  void Set_segment_stride_term_count(mINT32 i) { 
    u2.u1._segment_stride_term_count=i; 
  }

  BOOL Is_messy_ub() const { return _flags & MESSY_UPPER_BOUND;}
  void Set_messy_ub()      { _flags |= MESSY_UPPER_BOUND; }
  
  BOOL Is_messy_lb() const { return _flags & MESSY_LOWER_BOUND; }
  void Set_messy_lb()      { _flags |= MESSY_LOWER_BOUND; }
  
  BOOL Is_messy_step() const { return _flags & MESSY_STRIDE; }
  void Set_messy_step()      { _flags |= MESSY_STRIDE; }

  BOOL Has_a_messy_bound() const 
  { return _flags & (MESSY_UPPER_BOUND | MESSY_LOWER_BOUND | MESSY_STRIDE); }

  BOOL Has_all_messy_bounds() const { 
    return ((_flags & MESSY_UPPER_BOUND) && 
            (_flags & MESSY_LOWER_BOUND) &&
            (_flags & MESSY_STRIDE));
  }
  
  void Set_all_messy_bounds() 
  {_flags |= (MESSY_UPPER_BOUND | MESSY_LOWER_BOUND | MESSY_STRIDE); }
  
  BOOL Is_unprojected() const { return _flags & UNPROJECTED; }
  void Set_unprojected()      { _flags |= UNPROJECTED; }
  void Reset_is_unprojected() { _flags &= ~UNPROJECTED; }

  BOOL Is_assumed_shape() const { return _flags & ASSUMED_SHAPE; }
  void Set_assumed_shape()      { _flags |= ASSUMED_SHAPE; }

  mUINT32 Get_flags() const     { return _flags; }
  void Set_flags(mUINT32 flags) { _flags = flags; }


  void Create_linex(TERM* t);

  // return the constant term in linex lower
  INT Get_constant_term();

  void Set_linexs(const SYSTEM_OF_EQUATIONS *soe,
		  INT i, 
                  INT j, 
		  const LOOP_SYMBOL_ARRAY *syms, 
                  INT depth,
                  INT dim,
		  INT stride);

  void Set_linexs(LINEX* low_new, 
		  LINEX* up_new,
		  LINEX* step_new,
		  LINEX* segment_length_new,
		  LINEX* segment_stride_new);
  
  void Set_linex_eq(const SYSTEM_OF_EQUATIONS *soe,
                    INT i, 
                    INT j, 
                    const LOOP_SYMBOL_ARRAY *syms,
                    INT depth, 
                    INT dim,
                    INT stride);

  void Set_linex_le(const SYSTEM_OF_EQUATIONS *soe,
                    INT i, 
                    INT j, 
                    const LOOP_SYMBOL_ARRAY *syms,
                    INT depth, 
                    INT dim,
                    INT stride);

  // set constant linexs for this projected node
  void Set_constant_linexs(INT32 upper, 
                           INT32 lower, 
			   INT32 step, 
                           INT32 segment_length, 
		           INT32 segment_stride);
  
  // Set linexes for a constant two-strided array section
  void Set_constant_two_strided_section(INT32 lower, 
                                        INT32 upper, 
                                        INT32 step, 
                                        INT32 seg_len,
                                        INT32 seg_stride);

  // reset the node fields, clear up all the terms
  void Reset_node();

  BOOL Equivalent( PROJECTED_NODE &b);

  // copy to 
  void Copy(PROJECTED_NODE *to);

  // print
  void Print(FILE *fp = stderr);
  void Print_file(FILE* fp = stderr);
  void IPA_LNO_Print(FILE *fp = stderr, 
                     IPA_LNO_READ_FILE* IPA_LNO_File = NULL);
  void IPA_LNO_Print_File(FILE *fp = stderr, INT pn_index = -1);
  void WB_Print(FILE* fp, INT proj_node_index);

  void Set_to_kernel_image(PROJECTED_NODE* pn_kernel, LINEX* lx_offset);

  void Fill_Out(); 

  void Simplify();
  void LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File, WN* wn_call);
  BOOL Matching_Segment_Stride(PROJECTED_NODE* pn);
};

typedef DYN_ARRAY<PROJECTED_NODE> PROJECTED_ARRAY;

class PROJECTED_KERNEL;

//------------------------------------------------------------------------
// An array of projected nodes, one for each dimension of the array
// this is equivalent to the REGION structure
//------------------------------------------------------------------------
class PROJECTED_REGION
{
private:
#define MESSY_REGION 1
#define NON_MESSY_REGION 2
#define UNPROJECTED_REGION 4
#define IS_MAY_KILL 8
#define IS_MAY_USE 16
#define IS_PASSED 32
#define IS_FORMAL 64

  union  {
    PROJECTED_ARRAY* _region;
    mINT32 _id;
  } u1;

  mINT16 _type;
  mUINT8 _num_dims;
  mUINT8 _depth;

  // this is an index into the linex kernel (if one exists), 
  // used for optimizing away the projection operation
  union {
    mINT32 _projected_kernel;
    PROJECTED_KERNEL *_p;
    struct {
	mINT16 _callsite_id;
	mINT16 _actual_id;
      } u22;
    } u2;

  MEM_POOL* _mem_pool; 

public:

  PROJECTED_REGION(ACCESS_ARRAY* ar, 
                   MEM_POOL* mem_pool, 
		   LOOPINFO *loop, 
                   BOOL in_ipl = TRUE,
		   IPA_LNO_READ_FILE* IPA_LNO_File = NULL);

  PROJECTED_REGION(PROJECTED_REGION* p);

  PROJECTED_REGION(mINT16 type,
                   mUINT8 depth, 
                   mUINT8 num_dim, 
                   MEM_POOL* mem_pool);
  

  void Set_Mem_Pool(MEM_POOL* mem_pool) { _mem_pool = mem_pool; }
  MEM_POOL* Mem_Pool() { return _mem_pool; }

  void Set_callsite_id(INT16 id) { u2.u22._callsite_id = id;};
  INT16 Get_callsite_id() const { return u2.u22._callsite_id;};

  void Set_actual_id(INT16 id) { u2.u22._actual_id = id;};
  INT16 Get_actual_id() const { return u2.u22._actual_id;};

  void Set_id(INT32 i)  { u1._id = i;};
  INT32 Get_id() const { return u1._id;};

  void Set_type(mINT16 type) { _type = type;};
  mINT16 Get_type() const { return _type; };

  void Set_num_dims(mUINT8 num_dims) { _num_dims = num_dims;};
  mUINT8 Get_num_dims() const { return _num_dims;};
  
  void Set_depth(mUINT8 depth) { _depth = depth;};
  mUINT8 Get_depth() const { return _depth;};

  mINT32 Get_projected_kernel_id() const { return u2._projected_kernel;};
  void Set_projected_kernel_id(mINT32 id) { u2._projected_kernel = id; };

  PROJECTED_KERNEL* Get_projected_kernel() const { return u2._p;};
  void Set_projected_kernel(PROJECTED_KERNEL *p) { u2._p = p;};
  
  BOOL Is_unprojected_region() const { return _type &
				       UNPROJECTED_REGION;};
  void Set_unprojected() { _type = _type | UNPROJECTED_REGION;};

  void Reset_is_unprojected() { _type = _type & ~UNPROJECTED_REGION;};

  BOOL Is_messy_region() const { return _type & MESSY_REGION;};
  void Set_messy_region() { _type = _type | MESSY_REGION;};

  void  Reset_messy_region() { _type = _type & ~MESSY_REGION;};
  
  BOOL Is_may_kill() const { return _type & IS_MAY_KILL;};
  void Set_is_may_kill() { _type = _type | IS_MAY_KILL;};

  BOOL Is_may_use() const { return _type & IS_MAY_USE;};
  void Set_is_may_use() { _type = _type | IS_MAY_USE;};

  BOOL Is_passed() const { return _type & IS_PASSED;};
  void Set_is_passed() { _type = _type | IS_PASSED;};

  BOOL Is_formal() const { return _type & IS_FORMAL;};
  void Set_is_formal() { _type = _type | IS_FORMAL;};

  void Set_projected_node(PROJECTED_NODE *node) {
    u1._region->AddElement(*node);
  }
  PROJECTED_NODE* Get_projected_node(INT i) { return &(*u1._region)[i]; }

  BOOL Has_Messy_Bounds();
  BOOL Has_Important_Messy_Bounds(); // don't count the "adjustable" dim
  void Set_Messy_If_Local_Symbol();

  void Copy_projected_node(PROJECTED_NODE* node);
  void Copy_write(PROJECTED_REGION *p_in);
  
  void Set_projected_array(PROJECTED_ARRAY* region) { u1._region = region; }
  PROJECTED_ARRAY* Get_projected_array() const      { return u1._region; }


  // compare 2 projected regions
  INT Compare(PROJECTED_REGION *b);

  LINEX_ARRAY* Map_to_linex_array();

  void Project(INT depth, LOOPINFO *l);
  PROJECTED_REGION *Union(PROJECTED_REGION &b, const LOOPINFO& l);

  BOOL May_Union(PROJECTED_REGION& b, BOOL trace);

  void Set_region(SYSTEM_OF_EQUATIONS *soe, 
                  LOOP_SYMBOL_ARRAY *syms, 
                  INT stride[], 
                  INT pivot_row,
                  INT pos, 
                  INT loop_step, 
                  INT projected_axle);

  BOOL Equivalent(PROJECTED_REGION* p);

  BOOL Constant_bounds(mUINT8 num_dims);

  void Print(FILE* fp = stderr);
  void Print_file(FILE* fp = stderr);
  void IPA_LNO_Print(FILE* fp = stderr, IPA_LNO_READ_FILE* 
    IPA_LNO_File = NULL);
  void IPA_LNO_Print_File(FILE* fp = stderr, INT pr_index = -1);
  void WB_Print(FILE* fp, INT proj_region_index);

  void Simplify();
  void LNO_Simplify(IPA_LNO_READ_FILE* IPA_LNO_File, WN* wn_call);
  void Fill_Out();
  BOOL Matching_Segment_Stride(PROJECTED_REGION* pr);
};

typedef DYN_ARRAY<PROJECTED_REGION> PROJECTED_REGION_ARRAY;

//------------------------------------------------------------------------
// A kernel is simply a projected region without the constant terms
// This is used as an optimization to reduce the number of times a
// system of equations is solved to eliminate variables during projection
//------------------------------------------------------------------------
class PROJECTED_KERNEL
{
private:
  #define PROJECTED 1
  #define MESSY_KERNEL 2
  LINEX_ARRAY* _array;
  BOOL     *_is_independent;
  mINT16 _projected_level;
  mUINT8 _depth;
  mUINT8 _type;
  LINEX_ARRAY* _difference; 
  union {
    PROJECTED_REGION *_region;
    INT _id;
  } u1;
  MEM_POOL* _mem_pool;

public:

  void Set_Mem_Pool(MEM_POOL* mem_pool) { _mem_pool = mem_pool; }
  MEM_POOL* Mem_Pool() { return _mem_pool; }

  void Init(PROJECTED_REGION*, LOOPINFO*);

  BOOL Is_independent(mINT32 i) const { return _is_independent[i];};
  
  void Set_depth(mUINT8 depth) { _depth = depth;};
  mUINT8 Get_depth() const { return _depth;};
  
  void Set_projected_level(mINT16 level) 
    { _projected_level = level;};

  void Set_region(PROJECTED_REGION* r)  { u1._region = r;};
  PROJECTED_REGION* Get_region() const { return u1._region;};

  mINT16 Get_projected_level() { return _projected_level;};
  mINT16 Get_num_dims() { return _array->Lastidx()+1;};

  LINEX* Get_linex(INT32 i) { return &(*_array)[i]; };

  void Set_is_projected() { _type = _type | PROJECTED;};
  BOOL Is_projected() const {  return _type & PROJECTED;};

  void Set_messy_kernel() { _type = _type | MESSY_KERNEL;};
  BOOL Is_messy_kernel() const {  return _type & MESSY_KERNEL;};
  
  void Project(mUINT8 depth, LOOPINFO* loop); 
  void Print(FILE* fp = stderr);

  LINEX_ARRAY* Get_Difference() {return _difference;};
  LINEX* Get_Difference(INT i) 
    {return _difference != NULL && i <= _difference->Lastidx() 
      ? &(*_difference)[i] : NULL;}; 
  void Set_Difference(PROJECTED_REGION* pr); 
};

//===================================================================
// store a pointer to the projected region
//===================================================================
class PROJECTED_REGION_INFO
{
private:
  PROJECTED_REGION *_p;
  
public:
  void Set_projected_region(PROJECTED_REGION *p) { _p = p; };
  PROJECTED_REGION*
    Get_projected_region() { return _p;};
  void Print(FILE *fp = stderr);
};

typedef DYN_ARRAY<PROJECTED_REGION_INFO> PROJECTED_REGION_INFO_ARRAY;

#define IPL_HAS_BAD_ALIAS 1
#define IPL_IS_LOOP_INVARIANT 2
#define IPL_IS_DEF 4 // is it a definition?
#define IPL_IS_USE 8 // is it a use?
#define IPL_IS_PASSED 16 // is is passed?
#define IPL_IS_MAY_USE 32  // may be used
#define IPL_IS_MAY_DEF 64 // may be defined
#define IPL_IS_FORMAL 128 // is a formal parameter

//===================================================================
// equivalent to ARA_REF in LNO's code
//===================================================================
class REGION_ARRAYS
{
private:
  mUINT8 _type;
  mINT32 _sym_index;
  mINT32 _element_size;

  union {
    PROJECTED_REGION_INFO_ARRAY *_regions;
    struct {
      mINT32 _idx; // index into the PROJECTED_REGION array
      mINT32 _count; // count of the elements in the PROJECTED_REGION array
    } u2;
  } u1;

public:
  REGION_ARRAYS(MEM_POOL *m, mINT32 index) {
    u1._regions = (PROJECTED_REGION_INFO_ARRAY*) 
      CXX_NEW(PROJECTED_REGION_INFO_ARRAY(m),m);
    _type = 0;
    _sym_index  = index;
  }

  void Set_type(mUINT8 t ) { _type = t;};
  mUINT8 Get_type() const { return _type;};

  void Init(mINT32 index, mINT32 element_size, MEM_POOL *m);

  void Copy_write(REGION_ARRAYS *r);

  PROJECTED_REGION_INFO_ARRAY*
    Get_projected_region_array() const { return u1._regions; };
  
  void Set_has_bad_alias() { _type = _type | IPL_HAS_BAD_ALIAS;};
  BOOL Is_bad_alias() const { return _type & IPL_HAS_BAD_ALIAS ;};

  void Set_is_loop_invariant() { _type = _type |
				   IPL_IS_LOOP_INVARIANT;};
  BOOL Is_loop_invariant() const { return _type & IPL_IS_LOOP_INVARIANT;};

  void Set_is_use() { _type = _type | IPL_IS_USE;}; 
  BOOL Is_use() const { return _type & IPL_IS_USE;};

  void Set_is_def() { _type = _type | IPL_IS_DEF;};
  BOOL Is_def() const { return _type & IPL_IS_DEF;};

  void Set_is_passed() { _type = _type | IPL_IS_PASSED;};
  BOOL Is_passed() const { return _type & IPL_IS_PASSED;};

  void Set_is_may_def() { _type = _type | IPL_IS_MAY_DEF;};
  BOOL Is_may_def() const { return _type & IPL_IS_MAY_DEF;};

  void Set_is_may_use() { _type = _type | IPL_IS_MAY_USE;};
  BOOL Is_may_use() const { return _type & IPL_IS_MAY_USE;};

  void Set_is_formal() { _type = _type | IPL_IS_FORMAL;};
  BOOL Is_formal() const { return _type & IPL_IS_FORMAL;};

  mINT32 Get_sym_id() const { return _sym_index;};
  void Set_sym_id(INT id) { _sym_index = id;};   

  mINT32 Get_element_size() const {return _element_size;};
  void Set_element_size(mINT32 element_size) {_element_size = element_size;};

  // this is an index into the projected region array
  mINT32 Get_idx() const { return u1.u2._idx;};
  void   Set_idx(mINT32 id) { u1.u2._idx = id; };

  // this is a count of the number of elements in the projected
  // region array
  mINT32 Get_count() const { return u1.u2._count; };
  void Set_count(mINT32 count) { u1.u2._count = count;};

  PROJECTED_REGION*
    Get_Projected_Region(INT i);


  void Print(FILE *fp = stderr);
  void Print_file(FILE* fp = stderr);
  void WB_Print(FILE* fp, INT region_index, const char* name, const char* func_name);
};

typedef DYN_ARRAY<REGION_ARRAYS> ARRAY_OF_REGION_ARRAYS;
// typedef DYN_ARRAY<INT>  INT_ARRAY;

#define IPA_SCALAR_MAY_KILL 1
#define IPA_SCALAR_MAY_USE 2
#define IPA_SCALAR_MAY_REDUC 4
#define IPA_SCALAR_KILL 8
#define IPA_SCALAR_USE 16
#define IPA_SCALAR_REDUC 32
#define IPA_ARRAY_REDUC 64
#define IPA_ARRAY_MAY_REDUC 128
#define IPA_SCALAR_PASSED 256
#define IPA_SCALAR_EUSE 512
#define IPA_SCALAR_CALL_EUSE 1024
#define IPA_SCALAR_MAY_PASS 2048
class SCALAR_INFO
{
public:
  mINT32 _index; // index into the symbols array
  mINT16 _type;   // type information
  mINT16 _call_index; // index into the callsite array in the case
                      // that it has been passed as a reference parameter
public:
  mINT32 Get_id() const { return _index;};
  void Set_id(mINT32 i) { _index = i;};

  mINT16 Get_type() const { return _type;};
  void Set_type(mINT16 t) {  _type = t;};

  void Set_callsite_id(mINT16 c) { _call_index = c;};
  mINT16 Get_callsite_id() const { return _call_index;};

  void Set_may_kill() { _type = _type | IPA_SCALAR_MAY_KILL;};
  BOOL Is_may_kill() const { return _type & IPA_SCALAR_MAY_KILL;};

  void Set_may_use() { _type = _type | IPA_SCALAR_MAY_USE;};
  BOOL Is_may_use() const { return _type & IPA_SCALAR_MAY_USE;};
  
  void Set_may_reduc() { _type = _type | IPA_SCALAR_MAY_REDUC;};
  BOOL Is_may_reduc() const { return _type & IPA_SCALAR_MAY_REDUC;};

  void Set_kill() { _type = _type | IPA_SCALAR_KILL;};
  BOOL Is_kill() const { return _type & IPA_SCALAR_KILL;};
  
  void Set_use() { _type = _type | IPA_SCALAR_USE;};
  BOOL Is_use() const { return _type & IPA_SCALAR_USE;};
  
  void Set_euse() { _type = _type | IPA_SCALAR_EUSE;};
  BOOL Is_euse() const { return  _type  & IPA_SCALAR_EUSE;};

  void Set_call_euse() { _type = _type | IPA_SCALAR_CALL_EUSE;};
  BOOL Is_call_euse() const { return  _type & IPA_SCALAR_CALL_EUSE;};
 
  void Set_reduc() { _type = _type | IPA_SCALAR_REDUC;};
  BOOL Is_reduc() const { return _type & IPA_SCALAR_REDUC;};

  void Set_array_reduc() { _type = _type | IPA_ARRAY_REDUC;};
  BOOL Is_array_reduc() const { return _type & IPA_ARRAY_REDUC;};

  void Set_array_may_reduc() { _type = _type | IPA_ARRAY_MAY_REDUC;};
  BOOL Is_array_may_reduc() const { return _type & IPA_ARRAY_MAY_REDUC;};

  void Set_passed_ref() { _type = _type | IPA_SCALAR_PASSED;};
  BOOL Is_passed_ref() const { return _type & IPA_SCALAR_PASSED;};

  void Set_may_passed_ref() { _type = _type | IPA_SCALAR_MAY_PASS;};
  BOOL Is_may_passed_ref() const { return _type & IPA_SCALAR_MAY_PASS;};
  
  SCALAR_INFO () { _type = 0; _call_index =-1; _index = 0;};
  void Init() { _type =0; _call_index = -1; _index =0; };

  void Print_file(FILE *fp = stderr);
  void Print(FILE *fp = stderr) { Print_file(fp); };
  void WB_Print(FILE* fp, INT scalar_index, const char* name, const char* func_name);
};

typedef DYN_ARRAY<SCALAR_INFO> INT_ARRAY;


//-------------------------------------------------------------------------
// at each control flow node, attach the following sets:
//-------------------------------------------------------------------------
class CFG_NODE_INFO
{
private:
  union {
    ARRAY_OF_REGION_ARRAYS *_def;  // kill set of array regions
    mINT32 _def_index;             // index into region_arrays
  } u1;
  union {
    ARRAY_OF_REGION_ARRAYS *_use;  // upwardly exposed use set of array regions
    mINT32 _use_index;             // index into region_arrays
  } u2;
  union {
    ARRAY_OF_REGION_ARRAYS *_param;// array sections passed as actuals
    mINT32 _param_index;           // index into region_arrays
  } u3;
  union {
    INT_ARRAY *_scalar_info;       // kill set of scalars
    mINT32 _scalar_index;          // index into the scalar array
  } u4;
  union {
    LOOPINFO *_loop;               // loop node
    mINT32 _index;                 // index 
  } u5;
  union {
    ARRAY_OF_REGION_ARRAYS *_formal; // array sections for formals
    mINT32 _formal_index;            // index into region_arrays
  } u6;

  mINT16 _def_count;               // number of array regions in kill set
  mINT16 _use_count;               // number of array regions in euse set
  mINT16 _param_count;             // number of array regions in param set
  mINT16 _scalar_count;            // number of scalars
  mINT16 _formal_count;            // number of formals

  enum _cfg_type {
    CFG_IF      = 1,
    CFG_DO_LOOP = 2,
    CFG_ENTRY   = 3,
    CFG_ELSE    = 4,
    CFG_UNKNOWN = 5
  } _type : 4;

  enum _cfg_state {
    CFG_STATE_CLEAR = 0x0,
    CFG_HAS_CALLS   = 0x1,
    CFG_IS_EXECUTED = 0x2 // this is to be used for do loops, i.e. are
                           // they definitely executed? if yes then we
                           // need to take that into account when
                           // computing kill information for instance
  } _state : 4;

  mINT32 _cd_index : 24;   // control dependence index (-1 for else nodes)

public:

  void Set_type_if()      { _type = CFG_IF; }
  void Set_type_do_loop() { _type = CFG_DO_LOOP; }
  void Set_type_entry()   { _type = CFG_ENTRY; }
  void Set_type_else()    { _type = CFG_ELSE; }

  BOOL Is_if()      const { return _type == CFG_IF; }
  BOOL Is_do_loop() const { return _type == CFG_DO_LOOP; }
  BOOL Is_entry()   const { return _type == CFG_ENTRY; }
  BOOL Is_else()    const { return _type == CFG_ELSE; }

  void Set_has_calls()   { _state = (_cfg_state) (_state | CFG_HAS_CALLS); }
  void Set_is_executed() { _state = (_cfg_state) (_state | CFG_IS_EXECUTED); }

  BOOL Has_calls()   const { return _state & CFG_HAS_CALLS; }
  BOOL Is_executed() const { return _state & CFG_IS_EXECUTED; }

  void Set_cd_index(INT index) { _cd_index = index; }
  INT  Get_cd_index() const { return _cd_index; }
  
  CFG_NODE_INFO (MEM_POOL* m, INT16 index) { 
    u1._def = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u2._use = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u3._param = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u4._scalar_info = (INT_ARRAY*)CXX_NEW(INT_ARRAY(m), m);
    u5._loop = (LOOPINFO*)CXX_NEW(LOOPINFO(m, index), m);
    u6._formal = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    _state = CFG_STATE_CLEAR;
    _type = CFG_UNKNOWN;
    _cd_index = -1;
  }

  void Init (MEM_POOL* m) { 
    u1._def = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u2._use = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u3._param = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    u4._scalar_info = (INT_ARRAY*)CXX_NEW(INT_ARRAY(m), m);
    u5._loop = NULL;
    u6._formal = (ARRAY_OF_REGION_ARRAYS*)CXX_NEW(ARRAY_OF_REGION_ARRAYS(m), m);
    _state = CFG_STATE_CLEAR;
    _type = CFG_UNKNOWN;
    _cd_index = -1;
  }

  void Init_Out () { 
    BZERO(this, sizeof(CFG_NODE_INFO));
    _type = CFG_UNKNOWN;
  }

  void Add_array_param(PROJECTED_REGION *p, mINT32 sym_index,
                       mINT32 element_size, INT16 callsite_id, 
		       INT16 actual_id);
  void Add_formal_array(PROJECTED_REGION *p, mINT32 element_size,
			mINT32 idx_symbol, mINT32 idx_formal);
  void Add_def_array(PROJECTED_REGION* p, mINT32 element_size, 
			mINT32 sym_index);
  void Add_may_def_array(PROJECTED_REGION* p, mINT32 element_size,
			mINT32 sym_index);
  void Add_use_array(PROJECTED_REGION* p, mINT32 element_size,
			mINT32 sym_index);
  void Add_may_use_array(PROJECTED_REGION* p, mINT32 element_size,
			mINT32 sym_index);

  void Add_scalar_def(mINT32 id);
  void Add_scalar_use(mINT32 id);
  void Add_scalar_reduc(mINT32 id);
  void Add_array_reduc(mINT32 id);
  void Add_array_may_reduc(mINT32 id);
  void Add_scalar_may_reduc(mINT32 id);
  void Add_scalar_may_def(mINT32 id);
  void Add_scalar_may_use(mINT32 id);
  INT Add_scalar_ref_passed(mINT32 id, mINT16 callsite_id);
  INT Add_scalar_ref_may_passed(mINT32 id, mINT16 callsite_id);

  ARRAY_OF_REGION_ARRAYS* Get_def_array() const { return u1._def; }
  ARRAY_OF_REGION_ARRAYS* Get_use_array() const { return u2._use; }
  ARRAY_OF_REGION_ARRAYS* Get_param_array() const { return u3._param; }
  ARRAY_OF_REGION_ARRAYS* Get_formal_array() const { return u6._formal; }

  INT_ARRAY* Get_scalar_array() const { return u4._scalar_info; }
  INT_ARRAY* Get_scalar_def_array() const { return u4._scalar_info; }
  INT_ARRAY* Get_scalar_use_array() const { return u4._scalar_info; }
  INT_ARRAY* Get_scalar_reduc_array() const { return u4._scalar_info; }
  INT_ARRAY* Get_array_reduc() const { return u4._scalar_info; }

  LOOPINFO *Get_loopinfo() const { return u5._loop; }
  void Set_loopinfo(LOOPINFO *l) { u5._loop = l; }

  void Print(FILE *fp = stderr);
  void Print_file(FILE *fp = stderr);
  void WB_Print(FILE* fp, INT cfg_index);

  // output related utils
  void Set_def_count(INT count) { _def_count = count; }
  void Set_def_index(INT index) {u1._def_index = index; }

  INT Get_def_count() const { return _def_count; }
  INT Get_def_index() const { return u1._def_index; }

  void Set_use_count(INT count) { _use_count = count; }
  void Set_use_index(INT index) { u2._use_index = index; }

  INT Get_use_count() const { return _use_count; }
  INT Get_use_index() const { return u2._use_index; }

  void Set_param_count(INT count) { _param_count = count; }
  void Set_param_index(INT index) { u3._param_index = index; }

  INT Get_param_count() const { return _param_count; }
  INT Get_param_index() const { return u3._param_index; }

  void Set_scalar_count(INT count) { _scalar_count = count; }
  void Set_scalar_index(INT index) { u4._scalar_index = index; }

  INT Get_scalar_count() const { return _scalar_count; }
  INT Get_scalar_index() const { return u4._scalar_index; }

  void Set_formal_count(INT count) { _formal_count = count; }
  void Set_formal_index(INT index) { u6._formal_index = index; }

  INT Get_formal_count() const { return _formal_count; }
  INT Get_formal_index() const { return u6._formal_index; }

  void Set_loop_index(INT index) { u5._index = index; }
  INT  Get_loop_index() const { return u5._index; }

  // used if the type of the cfg node is CFG_IF
  void Set_else_index(INT index) { 
    if (Is_if()) 
      u5._index = index;
    else
      Fail_FmtAssertion("Invalid type when setting else index for CFG_NODE\n");
  };

  INT Get_else_index() { 
    if (Is_if())
      return u5._index;
    else
      Fail_FmtAssertion("Invalid type when using Get else index for CFG_NODE");
    return -1;
  };

  // used if the type of the cfg node is CFG_ELSE
  void Set_if_index(INT index) { 
    if (Is_else()) 
      u5._index = index;
    else
      Fail_FmtAssertion("Invalid type when using Set_if_index for CFG_NODE");
  };
  
  // used if the type of the cfg node is CFG_IF
  INT Get_if_index()  {
    if (Is_else())
      return u5._index;
    else
      Fail_FmtAssertion("Invalid type when using Get_if_index for CFG_NODE");
    return -1;
  };

};


extern void Init_ivar_arrays();

typedef DYN_ARRAY<CFG_NODE_INFO> CFG_NODE_INFO_ARRAY;

//--------------------------------------------------------------------------
// ids for mapping the scalar info ids in the summary actual nodes
//--------------------------------------------------------------------------
class INT_IDS
{
private:
  INT32 _id; 
  INT32 _cd_idx;

public:
  INT32 Get_id() const { return _id;};
  void Set_id(INT32 i) { _id = i;};

  INT32 Get_cd_idx() const { return _cd_idx;};
  void Set_cd_idx(INT32 t) {  _cd_idx = t;};
};

//----------------------------------------------------------------------
// construct the tlog information
//----------------------------------------------------------------------
class TLOG_INFO
{
private:
  INT _cterm_count, _lterm_count, _iv_g_term_count,  _iv_term_count;
  INT _sub_term_count;

public:
  TLOG_INFO() { BZERO(this, sizeof(TLOG_INFO)); };
  INT& Get_cterm_count()  { return _cterm_count;};
  INT& Get_lterm_count()  { return _lterm_count;};
  INT& Get_iv_gterm_count()  { return _iv_g_term_count;};
  INT& Get_iv_term_count()  { return _iv_term_count;};
  INT& Get_sub_term_count() { return _sub_term_count;};

  void Set_cterm_count(INT count ) {_cterm_count = count;};
  void Set_lterm_count(INT count ) {_lterm_count = count;};
  void Set_iv_gterm_count(INT count) {_iv_g_term_count = count;};
  void Set_iv_term_count(INT count ) {_iv_term_count = count;};
  void Set_sub_term_count(INT count ) {_sub_term_count = count;};
 
};

//--------------------------------------------------------------------------
// array section summary information 
//--------------------------------------------------------------------------
class ARRAY_SUMMARY
{
private:
  MEM_POOL _array_pool;
  MEM_POOL _local_array_pool;
  MEM_POOL _write_pool;
  IVAR_ARRAY *_ivar;
  TERM_ARRAY *_term_array;
  PROJECTED_ARRAY *_project_nodes;
  PROJECTED_REGION_ARRAY *_projected_regions;
  ARRAY_OF_REGION_ARRAYS *_region_arrays;
  CFG_NODE_INFO_ARRAY *_cfg_nodes;
  LOOPINFO_ARRAY *_loop_nodes;
  INT_IDS *_actual_scalar_info_map;
  INT *_cd_map; // map the cd index to the "real cd index"
  INT _formal_start_idx;
  INT _formal_count;
  INT  _actual_start_idx;
  INT _actual_count;
  INT  _callsite_start_idx;
  INT _callsite_count;
  TLOG_INFO *_tlog_info;

public:
  void Init(INT formal_count, INT formal_idx, 
	    INT actual_count, INT actual_idx,
	    INT callsite_count, INT callsite_idx, 
            INT cd_size)
    {
      MEM_POOL_Initialize(&_array_pool, "array section pool", 0);
      MEM_POOL_Initialize(&_local_array_pool, "local array pool", 0);
      MEM_POOL_Initialize(&_write_pool, "write array pool", 0);
      MEM_POOL_Push(&_array_pool);
      MEM_POOL_Push(&_local_array_pool);
      MEM_POOL_Push(&_write_pool);
      _ivar = CXX_NEW(IVAR_ARRAY(&_write_pool), &_write_pool);
      _term_array = CXX_NEW(TERM_ARRAY(&_write_pool), &_write_pool);
      _project_nodes = CXX_NEW(PROJECTED_ARRAY(&_write_pool),
			       &_write_pool);
      _projected_regions =
	CXX_NEW(PROJECTED_REGION_ARRAY(&_write_pool), &_write_pool);
      _region_arrays =
	CXX_NEW(ARRAY_OF_REGION_ARRAYS(&_write_pool), &_write_pool);
      _cfg_nodes =
	CXX_NEW(CFG_NODE_INFO_ARRAY(&_write_pool), &_write_pool);
      _loop_nodes =
	CXX_NEW(LOOPINFO_ARRAY(&_write_pool), &_write_pool);
      _actual_scalar_info_map = (INT_IDS*)
	MEM_POOL_Alloc(&_write_pool, sizeof(INT_IDS)*(actual_count+1));
      _formal_start_idx = formal_idx;
      _formal_count = formal_count;
      _actual_start_idx = actual_idx;
      _actual_count = actual_count;
      _callsite_start_idx = callsite_idx;
      _callsite_count = callsite_count;
      BZERO(_actual_scalar_info_map, sizeof(INT_IDS)*(actual_count+1));
      if (cd_size)
	_cd_map = (INT*)
	  MEM_POOL_Alloc(&_write_pool, sizeof(INT)*cd_size);
      _tlog_info = CXX_NEW(TLOG_INFO(), &_write_pool);;
      Init_ivar_arrays();
      //Set_Trace_Sections();
    };

  void Finalize() 
    {
      MEM_POOL_Pop(&_array_pool);
      MEM_POOL_Pop(&_local_array_pool);
      MEM_POOL_Pop(&_write_pool);

      MEM_POOL_Delete(&_array_pool);
      MEM_POOL_Delete(&_local_array_pool);
      MEM_POOL_Delete(&_write_pool);
    };
  
  MEM_POOL* Get_local_pool()  { return &_local_array_pool;};
  MEM_POOL* Get_array_pool()  { return &_array_pool;};
  MEM_POOL* Get_write_pool() { return &_write_pool;};
  
  IVAR_ARRAY* Get_ivar_array() const { return _ivar;};
  TERM_ARRAY* Get_term_array()  { return _term_array;};
  PROJECTED_ARRAY* Get_projected_array() 
    { return _project_nodes; };
  PROJECTED_REGION_ARRAY* Get_projected_region_array() 
    { return _projected_regions;};
  ARRAY_OF_REGION_ARRAYS* Get_region_array() 
    { return _region_arrays; };
  CFG_NODE_INFO_ARRAY *Get_cfg_node_array() 
    { return _cfg_nodes;};
  LOOPINFO_ARRAY *Get_loopinfo_array()
    { return _loop_nodes;};

  INT_IDS* Get_actual_scalar_info_map() { return _actual_scalar_info_map;};

  INT Get_actual_scalar_info_id(INT id) { 
    return _actual_scalar_info_map[id].Get_id();
  };

  INT Get_actual_scalar_info_cd_idx(INT id) {
    return _actual_scalar_info_map[id].Get_cd_idx();
  };

  void Set_actual_scalar_info_map(INT id, INT cd_idx, INT actual_id)  { 
    _actual_scalar_info_map[actual_id].Set_id(id);
    _actual_scalar_info_map[actual_id].Set_cd_idx(cd_idx);
  };

  INT* Get_cd_map() const { return _cd_map;};
  IVAR* Get_ivar_array(INT i);
  TERM* Get_term_array(INT i);
  PROJECTED_NODE* Get_projected_array(INT i);
  PROJECTED_REGION* Get_projected_region_array(INT i);
  REGION_ARRAYS* Get_region_array(INT i); 
  CFG_NODE_INFO *Get_cfg_node_array(INT i); 
  LOOPINFO *Get_loopinfo_array(INT i);
  TLOG_INFO* Get_tlog_info() const { return _tlog_info;};

  INT Get_ivar_array_count() const { return _ivar->Lastidx()+1;};
  INT Get_term_array_count()  const { return _term_array->Lastidx()+1;};
  INT Get_projected_array_count() const
    { return _project_nodes->Lastidx() + 1; };
  INT Get_projected_region_array_count()  const
    { return _projected_regions->Lastidx() + 1;};
  INT Get_region_array_count() const
    { return _region_arrays->Lastidx() + 1; };
  INT Get_cfg_node_array_count() const
    { return _cfg_nodes->Lastidx() + 1;};
  INT Get_loopinfo_array_count() const
    { return _loop_nodes->Lastidx() + 1;};
  INT Get_formal_start_idx() const
    { return _formal_start_idx;};
  INT Get_formal_count() const
    { return _formal_count;};
  INT Get_actual_start_idx() const
    { return _actual_start_idx;};
  INT Get_actual_count() const
    { return _actual_count;};
  INT Get_callsite_start_idx() const
    { return _callsite_start_idx;};
  INT Get_callsite_count() const
    { return _callsite_count;};

  void Record_tlogs(TERM_ARRAY* t, INT offset);
};

#endif
