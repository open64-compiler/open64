/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company, 
//  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// ====================================================================
//
#ifndef opt_lmv_helper_INCLUDED
#define opt_lmv_helper_INCLUDED

#include "defs.h"
#include "opt_defs.h"
#include "cxx_base.h"
#include "id_map.h"
#include "opt_htable.h"
#include "cxx_memory.h"

// classes used to depict memory operations in particular loop
class MEM_ACCESS;
class MA_POINTER;
class MA_OFFSET;
class MA_PTR_MGR;
class MEM_ACCESS_ANALYZER;
class MEM_GROUP;

// classes about cost model 
class LMV_HEURISTIC; 

// classes about code generator
class LMV_CANDIDATE; 

// Misc classes
class ADDR_LINEAR_EXPR;
class MEM_RANGE;
class LMV_LOOP_INFO;
class VAR_VAL_RANGE;
class LOOP_MULTIVER;

// define the type of vector<MA_PTR_ALLOC*>
typedef mempool_allocator<MA_POINTER*>  MA_PTR_ALLOC;
typedef std::vector<MA_POINTER*, MA_PTR_ALLOC>  MA_PTR_VECT;
typedef MA_PTR_VECT::iterator MA_PTR_VECT_ITER;

// define the type of vector<MA_PTR_ALLOC*>
typedef mempool_allocator<MEM_ACCESS*>  MEM_ACCESS_ALLOC;
typedef std::vector<MEM_ACCESS*, MEM_ACCESS_ALLOC>  MEM_ACCESS_VECT;
typedef MEM_ACCESS_VECT::iterator MEM_ACCESS_VECT_ITER;
typedef MEM_ACCESS_VECT::const_iterator MEM_ACCESS_VECT_CITER;

typedef mempool_allocator<MEM_GROUP *> MEM_GROUP_ALLOC;
typedef std::vector<MEM_GROUP*, MEM_GROUP_ALLOC> MEM_GROUP_VECT;
typedef MEM_GROUP_VECT::iterator MEM_GROUP_VECT_ITER;
typedef MEM_GROUP_VECT::const_iterator MEM_GROUP_VECT_CITER;


////////////////////////////////////////////////////////////////////////////////
//
//   LMV_LOOP_INFO: Collect the loop info 
//
////////////////////////////////////////////////////////////////////////////////
//
class LMV_LOOP_INFO {
private:  
  MEM_POOL* _mp;
  BB_LOOP*  _loop;
  BOOL      _trace;
  ID_MAP<VAR_VAL_RANGE*, AUX_ID> _val_range_map;
  ID_MAP<IV_CAND*, AUX_ID> _iv_map;

public:
  LMV_LOOP_INFO (BB_LOOP* loop, MEM_POOL* mp,
                 IVR &ivr, BOOL tracing);

  BB_LOOP* Loop(void) const { return  _loop; }

  ID_MAP<IV_CAND *,AUX_ID> &Iv_map(void) { return _iv_map; }

  BOOL Equivalent_iv(CODEREP *coderep, AUX_ID aux_id);
  BOOL Get_iv_upperbound (IV_CAND *iv,VAR_VAL_RANGE *vr);

  // Analyze the value range of <var> of given <ver>. If <ver> is 
  // not specified (i.e ver=0), this function tries to figure out 
  // the range of possible values of <var> used within current 
  // loop regardless its verion. 
  // 
  // The analysis is done in conservative manner, meaning the returned
  // value-range always covers the all possible values.
  //
  void Analyze_var_value_range (AUX_ID var, VER_ID ver, VAR_VAL_RANGE* val);
};

////////////////////////////////////////////////////////////////////////////////
//
//    ADDR_LINEAR_EXPR: describe linear expr, used by offset etc.
//
////////////////////////////////////////////////////////////////////////////////
//
class ADDR_LINEAR_EXPR {
private:
  enum {
    ALE_CONST = 1, 
    ALE_NON_CONST = 2, 
  };
  UINT _flags;

  // a constant linear expr has value <_const>, a non-constant 
  // linear expr is in the form of "_coeff * _var + _const"
  INT _coeff; 
  INT _const; 
  CODEREP *_cr;

  void Reset_const (void) { _flags &= ~ALE_CONST; }
  void Reset_nonconst (void) { _flags &= ~ALE_NON_CONST; }

  void Set_const (void) { Reset_nonconst (); _flags |= ALE_CONST; }
  void Set_nonconst (void) { Reset_const(); _flags |= ALE_NON_CONST; }

  void Chk_non_const (void) const {
         Is_True (_flags & ALE_NON_CONST, ("should be non-const expr"));
       }
  void Chk_const (void) const {
         Is_True (_flags & ALE_CONST, ("should be non-const expr"));
       }

public:
  void Set_linear_expr
           (INT coeff, CODEREP *cr, INT const_part) {
           _coeff = coeff; _cr = cr; _const = const_part;
           if (cr != NULL) Set_nonconst(); else Set_const();
         }

  void Set_const_expr (INT val) { Set_const(); _const = val; }

  BOOL Is_const (void) const { return _flags & ALE_CONST; }
  BOOL Is_nonconst (void) const { return _flags & ALE_NON_CONST; }
  BOOL Is_invalid (void) const { return _flags == 0; }

  INT Coefficient (void) const { Chk_non_const (); return _coeff; }
  INT Const_part (void) const { Chk_non_const (); return _const; } 
  CODEREP *cr(void) const { Chk_non_const(); return _cr; }
  AUX_ID Var (void) const { Chk_non_const (); return _cr->Aux_id(); }
  VER_ID Var_ver (void) const { Chk_non_const(); return _cr->Version(); }

  INT Const_val (void) const { Chk_const (); return _const; }

  inline BOOL operator == (const ADDR_LINEAR_EXPR& that) const; 

  BOOL Add (const ADDR_LINEAR_EXPR &that);
  void Add (INT v) { _const += v; }
  void Multiply (INT);

  void Get_range (VAR_VAL_RANGE*, LMV_LOOP_INFO*) const;

  ADDR_LINEAR_EXPR (void) { _flags = 0; }
  ADDR_LINEAR_EXPR (INT v) { _flags = 0; Set_const_expr (v); }
  ADDR_LINEAR_EXPR (INT coeff, CODEREP *cr, INT c)
     { _flags = 0; Set_linear_expr (coeff, cr, c); }
  void Init (void) { _flags = 0; }
  void Print (FILE* f);
};

class ADDR_LINEAR_EXPR_RANGE {
public:
  ADDR_LINEAR_EXPR low, high;  
  void Print (FILE* f);
};

////////////////////////////////////////////////////////////////////////////////
//
//    MA_POINTER: class depicting pointer 
//
////////////////////////////////////////////////////////////////////////////////
//
// kind of pointer
typedef enum {
  MA_PTR_INVALID   = 0, 
  MA_PTR_PREG      = 1, 
  MA_PTR_SYM       = 2, 
  MA_PTR_INDIRECT  = 3,
  MA_PTR_LDA       = 4,
  MA_PTR_TOO_MESSY = 5,
} MA_PTR_KIND;

class MA_POINTER {
friend class MA_PTR_MGR;
private:
  INT _id;
  MA_PTR_KIND _kind;
  CODEREP* _cr; // the corresponding CODEREP
  MEM_ACCESS_VECT _mem_access;  // all memops via this pointer

  union {
    struct { PREG_NUM _preg_num; } _is_preg;
    struct { AUX_ID _aux_id; ST* _name; } _is_sym; 
    struct { AUX_ID _aux_id; ST* _base; UINT _afield_id; } _is_lda;
    struct { MEM_ACCESS* _ma; } _is_indirect;
  };
  VER_ID _ver;
  TY_IDX _ty;           // ty of this pointer 

  INT _ind_level;
  INT _ld_cnt, _st_cnt; // number of ld/st through this ptr

  enum {
    MA_LOOP_INVAR = 1, 
  };
  INT32  _flags;

  // Private public constructor prevent any class except PTR_MGR from 
  // allocating a instance.
  // 
  MA_POINTER (INT id)
    { _id = id, _kind = MA_PTR_INVALID; _flags = 0, _ver = 0; _ty = (TY_IDX)0; }

public:
  INT Id(void) const { return _id; }

  void Init_ptr_as_preg (PREG_NUM preg, VER_ID ver, TY_IDX ty, CODEREP* cr)
       { _kind = MA_PTR_PREG, _ver = ver, _ty = ty, _cr = cr,
         _ld_cnt = _st_cnt = 0;
       }

  void Init_ptr_as_named_sym (AUX_ID aux, VER_ID ver, ST* name, 
                              TY_IDX ty, CODEREP* cr)
       { _kind = MA_PTR_SYM, _ver = ver, _is_sym._aux_id = aux, 
         _is_sym._name = name, _ty = ty, _ld_cnt = _st_cnt = 0; 
       }

  void Init_too_messy_ptr (TY_IDX ty, CODEREP* cr) 
       { _kind = MA_PTR_TOO_MESSY, _ver=(TY_IDX)0, _ty = ty, _cr = cr,
         _ld_cnt = _st_cnt = 0, _ind_level = 1;
       }

  void Init_indirect_ptr (MEM_ACCESS* ptr_ld, TY_IDX ty, 
                          INT ind_level, CODEREP* cr)
       { _kind = MA_PTR_INDIRECT, _is_indirect._ma = ptr_ld, 
         _ver = (TY_IDX)0, _ty = ty, _ld_cnt = _st_cnt = 0, 
         _cr = cr, _ind_level = ind_level;
       }

  void Init_lda_ptr (AUX_ID auxid, ST* base_st, TY_IDX ptr_ty, 
                     UINT afield_id, CODEREP* addr) 
       {
         _kind = MA_PTR_LDA, _is_lda._aux_id = auxid, _is_lda._base = base_st, 
	 _is_lda._afield_id = afield_id, _ver = 0, _ty = ptr_ty, _cr = addr, 
	 _ind_level =1;
       }

  CODEREP* Coderep (void) const { return _cr; }
  void Set_coderep (CODEREP* cr) { _cr = cr; }

  MA_PTR_KIND Kind (void) const { return _kind; }
  void Set_kind (MA_PTR_KIND kind) { _kind = kind; }

  VER_ID Ver (void) const {
        Is_True (_kind == MA_PTR_PREG || _kind == MA_PTR_SYM, 
                 ("version make sense only to PREG or named pointer"));
        return _ver;
      }

  void Set_ver (INT ver) {
        Is_True (_kind == MA_PTR_PREG || _kind == MA_PTR_SYM, 
                 ("version make sense only to PREG or named pointer"));
        _ver = ver;
      }
  
  // all about type
  TY_IDX Ty (void) const { return _ty; }
  void Set_ty (TY_IDX ty) { _ty = ty; }
  TY_IDX Pointed_ty (void) const;

  // return TRUE iff pointer is loop invariant.   
  BOOL Is_loop_invar (void) { return _flags & MA_LOOP_INVAR; }
  void Set_loop_invar (void) { _flags |= MA_LOOP_INVAR; }
  void Reset_loop_invar (void) { _flags &= ~MA_LOOP_INVAR; }

  INT Ld_cnt (void) const { return _ld_cnt; }
  INT St_cnt (void) const { return _st_cnt; }
  void Set_ld_cnt (INT c) { _ld_cnt = c; }
  void Set_st_cnt (INT c) { _st_cnt = c; }

  INT Indirect_level (void) const { return _ind_level; }
  void Set_indirect_level (INT l) { _ind_level = l; }  

  MEM_ACCESS_VECT& All_mem_access (void) { return _mem_access; }
  void Add_mem_access (MEM_ACCESS* ma) { _mem_access.push_back (ma); }

  void Print (FILE* f, BOOL verbose=FALSE);
};

////////////////////////////////////////////////////////////////////////////////
//
//    MA_OFFSET: class depicting mem-op offset 
//
////////////////////////////////////////////////////////////////////////////////
//
// kind of offset
typedef enum {
  MA_OFST_INVALID = 0, 
  MA_OFST_FIXED   = 1, 
  MA_OFST_LINEAR = 2, // linear expr of primary-IV.
  // the offset falls into a constant range
  MA_OFST_RANGE = 3, 
  MA_OFST_TOO_MESSY = 4, 
  MA_OFST_UNKNOWN = 5, 
} MA_OFST_KIND;

class MA_OFFSET {
private:
  MA_OFST_KIND _kind;
  CODEREP* _expr;

  // use _ofst.low only if _kind != MA_OFST_RANGES
  ADDR_LINEAR_EXPR_RANGE _ofst; 

public:

  MA_OFST_KIND Kind (void) const { return _kind; } 

  CODEREP* Expr (void) const { return _expr; }
  void Set_expr (CODEREP* cr) { _expr = cr; }

  void Set_linear_ofst (INT coeff, CODEREP *cr, INT const_part)
     { _kind = (cr != NULL) ? MA_OFST_LINEAR: MA_OFST_FIXED;
       _ofst.low.Set_linear_expr (coeff, cr, const_part);
     }
  void Set_fixed_ofst (INT ofst) 
     { _kind = MA_OFST_FIXED; _ofst.low.Set_const_expr (ofst); }

  INT Get_fixed_ofst (void) const { 
        Is_True (_kind == MA_OFST_FIXED, ("offset should be fixed"));
        return _ofst.low.Const_val();
     }

  const ADDR_LINEAR_EXPR* Linear_ofst (void) const { 
        Is_True (_kind == MA_OFST_LINEAR || _kind == MA_OFST_FIXED, 
	         ("offset is not a linear exprr"));
        return &_ofst.low;
     }

  const ADDR_LINEAR_EXPR_RANGE* Linear_ofst_range (void) const {
        Is_True (_kind == MA_OFST_RANGE, 
	         ("offset should be a range of linear expr"));
        return &_ofst;
     }
  void Set_linear_ofst_range (const ADDR_LINEAR_EXPR& low, 
                              const ADDR_LINEAR_EXPR& high) {
       _kind = MA_OFST_RANGE; _ofst.low = low; _ofst.high = high; 
     }

  void Set_messy_ofst (CODEREP* cr) 
     { _kind = MA_OFST_TOO_MESSY, _expr = cr; }

  void Set_unknown_ofst (void) { _kind = MA_OFST_UNKNOWN; }

  BOOL operator == (const MA_OFFSET& t) const; 
  void Add (INT adjust);
  void Add (MA_OFFSET* adjust);

  // work out a offset range that cover both <this> and <that>
  void Union (const MA_OFFSET* that, LMV_LOOP_INFO* loopinfo);

  MA_OFFSET (void) { _kind = MA_OFST_INVALID; _expr = NULL; }
  MA_OFFSET (const ADDR_LINEAR_EXPR* la) {
     _kind = MA_OFST_INVALID; _expr = NULL;
     if (la->Is_const ()) { 
       Set_fixed_ofst (la->Const_val());
     } else if (la->Is_nonconst()) {
       Set_linear_ofst (la->Coefficient (), la->cr(), la->Const_part());
     } else {
       FmtAssert (FALSE, ("Invalid linear address"));
     }
  }

  void Print (FILE* f);
};

////////////////////////////////////////////////////////////////////////////////
//
// MA_PTR_MGR: 
//     As its name suggests, it is the "manager" of all MA_POINTERs. It responsible
//  for: 
//    - allocating MA_POINTER, MA_OFFSET, PTR_OFST_PAIR -- 
//      MA_POINTER/MA_OFFSET/PTR_OFST_PAIR  instances cannot be allocated by class 
//      other than MA_PTR_MGR. The private construction of MA_POINTER guarantee that.
//   
//    - maintaining a map between MA_POINTER instances and their IDs.
//
//    - associating CODEREPs with their corresonding PTR_OFST_PAIRs. 
//
////////////////////////////////////////////////////////////////////////////////
//
class PTR_OFST_PAIR {
public:
  MA_POINTER* first;
  MA_OFFSET* second;

  PTR_OFST_PAIR (MA_POINTER* ptr, MA_OFFSET* ofst) 
    { first = ptr, second = ofst; }
  
  void Print (FILE* f) {};
};

class MA_PTR_MGR {
private:
  MEM_POOL* _mp;
  MA_PTR_VECT _all_ptrs;
  ID_MAP<PTR_OFST_PAIR*,INT32> _cr_ptr_info;

  INT _next_ptr_id;  

public:
  MA_PTR_MGR (MEM_POOL* mp) : _mp(mp), _all_ptrs(mp), 
    _cr_ptr_info(256, NULL, mp, FALSE) 
    { _next_ptr_id=1; _cr_ptr_info.Init(); }
  ~MA_PTR_MGR (void) {}

  PTR_OFST_PAIR* Ptr_ofst (CODEREP* cr) 
    { return _cr_ptr_info.Lookup (cr->Coderep_id()); } 

  void Set_cr_ptr_ofst (CODEREP* cr, MA_POINTER* ptr, MA_OFFSET* ofst) 
    { PTR_OFST_PAIR* pair = CXX_NEW(PTR_OFST_PAIR(ptr, ofst), _mp);
      _cr_ptr_info.Insert (cr->Coderep_id(), pair);
    }
 
  MA_POINTER* Alloc_ptr (void) {
      MA_POINTER* t = CXX_NEW (MA_POINTER(_next_ptr_id++), _mp); 
      _all_ptrs.push_back (t); 
      return t;
    }
  MA_OFFSET* Alloc_ofst (void) { return CXX_NEW (MA_OFFSET, _mp); }

  PTR_OFST_PAIR* Alloc_ptr_ofst_pair (MA_POINTER* ptr=NULL, MA_OFFSET* ofst=NULL)
    { return CXX_NEW (PTR_OFST_PAIR(ptr, ofst), _mp); }

  void Map_pointer (CODEREP* cr, PTR_OFST_PAIR* ptr_ofst_pair) 
    { _cr_ptr_info.Insert (cr->Coderep_id(), ptr_ofst_pair); }

  PTR_OFST_PAIR* Get_pointer_ofst_pair (CODEREP* cr) const 
    { return _cr_ptr_info.Lookup (cr->Coderep_id()); }

  MA_POINTER* Get_pointer (INT ptid) const 
    {
      if (ptid < 1 || ptid >= _next_ptr_id) return NULL; 
      MA_POINTER* ptr = _all_ptrs[ptid-1];
      Is_True (ptr->Id() == ptid, ("id is inconsistent!"));
      return ptr;
    }

  MA_PTR_VECT& All_ptrs (void) { return _all_ptrs; }
  INT Ptr_sum (void) const { return _all_ptrs.size (); }
  INT Next_ptr_id(void) const { return _next_ptr_id; } 

  void Print (FILE* f);
};


////////////////////////////////////////////////////////////////////////////////
//
//   class MEM_ACCESS : describe a memory operation
//
////////////////////////////////////////////////////////////////////////////////
//
class MEM_ACCESS {
friend class MEM_ACCESS_ANALYZER; 

private:
  INT32  _id;     // every mem access has unique id
  union {
    CODEREP* _coderep; // when _flags & MA_READ 
    STMTREP* _stmtrep; // when _flags && MA_WRITE
  };
  union {
    ST* _st;       // not indrect access
    MA_POINTER* _ptr;       
  };
  MA_OFFSET _ofst;
  mINT64 _size;   // size in byte

  UINT32 _field_id; 
  TY_IDX _obj_ty, _hl_ty;  

  INT32 _flags;
  enum {
    MA_READ = 1, 
    MA_WRITE = 2, 
  };

  // misc
  INT _ind_level;  // indirect access level
  INT _group;

  MEM_ACCESS (INT id) { 
    _ind_level = -1, _coderep = NULL, _st = NULL, 
    _size = 0, _id = id, _field_id = 0, 
    _obj_ty = _hl_ty = (TY_IDX)0, _group = 0, _flags = 0;
  }

public:
  INT32 Id (void) const { return _id; }
 
  INT Indirect_level (void) const { return _ind_level; } 
  void Set_indirect_level (INT level) { _ind_level = level; }

  TY_IDX Obj_ty (void) const { return _obj_ty; }
  void Set_obj_ty (TY_IDX ty) { _obj_ty = ty; }

  TY_IDX Highlevel_ty (void) const { return _hl_ty; }
  void Set_hl_ty (TY_IDX ty) { _hl_ty = ty; }

  UINT32 Field_id (void) const { return _field_id; } 
  void Set_field_id (UINT32 id) { _field_id = id; }

  mINT64 Byte_size (void) const { return _size; }
  void Set_byte_size (mINT64 sz) { _size = sz; }

  MA_OFFSET& Ofst (void) { return _ofst ; }
  void Set_ofst (MA_OFFSET ofst) { _ofst = ofst; }

  INT Group (void) const { return _group; }
  void Set_group (INT grp) { _group = grp; }

  // the last pointer the multi-level indirect access, 
  // e.g the "ultimate" pointer of "*((*p)->field" is "p".
  MA_POINTER* Ultimate_ptr (void); 

  BOOL Is_read (void) const { return _flags & MA_READ; }
  void Set_is_read (CODEREP* cr) 
    { _flags &= ~MA_WRITE; _flags |= MA_READ; _coderep = cr; }

  BOOL Is_write (void) const { return _flags & MA_WRITE; }
  void Set_is_write (STMTREP* sr) 
    { _flags &= ~MA_READ; _flags |= MA_WRITE; _stmtrep = sr; }

  void Set_ptr(MA_POINTER* ptr) { _ptr = ptr; }
  MA_POINTER* Ptr(void) const {
    Is_True (_ind_level != 0, ("direct ld/st should not have pointer"));
    return _ptr;
  }
  void Adjust_ofst (MA_POINTER* ptr, MA_OFFSET* ofst); 

  void Set_st (ST* st) {
    Is_True (_ind_level == 0, ("indirect ld/st is not associated with symbol"));
    _st = st; 
  }

  ST* St (void) const { 
    Is_True (_ind_level == 0, ("indirect ld/st is not associated with symbol"));
    return _st;
  }

  CODEREP* Coderep (void) const 
     { return Is_read() ?_coderep:_stmtrep->Lhs(); }
  STMTREP* Stmtrep (void) const { 
       Is_True (Is_write(), ("stmt is recorded only when access is write"));
       return _stmtrep;
     }

  POINTS_TO* Points_to (OPT_STAB* opt_stab) 
    { return Coderep()->Points_to(opt_stab); }

  void Print (FILE* f);
};

class MEM_GROUP {
private:
  BOOL _write;      // Does the memory group contain a write?
  MEM_ACCESS_VECT&  _mem_accesses;
  MEM_RANGE *       _mem_range;

public:
  MEM_GROUP(MEM_ACCESS_VECT &accesses, MEM_RANGE *range, BOOL write)
  : _write(write),
    _mem_accesses(accesses),
    _mem_range(range)
    {}

  ~MEM_GROUP() {}

  BOOL Write(void) const { return _write; }
  MEM_ACCESS_VECT& Mem_accesses(void) { return _mem_accesses; }
  MEM_RANGE* Mem_range(void) { return _mem_range; }

  void Print(FILE *f);
};

////////////////////////////////////////////////////////////////////////////////
//
//   class MEM_ACCESS_ANALYZER: Analyzer of the mem-ops of given innermost loop
//
////////////////////////////////////////////////////////////////////////////////
//
class MEM_ACCESS_ANALYZER {
friend class LMV_HEURISTIC;
private:
  BOOL _trace;
  BOOL _trace_detail;
  MEM_POOL* _mp;

  OPT_STAB* _opt_stab;
  CFG* _cfg;
  LMV_LOOP_INFO* _loopinfo;

  ID_MAP<MEM_ACCESS* ,INT32> _ma_map; 
  MEM_ACCESS_VECT _all_ma;

  MA_PTR_MGR _ptr_mgr;
  
  // misc
  INT _read_cnt, _write_cnt;
  INT _last_ma_id;

  BOOL Expr_of_ptr_ty (CODEREP* cr);

  PTR_OFST_PAIR* Analyze_preg_pointer (CODEREP* addr, BOOL is_read);
  PTR_OFST_PAIR* Analyze_iload_pointer (CODEREP*, STMTREP*, BOOL is_read);
  PTR_OFST_PAIR* Analyze_array_access (CODEREP*, STMTREP*, BOOL);
  PTR_OFST_PAIR* Analyze_named_symbol_pointer (CODEREP* , BOOL is_read);
  PTR_OFST_PAIR* Analyze_lda_pointer (CODEREP* , BOOL is_read);
  PTR_OFST_PAIR* Analyze_pointer (CODEREP*, STMTREP*, BOOL is_read);
  PTR_OFST_PAIR* Gen_messy_access_pair (CODEREP*, STMTREP*, BOOL);
  void Analyze_ofst_helper (CODEREP* ofst, MA_OFFSET* res);
  void Analyze_ofst (CODEREP* ofst, MA_OFFSET* res);

  MEM_ACCESS* Analyze_mem_access (CODEREP*, STMTREP*, BOOL is_read);

  inline MEM_ACCESS* Alloc_mem_access (void);
  
  // Access the MEM_ACCESS associated with <cr>
  void Map_mem_access (CODEREP* cr, MEM_ACCESS* ma) 
        { _ma_map.Insert (cr->Coderep_id(), ma); }
  MEM_ACCESS* Get_mem_access (CODEREP* cr) 
        { return _ma_map.Lookup (cr->Coderep_id()); }

  void Map_pointer (CODEREP* cr, PTR_OFST_PAIR* ptr_ofst_pair)
        { _ptr_mgr.Map_pointer (cr, ptr_ofst_pair); }

  PTR_OFST_PAIR* Get_pointer_ofst_pair (CODEREP* cr)
        { return _ptr_mgr.Get_pointer_ofst_pair (cr); }

  // return TRUE iff cr is loop invariant. 
  BOOL Is_loop_invariant (CODEREP* cr) const 
    { return _loopinfo->Loop()->Invariant_cr_rec(cr); }

public:
  MEM_ACCESS_ANALYZER 
    (OPT_STAB* opt_stab, LMV_LOOP_INFO* loopinfo, 
     MEM_POOL* mp, BOOL trace); 

  ~MEM_ACCESS_ANALYZER (void) {};

  void Analyze_mem_access (void);

  BOOL Assemble_aliased_mem_groups(const ALIAS_RULE *alias_rule,
                                   MEM_GROUP_VECT &groups);

  void Print (FILE* f);
};


////////////////////////////////////////////////////////////////////
//
//     Miscellaneous classes
//
///////////////////////////////////////////////////////////////////
//
class MEM_RANGE {
private:
  UINT32 _flags;
  enum {
    BASE_IS_SYMBOL = 0x1,
    BASE_IS_PTR    = 0x2,
    BASE_IS_INVALID = 0x4,
  };
  union {
    ST* _base_sym;
    MA_POINTER* _base_ptr;
  };

  ADDR_LINEAR_EXPR_RANGE _access_range;

public:
  void Init (void) { _flags = BASE_IS_INVALID, _base_sym = NULL, 
                     _access_range.low.Init(); 
                     _access_range.high.Init(); 
                   }
  MEM_RANGE (void) { Init(); }

  BOOL Base_is_symbol (void) const { return _flags & BASE_IS_SYMBOL; } 
  void Set_base_sym (ST* sym) { _flags |= BASE_IS_SYMBOL; _base_sym = sym; }
  ST* Base_sym (void) const { return Base_is_symbol() ? _base_sym : NULL; } 

  BOOL Base_is_ptr (void) const { return _flags & BASE_IS_PTR ; }
  MA_POINTER* Base_ptr (void) const { return Base_is_ptr() ? _base_ptr : NULL; }
  void Set_base_ptr (MA_POINTER* ptr) { _flags |= BASE_IS_PTR, _base_ptr = ptr;}

  void Set_access_range (const ADDR_LINEAR_EXPR_RANGE& r) {_access_range = r; }
  const ADDR_LINEAR_EXPR_RANGE& Access_range (void) const { return _access_range;} 
  
  void Set_access_range (MA_OFFSET* ofst, LMV_LOOP_INFO* loopinfo, INT access_sz);
  
  void Print (FILE*);
};

typedef enum {
  RC_LT, RC_GT, RC_IN_RANGE, RC_UNKNOWN,
} RANGE_COMP;

class VAR_VAL_RANGE {
private:
  union { INT _low_val; CODEREP* _low_cr; };
  union { INT _high_val; CODEREP* _high_cr; };
  enum { 
    LOW_IS_CONST = 1, 
    LOW_IS_CR = 2, 
    LOW_IS_INVALID = 4,
    HIGH_IS_CONST  = 8,
    HIGH_IS_CR = 16, 
    HIGH_IS_INVALID = 32,
    HIGH_IS_CR_SUBONE = 64,
  };
  UINT _flags;
  
  void Set_low_is_const (void) 
    { _flags &= ~(LOW_IS_CR|LOW_IS_INVALID), _flags |= LOW_IS_CONST; }
  void Set_low_is_cr (void) 
    { _flags |= LOW_IS_CR, _flags &= ~(LOW_IS_CONST|LOW_IS_INVALID); }

  void Set_high_is_const (void) 
    { _flags &= ~(HIGH_IS_CR|HIGH_IS_INVALID), _flags |= HIGH_IS_CONST; }
  void Set_high_is_cr (void) 
    { _flags |= HIGH_IS_CR, _flags &= ~(HIGH_IS_CONST|HIGH_IS_INVALID); }


public:
  void Init (void) {_flags = HIGH_IS_INVALID|LOW_IS_INVALID; }
  VAR_VAL_RANGE (void) { Init(); }
  
  BOOL Low_is_const (void) const { return _flags & LOW_IS_CONST; }
  BOOL Low_is_cr (void) const { return _flags & LOW_IS_CR; }
  BOOL Low_is_invalid(void) const { return _flags & LOW_IS_INVALID; }
  INT  Low_val (void) const { 
         Is_True (Low_is_const (), ("low should be const"));
	 return _low_val;
       }
  CODEREP* Low_cr (void) const {
         Is_True (Low_is_cr (), ("low should be const"));
         return _low_cr;
      }
  void Set_low (INT v) { Set_low_is_const();  _low_val = v; }
  void Set_low (CODEREP* v) { Set_low_is_cr(); _low_cr = v; }

  BOOL High_is_const (void) const { return _flags & HIGH_IS_CONST; }
  BOOL High_is_cr (void) const { return _flags & HIGH_IS_CR; }
  BOOL High_is_cr_subone(void) const { return _flags & HIGH_IS_CR_SUBONE; }
  BOOL High_is_invalid(void) const { return _flags & HIGH_IS_INVALID; } 
  INT  High_val (void) const { 
         Is_True (High_is_const (), ("high should be const"));
	 return _high_val;
       }
  CODEREP* High_cr (void) const {
         Is_True (High_is_cr (), ("high should be const"));
         return _high_cr;
       }
  void Set_high (INT v) { Set_high_is_const();  _high_val = v; }
  void Set_high (CODEREP* v) { Set_high_is_cr(); _high_cr = v; }
  void Set_high_is_cr_subone(void) { _flags |= HIGH_IS_CR_SUBONE; }

  RANGE_COMP Compare (INT v, LMV_LOOP_INFO* loopinfo);
  void Print (FILE* f);
};


////////////////////////////////////////////////////////////////////
//
//     Classes about Cost model 
//
///////////////////////////////////////////////////////////////////
//
class LMV_HEURISTIC {
private:
  MEM_POOL* _mp;
  LOOP_MULTIVER* _lm;
  MEM_ACCESS_ANALYZER* _maa;
  LMV_LOOP_INFO* _loopinfo;
  OPT_STAB* _opt_stab;
  const ALIAS_RULE* _alias_rule;
  BOOL _tracing;

  // loop with trip count less than this number should not 
  // perform loop multiversioning.
  static const INT _low_trip_count;

  // Upper bound on the number of allowable access vectors
  // to disambiguate at runtime
  static const INT _max_access_vectors;

  // The number of runtime checks is proportional to the
  // number of write vectors * the total number of vectors
  static const INT _max_write_vectors;

  // We need at least one write vector for multiversoning
  // to make sense
  static const INT _min_write_vectors;

public:

  LMV_HEURISTIC (MEM_POOL* mp, LOOP_MULTIVER* lm, LMV_LOOP_INFO* loopinfo, 
                OPT_STAB* opt_stab, MEM_ACCESS_ANALYZER* maa, 
	        const ALIAS_RULE* alias_rule, BOOL trace) 
    { _mp = mp, _lm = lm, _maa = maa, _loopinfo = loopinfo, 
      _opt_stab = opt_stab, _alias_rule = alias_rule; 
      _maa = maa, _tracing = trace; 
    }

  void Init (LOOP_MULTIVER* lm, LMV_LOOP_INFO* loopinfo, 
             OPT_STAB* opt_stab, MEM_ACCESS_ANALYZER* maa, 
	     const ALIAS_RULE* alias_rule) 
    { _lm = lm, _maa = maa, _loopinfo = loopinfo, _opt_stab = opt_stab, 
      _alias_rule = alias_rule; 
    }

  BB_LOOP* Loop (void) const { return _loopinfo->Loop(); }

  static INT Low_trip_count_threshold(void) { return _low_trip_count; }
  static INT Max_access_vectors(void) { return _max_access_vectors; }
  static INT Max_write_vectors(void)  { return _max_write_vectors; }
  static INT Min_write_vectors(void)  { return _min_write_vectors; }

  // Returns true of LMV is beneficial based on the selected memory groups
  BOOL Apply(MEM_GROUP_VECT &groups);
};

////////////////////////////////////////////////////////////////////
//
//  class LMV_CANDIDATE
// 
//  It will cause code explosion if we perform transformation upon
// any candidate. Therefore, the transformation should be postponed
// until all candidate are identified. At that time, few best
// candidates are carefully selected for the transformation. Now that
// there is a time gap between analysis and transformation, some 
// essential information should be recorded down and pass from analysis
// phase to transformation phase. LMV_CANDIDATE serves this purpose.
//
////////////////////////////////////////////////////////////////////
//
class LMV_CANDIDATE {
private:
  MEM_ACCESS_ANALYZER* _mem_analyzer;
  MEM_GROUP_VECT _mem_groups;
  BB_LOOP* _loop;
  INT _stmt_num;
  float _code_sz_percentage; 

public:
  LMV_CANDIDATE (MEM_POOL* mp) :
    _mem_analyzer(NULL),
    _mem_groups(mp),
    _loop(NULL),
    _stmt_num(0),
    _code_sz_percentage(0.0f)
    {}

  MEM_ACCESS_ANALYZER* Mem_access_analyzer (void) const 
    { return _mem_analyzer; } 
  //const MEM_RANGE& Mem_range_1 (void) const { return _r1; }
  //const MEM_RANGE& Mem_range_2 (void) const { return _r2; }
  //const MEM_ACCESS_VECT& Mem_op_group1 (void) const { return  _mem_grp1; }
  //const MEM_ACCESS_VECT& Mem_op_group2 (void) const { return  _mem_grp2; }
  const MEM_GROUP_VECT &Mem_groups(void) const { return _mem_groups; }

  BB_LOOP* Loop (void) const { return _loop; }
  INT Stmt_num (void) const { return _stmt_num; } 
  float Code_sz_percentage (void) const { return _code_sz_percentage; }

  void Set_mem_access_analyzer (MEM_ACCESS_ANALYZER* t) { _mem_analyzer = t;}
  void Set_mem_groups(const MEM_GROUP_VECT &groups)
  {
      _mem_groups = groups;
  }
  void Set_loop (BB_LOOP* loop) { _loop = loop; }
  void Set_stmt_num (INT stmt_num) { _stmt_num = stmt_num; }
  void Set_code_sz_percentage (float f) { _code_sz_percentage = f; }

  void Print_mem_groups(FILE *f);
};

#endif /*opt_lmv_helper_INCLUDED*/
