/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#ifndef opt_addr_util_INCLUDED
#define opt_addr_util_INCLUDED   "opt_vsa_util.h"

// address expression analysis
// ------------------------------------------------------------
// give a complex address expression used in IVAR ilod/istr base
// this analyzer gives out the base, scale, index and offset of
// the address to be deferenced.
// address = base + index * scale + offset
//   base and index are CODEREPs. _kind indicates where the base
//    points to.
//   scale is CONST CODEREP. Basically it equals to sizeof(elememt)
//   offset is CODEREP and CONST value.

#include <vector>

class CODEREP;
class COMP_UNIT;
class DNA_NODE;
class VSA;
class VRA;

enum PTR_KIND {
  PTR_UNKNOWN = 0, // unknown pointer
  PTR_FORMAL,      // pointer value comes from formal/global
                   //   void foo(int *x) { int *p = x; ... }
  PTR_RETURN,      // pointer value comes from return value
                   //   int *p = foo(...);
  PTR_HEAP,        // pointer value comes from malloc
                   //   int *p = malloc(...);
  PTR_ST,          // pointer value comes from LDA of ST
                   //   int *p = &a;
  PTR_CONST,       // pointer value comes from IntConst 
                   //   int *p = (int *)0xblahblah;
  PTR_VAR_CR,      // pointer value comes from VAR
                   //   int *p = b;
  PTR_IVAR,        // pointer value comes from ILOAD
                   //   int *p = *x;
  PTR_PHI,         // pointer have multiple definitions
                   //   int *p3 = phi(p1, p2)
  PTR_KIND_LAST
};

extern const char* PTR_KIND_NAME(PTR_KIND kind);

// =============================================================================
// VSA_ADDRESS_INFO
// =============================================================================
class VSA_ADDRESS_INFO {
  friend class VSA_ADDRINFO_BUILDER;
  typedef std::vector<CODEREP *> CR_VEC;
  typedef std::vector<std::pair<CODEREP *, CODEREP *> > CR_PAIR_VEC;

private:
  STMTREP    *_stmt;       // original stmtrep
  DNA_NODE   *_dna;        // dna which the base belongs to
  STMTREP    *_index_stmt; // stmtrep which the index is used
  DNA_NODE   *_index_dna;  // dna which the index belongs to
  CODEREP*    _base;       // base address
  CR_PAIR_VEC _pos_index;  // positive index with scale
  CR_PAIR_VEC _neg_index;  // negtive index with scale
  CR_VEC      _pos_offset; // positive offset
  CR_VEC      _neg_offset; // negtive offset
  UINT32      _fld_id;     // field id
  MTYPE       _mtype;      // mtype for ILOAD/ISTORE
  INT64       _fix_ofst;   // offset in constant

  PTR_KIND    _kind;       // pointer kind

  VSA_ADDRESS_INFO(const VSA_ADDRESS_INFO&);             // REQUIRED UNDEFINED UNWANTED methods
  VSA_ADDRESS_INFO& operator = (const VSA_ADDRESS_INFO&);// REQUIRED UNDEFINED UNWANTED methods

private:
  void Set_stmt(STMTREP* sr)       { _stmt = sr;       }
  void Set_index_stmt(STMTREP* sr) { _index_stmt = sr; }
  void Set_dna(DNA_NODE *dna)      { _dna = dna;       }
  void Set_index_dna(DNA_NODE *dna){ _index_dna = dna; }
  void Set_base(CODEREP* base)     { _base = base;     }
  void Set_fix_ofst(INT64 ofst)    { _fix_ofst = ofst; }
  void Set_fld_id(UINT32 fid)      { _fld_id = fid;    }
  void Set_mtype(MTYPE mty)        { _mtype = mty;     }
  void Set_kind(PTR_KIND kind)     { _kind = kind;     }

  void Set_index(CR_PAIR_VEC& pos, CR_PAIR_VEC& neg)
  {
     _pos_index.swap(pos);
     _neg_index.swap(neg);
  }

  void Set_var_ofst(CR_VEC& pos, CR_VEC& neg)
  {
    _pos_offset.swap(pos);
    _neg_offset.swap(neg);
  }

public:
  VSA_ADDRESS_INFO()
    : _stmt(NULL), _dna(NULL),
      _index_stmt(NULL), _index_dna(NULL), _base(NULL),
      _mtype(MTYPE_UNKNOWN), _fld_id(0),
      _fix_ofst(0), _kind(PTR_UNKNOWN)
  {
  }

  VSA_ADDRESS_INFO(const VSA_ADDRESS_INFO* info)
    : _stmt(info->_stmt), _dna(info->_dna),
      _index_stmt(info->_index_stmt), _index_dna(info->_index_dna), _base(info->_base),
      _pos_index(info->_pos_index), _neg_index(info->_neg_index),
      _pos_offset(info->_pos_offset), _neg_offset(info->_neg_offset),
      _mtype(info->_mtype), _fld_id(info->_fld_id),
      _fix_ofst(info->_fix_ofst), _kind(info->_kind)
  {
  }

public:
  STMTREP*  Stmt() const            { return _stmt;     }
  BB_NODE*  Bb() const              { return _stmt->Bb(); }
  DNA_NODE *Dna() const             { return _dna;      }
  STMTREP  *Index_stmt() const      { return _index_stmt; }
  BB_NODE  *Index_bb() const        { return _index_stmt->Bb(); }
  DNA_NODE *Index_dna() const       { return _index_dna;}
  CODEREP  *Base() const            { return _base;     }
  INT64     Fix_ofst() const        { return _fix_ofst; }
  UINT32    Fld_id() const          { return _fld_id;   }
  MTYPE     Mtype() const           { return _mtype;    }
  PTR_KIND  Kind() const            { return _kind;     }
  BOOL      Const_ofst() const      { return _pos_index.size() == 0 &&
                                             _neg_index.size() == 0 &&
                                             _pos_offset.size() == 0 &&
                                             _neg_offset.size() == 0; }

  const CR_PAIR_VEC& Pos_index() const  { return _pos_index;  }
  const CR_PAIR_VEC& Neg_index() const  { return _neg_index;  }

  const CR_VEC& Pos_offset() const { return _pos_offset; }
  const CR_VEC& Neg_offset() const { return _neg_offset; }

  void Merge(const VSA_ADDRESS_INFO* info);

  void Merge(CODEREP* cr, DNA_NODE* dna);

  void Subtract(const VSA_ADDRESS_INFO& subend);

  void Print(COMP_UNIT* cu, FILE *fp = stderr) const;
};

// =============================================================================
// VSA_ACCESS_INFO
// =============================================================================
class VSA_ACCESS_INFO {
  friend class VSA_ADDRINFO_BUILDER;
private:
  CODEREP *_index;        // index used to access heap_obj
  CODEREP *_scale;        // scale used to access heap_obj
  CODEREP *_lb;           // lower bound of loop
  CODEREP *_ub;           // upper bound of loop
  CODEREP *_stride;       // stride of loop
  INT64    _fix_ofst;     // fixed offset to access heap_obj
  INT64    _lb_val;       // up-to-date lower bound
  INT64    _ub_val;       // up-to-date upper bound
  INT32    _elem_width;   // byte size each time accessed
  bool     _store;        // is store or load
  bool     _exit_early;   // will the loop early exit

  VSA_ACCESS_INFO& operator=(const VSA_ACCESS_INFO&);  // disable assign opr

private:
  void     Set_index(CODEREP *index)
  {
    _index = index;
  }
  void     Set_scale(CODEREP *scale)
  {
    _scale = scale;
  }
  void     Set_lower_bound(CODEREP *lb)
  {
    _lb = lb;
    _lb_val = (lb && lb->Kind() == CK_CONST) ? lb->Const_val()
                                             : INT64_MAX;
  }
  void     Set_upper_bound(CODEREP *ub)
  {
    _ub = ub;
    _ub_val = (ub && ub->Kind() == CK_CONST) ? ub->Const_val()
                                             : INT64_MAX;
  }
  void     Set_stride(CODEREP *stride)
  {
    _stride = stride;
  }
  void     Set_fix_ofst(INT64 ofst)
  {
    _fix_ofst = ofst;
  }
  void     Set_elem_width(INT32 elem_width)
  {
    _elem_width = elem_width;
  }
  void     Set_store(BOOL store)
  {
    _store = store;
  }
  void     Set_exit_early(BOOL early_exit)
  {
    _exit_early = early_exit;
  }

public:
  // constructor
  VSA_ACCESS_INFO()
    : _index(NULL), _scale(NULL),
      _lb(NULL), _ub(NULL), _stride(NULL),
      _elem_width(0), _store(false), _exit_early(false) {}

public:
  CODEREP *Index() const       { return _index;  }
  CODEREP *Lower_bound() const { return _lb;     }
  CODEREP *Upper_bound() const { return _ub;     }
  CODEREP *Stride() const      { return _stride; }
  INT64    Fix_ofst() const    { return _fix_ofst;   }
  INT32    Elem_width() const  { return _elem_width; }
  BOOL     Is_store() const    { return _store;      }
  BOOL     Exit_early() const  { return _exit_early; }

  INT64    Scale_value() const
  {
    return _scale == NULL
             ? 1
             : _scale->Kind() == CK_CONST ? _scale->Const_val()
                                          : INT64_MAX;
  }

  INT64    Lower_bound_value() const
  {
    return _lb && _lb->Kind() == CK_CONST ? _lb->Const_val() : INT64_MAX;
  }

  INT64    Upper_bound_value() const
  {
    return _ub && _ub->Kind() == CK_CONST ? _ub->Const_val() : INT64_MAX;
  }

  INT64    Stride_value() const
  {
    return _stride && _stride->Kind() == CK_CONST ? _stride->Const_val() : INT64_MAX;
  }

  BOOL     Subtract(const VSA_ACCESS_INFO& rhs);

  void     Print(FILE *fp) const;
};

#endif // opt_addr_util_INCLUDED

