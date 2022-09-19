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

// ====================================================================
//
// Module: opt_vsa_hprop.h
//
// ====================================================================
//

#ifndef opt_vsa_hprop_INCLUDED
#define opt_vsa_hprop_INCLUDED

#include "defs.h"
#include "opt_defs.h"
#include "vsa_defs.h"
#include "erglob.h"
#include "opt_dna.h"

// heap_obj propagation
// for heap_obj size and state annotation

// TO_BE_FIX
// 1. size in byte --> number of elements
//    int *p = malloc(4 * sizeof(int)); foo(p, 4);
// 2. size of variant length struct
//    struct S { int len, int arr[]; };
//    S *s = malloc(sizeof(S) + 10 *sizeof(int)); foo(s, 10);
// 3. propagate hoa on parm in to HEAP_OBJ_REP on OPT_CHI
// 4. propagate hoa on parm out/retv to HEAP_OBJ_REP CALL chi
//    S *s = my_malloc(blah); s->x = ...; // s:ho0v1
// 5. hoa on parm out by checking PDV seems problematic

// TO_BE_REFINE:
// 1. HEAP_OBJ's size is CODEREP*, better to change to HO_ANNOT?


// heap_obj annotation kind
enum HOA_KIND {
  HOA_K_CODEREP    = 0,    // 0b000, pointer to coderep
  HOA_K_VALUE      = 1,    // 0b001, heap_obj annotation value
  HOA_K_ARRAY      = 2,    // 0b010, pointer to fixed length array 2~4
  HOA_K_ARRAY_3    = 3,    // 0b011, fixed length = 3 array
  HOA_K_ARRAY_4    = 4,    // 0b100, fixed length = 4 array
  HOA_K_VALUE_NA   = 5,    // 0b101, can't use 0b101, HOA_K_VALUE use it
  HOA_K_NA         = 6,    // 0b110, not used

  HOA_K_DYN_ARRAY  = 7,    // 0b111, dynamic length array, > 4 items
};

// HOA_VALUE
// heao_obj annotation value
// TODO: support heap_obj annotation on multi-level indirect load
class HOA_VALUE {
  friend class HOA_HANDLE;
  friend class HOA_BUILDER;
private:
  enum {
    TOP_VAL    = -1,       // uninitialize value before prop
    BOTTOM_VAL = 0,        // failed to merge after prop
  };

  // const_size:     32b   // is ho size const
  // parm_or_actual: 8b    // is ho size parm or actual
  // ho_field_id:    10b   // which field is the ho annotated
  // ho_size_fld:    10b   // is ho size another field in same obj
  // malloc:         1b    // is ho malloced
  // free:           1b    // is ho freed
  // flag:           2b    // flag for HOA_K_VALUE, 0b01
  uint64_t _value;         // value rep

  // return raw uint64_t value
  uint64_t Value() const {
    return _value;
  }

  // return value for merge operation
  static UINT Merge(UINT lhs, UINT rhs) {
    if (lhs == TOP_VAL)
      return rhs;
    else if (rhs == TOP_VAL || lhs == rhs)
      return lhs;
    else
      return BOTTOM_VAL;
  }

public:
  // create a Top value
  static uint64_t Top_value() {
    return (((uint64_t)(TOP_VAL)) << 4) | HOA_K_VALUE;
  }

  // create a bottom value
  static uint64_t Bottom_value() {
    return HOA_K_VALUE;
  }

  // constructor
  HOA_VALUE() : _value(Top_value()) {
  }

  // constructor
  HOA_VALUE(uint64_t v)
    : _value(v) {
    Is_True((v & HOA_K_VALUE) == HOA_K_VALUE, ("not hoa data"));
  }

  // create a Top value
  static HOA_VALUE Top_hoa_value() {
    return HOA_VALUE(Top_value());
  }

  // make a HOA_VALUE from values
  static HOA_VALUE Make_value(UINT ho, UINT cst, UINT parm,
                              UINT fld, BOOL md, BOOL fd) {
    Is_True(ho <= FIELD_MAX, ("ho out of range"));
    Is_True(parm == TOP_VAL || parm <= PARM_MAX,
            ("fld out of range"));
    Is_True(fld == TOP_VAL || fld <= FIELD_MAX,
            ("fld out of range"));
    Is_True(md == FALSE || md == TRUE, ("bad malloced flag"));
    Is_True(fd == FALSE || fd == TRUE, ("bad freed value"));
    return (((uint64_t)cst)              << 32) |
           (((uint64_t)parm & PARM_MAX)  << 24) |
           (((uint64_t)ho   & FIELD_MAX) << 14) |
           (((uint64_t)fld  & FIELD_MAX) << 4)  |
           ((md & 0x1)                   << 3)  |
           ((fd & 0x1)                   << 2)  |
           HOA_K_VALUE;
  }

public:
  enum {
    PARM_MAX  = 0xFF,       // maximal 255 params
    FIELD_MAX = 0x3FF,      // maximal 1024 fields
    CONST_MAX = 0xFFFFFFFF, // maximal const value
  };

  // return ho_field_id
  UINT Hoa_field_id() const {
    return (_value >> 14) & FIELD_MAX;
  }

  // return ho_const_size
  UINT Hoa_const_size() const {
    UINT sz = _value >> 32;
    return (sz == TOP_VAL) ? 0 : sz;
  }

  // return raw ho_const_size
  UINT Raw_const_size() const {
    return (UINT)(_value >> 32);
  }

  BOOL Has_const_size() const {
    UINT sz = _value >> 32;
    return sz != 0 && sz != UINT_MAX;
  }

  // return ho_size_param_id
  UINT Hoa_size_param_id() const {
    return (_value >> 24) & PARM_MAX;
  }

  // return ho_size_field_id
  UINT Hoa_size_field_id() const {
    return (_value >> 4) & FIELD_MAX;
  }

  // return if ho is malloced
  BOOL Hoa_malloced() const {
    return (_value >> 3) & 0x1;
  }

  // return if ho is freed
  BOOL Hoa_freed() const {
    return (_value >> 2) & 0x1;
  }

  // merge this with rhs and return a new HOA_K_VALUE
  HOA_VALUE Merge(HOA_VALUE rhs) const {
    Is_True(Hoa_field_id() == rhs.Hoa_field_id(),
            ("try merge different field"));
    // const size
    UINT cst = Merge(Raw_const_size(), rhs.Raw_const_size());
    // parm or actual
    UINT parm = Merge(Hoa_size_param_id(), rhs.Hoa_size_param_id());
    // field
    UINT fld = Merge(Hoa_size_field_id(), rhs.Hoa_size_field_id());
    // malloced
    BOOL md = Hoa_malloced() || rhs.Hoa_malloced();
    // fred
    BOOL fd = Hoa_freed() || rhs.Hoa_freed();

    return Make_value(Hoa_field_id(), cst, parm, fld, md, fd);
  }

  // check if HOA_K_VALUE contains useful info
  BOOL Is_valid() {
    return _value != Top_value();
  }

  // Print
  void Print(INT ident, FILE *fp) const;

  // Print for debugger
  void Print() const;

}; // HOA_VALUE

// HOA_HANDLE
// visit HOA_VALUE/HOA_ARRAY_N/HOA_DYN_ARRAY in one class
class HOA_HANDLE {
  friend class HOA_BUILDER;

  enum {
    HOA_SZ_ZERO      = 0,  // no item
    HOA_SZ_VALUE     = 1,  // 1 item stored as HOA_VALUE
    HOA_SZ_ARRAY_MIN = 2,  // 2~4 items stored as static array
    HOA_SZ_ARRAY_MAX = 4,
    HOA_SZ_ARRAY_DYN = 5,  // >= 5 items stored as dynamic array
  };

  enum {
    HOA_VAL_MASK     = 3,  // mask for value
    HOA_PTR_MASK     = 7,  // mask for pointer
  };

private:
  HO_ANNOT _value;         // value rep

  // constructor used by HOA_BUILDER
  HOA_HANDLE(void *ptr, UINT sz) {
    Is_True(((uint64_t)ptr & HOA_PTR_MASK) == 0,
            ("ptr not aligned"));
    Is_True(sz > HOA_SZ_VALUE,
            ("wrong size"));
    Is_True(sz < HOA_SZ_ARRAY_DYN || *(uint64_t *)ptr == sz,
            ("sz mismatch"));
    // sz = 2 --> mask = 0b010
    // sz = 3 --> mask = 0b011
    // sz = 4 --> mask = 0b100
    UINT mask = (sz < HOA_SZ_ARRAY_DYN) ? sz : HOA_K_DYN_ARRAY;
    _value = ((uint64_t)ptr) | mask;
  }

public:

  // constructor
  HOA_HANDLE(HO_ANNOT val) : _value(val) {
  }

  // constructor
  HOA_HANDLE(CODEREP *cr) {
    Is_True(((uint64_t)cr & HOA_PTR_MASK) == 0,
            ("ptr not aligned"));
    _value = ((uint64_t)cr) | HOA_K_CODEREP;
  }

  // return raw value
  HO_ANNOT Value() const {
    return _value;
  }

  // return HOA_KIND
  HOA_KIND Kind() const {
    UINT mask = _value & HOA_PTR_MASK;
    Is_True(mask != HOA_K_NA, ("invalid kind"));
    if (mask == HOA_K_CODEREP)
      return HOA_K_CODEREP;
    if (mask == HOA_K_DYN_ARRAY)
      return HOA_K_DYN_ARRAY;
    if ((mask & HOA_VAL_MASK) == HOA_K_VALUE)
      return HOA_K_VALUE;
    return HOA_K_ARRAY;
  }

  // return HOA_VALUE count
  UINT Hoa_count() const {
    UINT mask = _value & HOA_PTR_MASK;
    Is_True(mask != HOA_K_NA, ("invalid kind"));
    if (mask == HOA_K_CODEREP || mask == HOA_K_NA)
      return 0;
    if ((mask & HOA_VAL_MASK) == HOA_K_VALUE)
      return 1;
    if (mask == HOA_K_DYN_ARRAY)
      return *(uint64_t *)((_value >> 3) << 3);
    return mask;
  }

  // return HOA_VALUE at given index
  HOA_VALUE Hoa_value(UINT idx) const {
    Is_True(idx < Hoa_count(), ("index out of bound"));
    HOA_KIND kind = Kind();
    if (kind == HOA_K_VALUE) {
      return HOA_VALUE(_value);
    }
    uint64_t ptr = ((_value >> 3) << 3) + idx * sizeof(HOA_VALUE);
    if (kind == HOA_K_DYN_ARRAY) {
      ptr += sizeof(uint64_t);  // skip size
    }
    return *(HOA_VALUE *)ptr;
  }

  // return HOA_VALUE at given field id
  HOA_VALUE Find_hoa(UINT fld) const {
    UINT cnt = Hoa_count();
    for (UINT i = 0; i < cnt; ++i) {
      HOA_VALUE val = Hoa_value(i);
      if (val.Hoa_field_id() == fld)
        return val;
    }
    return HOA_VALUE::Top_value();
  }

  // return if the handle is CR
  CODEREP *Cr() const {
    Is_True((_value & HOA_PTR_MASK) == HOA_K_CODEREP, ("bad kind"));
    return (CODEREP *)_value;
  }

  // check if the handle is VALUE
  HOA_VALUE Hoa_value() const {
    Is_True((_value & HOA_VAL_MASK) == HOA_K_VALUE, ("bad kind"));
    return HOA_VALUE(_value);
  }

  // check if the handle is CR
  BOOL Is_cr() const {
    return (_value != 0) &&
           ((_value & HOA_PTR_MASK) == HOA_K_CODEREP);
  }

  // check if the handle is VALUE
  BOOL Is_value() const {
    return (_value & HOA_VAL_MASK) == HOA_K_VALUE;
  }

  // check if the handle is ARRAY
  BOOL Is_array() const {
    UINT mask = (_value & HOA_PTR_MASK);
    return (mask >= HOA_K_ARRAY && mask <= HOA_K_ARRAY_4);
  }

  // check if the handle is DYN_ARRAY
  BOOL Is_dyn_array() const {
    return (_value & HOA_PTR_MASK) == HOA_K_DYN_ARRAY;
  }

  // Print
  void Print(INT ident, FILE *fp) const;

  // Print for debugger
  void Print() const;

};  // HOA_HANDLE

// ==================================================================
// HOA_PROP
//
// forward propagate heap_obj size in single function
// - propagate caller's argument heap_obj size in RNA_NODE to this
//   callee's DNA_NODE's formal heap_obj size
// - propagate callee's return value heap_obj size in DNA_NODE to
//   this callstmt's RNA_NODE's actual heap_obj size
// - propagate flags on callee's output parameter heap_obj size in
//   DNA_NODE to this callstmt's RNA_NODE's actual heap_obj size
// ==================================================================
class HOA_PROP {
  friend class HOA_BUILDER;
private:
  IPSA      *_ipsa;        // IPSA to propagate flags for calls
  COMP_UNIT *_comp_unit;   // CU to be propagated
  BOOL       _trace;       // is trace on

  MEM_POOL  *Loc_pool() const {
    return _ipsa->Loc_pool();
  }

  MEM_POOL  *Mem_pool() const {
    return _ipsa->Mem_pool();
  }

  COMP_UNIT *Comp_unit() const {
    return _comp_unit;
  }

  OPT_STAB *Opt_stab() const {
    return _comp_unit->Opt_stab();
  }

  DNA_NODE *Dna() const {
    return _comp_unit->Dna();
  }

  VSA       *Vsa() const{
    return _comp_unit->Vsa();
  }

  // propagate heap_obj annotation to HEAP_OBJ_REP on cr
  void Propagate_hoa_to_cr(RNA_NODE *rna, INT i, CODEREP *cr, HO_ANNOT annot);

  // find hoa on chi def
  pair<HO_ANNOT, BOOL> Find_hoa_on_stmt(CODEREP *cr, HO_ANNOT out, STMTREP *def);

  // find hoa on generic cr
  HO_ANNOT Find_hoa_on_cr(CODEREP *cr, HO_ANNOT out, hash_set<IDTYPE>& visited);

  // find hoa on var's U-D
  HO_ANNOT Find_hoa_on_var(CODEREP *cr, HO_ANNOT out, hash_set<IDTYPE>& visited);

  // find hoa on vor's U-D
  HO_ANNOT Find_hoa_on_vor(VSYM_OBJ_REP *vor, HO_ANNOT out, hash_set<IDTYPE>& visited);

public:
  // constructor
  HOA_PROP(IPSA *ipsa, COMP_UNIT *cu)
    : _ipsa(ipsa), _comp_unit(cu) {
    _trace = Get_Trace(TP_VSA, VSA_HPROP_TRACE_FLAG);
  }

  // calculate local heap_obj annot for malloc on output param/return value
  // and free on input parameter
  void Calculate_local_ho_annot();

  // propagate heap_obj annot on clby's RNA_NODE's actual to formal
  void Propagate_ho_annot_on_entry();

  // propagate hea_obj annot on callee's DNA_NODE's return to actual
  void Propagate_ho_annot_from_rna(RNA_NODE *rna);

  // calculate heap_obj size on return and save to DNA_NODE
  void Calculate_ho_annot_on_return();

  // calculate heap_obj size on actual and save to RNA_NODE, propagate
  // heap_obj size on callee's DNA_NODE formal to actual
  void Calculate_ho_annot_on_rna(RNA_NODE *rna);

public:
  // Print hoa
  static void Print_hoa(HO_ANNOT annot, FILE *fp);

  // Print hoa for debugger
  static void Print_hoa(HO_ANNOT annot);

  // Print dna's hoa
  static void Print_dna(DNA_NODE *dna, FILE *fp);

  // Print dna's hoa for debugger
  static void Print_dna(DNA_NODE *dna);

}; // HOA_PROP

#endif /* opt_vsa_hprop_INCLUDED */

