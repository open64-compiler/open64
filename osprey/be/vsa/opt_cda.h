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

// ==================================================================
// opt_cda.h
//
// control dependency annotation (CDA)
// value range annotation (VRA)
// refer osprey/doc/design_vra_cda.txt for documentation
// ==================================================================

#ifndef opt_cda_INCLUDED
#define opt_cda_INCLUDED

#include "defs.h"
#include "vsa_defs.h"

// forward declaration
class IPSA;
class COMP_UNIT;
class CDA_VALUE;
class CDA_PHI_NODE;
class VSYM_OBJ_REP;

// ==================================================================
// CDA_KIND
// ==================================================================
enum CDA_KIND {
  CDA_CR        = 0,  // cd is a CR
  CDA_CONJ      = 1,  // cd is conjunction of multiple values
  CDA_PHI       = 2,  // cd at merge point there are multiple paths
  CDA_RHS_TRUE  = 3,  // cd with TBR/FBR where RHS is TRUE
  CDA_RHS_FALSE = 4,  // cd with TBR/FBR where RHS is FALSE
  CDA_BB        = 5,  // cd at BB is inherited from another BB
  CDA_ASSUME    = 6,  // cd is from user pragma assume
};

// ==================================================================
// VRA_KIND
// same definition as CDA_KIND.
// defined for reference
// ==================================================================
enum VRA_KIND {
  VRA_CR        = 0,  // value is a CR
  VRA_CONJ      = 1,  // value is conjunction of multiple values
  VRA_PHI       = 2,  // value at merge point there are multiple paths
  VRA_RHS_TRUE  = 3,  // value of TBR/FBR where RHS is TRUE
  VRA_RHS_FALSE = 4,  // value of TBR/FBR where RHS is FALSE
  VRA_BB        = 5,  // value at the BB is inherited from another BB
  VRA_ASSUME    = 6,  // value is from user proama assume
};

// ==================================================================
// CDA_VALUE
// represent a control dependency annotation value
// utilize lower 3-bit to store CDA_KIND
// ==================================================================
class CDA_VALUE {
private:
  uintptr_t _raw_ptr;  // combined cda kind and cr/sr/bb/phi/etc

  // initialize CDA_VALUE with generic pointer and kind
  void Init(void *ptr, UINT aux) {
    Is_True(aux < 0x8, ("aux out of range"));
    Is_True(((uintptr_t)ptr & 0x7) == 0, ("ptr unaligned"));
    _raw_ptr = (uintptr_t)ptr | (aux & 7);
  }

  // get raw data
  uintptr_t Raw_data() const {
    return _raw_ptr;
  }

  // construct a null value
  CDA_VALUE(uintptr_t v) {
    _raw_ptr = v;
  }

public:
  // constructor, CDA_VALUE is inherit from BB_NODE
  CDA_VALUE(BB_NODE *bb) {
    Init(bb, CDA_BB);
  }

  // constructor, CDA_VALUE is defined by TRUEBR/FALSEBR
  CDA_VALUE(STMTREP *sr, BOOL condtrue) {
    Is_True(sr->Opr() == OPR_TRUEBR || sr->Opr() == OPR_FALSEBR,
            ("invalid sr"));
    Init(sr, condtrue ? CDA_RHS_TRUE : CDA_RHS_FALSE);
  }

  // constructor, CDA_VALUE is defined by intrinsic call
  CDA_VALUE(STMTREP *sr) {
    Is_True(sr->Opr() == OPR_INTRINSIC_CALL || sr->Opr() == OPR_CALL,
            ("invalid sr"));
    Init(sr, CDA_ASSUME);
  }

  // constructor, CDA_VALUE is defined by CODEREP
  CDA_VALUE(CODEREP *cr) {
    Is_True(cr->Kind() == CK_VAR ||
            cr->Kind() == CK_IVAR ||
            (cr->Kind() == CK_OP &&
             (cr->Opr() == OPR_EQ || cr->Opr() == OPR_NE ||
              cr->Opr() == OPR_GE || cr->Opr() == OPR_GT ||
              cr->Opr() == OPR_LE || cr->Opr() == OPR_LT)),
            ("invalid cr"));
    Init(cr, CDA_CR);
  }

  // constructor, CDA_VALUE is represented by phi, which can be CDA_CONJ or CDA_PHI(DISJ)
  CDA_VALUE(CDA_PHI_NODE *phi, CDA_KIND kind) {
    Init(phi, kind);
  }

  // get pointer as given type
  template<typename T>
  T *Get_as() const {
    const uintptr_t mask = ((((uintptr_t)-1) >> 3) << 3);
    return (T*)(_raw_ptr & mask);
  }

  // get kind
  CDA_KIND Kind() const {
    return (CDA_KIND)(_raw_ptr & 0x7);
  }

  // get phi node
  CDA_PHI_NODE *Get_phi() const {
    Is_True(Kind() == CDA_PHI, ("bad kind"));
    return Get_as<CDA_PHI_NODE>();
  }

public:
  // create a null CDA_VALUE
  static CDA_VALUE Null() {
    return CDA_VALUE((uintptr_t)0);
  }

  // check if CDA_VALUE is null
  BOOL Is_null() const {
    return _raw_ptr == (uintptr_t)0;
  }

  // compare if two CDA_VALUEs are the same
  BOOL operator == (const CDA_VALUE rhs) const {
    return _raw_ptr == rhs._raw_ptr;
  }

  // get BB where the CDA_VALUE belongs to
  IDTYPE Bb(BB_NODE *bb) const;

public:
  // dump CDA_VALUE to FILE*
  void Dump(FILE *fp) const;

  // dump CDA_VALUE to stdout
  void Dump() const {
    Dump(stdout);
    fprintf(stdout, "\n");
  }
}; // CDA_VALUE


// ==================================================================
// CDA_PHI_NODE
// represent a control dependency annotation phi node
// ==================================================================
class CDA_PHI_NODE {
  friend class CDA;

private:
  IDTYPE    _bb;          // BB_NODE where the phi node belongs to
  UINT32    _size;        // number of operands
  CDA_VALUE _opnd[0];     // operand array, no result for CDA_PHI_NODE

  // private constructor, only CDA can create CDA_PHI_NODE object
  CDA_PHI_NODE(BB_NODE* bb, UINT len) : _bb(bb->Id()), _size(len) {
    Is_True(len > 1 &&
            (bb->Pred()->Len() == len ||
             bb->Kind() == BB_VARGOTO),
            ("bad len"));
    memset(_opnd, 0, sizeof(CDA_VALUE) * _size);
  }

  CDA_PHI_NODE(const CDA_PHI_NODE &);            // disable copy ctor
  CDA_PHI_NODE& operator=(const CDA_PHI_NODE &); // disable assignment

public:
  // set phi operand
  void Set_opnd(UINT opnd, CDA_VALUE val) {
    Is_True(opnd < _size, ("bad opnd"));
    _opnd[opnd] = val;
  }

  // check if opnd is null
  BOOL Is_opnd_null(UINT opnd) const {
    Is_True(opnd < _size, ("bad opnd"));
    return _opnd[opnd].Is_null();
  }

  // get phi operand
  CDA_VALUE Opnd(UINT opnd) const {
    Is_True(opnd < _size, ("bad opnd"));
    return _opnd[opnd];
  }

  // get number of phi operands
  UINT32 Size() const { return _size; }

  // get BB id
  IDTYPE Bb() const   { return _bb;   }

public:
  // dump phi to FILE*
  void Dump(FILE *fp) const;

  // dump phi to stdout
  void Dump() const {
    Dump(stdout);
    fprintf(stdout, "\n");
  }
}; // CDA_PHI_NODE


// ==================================================================
// CDA
// manage control dependency annotation for a function
// ==================================================================
class CDA {
private:
  // CDA_PAIR for CDA_MAP element
  typedef std::pair<IDTYPE, CDA_VALUE> CDA_PAIR;
  // CDA_MAP, key is bb id and value is CDA_VALUE
  typedef __gnu_cxx::hash_map<IDTYPE, CDA_VALUE,
                              __gnu_cxx::hash<IDTYPE>,
                              std::equal_to<IDTYPE>,
                              mempool_allocator<CDA_PAIR> > CDA_MAP;
  // VRA_PAIR for VRA_MAP eleement
  typedef std::pair<uint64_t, CDA_VALUE> VRA_PAIR;
  // VRA_MAP, key is combine of bb id and var/vor id, value is CDA_VALUE
  typedef __gnu_cxx::hash_map<uint64_t, CDA_VALUE,
                              __gnu_cxx::hash<uint64_t>,
                              std::equal_to<uint64_t>,
                              mempool_allocator<VRA_PAIR> > VRA_MAP;

  MEM_POOL   _cda_pool;    // mempool for CDA/VRA
  COMP_UNIT *_comp_unit;   // comp_unit
  CDA_MAP   *_cda_map;     // control dependency annotation
  VRA_MAP   *_vra_map;     // value range for var (CK_VAR) and vor (CK_IVAR, VSYM_OBJ_REP)

private:
  // initialize members in CDA
  void Initialize() {
    // push current mempool
    OPT_POOL_Push(&_cda_pool, VSA_DUMP_FLAG);
    // create CDA_MAP
    _cda_map = CXX_NEW(CDA_MAP(7,
                               __gnu_cxx::hash<IDTYPE>(),
                               std::equal_to<IDTYPE>(),
                               mempool_allocator<CDA_PAIR>(&_cda_pool)),
                       &_cda_pool);
    Is_True(_cda_map, ("failed to create CDA_MAP"));
    // create VRA_MAP for var (CK_VAR) and vor (CK_IVAR, VSYM_OBJ_REP)
    _vra_map = CXX_NEW(VRA_MAP(7,
                               __gnu_cxx::hash<uint64_t>(),
                               std::equal_to<uint64_t>(),
                               mempool_allocator<CDA_PAIR>(&_cda_pool)),
                       &_cda_pool);
    Is_True(_vra_map, ("failed to create VRA_MAP"));
  }

  // destroy members in CDA
  void Destroy() {
    // reset members to 0
    _cda_map = NULL;
    _vra_map = NULL;
    // pop the mempool
    OPT_POOL_Pop(&_cda_pool, VSA_DUMP_FLAG);
  }

public:
  // constructor
  CDA(COMP_UNIT *cu) : _comp_unit(cu), _cda_map(NULL), _vra_map(NULL) {
    // initialize mempool
    OPT_POOL_Initialize(&_cda_pool, "CDA pool", FALSE, VSA_CDA_TRACE_FLAG);
  }

  // destructor
  ~CDA() {
    // destroy the mempool
    OPT_POOL_Delete(&_cda_pool, VSA_CDA_TRACE_FLAG);
  }

  // build CDA
  void Build();

public:
  // Set pred bb as bb's CDA
  void Add_cda(BB_NODE *bb, BB_NODE *pred) {
    Is_True(_cda_map->find(bb->Id()) == _cda_map->end(),
            ("cda already added"));
    Is_True(bb->Pred() && bb->Pred()->Contains(pred),
            ("bad pred"));
    Is_True(pred->Succ() && bb->Succ()->Contains(bb),
            ("bad succ"));
    // find out the head node for straightline code
    while (pred->Pred() && !pred->Pred()->Multiple_bbs()) {
      pred = pred->Pred()->Node();
    }
    _cda_map->insert(CDA_PAIR(bb->Id(), CDA_VALUE(pred)));
  }

  // set STMTREP st as bb's CDA with condition to be true or false
  // STMTREP must be TRUEBR/FALSEBR
  void Add_cda(BB_NODE *bb, STMTREP *sr, BOOL cond_true) {
    Is_True(_cda_map->find(bb->Id()) == _cda_map->end(),
            ("cda already added"));
    Is_True(sr->Opr() == OPR_TRUEBR || sr->Opr() == OPR_FALSEBR,
            ("bad sr"));
    Is_True(bb->Pred() && !bb->Pred()->Multiple_bbs() &&
            bb->Pred()->Node() == sr->Bb(),
            ("bad pred"));
    Is_True(sr->Bb()->Succ() && sr->Bb()->Succ()->Contains(bb),
            ("bad succ"));
    _cda_map->insert(CDA_PAIR(bb->Id(), CDA_VALUE(sr, cond_true)));
  }

  // set cr as bb's CDA. bb must have unique pred which is COMPGOTO
  void Add_cda(BB_NODE *bb, CODEREP *cr) {
    Is_True(_cda_map->find(bb->Id()) == _cda_map->end(),
            ("cda already added"));
    Is_True(bb->Pred() && !bb->Pred()->Multiple_bbs() &&
            bb->Pred()->Node()->Kind() == BB_VARGOTO,
            ("bad pred"));
    _cda_map->insert(CDA_PAIR(bb->Id(), CDA_VALUE(cr)));
  }

  // create CDA_PHI_NODE for `bb' with `len' operands
  CDA_PHI_NODE* Create_cda_phi(BB_NODE *bb, INT32 len) {
    Is_True(bb->Pred() &&
            (bb->Pred()->Len() == len ||
             bb->Kind() == BB_VARGOTO ||
             (bb->Loop() && bb->Loop()->Body() == bb)),
            ("bad bb"));
    CDA_PHI_NODE* phi = CXX_NEW_VARIANT(CDA_PHI_NODE(bb, len),
                                        len * sizeof(CDA_VALUE),
                                        &_cda_pool);
    return phi;
  }

  // set CDA_PHI_NODE for bb
  CDA_PHI_NODE* Add_cda_phi(BB_NODE *bb, CDA_PHI_NODE *phi, CDA_KIND kind) {
    Is_True(_cda_map->find(bb->Id()) == _cda_map->end(),
            ("cda already added"));
    _cda_map->insert(CDA_PAIR(bb->Id(), CDA_VALUE(phi, kind)));
    return phi;
  }

  // create and set CDA_PHI_NODE for bb
  CDA_PHI_NODE* Add_cda_phi(BB_NODE *bb, INT32 len, CDA_KIND kind) {
    Is_True(_cda_map->find(bb->Id()) == _cda_map->end(),
            ("cda already added"));
    CDA_PHI_NODE* phi = Create_cda_phi(bb, len);
    _cda_map->insert(CDA_PAIR(bb->Id(), CDA_VALUE(phi, kind)));
    return phi;
  }

  // get CDA_VALUE for given bb
  CDA_VALUE Get_cda(BB_NODE *bb) const {
    CDA_MAP::const_iterator it = _cda_map->find(bb->Id());
    return (it != _cda_map->end()) ? it->second
                                   : CDA_VALUE::Null();
  }

private:
  // compose vra key with <is_vor, obj_id, bb_id>
  static UINT64 Compose_vra_key(bool is_vor, UINT32 obj_id, UINT32 bb_id) {
    return (UINT64)is_vor << 63 | (UINT64)obj_id << 32 | bb_id;
  }

  // extrace <is_vor, obj_id, bb_id> from vra key
  static void Extract_vra_key(UINT64 key, bool &is_vor, UINT32 &obj_id, UINT32& bb_id) {
    bb_id = (UINT32)key;
    obj_id = (key >> 32) & 0x7FFFFFFF;
    is_vor = (key >> 63) != 0;
  }

  // compose vra key for coderep (CK_VAR and CK_IVAR)
  UINT64 Compose_vra_key(BB_NODE *bb, CODEREP *cr) const;

  // compose vra key for vor
  UINT64 Compose_vra_key(BB_NODE *bb, VSYM_OBJ_REP *vor) const;

public:
  // set CDA_VALUE to obj's vra at given bb
  template<typename OBJ_TYPE>
  void Add_vra(BB_NODE *bb, OBJ_TYPE *obj, CDA_VALUE vra) {
    UINT64 key = Compose_vra_key(bb, obj);
    Is_True(_vra_map->find(key) == _vra_map->end(),
            ("vra already added"));
    _vra_map->insert(VRA_PAIR(key, vra));
  }

  // create and add CDA_PHI_NODE for vra
  template<typename OBJ_TYPE>
  CDA_PHI_NODE* Add_vra_phi(BB_NODE *bb, INT32 len,
                            OBJ_TYPE *obj, CDA_KIND kind) {
    UINT64 key = Compose_vra_key(bb, obj);
    Is_True(_vra_map->find(key) == _vra_map->end(),
            ("vra already added"));
    CDA_PHI_NODE* phi = Create_cda_phi(bb, len);
    _vra_map->insert(VRA_PAIR(key, CDA_VALUE(phi, kind)));
    return phi;
  }

  // get CDA_VALUE for given <bb, obj>
  template<typename OBJ_TYPE>
  CDA_VALUE Get_vra(BB_NODE *bb, OBJ_TYPE *obj) const {
    UINT64 key = Compose_vra_key(bb, obj);
    VRA_MAP::const_iterator it = _vra_map->find(key);
    return (it != _vra_map->end()) ? it->second
                                   : CDA_VALUE::Null();
  }

public:
  // dump cda and vra to FILE*
  void Dump(FILE *fp) const;

  // dump cda and vra to stdout
  void Dump() const {
    Dump(stdout);
  }

  // print cda/vra for given bb
  void Print(IDTYPE bb_id) const;

}; // CDA


#endif /* opt_cda_INCLUDED */
