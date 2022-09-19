//-*-c++-*-

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

// ============================================================================
// ============================================================================
//
// Module: opt_vsa_hva.h
//
// ============================================================================
//

#ifndef opt_vsa_hva_INCLUDEDD
#define opt_vsa_hva_INCLUDEDD

// ============================================================================
// HVA: Heap Vsym Analysis
//  Heap object and virtual symbol analysis
//  Refer doccument
//    "New Iterative HO/HOR and VO/VOR Creation and Renaming Algorithm"
// ============================================================================

#include "defs.h"
#include "vsa_defs.h"
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_vsa.h"


// ============================================================================
// CR_VISIT_FLAG
//
// bitwised flag for visiting CODEREP. CR is visited when the bit is set
// ============================================================================
enum CR_VISIT_FLAG {
  V_LDA            = CK_LDA,    // visit LDA
  V_CONST          = CK_CONST,  // visit CONST
  V_RCONST         = CK_RCONST, // visit RCONST
  V_VAR            = CK_VAR,    // visit VAR
  V_IVAR           = CK_IVAR,   // visit IVAR
  V_OP             = CK_OP,     // visit OP
  V_PARM           = 0x10000,   // visit parm IVAR's parm
  V_IVAR_BASE      = 0x20000,   // visit IVAR's base
  V_MLOAD_SIZE     = 0x40000,   // visit MLOAD's size
  V_OP_OPND        = 0x80000,   // visit OP's OPND
  V_ALL            = 0xFFFFF,   // visit all
};

// ============================================================================
// SR_VISIT_FLAG
//
// bitwised flag for visiting STMTREP. SR is visited when the bit is set
// ============================================================================
enum SR_VISIT_FLAG {
  V_STID           = 0x00001,   // visit STID
  V_ISTORE         = 0x00002,   // visit ISTORE
  V_MSTORE         = 0x00004,   // visit MSTORE
  V_ANY_STORE      = 0x0000F,   // visit any STORE above
  V_CALL           = 0x00010,   // visit CALL
  V_ICALL          = 0x00020,   // visit ICALL
  V_INTRINSIC_CALL = 0x00040,   // visit INTRINSIC_CALL
  V_ANY_CALL       = 0x000F0,   // visit any CALL aboe
  V_TRUEBR         = 0x00100,   // visit TRUEBR
  V_FALSEBR        = 0x00200,   // visit FALSEBR
  V_COMPGOTO       = 0x00400,   // visit COMPGOTO
  V_GOTO           = 0x00800,   // visit GOTO
  V_ANY_BRANCH     = 0x00F00,   // visit any BRANCH above
  V_OPT_CHI        = 0x01000,   // visit OPT_CHI
  V_RETURN         = 0x02000,   // visit RETURN
  V_ASM_STMT       = 0x10000,   // visit ASM_STMT
  V_ANY_STMT       = 0xFFFFF,   // visit any STMT above
};

// mod or ref
enum MOD_REF {
  HO_MOD = 0x1,          // ho is modified
  HO_REF = 0x2,          // ho is referenced
  ANY_VO_MOD = 0x4,      // vo with any vfr is been modified
  ANY_VO_REF = 0x8,      // vo with any vfr is been referenced
};

#define ULIST_CACHE_TABLE_SIZE 47
#define HOR_SET_TABLE_SIZE     23
#define PHI_TABLE_SIZE         23

// generic type defines
template<typename _T>
struct HASHER {
  size_t operator() (const _T* ptr) const { return (size_t)ptr; }
};

typedef struct {
  bool operator() (const HEAP_OBJ_REP* ptr1, const HEAP_OBJ_REP* ptr2) const
  {
    return (ptr1->Heap_obj_id() == ptr2->Heap_obj_id());
  }
} HO_ID_EQ;
typedef hash_set<HEAP_OBJ*, HASHER<HEAP_OBJ>,
                 std::equal_to<HEAP_OBJ*>,
                 mempool_allocator<HEAP_OBJ*> > HO_PTR_SET;
typedef hash_set<VSYM_OBJ*, HASHER<VSYM_OBJ>,
                 std::equal_to<VSYM_OBJ*>,
                 mempool_allocator<VSYM_OBJ*> > VO_PTR_SET;
typedef hash_set<CODEREP*, HASHER<CODEREP>,
                 std::equal_to<CODEREP*>,
                 mempool_allocator<CODEREP*> >  CR_PTR_SET;
typedef hash_set<STMTREP*, HASHER<STMTREP>,
                 std::equal_to<STMTREP*>,
                 mempool_allocator<STMTREP*> >  SR_PTR_SET;
typedef hash_set<HEAP_OBJ_REP*, HASHER<HEAP_OBJ_REP>,
                 std::equal_to<HEAP_OBJ_REP*>,
                 mempool_allocator<HEAP_OBJ_REP*> > HOR_PTR_SET;
typedef hash_set<VSYM_OBJ_REP*, HASHER<VSYM_OBJ_REP>,
                 std::equal_to<VSYM_OBJ_REP*>,
                 mempool_allocator<VSYM_OBJ_REP*> > VOR_PTR_SET;
typedef hash_set<PHI_NODE*, HASHER<PHI_NODE>,
                 std::equal_to<PHI_NODE*>,
                 mempool_allocator<PHI_NODE*> >     PHI_PTR_SET;

typedef std::pair<IDTYPE, MU_NODE*>                 ID_MU_PAIR;
typedef hash_map<IDTYPE, MU_NODE*,
                 __gnu_cxx::hash<IDTYPE>,
                 std::equal_to<IDTYPE>,
                 mempool_allocator<ID_MU_PAIR> >    ID_MU_MAP;

typedef std::pair<IDTYPE, CHI_NODE*>                ID_CHI_PAIR;
typedef hash_map<IDTYPE, CHI_NODE*,
                 __gnu_cxx::hash<IDTYPE>,
                 std::equal_to<IDTYPE>,
                 mempool_allocator<ID_CHI_PAIR> >   ID_CHI_MAP;

typedef std::vector<ID_MU_MAP*>                     MU_CACHE;
typedef std::vector<ID_CHI_MAP*>                    CHI_CACHE;

typedef std::pair<HEAP_OBJ_REP*, HOR_PTR_SET*>      ULIST_PAIR;
typedef hash_map<HEAP_OBJ_REP*, HOR_PTR_SET*,
                 HASHER<HEAP_OBJ_REP>,
                 HO_ID_EQ,
                 mempool_allocator<ULIST_PAIR> >    ULIST_CACHE;

typedef deque<PHI_NODE*, mempool_allocator<PHI_NODE*> > PHI_STACK;

// dump HO_PTR_SET/VO_PTR_SET
template<typename _T> static void
Dump_ptr_set(FILE *fp, const char* msg, const _T &ps) {
  if (ps.size() > 0) {
    fputs(msg, fp);
    typename _T::iterator it;
    for (it = ps.begin(); it != ps.end(); ++it) {
      fputs(" -", fp);
      (*it)->Print(fp);
    }
  }
}

// adapter to turn STL iterator to open64 iterator for FOR_ALL_NODE
template<typename _SET, typename _T>
struct HASH_SET_ITER {
  typename _SET::iterator _iter;
  _SET                   *_set;
  void Init(_SET* s)     { _iter = s->begin(); _set = s; }
  _T   First()           { return _iter != _set->end() ? *_iter : _T(); }
  BOOL Is_Empty() const  { return _iter == _set->end();   }
  _T   Next()            { ++_iter; return _iter != _set->end() ? *_iter : _T(); }
};

// a pair of <HEAP_OBJ*, paor<CODEREP*, UINT64> > about how the HEA_OBJ
// is used. CODEREP* is the CR where the HEAO_OBJ is annotated. UINT64
// is the MOD/REF
typedef pair<CODEREP*, UINT64>      CR_MOD_REF;
typedef pair<HEAP_OBJ_REP*, CR_MOD_REF> HO_MOD_REF;

// hash set for HO_MOD_REF
typedef hash_map<HEAP_OBJ_REP*, pair<CODEREP*, UINT64>,
                 HASHER<HEAP_OBJ_REP>, std::equal_to<HEAP_OBJ_REP*>,
                 mempool_allocator<HO_MOD_REF> > HO_MOD_REF_MAP;

// ============================================================================
// CFG_WALKER
//
// Walk the CFG BBs in linear order
// ============================================================================
template<typename _VISITOR>
class CFG_WALKER {
private:
  CFG      *_cfg;      // the cfg to be visited
  _VISITOR *_visitor;  // the visitor to visit the cfg

public:
  // constructor
  CFG_WALKER(CFG* cfg, _VISITOR *act) : _cfg(cfg), _visitor(act) { }

  // walk the cfg
  void Perform() {
    // initialize the visitor
    _visitor->Initialize();
    // iterate bb in linear order
    for (BB_NODE *bb = _cfg->First_bb(); bb; bb = bb->Next()) {
      // call visiter's Enter_bb
      _visitor->Enter_bb(bb);
      // iterate stmtrep inside the bb
      for (STMTREP *stmt = bb->First_stmtrep(); stmt; stmt = stmt->Next()) {
        // call visitor's Process_stmt
        _visitor->Process_stmt_fwd(stmt);
      }
      // call visiter's Exit_bb
      _visitor->Exit_bb(bb);
    }
    // finalize the visitor
    _visitor->Finalize();
  }

};  // CFG_WALKER

// ============================================================================
// DOM_WALKER
//
// Walk the CFG DOM tree
// ============================================================================
template<typename _VISITOR>
class DOM_WALKER {
private:
  CFG      *_cfg;       // the cfg to be visited
  _VISITOR *_visitor;   // the visitor to visit the cfg

private:
  // process bb recursively
  void Process_bb(BB_NODE* bb) {
    // enter bb, processing phi, etc
    _visitor->Enter_bb(bb);

    // forward processing stmtrep
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      _visitor->Process_stmt_fwd(stmt);
    }

    // before enter dom bb
    _visitor->Enter_dom_bb(bb);

    BB_NODE *dom_bb;
    BB_LIST_ITER dom_bb_iter;
    // traverse DOM bbs
    FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
      Process_bb(dom_bb);
    }

    // after exit dom bb
    _visitor->Exit_dom_bb(bb);

    // backward processing stmtrep
    FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
      _visitor->Process_stmt_rev(stmt);
    }

    // exit bb
    _visitor->Exit_bb(bb);
  }  // Process_bb

public:
  // constructor
  DOM_WALKER(CFG* cfg, _VISITOR *act) : _cfg(cfg), _visitor(act) { }

  // walk the cfg dom tree
  void Perform() {
    // initialize the visitor
    _visitor->Initialize();
    // start from entry bb
    Process_bb(_cfg->Entry_bb());
    // finalize the visitor
    _visitor->Finalize();
  }

};  // DOM_WALKER

// ============================================================================
// HOR_CACHE
//
// Cache hor finding result foor each round
// ============================================================================
class HOR_CACHE {
private:
  // per-round cache for cr/vor/phi -> HEAP_OBJ_REP* map
  typedef pair<uintptr_t, HEAP_OBJ_REP*> HOR_CACHE_PAIR;
  typedef hash_map<uintptr_t, HEAP_OBJ_REP*, __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<HOR_CACHE_PAIR> > HOR_CACHE_MAP;

  // first elem is phi nodes in the same SCC
  // second elem is HOR candicates for the SCC
  typedef pair<PHI_PTR_SET*, HOR_PTR_SET*> PHI_HOR_INFO;
  // phi to its hor candidates map
  typedef pair<uintptr_t, PHI_HOR_INFO> PHI_CAND_PAIR;
  // global phi -> <SCC, HOR_CAND> map
  typedef hash_map<uintptr_t, PHI_HOR_INFO, __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<PHI_CAND_PAIR> > PHI_CAND_MAP;

private:
  VSA           *_vsa;         // vsa pointer
  MEM_POOL       _pc_pool;     // phi -> <SCC, HOR_CAND> memory pool
  PHI_CAND_MAP  *_pc_map;      // phi -> <SCC, HOR_CAND> map

  HOR_CACHE_MAP *_cr_map;      // cr -> heap_oobj_rep map
  HOR_CACHE_MAP *_vor_map;     // vor -> heap_oobj_rep map
  HOR_CACHE_MAP *_phi_map;     // phi -> heap_oobj_rep map
  PHI_STACK     *_phi_stack;   // phi stack
  MEM_POOL      *_lpool;       // local memory pool

  HOR_CACHE(const HOR_CACHE&);             // no copy-ctor
  HOR_CACHE& operator=(const HOR_CACHE&);  // no assign-oper

public:
  // constructor
  HOR_CACHE(VSA *vsa, MEM_POOL *lpool)
    : _vsa(vsa),
      _cr_map(NULL), _vor_map(NULL), _phi_map(NULL),
      _lpool(lpool) {
    OPT_POOL_Initialize(&_pc_pool, "phi map ppool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_pc_pool, VSA_DUMP_FLAG);

    _pc_map = CXX_NEW(PHI_CAND_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                   std::equal_to<uintptr_t>(),
                                   mempool_allocator<PHI_CAND_PAIR>(&_pc_pool)),
                      &_pc_pool);
  }

  // destructor
  ~HOR_CACHE() {
    _pc_map = NULL;
    OPT_POOL_Pop(&_pc_pool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_pc_pool, VSA_DUMP_FLAG);
  }

  // begin iteration
  void Begin_iteration() {
    Is_True(_cr_map == NULL, ("_cr_map initialized"));
    Is_True(_vor_map == NULL, ("_vor_map initialized"));
    _cr_map = CXX_NEW(HOR_CACHE_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                       std::equal_to<uintptr_t>(),
                                       mempool_allocator<HOR_CACHE_PAIR>(_lpool)),
                      _lpool);
    _vor_map = CXX_NEW(HOR_CACHE_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                        std::equal_to<uintptr_t>(),
                                        mempool_allocator<HOR_CACHE_PAIR>(_lpool)),
                       _lpool);
    _phi_map = CXX_NEW(HOR_CACHE_MAP(127, __gnu_cxx::hash<uintptr_t>(),
                                        std::equal_to<uintptr_t>(),
                                        mempool_allocator<HOR_CACHE_PAIR>(_lpool)),
                       _lpool);
    _phi_stack = CXX_NEW(PHI_STACK(mempool_allocator<PHI_NODE*>()),
                         _lpool);
  }

  // end iteration
  void End_iteration() {
    Is_True(_phi_stack->size() == 0, ("phi stack corrupted"));
    _cr_map = NULL;
    _vor_map = NULL;
    _phi_map = NULL;
    _phi_stack = NULL;
  }

public:
  // push phi into phi stack
  BOOL Push_phi(PHI_NODE *phi) {
    Is_True(phi != NULL, ("invalid phi"));
    PHI_STACK::iterator end = _phi_stack->end();
    PHI_STACK::iterator pos = std::find(_phi_stack->begin(), end, phi);
    PHI_HOR_INFO& info = (*_pc_map)[(uintptr_t)phi];
    if (pos != end) {
      // if phi already in stack, unify all phis' phi_set and hor_set
      PHI_PTR_SET* phi_set = info.first;
      HOR_PTR_SET* hor_set = info.second;
      Is_True(phi_set && hor_set, ("phi_set or hor_set not initialized"));
      while ((++pos) != end) {
        PHI_HOR_INFO& next = (*_pc_map)[(uintptr_t)*pos];
        Is_True(next.first && next.second, ("phi_set or hor_set invalid"));
        if (phi_set != next.first) {
          Is_True(hor_set != next.second, ("hor set invalid"));
          // update hor_set
          hor_set->insert(next.second->begin(), next.second->end());
          // update phi set and phi's info
          for (PHI_PTR_SET::iterator t = next.first->begin();
               t != next.first->end(); ++t) {
            phi_set->insert(*t);
            (*_pc_map)[(uintptr_t)*t].first = phi_set;
            (*_pc_map)[(uintptr_t)*t].second = hor_set;
          }
        }
        Is_True(phi_set == (*_pc_map)[(uintptr_t)*pos].first,
                ("phi set mismatch"));
        Is_True(hor_set == (*_pc_map)[(uintptr_t)*pos].second,
                ("hor set mismatch"));
      }
      return FALSE;
    }

    // push phi into stack
    _phi_stack->push_back(phi);
    // make sure PHI_HOR_INFO is created
    if (info.second == NULL) {
      Is_True(info.first == NULL, ("phi set invalid"));
      info.first = CXX_NEW(PHI_PTR_SET(31, HASHER<PHI_NODE>(),
                                       std::equal_to<PHI_NODE*>(),
                                       mempool_allocator<PHI_NODE*>(&_pc_pool)),
                           &_pc_pool);
      info.second = CXX_NEW(HOR_PTR_SET(31, HASHER<HEAP_OBJ_REP>(),
                                        std::equal_to<HEAP_OBJ_REP*>(),
                                        mempool_allocator<HEAP_OBJ_REP*>(&_pc_pool)),
                            &_pc_pool);
      info.first->insert(phi);
    }
    Is_True(info.first && info.first->find(phi) != info.first->end(),
            ("phi not added"));
    return TRUE;
  }

  // pop phi from stack
  void Pop_phi(PHI_NODE *phi) {
    Is_True(!_phi_stack->empty() && _phi_stack->back() == phi,
            ("stack top mismatch"));
    // pop phi from stack
    _phi_stack->pop_back();

    // propagate hor_set to all elemment remain in stack
    PHI_CAND_MAP::iterator pc = _pc_map->find((uintptr_t)phi);
    Is_True(pc != _pc_map->end() &&
            pc->second.first && pc->second.second,
            ("phi not handled before"));
    HOR_PTR_SET* hor_set = pc->second.second;
    PHI_STACK::iterator end = _phi_stack->end();
    for (PHI_STACK::iterator it = _phi_stack->begin(); it != end; ++it) {
      PHI_HOR_INFO& info = (*_pc_map)[(uintptr_t)*it];
      Is_True(info.first && info.second, ("phi_set or hor_set invalid"));
      if (info.second != hor_set) {
        Is_True(info.first->find(phi) == info.first->end(),
                ("phi in the set"));
        info.second->insert(hor_set->begin(), hor_set->end());
      }
      else {
        Is_True(info.first->find(phi) != info.first->end(),
                ("phi not in the set"));
      }
    }
  }

  // get phi's hor candidate
  HOR_PTR_SET *Phi_cand(PHI_NODE *phi) {
    PHI_CAND_MAP::iterator it = _pc_map->find((uintptr_t)phi);
    Is_True(it != _pc_map->end(),
            ("phi not handled before"));
    return it->second.second;
  }

  // add hor as phi's candidate
  void Add_phi_hor(PHI_NODE *phi, HEAP_OBJ_REP *hor) {
    Is_True(phi && hor, ("bad phi or hor"));
    PHI_HOR_INFO& info = (*_pc_map)[(uintptr_t)phi];
    Is_True(info.first != NULL && info.second != NULL &&
            info.first->find(phi) != info.first->end(),
            ("invalid scc or hor cand"));
    info.second->insert(hor);
  }

public:
  // find cr's hor from per-round cache
  HEAP_OBJ_REP* Find_hor(CODEREP *cr) const {
    Is_True(cr &&
            (cr->Kind() == CK_LDA ||
             cr->Kind() == CK_VAR ||
             cr->Kind() == CK_IVAR), ("invalid cr"));
    HOR_CACHE_MAP::const_iterator it = _cr_map->find((uintptr_t)cr);
    return (it != _cr_map->end()) ? it->second : NULL;
  }

  // find vor's hor from per-round cache
  HEAP_OBJ_REP* Find_hor(VSYM_OBJ_REP *vor) const {
    Is_True(vor && !_vsa->Is_special_vor(vor), ("invalid vor"));
    HOR_CACHE_MAP::const_iterator it = _vor_map->find((uintptr_t)vor);
    return (it != _vor_map->end()) ? it->second : NULL;
  }

  // find phi's hor from per-round cache
  HEAP_OBJ_REP* Find_hor(PHI_NODE *phi) const {
    HOR_CACHE_MAP::const_iterator it = _phi_map->find((uintptr_t)phi);
    return (it != _phi_map->end()) ? it->second : NULL;
  }

  // add <cr, hor> to per-round cache
  void Cache_hor(CODEREP *cr, HEAP_OBJ_REP *hor) {
    Is_True(cr &&
            (cr->Kind() == CK_LDA ||
             cr->Kind() == CK_VAR ||
             cr->Kind() == CK_IVAR), ("invalid cr"));
    _cr_map->insert(HOR_CACHE_PAIR((uintptr_t)cr, hor));
  }

  // add <vor, hor> to per-round cache
  void Cache_hor(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor) {
    Is_True(vor && !_vsa->Is_special_vor(vor), ("invalid vor"));
    _vor_map->insert(HOR_CACHE_PAIR((uintptr_t)vor, hor));
  }

  // add <phi, hor> to per-round cache
  void Cache_hor(PHI_NODE *phi, HEAP_OBJ_REP *hor) {
    _phi_map->insert(HOR_CACHE_PAIR((uintptr_t)phi, hor));
  }
};

// ============================================================================
// HEAP_VSYM_ANALYSIS
//
// Perform iterative HEAP_OBJ and VSYM_OBJ analysis
// ============================================================================
class HEAP_VSYM_ANALYSIS {
private:
  COMP_UNIT        *_comp_unit;     // comp_unit pointer
  MEM_POOL         *_defbb_pool;    // mempool for def bbs for phi insertion
  VSA              *_vsa;           // vsa pointer
  HO_PTR_SET       *_ho_created;    // heap_obj created in this iteration
  HO_PTR_SET       *_ho_updated;    // heap_obj updated in this iteration
  VO_PTR_SET       *_vo_created;    // vsym_obj created in this iteration
  VO_PTR_SET       *_vo_updated;    // vsym_obj updated in this iteration
  std::vector<INT>  _bb_counter;    // counter for each bb
  std::vector<INT>  _sr_counter;    // counter for each stmtrep
  MEM_POOL          _local_pool;    // local memory pool, only valid for one iteration
  MEM_POOL          _temp_pool;     // temporary memory pool
  MEM_POOL          _hva_pool;      // memory pool valid for whole HEAP_VSYM_ANALYSIS lifecycle
  MU_CACHE          _vor_mu_cache;  // cache stmt vor mu,  entry of <vo_id, MU_NODE*>
  CHI_CACHE         _vor_chi_cache; // cache stmt vor chi, entry of <vo_id, CHI_NODE*>
  ULIST_CACHE      *_ulist_cache;   // cache ulist, map<HEAP_OBJ_REP*, hash_set<HEAP_OBJ_REP*> >
  PHI_CACHE         _vo_phi_cache;  // cache vo phi, vector of <bb_id, hash_set<vo_id>* >
  PHI_CACHE         _ho_phi_cache;  // cache ho phi, vector of <bb_id, hash_set<ho_id>* >
  HOR_CACHE         _hor_cache;     // hor cache for each round
  INT               _round;         // current round
  INT               _max_round;     // hard limit for max round
  IDTYPE            _first_ho_id;   // id for first hor created in this function
  IDTYPE            _first_vo_id;   // id for first vor created in this function
  IDTYPE            _next_vo_id;    // id for next vor to be created in this function
  BOOL              _next_round;    // is next round needed
  BOOL              _tracing;       // dump IR annotation after each phase

public:
  COMP_UNIT  *Comp_unit() const  { return _comp_unit;        }
  MEM_POOL   *Defbb_pool() const { return _defbb_pool;       }
  MEM_POOL   *Local_pool()       { return &_local_pool;      }
  MEM_POOL   *Temp_pool()        { return &_temp_pool;       }
  MEM_POOL   *Hva_pool()         { return &_hva_pool;       }
  VSA        *Vsa() const        { return _vsa;              }
  DNA_NODE   *Dna() const        { return _comp_unit->Dna(); }
  CFG        *Cfg() const        { return _comp_unit->Cfg(); }

  HO_PTR_SET &Ho_created()       { return *_ho_created;      }
  HO_PTR_SET &Ho_updated()       { return *_ho_updated;      }
  VO_PTR_SET &Vo_created()       { return *_vo_created;      }
  VO_PTR_SET &Vo_updated()       { return *_vo_updated;      }
  PHI_CACHE  *Vo_phi_cache()     { return &_vo_phi_cache;    }
  PHI_CACHE  *Ho_phi_cache()     { return &_ho_phi_cache;    }

private:
  void Add_vor(VSYM_OBJ_REP *vor) {
    if (vor->Vsym_obj()->Id() >= _next_vo_id) {
      // add vor to _vo_created map
      _vo_created->insert(vor->Vsym_obj());
      _next_vo_id = vor->Vsym_obj()->Id() + 1;
    }
    else if (!Vo_created(vor)) {
      // add vor to _vo_updated map if it's not in _vo_created
      _vo_updated->insert(vor->Vsym_obj());
      // set stmt defs vor on same vo to be visited so the vor rename
      // can be done correctly
      VSYM_OBJ_REP *ptr = vor->Next();
      while (ptr) {
        if (ptr->Attr() != ROR_DEF_BY_PHI &&
            ptr->Attr() != ROR_DEF_BY_HORPHI) {
          STMTREP *def = ptr->Stmt_def();
          Is_True(def != NULL, ("vor stmt def is NULL"));
          Set_visit_later(def);
        }
        ptr = ptr->Next();
      }
    }
  }

public:
  // constructor
  HEAP_VSYM_ANALYSIS(COMP_UNIT *cu, MEM_POOL* defbb_pool, INT max_round = 12)
    : _comp_unit(cu), _defbb_pool(defbb_pool), _vsa(cu->Vsa()),
      _ho_created(NULL), _ho_updated(NULL), _vo_created(NULL), _vo_updated(NULL),
      _bb_counter(cu->Cfg()->Total_bb_count()),
      _sr_counter(cu->Htable()->Stmtrep_id_cnt()),
      _vor_mu_cache(cu->Htable()->Stmtrep_id_cnt(), NULL),
      _vor_chi_cache(cu->Htable()->Stmtrep_id_cnt(), NULL),
      _vo_phi_cache(cu->Cfg()->Total_bb_count(), NULL),
      _ho_phi_cache(cu->Cfg()->Total_bb_count(), NULL),
      _hor_cache(cu->Vsa(), &_local_pool),
      _round(0), _max_round(max_round), _next_round(FALSE) {
    // initialize local pool
    _tracing = Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG);
    OPT_POOL_Initialize(&_local_pool, "HEAP_VSYM_ANALYSIS local pool", FALSE, VSA_NHV_TRACE_FLAG);
    OPT_POOL_Initialize(&_temp_pool, "HEAP_VSYM_ANALYSIS temporary pool", FALSE, VSA_NHV_TRACE_FLAG);
    OPT_POOL_Initialize(&_hva_pool, "HEAP_VSYM_ANALYSIS pool", FALSE, VSA_NHV_TRACE_FLAG);

    _ulist_cache = CXX_NEW(ULIST_CACHE(ULIST_CACHE_TABLE_SIZE, HASHER<HEAP_OBJ_REP>(),
                                       HO_ID_EQ(),
                                       mempool_allocator<ULIST_PAIR>(Hva_pool())),
                           Hva_pool());
    _first_ho_id = Vsa()->Last_heapobj_id();
    _first_vo_id = Vsa()->Last_vsymobj_id();
    _next_vo_id = _first_vo_id;
  }

  // destructor
  ~HEAP_VSYM_ANALYSIS() {
    // delete local pool
    OPT_POOL_Delete(&_local_pool, VSA_NHV_TRACE_FLAG);
    OPT_POOL_Delete(&_temp_pool, VSA_NHV_TRACE_FLAG);
    OPT_POOL_Delete(&_hva_pool, VSA_NHV_TRACE_FLAG);
  }

  // find cr where the ho/vo is associated
  CODEREP *Find_rsc_cr(STMTREP *sr, AUX_ID aux) {
    CODEREP *cr;
    // try param for OPT_CHI
    if (sr->Opr() == OPR_OPT_CHI &&
        (cr = Dna()->Find_param_cr(aux)) != NULL)
      return cr;
    // try return value for Call
    if (OPERATOR_is_call(sr->Opr()) &&
        (cr = Comp_unit()->Find_return_value(sr)) != NULL)
      return cr;
    // try stmt chi list
    CHI_NODE *chi;
    if (sr->Chi_list() &&
        (chi = sr->Chi_list()->Search_chi_node(aux)) != NULL &&
        chi->Live())
      return chi->RESULT();
    // nothing found
    return NULL;
  }

  // create new heap_obj and add to _ho_created set
  HEAP_OBJ_REP *Create_heap_obj(CODEREP *cr, BB_NODE *bb, MEM_POOL *mpool) {
    HEAP_OBJ_REP *hor = _vsa->Allocate_heap_obj(cr, bb, mpool);
    if (_ho_created)
      _ho_created->insert(hor->Heap_obj());
    return hor;
  }

  // create new heap_obj based on HEAP_OBJ
  HEAP_OBJ_REP *Create_heap_obj(HEAP_OBJ *ho) {
    HEAP_OBJ_REP *hor = _vsa->Allocate_heap_obj(ho, NULL);
    return hor;
  }

  // create heap_obj_rep for LDA
  HEAP_OBJ_REP *Create_heap_obj_for_lda(CODEREP *cr) {
    Is_True(cr && cr->Kind() == CK_LDA, ("not lda cr"));
    OPT_STAB *stab = Comp_unit()->Opt_stab();
    AUX_ID aux = cr->Lda_aux_id();
    Is_True(stab->St(aux) != NULL, ("lda st is NULL"));
    AUX_ID aux_verify = 0;

    // fix-up aux for aggregate
    mINT64 ofst = stab->St_ofst(aux);
    if (ofst > 0 && TY_kind(ST_type(stab->St(aux))) == KIND_STRUCT) {
      AUX_ID aux_id = aux;
      AUX_ID aux_sav = aux;
      aux_id = stab->St_group(aux_id);
      while (aux_id != aux_sav && aux_id != 0) {
        mINT64 ofst_tmp = stab->St_ofst(aux_id);
        if (ofst_tmp < ofst) {
          aux = aux_id;
          ofst = ofst_tmp;
          if (ofst_tmp == 0)
            break;   // smallest field id found
        }
        aux_id = stab->St_group(aux_id);
      }
    }
    else {
      aux_verify = aux;
    }

    // find cr at offset 0, probably need to calculate offset from fld_id?
    CODEREP *lda = Alloc_stack_cr(0);
    lda->Init_lda(cr->Dtyp(), aux, ofst, cr->Lda_ty(), cr->Lda_base_st(), 0);
    lda = Comp_unit()->Htable()->Hash_Lda(lda);
    Is_True(lda && lda->Kind() == CK_LDA, ("not lda"));
    aux = lda->Lda_aux_id();

    HEAP_OBJ *ho = _vsa->Find(aux, TRUE);
    if (ho)
      return ho->Entry_chi();


    HEAP_OBJ_REP *hor = _vsa->Allocate_heap_obj(lda, Cfg()->Entry_bb());
    Is_True(hor == hor->Heap_obj()->Entry_chi(), ("bad entry chi"));
    Is_True(hor->Heap_obj()->Sym_id() == aux, ("aux id mismatch"));
    hor->Set_attr(ROR_DEF_BY_LDA);
    if (_ho_created)
      _ho_created->insert(hor->Heap_obj());
    return hor;
  }

  // create new heap_obj def by call and add to _ho_created set
  HEAP_OBJ_REP *Create_heap_obj_for_chi(STMTREP *def, CODEREP *def_cr,
                                        CODEREP *ho_cr, MEM_POOL *mpool) {
    HEAP_OBJ_REP *hor = Create_heap_obj(ho_cr, def->Bb(), mpool);
    hor->Set_attr(ROR_DEF_BY_CHI);
    Is_True(ho_cr == NULL || hor->Heap_obj()->Sym_id() == _vsa->Cr_aux_id(ho_cr),
            ("aux id mismatch"));
    hor->Set_stmt_def(def, Dna());
    Set_visit_later(def);  // def stmt should be visited in renaming phase

    HEAP_OBJ_REP *chi_opnd = hor->Heap_obj()->Entry_chi();
    STMTREP *entry_stmt = _vsa->Get_entry_chi_stmt();
    AUX_ID aux_id = _vsa->Cr_aux_id(def_cr);
    CODEREP *parm_cr;
    if (entry_stmt != def &&
        (parm_cr = Find_rsc_cr(entry_stmt, aux_id)) != NULL) {
      HEAP_OBJ_REP *chi_res = Create_heap_obj(hor->Heap_obj());
      chi_res->Set_attr(ROR_DEF_BY_CHI);
      chi_res->Set_stmt_def(entry_stmt, Dna());
      Is_True(Vsa()->Find_stmt_hor_chi(entry_stmt, hor->Heap_obj()) == NULL,
              ("hor chi already append"));
      Vsa()->Append_stmt_hor_chi(entry_stmt, chi_res, chi_opnd, parm_cr);
    }

    Is_True(Vsa()->Find_stmt_hor_chi(def, hor->Heap_obj()) == NULL,
            ("hor chi already append"));
    Vsa()->Append_stmt_hor_chi(def, hor, chi_opnd, def_cr);
    return hor;
  }

  // find HEAP_OBJ_REP annotated on cr
  HEAP_OBJ_REP *Cr_2_heap_obj(CODEREP *cr) {
    return Vsa()->Cr_2_heap_obj(cr);
  }

  // set HEAP_OBJ_REP to cr
  void Enter_cr_heap_obj_map(CODEREP *cr, HEAP_OBJ_REP *hor, BOOL replace) {
    Vsa()->Enter_cr_heap_obj_map(cr, hor, replace);
  }

  // find VSYM_OBJ_REP annotated on cr
  VSYM_OBJ_REP *Cr_2_vor(CODEREP *cr) {
    return Vsa()->Cr_2_vor(cr);
  }

  // set heap_obj updated only if heap_obj is not in _ho_created set
  void Update_heap_obj(HEAP_OBJ_REP *hor, BB_NODE *bb, MEM_POOL *mpool) {
    Is_True(!Vsa()->Is_special_hor(hor), ("update special hor"));
    Is_True(_ho_created != NULL && _ho_updated != NULL,
            ("ho created/updated not initialized"));
    hor->Heap_obj()->Prepend_def_bbs(bb, mpool);
    if (_ho_created->find(hor->Heap_obj()) != _ho_created->end())
      return;
    if (_ho_updated->find(hor->Heap_obj()) != _ho_updated->end())
      return;
    _ho_updated->insert(hor->Heap_obj());
    // set hor in ulist updated
    if (hor->Ulist() != NULL) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor->Ulist())) {
        if (cur_hor->Heap_obj() == hor->Heap_obj() ||
            Vsa()->Is_special_hor(cur_hor))
          continue;
        if (_ho_created->find(cur_hor->Heap_obj()) != _ho_created->end())
          continue;
        _ho_updated->insert(cur_hor->Heap_obj());
      }
    }
  }

  // generate or reuse version for heap_obj_rep and push to stack
  IDTYPE Gen_name(HEAP_OBJ_REP *hor, STMTREP *sr) {
    Is_True(!Vsa()->Is_special_hor(hor), ("rename null hor"));
    IDTYPE v = hor->Version();
    if (v <= ROR_VERSION_NOT_SET) {
      v = hor->Heap_obj()->Gen_version();
      hor->Set_version(v);
    }
    Is_True(sr == NULL || hor->Stmt_def() == sr,
            ("def stmt mismatch"));
    hor->Heap_obj()->Push(hor, sr);
    return v;
  }

  // check if heap_obj is created in current iteration
  BOOL Ho_created(HEAP_OBJ_REP *hor) const {
    return (_ho_created &&
            _ho_created->find(hor->Heap_obj()) != _ho_created->end()) ?
           TRUE : FALSE;
  }

  // check if there is new heap_obj created in current iteration
  BOOL Has_ho_created() const {
    return _ho_created && _ho_created->size() > 0;
  }

  // check if heap_obj is updated in current iteration
  BOOL Ho_updated(HEAP_OBJ_REP *hor) const {
    return _ho_updated->find(hor->Heap_obj()) != _ho_updated->end() ?
           TRUE : FALSE;
  }

  // check if there is new heap_obj updated in current iteration
  BOOL Has_ho_updated() const {
    return _ho_updated->size() > 0;
  }

  // check if heao_obj should be visited in current iteration
  BOOL Visit_heap_obj(HEAP_OBJ_REP *hor) const {
    return Ho_created(hor) || Ho_updated(hor);
  }

  // get first ho id
  IDTYPE First_ho_id() const {
    return _first_ho_id;
  }

  // create or find vsym_obj and add to _vo_created set
  VSYM_OBJ_REP *Create_vsym_obj(BB_NODE *bb, HEAP_OBJ_REP *hor, IDTYPE ifldid, mINT32 ofst,
                                MEM_POOL *mpool, VS_FLD_KIND kind = FLD_K_ID) {
    if (hor == _vsa->Null_hor())
      return _vsa->Null_vor();
    if (hor == _vsa->Default_hor())
      return _vsa->Default_vor();

    VSYM_OBJ_REP *vor = _vsa->Allocate_vsym_obj(bb, hor, ifldid, ofst, mpool, kind);
    Add_vor(vor);
    return vor;
  }

  // create or find vsym_obj and add to _vo_created set
  VSYM_OBJ_REP *Create_vsym_obj(BB_NODE *bb, HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr,
                                MEM_POOL *mpool) {
    if (hor == _vsa->Null_hor())
      return _vsa->Null_vor();
    if (hor == _vsa->Default_hor())
      return _vsa->Default_vor();

    VSYM_OBJ_REP *vor = _vsa->Allocate_vsym_obj(bb, hor, vfr, mpool);
    Add_vor(vor);
    return vor;
  }

  // create vsym_obj_rep based on vsym_obj
  VSYM_OBJ_REP *Create_vsym_obj(VSYM_OBJ *vo, BB_NODE *bb, MEM_POOL *mpool) {
    Is_True(vo != _vsa->Null_vor()->Vsym_obj(), ("create null vor"));
    Is_True(bb != NULL, ("invalid bb"));
    Is_True(Vo_created(vo->Entry_chi()), ("vo not created in this round"));
    vo->Prepend_def_bbs(bb, mpool);
    return _vsa->Allocate_vsym_obj(vo);
  }

  // create or find vsym_obj for use only and add to _vo_created set
  VSYM_OBJ_REP *Create_vsym_obj_use(HEAP_OBJ_REP *hor, IDTYPE ifldid, mINT32 ofst,
                                    VS_FLD_KIND kind = FLD_K_ID) {
    if (hor == _vsa->Null_hor())
      return _vsa->Null_vor();
    if (hor == _vsa->Default_hor())
      return _vsa->Default_vor();

    VSYM_OBJ_REP *vor = _vsa->Allocate_vsym_obj(NULL, hor, ifldid, ofst, NULL, kind);
    Is_True(vor && vor->Is_entry_chi(), ("should be entry chi"));
    Add_vor(vor);
    // create vor entry chi def on stmt which defs cur_hor
    if (Vo_created(vor)) {
      Create_vsym_chi_for_hor(vor, hor);
    }
    return vor;
  }

  // create vsym_obj_rep based on HEAP_OBJ_REP and VSYM_FLD_REP
  VSYM_OBJ_REP *Create_vsym_obj_use(HEAP_OBJ_REP *hor, VSYM_FLD_REP *vfr) {
    if (hor == _vsa->Null_hor())
      return _vsa->Null_vor();
    if (hor == _vsa->Default_hor())
      return _vsa->Default_vor();

    Is_True(hor && vfr, ("heap_obj_rep or field_rep is null"));
    VSYM_OBJ_REP *vor = _vsa->Allocate_vsym_obj(NULL, hor, vfr, NULL);
    Is_True(vor && vor->Is_entry_chi(), ("should be entry chi"));
    Add_vor(vor);
    if (Vo_created(vor)) {
      Create_vsym_chi_for_hor(vor, hor);
    }
    return vor;
  }

  // create vsym_obj_rep based on HEAP_OBJ_REP and VSYM_FLD_REP
  VSYM_OBJ_REP *Create_vsym_obj_def(BB_NODE *bb, HEAP_OBJ_REP *hor,
                                    VSYM_FLD_REP *vfr, MEM_POOL *mpool) {
    if (hor == _vsa->Null_hor())
      return _vsa->Null_vor();
    if (hor == _vsa->Default_hor())
      return _vsa->Default_vor();

    VSYM_OBJ_REP *vor = _vsa->Allocate_vsym_obj(bb, hor, vfr, mpool);
    Add_vor(vor);
    if (Vo_created(vor) &&
        (hor->Attr() == ROR_DEF_BY_CHI)) {
      STMTREP *def_stmt = hor->Stmt_def();
      CODEREP *hor_cr = _vsa->Find_hor_chi_cr(def_stmt, hor);
      // create vor chi on chi stmt if there are other path
      // from def_stmt to current stmt
      if (def_stmt &&
          !bb->Pdom_bbs()->Contains(def_stmt->Bb()) &&
          hor_cr && (hor_cr->Kind() == CK_VAR)) {
        Create_vsym_chi_for_hor(vor->Vsym_obj()->Entry_chi(), hor);
      }
    }
    return vor;
  }

  // create vor phi for hor phi which operands are different HEAP_OBJ
  VSYM_OBJ_REP *Create_vsym_phi_for_hor_phi(VSYM_OBJ *vo, HEAP_OBJ_REP *hor) {
    Is_True(vo != NULL && hor != NULL, ("invalid vo or hor"));
    Is_True(vo->Base_hor() == hor, ("invalid hor"));
    Is_True(hor->Attr() == ROR_DEF_BY_PHI ||
            hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI, ("not by phi, varphi or vorphi"));
    PHI_NODE *hor_phi = hor->Phi_def();
    Is_True(hor_phi && hor_phi->Live() &&
            (hor->Attr() == ROR_DEF_BY_PHI ||
            (hor->Heap_obj()->Base_phi() &&
              hor_phi->Bb() == hor->Heap_obj()->Base_phi()->Bb())),
            ("invalid hor phi"));
    BB_NODE *bb = hor_phi->Bb();
    PHI_LIST *phi_list = Vsa()->Bb_vo_philist(bb);
    Is_True(phi_list, ("phi list is null"));
    PHI_NODE *vor_phi = Search_phi_node(Vo_phi_cache(), phi_list, bb, vo);
    if (vor_phi)
      return (VSYM_OBJ_REP*)vor_phi->RESULT();
    vor_phi = New_phi_node(Vo_phi_cache(), phi_list, vo->Id(), Vsa()->Mem_pool(), bb, TRUE);
    vor_phi->Set_live();
    VSYM_OBJ_REP *res = Create_vsym_obj(vo, bb, Defbb_pool());
    Is_True(res->Vsym_obj()->Ref_cr() != NULL, ("no ref_cr for vor phi res"));
    vor_phi->Set_result((CODEREP *)res);
    VSA_PHI_NODE(vor_phi).Set_res_is_vor();
    VSA_PHI_NODE(vor_phi).Set_opnd_mismatch();
    VSYM_FLD_REP *vfr = vo->Fld_rep_ptr();
    Is_True(vfr, ("field rep is null"));
    for (INT i = 0; i < hor_phi->Size(); ++i) {
      HEAP_OBJ_REP *opnd_hor = (HEAP_OBJ_REP *)hor_phi->OPND(i);
      Is_True(opnd_hor, ("opnd hor is NULL"));
      if (opnd_hor->Attr() != ROR_DEF_BY_LDA &&
          opnd_hor->Is_entry_chi()) {
        // no def on this hor, assume null_vor
        vor_phi->Set_opnd(i, (CODEREP *)Vsa()->Null_vor());
        continue;
      }
      VSYM_OBJ_REP *opnd = Create_vsym_obj_use(opnd_hor, vfr);
      vor_phi->Set_opnd(i, (CODEREP *)opnd);
    }
    res->Set_attr(ROR_DEF_BY_HORPHI);
    res->Set_phi_def(vor_phi);
    Is_Trace(Tracing(), (TFile, "VOC[%d]: Create phivor ", Round()));
    Is_Trace_cmd(Tracing(), Vsa()->Print_vor_phi(vor_phi, TFile));
    Is_Trace(Tracing(), (TFile, " for "));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in phi in BB%d.\n", bb->Id()));
    return res;
  }

  // create chi for vsym on statement which defined the base hor
  VSYM_OBJ_REP *Create_vsym_chi_for_hor(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor)
  {
    Is_True(vor && vor->Is_entry_chi() && hor, ("bad param"));
    if (hor->Attr() == ROR_DEF_BY_CHI ||
        hor->Attr() == ROR_DEF_BY_FREE) {
      STMTREP *def = hor->Stmt_def();
      if (def) {
        CHI_NODE *hor_chi = Vsa()->Find_stmt_hor_chi(def, hor->Heap_obj());
        Is_True(hor_chi != NULL, ("not find hor chi"));
        CHOR *hor_res = (CHOR*)hor_chi->RESULT();
        Is_True(hor_res->first == hor, ("hor mismatch"));
        Append_stmt_vor_chi(hor_res->second, def, vor, Defbb_pool());
      } else {
        Is_Trace(Tracing(), (TFile, "TODO: add default hor instead of null def hor:"));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " skip append vor chi on null def hor\n"));
      }
    }
    else if (hor->Attr() == ROR_DEF_BY_ALLOC) {
      STMTREP *def = hor->Stmt_def();
      Is_True(def != NULL, ("not find hor def stmt"));
      CODEREP* ret = Comp_unit()->Find_return_value(def);
      CODEREP* bind_cr = ret ? ret : def->Rhs();
      Is_True(bind_cr != NULL,  ("not find alloc ret value"));
      Append_stmt_vor_chi(bind_cr, def, vor, Defbb_pool());
    }
    else if (hor->Attr() == ROR_DEF_BY_ALLOCA) {
      STMTREP *def = hor->Stmt_def();
      Is_True(def != NULL, ("not find hor def stmt"));
      Is_True(def->Rhs()->Kind() == CK_OP && def->Rhs()->Opr() == OPR_ALLOCA,
              ("bad alloca defstmt"));
      Append_stmt_vor_chi(def->Lhs(), def, vor, Defbb_pool());
    }
    else if (hor->Attr() == ROR_DEF_BY_PHI ||
            hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI) {
      vor = Create_vsym_phi_for_hor_phi(vor->Vsym_obj(), hor);
    }
    else if (hor->Attr() == ROR_DEF_BY_LDA) {
      // do nothing?
    }
    else {
      // TODO: set this vor's def stmt
      Is_True(FALSE, ("TODO: %d", hor->Attr()));
    }
    return vor;
  }

  // generate or reuse version for vsym_obj_rep push to stack
  IDTYPE Gen_name(VSYM_OBJ_REP *vor, STMTREP *sr) {
    Is_True(vor != _vsa->Null_vor(), ("rename null vor"));
    IDTYPE v = vor->Version();
    if (v <= 0) {
      v = vor->Vsym_obj()->Gen_version();
      vor->Set_version(v);
    }
    Is_True(sr == NULL || vor->Stmt_def() == sr,
            ("def stmt mismatch"));
    vor->Vsym_obj()->Push(vor, sr);
    return v;
  }

  MU_NODE *Find_stmt_vor_mu(STMTREP *stmt, VSYM_OBJ *vo) {
    Is_True(stmt->Stmtrep_id() > 0 && stmt->Stmtrep_id() < _vor_mu_cache.size(),
            ("invalid sr id"));
    ID_MU_MAP *mu_map = _vor_mu_cache[stmt->Stmtrep_id()];
    if (mu_map) {
      ID_MU_MAP::iterator it = mu_map->find(vo->Id());
      if (it != mu_map->end()) {
        return it->second;
      }
    }
    return NULL;
  }

  CHI_NODE *Find_stmt_vor_chi(STMTREP *stmt, VSYM_OBJ *vo) {
    Is_True(stmt->Stmtrep_id() > 0 && stmt->Stmtrep_id() < _vor_chi_cache.size(),
            ("invalid sr id"));
    ID_CHI_MAP *chi_map = _vor_chi_cache[stmt->Stmtrep_id()];
    if (chi_map) {
      ID_CHI_MAP::iterator it = chi_map->find(vo->Id());
      if (it != chi_map->end()) {
        return it->second;
      }
    }
    return NULL;
  }

  CODEREP *Find_stmt_vor_chi_cr(STMTREP *stmt, VSYM_OBJ_REP *vor) {
    CHI_NODE *cnode = Find_stmt_vor_chi(stmt, vor->Vsym_obj());
    if (cnode) {
      CVOR *cres = (CVOR*)cnode->RESULT();
      CVOR *copnd = (CVOR*)cnode->OPND();
      Is_True(cres->second == copnd->second, ("cvor cr mismatch"));
      if (cres->first == vor || copnd->first == vor)
        return cres->second;
    }
    return NULL;
  }

  VSYM_OBJ_REP *Find_stmt_cur_vor(STMTREP *stmt, VSYM_OBJ *vo) {
    MU_NODE *mu = Find_stmt_vor_mu(stmt, vo);
    if (mu) {
      return ((CVOR*)mu->OPND())->first;
    } else {
      CHI_NODE *chi = Find_stmt_vor_chi(stmt, vo);
      if (chi) {
        return ((CVOR*)chi->OPND())->first;
      }
    }
    return NULL;
  }

  void Insert_vor_mu_cache(STMTREP *stmt, VSYM_OBJ_REP *vor, MU_NODE *mu) {
    Is_True(stmt->Stmtrep_id() > 0 && stmt->Stmtrep_id() < _vor_mu_cache.size(),
            ("invalid sr id"));
    ID_MU_MAP *mu_map = _vor_mu_cache[stmt->Stmtrep_id()];
    if (mu_map == NULL) {
      mu_map = CXX_NEW(ID_MU_MAP(3, __gnu_cxx::hash<IDTYPE>(),
                                 std::equal_to<IDTYPE>(),
                                 mempool_allocator<ID_MU_PAIR>(Hva_pool())),
                       Hva_pool());
      _vor_mu_cache[stmt->Stmtrep_id()] = mu_map;
    }
    Is_True(mu_map->find(vor->Vsym_obj_id()) == mu_map->end(), 
            ("vor already add to mu list"));
    mu_map->insert(std::make_pair(vor->Vsym_obj_id(), mu));
  }

  void Insert_vor_chi_cache(STMTREP *stmt, VSYM_OBJ_REP *vor, CHI_NODE *chi) {
    Is_True(stmt->Stmtrep_id() > 0 && stmt->Stmtrep_id() < _vor_chi_cache.size(),
            ("invalid sr id"));
    ID_CHI_MAP *chi_map = _vor_chi_cache[stmt->Stmtrep_id()];
    if (chi_map == NULL) {
      chi_map = CXX_NEW(ID_CHI_MAP(3, __gnu_cxx::hash<IDTYPE>(),
                                  std::equal_to<IDTYPE>(),
                                  mempool_allocator<ID_CHI_PAIR>(Hva_pool())),
                        Hva_pool());
      Is_True(chi_map->find(vor->Vsym_obj_id()) == chi_map->end(),
              ("vor already add to chi list"));
      _vor_chi_cache[stmt->Stmtrep_id()] = chi_map;
    }
    chi_map->insert(std::make_pair(vor->Vsym_obj_id(), chi));
  }

  // create vor mu node and append to stmt vor list
  void Append_stmt_vor_mu(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor) {
    MU_LIST *mu_list = _vsa->Stmt_vor_mu(stmt);
    if (mu_list == NULL) {
      mu_list = CXX_NEW(MU_LIST, Vsa()->Mem_pool());
      Vsa()->Enter_stmt_vor_mu_map(stmt, mu_list);
    }
    if (Find_stmt_vor_mu(stmt, vor->Vsym_obj()) == NULL) {
      MU_NODE *mu = _vsa->Create_stmt_vsym_mu(cr, stmt, vor);
      mu_list->Append(mu);
      Insert_vor_mu_cache(stmt, vor, mu);
    }
  }

  // create vor chi node and append to stmt vor list
  CHI_NODE *Append_stmt_vor_chi(CODEREP *cr, STMTREP *stmt, VSYM_OBJ_REP *vor, MEM_POOL *mpool) {
    CHI_LIST *chi_list = _vsa->Stmt_vor_chi(stmt);
    if (chi_list == NULL) {
      chi_list = CXX_NEW(CHI_LIST, Vsa()->Mem_pool());
      Vsa()->Enter_stmt_vor_chi_map(stmt, chi_list);
    }
    CHI_NODE *chi = Find_stmt_vor_chi(stmt, vor->Vsym_obj());
    if (chi == NULL) {
      chi = _vsa->Create_stmt_vsym_chi(cr, stmt, vor, mpool);
      chi_list->Append(chi);
      Insert_vor_chi_cache(stmt, vor, chi);
    }
    return chi;
  }

  HOR_LIST *Create_ulist(HEAP_OBJ_REP *hor, MEM_POOL *mpool) {
    HOR_LIST *ulist = CXX_NEW(HOR_LIST(), mpool);
    hor->Set_ulist(ulist);
    HOR_PTR_SET *hor_set = CXX_NEW(HOR_PTR_SET(HOR_SET_TABLE_SIZE,
                                               HASHER<HEAP_OBJ_REP>(),
                                               std::equal_to<HEAP_OBJ_REP*>(),
                                               mempool_allocator<HEAP_OBJ_REP*>(Hva_pool())), Hva_pool());
    _ulist_cache->insert(ULIST_PAIR(hor, hor_set));
    return ulist;
  }

  void Dfs_ulist(HEAP_OBJ_REP *hor, HOR_LIST *ulist, HOR_PTR_SET *visited_set,
                 stack<HEAP_OBJ_REP *> *dfs_order,
                 ULIST_CACHE *rev_ulist_map, MEM_POOL *mpool) {
    if (ulist) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER ulist_iter;
      FOR_ALL_NODE(cur_hor, ulist_iter, Init(ulist)) {
        ULIST_CACHE::iterator rev_iter = rev_ulist_map->find(cur_hor);
        if (rev_iter == rev_ulist_map->end()) {
          HOR_PTR_SET *rev_ulist = CXX_NEW(HOR_PTR_SET(31, HASHER<HEAP_OBJ_REP>(),
                                                       std::equal_to<HEAP_OBJ_REP*>(),
                                                       mempool_allocator<HEAP_OBJ_REP*>(mpool)),
                                                       mpool);
          rev_ulist->insert(hor);
          rev_ulist_map->insert(std::make_pair(cur_hor, rev_ulist));
        } else {
          HOR_PTR_SET *rev_ulist = rev_iter->second;
          rev_ulist->insert(hor);
        }

        if (visited_set->find(cur_hor) == visited_set->end()) {
          visited_set->insert(cur_hor);
          HOR_LIST *cur_ulist = cur_hor->Ulist();
          if (cur_ulist) {
            Dfs_ulist(cur_hor, cur_ulist, visited_set, dfs_order, rev_ulist_map, mpool);
          } else {
            dfs_order->push(cur_hor);
          }
        }
      }
    }
    dfs_order->push(hor);
  }

  void Rev_dfs_ulist(HEAP_OBJ_REP *hor, ULIST_CACHE *rev_map,
                     HOR_PTR_SET *scc, HOR_PTR_SET *visited_set)
  {
    scc->insert(hor);
    ULIST_CACHE::iterator iter = rev_map->find(hor);
    if (iter != rev_map->end()) {
      HOR_PTR_SET *ulist = iter->second;
      if (ulist) {
        HOR_PTR_SET::iterator ulist_iter;
        for (ulist_iter = ulist->begin(); ulist_iter != ulist->end(); ulist_iter++) {
          HEAP_OBJ_REP *cur_hor = *ulist_iter;
          if (visited_set->find(cur_hor) == visited_set->end()) {
            visited_set->insert(cur_hor);
            Rev_dfs_ulist(cur_hor, rev_map, scc, visited_set);
          }
        }
      }
    }
  }

  // Merge_ulist: Kosaraju algorithm to calculate SCC hors with DFS order, 
  //              DFS traverse through SCC, update ulist
  void Merge_ulist() {
    Is_Trace(Tracing(), (TFile, "Ulist before merge:\n"));
    Is_Trace_cmd(Tracing(), Print_ulist_cache(TFile));
    CXX_MEM_POOL ulist_pool("Ulist merge pool", FALSE);
    ULIST_CACHE rev_ulist_map;
    HOR_PTR_SET dfs_visited_set;
    stack<HEAP_OBJ_REP *> dfs_order;
    ULIST_CACHE::iterator cached_it;
    // Step 1: Dfs iterate all ulist, build dfs_order and construct reverse graph
    for (cached_it = _ulist_cache->begin(); cached_it != _ulist_cache->end(); cached_it++) {
      if (dfs_visited_set.find(cached_it->first) == dfs_visited_set.end()) {
        dfs_visited_set.insert(cached_it->first);
        HOR_LIST *ulist = cached_it->first->Ulist();
        if (ulist) {
          Dfs_ulist(cached_it->first, ulist, &dfs_visited_set, &dfs_order, &rev_ulist_map, ulist_pool());
        }
      }
    }

    // Step 2: Dfs iterate reverse graph, add scc
    HOR_PTR_SET rev_dfs_visited_set;
    vector<HOR_PTR_SET *> scc_list;
    while (!dfs_order.empty()) {
      HEAP_OBJ_REP *top = dfs_order.top();
      dfs_order.pop();
      if (rev_dfs_visited_set.find(top) == rev_dfs_visited_set.end()) {
        rev_dfs_visited_set.insert(top);
        HOR_PTR_SET *scc = CXX_NEW(HOR_PTR_SET(7, HASHER<HEAP_OBJ_REP>(),
                                               std::equal_to<HEAP_OBJ_REP*>(),
                                               mempool_allocator<HEAP_OBJ_REP*>(ulist_pool())),
                                   ulist_pool());
        scc_list.push_back(scc);
        Rev_dfs_ulist(top, &rev_ulist_map, scc, &rev_dfs_visited_set);
      }
    }

    // Step 3: reverse iterate scc_list, update ulist
    for (int idx = scc_list.size() - 1; idx >= 0; idx--) {
      HOR_PTR_SET *scc = scc_list[idx];
      HOR_PTR_SET::iterator scc_iter;
      HOR_PTR_SET union_hors;
      for (scc_iter = scc->begin(); scc_iter != scc->end(); scc_iter++) {
        HEAP_OBJ_REP *scc_hor = *scc_iter;
        HOR_LIST *ulist = scc_hor->Ulist();
        if (ulist) {
          HEAP_OBJ_REP *cur_hor;
          HOR_LIST_ITER ulist_iter;
          FOR_ALL_NODE(cur_hor, ulist_iter, Init(ulist)) {
            union_hors.insert(cur_hor);
            if (cur_hor->Ulist()) {
              HEAP_OBJ_REP *sub_hor;
              HOR_LIST_ITER sub_iter;
              FOR_ALL_NODE(sub_hor, sub_iter, Init(cur_hor->Ulist())) {
                union_hors.insert(sub_hor);
              }
            }
          }
        }
      }
      for (scc_iter = scc->begin(); scc_iter != scc->end(); scc_iter++) {
        HEAP_OBJ_REP *scc_hor = *scc_iter;
        HOR_LIST *ulist = scc_hor->Ulist();
        if (ulist) {
          HOR_PTR_SET::iterator union_iter;
          for (union_iter = union_hors.begin(); union_iter != union_hors.end(); union_iter++) {
            if (scc_hor != *union_iter) {
              Find_or_append_ulist_hor(scc_hor, *union_iter);
            }
          }
        }
      }
    }
    Is_Trace(Tracing(), (TFile, "Ulist after merge:\n"));
    Is_Trace_cmd(Tracing(), Print_ulist_cache(TFile));
  }

  HOR_LIST *Clone_ulist(HEAP_OBJ_REP *res, HEAP_OBJ_REP *opnd, MEM_POOL *mpool) {
    HOR_LIST *ulist = CXX_NEW(HOR_LIST(), mpool);
    res->Set_ulist(ulist);
    HOR_PTR_SET *hor_set = CXX_NEW(HOR_PTR_SET(HOR_SET_TABLE_SIZE,
                                               HASHER<HEAP_OBJ_REP>(),
                                               std::equal_to<HEAP_OBJ_REP*>(),
                                               mempool_allocator<HEAP_OBJ_REP*>(Hva_pool())), Hva_pool());
    _ulist_cache->insert(ULIST_PAIR(res, hor_set));
    if (opnd->Ulist()) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(opnd->Ulist())) {
        Is_True(!Vsa()->Is_special_hor(cur_hor), ("special hor"));
        ulist->Append(CXX_NEW(HOR_NODE(cur_hor), Vsa()->Mem_pool()));
        hor_set->insert(cur_hor);
      }
    }
    return ulist;
  }

  BOOL Find_or_append_ulist_hor(HEAP_OBJ_REP *hor, HEAP_OBJ_REP *ulist_hor) {
    HOR_LIST *ulist = hor->Ulist();
    Is_True_Ret(ulist, ("ulist not created before insert"), FALSE);
    ULIST_CACHE::iterator cached_it = _ulist_cache->find(hor);
    Is_True_Ret(cached_it != _ulist_cache->end(), ("ulist cache not setup"), FALSE);
    HOR_PTR_SET *hor_set = cached_it->second;
    Is_True_Ret(hor_set, ("ulist cached ho_set is null"), FALSE);
    if (hor_set->find(ulist_hor) == hor_set->end()) {
      hor_set->insert(ulist_hor);
      ulist->Append(CXX_NEW(HOR_NODE(ulist_hor), Vsa()->Mem_pool()));
      return FALSE;
    }
    return TRUE;
  }

  void Print_ulist_cache(FILE *fp)
  {
    fprintf(fp, "%s", SBar);
    ULIST_CACHE::iterator cached_it;
    for (cached_it = _ulist_cache->begin(); cached_it != _ulist_cache->end(); cached_it++) {
      cached_it->first->Print_detail(fp);
    }
    fprintf(fp, "%s", SBar);
  }

  template<typename RSCOBJ>
  PHI_NODE *Search_phi_node(PHI_CACHE *phi_cache, PHI_LIST *phi_list, BB_NODE *bb, RSCOBJ *rsc_obj) {
    Is_True(phi_cache, ("phi cache not setup"));
    if (phi_cache) {
      ID_PHI_MAP *bb_phi_cache = phi_cache->at(bb->Id());
      Is_True_Ret(bb_phi_cache, ("phi cache not set up for bb%d", bb->Id()), NULL);
      ID_PHI_MAP::iterator phi_it = bb_phi_cache->find(rsc_obj->Id());
      if (phi_it != bb_phi_cache->end()) {
        return phi_it->second;
      }
    } else {
      return Vsa()->Search_phi_node(phi_list, rsc_obj);
    }
    return NULL;
  }

  PHI_NODE *New_phi_node(PHI_CACHE *phi_cache, PHI_LIST *phi_list, IDTYPE id,
                         MEM_POOL *pool, BB_NODE *bb, BOOL append = FALSE) {
    Is_True(phi_cache, ("phi cache not setup"));
    PHI_NODE *phi_node = phi_list->New_phi_node(id, pool, bb, append);
    if (phi_cache) {
      ID_PHI_MAP *bb_phi_cache = phi_cache->at(bb->Id());
      Is_True_Ret(bb_phi_cache, ("phi cache not set up for bb%d", bb->Id()), phi_node);
      Is_True_Ret(bb_phi_cache->find(id) == bb_phi_cache->end(),
                  ("phi node with id%d with bb%d already added", id, bb->Id()), phi_node);
      bb_phi_cache->insert(std::make_pair(id, phi_node));
    }
    return phi_node;
  }


  // Set vo updated
  void Set_vo_updated(VSYM_OBJ *vo) {
    Is_True(vo->Id() < _next_vo_id &&
            _vo_created->find(vo) ==  _vo_created->end(),
            ("vo is created just now"));
    _vo_updated->insert(vo);
  }

  // check if vsym_obj is created in current iteration
  BOOL Vo_created(VSYM_OBJ_REP *vor) const {
    return (_vo_created &&
            _vo_created->find(vor->Vsym_obj()) != _vo_created->end()) ?
           TRUE : FALSE;
  }

  // check if there is new vsym_obj created in current iteration
  BOOL Has_vo_created() const {
    return _vo_created && _vo_created->size() > 0;
  }

  // check if vsym_obj is updated in current iteration
  BOOL Vo_updated(VSYM_OBJ_REP *vor) const {
    return (_vo_updated &&
            _vo_updated->find(vor->Vsym_obj()) != _vo_updated->end()) ?
           TRUE : FALSE;
  }

  // check if there is new vsym_obj updated in current iteration
  BOOL Has_vo_updated() const {
    return _vo_updated && _vo_updated->size() > 0;
  }

  // check if vsym_obj should be visited in current iteration
  BOOL Visit_vsym_obj(VSYM_OBJ_REP *vor) const {
    return Vo_created(vor) || Vo_updated(vor);
  }

  // get first vo id
  IDTYPE First_vo_id() const {
    return _first_vo_id;
  }

  // get next vo id
  IDTYPE Next_vo_id() const {
    return _next_vo_id;
  }

  // check if stmt should be visited in current iteration
  BOOL Visit_stmt(STMTREP* sr) const {
    Is_True(sr->Stmtrep_id() > 0 && sr->Stmtrep_id() < _sr_counter.size(),
            ("invalid sr id"));
    Is_True(_sr_counter[sr->Stmtrep_id()] <= _round + 1,
            ("invalid sr counter"));
    return _sr_counter[sr->Stmtrep_id()] >= _round;
  }

  // set stmt should be visited in this iteration so newly created ho/vo
  // can be renamed correctly
  void Set_visit_later(STMTREP* sr) {
    Is_True(sr->Stmtrep_id() > 0 && sr->Stmtrep_id() < _sr_counter.size(),
            ("invalid sr id"));
    if (_sr_counter[sr->Stmtrep_id()] < _round) {
      _sr_counter[sr->Stmtrep_id()] = _round;
    }
  }

  // set stmt should be visited in next iteration
  void Set_visit_next(STMTREP* sr) {
    Is_True(sr->Stmtrep_id() > 0 && sr->Stmtrep_id() < _sr_counter.size(),
            ("invalid sr id"));
    Is_True(_sr_counter[sr->Stmtrep_id()] == _round ||
            _sr_counter[sr->Stmtrep_id()] == _round + 1,
            ("invalid sr counter"));
    if (_sr_counter[sr->Stmtrep_id()] == _round) {
      _sr_counter[sr->Stmtrep_id()] = _round + 1;
      Is_Trace(Tracing(),
               (TFile, "HVA[%d]: set sr%d %s visit next.\n", Round(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }

    // need to run next round
    if (_next_round == FALSE)
      _next_round = TRUE;
  }

  // check if current iteration is the first round
  BOOL First_round() const {
    return _round == 0;
  }

  // return TRUE if next round is needed
  BOOL Need_next_round() const {
    return _next_round;
  }

  // get round
  INT Round() const {
    return _round;
  }

  // increase the round and reset _next_round flag
  INT Incr_round() {
    _next_round = FALSE;
    ++ _round;
    return _round;
  }

  // is tracing on
  BOOL Tracing() const {
    return _tracing;
  }

private:
  // begin an new iteration
  void Begin_iteration() {
    // push local pool
    OPT_POOL_Push(&_local_pool, VSA_NHV_TRACE_FLAG);
    // create _ho_created, _ho_updated, _vo_created
    _ho_created = CXX_NEW(HO_PTR_SET(13,
                                     HASHER<HEAP_OBJ>(),
                                     std::equal_to<HEAP_OBJ*>(),
                                     mempool_allocator<HEAP_OBJ*>(&_local_pool)),
                          &_local_pool);
    _ho_updated = CXX_NEW(HO_PTR_SET(7,
                                     HASHER<HEAP_OBJ>(),
                                     std::equal_to<HEAP_OBJ*>(),
                                     mempool_allocator<HEAP_OBJ*>(&_local_pool)),
                          &_local_pool);
    _vo_created = CXX_NEW(VO_PTR_SET(13,
                                     HASHER<VSYM_OBJ>(),
                                     std::equal_to<VSYM_OBJ*>(),
                                     mempool_allocator<VSYM_OBJ*>(&_local_pool)),
                          &_local_pool);
    _vo_updated = CXX_NEW(VO_PTR_SET(7,
                                     HASHER<VSYM_OBJ>(),
                                     std::equal_to<VSYM_OBJ*>(),
                                     mempool_allocator<VSYM_OBJ*>(&_local_pool)),
                          &_local_pool);
    // call _hor_cache's Begin_iteration
    _hor_cache.Begin_iteration();
  }

  // end current iteration
  void End_iteration() {
    // call _hor_cache's End_iteratioon
    _hor_cache.End_iteration();
    OPT_POOL_Pop(&_local_pool, VSA_NHV_TRACE_FLAG);
    _ho_created = NULL;
    _ho_updated = NULL;
    _vo_created = NULL;
    _vo_updated = NULL;
  }

  // perform a single step in one iteration. _VISITOR can be:
  // HEAP_OBJ_CREATION, HEAP_OBJ_RENAMING
  // VSYM_OBJ_CREATION, VSYM_OBJ_RENAMING
  template<typename _VISITOR> void
  Perform_single_step() {
    _VISITOR action(this);
    action.Perform();
  }

  // verifier
  BOOL Verify();
public:
  // perform the iterative heap_obj/vsym_obj creation and renaming
  void Perform(INT max_iter);

  // API for HVA with final ho renaming
private:
  // process call for models
  void Process_call_for_rsc(STMTREP *sr);

  // process call for malloc
  void Process_call_for_malloc(STMTREP *sr);

  // process cr for lda
  void Process_cr_for_lda(STMTREP *sr, CODEREP *cr);

  // process call and lda for vsym creation/renaming
  void Process_call_and_lda_for_vsym();

  // process call for heap_obj creation/renaming
  void Process_call_for_heap();

public:
  // find hor on cr
  HEAP_OBJ_REP *Find_cr_hor(STMTREP *sr, CODEREP *cr);

  // perform the iterative vsym_obj creation and renaming
  void Perform_with_delayed_ho_renaming(INT max_iter);

  pair<CODEREP *, CODEREP *> Callee_frees_heap_memory(STMTREP *sr);
};  // HEAP_VSYM_ANALYSIS

// ============================================================================
// CFG_VISITOR_BASE
//
// Base class for 4 actions:
//  - HEAP_OBJ_CREATION
//  - HEAP_OBJ_RENAMING
//  - VSYM_OBJ_CREATION
//  - VSYM_OBJ_RENAMING
// ============================================================================
template<typename _VISITOR>
class CFG_VISITOR_BASE {
protected:
  HEAP_VSYM_ANALYSIS *_hva; // HEAP_VSYM_ANALYSIS pointer
  COMP_UNIT *_comp_unit;    // compile unit pointer
  OPT_STAB  *_opt_stab;     // opt symbol table pointer
  CFG       *_cfg;          // cfg pointer
  VSA       *_vsa;          // vsa pointer
  DNA_NODE  *_dna;          // dna pointer
  MEM_POOL  *_defbb_pool;   // bb defs pool
  _VISITOR  *_visitor;      // visitor pointer
  BOOL       _tracing;      // tracing flag

protected:
  // constructor
  CFG_VISITOR_BASE(HEAP_VSYM_ANALYSIS *hva, _VISITOR* visitor)
   : _hva(hva),
     _comp_unit(hva->Comp_unit()), _opt_stab(hva->Comp_unit()->Opt_stab()),
     _cfg(hva->Comp_unit()->Cfg()), _vsa(hva->Comp_unit()->Vsa()),
     _dna(hva->Comp_unit()->Dna()), _defbb_pool(hva->Defbb_pool()),
     _visitor(visitor), _tracing(Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG)) { }

public:
  COMP_UNIT *Comp_unit() const       { return _comp_unit; }
  OPT_STAB  *Opt_stab() const        { return _opt_stab;  }
  CFG       *Cfg() const             { return _cfg; }
  VSA       *Vsa() const             { return _vsa; }
  DNA_NODE  *Dna() const             { return _dna; }
  MEM_POOL  *Defbb_pool() const      { return _defbb_pool; }
  BOOL       Tracing() const         { return _tracing; }

  // pseudo implementation
  void Initialize()                  { }
  void Finalize()                    { }
  void Enter_bb(BB_NODE* bb)         { }
  void Enter_dom_bb(BB_NODE* bb)     { }
  void Exit_dom_bb(BB_NODE* bb)      { }
  void Exit_bb(BB_NODE* bb)          { }

  // forward process stmtrep
  void Process_stmt_fwd(STMTREP* sr) {
    Process_stmtrep<TRUE>(sr);
  }

  // backward process stmtrep
  void Process_stmt_rev(STMTREP* sr) {
    Process_stmtrep<FALSE>(sr);
  }

  // Process_coderep: _T is the return value
  template<typename _T>
  _T Process_coderep(STMTREP* sr, CODEREP* cr, UINT flag) {
    switch (cr->Kind()) {
    case CK_LDA:
      // visit LDA if V_LDA is set
      if (_VISITOR::CR_VFLAG & V_LDA)
        return _visitor->template Process_cr<CK_LDA>(sr, cr, flag);
      break;

    case CK_CONST:
      // visit CONST if V_CONST is set
      if (_VISITOR::CR_VFLAG & V_CONST)
        return _visitor->template Process_cr<CK_CONST>(sr, cr, flag);
      break;

    case CK_RCONST:
      // visit RCONST if V_RCONST is set
      if (_VISITOR::CR_VFLAG & V_RCONST)
        return _visitor->template Process_cr<CK_RCONST>(sr, cr, flag);
      break;

    case CK_VAR:
      // visit VAR if V_VAR is set
      if (_VISITOR::CR_VFLAG & V_VAR)
        return _visitor->template Process_cr<CK_VAR>(sr, cr, flag);
      break;

    case CK_IVAR:
      // visit IVAR if V_IVAR is set
      if (_VISITOR::CR_VFLAG & V_IVAR)
        return _visitor->template Process_cr<CK_IVAR>(sr, cr, flag);
      break;

    case CK_OP:
      // visit OP if V_OP is set
      if (_VISITOR::CR_VFLAG & V_OP)
        return _visitor->template Process_cr<CK_OP>(sr, cr, flag);
      break;

    default:
      Is_True(FALSE, ("unknown cr"));
    }
    return _T();
  }  // Process_coderep

  BOOL Visit_stmt(STMTREP *sr) {
    return _hva->Visit_stmt(sr);
  }

  // Process_stmtrep: _FWD indicates if stmtrep is processed in forward
  // or reverse pass.
  template<BOOL _FWD>
  void Process_stmtrep(STMTREP* sr) {
    OPERATOR opr = sr->Opr();
    switch (opr) {
    case OPR_STID:
      // only visit STID if V_STID is set
      if ((_VISITOR::SR_VFLAG & V_STID) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_STID, _FWD>(sr);
      break;

    case OPR_ISTORE:
      // only visit ISTORE if V_ISTORE is set
      if ((_VISITOR::SR_VFLAG & V_ISTORE) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_ISTORE, _FWD>(sr);
      break;

    case OPR_MSTORE:
      // only visit MSTORE if V_MSTORE is set
      if ((_VISITOR::SR_VFLAG & V_MSTORE) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_MSTORE, _FWD>(sr);
      break;

    case OPR_CALL:
    case OPR_ICALL:
      // process CALL and ICALL in the same function
      if (_VISITOR::SR_VFLAG & (V_CALL | V_ICALL))
        _visitor->template Process_sr<OPR_CALL, _FWD>(sr);
      break;

    case OPR_INTRINSIC_CALL:
      // only visit INTRINSIC_CALL if V_INTRINSIC_CALL is set
      if ((_VISITOR::SR_VFLAG & V_INTRINSIC_CALL) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_INTRINSIC_CALL, _FWD>(sr);
      break;

    case OPR_TRUEBR:
      // only visit TRUEBR if V_TRUEBR is set
      if ((_VISITOR::SR_VFLAG & V_TRUEBR) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_TRUEBR, _FWD>(sr);
      break;

    case OPR_FALSEBR:
      // only visit FALSEBR if V_FALSEBR is set
      if ((_VISITOR::SR_VFLAG & V_FALSEBR) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_FALSEBR, _FWD>(sr);
      break;

    case OPR_COMPGOTO:
      // only visit COMPGOTO if V_COMPGOTO is set
      if ((_VISITOR::SR_VFLAG & V_COMPGOTO) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_COMPGOTO, _FWD>(sr);
      break;

#if 0
    case OPR_GOTO:
      // only visit GOTO if V_GOTO is set
      if ((_VISITOR::SR_VFLAG & V_GOTO) && _hva->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_GOTO, _FWD>(sr);
      break;
#endif

    case OPR_OPT_CHI:
      // only visit OPT_CHI if V_OPT_CHI is set
      if ((_VISITOR::SR_VFLAG & V_OPT_CHI) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_OPT_CHI, _FWD>(sr);
      break;

    case OPR_RETURN:
      // only visit RETURN if V_RETURN is set regardless if return stmt
      // is marked to visit in this iteration
      if ((_VISITOR::SR_VFLAG & V_RETURN))
        _visitor->template Process_sr<OPR_RETURN, _FWD>(sr);
      break;

    case OPR_ASM_STMT:
      // only visit ASM_STMT if V_ASM_STMT is set
      if ((_VISITOR::SR_VFLAG & V_ASM_STMT) && _visitor->Visit_stmt(sr))
        _visitor->template Process_sr<OPR_ASM_STMT, _FWD>(sr);
      break;

    // ignore the following operator
    case OPR_GOTO:
    case OPR_AGOTO:
    case OPR_DEALLOCA: // don't check DEALLOCA because it's compiler generated
    case OPR_EVAL:
    case OPR_LABEL:
    case OPR_PRAGMA:
    case OPR_XPRAGMA:
    case OPR_PREFETCH:
    case OPR_FORWARD_BARRIER:
    case OPR_BACKWARD_BARRIER:
      break;

    // assert the following operator
    case OPR_ISTOREX:
    case OPR_PICCALL:
    case OPR_RETURN_VAL:
    case OPR_VFCALL:
    default:
      Is_True(FALSE, ("unexpected %s", OPERATOR_name(opr) + 4));
      break;
    }
  }  // Process_stmtrep

};  // CFG_VISITOR_BASE

// struct about how the HEAP_OBJ* is used by the _stmt
struct STMT_HOR_SET {
private:
  HEAP_VSYM_ANALYSIS *_hva; // hva pointer
  STMTREP            *_stmt;    // the stmt uses the _ho_set
  HO_MOD_REF_MAP     *_hor_set; // how the HEAP_OBJ_REP* is used by _stmt

private:
  // append hor's field hor to _ho_set because vsym based on them can be
  // accessed by the field
  void Add_field_hor(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
    FIELD_OBJ_REP* fl = hor->Flist();
    while (fl != NULL) {
      HEAP_OBJ_REP* fobj = fl->Hor();
      if (!_hva->Vsa()->Is_special_hor(fobj))
        Add(fobj, cr, mr);
      fl = fl->Next();
    }
  }

  // append hor's sub hor (attached on vor which is based on the hor) because
  // they can be accessed in call/return value by the base pointer
  void Add_sub_hor(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
    HOR_VO_LIST_ITER iter(hor);
    VSYM_OBJ *fld_vo;
    FOR_ALL_NODE(fld_vo, iter, Init()) {
      // find current version
      VSYM_OBJ_REP *fld_vor = _hva->Find_stmt_cur_vor(_stmt, fld_vo);
      HEAP_OBJ_REP *fld_hor;
      if (fld_vor != NULL &&
          (fld_hor = fld_vor->Hor()) != NULL &&
          !_hva->Vsa()->Is_special_hor(fld_hor)) {
        Add(fld_hor, cr, mr);
      }
    }
  }

  // append hor's ulist hor into _ho_set because vsym based on them can be
  // accessed by these aliased hor
  void Add_ulist_hor(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
    HOR_LIST *ulist = hor->Ulist();
    if (!ulist)
      return;
    HEAP_OBJ_REP *cur_hor;
    HOR_LIST_ITER hor_list_iter;
    FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
      if (!_hva->Vsa()->Is_special_hor(cur_hor))
        Add(cur_hor, cr, mr);
    }
  }

public:
  // constructor
  STMT_HOR_SET(HEAP_VSYM_ANALYSIS *hva, STMTREP *stmt, MEM_POOL *mp)
    : _hva(hva), _stmt(stmt),
      _hor_set(CXX_NEW(HO_MOD_REF_MAP(31,
                                      HASHER<HEAP_OBJ_REP>(),
                                      std::equal_to<HEAP_OBJ_REP*>(),
                                      mempool_allocator<HO_MOD_REF>(mp)),
                       mp)) { }

  // get the stmtrep
  STMTREP        *Stmtrep() const { return _stmt;   }

  // get the ho set
  HO_MOD_REF_MAP *Hor_set() const { return _hor_set; }

  VSA            *Vsa()           { return _hva->Vsa(); }

  // check if HEAP_OBJ_REP is in the set
  BOOL            Contains(HEAP_OBJ_REP* hor) const {
    return _hor_set->find(hor) != _hor_set->end();
  }

  // append ho and its mod/ref into ho_set
  void Add(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
    Is_True(cr->Kind() != CK_IVAR || cr->Opr() != OPR_PARM,
            ("cr is PARM"));
    Is_True(!Vsa()->Is_special_hor(hor), ("special hor added"));
    HO_MOD_REF_MAP::iterator it = _hor_set->find(hor);
    if (it != _hor_set->end()) {
      Is_Trace(Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG) && it->second.first != cr,
               (TFile, "WARN[VOC]: same ho passed to callee in different cr.\n"));
      it->second.second |= mr;
      // call args may be aliased with sub field
      // always bind with cr which is hor's owner
      HEAP_OBJ_REP *cr_hor = Vsa()->Cr_2_heap_obj(cr);
      if (cr_hor && cr_hor->Heap_obj() == hor->Heap_obj()) {
        it->second.first = cr;
      }
    }
    else {
      _hor_set->insert(HO_MOD_REF(hor, CR_MOD_REF(cr, mr)));
      // if not malloc, append field hor
      if (cr->Kind() != CK_OP || !OPERATOR_is_call(cr->Opr())) {
        Add_sub_hor(hor, cr, mr);
        Add_ulist_hor(hor, cr, mr);
      }
    }
  }

};  // STMT_HOR_SET

#endif /* opt_vsa_hva_INCLUDEDD */
