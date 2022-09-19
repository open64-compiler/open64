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
// Module: opt_vsa_vsym.cxx
//
// ============================================================================
//

// ============================================================================
// VSA::Perform_heap_vsym_analysis
//   Perform heap and vsym creation and renaming iteratively
//   do {
//     Create_heap_obj();
//     Rename_heap_obj();
//     Create_vsym_obj();
//     Rename_vsym_obj();
//   } while (Has_pending_ivar() or max iteration count reached);
// ============================================================================

#include "opt_vsa_hva.h"
#include "erglob.h"
#include "config_vsa.h"
#include "builtin_rule_defs.h"
#include "opt_main.h"
#include "opt_addr_util.h"
#include "opt_vsa_util.h"
#include "opt_vsa_eh.h"
#include "opt_vsa_rbc.h"
#include "intrn_info.h"
#include "vsa_annot.h"
#include <list>

// hor not found during fixup, has to be resolved later
#define HOR_NOT_FOUND ((HEAP_OBJ_REP*)0)
// hor is pending in current fixup run
#define HOR_PENDING   ((HEAP_OBJ_REP*)1)
// check if hor is valid
inline BOOL Is_hor_valid(HEAP_OBJ_REP *hor) {
  return hor > HOR_PENDING;
}

// ============================================================================
// HOR_PHI_FIXABLE
//
// Check if the hor on recursive dependent VAR/VOR can be fixed or not.
// ============================================================================

class HOR_PHI_FIXABLE {
public:
  enum STAT {
    IN_STACK,     // phi is in stack
    NOT_FIXABLE,  // phi is fixable
    FIXABLE,      // phi is not fixable
  };
  typedef pair<uintptr_t, STAT> PTR_STAT_PAIR;
  typedef hash_map<uintptr_t, STAT, __gnu_cxx::hash<uintptr_t>,
                   std::equal_to<uintptr_t>,
                   mempool_allocator<PTR_STAT_PAIR> > PTR_STAT_MAP;

private:
  HEAP_VSYM_ANALYSIS  *_hva;
  PTR_STAT_MAP        *_ptr_map;    // ptr->fixable/not_fixable map
  PHI_STACK           *_phi_stack;  // phi stack
  MEM_POOL             _lpool;

  HOR_PHI_FIXABLE(const HOR_PHI_FIXABLE&);            // no copy-ctor
  HOR_PHI_FIXABLE& operator=(const HOR_PHI_FIXABLE&); // no assign-oper

public:
  HOR_PHI_FIXABLE(HEAP_VSYM_ANALYSIS *hva)
    : _hva(hva) {
    OPT_POOL_Initialize(&_lpool, "hor phi fixable", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_lpool, VSA_DUMP_FLAG);
    _ptr_map = CXX_NEW(PTR_STAT_MAP(3, __gnu_cxx::hash<uintptr_t>(),
                                    std::equal_to<uintptr_t>(),
                                    mempool_allocator<PTR_STAT_PAIR>(&_lpool)),
                       &_lpool);
    _phi_stack = CXX_NEW(PHI_STACK(mempool_allocator<PHI_NODE*>()),
                         &_lpool);
  }

  ~HOR_PHI_FIXABLE() {
    Is_True(_phi_stack->size() == 0, ("phi stack corrupted"));
    OPT_POOL_Pop(&_lpool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_lpool, VSA_DUMP_FLAG);
  }

private:
  STAT Is_var_fixable(CODEREP *cr) {
    Is_True(cr->Kind() == CK_VAR, ("bad var"));

    if (_hva->Cr_2_heap_obj(cr) != NULL)
      return FIXABLE;

    PTR_STAT_MAP::iterator it = _ptr_map->find((uintptr_t)cr);
    if (it != _ptr_map->end())
      return it->second;

    STAT stat = IN_STACK;
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
        cr->Is_var_volatile()) {
      stat = FIXABLE;
    }
    else if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
      Is_True(cr->Defphi(), ("bad defphi"));
      stat = Is_phi_fixable<CODEREP>(cr->Defphi());
    }
    else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def, ("bad defchi"));
      if (def->Opr() == OPR_OPT_CHI ||
          OPERATOR_is_call(def->Opr())) {
        stat = FIXABLE;
      }
      else {
        CHI_NODE* chi = cr->Defchi();
        Is_True(chi, ("bad chinode"));
        stat = Is_var_fixable(chi->OPND());
      }
    }
    else {
      STMTREP *def = cr->Get_defstmt();
      Is_True(def && def->Lhs() == cr && def->Rhs(), ("bad defstmt"));
      stat = Is_cr_fixable(def->Rhs());
    }

    // add to map
    (*_ptr_map)[(uintptr_t)cr] = stat;
    return stat;
  }

  STAT Is_ivar_fixable(CODEREP *cr) {
    Is_True(cr->Kind() == CK_IVAR, ("bad ivar"));

    if (_hva->Cr_2_heap_obj(cr) != NULL)
      return FIXABLE;

    PTR_STAT_MAP::iterator it = _ptr_map->find((uintptr_t)cr);
    if (it != _ptr_map->end())
      return it->second;

    STAT stat = IN_STACK;
    VSYM_OBJ_REP *vor = _hva->Cr_2_vor(cr);
    if (vor == NULL) {
      CODEREP *base = Find_ilod_base(cr->Ilod_base());
      if (base != NULL) {
        stat = Is_cr_fixable(base);
        stat = (stat == IN_STACK) ? IN_STACK : NOT_FIXABLE;
      }
      else {
        stat = FIXABLE;
      }
    }
    else {
      stat = Is_vor_fixable(vor);
    }

    // add to map
    (*_ptr_map)[(uintptr_t)cr] = stat;
    return stat;
  }

  STAT Is_cr_fixable(CODEREP *cr) {
    switch(cr->Kind()) {
    case CK_LDA:
    case CK_CONST:
    case CK_RCONST:
      return FIXABLE;
    case CK_VAR:
      return Is_var_fixable(cr);
    case CK_IVAR:
      return Is_ivar_fixable(cr);
    case CK_OP:
      cr = Find_ilod_base(cr);
      return cr ? Is_cr_fixable(cr) : FIXABLE;
    default:
      Is_True(FALSE, ("unknown cr kind"));
      return FIXABLE;
    }
  }

  STAT Is_vor_fixable(VSYM_OBJ_REP *vor) {
    if (vor == _hva->Vsa()->Null_vor() ||
        vor->Hor() != NULL)
      return FIXABLE;

    PTR_STAT_MAP::iterator it = _ptr_map->find((uintptr_t)vor);
    if (it != _ptr_map->end())
      return it->second;

    HEAP_OBJ_REP *hor;
    STAT stat = IN_STACK;
    switch (vor->Attr()) {
    case ROR_DEF_BY_PHI:
    case ROR_DEF_BY_HORPHI:
      Is_True(vor->Phi_def(), ("bad vor phidef"));
      stat = Is_phi_fixable<VSYM_OBJ_REP>(vor->Phi_def());
      break;

    case ROR_DEF_BY_CHI:
      hor = vor->Vsym_obj()->Base_hor();
      Is_True(hor, ("bad base hor"));
      if (vor->Is_entry_chi() || hor->Is_entry_chi()) {
        stat = FIXABLE;
      }
      else {
        STMTREP *def = vor->Stmt_def();
        Is_True(def, ("bad defchi"));
        if (def->Opr() == OPR_OPT_CHI ||
            OPERATOR_is_call(def->Opr()) ||
            def->Opr() == OPR_ISTORE ||
            def->Opr() == OPR_MSTORE ||
            (hor->Attr() == ROR_DEF_BY_ALLOCA && hor->Stmt_def() == def)) {
          stat = FIXABLE;
        }
        else {
          Is_True(FALSE, ("TODO: chi def by %s", OPERATOR_name(def->Opr()) + 4));
          stat = FIXABLE;
        }
      }
      break;

    case ROR_DEF_BY_COPY:
    case ROR_DEF_BY_ISTORE:
      Is_True(vor->Stmt_def(), ("bad defstmt"));
      stat = Is_cr_fixable(vor->Stmt_def()->Rhs());
      break;

    case ROR_DEF_BY_NONE:
    default:
      Is_True(FALSE, ("vor def by none?"));
      break;
    }

    // add to map
    (*_ptr_map)[(uintptr_t)vor] = stat;
    return stat;
  }

  BOOL In_stack(PHI_NODE *phi) {
    PHI_STACK::iterator end = _phi_stack->end();
    return std::find(_phi_stack->begin(), end, phi) != end;
  }

  template<typename _OBJECT>
  STAT Is_phi_fixable(PHI_NODE *phi) {
    Is_True(!Phi_res_is_hor(phi), ("bad hor phi"));

    PTR_STAT_MAP::iterator it = _ptr_map->find((uintptr_t)phi);
    if (it != _ptr_map->end())
      return it->second;

    if (In_stack(phi))
      return IN_STACK;

    _phi_stack->push_back(phi);

    PHI_OPND_ITER phi_opnd_iter(phi);
    CODEREP *opnd_cr;
    STAT stat = FIXABLE;
    BOOL in_stack = FALSE;
    BOOL is_vor = Phi_res_is_vor(phi);
    FOR_ALL_ELEM(opnd_cr, phi_opnd_iter, Init()) {
      stat = is_vor ? Is_vor_fixable((VSYM_OBJ_REP*)opnd_cr)
                    : Is_var_fixable(opnd_cr);
      if (stat == NOT_FIXABLE)
        break;
      if (stat == IN_STACK && in_stack == FALSE)
        in_stack = TRUE;
    }

    Is_True(!_phi_stack->empty() && _phi_stack->back() == phi,
            ("stack top mismatch"));
    _phi_stack->pop_back();

    if (stat != NOT_FIXABLE && in_stack == TRUE)
      stat = IN_STACK;

    // reset IN_STACK for operands
    if (stat == NOT_FIXABLE && in_stack == TRUE) {
      FOR_ALL_ELEM(opnd_cr, phi_opnd_iter, Init()) {
        if ((*_ptr_map)[(uintptr_t)opnd_cr] == IN_STACK)
          (*_ptr_map)[(uintptr_t)opnd_cr] = NOT_FIXABLE;
      }
    }

    // add to map
    (*_ptr_map)[(uintptr_t)phi] = stat;
    return stat;
  }

public:
  BOOL Check_cr_fixable(CODEREP *cr) {
    STAT stat = Is_cr_fixable(cr);
    return stat != NOT_FIXABLE ? TRUE : FALSE;
  }

}; // HOR_PHI_FIXABLE

// ============================================================================
// HOR_PHI_FIXUP
//
// To track the hor phi list which has operands defined by other phi visited
// later. These phi should be fixed up when all phis are visited.
// ============================================================================

class HOR_PHI_FIXUP {
public:
  // map from PHI_NODE to set of HO which may reach the phi
  typedef pair<PHI_NODE*, HO_PTR_SET*> PHI_HO_PAIR;
  typedef hash_map<PHI_NODE*, HO_PTR_SET*, HASHER<PHI_NODE>,
                   std::equal_to<PHI_NODE*>,
                   mempool_allocator<PHI_HO_PAIR> > PHI_HO_MAP;

  // iterator for all vor/var phi visited in DFS search
  typedef PHI_HO_MAP::const_iterator phi_iterator;

  // iterator for all hor created for var/vor phi with diff hor operands
  typedef HOR_PTR_SET::const_iterator hor_iterator;

private:
  HEAP_VSYM_ANALYSIS  *_hva;
  MEM_POOL            *_lpool;
  mutable PHI_HO_MAP  *_phi_visited;   // all phi nodes visited
  mutable VOR_PTR_SET *_vor_visited;   // all vor nodes visited
  mutable CR_PTR_SET  *_cr_visited;    // all cr nodes visited
  mutable PHI_PTR_SET *_phi_fixup;     // all phi nodes visited in fixup
  HOR_PTR_SET         *_phi_hor_set;   // new hor created for phi
  PHI_STACK           *_phi_stack;     // phi stack during U-D traversal
  BOOL                 _do_fixup;      // is in fixup stage

  HOR_PHI_FIXUP(const HOR_PHI_FIXUP&);            // disable copy ctor
  HOR_PHI_FIXUP& operator=(const HOR_PHI_FIXUP&); // disable assign operator

public:
  // constructor
  HOR_PHI_FIXUP(HEAP_VSYM_ANALYSIS *hva, MEM_POOL *lpool)
    : _hva(hva), _lpool(lpool), _do_fixup(FALSE) {
    OPT_POOL_Push(lpool, VSA_DUMP_FLAG);
    _phi_visited = CXX_NEW(PHI_HO_MAP(3, HASHER<PHI_NODE>(),
                                      std::equal_to<PHI_NODE*>(),
                                      mempool_allocator<PHI_HO_PAIR>(lpool)),
                           lpool);
    _vor_visited = CXX_NEW(VOR_PTR_SET(3, HASHER<VSYM_OBJ_REP>(),
                                       std::equal_to<VSYM_OBJ_REP*>(),
                                       mempool_allocator<VSYM_OBJ_REP*>(lpool)),
                           lpool);
    _cr_visited = CXX_NEW(CR_PTR_SET(3, HASHER<CODEREP>(),
                                     std::equal_to<CODEREP*>(),
                                     mempool_allocator<CODEREP*>(lpool)),
                          lpool);
    _phi_fixup = CXX_NEW(PHI_PTR_SET(3, HASHER<PHI_NODE>(),
                                     std::equal_to<PHI_NODE*>(),
                                     mempool_allocator<PHI_NODE*>(lpool)),
                         lpool);
    _phi_hor_set = CXX_NEW(HOR_PTR_SET(3, HASHER<HEAP_OBJ_REP>(),
                                       std::equal_to<HEAP_OBJ_REP*>(),
                                       mempool_allocator<HEAP_OBJ_REP*>(lpool)),
                           lpool);
    _phi_stack = CXX_NEW(PHI_STACK(mempool_allocator<PHI_NODE*>()),
                         lpool);
  }

  // destructor
  ~HOR_PHI_FIXUP() {
    // done, stack should be empty
    Is_True(_phi_stack->size() == 0, ("phi stack corrupted"));
    OPT_POOL_Pop(_lpool, VSA_DUMP_FLAG);
  }

  // set flag to indicate in fixup stage
  void Set_do_fixup() {
    // first stage done, stack should be empty
    Is_True(_phi_stack->size() == 0, ("phi stack corrupted"));
    _do_fixup = TRUE;
  }

  // check if is in fixup stage
  BOOL Is_fixup() const {
    return _do_fixup;
  }

  // clear _phi_fixup set
  void Clear_phi_fixup() {
    // finish fixing one phi, stack should be empty
    Is_True(_phi_stack->size() == 0, ("phi stack corrupted"));
    _phi_fixup->clear();
  }

public:
  // check if phi was visited before
  BOOL Visited(PHI_NODE *phi) const {
    if (_do_fixup) {
      // use another set to track phi visited in fixup stage
      if (_phi_fixup->find(phi) != _phi_fixup->end())
        return TRUE;
      _phi_fixup->insert(phi);
      return FALSE;
    }
    if (_phi_visited->find(phi) != _phi_visited->end())
      return TRUE;
    // first time to visit this phi, create ho_set to track HEAP_OBJ
    // may reach this phi
    HO_PTR_SET *ho_set = CXX_NEW(HO_PTR_SET(3, HASHER<HEAP_OBJ>(),
                                            std::equal_to<HEAP_OBJ*>(),
                                            mempool_allocator<HEAP_OBJ>(_lpool)),
                                 _lpool);
    _phi_visited->insert(std::make_pair(phi, ho_set));
    return FALSE;
  }

  // check if CODEREP visited before
  BOOL Visited(CODEREP *cr) const {
    if (_cr_visited->find(cr) != _cr_visited->end())
      return TRUE;
    _cr_visited->insert(cr);
    return FALSE;
  }

  // check if VSYM_OBJ_REP visited before
  BOOL Visited(VSYM_OBJ_REP *vor) const {
    if (_vor_visited->find(vor) != _vor_visited->end())
      return TRUE;
    _vor_visited->insert(vor);
    return FALSE;
  }

  // push phi to PHI_STACK
  void Push_phi(PHI_NODE *phi) {
    Is_True(std::find(_phi_stack->begin(), _phi_stack->end(), phi)
              == _phi_stack->end() ||
            phi == _phi_stack->front(), ("phi already in stack"));
    _phi_stack->push_back(phi);
  }

  // pop phi from PHI_STACK
  void Pop_phi(PHI_NODE *phi) {
    Is_True(!_phi_stack->empty(), ("stack empty"));
    Is_True(_phi_stack->back() == phi, ("stack top mismatch"));
    _phi_stack->pop_back();
  }

  // add HEAP_OBJ as candidate
  void Add_cand(HEAP_OBJ *ho) {
    for (PHI_STACK::iterator it = _phi_stack->begin();
         it != _phi_stack->end(); ++it) {
      PHI_NODE *phi = *it;
      PHI_HO_MAP::iterator pho = _phi_visited->find(phi);
      Is_True(pho != _phi_visited->end() && pho->first == phi,
              ("phi not visited"));
      HO_PTR_SET *ho_set = pho->second;
      Is_True(ho_set != NULL, ("ho set is null"));
      ho_set->insert(ho);
    }
  }

  // find HO_PTR_SET which contains HEAP_OBJs reach the phi
  HO_PTR_SET *Find_phi_ho_set(PHI_NODE *phi) {
    PHI_HO_MAP::iterator it = _phi_visited->find(phi);
    Is_True(it != _phi_visited->end(), ("phi not visited"));
    Is_True(it->first == phi && it->second != NULL,
            ("ho set is null"));
    return it->second;
  }

  void Add_phi_hor(HEAP_OBJ_REP *hor) {
    Is_True(hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI, ("not def by var/vor phi"));
    Is_True(hor->Phi_def() != NULL, ("phi def is null"));
    _phi_hor_set->insert(hor);
  }

  // find phi_node which defines the object (CODEREP or VSYM_OBJ_REP)
  template<typename _OBJECT>
  PHI_NODE *Find_phi(_OBJECT* obj) {
    for (PHI_HO_MAP::const_iterator it = _phi_visited->begin();
         it != _phi_visited->end(); ++it) {
      PHI_NODE *phi = it->first;
      if (phi->RESULT() == (CODEREP*)obj)
        return phi;
    }
    return NULL;
  }

  // find HEAP_OBJ_REP from CODEREP
  HEAP_OBJ_REP *Find_hor(CODEREP *cr) const {
    HEAP_OBJ_REP *hor = _hva->Cr_2_heap_obj(cr);
    if (hor)
      return hor;
    if (Visited(cr) && !_do_fixup)
      return HOR_PENDING;
    while (cr->Kind() == CK_IVAR) {
      Is_True(cr->Opr() != OPR_PARM, ("parm not allowed"));
      cr = Find_ilod_base(cr->Ilod_base());
      if (cr == NULL)
        return _hva->Vsa()->Null_hor();
      if (_cr_visited->find(cr) != _cr_visited->end()) {
        // base visited just now
        return _do_fixup ? Find_hor(cr) : HOR_PENDING;
      }
    }
    return HOR_NOT_FOUND;
  }

  // find HEAP_OBJ_REP from VSYM_OBJ_REP
  HEAP_OBJ_REP *Find_hor(VSYM_OBJ_REP *vor) const {
    HEAP_OBJ_REP *hor = vor->Hor();
    if (hor)
      return hor;
    if (_do_fixup)
      return HOR_NOT_FOUND;
    return Visited(vor) ? HOR_PENDING : HOR_NOT_FOUND;
  }

  // mark HEAP_OBJ_REP on CODEREP
  void Mark_hor(CODEREP *cr, HEAP_OBJ_REP *hor, BOOL defphi) {
    BOOL visited = Visited(cr);
    if (Is_hor_valid(hor)) {
      _hva->Enter_cr_heap_obj_map(cr, hor, visited);
      if (_do_fixup && cr->Kind() == CK_IVAR) {
        // do fixup, annotate hor to ILOAD base
        // cr probably lhs of ISTORE
        cr = cr->Istr_base() ? cr->Istr_base() : cr->Ilod_base();
        cr = Find_ilod_base(cr);
        while (cr != NULL &&
               cr->Kind() == CK_IVAR &&
               _hva->Cr_2_heap_obj(cr) == NULL) {
          _hva->Enter_cr_heap_obj_map(cr, hor, FALSE);
          cr = Find_ilod_base(cr->Ilod_base());
        }
      }
      if (defphi)
        hor->Set_attr(ROR_DEF_BY_VARPHI);
    }
  }

  // mark HEAP_OBJ_REP to VSYM_OBJ_REP
  void Mark_hor(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor, BOOL defphi) {
    if (vor->Hor() == hor)
      return;  // already set
    BOOL visited = Visited(vor);
    Is_True(visited == TRUE || vor->Hor() == NULL ||
            vor->Hor()->Heap_obj() == hor->Heap_obj(), ("hor already set"));
    if (Is_hor_valid(hor)) {
      Is_True(!_hva->Vsa()->Is_special_vor(vor), ("set hor on special vor"));
      vor->Set_hor(hor);
      if (defphi)
        hor->Set_attr(ROR_DEF_BY_VORPHI);
    }
  }

  // begin iterator for all phi visited
  phi_iterator phi_begin() const {
    return _phi_visited->begin();
  }

  // end iterator for all phi visited
  phi_iterator phi_end() const {
    return _phi_visited->end();
  }

  // begin iterator for all varhor/vorhor created
  hor_iterator hor_begin() const {
    return _phi_hor_set->begin();
  }

  // end iterator for all varhor/vorhor created
  hor_iterator hor_end() const {
    return _phi_hor_set->end();
  }

};  // HOR_PHI_FIXUP

// ============================================================================
// HEAP_OBJ_CREATION
//
// Check coderep and create HEAP_OBJ/HEAP_OBJ_REP
// ============================================================================
class HEAP_OBJ_CREATION : public CFG_VISITOR_BASE<HEAP_OBJ_CREATION> {
  typedef pair<STMTREP*, CODEREP*> ST_CR_PAIR;
  typedef vector<ST_CR_PAIR,
                 mempool_allocator<ST_CR_PAIR> > PENDING_CR_LIST;
  typedef pair<AUX_ID, HEAP_OBJ_REP*> AUX_HOR_PAIR;
  typedef hash_map<AUX_ID, HEAP_OBJ_REP*,
                   __gnu_cxx::hash<AUX_ID>,
                   std::equal_to<AUX_ID>,
                   mempool_allocator<AUX_HOR_PAIR> > AUX_HOR_MAP;

  enum KID_KIND {
    NOT_POINTER,   // the cr is not used pointer for dereference
    POINTER,       // the cr is used as pointer for dereference
  };

  enum FIX_RESULT {
    R_UNCHANGED,   // fix fail, no change in result or opnd
    R_CHANGED,     // fix succeed, changed result or opnd
    R_DONE         // done, result/opnds already have hor
  };

private:
  // TODO: create _pending_cr_list on local mempool
  PENDING_CR_LIST *_pending_cr_list;    // for un-annotated cr phi result
  AUX_HOR_MAP     *_aux_hor_map;

public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_ASM_STMT,
  };

  // constructor
  HEAP_OBJ_CREATION(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) {
    MEM_POOL *mp = _hva->Local_pool();
    // create _pending_cr_list on local pool. this will be destroyed when
    // current iteration is done
    _pending_cr_list = CXX_NEW(PENDING_CR_LIST(mempool_allocator<AUX_HOR_PAIR>(mp)),
                               mp);
    Is_True(_pending_cr_list, ("out of memory?"));
    _aux_hor_map = CXX_NEW(AUX_HOR_MAP(5,
                                       __gnu_cxx::hash<AUX_ID>(),
                                       std::equal_to<AUX_ID>(),
                                       mempool_allocator<AUX_HOR_PAIR>(mp)),
                            mp);
    Is_True(_aux_hor_map, ("out of memory?"));
  }

private:
  // process CALL, ICALL and INTRINSIC_CALL to handle the RSC objects
  void Process_call_for_rsc(STMTREP* sr);

  // process CALL, ICALL and INTRINSIC_CALL to create heap_obj
  void Process_call(STMTREP* sr);

  // process ISTORE, MSTORE
  void Process_istore(STMTREP *sr);

  // annotate heap_obj for call which frees memory
  void Annotate_free_call(STMTREP *sr, CODEREP *arg, HEAP_OBJ_REP *hor);

  // find hor on coderep or vsym_obj_rep directly
  template<typename _OBJECT>
  HEAP_OBJ_REP *Find_hor_on_object(_OBJECT *obj);

  // mark hor on coderep or vsym_obj_rep
  template<typename _OBJECT>
  void          Mark_hor_on_object(_OBJECT *obj, HEAP_OBJ_REP *hor);

  // find hor on coderep or vsym_obj_rep
  template<typename _OBJECT>
  HEAP_OBJ_REP *Find_hor_on_object(_OBJECT *obj, BOOL opnd_ud,
                                   HOR_PHI_FIXUP &fixup);

  // try create hor for phi node
  template<typename _OBJECT>
  HEAP_OBJ_REP *Find_hor_for_phi(PHI_NODE *phi, BOOL opnd_ud,
                                 HOR_PHI_FIXUP &fixup);

  // traverse the U-D of VAR cr to find out the HEAP_OBJ_REP been used
  HEAP_OBJ_REP *Find_hor_on_var(STMTREP *sr, CODEREP *cr,
                                HOR_PHI_FIXUP &fixup);

  // find an existing hor from visited phi_node
  HEAP_OBJ_REP *Find_hor_on_ilod_base(STMTREP *sr, CODEREP *cr,
                                      HOR_PHI_FIXUP &fixup);

  // traverse the U-D of IVAR cr to find out the HEAP_OBJ_REP been used
  HEAP_OBJ_REP *Find_hor_on_ivar(STMTREP *sr, CODEREP *cr,
                                 HOR_PHI_FIXUP &fixup);

  // traverse the U-D of VOR to find out the HEAP_OBJ_REP been used
  HEAP_OBJ_REP *Find_hor_on_vor(VSYM_OBJ_REP *vor, CODEREP *cr,
                                HOR_PHI_FIXUP &fixup);

  // check the generic cr and call functions above to find the HEAP_OBJ_REP
  HEAP_OBJ_REP *Find_hor_on_cr(STMTREP *sr, CODEREP *cr,
                               HOR_PHI_FIXUP &fixup);

  // propagate hor on coderep or vsym_obj_rep from lhs to rhs
  template<typename _OBJECT>
  void          Prop_hor_on_object(_OBJECT *obj, HEAP_OBJ_REP *hor,
                                   HOR_PHI_FIXUP &fixup);

  // propagate hor on coderep and it's U-D
  void          Prop_hor_on_cr(CODEREP *cr, HEAP_OBJ_REP *hor,
                               HOR_PHI_FIXUP &fixup);

  // fix up missing result/operands for single phi
  template<typename _OBJECT>
  FIX_RESULT    Fixup_hor_on_phi(PHI_NODE *phi, HOR_PHI_FIXUP &fixup);

  // fix up missing result/operands for all hor phi
  HEAP_OBJ_REP *Fixup_hor_on_cr(STMTREP *sr, CODEREP *cr, HEAP_OBJ_REP *hor_found,
                                HOR_PHI_FIXUP &fixup);

public:
  // in first round, create hor and vor phi list if preds more than 1
  void Enter_bb(BB_NODE* bb) {
    if (_hva->First_round() &&
        bb->Pred() && bb->Pred()->Multiple_bbs()) {
      Vsa()->Enter_bb_ho_philist(bb, CXX_NEW(PHI_LIST(bb), Vsa()->Mem_pool()));
      Vsa()->Enter_bb_vo_philist(bb, CXX_NEW(PHI_LIST(bb), Vsa()->Mem_pool()));
      ID_PHI_MAP *vo_phi_cache = CXX_NEW(ID_PHI_MAP(PHI_TABLE_SIZE,
                                                    __gnu_cxx::hash<IDTYPE>(),
                                                    std::equal_to<IDTYPE>(),
                                                     mempool_allocator<ID_PHI_PAIR>(_hva->Hva_pool())),
                                                    _hva->Hva_pool());

      ID_PHI_MAP *ho_phi_cache = CXX_NEW(ID_PHI_MAP(PHI_TABLE_SIZE,
                                                    __gnu_cxx::hash<IDTYPE>(),
                                                    std::equal_to<IDTYPE>(),
                                                     mempool_allocator<ID_PHI_PAIR>(_hva->Hva_pool())),
                                                    _hva->Hva_pool());
      PHI_CACHE *vo_cache = _hva->Vo_phi_cache();
      PHI_CACHE *ho_cache = _hva->Ho_phi_cache();
      (*vo_cache)[bb->Id()] = vo_phi_cache;
      (*ho_cache)[bb->Id()] = ho_phi_cache;
    }
  }

  // process stmtrep to create heap_obj
  template<OPERATOR opr, BOOL _FWD> void
  Process_sr(STMTREP* sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<HEAP_OBJ_REP *>(sr, sr->Rhs(), NOT_POINTER);
  }

  // process coderep to create heap_obj
  template<CODEKIND kind> HEAP_OBJ_REP*
  Process_cr(STMTREP* sr, CODEREP* cr, UINT flag) { return NULL; }

  // perform the heap_obj creation phase
  void Perform() {
    // walk the CFG to create heap_obj
    CFG_WALKER<HEAP_OBJ_CREATION> walker(Cfg(), this);
    walker.Perform();
  }

  // Initialize HEAP_OBJ_REP creation phase
  void Initialize();

  // finalize HEAP_OBJ_REP creation phase
  void Finalize();
};

// Process_cr<CK_LDA>: create HOR for LDA
template<> HEAP_OBJ_REP*
HEAP_OBJ_CREATION::Process_cr<CK_LDA>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_LDA, ("invalid lda cr"));
  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor)
    return hor;

  // create HO/HOR for LDA
  hor = _hva->Create_heap_obj_for_lda(cr);
  if (hor->Stmt_def() == NULL)
    hor->Set_stmt_def(sr, Dna());
  Vsa()->Enter_cr_heap_obj_map(cr, hor);
  Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(),
           (TFile, " for LDA sym%d cr%d in sr%d %s.\n",
                   cr->Lda_aux_id(), cr->Coderep_id(),
                   sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
  return hor;
}

// Process_cr<CK_VAR>: create hor for var
template<> HEAP_OBJ_REP*
HEAP_OBJ_CREATION::Process_cr<CK_VAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_VAR, ("invalid var cr"));
  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor != NULL)
    return hor;

  // no hor annotated but a hor is needed, add to pending list
  if (flag == POINTER) {
    IDTYPE param_idx;
    if (cr->Dtyp() != Pointer_Mtype) {
      hor = Vsa()->Null_hor();
    }
    else {
      Is_Trace(Tracing(),
               (TFile, "HOC[%d]: Add cr%d sr%d to pending cr list.\n",
                       _hva->Round(), cr->Coderep_id(), sr->Stmtrep_id()));
      _pending_cr_list->push_back(ST_CR_PAIR(sr, cr));
    }
  }

  // return hor if it's annotated
  return hor;
}

// Process_cr<CK_IVAR>: create hor for ivar
template<> HEAP_OBJ_REP*
HEAP_OBJ_CREATION::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    CODEREP *base = cr->Ilod_base();
    if (OPERATOR_is_call(sr->Opr()) && !Vsa()->Callee_frees_heap_memory(sr)) {
      RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
      IDTYPE    param = rna->Get_arg_with_cr(base);
      // create ho if parm is deref/mod in callee
      if (rna->Is_set_arg_flag(param, REF_ILOAD|REF_ISTORE)) {
        flag = POINTER;
      }
    }
    else if (flag == POINTER) {
      // reset flag from POINTER to NOT_POINTER for intrinsic_op param
      flag = NOT_POINTER;
    }
    Process_coderep<HEAP_OBJ_REP*>(sr, base, flag);
    return NULL;  // no HOR for parm
  }

  CODEREP* base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
  Is_True(base != NULL, ("base is null"));
  // try to create hor for base
  HEAP_OBJ_REP* base_hor = Process_coderep<HEAP_OBJ_REP*>(sr, base, POINTER);
  if (base_hor == NULL && Find_ilod_base(base) == NULL) {
    // not find base, set to null hor
    base_hor = Vsa()->Null_hor();
    Vsa()->Enter_cr_heap_obj_map(base, base_hor);
  }

  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor != NULL)
    return hor;

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(cr);
  if (vor != NULL && vor->Hor() != NULL)
    return vor->Hor();

  if (cr->Opr() == OPR_MLOAD) {
    // handle MLOAD size
    Process_coderep<HEAP_OBJ_REP*>(sr, cr->Mload_size(), NOT_POINTER);
  }

  if (flag == POINTER) {
    // need a heap_obj if flag == POINTER
    if (vor != NULL) {
      Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
      if (cr->Dtyp() != Pointer_Mtype) {
        hor = Vsa()->Null_hor();
        vor->Set_hor(hor);
      }
      else {
        // we already have VOR, add to _pending_cr_list
        Is_Trace(Tracing(),
                 (TFile, "HOC[%d]: Add cr%d sr%d to pending cr list.\n",
                         _hva->Round(), cr->Coderep_id(), sr->Stmtrep_id()));
        _pending_cr_list->push_back(ST_CR_PAIR(sr, cr));
      }
    }
    else {
      // need next round
      _hva->Set_visit_next(sr);
    }
  }

  return hor;
}

// Process_cr<CK_OP>: Create hor for CK_OP
template<> HEAP_OBJ_REP*
HEAP_OBJ_CREATION::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));
  HEAP_OBJ_REP *hor = NULL;

  // special processing of alloca
  if (cr->Opr() == OPR_ALLOCA) {
    Process_coderep<HEAP_OBJ_REP*>(sr, cr->Opnd(0), NOT_POINTER);
    hor = Vsa()->Cr_2_heap_obj(cr);
    if (hor == NULL) {
      hor = _hva->Create_heap_obj(cr, sr->Bb(), Defbb_pool());
      hor->Set_attr(ROR_DEF_BY_ALLOCA);
      hor->Set_stmt_def(sr, Dna());
      hor->Set_srcpos_node(sr, Dna(), PATHINFO_ALLOC);
      hor->Heap_obj()->Set_byte_size(cr->Opnd(0));
      Vsa()->Enter_cr_heap_obj_map(cr, hor);
      Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " for cr%d in alloca sr%d %s:\n",
                       cr->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));
    }
    return hor;
  }

  // normal processing of operators
  for (INT i = 0; i < cr->Kid_count(); ++i) {
    CODEREP* kid = cr->Opnd(i);
    UINT kid_flag = (flag == POINTER &&
                     (i == 0 ||
                      cr->Opr() == OPR_ADD ||
                      cr->Opr() == OPR_BAND ||
                      cr->Opr() == OPR_BIOR) &&
                     (kid->Kind() == CK_LDA ||
                      (kid->Kind() == CK_OP &&
                       (kid->Opr() == OPR_ADD ||
                        kid->Opr() == OPR_SUB ||
                        kid->Opr() == OPR_BAND ||
                        kid->Opr() == OPR_BIOR)) ||
                      TY_kind(kid->object_ty()) == KIND_POINTER)) ?
                        POINTER : NOT_POINTER;
    HEAP_OBJ_REP *sub = Process_coderep<HEAP_OBJ_REP*>(sr, kid, kid_flag);
    if (sub &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB ||
         cr->Opr() == OPR_BAND || cr->Opr() == OPR_BIOR) &&
        sub != hor) {
      Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), sub->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " for OPND %d cr%d of OP cr%d in sr%d %s.",
                       i, kid->Coderep_id(), cr->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      // reset kid_flag to NOT POINTER for subtrahend
      if (kid_flag == POINTER &&
          kid->Kind() == CK_OP && kid->Opr() == OPR_SUB && i != 0)
        kid_flag = NOT_POINTER;
      if (hor == NULL || kid_flag == POINTER) {
        if (hor) {
          Is_Trace(Tracing(), (TFile, " REPLACE OLD hor: "));
          Is_Trace_cmd(Tracing(), hor->Print(TFile));
        }
        hor = sub;
      }
      Is_Trace(Tracing(), (TFile, "\n"));
    }
  }

  return hor;
}

// Process_sr<OPR_STID, TRUE>: forward process STID
template<> void
HEAP_OBJ_CREATION::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  CODEREP* lhs = sr->Lhs();
  CODEREP* rhs = sr->Rhs();
  if (rhs->Kind() == CK_CONST && rhs->Const_val() == 0) {
    if (TY_kind(lhs->object_ty()) == KIND_POINTER) {
      //hor = Vsa()->Get_null_hor(sr->Bb(), Defbb_pool());
      HEAP_OBJ_REP *hor = Vsa()->Null_hor();
      Is_True(hor != NULL, ("null hor is NULL"));
      Vsa()->Enter_cr_heap_obj_map(lhs, hor);
      Is_Trace(Tracing(), (TFile, "HOC[%d]: Create Null hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " STID lhs sym%dv%d cr%d:\n",
                                  lhs->Aux_id(), lhs->Version(),
                                  lhs->Coderep_id()));
      Is_Trace_cmd(Tracing(), sr->Print(TFile));
    }
    return;
  }

  UINT flag = (TY_kind(lhs->object_ty()) == KIND_POINTER &&
               Vsa()->Cr_2_heap_obj(lhs) != NULL) ? POINTER : NOT_POINTER;
  HEAP_OBJ_REP *hor = Process_coderep<HEAP_OBJ_REP*>(sr, rhs, flag);
  if (flag == POINTER && hor == NULL && Find_ilod_base(rhs) == NULL) {
    // no base cr, set to null hor
    hor = Vsa()->Null_hor();
  }

  if (hor == NULL)
    return;

  if (Vsa()->Cr_2_heap_obj(rhs) == NULL)
    Vsa()->Enter_cr_heap_obj_map(rhs, hor);
  Vsa()->Enter_cr_heap_obj_map(lhs, hor);
  Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " STID lhs sym%dv%d cr%d:\n",
                              lhs->Aux_id(), lhs->Version(),
                              lhs->Coderep_id()));

  if (Vsa()->Is_special_hor(hor))
    return;

  if (_aux_hor_map->find(lhs->Aux_id()) == _aux_hor_map->end())
    (*_aux_hor_map)[lhs->Aux_id()] = hor;

  if (hor->Attr() == ROR_DEF_BY_PHI ||
      hor->Attr() == ROR_DEF_BY_VARPHI ||
      hor->Attr() == ROR_DEF_BY_VORPHI)
    return;

  // reset rhs hor's sym id if it's not set
  HEAP_OBJ* ho = hor->Heap_obj();
  CODEREP *cr = ho->Ho_cr();
  if (cr && cr->Kind() == CK_OP) { // ALLOCA
    Is_True(cr->Opr() == OPR_ALLOCA,
            ("TODO: handle %s", OPERATOR_name(cr->Opr()) + 4));
    ho->Set_ho_cr(lhs);
  }
}

// Process_sr<OPR_ISTORE, TRUE>: forward process ISTORE
template<> void
HEAP_OBJ_CREATION::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore(sr);
}

// Process_sr<OPR_MSTORE, TRUE>: forward process MSTORE
template<> void
HEAP_OBJ_CREATION::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  // process mstore size
  Process_coderep<HEAP_OBJ_REP*>(sr, sr->Lhs()->Mstore_size(), NOT_POINTER);
  // process istore
  Process_istore(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward process INTRINSIC_CALL
template<> void
HEAP_OBJ_CREATION::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn_call sr"));
  Process_call(sr);
}

// Process_call_for_rsc: process call to handle RSC objects
void
HEAP_OBJ_CREATION::Process_call_for_rsc(STMTREP* sr)
{
  RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
  Is_True_Ret(rna != NULL, ("not find rna node"));

  if (rna->Is_flag_set(RBC_SE_MODEL)) {
    Vsa()->Ipsa()->Rbc()->Eval__mvsa_model(Dna(), rna, _hva->Defbb_pool());
  }
}

// Process_call: check if call malloc or free memory and annotate to cr
void
HEAP_OBJ_CREATION::Process_call(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL ||
          sr->Opr() == OPR_INTRINSIC_CALL,
          ("not a call"));

  // process resource (RSC) objects only once in first round
  if (_hva->First_round()) {
    Process_call_for_rsc(sr);
  }

  // process rhs
  Process_coderep<HEAP_OBJ_REP*>(sr, sr->Rhs(), NOT_POINTER);

  // TODO: need more precise modeling on malloc/free function, for example:
  // struct *my_malloc() {
  //   struct S *s = malloc();
  //   s->p = malloc();
  //   return s;
  // }
  // void my_free(struct S *s) {
  //   free(s->p);
  //   free(s);
  // }
  // The malloc/free on parameter/return value field should be modeled

  HEAP_OBJ_REP* hor;
  // process call frees memory
  CODEREP *arg = Vsa()->Callee_frees_heap_memory(sr);
  CODEREP *base;
  if (arg != NULL) {
    CODEREP *base = (arg->Kind() == CK_OP) ? Find_ilod_base(arg)
                                         : arg;
    if (base == NULL) {
      // free non-pointer, report D UDR here
      SRCPOS_HANDLE sp_h(arg, sr, Dna(), _hva->Local_pool(), Vsa());
      const char *name = sp_h.Find_cr_stname(arg, sr, Dna());
      sp_h.Set_key_srcpos(Dna(), sr->Bb(), sr->Linenum(), name);
      sp_h.Set_msgid("UDR.1");
      Vsa()->Report_vsa_error(arg, (char*)NULL, UDR, IC_DEFINITELY, &sp_h);
      return;
    }

    Is_True(base != NULL &&
            base->Kind() == CK_CONST ||
            base->Kind() == CK_LDA ||
            base->Kind() == CK_VAR ||
            base->Kind() == CK_IVAR,
            ("not VAR or IVAR"));
    hor = base->Kind() == CK_CONST ? Vsa()->Null_hor()
                                   : Vsa()->Cr_2_heap_obj(base);
    if (hor == NULL) {
      Is_True(base->Kind() == CK_VAR || base->Kind() == CK_IVAR,
              ("not VAR or IVAR"));
      Is_Trace(Tracing(),
               (TFile, "HOC[%d]: Add cr%d sr%d(free) to pending cr list.\n",
                       _hva->Round(), base->Coderep_id(), sr->Stmtrep_id()));
      _pending_cr_list->push_back(ST_CR_PAIR(sr, base));
    }
    else {
      Annotate_free_call(sr, arg, hor);
    }
  }

  // check if rhs already annotated with a heap_obj
  CODEREP* rhs = sr->Rhs();
  hor = Vsa()->Cr_2_heap_obj(rhs);
  if (hor != NULL) {
    Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " for CALL rhs cr%d in sr%d %s.\n",
                     rhs->Coderep_id(), sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
    return;
  }

  // process call mallocs memory
  if (Vsa()->Callee_returns_new_heap_memory(sr)) {
    // get return cr
    CODEREP* ret = Comp_unit()->Find_return_value(sr);
    HEAP_OBJ_REP* hor = _hva->Create_heap_obj(ret, sr->Bb(), Defbb_pool());
    hor->Set_attr(ROR_DEF_BY_ALLOC);
    hor->Set_stmt_def(sr, Dna());
    hor->Set_srcpos_node(sr, Dna(), PATHINFO_ALLOC);
    Vsa()->Enter_cr_heap_obj_map(rhs, hor);
    // get size
    CODEREP* size = Vsa()->Vsa_malloc_size(sr, ret, Comp_unit());
    if (size != NULL)
      hor->Heap_obj()->Set_byte_size(size);
    if (ret)
      Vsa()->Enter_cr_heap_obj_map(ret, hor);

    Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " for cr%d in malloc sr%d %s:\n",
                     rhs->Coderep_id(), sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
  }
}

// Process_istore: generate for istore, mstore
void
HEAP_OBJ_CREATION::Process_istore(STMTREP *sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalid istore"));

  // process istore base
  CODEREP *base = sr->Lhs()->Istr_base();
  HEAP_OBJ_REP *base_hor = Process_coderep<HEAP_OBJ_REP*>(sr, base, POINTER);
  if (base_hor == NULL && Find_ilod_base(base) == NULL) {
    // not find base, set to null hor
    base_hor = Vsa()->Null_hor();
    Vsa()->Enter_cr_heap_obj_map(base, base_hor);
  }

  // process istore rhs
  CODEREP *rhs = sr->Rhs();
  HEAP_OBJ_REP *rhs_hor = Process_coderep<HEAP_OBJ_REP*>(sr, rhs, NOT_POINTER);

  if (rhs_hor == NULL)
    return;

  // check HOR on lhs
  CODEREP *lhs = sr->Lhs();
  HEAP_OBJ_REP *lhs_hor = Vsa()->Cr_2_heap_obj(lhs);
  if (lhs_hor != NULL) {
    Is_True(lhs_hor == rhs_hor ||
            Vsa()->Find_stmt_cur_hor(sr, rhs_hor->Heap_obj()),
            ("lhs and rhs hor mismatch"));
    Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), lhs_hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " for ISTORE lhs cr%d in sr%d %s.\n",
                     lhs->Coderep_id(), sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
    return;
  }

  if (Vsa()->Cr_2_heap_obj(rhs) == NULL)
    Vsa()->Enter_cr_heap_obj_map(rhs, rhs_hor);
  Vsa()->Enter_cr_heap_obj_map(lhs, rhs_hor);
  VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(lhs);
  if (vor != NULL && !Vsa()->Is_special_vor(vor))
    vor->Set_hor(rhs_hor);

  Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), rhs_hor->Print(TFile));
  Is_Trace(Tracing(),
           (TFile, " for ISTORE rhs cr%d in sr%d %s.\n",
                   lhs->Coderep_id(), sr->Stmtrep_id(),
                   OPERATOR_name(sr->Opr()) + 4));
  // TODO: reset hor;s sym id to vor id?
}


// Process_sr<OPR_CALL, TRUE>: forward process CALL
template<> void
HEAP_OBJ_CREATION::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call(sr);
}

// Annotate_free_call: generate annotations on free
void
HEAP_OBJ_CREATION::Annotate_free_call(STMTREP *sr, CODEREP *arg, HEAP_OBJ_REP *hor) {
  Is_True(Vsa()->Callee_frees_heap_memory(sr) == arg,
          ("free arg mismatch"));
  Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " for cr%d in free sr%d.\n",
                      arg->Coderep_id(), sr->Stmtrep_id()));

  if (hor == Vsa()->Null_hor()) {
    // report UDR in heap checker
    return;
  }
  // create chi list and add chi to chi list
  HEAP_OBJ_REP *res;
  CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, hor->Heap_obj());
  if (chi != NULL) {
    CHOR *chor = (CHOR*)chi->RESULT();
    res = chor->first;
    Is_True(res->Heap_obj() == hor->Heap_obj(),
            ("heap obj mismatch"));
    Is_True((res->Attr() == ROR_DEF_BY_CHI ||
             res->Attr() == ROR_DEF_BY_FREE) &&
             res->Stmt_def() == sr,
            ("def sr mismatch"));
    if (chor->second != arg) {
      chor->second = arg;
      chor = (CHOR*)chi->OPND();
      chor->second = arg;
    }
    if (res->Attr() != ROR_DEF_BY_FREE)
      res->Set_attr(ROR_DEF_BY_FREE);
  }
  else {
    Vsa()->Append_stmt_hor_mu(sr, hor, arg);
    res = Vsa()->Append_stmt_hor_chi(sr, hor, arg, ROR_DEF_BY_FREE, Defbb_pool());
  }
  if (!Vsa()->Callee_returns_new_heap_memory(sr))
    Vsa()->Enter_cr_heap_obj_map(sr->Rhs(), res);
  _hva->Update_heap_obj(hor, sr->Bb(), Defbb_pool());
}

// find hor on coderep
template<> inline HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_object(CODEREP *cr)
{
  return Vsa()->Cr_2_heap_obj(cr);
}

// find hor on vsym_obj_rep
template<> inline HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_object(VSYM_OBJ_REP *vor)
{
  return vor->Hor();
}

// mark hor on coderep
template<> inline void
HEAP_OBJ_CREATION::Mark_hor_on_object(CODEREP *cr, HEAP_OBJ_REP *hor)
{
  Vsa()->Enter_cr_heap_obj_map(cr, hor);
}

// mark hor on vsym_obj_rep
template<> inline void
HEAP_OBJ_CREATION::Mark_hor_on_object(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor)
{
  Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
  vor->Set_hor(hor);
}

// find hor on coderep
template<> inline HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_object(CODEREP *cr, BOOL opnd_ud,
                                      HOR_PHI_FIXUP &fixup)
{
  return opnd_ud ? Find_hor_on_var(NULL, cr, fixup)
                 : fixup.Find_hor(cr);
}

// find hor on vsym_obj_rep
template<> inline HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_object(VSYM_OBJ_REP *vor, BOOL opnd_ud,
                                      HOR_PHI_FIXUP &fixup)
{
  return opnd_ud ? Find_hor_on_vor(vor, NULL, fixup)
                 : fixup.Find_hor(vor);
}

// Create_hor_for_var_phi: find hor for phi node
template<typename _OBJECT> HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_for_phi(PHI_NODE *phi, BOOL opnd_ud,
                                    HOR_PHI_FIXUP &fixup)
{
  Is_True(phi && phi->Live(), ("phi is not live"));
  vector<HEAP_OBJ_REP*> opnd_hor;
  opnd_hor.reserve(phi->Size());
  HEAP_OBJ_REP *res_hor = NULL;
  BB_NODE *bb = phi->Bb();
  _OBJECT *res_cr = (_OBJECT *)phi->RESULT();
  BOOL same_ho = TRUE;
  INT  null_hor_cnt = 0;

  // make sure HO_PTR_SET is created for this phi
  HO_PTR_SET *ho_cand = fixup.Find_phi_ho_set(phi);
  if (opnd_ud)
    fixup.Push_phi(phi);

  PHI_OPND_ITER phi_opnd_iter(phi);
  CODEREP *opnd_cr;
  FOR_ALL_ELEM(opnd_cr, phi_opnd_iter, Init()) {
    _OBJECT *opnd = (_OBJECT *)opnd_cr;
    HEAP_OBJ_REP *hor = Find_hor_on_object(opnd, opnd_ud, fixup);
    opnd_hor.push_back(hor == HOR_PENDING ? NULL : hor);
    // not found the hor
    if (hor == HOR_NOT_FOUND) {
      ++ null_hor_cnt;
      continue;
    }
    // hor depends on cr/vor visited this time
    if (hor == HOR_PENDING) {
      continue;
    }

    fixup.Add_cand(hor->Heap_obj());
    Is_Trace(Tracing(), (TFile, "HOC[%d]: find hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " for "));
    Is_Trace_cmd(Tracing(), opnd->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in phi in BB%d.\n", bb->Id()));

    if (same_ho) {
      if (res_hor == NULL)
        res_hor = hor;
      else if (res_hor->Heap_obj() != hor->Heap_obj())
        same_ho = FALSE;
    }
  }
  Is_True(opnd_hor.size() == phi->Size(), ("opnd missing?"));

  if (opnd_ud)
    fixup.Pop_phi(phi);

  // return NULL if operand' hor is NULL in first stage
  if (null_hor_cnt && !fixup.Is_fixup())
    return HOR_NOT_FOUND;

  // all operand is in pending state
  if (res_hor == NULL)
    return HOR_PENDING;

  // return res_hor is all opnds have the same heap_obj
  if (same_ho && ho_cand->size() <= 1) {
    for (INT i = 0; i < phi->Size(); ++i) {
      _OBJECT *opnd_cr = (_OBJECT *)phi->OPND(i);
      fixup.Mark_hor(opnd_cr, res_hor, FALSE);
    }
    res_hor->Set_injured();  // phi result hor used in phi operand
    fixup.Mark_hor(res_cr, res_hor, FALSE);
    Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), res_hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " for "));
    Is_Trace_cmd(Tracing(), res_cr->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in phi in BB%d.\n", bb->Id()));
    return res_hor;
  }

  // create a new ho for phi result with special settings:
  // phi res is a new hor with different hor from its operands
  // ulist is created in renaming phase
  res_hor = _hva->Create_heap_obj(NULL, bb, Defbb_pool());
  // create a chi on entry for this new hor
  STMTREP *entry_stmt = _vsa->Get_entry_chi_stmt();
  CODEREP *parm_cr = _hva->Find_rsc_cr(entry_stmt, phi->Aux_id());
  if (parm_cr) {
    HEAP_OBJ_REP *chi_opnd = res_hor->Heap_obj()->Entry_chi();
    Is_True(chi_opnd->Attr() == ROR_DEF_BY_CHI && chi_opnd->Stmt_def() == NULL,
            ("wrong entry chi attr or stmt def"));
    HEAP_OBJ_REP *chi_res = _hva->Create_heap_obj(chi_opnd->Heap_obj());
    chi_res->Set_attr(ROR_DEF_BY_CHI);
    chi_res->Set_stmt_def(entry_stmt, Dna());
    Is_True(Vsa()->Find_stmt_hor_chi(entry_stmt, chi_opnd->Heap_obj()) == NULL,
            ("hor chi already added"));
    Vsa()->Append_stmt_hor_chi(entry_stmt, chi_res, chi_opnd, parm_cr);
  }

  PHI_LIST *phi_list = Vsa()->Bb_ho_philist(bb);
  Is_True(phi_list, ("phi list is null"));
  PHI_NODE *hor_phi = _hva->New_phi_node(_hva->Ho_phi_cache(), phi_list, res_hor->Heap_obj()->Id(),
                                         Vsa()->Mem_pool(), bb, TRUE);
  hor_phi->Set_live();
  hor_phi->Set_result((CODEREP *)res_hor);
  VSA_PHI_NODE(hor_phi).Set_res_is_hor();
  VSA_PHI_NODE(hor_phi).Set_opnd_mismatch();
  for (INT i = 0; i < phi->Size(); ++i) {
    hor_phi->Set_opnd(i, (CODEREP *)opnd_hor[i]);
  }
  fixup.Mark_hor(res_cr, res_hor, TRUE);
  res_hor->Set_phi_def(hor_phi);
  res_hor->Heap_obj()->Set_base_phi(phi);
  fixup.Add_phi_hor(res_hor);
  Is_Trace(Tracing(), (TFile, "HOC[%d]: Create phihor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_hor_phi(hor_phi, TFile));
  Is_Trace(Tracing(), (TFile, " for "));
  Is_Trace_cmd(Tracing(), res_cr->Print(TFile));
  Is_Trace(Tracing(), (TFile, " in phi in BB%d.\n", bb->Id()));
  return res_hor;
}


// Find_hor_on_var: traverse the U-D of VAR cr to find heap_obj
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_var(STMTREP *sr, CODEREP *cr,
                                   HOR_PHI_FIXUP &fixup) {
  Is_True(cr->Kind() == CK_VAR, ("not var"));

  HEAP_OBJ_REP *hor;
  // check if cr already annotated with a heap_obj
  hor = fixup.Find_hor(cr);
  if (hor)
    return hor;

  // handle zero version or volatile cr
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_var_volatile()) {
    Is_Trace(Tracing(), (TFile, "HOC[%d]: Find_hor_on_var cr%d sr%d %s is zero or volatile.\n",
                         _hva->Round(), cr->Coderep_id(), sr ? sr->Stmtrep_id() : -1,
                         sr ? OPERATOR_name(sr->Opr()) + 4 : "null"));
    // This should not used with zero_versioning is off
    AUX_ID aux = cr->Aux_id();
    AUX_HOR_MAP::iterator it = _aux_hor_map->find(aux);
    if (it != _aux_hor_map->end()) {
      hor = it->second;
    }
    else {
      STMTREP *entry = _vsa->Get_entry_chi_stmt();
      CODEREP *chi_res = _hva->Find_rsc_cr(entry, aux);
      if (chi_res) {
        // used at entry, create a new ho/hor
        hor = _hva->Create_heap_obj(chi_res, entry->Bb(), Defbb_pool());
        hor->Set_attr(ROR_DEF_BY_CHI);
        Is_True(hor->Heap_obj()->Sym_id() == aux, ("aux id mismatch"));
        (*_aux_hor_map)[aux] = hor;
        hor->Set_stmt_def(entry, Dna());
        Is_True(Vsa()->Find_stmt_hor_chi(entry, hor->Heap_obj()) == NULL,
                ("hor chi already added"));
        Vsa()->Append_stmt_hor_chi(entry, hor, hor->Heap_obj()->Entry_chi(), chi_res);
      }
      else {
        // not used at entry
        hor = Vsa()->Null_hor();
      }
    }

    fixup.Mark_hor(cr, hor, FALSE);
    return hor;
  }

  // handle define by phi
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    BB_NODE  *bb  = phi->Bb();
    BOOL visited = fixup.Visited(phi);
    return Find_hor_for_phi<CODEREP>(phi, !visited, fixup);
  }

  // handle def by chi
  if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *def = cr->Get_defstmt();
    Is_True(def != NULL, ("no def for chi cr"));

    if (def->Opr() == OPR_OPT_CHI) {
      // check if cr is parameter or global variable
      if (Dna()->Is_param(cr) == INVALID_VAR_IDX &&
          !Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_global()) {
        hor = Vsa()->Null_hor();
        fixup.Mark_hor(cr, hor, FALSE);
        return hor;
      }
    }

    if (def->Opr() == OPR_OPT_CHI) {
      hor = _hva->Create_heap_obj_for_chi(def, cr, cr, Defbb_pool());
      Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " for sym%d cr%d in sr%d %s.\n",
                       cr->Aux_id(), cr->Coderep_id(),
                       sr ? sr->Stmtrep_id() : -1,
                       sr ? OPERATOR_name(sr->Opr()) + 4 : "null"));
    }
    else {
      CHI_NODE* chi = cr->Defchi();
      Is_True(chi && chi->Live() &&
              chi->RESULT() == cr, ("invalid chi def"));
      hor = Find_hor_on_cr(def, chi->OPND(), fixup);
      if (OPERATOR_is_call(def->Opr())) {
        if (Is_hor_valid(hor) &&
            !Vsa()->Is_special_hor(hor)) {
          if (Vsa()->Find_stmt_hor_chi(def, hor->Heap_obj()) == NULL) {
            // call will change hor, create a hor chi on the call
            // TODO: check RNA flag not to create hor mu/chi if callee
            // doesn't change the hor
            // TODO: return new hor of chi RESULT? it seems this is taken by
            // renaming phase
            Vsa()->Append_stmt_hor_mu(def, hor, chi->OPND());
            Vsa()->Append_stmt_hor_chi(def, hor, cr, ROR_DEF_BY_CHI, Defbb_pool());
          }
        }
        else {
          // always create heap_obj for call
          hor = _hva->Create_heap_obj_for_chi(def, cr, cr, Defbb_pool());
        }
      }
      Is_Trace(Tracing(), (TFile, "HOC[%d]: follow chi U-D", _hva->Round()));
      Is_Trace(Tracing(),
               (TFile, " for sym%d cr%d=chi(cr%d) in sr%d %s.\n",
                       cr->Aux_id(),
                       cr->Coderep_id(), chi->OPND()->Coderep_id(),
                       sr ? sr->Stmtrep_id() : -1,
                       sr ? OPERATOR_name(sr->Opr()) + 4 : "null"));
    }
    fixup.Mark_hor(cr, hor, FALSE);
    return hor;
  }

  // handle def by stmt
  STMTREP *def = cr->Get_defstmt();
  Is_True(def->Lhs() == cr, ("def stmt lhs mismatch"));
  CODEREP *rhs = def->Rhs();
  hor = Vsa()->Cr_2_heap_obj(rhs);
  if (hor == NULL) {
    if (rhs->Kind() == CK_CONST)
      hor = Vsa()->Null_hor();
    else {
      hor = Find_hor_on_cr(def, rhs, fixup);
      fixup.Mark_hor(rhs, hor, FALSE);
    }
  }
  // annotate hor to cr
  fixup.Mark_hor(cr, hor, FALSE);

  return hor;
}

// find an existing hor from visited phi_node
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_ilod_base(STMTREP *sr, CODEREP *cr,
                                         HOR_PHI_FIXUP &fixup) {
  if (cr->Kind() == CK_LDA) {
    return Process_cr<CK_LDA>(sr, cr, 0);
  }

  Is_True(cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR, ("bad cr kind"));
  BOOL is_var = (cr->Kind() == CK_VAR);
  HEAP_OBJ_REP *hor;
  if (is_var) {
    hor = Find_hor_on_var(sr, cr, fixup);
    Is_True(!Is_hor_valid(hor) ||
            !fixup.Is_fixup() ||
            hor == Vsa()->Cr_2_heap_obj(cr), ("cr not annotated"));
  }
  else {
    hor = Find_hor_on_ivar(sr, cr, fixup);
    VSYM_OBJ_REP *vor;
    Is_True(!Is_hor_valid(hor) ||
            !fixup.Is_fixup() ||
            hor == Vsa()->Cr_2_heap_obj(cr) ||
            ((vor = Vsa()->Cr_2_vor(cr)) != NULL &&
             hor == vor->Hor()), ("cr not annotated"));
  }

  if (fixup.Is_fixup() && Is_hor_valid(hor))
    hor->Set_injured();  // ilod_base hor used in ilod cr
  return hor;
}

// Find_hor_on_ivar: check IVAR cr to find heap_obj
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_ivar(STMTREP *sr, CODEREP *cr,
                                    HOR_PHI_FIXUP &fixup)
{
  Is_True(cr->Kind() == CK_IVAR, ("not ivar cr"));

  // check if cr already annotated with hor
  HEAP_OBJ_REP *hor = fixup.Find_hor(cr);
  if (hor != NULL)
    return hor;

  // if no vor found, do nothing because no U-D for this IVAR
  VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(cr);
  if (vor == NULL) {
    CODEREP *base = Find_ilod_base(cr->Ilod_base());
    if (base == NULL)
      hor = Vsa()->Null_hor();
    else
      hor = Find_hor_on_ilod_base(sr, base, fixup);
    if (fixup.Is_fixup() && Is_hor_valid(hor))
      Vsa()->Enter_cr_heap_obj_map(cr, hor);
    return hor;
  }

  // find hor on vsym_obj
  return Find_hor_on_vor(vor, cr, fixup);
}

// Find_hor_on_vor: check U-D of vsym_obj to find heap_obj
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_vor(VSYM_OBJ_REP *vor, CODEREP *cr,
                                   HOR_PHI_FIXUP &fixup) {
  Is_True(vor != NULL, ("invalid vor"));

  // check if vor already annotated with a heap_obj
  HEAP_OBJ_REP *hor = fixup.Find_hor(vor);
  if (hor)
    return hor;

  // follow the U-D of the vsym_obj to find the heap_obj
  switch (vor->Attr()) {
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
    {
      PHI_NODE *phi = vor->Phi_def();
      BB_NODE  *bb = phi->Bb();
      BOOL visited = fixup.Visited(phi);
      return Find_hor_for_phi<VSYM_OBJ_REP>(phi, !visited, fixup);
#if 0
      HEAP_OBJ_REP *prev_hor = NULL;
      vector<HEAP_OBJ_REP*> opnd_hor;
      opnd_hor.reserve(phi->Size());
      BOOL has_null_hor = FALSE;
      for (INT i = 0; i < phi->Size(); ++i) {
        VSYM_OBJ_REP *opnd = (VSYM_OBJ_REP *)phi->OPND(i);
        Is_True(opnd != NULL, ("invalid vor phi opnd"));
        hor = Find_hor_on_vor(opnd, cr, fixup);
        if (hor == NULL) {
          Is_Trace(Tracing(), (TFile, "HOC[%d]: not find hor on vor", _hva->Round()));
          Is_Trace_cmd(Tracing(), opnd->Print(TFile));
          Is_Trace(Tracing(), (TFile, " phi opnd %d in BB%d.\n", i, bb->Id()));
          continue;
        }
        if (hor == Vsa()->Null_hor()) { // should Null hor ignored here?
          Is_Trace(Tracing(), (TFile, "HOC[%d]: find null hor on vor", _hva->Round()));
          Is_Trace_cmd(Tracing(), opnd->Print(TFile));
          Is_Trace(Tracing(), (TFile, " phi opnd %d in BB%d.\n", i, bb->Id()));
          if (!has_null_hor)
            has_null_hor = TRUE;
          continue;
        }
        if (hor != prev_hor)
          opnd_hor.push_back(hor);
        if (prev_hor == NULL)
          prev_hor = hor;
      }
      // return if no valid heap obj found
      if (opnd_hor.size() == 0)
        return has_null_hor ? Vsa()->Null_hor() : NULL;

      hor = opnd_hor.front();
      if (opnd_hor.size() == 1 || !VSA_New_HVA_Unification) {
        // if unification is ON, return the first hor
        vor->Set_hor(hor);
        Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " for vor "));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " for phi in BB%d.\n", bb->Id()));
        return hor;
      }

      // check if these hors are same ho or not
      BOOL same_ho = TRUE;
      vector<HEAP_OBJ_REP*>::iterator it = opnd_hor.begin();
      HEAP_OBJ *ho = (*it)->Heap_obj();
      ++ it;
      for (; it != opnd_hor.end(); ++it) {
        if ((*it)->Heap_obj() != ho) {
          same_ho = FALSE;
          break;
        }
      }

      if (same_ho) {
        // need to create a heap_obj phi here?
        vor->Set_hor(hor);
        Is_Trace(Tracing(), (TFile, "HOC[%d]: Find hor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " for vor "));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " for phi in BB%d.\n", bb->Id()));
        return hor;
      }

      // create a new ho for phi result
      hor = _hva->Create_heap_obj(bb, Defbb_pool());
      // TODO: a new attr for phi result?
      hor->Set_attr(ROR_DEF_BY_PHI);
      hor->Set_phi_def(phi);
      vor->Set_hor(hor);
      Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " for vo%dv%d phi in BB%d.\n",
                       vor->Vsym_obj()->Id(), vor->Version(),
                       bb->Id()));
      return hor;
#endif
    }

  case ROR_DEF_BY_CHI:
    {
      if (vor->Is_entry_chi()) { // no def for this vor, return null hor
        hor = Vsa()->Null_hor();
        fixup.Mark_hor(vor, hor, FALSE);
        return hor;
      }
      STMTREP *def = vor->Stmt_def();
      Is_True(def != NULL, ("vor istore def is NULL"));
      HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
      if (Vsa()->Is_special_hor(base_hor)) {
        fixup.Mark_hor(vor, base_hor, FALSE);
        return base_hor;
      }
      // TODO: refine vor ROR_DEF_BY_CHI
      if (base_hor->Attr() == ROR_DEF_BY_LDA ||
          base_hor->Attr() == ROR_DEF_BY_ALLOCA) {
        // base hor is LDA but there is no vor is defined by CHI which means no
        // store to the variale, set to Null_hor
        hor = Vsa()->Null_hor();
      }
      else if (def->Opr() == OPR_OPT_CHI ||     // p is parm
               def->Opr() == OPR_ISTORE ||      // *p = blah
               OPERATOR_is_call(def->Opr())) {  // p is retval
        CODEREP *def_cr;
        if (base_hor->Attr() == ROR_DEF_BY_ALLOC &&
            Vsa()->Callee_returns_new_heap_memory(def))
          def_cr = Comp_unit()->Find_return_value(def);
        else
          def_cr = Vsa()->Find_vor_chi_cr(def, vor);
        Is_True(def_cr != NULL, ("not find def_cr"));

        // check if def_cr is the call itself, which means the vo is
        // defined by the malloc call. set its field hor to null hor
        if (def_cr->Kind() == CK_OP && def_cr->Opr() == OPR_CALL)
          hor = Vsa()->Null_hor();
        else {
          hor = _hva->Create_heap_obj_for_chi(def, def_cr, cr, Defbb_pool());
          hor->Set_owner_vo(vor->Vsym_obj());
        }
        fixup.Mark_hor(vor, hor, FALSE);
        Is_Trace(Tracing(), (TFile, "HOC[%d]: Create hor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " for vo%dv%d in sr%d %s.\n",
                         vor->Vsym_obj()->Id(), vor->Version(),
                         def->Stmtrep_id(), OPERATOR_name(def->Opr()) + 4));
      }
      else if (def->Opr() == OPR_MSTORE) {
        if (def->Rhs()->Kind() == CK_CONST) {  // memset(p, c, sz)
          hor = Vsa()->Null_hor();
        }
        else { // memcpy(p, q, sz)
          VSA_ADDRESS_INFO rhs_info;
          Comp_unit()->Analyze_pointer_info(NULL, def->Rhs()->Ilod_base(),
                                            &rhs_info, FALSE);
          VSA_ADDRESS_INFO lhs_info;
          Comp_unit()->Analyze_pointer_info(NULL, def->Lhs()->Istr_base(),
                                            &lhs_info, FALSE);
          rhs_info.Subtract(lhs_info);
          if (!rhs_info.Const_ofst() || rhs_info.Base() == NULL) {
            // TODO: not constant offset, no idea to map to src's vor
            hor = Vsa()->Null_hor();
          }
          else {
            VSYM_FLD_REP *lhs_vfr = vor->Vsym_obj()->Fld_rep_ptr();
            VSYM_FLD_REP rhs_vfr(lhs_vfr->Kind(), 0, lhs_vfr->Ofst() + rhs_info.Fix_ofst());
            VSYM_OBJ_REP *rhs_vor = Vsa()->Find_vor_mu_vor(def, rhs_info.Base(), &rhs_vfr);
            if (rhs_vor) {
              // find rhs vor and find hor from rhs vor
              hor = Find_hor_on_vor(rhs_vor, rhs_info.Base(), fixup);
            }
            else {
              hor = Find_hor_on_cr(def, rhs_info.Base(), fixup);
              if (hor == NULL) {
                hor = Vsa()->Null_hor();
              }
              else {
                // TODO: create a vor on src's hor and connect with input vor???
                hor = _hva->Create_heap_obj_for_chi(def, rhs_info.Base(), cr, Defbb_pool());
              }
            }
          }
        }
      }
      else {
        Is_Trace(Tracing(), (TFile, "HOC[%d]: No idea for vor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " in sr%d %s.\n",
                         def->Stmtrep_id(), OPERATOR_name(def->Opr()) + 4));
      }
      fixup.Mark_hor(vor, hor, FALSE);
      return hor;
    }

  case ROR_DEF_BY_COPY:   // arr[i] = p;
  case ROR_DEF_BY_ISTORE: // p->fld = p;
    {
      STMTREP *def = vor->Stmt_def();
      Is_True(def != NULL, ("vor istore def is NULL"));
      CODEREP *rhs = def->Rhs();
      hor = Vsa()->Cr_2_heap_obj(rhs);
      if (hor == NULL) {
        if (rhs->Kind() == CK_CONST)
          hor = Vsa()->Null_hor();
        else {
          hor = Vsa()->Cr_2_heap_obj(def->Lhs());
          if (hor == NULL)
            hor = Find_hor_on_cr(def, rhs, fixup);
          // TODO: propagate lhs hor to rhs?
          // else
          // Prop_hor_on_cr(rhs, hor, fixup);
          fixup.Mark_hor(rhs, hor, FALSE);
        }
      }
      // annotate hor to cr
      if (hor) {
        CODEREP *lhs = def->Lhs();
        fixup.Mark_hor(lhs, hor, FALSE);
        fixup.Mark_hor(vor, hor, FALSE);
      }

      return hor;
    }

  case ROR_DEF_BY_NONE:
  default:
    Is_True(FALSE, ("vor def by none?"));
    break;
  }
  return hor;
}

// Find_hor_on_cr: check generic cr to find heap_obj
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Find_hor_on_cr(STMTREP *sr, CODEREP *cr,
                                  HOR_PHI_FIXUP &fixup) {
  switch (cr->Kind()) {
  case CK_LDA:
    return Vsa()->Cr_2_heap_obj(cr);

  case CK_CONST:
    Is_True(FALSE, ("hor for LDA and CONST should be created before"));
    return NULL;

  case CK_RCONST:
    return NULL;

  case CK_VAR:
    return Find_hor_on_var(sr, cr, fixup);

  case CK_IVAR:
    return Find_hor_on_ivar(sr, cr, fixup);

  case CK_OP:
    cr = Find_ilod_base(cr);
    return cr ? Find_hor_on_cr(sr, cr, fixup) : Vsa()->Null_hor();

  default:
    Is_True(FALSE, ("unknown cr kind"));
    return NULL;
  }
}

// propagate hor on coderep from lhs to rhs
template<> void
HEAP_OBJ_CREATION::Prop_hor_on_object(CODEREP *cr, HEAP_OBJ_REP *hor,
                                      HOR_PHI_FIXUP &fixup) {
  Is_True(cr && cr->Kind() == CK_VAR && Is_hor_valid(hor),
          ("bad cr/hor"));
  if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
      cr->Is_var_volatile())
    return;

  if (cr->Is_flag_set(CF_DEF_BY_PHI) ||
      cr->Is_flag_set(CF_DEF_BY_CHI))
    return;

  STMTREP *def = cr->Get_defstmt();
  Is_True(def->Lhs() == cr, ("def stmt lhs mismatch"));
  Prop_hor_on_cr(def->Rhs(), hor, fixup);
}

// propagate hor on vsym_obj_rep from lhs to rhs
template<> void
HEAP_OBJ_CREATION::Prop_hor_on_object(VSYM_OBJ_REP *vor, HEAP_OBJ_REP *hor,
                                      HOR_PHI_FIXUP &fixup) {
  Is_True(vor && Is_hor_valid(hor), ("bad vor/hor"));
  switch (vor->Attr()) {
  case ROR_DEF_BY_PHI:
  case ROR_DEF_BY_HORPHI:
  case ROR_DEF_BY_CHI:
  case ROR_DEF_BY_NONE:
  case ROR_DEF_BY_NULL:
    break;
  case ROR_DEF_BY_COPY:   // arr[i] = x;
  case ROR_DEF_BY_ISTORE: // p->fld = x;
    {
      STMTREP *def = vor->Stmt_def();
      Is_True(def != NULL, ("vor istore def is NULL"));
      Prop_hor_on_cr(def->Rhs(), hor, fixup);
    }
    break;
  default:
    Is_True(FALSE, ("vor def by what?"));
    break;
  }
}

void
HEAP_OBJ_CREATION::Prop_hor_on_cr(CODEREP *cr, HEAP_OBJ_REP *hor,
                                  HOR_PHI_FIXUP &fixup) {
  Is_True(cr != NULL && Is_hor_valid(hor), ("bad cr/hor"));
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
    break;
  case CK_LDA:
    break;
  case CK_VAR:
    if (Find_hor_on_object(cr) == NULL) {
      Mark_hor_on_object(cr, hor);
      Prop_hor_on_object(cr, hor, fixup);
    }
    break;
  case CK_IVAR:
    Is_True(cr->Opr() != OPR_PARM && cr->Ilod_base() != NULL,
            ("bad ivar"));
    Prop_hor_on_cr(cr->Ilod_base(), hor, fixup);
    if (Find_hor_on_object(cr) == NULL)
      Mark_hor_on_object(cr, hor);
    break;
  case CK_OP:
    if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_SUB) {
      Prop_hor_on_cr(cr->Opnd(0), hor, fixup);
    }
    else if (cr->Opr() == OPR_ADD) {
      cr = Comp_unit()->Analyze_base_info(NULL, cr, FALSE);
      if (cr) {
        Prop_hor_on_cr(cr, hor, fixup);
      }
    }
    break;
  default:
    Is_True(FALSE, ("unknown cr kind"));
    break;
  }
}

//  Fixup_hor_on_phi: fixup missing phi result/operands for CODEREP phi
template<typename _OBJECT> HEAP_OBJ_CREATION::FIX_RESULT
HEAP_OBJ_CREATION::Fixup_hor_on_phi(PHI_NODE *phi, HOR_PHI_FIXUP &fixup)
{
  Is_True(!Phi_res_is_hor(phi), ("bad hor phi"));
  BOOL done = TRUE;
  BOOL changed = FALSE;
  // check result
  _OBJECT *res = (_OBJECT *)phi->RESULT();
  HEAP_OBJ_REP *res_hor = Find_hor_on_object(res);
  if (!Is_hor_valid(res_hor)) {
    fixup.Clear_phi_fixup();
    res_hor = Find_hor_for_phi<_OBJECT>(phi, TRUE, fixup);
    if (Is_hor_valid(res_hor)) {
      changed = TRUE;
      Is_True(res_hor == Find_hor_on_object(res), ("res hor not marked"));
    }
    else if (done == TRUE) {
      done = FALSE;
    }
  }

  // check operands
  for (INT i = 0; i < phi->Size(); ++i) {
    _OBJECT *opnd = (_OBJECT *)phi->OPND(i);
    HEAP_OBJ_REP *opnd_hor = Find_hor_on_object(opnd);
    if (!Is_hor_valid(opnd_hor)) {
      fixup.Clear_phi_fixup();
      opnd_hor = Find_hor_on_object(opnd, TRUE, fixup);
      if (Is_hor_valid(opnd_hor)) {
        if (changed == FALSE)
          changed = TRUE;
        Is_True(opnd_hor == Find_hor_on_object(opnd), ("opnd hor not marked"));
      }
      else if (done == TRUE) {
        done = FALSE;
      }
    }
    else {
      Prop_hor_on_object(opnd, opnd_hor, fixup);
    }
  }

  return done ? R_DONE : (changed ? R_CHANGED : R_UNCHANGED);
}

// Fixup_hor_on_cr: fixup missing phi operands and update the result HOR ulist
HEAP_OBJ_REP *
HEAP_OBJ_CREATION::Fixup_hor_on_cr(STMTREP *sr, CODEREP *cr, HEAP_OBJ_REP *hor_found,
                                   HOR_PHI_FIXUP &fixup)
{
  BOOL changed, all_done;
  INT  iter = 0;

  // set to fixup stage
  fixup.Set_do_fixup();

  // fixup phi result at first
  do {
    changed = FALSE;   // assume no change
    all_done = TRUE;   // assume all phis are done
    for (HOR_PHI_FIXUP::phi_iterator it = fixup.phi_begin();
         it != fixup.phi_end(); ++it) {
      PHI_NODE *phi = it->first;
      Is_True(!Phi_res_is_hor(phi), ("hor phi"));
      BOOL is_vor = Phi_res_is_vor(phi);
      FIX_RESULT res = is_vor ? Fixup_hor_on_phi<VSYM_OBJ_REP>(phi, fixup)
                              : Fixup_hor_on_phi<CODEREP>(phi, fixup);

      if (res != R_DONE && all_done)
        all_done = FALSE;

      if (res == R_CHANGED && changed)
        changed = FALSE;
    }
    Is_True(all_done || changed, ("nothing changed in 1 iteration"));
    ++ iter;
  } while (changed && iter < 16);

  Is_True(iter < 16, ("loop more than 16 times?"));

  // make sure hor phi created have valid hor on result and opnd
  for (HOR_PHI_FIXUP::hor_iterator it = fixup.hor_begin();
       it != fixup.hor_end(); ++it) {
    HEAP_OBJ_REP *hor = *it;
    Is_True(hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI, ("hor not def by phi"));
    PHI_NODE *hor_phi = hor->Phi_def();
    Is_True(hor_phi != NULL && hor == (HEAP_OBJ_REP*)hor_phi->RESULT(),
            ("phi result mismatch"));
    PHI_NODE *var_phi = hor->Heap_obj()->Base_phi();
    Is_True(var_phi != NULL && !Phi_res_is_hor(var_phi) &&
            var_phi->Size() == hor_phi->Size() &&
            var_phi->Bb() == hor_phi->Bb(),
            ("invalid base phi"));
    BOOL is_vor = Phi_res_is_vor(var_phi);
    Is_True(Is_hor_valid((HEAP_OBJ_REP*)hor_phi->RESULT()),
            ("res hor invalid"));
    HEAP_OBJ_REP *opnd_hor;
    for (INT i = 0; i < var_phi->Size(); ++i) {
      opnd_hor = is_vor ? ((VSYM_OBJ_REP*)var_phi->OPND(i))->Hor()
                        : Vsa()->Cr_2_heap_obj(var_phi->OPND(i));
      Is_True(Is_hor_valid(opnd_hor), ("opnd(%d) hor invalid", i));
      hor_phi->Set_opnd(i, (CODEREP *)opnd_hor);
    }
  }

  // already found a valid hor, just return it
  if (Is_hor_valid(hor_found))
    return hor_found;

  // fixup sr & cr
  fixup.Clear_phi_fixup();
  return Find_hor_on_cr(sr, cr, fixup);
}

// Initialize: initialize heap_obj creation phase
void
HEAP_OBJ_CREATION::Initialize() {
}

void
HEAP_OBJ_CREATION::Finalize() {
  HOR_PHI_FIXABLE fixable(_hva);

  PENDING_CR_LIST free_call_list;
  // try resolve pending hor for free()
  PENDING_CR_LIST::iterator end = _pending_cr_list->end();
  PENDING_CR_LIST::iterator it;
  for (it = _pending_cr_list->begin(); it != end; ++it) {
    STMTREP *sr = it->first;
    CODEREP *cr = it->second;
    Is_True(cr->Kind() == CK_VAR || cr->Kind() == CK_IVAR,
            ("invalid cr kind"));
    BOOL is_free = (OPERATOR_is_call(sr->Opr()) &&
                    Vsa()->Callee_frees_heap_memory(sr) == cr);
    if (fixable.Check_cr_fixable(cr) == FALSE) {
      // add free call to a list because cr hor may be fixed in other stmts
      if (is_free) {
        free_call_list.push_back(ST_CR_PAIR(sr, cr));
      }
      else {
        _hva->Set_visit_next(sr);
      }
      continue;
    }
    HOR_PHI_FIXUP fixup(_hva, _hva->Temp_pool());
    HEAP_OBJ_REP *hor = Find_hor_on_cr(sr, cr, fixup);
    // wait for next round
    if (hor == HOR_NOT_FOUND) {
      // add free call to a list because cr hor may be fixed in other stmts
      if (is_free) {
        free_call_list.push_back(ST_CR_PAIR(sr, cr));
      }
      else {
        _hva->Set_visit_next(sr);
      }
      continue;
    }
    hor = Fixup_hor_on_cr(sr, cr, hor, fixup);
    Is_True(Is_hor_valid(hor), ("fixup failed?"));

    if (!Vsa()->Cr_2_heap_obj(cr)) {
      Vsa()->Enter_cr_heap_obj_map(cr, hor);
    }
    if (is_free) {
      Annotate_free_call(sr, cr, hor);
    }
  }

  end = free_call_list.end();
  for (it = free_call_list.begin(); it != end; ++it) {
    STMTREP *sr = it->first;
    CODEREP *cr = it->second;
    Is_True(OPERATOR_is_call(sr->Opr()) &&
            Vsa()->Callee_frees_heap_memory(sr) == cr,
            ("not free"));
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(cr);
    if (hor != NULL) {
      // cr hor is annotated, annotate the stmt
      Annotate_free_call(sr, cr, hor);
    }
    else {
      _hva->Set_visit_next(sr);
    }
  }

  // dump heap_obj created or updated
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif
  Is_Trace(Tracing(), (TFile, "HOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "HOC[%d]: FINISH HEAP_OBJ_CREATION.\n", _hva->Round()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+heap obj created:\n", _hva->Ho_created()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+heap obj updated:\n", _hva->Ho_updated()));
  Is_Trace(Tracing(), (TFile, "HOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  //Is_Trace(Tracing(), (TFile, "HOC[%d] HEAP OBJECT ANNOTATION dump:\n", _hva->Round()));
  //Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
}

// ============================================================================
// HEAP_OBJ_RENAMING
//
// Rename heap_obj
// ============================================================================
class HEAP_OBJ_RENAMING : public CFG_VISITOR_BASE<HEAP_OBJ_RENAMING> {
public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_OPT_CHI | V_RETURN | V_ASM_STMT,
  };

  HEAP_OBJ_RENAMING(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) { }

private:
  void
  Update_ulist_w_ho_rename(HEAP_OBJ_REP *hor, HEAP_OBJ_REP *prev, STMTREP *sr,
                           CODEREP *cr, ROR_ATTR attr);

  void
  Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *sr);

  template<BOOL _FWD> void
  Process_chi(STMTREP* sr);

  template<BOOL _FWD> void
  Process_call(STMTREP* sr, BOOL handle_rhs);

  template<BOOL _FWD> void
  Process_stid(STMTREP* sr, BOOL handle_rhs);

  template<BOOL _FWD> void
  Process_istore(STMTREP* sr, BOOL handle_rhs);

public:
  // rename phi when entering the bb
  void Enter_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_ho_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      HEAP_OBJ_REP *hor = (HEAP_OBJ_REP*)phi->RESULT();
      Is_True(hor, ("invalid hor phi result"));
      //if (hor->Version_not_set() == FALSE)
      //  continue;
      if (!_hva->Visit_heap_obj(hor))
        continue;

      // update ulist if this hor is defined by varphi or vorphi
      if (hor->Attr() == ROR_DEF_BY_VARPHI ||
          hor->Attr() == ROR_DEF_BY_VORPHI) {
        HOR_LIST *ulist = hor->Ulist();
        if (ulist == NULL) {
          ulist = CXX_NEW(HOR_LIST, Vsa()->Mem_pool());
          hor->Set_ulist(ulist);
        }
        for (INT i = 0; i < phi->Size(); ++i) {
          HEAP_OBJ_REP *opnd = (HEAP_OBJ_REP *)phi->OPND(i);
          Is_True(opnd != NULL, ("hor phi opnd is NULL"));
          if (Vsa()->Is_special_hor(opnd))
            continue;
          HEAP_OBJ_REP *tos = opnd->Heap_obj()->Top_of_stack();
          if (ulist->Find(tos) == NULL) {
            // add tos to hor's ulist
            ulist->Append(CXX_NEW(HOR_NODE(tos), Vsa()->Mem_pool()));
            HOR_LIST *tos_ulist = tos->Ulist();
            if (tos_ulist == NULL) {
              tos_ulist = CXX_NEW(HOR_LIST, Vsa()->Mem_pool());
              tos->Set_ulist(tos_ulist);
            }
            // add hor to tos's ulist
            if (tos_ulist->Find(hor) == NULL) {
              tos_ulist->Append(CXX_NEW(HOR_NODE(hor), Vsa()->Mem_pool()));
            }
            Is_Trace(Tracing(), (TFile, "HOR[%d]: add ulist between res ", _hva->Round()));
            Is_Trace_cmd(Tracing(), hor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " and opnd %d ", i));
            Is_Trace_cmd(Tracing(), tos->Print(TFile));
            Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
          }
        }
      }
      // push hor to stack
      IDTYPE version = _hva->Gen_name(hor, NULL);
      Is_True(hor->Phi_def() == phi, ("phi def mismatch"));
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // rename succ's phi operand before entering dom bb
  void Enter_dom_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    BB_NODE     *succ;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
      PHI_LIST* phi_list = Vsa()->Bb_ho_philist(succ);
      if (!phi_list)
        continue;
      INT pos = succ->Pred()->Pos(bb);
      Is_True(pos != -1, ("invalid pos %d", pos));
      PHI_NODE     *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
        HEAP_OBJ_REP *opnd = Phi_opnd_mismatch(phi)
                               ? (HEAP_OBJ_REP *) phi->OPND(pos)
                               : (HEAP_OBJ_REP *) phi->RESULT();
        Is_True(opnd != NULL, ("found null hor phi opnd"));
        Is_True(Phi_opnd_mismatch(phi) ||
                opnd->Heap_obj() == ((HEAP_OBJ_REP*)phi->RESULT())->Heap_obj(),
                ("ho mismatch"));

        if (!_hva->Visit_heap_obj(opnd))
          continue;
        HEAP_OBJ_REP *tos = opnd->Heap_obj()->Top_of_stack();
        Is_True(((tos->Attr() == ROR_DEF_BY_PHI ||
                  tos->Attr() == ROR_DEF_BY_VARPHI ||
                  tos->Attr() == ROR_DEF_BY_VORPHI) &&
                 tos->Phi_def() != NULL) ||
                tos->Stmt_def() != NULL ||
                tos->Is_entry_chi(),
                ("hor not defined"));
        phi->Set_opnd(pos, (CODEREP *)tos);
        Is_Trace(Tracing(),
                 (TFile, "HOR[%d]: update opnd %d for hor ",
                         _hva->Round(), pos));
        Is_Trace_cmd(Tracing(), opnd->Print(TFile));
        Is_Trace(Tracing(), (TFile, " to hor "));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " from BB%d to BB%d.\n",
                                    bb->Id(), succ->Id()));
      }
    }
  }

  // pop phi result from renaming stack before exits the bb
  void Exit_bb(BB_NODE* bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_ho_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      HEAP_OBJ_REP *hor = (HEAP_OBJ_REP *) phi->RESULT();
      if (!_hva->Visit_heap_obj(hor))
        continue;
      Is_True(hor == hor->Heap_obj()->Top_of_stack(), ("stack mismatch"));
      hor->Heap_obj()->Pop();
      Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // forward process the stmtrep
  void Process_stmt_fwd(STMTREP* sr);

  // backward process the stmtrep
  void Process_stmt_rev(STMTREP* sr);

  // process the rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP* sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<void>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  void Process_cr(STMTREP* sr, CODEREP* cr, UINT flag) { }

  // perform HEAP_OBJ_REP renaming phase
  void Perform() {
    DOM_WALKER<HEAP_OBJ_RENAMING> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // Initialize HEAP_OBJ_REP renaming phase
  void Initialize();

  // finalize HEAP_OBJ_REP renaming phase
  void Finalize();
};

// Update ulist and rename (push) all hos in hor list
void
HEAP_OBJ_RENAMING::Update_ulist_w_ho_rename(HEAP_OBJ_REP *hor, HEAP_OBJ_REP *prev, STMTREP *sr,
                                            CODEREP *cr, ROR_ATTR attr)
{
  HOR_LIST *hor_list = prev->Ulist();
  if (hor_list == NULL)
    return;

#ifdef Is_True_On
  hash_set<uintptr_t> ho_set;
#endif
  HOR_LIST *new_ulist = CXX_NEW(HOR_LIST, Vsa()->Mem_pool());
  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;
  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (cur_hor->Heap_obj() == hor->Heap_obj() ||
        Vsa()->Is_special_hor(cur_hor))
      continue;
    if (cur_hor->Attr() == ROR_DEF_BY_LDA ||
        cur_hor->Attr() == ROR_DEF_BY_ALLOCA)
      continue;
#ifdef Is_True_On
    Is_True(hor->Heap_obj() != cur_hor->Heap_obj(),
            ("ho itself in hor ulist"));
    Is_True(ho_set.find((uintptr_t)cur_hor->Heap_obj()) == ho_set.end(),
            ("Update_ulist_w_ho_rename: ho added again"));
    ho_set.insert((uintptr_t)cur_hor->Heap_obj());
#endif
    HEAP_OBJ_REP *new_hor;
    HEAP_OBJ_REP *tos_hor = cur_hor->Heap_obj()->Top_of_stack();
    CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, cur_hor->Heap_obj());
    if (chi != NULL) {
      CHOR *chi_opnd = (CHOR*)chi->OPND();
      chi_opnd->first = tos_hor;
      CHOR *chi_res = (CHOR*)chi->RESULT();
      new_hor = chi_res->first;
      Is_True((new_hor->Attr() == ROR_DEF_BY_CHI ||
               new_hor->Attr() == ROR_DEF_BY_FREE) &&
              new_hor->Stmt_def() == sr,
              ("attr or stmt_def mismatch"));
    }
    else {
      new_hor = Vsa()->Clone_heap_obj(tos_hor, sr->Bb(), Vsa()->Mem_pool());
      new_hor->Set_attr(attr);
      new_hor->Set_srcpos_node(sr, Dna(), PATHINFO_PARM);
      new_hor->Set_stmt_def(sr, Dna());
      new_hor->Set_prev_ver(cur_hor);
      Is_True(Vsa()->Find_stmt_hor_chi(sr, tos_hor->Heap_obj()) == NULL,
              ("hor chi already added"));
      Vsa()->Append_stmt_hor_chi(sr, new_hor, tos_hor, cr);
    }
    new_hor->Gen_name(sr);
    new_ulist->Append(CXX_NEW(HOR_NODE(new_hor), Vsa()->Mem_pool()));
    Is_Trace(Tracing(),
             (TFile, "Update_ulist_w_ho_rename: push hor "));
    Is_Trace_cmd(Tracing(), new_hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in the ulist of "));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(), (TFile, " in sr%d %s\n",
                         sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
  }
  hor->Set_ulist(new_ulist);
}

// Pop all hoos in hor list
void
HEAP_OBJ_RENAMING::Update_ulist_w_ho_rename_rev(HEAP_OBJ_REP *hor, STMTREP *sr)
{
  HOR_LIST *hor_list = hor->Ulist();
  if (hor_list == NULL)
    return;

  HOR_LIST_ITER hor_list_iter;
  HEAP_OBJ_REP *cur_hor;
  FOR_ALL_NODE(cur_hor, hor_list_iter, Init(hor_list)) {
    if (Vsa()->Is_special_hor(cur_hor) ||
        cur_hor->Attr() == ROR_DEF_BY_LDA ||
        cur_hor->Attr() == ROR_DEF_BY_ALLOCA)
      continue;
    if (cur_hor->Heap_obj()->Top_match_sr(sr)) {
      Is_Trace(Tracing(),
               (TFile, "Update_ulist_w_ho_rename_rev: pop hor "));
      Is_Trace_cmd(Tracing(),
                   cur_hor->Heap_obj()->Top_of_stack()->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in the ulist of "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in sr%d %s\n",
                           sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));

      cur_hor->Heap_obj()->Pop();
    }
  }
}

// Process_chi<TRUE>: forward pass to rename chi
template<> void
HEAP_OBJ_RENAMING::Process_chi<TRUE>(STMTREP* sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_OPT_CHI ||
           OPERATOR_is_store(sr->Opr()) ||
           OPERATOR_is_call(sr->Opr())), ("invalid sr"));

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  CODEREP* free_arg = Vsa()->Callee_frees_heap_memory(sr);
  if (chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      // skip rename for free_arg, already processed in Process_call
      if (cr == free_arg) {
        continue;
      }
      if (_hva->Visit_heap_obj(hor)) {
        HEAP_OBJ_REP* tos = hor->Heap_obj()->Top_of_stack();
        copnd->first = tos;
        _hva->Gen_name(hor, sr);
        Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " to chi cr%d in sr%d %s.\n",
                         cr->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }
}

// Process_chi<FALSE>: forward pass to rename chi
template<> void
HEAP_OBJ_RENAMING::Process_chi<FALSE>(STMTREP* sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_OPT_CHI ||
           OPERATOR_is_store(sr->Opr()) ||
           OPERATOR_is_call(sr->Opr())), ("invalid sr"));

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  CODEREP* free_arg = Vsa()->Callee_frees_heap_memory(sr);
  if (chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(!Vsa()->Is_special_hor(hor),
              ("TODO: special hor?"));
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      // skip rename for free_arg, already processed in Process_call
      if (cr == free_arg) {
        continue;
      }
      if (hor->Heap_obj()->Top_match_sr(sr)) {
        hor->Heap_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor chi result ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " from chi cr%d in sr%d %s.\n",
                         cr->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }
}

// Process_call<TRUE>: forward pass to rename call
template<> void
HEAP_OBJ_RENAMING::Process_call<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  CODEREP* rhs = sr->Rhs();
  if (handle_rhs)
    Process_coderep<void>(sr, rhs, FALSE);

  CODEREP* arg = Vsa()->Callee_frees_heap_memory(sr);
  if (arg != NULL) {
    HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(arg);
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      HEAP_OBJ_REP* tos = hor->Heap_obj()->Top_of_stack();
      hor = Vsa()->Update_stmt_hor_chi(sr, tos, arg);
      // perform hor unification
      if (VSA_Hor_Unification || VSA_New_HVA_Unification) {
        Update_ulist_w_ho_rename(hor, tos, sr, arg, ROR_DEF_BY_FREE);
      }

      if (!Vsa()->Callee_returns_new_heap_memory(sr))
        Vsa()->Enter_cr_heap_obj_map(sr->Rhs(), hor);
      if (VSA_New_HVA_Compat) {
        hor->Set_prev_ver(tos);
        Vsa()->Enter_cr_heap_obj_refmap(sr->Rhs(), tos);
      }
    }
  }

  if (Vsa()->Callee_returns_new_heap_memory(sr)) {
    HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(rhs);
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      _hva->Gen_name(hor, sr);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to malloc CALL cr%d in sr%d %s.\n",
                       rhs->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));

      CODEREP* ret = Comp_unit()->Find_return_value(sr);
      if (ret != NULL)
        Vsa()->Enter_cr_heap_obj_map(ret, hor);
    }
  }

  // process chi but skip chi cr which is free arg
  Process_chi<TRUE>(sr);

#if 0
  // How to handle RBC model? This should be done earlier before HO/VO
  // creation phase
  RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
  const CALLEE_VECTOR& callee_list = rna->Callee_list();
  for (CALLEE_VECTOR::const_iterator iter = callee_list.begin();
       iter != callee_list.end(); iter++) {
    DNA_NODE *callee = Vsa()->Ipsa()->Get_dna(iter->Callee());
    if (callee != NULL && callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
      HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj_ref(rhs);
      if (hor != NULL) {
        HEAP_OBJ_REP* tos = hor->Heap_obj()->Top_of_stack();
        Vsa()->Enter_cr_heap_obj_refmap(rhs, tos);
        hor->Gen_name(sr);
        hor->Set_prev_ver(tos);
        hor->Set_srcpos_node(sr, Dna(), PATHINFO_FREE);
        Is_Trace(Tracing(), (TFile, "HOR: push hor "));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " to RBC free cr%d in sr%d %s.\n",
                         rhs->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
        if (VSA_Hor_Unification)
          Vsa()->Update_ulist_w_free_rename(hor, tos, sr);
      }
      else {
        hor = Vsa()->Cr_2_heap_obj(rhs);
        if (hor != NULL) {
          hor->Gen_name(sr);
          Is_Trace(Tracing(), (TFile, "HOR: push hor "));
          Is_Trace_cmd(Tracing(), hor->Print(TFile));
          Is_Trace(Tracing(),
                   (TFile, " to RBC call cr%d in sr%d %s.\n",
                           rhs->Coderep_id(), sr->Stmtrep_id(),
                           OPERATOR_name(sr->Opr()) + 4));
        }
      }
    }
  } // end for all callee
#endif

}

// Process_call<FALSE>: reverse pass to rename CALL
template<> void
HEAP_OBJ_RENAMING::Process_call<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  CODEREP* arg = Vsa()->Callee_frees_heap_memory(sr);
  if (arg != NULL) {
    HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_chi(sr, arg);
    if (hor != NULL &&
        !Vsa()->Is_special_hor(hor)) {
      if ((VSA_Hor_Unification || VSA_New_HVA_Unification) &&
          hor->Heap_obj()->Top_match_sr(sr)) {
        Update_ulist_w_ho_rename_rev(hor, sr);
      }
      if (hor->Heap_obj()->Top_match_sr(sr)) {
        hor->Heap_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor free param ", _hva->Round()));
        Is_Trace_cmd(Tracing(), hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " from param cr%d in sr%d %s.\n",
                         arg->Coderep_id(), sr->Stmtrep_id(),
                         OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
  if (hor != NULL &&
      !Vsa()->Is_special_hor(hor) &&
      hor->Heap_obj()->Top_match_sr(sr)) {
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor call rhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from rhs of sr%d %s.\n",
                     sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
  }

  CODEREP* ret = Comp_unit()->Find_return_value(sr);
  if (ret != NULL && TY_kind(ret->object_ty()) == KIND_POINTER) {
    hor = Vsa()->Cr_2_heap_obj(ret);
    if (hor != NULL &&
        !Vsa()->Is_special_hor(hor) &&
        hor->Heap_obj()->Top_match_sr(sr)) {
      hor->Heap_obj()->Pop();
      Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor call retval ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " from retval cr%d of sr%d %s.\n",
                       ret->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));
    }
  }

  // process chi but skip chi cr which is free arg
  Process_chi<FALSE>(sr);
}

template<> void
HEAP_OBJ_RENAMING::Process_istore<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalis istore"));

  // rename rhs
  Process_coderep<void>(sr, sr->Rhs(), 0);

  // rename lhs
  CODEREP *lhs = sr->Lhs();
  Process_coderep<void>(sr, lhs, 0);

  // process hor chi annotated on this stmtrep
  HEAP_OBJ_RENAMING::Process_chi<TRUE>(sr);
}

template<> void
HEAP_OBJ_RENAMING::Process_istore<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalis istore"));

  // process hor chi annotated on this stmtrep
  HEAP_OBJ_RENAMING::Process_chi<FALSE>(sr);

  // rename lhs
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Lhs());
  if (hor &&
      _hva->Visit_heap_obj(hor) &&
      hor->Attr() == ROR_DEF_BY_ALLOCA &&
      hor->Stmt_def() == sr) {
    Is_True(hor->Heap_obj()->Top_match_sr(sr), ("hor tos mismatch"));
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor istore lhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from lhs cr%d of sr%d for ALLOCA-ISTORE.\n",
                     sr->Lhs()->Coderep_id(), sr->Stmtrep_id()));
  }
  Is_True(hor == NULL ||
          Vsa()->Is_special_hor(hor) ||
          !hor->Heap_obj()->Top_match_sr(sr),
          ("istore creates new hor"));
}

// forward process stmtrep
void
HEAP_OBJ_RENAMING::Process_stmt_fwd(STMTREP* sr)
{
  // Process stmt ho mu list
  MU_LIST *mu_list = Vsa()->Stmt_hor_mu(sr);
  if (mu_list) {
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(Vsa()->Stmt_hor_mu(sr))) {
      CHOR *chor = (CHOR *)mnode->OPND();
      HEAP_OBJ_REP *hor = chor->first;
      if (!_hva->Visit_heap_obj(hor))
        continue;
      HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
      if (hor != tos) {
        chor->first = tos;
        Is_Trace(Tracing(), (TFile, "HOR[%d]: set mu opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  // process stmt
  // if stmt is marked to be visited, visit it include rhs. otherwise only
  // visit call because it may create new version for HO
  Process_stmtrep<TRUE>(sr);
  if (OPERATOR_is_call(sr->Opr()) && !_hva->Visit_stmt(sr))
    Process_call<TRUE>(sr, FALSE);
  // visit chi because it may be impacted by newly created HO
  else if (sr->Opr() == OPR_OPT_CHI && !_hva->Visit_stmt(sr))
    Process_chi<TRUE>(sr);

  // do not process hor chi list because only call can have hor chi and it's
  // handled in Process_call.
}

// backward process stmtrep
void
HEAP_OBJ_RENAMING::Process_stmt_rev(STMTREP* sr)
{
  // if stmt is marked to be visited, visit it include rhs. otherwise only
  // visit call because it may create new version for HO
  Process_stmtrep<FALSE>(sr);
  if (OPERATOR_is_call(sr->Opr()) && !_hva->Visit_stmt(sr))
    Process_call<FALSE>(sr, FALSE);
  // visit chi because it may be impacted by newly created HO
  else if (sr->Opr() == OPR_OPT_CHI && !_hva->Visit_stmt(sr))
    Process_chi<FALSE>(sr);
}

// Process_cr<CK_VAR>: rename VAR coderep
template<> void
HEAP_OBJ_RENAMING::Process_cr<CK_VAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_VAR, ("invalid var cr"));

  HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_mu(sr, cr);
  if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
    HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
    if (tos != hor) {
      if (_hva->Ho_created(hor))
        Vsa()->Append_stmt_hor_mu(sr, tos, cr);
      else if (_hva->Ho_updated(hor))
        Vsa()->Update_stmt_hor_mu(sr, hor, cr, TRUE);
      else
        return;

      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor mu ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to sym%dv%d cr%d in sr%d %s.\n",
                       cr->Aux_id(), cr->Version(), cr->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }
  }
}

// Process_cr<CK_IVAR>: renmae IVAR coderep
template<> void
HEAP_OBJ_RENAMING::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    return Process_coderep<void>(sr, cr->Ilod_base(), flag);
  }

  CODEREP* base = sr->Lhs() == cr ? cr->Istr_base() : cr->Ilod_base();
  Process_coderep<void>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD)
    Process_coderep<void>(sr, cr->Mload_size(), flag);

  HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
  if (hor != NULL &&
      !Vsa()->Is_special_hor(hor) &&
      hor->Heap_obj()->Kind() != RSC_KIND_LDA &&
      _hva->Visit_heap_obj(hor)) {
    HEAP_OBJ_REP *tos;
    if (hor->Attr() == ROR_DEF_BY_ALLOCA &&
        hor->Stmt_def() == sr && cr == sr->Lhs()) {
      _hva->Gen_name(hor, sr);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d for ALLOCA-ISTORE.\n",
                       cr->Coderep_id(), sr->Stmtrep_id()));
      tos = hor;
    }
    else {
      tos = hor->Heap_obj()->Top_of_stack();
    }
    if (hor != tos) {
      Vsa()->Enter_cr_heap_obj_map(cr, tos);
      VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(cr);
      if (vor != NULL && vor->Hor() != tos) {
        Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
        vor->Set_hor(tos);
      }
      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      if (vor != NULL) {
        Is_Trace(Tracing(), (TFile, " to vor "));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
      }
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d %s.\n",
                       cr->Coderep_id(), sr->Stmtrep_id(),
                       OPERATOR_name(sr->Opr()) + 4));
    }
  }
}

// Process_cr<CK_OP>: rename OP coderep
template<> void
HEAP_OBJ_RENAMING::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<void>(sr, cr->Opnd(i), flag);
  }
}

// Process_sr<OPR_STID, TRUE>: forward pass to rename stid
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  Process_coderep<void>(sr, sr->Rhs(), 0);

  CODEREP *lhs = sr->Lhs();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(lhs);
  if (hor && _hva->Visit_heap_obj(hor)) {
    if (hor->Attr() == ROR_DEF_BY_ALLOCA &&
        hor->Stmt_def() == sr) {
      _hva->Gen_name(hor, sr);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: push hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to cr%d in sr%d for ALLOCA-STID.\n",
                       lhs->Coderep_id(), sr->Stmtrep_id()));
      return;
    }
    HEAP_OBJ_REP *tos = hor->Heap_obj()->Top_of_stack();
    if (hor != tos) {
      Vsa()->Enter_cr_heap_obj_map(lhs, tos);
      Is_Trace(Tracing(), (TFile, "HOR[%d]: Annotate hor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), tos->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to sym%dv%d cr%d in sr%d %s.\n",
                       lhs->Aux_id(), lhs->Version(), lhs->Coderep_id(),
                       sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }
  }
}

// Process_sr<OPR_STID, FALSE>: reverse pass to rename stid
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_STID, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  CODEREP *lhs = sr->Lhs();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(lhs);
  if (hor &&
      _hva->Visit_heap_obj(hor) &&
      hor->Attr() == ROR_DEF_BY_ALLOCA &&
      hor->Stmt_def() == sr) {
    Is_True(hor->Heap_obj()->Top_match_sr(sr), ("hor tos mismatch"));
    hor->Heap_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "HOR[%d]: pop hor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), hor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from cr%d in sr%d for ALLOCA-STID.\n",
                     lhs->Coderep_id(), sr->Stmtrep_id()));
    return;
  }
  Is_True(hor == NULL ||
          Vsa()->Is_special_hor(hor) ||
          !hor->Heap_obj()->Top_match_sr(sr),
          ("stid creates new hor"));
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to rename istore
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename istore
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_ISTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<FALSE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to rename mstore
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  // process mstore size
  Process_coderep<void>(sr, sr->Lhs()->Mstore_size(), 0);
  // process istore
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, FALSE>: reverse pass to rename mstore
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_MSTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  Process_istore<FALSE>(sr, TRUE);
}

// Process_sr<OPR_CALL, TRUE>: forward pass to rename call/icall
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  if (_hva->Visit_stmt(sr)) {
    Process_call<TRUE>(sr, TRUE);
  }
}

// Process_sr<OPR_CALL, FALSE>: reverse pass to rename call/icall
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  if (_hva->Visit_stmt(sr)) {
    Process_call<FALSE>(sr, FALSE);
  }
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass rename intrinsic call
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn call sr"));
  if (_hva->Visit_stmt(sr)) {
    Process_call<TRUE>(sr, TRUE);
  }
}

// Process_sr<OPR_INTRINSIC_CALL, FALSE>: reverse pass rename intrinsic call
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_INTRINSIC_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn call sr"));
  Process_call<FALSE>(sr, FALSE);
}

// Process_sr<OPR_OPT_CHI, TRUE>: rename hor annotated on entry chi
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid opt_chi sr"));
  Process_chi<TRUE>(sr);
}

// Process_sr<OPR_OPT_CHI, FALSE>: reverse pass rename entry chi
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_OPT_CHI, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid opt_chi sr"));
  Process_chi<FALSE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: rename hor annotated on return stmt
template<> void
HEAP_OBJ_RENAMING::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_RETURN, ("invalid return sr"));
  BOOL ho_created = _hva->Has_ho_created();
  BOOL ho_updated = _hva->Has_ho_updated();
  if (ho_created == FALSE && ho_updated == FALSE)
    return;

  STMT_HOR_SET hor_set(_hva, sr, _hva->Local_pool());
  // create/update hor for return value
  HEAP_OBJ_REP *hor;
  STMTREP *stid = sr->Prev();
  if (stid && OPERATOR_is_scalar_store(stid->Opr())) {
    CODEREP *retv = stid->Lhs();
    Is_True(retv && retv->Kind() == CK_VAR, ("bad retv"));
    if (Opt_stab()->Aux_stab_entry(retv->Aux_id())->Is_return_preg() &&
        (hor = Vsa()->Cr_2_heap_obj(retv)) != NULL &&
        !Vsa()->Is_special_hor(hor))
      hor_set.Add(hor, retv, HO_MOD);
  }

  // create/update hor for return var mu
  MU_NODE *mu;
  MU_LIST_ITER mu_iter;
  FOR_ALL_NODE(mu, mu_iter, Init(sr->Mu_list())) {
    if (mu->Aux_id() == Opt_stab()->Default_vsym() ||
        mu->Aux_id() == Opt_stab()->Return_vsym())
      continue;
    CODEREP *opnd = mu->OPND();
    if ((hor = Vsa()->Cr_2_heap_obj(opnd)) != NULL &&
        !Vsa()->Is_special_hor(hor))
      hor_set.Add(hor, opnd, HO_MOD);
  }

  // create/update hor for entry chi ho
  HO_PTR_SET& created = _hva->Ho_created();
  for (HO_PTR_SET::iterator it = created.begin();
       it != created.end(); ++it) {
    HEAP_OBJ_REP *hor = (*it)->Top_of_stack();
    HEAP_OBJ *ho = hor->Heap_obj();
    AUX_ID aux_id = ho->Sym_id();
    if (aux_id != INVALID_ID) {
      STMTREP *entry_stmt = _vsa->Get_entry_chi_stmt();
      CHI_NODE* chi = entry_stmt ? entry_stmt->Chi_list()->Search_chi_node(aux_id) : NULL;
      if (chi && chi->Live()) {
        hor_set.Add(hor, chi->RESULT(), HO_REF);
      }
    }
  }
  HO_PTR_SET& updated = _hva->Ho_updated();
  for (HO_PTR_SET::iterator it = updated.begin();
       it != updated.end(); ++it) {
    HEAP_OBJ_REP *hor = (*it)->Top_of_stack();
    HEAP_OBJ *ho = hor->Heap_obj();
    AUX_ID aux_id = ho->Sym_id();
    if (aux_id != INVALID_ID) {
      STMTREP *entry_stmt = _vsa->Get_entry_chi_stmt();
      CHI_NODE* chi = entry_stmt ? entry_stmt->Chi_list()->Search_chi_node(aux_id) : NULL;
      if (chi && chi->Live()) {
        hor_set.Add(hor, chi->RESULT(), HO_REF);
      }
    }
  }

  // check ho and create hor mu
  HO_MOD_REF_MAP *set = hor_set.Hor_set();
  HO_MOD_REF_MAP::iterator end = set->end();
  for (HO_MOD_REF_MAP::iterator it = set->begin(); it != end; ++it) {
    HEAP_OBJ *ho = it->first->Heap_obj();
    CODEREP  *cr = it->second.first;
    HEAP_OBJ_REP *hor = ho->Top_of_stack();
    if (ho_created && _hva->Ho_created(hor))
      Vsa()->Append_stmt_hor_mu(sr, hor, cr);
    else if (ho_updated && _hva->Ho_updated(hor))
      Vsa()->Update_stmt_hor_mu(sr, hor, cr, TRUE);
  }

  if (VSA_New_HVA_Compat) {
    HO_PTR_SET& created = _hva->Ho_created();
    // create HOR_ARRAY on exit bb. refer VSA::Generate_ho_exit_mu
    BB_NODE *bb = sr->Bb();
    HOR_ARRAY *array = Vsa()->Bb_horarr_map()->Lookup(bb->Id());
    if (array == NULL) {
      MEM_POOL* mp = Vsa()->Mem_pool();
      array = CXX_NEW(HOR_ARRAY(mempool_allocator<HEAP_OBJ_REP*>(mp)),
                      mp);
      Is_True(array != NULL, ("out of memory"));
      Vsa()->Bb_horarr_map()->Insert(bb->Id(), array);
      array->reserve(created.size() * 2 + 4);
      array->resize(created.size());
    }
    else {
      if (created.size())
        array->resize(array->size() + created.size());
    }

    IDTYPE first_ho_id = _hva->First_ho_id();
    // update hor mu
    HO_PTR_SET& updated = _hva->Ho_updated();
    for (HO_PTR_SET::iterator it = updated.begin();
         it != updated.end(); ++it) {
      HEAP_OBJ_REP *hor = (*it)->Top_of_stack();
      INT index = hor->Heap_obj()->Id() - first_ho_id;
      Is_True(index >= 0 && index < array->size(),
              ("index out of bound"));
      (*array)[index] = hor;
    }

    // append hor mu
    for (HO_PTR_SET::iterator it = created.begin();
         it != created.end(); ++it) {
      HEAP_OBJ_REP *hor = (*it)->Top_of_stack();
      INT index = hor->Heap_obj()->Id() - first_ho_id;
      Is_True(index >= 0 && index < array->size(),
              ("index out of bound"));
      (*array)[index] = hor;
    }
  }
}

// initialize HEAP_OBJ_REP renaming phase
void
HEAP_OBJ_RENAMING::Initialize() {
  // place phi for ho created or updated in this iteration
  VSA::PHILIST_MAP *bb_ro_philist = _vsa->Bb_ho_philist();
  HEAP_OBJ         *heap_obj;
  HEAP_OBJ_REP     *hor;
  if (_hva->Has_ho_updated()) {
    // insert or update phi for heap_obj updated
    HASH_SET_ITER<HO_PTR_SET, HEAP_OBJ*> iter;
    _vsa->Place_ro_phi_node(bb_ro_philist, heap_obj, &_hva->Ho_updated(), &iter, hor, TRUE, _hva->Ho_phi_cache());
  }
  if (_hva->Has_ho_created()) {
    // insert phi for heap_obj created
    // has to set last param TRUE here because for HO created for varphi/vorphi, the
    // phi node has been added
    HASH_SET_ITER<HO_PTR_SET, HEAP_OBJ*> iter;
    _vsa->Place_ro_phi_node(bb_ro_philist, heap_obj, &_hva->Ho_created(), &iter, hor, TRUE, _hva->Ho_phi_cache());
  }
}

// finalize HEAP_OBJ_REP renaming phase
void
HEAP_OBJ_RENAMING::Finalize() {
  // verify ho renaming stack
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif

  // trace CFG with ho/vo annotation
  Is_Trace(Tracing(), (TFile, "HOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: FINISH HEAP_OBJ_RENAMING.\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  //Is_Trace_cmd(Tracing(), Ipsa()->Print_fld_name_map(TFile));
  Is_Trace(Tracing(), (TFile, "HOR[%d]: HEAP OBJECT ANNOTATION dump:\n", _hva->Round()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
}

// ============================================================================
// VSYM_OBJ_CREATION
//
// Check IR and create VSYM based on HEAP_OBJ
// ============================================================================
class VSYM_OBJ_CREATION : public CFG_VISITOR_BASE<VSYM_OBJ_CREATION> {
private:
  // flags for traversing coderep
  enum CR_FLAG {
    CF_NONE    = 0x1,      // no special flag
    CF_IN_CALL = 0x2,      // CR used in call
    CF_IN_PARM = 0x4,      // CR used in parm
  };

  // a vector for all pending call stmt which has HEAP_OBJ used in callee
  typedef vector<STMT_HOR_SET*,
                 mempool_allocator<STMT_HOR_SET*> > PENDING_STMT_VEC;

  // helper function to collect HO used by a call
  class STMT_HOR_SET_HELPER {
  private:
    PENDING_STMT_VEC   *_vec;
    MEM_POOL           *_mpool;
    HEAP_VSYM_ANALYSIS *_hva;
    STMTREP            *_stmt;
    STMT_HOR_SET       *_set;

  public:
    // constructor
    STMT_HOR_SET_HELPER(PENDING_STMT_VEC *vec, MEM_POOL *mpool, HEAP_VSYM_ANALYSIS *hva, STMTREP *stmt)
      : _vec(vec), _mpool(mpool), _hva(hva), _stmt(stmt), _set(NULL) { }

    // add hor and its field hor to _ho_set of the stmt
    void Add(HEAP_OBJ_REP *hor, CODEREP *cr, UINT mr) {
      if (_set == NULL) {
        if (_vec->empty() || _vec->back()->Stmtrep() != _stmt) {
          _set = CXX_NEW(STMT_HOR_SET(_hva, _stmt, _mpool), _mpool);
          _vec->push_back(_set);
        }
        else {
          _set = _vec->back();
        }
      }
      Is_True(!_hva->Vsa()->Is_special_hor(hor), ("special hor added"));
      _set->Add(hor, cr, mr);
    }
  };

  PENDING_STMT_VEC *_pending_stmts;     // calls and ho it refered

public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_RETURN | V_ASM_STMT,
  };

  // constructor
  VSYM_OBJ_CREATION(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) {
    // create _pending_stmts on local pool. this will be destroyed when
    // current iteration is done
    _pending_stmts = CXX_NEW(PENDING_STMT_VEC(), _hva->Local_pool());
    Is_True(_pending_stmts, ("out of memory?"));
  }

private:
  // process call
  template<BOOL _FWD>
  void Process_call(STMTREP *sr);

  // process istore
  template<BOOL _FWD>
  void Process_istore(STMTREP *sr);

  VSYM_OBJ_REP *Process_param(STMTREP *sr, CODEREP *cr, UINT flag);

public:
  // enter bb
  void Enter_bb(BB_NODE *bb) {
    // do nothing. phi list created in HEAP_OBJ_CREATION
  }

  // process rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP *sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<VSYM_OBJ_REP *>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  VSYM_OBJ_REP *Process_cr(STMTREP *sr, CODEREP *cr, UINT flag) { return NULL; }

  // perform the VSYM_OBJ_REP creation phase
  void Perform() {
    CFG_WALKER<VSYM_OBJ_CREATION> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // initialize the VSYM_OBJ_REP creation phase
  void Initialize();

  // finalize the VSYM_OBJ_REP creation phase
  void Finalize();
};

// Process_call<TRUE>: forward pass to create vsym for call
template<> void
VSYM_OBJ_CREATION::Process_call<TRUE>(STMTREP *sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));

  // process rhs
  CODEREP *rhs = sr->Rhs();
  Process_coderep<VSYM_OBJ_REP *>(sr, rhs, FALSE);

  // STMT_HOR_SET for this call
  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);

  // check if call frees memory
  CODEREP *arg = Vsa()->Callee_frees_heap_memory(sr);
  if (arg != NULL) {
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      // add to _pending calls and handled later
      ho_set.Add(hor, arg, HO_MOD);
    }
  }

  // check if call mallocs memory
  if (Vsa()->Callee_returns_new_heap_memory(sr)) {
    HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(sr->Rhs());
    CODEREP* ret = Comp_unit()->Find_return_value(sr);
    CODEREP* bind_cr = sr->Rhs();
    // check if return value has hor
    if (ret != NULL && TY_kind(ret->object_ty()) == KIND_POINTER) {
      HEAP_OBJ_REP *ret_hor = Vsa()->Cr_2_heap_obj(ret);
      Is_True(ret_hor == hor, ("hor mismatch for malloc"));
      // bind to return value
      bind_cr = ret;
    }
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      // add to _pending calls and handled later
      ho_set.Add(hor, bind_cr, HO_MOD);
    }
  }

  CHI_LIST *chi_list = Vsa()->Stmt_hor_chi(sr);
  if (arg == NULL && chi_list != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      if (_hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, cr, HO_MOD);
      }
    }
  }
}

// Process_istore<TRUE>: forward pass to create vsym for istore
template<> void
VSYM_OBJ_CREATION::Process_istore<TRUE>(STMTREP *sr)
{
  Is_True(sr &&
          (sr->Opr() == OPR_ISTORE || sr->Opr() == OPR_MSTORE),
          ("invalid istore"));

  // process rhs
  CODEREP *rhs = sr->Rhs();
  Process_coderep<VSYM_OBJ_REP*>(sr, rhs, 0);

  // process lhs
  CODEREP *lhs = sr->Lhs();
  CODEREP *base = lhs->Istr_base();
  // process lhs base
  Process_coderep<VSYM_OBJ_REP*>(sr, base, 0);

  // check if vsym_obj already annotated
  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(lhs);
  if (vor != NULL)
    return;

  // create vor for lhs
  HEAP_OBJ_REP* hor;
  base = Find_ilod_base(base);
  if (base == NULL) {
    hor = Vsa()->Null_hor();
  }
  else {
    hor = Vsa()->Find_stmt_hor_mu(sr, base);
    if (hor == NULL) {
      _hva->Set_visit_next(sr);
      return;
    }
  }
  Is_True(hor != NULL, ("invalid hor"));

  if (!Vsa()->Is_special_hor(hor)) {
    VSYM_FLD_REP vfr = Vsa()->Cr_vfr(lhs);
    vor = _hva->Create_vsym_obj_def(sr->Bb(), hor, &vfr, Defbb_pool());

    // check if vfr is FLD_K_ANY and set hor flag
    if (vfr.Is_any() && !hor->Field_any())
      hor->Set_field_any(TRUE);

    if (vor->Vsym_obj()->Ref_cr() == NULL)
      vor->Vsym_obj()->Set_ref_cr(lhs);
    vor->Set_srcpos_node(sr, Dna(), PATHINFO_ISTORE);
    vor->Set_attr(ROR_DEF_BY_ISTORE);
    vor->Set_stmt_def(sr, Dna());
    // create vsym chi if hor has ulist
    HOR_LIST *ulist = hor->Ulist();
    if (ulist) {
      HEAP_OBJ_REP *cur_hor;
      HOR_LIST_ITER hor_list_iter;
      FOR_ALL_NODE(cur_hor, hor_list_iter, Init(ulist)) {
        VSYM_OBJ_REP *cur_vor;
        if (cur_hor->Attr() != ROR_DEF_BY_LDA &&
            cur_hor->Is_entry_chi())
          continue;

        if (vfr.Is_any() && !cur_hor->Field_any())
          cur_hor->Set_field_any(TRUE);

        cur_vor = _hva->Create_vsym_obj_use(cur_hor, &vfr);
        if (cur_vor->Vsym_obj()->Ref_cr() == NULL)
          vor->Vsym_obj()->Set_ref_cr(lhs);
        // create vor chi on this stmt
        _hva->Append_stmt_vor_chi(lhs, sr, cur_vor, Defbb_pool());

        Is_Trace(Tracing(), (TFile, "VOC[%d]: Create chi vor ", _hva->Round()));
        Is_Trace_cmd(Tracing(), cur_vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on ulist hor "));
        Is_Trace_cmd(Tracing(), cur_hor->Print(TFile));
        Is_Trace(Tracing(),
                 (TFile, " to ISTORE lhs cr%d:\n", lhs->Coderep_id()));
        Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
      }
    }

    // annotate rhs_hor to lhs vor and set as hor field
    HEAP_OBJ_REP* rhs_hor;
    if (rhs != NULL && (rhs_hor = Vsa()->Find_stmt_hor_mu(sr, rhs)) != NULL) {
      Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
      vor->Set_hor(rhs_hor);
      FIELD_OBJ_REP* fl = CXX_NEW(FIELD_OBJ_REP(rhs_hor, vfr),
                                  Vsa()->Mem_pool());
      fl->Set_Next(hor->Flist());
      hor->Set_flist(fl);
      if(!_vsa->Is_special_hor(rhs_hor)) {
        STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
        ho_set.Add(rhs_hor, rhs, HO_REF);
      }
    }
  }
  else if (hor == Vsa()->Default_hor()) {
    // use Default_vor for Default_hor
    vor = Vsa()->Default_vor();
  }
  else {
    // use Null_vor for NULL or Null_hor
    vor = Vsa()->Default_vor();
  }

  Vsa()->Enter_cr_vor_map(lhs, vor);
  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(),
           (TFile, " to ISTORE lhs cr%d:\n", lhs->Coderep_id()));
  Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
}

VSYM_OBJ_REP *
VSYM_OBJ_CREATION::Process_param(STMTREP *sr, CODEREP *cr, UINT flag)
{
  Is_True(sr && cr && cr->Opr() == OPR_PARM, ("invalid ivar cr"));
  UINT mod_ref = 0;
  BOOL parm_need_vsym = FALSE;
  CODEREP *base = cr->Ilod_base();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(base);
  HEAP_OBJ_REP *hor_mu = NULL;
  HEAP_OBJ_REP *hor_chi = NULL;
  VSYM_OBJ_REP *base_vor = Process_coderep<VSYM_OBJ_REP*>(sr, base, flag);
  if (hor != NULL && !Vsa()->Is_special_hor(hor)) {
    hor_mu = Vsa()->Find_stmt_hor_mu(sr, base);
    CHI_NODE *chi = Vsa()->Find_stmt_hor_chi(sr, hor->Heap_obj());
    // if no hor chi on sr (heap obj not been mod), use hor mu
    hor_chi = chi ? ((CHOR*)chi->RESULT())->first : hor_mu;
  }
  if (OPERATOR_is_call(sr->Opr()) && (hor_mu || hor_chi)) {
    RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
    IDTYPE    param = rna->Get_arg_with_cr(base);
    // do a kack for intrinsic call for testing purpose
    INTRINSIC intrn = Get_call_intrinsic(sr);
    if (intrn == INTRN_STRCPY || intrn == INTRN_STRNCPY ||
        intrn == INTRN_MEMCPY) {
      if (param == 1)
        mod_ref |= HO_MOD;
      else if (param == 2)
        mod_ref |= HO_REF;
    }
    else if (intrn == INTRN_MEMSET && param == 1)
      mod_ref |= HO_MOD;

    if (rna->Is_set_arg_flag(param, REF_ILOAD))
      mod_ref |= HO_REF;
    if (rna->Is_set_arg_flag(param, REF_ISTORE))
      mod_ref |= HO_MOD;
    if (mod_ref != 0) {
      // if the base contents is not modified in current PU
      // do not need to create vsym or vsym mu/chi on stmt
      // UD follows with base pointer
      V_ANNOT annot = base->Kind() == CK_VAR ? base->Vsa_annot() : VANT_UTIL::Empty();
      parm_need_vsym = TRUE;
      if (annot != VANT_UTIL::Empty() && (mod_ref & HO_MOD == 0) &&
          VANT_UTIL::Get(annot, ANT_VWRITE) == ANT_NO) {
        parm_need_vsym = FALSE;
      }
      // STMT_HOR_SET for this call, add sr to pending stmts to create vsym mu/chi
      // on sr in Finalize phase
      if (parm_need_vsym) {
        VSYM_OBJ *base_vo = base_vor ? base_vor->Vsym_obj() : NULL;
        STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
        if (mod_ref & HO_REF) {
          Is_True(hor_mu, ("null hor mu for HO_REF"));
          ho_set.Add(hor_mu, base, HO_REF);
          // if base is an vsym, add the vsym to vor mu list
          if (base_vo && Vsa()->Find_vor_mu(sr, base_vo) == NULL) {
            _hva->Append_stmt_vor_mu(base, sr, base_vo->Entry_chi());
          }
        }
        if (mod_ref & HO_MOD) {
          Is_True(hor_chi, ("null hor chi for HO_MOD"));
          ho_set.Add(hor_chi, base, HO_MOD);
          // if base is an vsym, add the vsym to vor chi list
          // to represent chi for any field
          if (base_vo && Vsa()->Find_vor_chi(sr, base_vo) == NULL) {
            _hva->Append_stmt_vor_chi(base, sr, base_vo->Entry_chi(), Defbb_pool());
            if (!_hva->Vo_created(base_vor)) {
              _hva->Set_vo_updated(base_vor->Vsym_obj());
            }
          }
        }
        // if base is LDA, add base's mu's hor. for example:
        // b = &a; foo(&b);
        // add hor of LDA.
        if (base->Kind() == CK_LDA) {
          ST *st = base->Lda_base_st();
          Is_True(st != NULL, ("Lda base st is NULL"));
          // so far only iterate chi_list. if needed, iterate mu_list
          CHI_NODE *chi;
          CHI_LIST_ITER chi_iter;
          FOR_ALL_NODE (chi, chi_iter, Init(sr->Chi_list())) {
            if (!chi->Live() ||
                chi->Aux_id() == Opt_stab()->Default_vsym() ||
                chi->Aux_id() == Opt_stab()->Return_vsym())
              continue;
            CODEREP *opnd = chi->OPND();
            if (opnd->Is_flag_set(CF_IS_ZERO_VERSION) ||
                opnd->Is_var_volatile())
              continue;
            AUX_STAB_ENTRY *aux = Opt_stab()->Aux_stab_entry(chi->Aux_id());
            Is_True(aux, ("aux entry is NULL"));
            if (aux->St() != st)
              continue;
            VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(opnd);
            HEAP_OBJ_REP *vor_hor;
            if (vor != NULL &&
                (vor_hor = vor->Hor()) != NULL &&
                !Vsa()->Is_special_hor(vor_hor)) {
              // TODO: add with chi RESULT/OPND?
              ho_set.Add(vor_hor, base, mod_ref);
            }
          }
        }
      }
    }
  }
  // if vsym is needed for the base pointer, create vsym with any or zero field
  if (parm_need_vsym && base_vor == NULL) {
    VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
    VSYM_FLD_REP any_fld(FLD_K_ANY, 0, 0);
    VSYM_FLD_REP *vfr = (base->Kind() == CK_LDA) ? &zero_fld : &any_fld;
    if ((mod_ref & HO_REF) &&
        !Vsa()->Is_special_hor(hor_mu)) {
      VSYM_OBJ_REP *mu_vor = _hva->Create_vsym_obj_use(hor_mu, vfr);
      if (_hva->Vo_created(mu_vor)) {
        if (mu_vor->Vsym_obj()->Ref_cr() == NULL)
          mu_vor->Vsym_obj()->Set_ref_cr(base);
      }

      Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), mu_vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor_mu->Print(TFile));
      Is_Trace(Tracing(),
              (TFile, " to call mu for parm cr%d\n", cr->Coderep_id()));
    }
    if ((mod_ref & HO_MOD) &&
        !Vsa()->Is_special_hor(hor_chi) ) {
      VSYM_OBJ_REP *chi_vor = _hva->Create_vsym_obj_def(sr->Bb(), hor_chi, vfr, Defbb_pool());
      if (_hva->Vo_created(chi_vor)) {
        if (chi_vor->Vsym_obj()->Ref_cr() == NULL)
          chi_vor->Vsym_obj()->Set_ref_cr(base);
      }
      chi_vor->Set_srcpos_node(sr, Dna(), PATHINFO_CHI);
      chi_vor->Set_attr(ROR_DEF_BY_CHI);
      chi_vor->Set_stmt_def(sr, Dna());
      Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), chi_vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor_chi->Print(TFile));
      Is_Trace(Tracing(),
              (TFile, " to call chi for parm cr%d\n", cr->Coderep_id()));
    }
  }
  return base_vor;
}

// Process_cr<CK_IVAR>: create VSYM for IVAR
template<> VSYM_OBJ_REP*
VSYM_OBJ_CREATION::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    return Process_param(sr, cr, flag);
  }

  CODEREP* base = sr->Lhs() == cr ? cr->Istr_base() : cr->Ilod_base();
  // create VOR for ICALL vptr/vtable?
  // if we don't need vor for ICALL vptr/vtable, the last kid of ICALL shouldn't visit
  //if (!Vsa()->Is_ivar_need_vsym(cr, sr))
  //  return NULL;

  // create vsym_obj in base
  Process_coderep<VSYM_OBJ_REP*>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD) {
    // handle MLOAD size
    Process_coderep<VSYM_OBJ_REP*>(sr, cr->Mload_size(), FALSE);
  }

  // check if vor already created
  VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(cr);
  if (vor != NULL)
    return vor;

  HEAP_OBJ_REP *hor;
  base = Find_ilod_base(base);
  if (base == NULL) {
    hor = Vsa()->Null_hor();
  }
  else {
    hor = Vsa()->Find_stmt_hor_mu(sr, base);
    if (hor == NULL) {
      _hva->Set_visit_next(sr);
      return NULL;
    }
  }
  Is_True(hor != NULL, ("invalid hor"));

  if (!Vsa()->Is_special_hor(hor) &&
      (hor->Attr() == ROR_DEF_BY_LDA || !hor->Is_entry_chi())) {
    Is_True(hor->Attr() != ROR_DEF_BY_ISTORE && hor->Attr() != ROR_DEF_BY_COPY &&
            hor->Attr() != ROR_DEF_BY_DANGLE,
            ("TODO: phi, varphi, vorphi, istore, copy, dangle"));

    VSYM_FLD_REP vfr = Vsa()->Cr_vfr(cr);
    vor = _hva->Create_vsym_obj_use(hor, &vfr);

    if (vfr.Is_any() && !hor->Field_any())
      hor->Set_field_any(TRUE);

    // create vor entry chi def on stmt which defs cur_hor
    if (_hva->Vo_created(vor)) {
      if (vor->Vsym_obj()->Ref_cr() == NULL)
        vor->Vsym_obj()->Set_ref_cr(cr);
    }
  }
  else {
    vor = Vsa()->Null_vor();
  }
  Is_True(vor && hor, ("invalid vor"));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), hor->Print(TFile));
  Is_Trace(Tracing(),
          (TFile, " to IVAR cr%d:\n", cr->Coderep_id()));
  Vsa()->Enter_cr_vor_map(cr, vor);
  Is_Trace_cmd(Tracing(), cr->Print(2, TFile));
  return vor;
}

// Process_cr<CK_OP>: create VSYM for OP cr
template<> VSYM_OBJ_REP*
VSYM_OBJ_CREATION::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<VSYM_OBJ_REP*>(sr, cr->Opnd(i), flag);
  }
  return NULL;
}

// Process_sr<OPR_STID, TRUE>: forward process STID
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  // check rhs
  Process_coderep<VSYM_OBJ_REP*>(sr, sr->Rhs(), 0);

  CODEREP *lhs = sr->Lhs();
  Is_True(lhs->Kind() == CK_VAR, ("bad lhs cr"));

  AUX_ID lhs_id = lhs->Aux_id();

  if (Vsa()->Cr_2_vor(lhs))
    return;

  HEAP_OBJ *ho = NULL;
  AUX_ID aux_id = lhs_id;
  do {
    ho = Vsa()->Find(aux_id, TRUE);
    if (ho != NULL)
      break;
    aux_id = Opt_stab()->St_group(aux_id);
  } while (aux_id != lhs_id && aux_id != 0);

  if (ho == NULL)
    return;

  Is_True(ho->Kind() == RSC_KIND_LDA, ("bad ho attr"));

  VSYM_FLD_REP vfr = Vsa()->Cr_vfr(lhs);
  VSYM_OBJ_REP *vor = _hva->Create_vsym_obj_def(sr->Bb(),
                                                ho->Entry_chi(),
                                                &vfr,
                                                Defbb_pool());

  if (vor->Vsym_obj()->Ref_cr() == NULL)
    vor->Vsym_obj()->Set_ref_cr(lhs);
  vor->Set_srcpos_node(sr, Dna(), PATHINFO_COPY);
  vor->Set_attr(ROR_DEF_BY_COPY);
  vor->Set_stmt_def(sr, Dna());
  Vsa()->Enter_cr_vor_map(lhs, vor);

  Is_Trace(Tracing(), (TFile, "VOC[%d]: Create vor ", _hva->Round()));
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " on hor "));
  Is_Trace_cmd(Tracing(), ho->Entry_chi()->Print(TFile));
  Is_Trace(Tracing(), (TFile, " for STID sym%dv%d cr%d sr%d.\n",
                              lhs->Aux_id(), lhs->Version(),
                              lhs->Coderep_id(), sr->Stmtrep_id()));

  // annotate rhs_hor to lhs vor and set as hor field
  HEAP_OBJ_REP* rhs_hor;
  CODEREP *rhs = sr->Rhs();
  HEAP_OBJ_REP *lhs_hor = ho->Entry_chi();
  if (rhs != NULL && (rhs_hor = Vsa()->Find_stmt_hor_mu(sr, rhs)) != NULL) {
    Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
    vor->Set_hor(rhs_hor);
    FIELD_OBJ_REP* fl = CXX_NEW(FIELD_OBJ_REP(rhs_hor, FLD_K_ID, 
                                              Vsa()->Cr_fldid(lhs), Vsa()->Cr_ofst(lhs)),
                                Vsa()->Mem_pool());
    fl->Set_Next(lhs_hor->Flist());
    lhs_hor->Set_flist(fl);
  }
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to create VSYM for ISTORE
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to create VSYM for MSTORE
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid istore sr"));
  // process mstore size
  Process_coderep<VSYM_OBJ_REP*>(sr, sr->Lhs()->Mstore_size(), FALSE);
  // process istore
  Process_istore<TRUE>(sr);

  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);
  // handle vor on lhs
  CODEREP *base = sr->Lhs()->Istr_base();
  HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(base);
  if (hor != NULL && _hva->Visit_heap_obj(hor)) {
    ho_set.Add(hor, base, HO_MOD);
  }
  // handle vor on rhs
  if (sr->Rhs()->Kind() == CK_IVAR) {
    base = sr->Rhs()->Ilod_base();
    hor = Vsa()->Cr_2_heap_obj(base);
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      ho_set.Add(hor, base, HO_REF);
    }
  }
  else {
    Is_True(sr->Rhs()->Kind() == CK_CONST, ("TODP: var or op"));
  }
}

// Process_sr<OPR_OPT_CHI, TRUE>: forward pass to create vsym for OPT_CHI
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Nothing to do? remove this function later
}

// Process_sr<OPR_CALL, TRUE>: forward pass to create vsym for CALL
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass for INTRINSIC_CALL
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not a call"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: forward pass for RETURN
template<> void
VSYM_OBJ_CREATION::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_RETURN, ("not return stmt"));

  // STMT_HOR_SET for return stmt
  STMT_HOR_SET_HELPER ho_set(_pending_stmts, _hva->Local_pool(), _hva, sr);

  // check hor on return value
  STMTREP* prev = sr->Prev();
  if (prev != NULL && OPERATOR_is_scalar_store(prev->Opr()) &&
      Opt_stab()->Aux_stab_entry(prev->Lhs()->Aux_id())->Is_dedicated_preg()) {
    HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(prev->Lhs());
    if (hor != NULL && _hva->Visit_heap_obj(hor)) {
      ho_set.Add(hor, prev->Lhs(), HO_REF);
    }
  }

  // check var mu on return stmt
  MU_LIST *var_mu_list = sr->Mu_list();
  if (var_mu_list != NULL && !var_mu_list->Is_Empty()) {
    MU_NODE *mu;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mu, mu_iter, Init(var_mu_list)) {
      // ignore special vsym
      if (mu->Aux_id() == Opt_stab()->Default_vsym() ||
          mu->Aux_id() == Opt_stab()->Return_vsym())
        continue;
      AUX_STAB_ENTRY *aux_entry = Opt_stab()->Aux_stab_entry(mu->Aux_id());
      // no vor on reg/auto
      if (aux_entry->St() == NULL ||
          ST_sclass(aux_entry->St()) == SCLASS_REG ||
          ST_sclass(aux_entry->St()) == SCLASS_AUTO)
        continue;
      // no vor for non-pointer type formal
      if ((ST_sclass(aux_entry->St()) == SCLASS_FORMAL ||
           ST_sclass(aux_entry->St()) == SCLASS_FORMAL_REF) &&
          TY_kind(aux_entry->Ty()) != KIND_POINTER)
        continue;
      HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(mu->OPND());
      if (hor != NULL && _hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, mu->OPND(), HO_REF);
      }
    }
  }

  // check hor mu
  MU_LIST *hor_mu_list = Vsa()->Stmt_hor_mu(sr);
  if (hor_mu_list != NULL && !hor_mu_list->Is_Empty()) {
    MU_NODE *hor_mu;
    MU_LIST_ITER hor_mu_iter;
    FOR_ALL_NODE(hor_mu, hor_mu_iter, Init(hor_mu_list)) {
      CHOR *chor = (CHOR*)hor_mu->OPND();
      HEAP_OBJ_REP *hor = chor->first;
      if (hor!= NULL && _hva->Visit_heap_obj(hor)) {
        ho_set.Add(hor, chor->second, HO_REF);
      }
    }
  }
  // others, say escaped hor, vor on return?
}

// initialize vsym_obj creation
void
VSYM_OBJ_CREATION::Initialize() {
}

// finalize vsym_obj creation, create vor mu/chi for call and return
void
VSYM_OBJ_CREATION::Finalize() {
  // create vor mu/chi for calls
  Is_True(_pending_stmts, ("pending calls vector is null"));
  // TODO: should vo_updated be processed here?
  if (!_hva->Has_vo_created()) {
    Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
    Is_Trace(Tracing(), (TFile, "VOC[%d]: NO VO created\n", _hva->Round()));
    if (_hva->Has_vo_updated()) {
      Is_Trace_cmd(Tracing(),
                   Dump_ptr_set(TFile, " +VO updated:\n", _hva->Vo_updated()));
    }
    else {
      Is_Trace(Tracing(), (TFile, "VOC[%d]: NO VO updated\n", _hva->Round()));
    }
    Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
    return;
  }

#if 0
  // create vor chi for entry chi
  STMTREP *entry = Vsa()->Get_entry_chi_stmt();
  Is_True(entry && entry->Opr() == OPR_OPT_CHI,
          ("invalid entry chi stmt"));
  STMT_HOR_SET ent_set(entry, _hva->Local_pool());
  CHI_LIST *entry_chi = Vsa()->Stmt_hor_chi(entry);
  if (entry_chi != NULL) {
    CHI_NODE *chi;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(chi, chi_iter, Init(entry_chi)) {
      CHOR *copnd = (CHOR *)chi->OPND();
      CHOR *cres = (CHOR*)chi->RESULT();
      HEAP_OBJ_REP *hor = cres->first;
      CODEREP *cr = cres->second;
      Is_True(hor->Heap_obj() == copnd->first->Heap_obj(),
              ("Heap obj mismatch"));
      Is_True(cr && cr == copnd->second,
              ("cr mismatch"));
      ent_set.Add(hor, cr, HO_REF);
    }
  }
#endif

  VSYM_OBJ *vobj;
  HASH_SET_ITER<VO_PTR_SET, VSYM_OBJ*> vo_iter;
  // traverse all vsym obj
  PENDING_STMT_VEC::iterator cend = _pending_stmts->end();
  FOR_ALL_NODE(vobj, vo_iter, Init(&_hva->Vo_created())) {
    HEAP_OBJ_REP *base_hor = vobj->Base_hor();
    VSYM_OBJ_REP *vor = vobj->Entry_chi();
    VSYM_FLD_REP vfr = vobj->Fld_rep();
    if (base_hor->Field_any()) {
      // append vor chi for aliased vo/vor
      HOR_VO_LIST_ITER iter(base_hor);
      VSYM_OBJ *alias_vo;
      FOR_ALL_NODE(alias_vo, iter, Init()) {
        if (alias_vo == vobj || !vfr.Aliased(alias_vo->Fld_rep())) {
          continue;
        }
        VSYM_OBJ_REP *alias_vor = alias_vo->Entry_chi();
        BOOL visit_vor = _hva->Visit_vsym_obj(alias_vor);
        Is_True(alias_vor != NULL, ("no entry chi"));
        alias_vor = alias_vor->Next();
        while (alias_vor) {
          if (alias_vor->Attr() == ROR_DEF_BY_COPY ||
              alias_vor->Attr() == ROR_DEF_BY_ISTORE) {
            STMTREP *stmt = alias_vor->Stmt_def();
            Is_True(stmt && OPERATOR_is_store(stmt->Opr()),
                    ("not store"));
            _hva->Append_stmt_vor_chi(stmt->Lhs(), stmt, vor, Defbb_pool());
            _hva->Set_visit_later(stmt);
            if (visit_vor == FALSE) {
              visit_vor = TRUE;
              _hva->Set_vo_updated(alias_vo);
            }

            Is_Trace(Tracing(), (TFile, "VOC[%d]: Create chi for vor ", _hva->Round()));
            Is_Trace_cmd(Tracing(), vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " for alias vor "));
            Is_Trace_cmd(Tracing(), alias_vor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " based on hor "));
            Is_Trace_cmd(Tracing(), base_hor->Print(TFile));
            Is_Trace(Tracing(), (TFile, " on lhs cr%d of %s sr%d.\n",
                                 stmt->Lhs()->Coderep_id(),
                                 OPERATOR_name(stmt->Opr()) + 4,
                                 stmt->Stmtrep_id()));
          }
          alias_vor = alias_vor->Next();
        }
      }
    }

    // if base_hor is created from VARPHI/VORPHI, create
    // related VOR phi which has attr of DEF_BY_HORPHI
    if (base_hor->Attr() == ROR_DEF_BY_VARPHI ||
        base_hor->Attr() == ROR_DEF_BY_VORPHI)
      _hva->Create_vsym_phi_for_hor_phi(vobj, base_hor);

    HEAP_OBJ *ho = base_hor->Heap_obj();
#if 0
    // create stmt vor chi for entry chi
    HO_MOD_REF_MAP::iterator ent_it = ent_set.Ho_set()->find(ho);
    if (ent_it != ent_set.Ho_set()->end()) {
      CODEREP *cr = ent_it->second.first;
      _hva->Append_stmt_vor_chi(cr, entry, vor, Defbb_pool());
    }
#endif

    // traverse all pending calls
    for (PENDING_STMT_VEC::iterator cit = _pending_stmts->begin();
         cit != cend; ++cit) {
      STMTREP *stmt = (*cit)->Stmtrep();
      HO_MOD_REF_MAP *set = (*cit)->Hor_set();
      Is_True(stmt && set, ("stmt or set is null"));
      HO_MOD_REF_MAP::iterator hit = set->find(base_hor);
      if (hit != set->end()) {
        CODEREP  *cr = hit->second.first;
        UINT      mr = hit->second.second;
        if (mr & HO_REF)
          _hva->Append_stmt_vor_mu(cr, stmt, vor);
        if (mr & HO_MOD)
          _hva->Append_stmt_vor_chi(cr, stmt, vor, Defbb_pool());

        Is_Trace(Tracing(),
                 (TFile, "VOC[%d]: Create %s for vor ", _hva->Round(),
                  mr == (HO_REF | HO_MOD) ? "mu/chi"
                      : mr == HO_REF ? "mu"
                          : mr == HO_MOD ? "chi" : "-err-"));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " on cr%d of %s sr%d.\n",
                             cr->Coderep_id(),
                             OPERATOR_name(stmt->Opr()) + 4,
                             stmt->Stmtrep_id()));
      }
    }
  }

  // dump vo created or updated in this iteration
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif
  Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: FINISH VSYM_OBJ_CREATION.\n", _hva->Round()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+vsym obj created:\n", _hva->Vo_created()));
  Is_Trace_cmd(Tracing(),
               Dump_ptr_set(TFile, "+vsym obj updated:\n", _hva->Vo_updated()));
  Is_Trace(Tracing(), (TFile, "VOC[%d]: ---------------------------------------------------------------\n", _hva->Round()));

}  // VSYM_OBJ_CREATION::Finalize

// ============================================================================
// VSYM_OBJ_RENAMING
//
// Traverse DOM tree and rename VSYM
// ============================================================================
class VSYM_OBJ_RENAMING : public CFG_VISITOR_BASE<VSYM_OBJ_RENAMING> {
public:
  // set visit flag
  enum {
    // visit LDA, CONST, VAR, IVAR and OP CODEREP
    CR_VFLAG = V_LDA | V_CONST | V_VAR | V_IVAR | V_OP,
    // visit store and call STMTREP
    SR_VFLAG = V_ANY_STORE | V_ANY_CALL | V_ANY_BRANCH | V_OPT_CHI | V_RETURN | V_ASM_STMT,
  };

  // constructor
  VSYM_OBJ_RENAMING(HEAP_VSYM_ANALYSIS *hva)
   : CFG_VISITOR_BASE(hva, this) { }

private:
  // process call
  template<BOOL _FWD>
  void Process_call(STMTREP *sr);

  // process istore
  template<BOOL _FWD>
  void Process_istore(STMTREP *sr, BOOL handle_rhs);

public:
  // Enter_bb: before enter the bb, rename the vor phi result
  void Enter_bb(BB_NODE *bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_vo_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
      Is_True(vor, ("invalid vor phi result"));
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      IDTYPE version = _hva->Gen_name(vor, NULL);
      //vor->Set_phi_def(phi);
      Is_True(vor->Phi_def() == phi, ("phi def mismatch"));
      Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // Enter_dom_bb: before enter the dom bb, rename the vor phi opnd
  void Enter_dom_bb(BB_NODE* bb)
  {
    BB_NODE*     succ;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (succ, bb_iter, Init(bb->Succ())) {
      PHI_LIST* phi_list = Vsa()->Bb_vo_philist(succ);
      if (!phi_list)
        continue;
      INT pos = succ->Pred()->Pos(bb);
      Is_True(pos != -1, ("invalid pos %d", pos));
      PHI_NODE     *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
        VSYM_OBJ_REP* opnd = Phi_opnd_mismatch(phi)
                               ? (VSYM_OBJ_REP *) phi->OPND(pos)
                               : (VSYM_OBJ_REP *) phi->RESULT();
        Is_True(opnd != NULL, ("found null vor phi opnd"));
        Is_True(Phi_opnd_mismatch(phi) ||
                opnd->Vsym_obj() == ((VSYM_OBJ_REP*)phi->RESULT())->Vsym_obj(),
                ("vo mismatch"));
        if (!_hva->Visit_vsym_obj(opnd))
          continue;

        VSYM_OBJ_REP *tos = opnd->Vsym_obj()->Top_of_stack();
        Is_True(((tos->Attr() == ROR_DEF_BY_PHI ||
                  tos->Attr() == ROR_DEF_BY_HORPHI) &&
                 tos->Phi_def() != NULL) ||
                tos->Stmt_def() != NULL ||
                tos->Is_entry_chi(),
                ("vor not defined"));
        phi->Set_opnd(pos, (CODEREP*)tos);
        Is_Trace(Tracing(),
                 (TFile, "VOR[%d]: update opnd %d for vor ",
                         _hva->Round(), pos));
        Is_Trace_cmd(Tracing(), opnd->Print(TFile));
        Is_Trace(Tracing(), (TFile, " to vor "));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " from BB%d to BB%d.\n",
                                    bb->Id(), succ->Id()));
      }
    }
  }

  // Exit_bb: when exit the bb, pop the vor phi result
  void Exit_bb(BB_NODE *bb) {
    Is_True(bb != NULL, ("invalid bb"));

    PHI_LIST* phi_list = Vsa()->Bb_vo_philist(bb);
    if (!phi_list)
      return;
    PHI_NODE     *phi;
    PHI_LIST_ITER phi_iter;
    FOR_ALL_ELEM (phi, phi_iter, Init(phi_list)) {
      VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      Is_True(vor == vor->Vsym_obj()->Top_of_stack(), ("stack mismatch"));
      vor->Vsym_obj()->Pop();
      Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor phi result ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in BB%d.\n", bb->Id()));
    }
  }

  // Process_stmt_fwd: forward process the stmtrep
  void Process_stmt_fwd(STMTREP *sr);

  // Process_stmt_rev: backward process the stmtrep
  void Process_stmt_rev(STMTREP* sr);

  // process rhs of the stmtrep
  template<OPERATOR opr, BOOL _FWD>
  void Process_sr(STMTREP* sr) {
    Is_True(sr->Lhs() == NULL, ("TODO: %s", OPERATOR_name(sr->Opr()) + 4));
    if (sr->Rhs())
      Process_coderep<void>(sr, sr->Rhs(), FALSE);
  }

  // process coderep
  template<CODEKIND kind>
  void Process_cr(STMTREP* sr, CODEREP* cr, UINT flag) { }

  // perform VSYM_OBJ_REP renaming phase
  void Perform() {
    DOM_WALKER<VSYM_OBJ_RENAMING> walker(_comp_unit->Cfg(), this);
    walker.Perform();
  }

  // initialize VSYM_OBJ_REP renaming phase
  void Initialize();

  // finalize VSYM_OBJ_REP renaming phase
  void Finalize();

};  // VSYM_OBJ_RENAMING

// Process_call<TRUE>: forward pass to rename vsym for call
template<> void
VSYM_OBJ_RENAMING::Process_call<TRUE>(STMTREP* sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));
  // vor mu and chi are handled in Process_stmt_fwd()
  // only rename the rhs
  Process_coderep<void>(sr, sr->Rhs(), FALSE);
}

// Process_call<FALSE>: reverse pass to rename vsym for call
template<> void
VSYM_OBJ_RENAMING::Process_call<FALSE>(STMTREP* sr)
{
  Is_True(sr && OPERATOR_is_call(sr->Opr()), ("invalid call sr"));
  // vor mu and chi are handled in Process_stmt_rev()
}

// Process_istore<TRUE>: forward pass to rename vsym for istore
template<> void
VSYM_OBJ_RENAMING::Process_istore<TRUE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (OPERATOR_is_scalar_istore(sr->Opr()) ||
           sr->Opr() == OPR_MSTORE), ("invalid istore sr"));

  // rename rhs
  if (handle_rhs) {
    CODEREP *rhs = sr->Rhs();
    Process_coderep<void>(sr, rhs, FALSE);
  }

  // rename lhs
  CODEREP *lhs = sr->Lhs();
  CODEREP *base = lhs->Istr_base();
  Process_coderep<void>(sr, base, FALSE);

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(lhs);
  if (vor == NULL) {
    // check base hor
    CODEREP *base_cr = Find_ilod_base(base);
    HEAP_OBJ_REP *hor = base_cr ? Vsa()->Find_cr_heap_obj(base_cr)
                                : Vsa()->Null_hor();
    if (Vsa()->Is_special_hor(hor)) {
      Vsa()->Enter_cr_vor_map(lhs, Vsa()->Null_vor());
      Is_Trace(Tracing(), (TFile, "VOR[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to IVAR cr%d:\n", lhs->Coderep_id()));
      Is_Trace_cmd(Tracing(), lhs->Print(2, TFile));
      return;
    }
    // not found vor, so base shouldn't have a hor, or the
    // hor is created just now
    VSYM_OBJ_REP *vor;
    Is_True(hor == NULL || _hva->Visit_heap_obj(hor) ||
            base_cr->Kind() == CK_VAR ||
            (base_cr->Kind() == CK_IVAR &&
             ((vor = Vsa()->Cr_2_vor(base_cr)) == NULL ||
              _hva->Visit_vsym_obj(vor))), ("no vor found"));
    // the sr should be visited next time
    _hva->Set_visit_next(sr);
  }
  else if (_hva->Visit_vsym_obj(vor)) {
    // TODO: if any doesn't overwrite the vor being visited, check prev vor
    //if (vor->Vsym_obj()->Fld_rep().Is_any())
    //  vor->Set_prev(vor->Vsym_obj()->Top_of_stack());
    _hva->Gen_name(vor, sr);
    Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    HEAP_OBJ_REP* rhs_hor = Vsa()->Find_stmt_hor_mu(sr, sr->Lhs());
    if (rhs_hor != NULL) {
      Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
      vor->Set_hor(rhs_hor);
    }
  }
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename vsym for ISTORE
template<> void
VSYM_OBJ_RENAMING::Process_istore<FALSE>(STMTREP* sr, BOOL handle_rhs)
{
  Is_True(sr &&
          (OPERATOR_is_scalar_istore(sr->Opr()) ||
           sr->Opr() == OPR_MSTORE), ("invalid istore sr"));

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL &&
      !Vsa()->Is_special_vor(vor) &&
      vor->Vsym_obj()->Top_match_sr(sr)) {
    vor->Vsym_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor istore lhs ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " from lhs cr%d of sr%d %s.\n",
                     sr->Lhs()->Coderep_id(), sr->Stmtrep_id(),
                     OPERATOR_name(sr->Opr()) + 4));
  }
}

// Process_stmt_fwd: forward process the stmtrep
void
VSYM_OBJ_RENAMING::Process_stmt_fwd(STMTREP* sr)
{
  // Process stmt vo mu list
  MU_LIST *mu_list = Vsa()->Stmt_vor_mu(sr);
  if (mu_list) {
    MU_NODE     *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE (mnode, mu_iter, Init(Vsa()->Stmt_vor_mu(sr))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_OBJ_REP *vor = cvor->first;
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      VSYM_OBJ_REP *tos = vor->Vsym_obj()->Top_of_stack();
      if (vor != tos) {
        cvor->first = tos;
        Is_Trace(Tracing(), (TFile, "VOR[%d]: set mu opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  // Process stmt
  // if stmt is marked to be visited, visit the stmt include the rhs. otherwise
  // only visit istore/mstore because it may generate new version for VO
  Process_stmtrep<TRUE>(sr);
  if ((OPERATOR_is_scalar_istore(sr->Opr()) ||
       sr->Opr() == OPR_MSTORE) && !_hva->Visit_stmt(sr))
    Process_istore<TRUE>(sr, FALSE);

  // Process stmt vo chi list
  CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
  if (chi_list) {
    CHI_NODE     *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE (cnode, chi_iter, Init(chi_list)) {
      CVOR* cvor = (CVOR*)cnode->OPND();
      VSYM_OBJ_REP* vor = cvor->first;
      if (!_hva->Visit_vsym_obj(vor))
        continue;
      VSYM_OBJ_REP* tos = vor->Vsym_obj()->Top_of_stack();
      if (vor != tos) {
        cvor->first = tos;
        Is_Trace(Tracing(), (TFile, "VOR[%d]: set chi opnd ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), tos->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
      cvor = (CVOR*)cnode->RESULT();
      vor = cvor->first;
      IDTYPE version = _hva->Gen_name(vor, sr);

      // set vor chi rhs hor
      VSYM_OBJ_REP *bind_vor = Vsa()->Cr_2_vor(cvor->second);
      if (bind_vor && (bind_vor->Vsym_obj() == tos->Vsym_obj())) {
        HEAP_OBJ_REP *hor = Vsa()->Cr_2_heap_obj(cvor->second);
        if (hor) {
          CHI_NODE *hor_chi = Vsa()->Find_stmt_hor_chi(sr, hor->Heap_obj());
          HEAP_OBJ_REP* hor_mu = Vsa()->Find_stmt_hor_mu(sr, cvor->second);
          HEAP_OBJ_REP *rhs_hor = hor_chi ? ((CHOR*)hor_chi->RESULT())->first : hor_mu;
          if (rhs_hor) {
            vor->Set_hor(rhs_hor);
          }
        }
      }

      Is_Trace(Tracing(), (TFile, "VOR[%d]: push chi result ",  _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                           sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    }
  }
}

// Process_stmt_rev: backward process the stmtrep
void
VSYM_OBJ_RENAMING::Process_stmt_rev(STMTREP* sr)
{
  // Process stmt vo chi list
  CHI_LIST *chi_list = Vsa()->Stmt_vor_chi(sr);
  if (chi_list) {
    CHI_NODE     *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE (cnode, chi_iter, Init(Vsa()->Stmt_vor_chi(sr))) {
      CVOR* cvor = (CVOR*)cnode->RESULT();
      VSYM_OBJ_REP* vor = cvor->first;
      Is_True(!Vsa()->Is_special_vor(vor), ("TODO: special vor"));
      if (vor->Vsym_obj()->Top_match_sr(sr)) {
        vor->Vsym_obj()->Pop();
        Is_Trace(Tracing(), (TFile, "VOR[%d]: pop chi result ",  _hva->Round()));
        Is_Trace_cmd(Tracing(), vor->Print(TFile));
        Is_Trace(Tracing(), (TFile, " in sr%d %s.\n",
                             sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
      }
    }
  }

  // Process stmt
  // if stmt is marked to be visited, visit the stmt include the rhs. otherwise
  // only visit istore/mstore because it may generate new version for VO
  Process_stmtrep<FALSE>(sr);
  if ((OPERATOR_is_scalar_istore(sr->Opr()) ||
       sr->Opr() == OPR_MSTORE) && !_hva->Visit_stmt(sr))
    Process_istore<FALSE>(sr, FALSE);

  // nothing to do with stmt vor mu
}

// Process_cr<CK_VAR>: rename vsym on IVAR
template<> void
VSYM_OBJ_RENAMING::Process_cr<CK_IVAR>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_IVAR, ("invalid ivar cr"));

  if (cr->Opr() == OPR_PARM) {
    return Process_coderep<void>(sr, cr->Ilod_base(), flag);
  }

  CODEREP* base = (cr == sr->Lhs()) ? cr->Istr_base() : cr->Ilod_base();
  Process_coderep<void>(sr, base, flag);

  if (cr->Opr() == OPR_MLOAD)
    Process_coderep<void>(sr, cr->Mload_size(), flag);

  // rename VOR for ICALL vptr/vtable?
  // if we don't need vor for ICALL vptr/vtable, the last kid of ICALL shouldn't visit
  //if (!Vsa()->Is_ivar_need_vsym(cr, sr))
  //  return;

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(cr);
  if (vor == NULL) {
    // check base hor
    CODEREP *base_cr = Find_ilod_base(base);
    HEAP_OBJ_REP *hor = base_cr ? Vsa()->Find_cr_heap_obj(base_cr)
                                : Vsa()->Null_hor();
    if (Vsa()->Is_special_hor(hor)) {
      Vsa()->Enter_cr_vor_map(cr, Vsa()->Null_vor());
      Is_Trace(Tracing(), (TFile, "VOR[%d]: Create vor ", _hva->Round()));
      Is_Trace_cmd(Tracing(), vor->Print(TFile));
      Is_Trace(Tracing(), (TFile, " on hor "));
      Is_Trace_cmd(Tracing(), hor->Print(TFile));
      Is_Trace(Tracing(),
               (TFile, " to IVAR cr%d:\n", cr->Coderep_id()));
      Is_Trace_cmd(Tracing(), cr->Print(2, TFile));
      return;
    }
    // not found vor, so base shouldn't have a hor, or the
    // hor is created just now
    VSYM_OBJ_REP *vor;
    Is_True(hor == NULL || _hva->Visit_heap_obj(hor) ||
            (base_cr->Kind() == CK_IVAR &&
             ((vor = Vsa()->Cr_2_vor(base_cr)) == NULL ||
              _hva->Visit_vsym_obj(vor))), ("no vor found"));
  }
  else if (_hva->Visit_vsym_obj(vor)) {
    VSYM_OBJ_REP *cur_vor = vor->Vsym_obj()->Top_of_stack();
    if (vor != cur_vor) {
      Vsa()->Enter_cr_vor_map(cr, cur_vor);
      if (cur_vor->Hor())
        Vsa()->Enter_cr_heap_obj_map(cr, cur_vor->Hor(), TRUE);
    }
  }
}

// Process_cr<CK_OP>: rename vsym on OP
template<> void
VSYM_OBJ_RENAMING::Process_cr<CK_OP>(STMTREP* sr, CODEREP* cr, UINT flag)
{
  Is_True(sr && cr && cr->Kind() == CK_OP, ("invalid op cr"));

  for (INT32 i = 0; i < cr->Kid_count(); ++i) {
    Process_coderep<void>(sr, cr->Opnd(i), flag);
  }
}

// Process_sr<OPR_STID, TRUE>: forward pass to rename vsym for STID
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_STID, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  // rename rhs
  Process_coderep<void>(sr, sr->Rhs(), 0);

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL && _hva->Visit_vsym_obj(vor)) {
    _hva->Gen_name(vor, sr);
    Is_Trace(Tracing(), (TFile, "VOR[%d]: push vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
    HEAP_OBJ_REP* hor = Vsa()->Find_stmt_hor_mu(sr, sr->Lhs());
    if (hor != NULL) {
      Is_True(!Vsa()->Is_special_vor(vor), ("set hor on special vor"));
      vor->Set_hor(hor);
    }
  }
}

// Process_sr<OPR_STID, FALSE>: reverse pass to rename vsym for STID
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_STID, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_STID, ("invalid stid sr"));

  VSYM_OBJ_REP* vor = Vsa()->Cr_2_vor(sr->Lhs());
  if (vor != NULL &&
      _hva->Visit_vsym_obj(vor) &&
      vor->Vsym_obj()->Top_match_sr(sr)) {
    vor->Vsym_obj()->Pop();
    Is_Trace(Tracing(), (TFile, "VOR[%d]: pop vor ", _hva->Round()));
    Is_Trace_cmd(Tracing(), vor->Print(TFile));
    Is_Trace(Tracing(),
             (TFile, " to sr%d %s.\n",
                     sr->Stmtrep_id(), OPERATOR_name(sr->Opr()) + 4));
  }
}

// Process_sr<OPR_ISTORE, TRUE>: forward pass to rename vsym for ISTORE
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_ISTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_ISTORE, FALSE>: reverse pass to rename vsym for ISTORE
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_ISTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_ISTORE, ("invalid istore sr"));
  Process_istore<FALSE>(sr, FALSE);
}

// Process_sr<OPR_MSTORE, TRUE>: forward pass to rename vsym for MSTORE
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_MSTORE, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  // process mstore size
  Process_coderep<void>(sr, sr->Lhs()->Mstore_size(), FALSE);
  // process istore
  Process_istore<TRUE>(sr, TRUE);
}

// Process_sr<OPR_MSTORE, FALSE>: reverse pass to rename vsym for MSTORE
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_MSTORE, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_MSTORE, ("invalid mstore sr"));
  Process_istore<FALSE>(sr, FALSE);
}

// Process_sr<OPR_OPT_CHI, TRUE>: forward pass to rename vsym for OPT_CHI
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_OPT_CHI, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Do nothing, already handled in Process_stmtrep<TRUE>() for mu list
  //Vsa()->Rename_entry_vsym_chi(sr, Defbb_pool(), TRUE);
}

// Process_sr<OPR_OPT_CHI, FALSE>: reverse pass to rename vsym for OPT_CHI
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_OPT_CHI, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_OPT_CHI, ("invalid entry_chi sr"));
  // Do nothing, already handled in Process_stmtrep<TRUE>() for mu list
  //Vsa()->Rename_entry_vsym_chi(sr, Defbb_pool(), FALSE);
}

// Process_sr<OPR_CALL, TRUE>: forward pass to rename vsym for CALL
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_CALL, FALSE>: reverse pass to rename vsym for CALL
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr->Opr() == OPR_CALL || sr->Opr() == OPR_ICALL, ("not a call"));
  Process_call<FALSE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, TRUE>: forward pass to rename vsym for intrn_call
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_INTRINSIC_CALL, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn_call sr"));
  Process_call<TRUE>(sr);
}

// Process_sr<OPR_INTRINSIC_CALL, FALSE>: reverse pass to rename vsym for intrn_call
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_INTRINSIC_CALL, FALSE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_INTRINSIC_CALL, ("invalid intrn_call sr"));
  Process_call<FALSE>(sr);
}

// Process_sr<OPR_RETURN, TRUE>: forward pass to rename vsym for RETURN
template<> void
VSYM_OBJ_RENAMING::Process_sr<OPR_RETURN, TRUE>(STMTREP* sr)
{
  Is_True(sr && sr->Opr() == OPR_RETURN, ("invalid return sr"));
  // Do nothing, already handled in Process_stmtrep<TRUE>() for mu list
}

// initialize VSYM_OBJ_REP renaming phase
void
VSYM_OBJ_RENAMING::Initialize() {
  // place vsym_obj phi
  VSA::PHILIST_MAP *bb_ro_philist = _vsa->Bb_vo_philist();
  VSYM_OBJ         *vsym_obj;
  VSYM_OBJ_REP     *vor;
  HASH_SET_ITER<VO_PTR_SET, VSYM_OBJ*> iter;
  if (_hva->Has_vo_updated()) {
    // insert or update phi for heap_obj updated
    _vsa->Place_ro_phi_node(bb_ro_philist, vsym_obj, &_hva->Vo_updated(), &iter, vor, TRUE, _hva->Vo_phi_cache());
  }
  if (_hva->Has_vo_created()) {
    // insert phi for vsym_obj created
    // has to set last param TRUE here because for VO created for horphi, the
    // phi node has been added
    _vsa->Place_ro_phi_node(bb_ro_philist, vsym_obj, &_hva->Vo_created(), &iter, vor, TRUE, _hva->Vo_phi_cache());
  }
}

void
VSYM_OBJ_RENAMING::Finalize() {
  // verify renaming stack
#ifdef Is_True_On
  Vsa()->Verify_heap_obj_stack();
#endif
  Is_Trace(Tracing(), (TFile, "VOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace(Tracing(), (TFile, "VOR[%d]: FINISH VSYM_OBJ_RENAMING.\n", _hva->Round()));
  //Is_Trace_cmd(Tracing(), Ipsa()->Print_fld_name_map(TFile));
  //Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
  Is_Trace(Tracing(), (TFile, "VOR[%d]: ---------------------------------------------------------------\n", _hva->Round()));
  Is_Trace_cmd(Tracing(), Vsa()->Print_hor(TFile));
}


// ============================================================================
// HEAP_VSYM_ANALYSIS::Perform
// main driver for HEAP_VSYM_ANALYSIS::Perform
// ============================================================================
void
HEAP_VSYM_ANALYSIS::Perform(INT max_iter) {
  do {
    // begin iteration
    Begin_iteration();

    // create heap_obj
    Perform_single_step<HEAP_OBJ_CREATION>();
    // rename heap_obj_rep
    Perform_single_step<HEAP_OBJ_RENAMING>();
    // create vsym_obj
    Perform_single_step<VSYM_OBJ_CREATION>();
    // rename vsym_obj_rep
    Perform_single_step<VSYM_OBJ_RENAMING>();

    // end iteration
    End_iteration();

    if (!Need_next_round())
      break;

  } while (Incr_round() <= max_iter);

#ifdef Is_True_On
  Verify();
#endif
  VSA_STATS_inc_n(hva_round, Round());
  Is_True(Round() <= max_iter, ("infinite loop?"));
}

// =============================================================================
// VSA::Search_phi_node
// Search phi_node which defines the heap_obj from the phi list
// =============================================================================
PHI_NODE *
VSA::Search_phi_node(PHI_LIST *list, HEAP_OBJ *ho) {
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM (phi, phi_iter, Init(list)) {
    Is_True(VSA_PHI_NODE(phi).Res_is_hor(), ("phi is not hor"));
    HEAP_OBJ_REP *hor = (HEAP_OBJ_REP*)phi->RESULT();
    if (hor->Heap_obj() == ho)
      return phi;
  }
  return NULL;
}

// =============================================================================
// VSA::Search_phi_node
// Search phi_node which defines the vsym_obj from the phi list
// =============================================================================
PHI_NODE *
VSA::Search_phi_node(PHI_LIST *list, VSYM_OBJ *vo) {
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM (phi, phi_iter, Init(list)) {
    Is_True(VSA_PHI_NODE(phi).Res_is_vor(), ("phi is not vor"));
    VSYM_OBJ_REP *vor = (VSYM_OBJ_REP*)phi->RESULT();
    if (vor->Vsym_obj() == vo)
      return phi;
  }
  return NULL;
}


// =============================================================================
// Find_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ *ho)
// Find the current HEAP_OBJ_REP for ho from stmt hor mu list
// =============================================================================
MU_NODE *
VSA::Find_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ *ho)
{
  Is_True(stmt->Stmtrep_id() != 0, ("invalid stmtrep id"));
  MU_LIST *mu_list = Stmt_hor_mu(stmt);
  if (mu_list == NULL)
    return NULL;

  MU_LIST_ITER mu_iter;
  MU_NODE *mu;
  FOR_ALL_NODE(mu, mu_iter, Init(mu_list)) {
    CHOR *hor = (CHOR *)mu->OPND();
    if (hor->first->Heap_obj() == ho)
      return mu;
  }
  return NULL;
}

// =============================================================================
// Find_stmt_hor(STMTREP *stmt, CODEREP *cr)
// Find the current HEAP_OBJ_REP for ho annotated on cr from stmt hor mu list
// =============================================================================
HEAP_OBJ_REP *
VSA::Find_stmt_hor_mu(STMTREP *stmt, CODEREP *cr)
{
  HEAP_OBJ_REP *hor = Find_cr_heap_obj(cr);
  if (hor == NULL)
    return NULL;

  MU_NODE *mu = Find_stmt_hor_mu(stmt, hor->Heap_obj());
  return mu ? ((CHOR*)mu->OPND())->first : hor;
}

CODEREP *
VSA::Find_hor_chi_cr(STMTREP *stmt, HEAP_OBJ_REP *hor) const
{
  CHI_LIST *chi_list = Stmt_hor_chi(stmt);
  if (chi_list && !chi_list->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
      CHOR *cres = (CHOR*)cnode->RESULT();
      CHOR *copnd = (CHOR*)cnode->OPND();
      Is_True(cres->second == copnd->second, ("chor cr mismatch"));
      if (cres->first == hor || copnd->first == hor)
        return cres->second;
    }
  }
  return NULL;
}

// =============================================================================
// Append_stmt_hor_mu
// Create new mu node for hor and append to stmt hor mu list
// =============================================================================
void
VSA::Append_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr)
{
  Is_True(!Is_special_hor(hor), ("no chi for special hor"));
  Is_True(stmt->Stmtrep_id() != 0, ("invalid stmtrep id"));
  MU_LIST *mu_list = Stmt_hor_mu(stmt);
  if (mu_list == NULL) {
    mu_list = CXX_NEW(MU_LIST, Mem_pool());
    Enter_stmt_hor_mu_map(stmt, mu_list);
  }
  MU_NODE *mu = CXX_NEW(MU_NODE, Mem_pool());
  CHOR *chor = CXX_NEW(CHOR(hor, cr), Mem_pool());
  mu->Set_OPND((CODEREP*)chor, FALSE);
  mu_list->Append(mu);

  VSA_STATS_inc(ho_mu);
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, "Create_stmt_hor_mu creates "));
  Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               hor->Print(TFile));
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, " on sr%d\n", stmt->Stmtrep_id()));
}

// =============================================================================
// Update_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ_REP *hor, BOOL insert)
// Update mu opnd to hor. If not found and insert is TRUE, append the hor
// =============================================================================
void
VSA::Update_stmt_hor_mu(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr, BOOL append)
{
  MU_NODE *mu = Find_stmt_hor_mu(stmt, hor->Heap_obj());
  if (append && !mu) {
    Append_stmt_hor_mu(stmt, hor, cr);
    return;
  }
  Is_True_Ret(mu != NULL, ("Update_stmt_hor_mu not find mu"));
  CHOR *opnd = (CHOR*)mu->OPND();
  Is_True(opnd->first->Heap_obj() == hor->Heap_obj(), ("mu opnd mismatch"));
  if (opnd->first != hor) {
    opnd->first = hor;
    Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
             (TFile, "Update_stmt_hor_mu updates "));
    Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
                 hor->Print(TFile));
    Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
             (TFile, " on sr%d\n", stmt->Stmtrep_id()));
  }
}

// =============================================================================
// Find_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ *ho)
// Find the chi node for given heap obj in stmt hor chi list
// =============================================================================
CHI_NODE*
VSA::Find_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ *ho)
{
  Is_True(stmt->Stmtrep_id() != 0, ("invalid stmtrep id"));
  CHI_LIST *chi_list = Stmt_hor_chi(stmt);
  if (chi_list == NULL)
    return NULL;

  CHI_LIST_ITER chi_iter;
  CHI_NODE *chi;
  FOR_ALL_NODE(chi, chi_iter, Init(chi_list)) {
    CHOR *opnd = (CHOR *)chi->OPND();
    CHOR *res = (CHOR*)chi->RESULT();
    Is_True(opnd->first->Heap_obj() == res->first->Heap_obj(),
            ("Heap obj mismatch"));
    if (opnd->first->Heap_obj() == ho)
      return chi;
  }
  return NULL;
}

// =============================================================================
// Find_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ *ho)
// Find the chi node for given heap obj in stmt hor chi list
// =============================================================================
HEAP_OBJ_REP*
VSA::Find_stmt_hor_chi(STMTREP *stmt, CODEREP *cr)
{
  HEAP_OBJ_REP *hor = Cr_2_heap_obj(cr);
  if (hor == NULL)
    return NULL;

  CHI_NODE *chi = Find_stmt_hor_chi(stmt, hor->Heap_obj());
  return chi ? ((CHOR*)chi->RESULT())->first : hor;
}

// =============================================================================
// Append_stmt_hor_chi
// Create new hor chi and add to stmt hor chi list
// =============================================================================
void
VSA::Append_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *res,
                         HEAP_OBJ_REP *opnd, CODEREP *cr)
{
  Is_True(!Is_special_hor(opnd), ("no chi for special hor"));
  Is_True(stmt->Stmtrep_id() != 0, ("invalid stmtrep id"));
  Is_True(res->Stmt_def() == stmt, ("invalid stmtdef"));

  CHI_LIST *chi_list = Stmt_hor_chi(stmt);
  if (chi_list == NULL) {
    chi_list = CXX_NEW(CHI_LIST, Mem_pool());
    Enter_stmt_hor_chi_map(stmt, chi_list);
  }

  CHI_NODE *chi = CXX_NEW(CHI_NODE, Mem_pool());
  CHOR *copnd = CXX_NEW(CHOR(opnd, cr), Mem_pool());
  chi->Set_OPND((CODEREP *)copnd, FALSE);
  CHOR *cres = CXX_NEW(CHOR(res, cr), Mem_pool());
  chi->Set_RESULT((CODEREP *)cres);
  chi_list->Append(chi);

  VSA_STATS_inc(ho_chi);
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, "Append_stmt_hor_chi creates "));
  Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               res->Print(TFile));
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, "<-"));
  Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               opnd->Print(TFile));
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, " on sr%d\n", stmt->Stmtrep_id()));
}

// =============================================================================
// Append_stmt_hor_chi
// Create new hor chi and add to stmt hor chi list
// =============================================================================
HEAP_OBJ_REP *
VSA::Append_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr,
                         ROR_ATTR attr, MEM_POOL *def_bbs_pool)
{
  Is_True(!Is_special_hor(hor), ("no chi for special hor"));
  HEAP_OBJ_REP *res = Clone_heap_obj(hor, stmt->Bb(), def_bbs_pool);
  res->Set_attr(attr);
  res->Set_stmt_def(stmt, Comp_unit()->Dna());
  if (hor->Injured())
    res->Set_injured();
  Append_stmt_hor_chi(stmt, res, hor, cr);
  return res;
}

// =============================================================================
// Update_stmt_hor_chi
// Update result and opnd for chi in stmt hor chi list
// =============================================================================
HEAP_OBJ_REP *
VSA::Update_stmt_hor_chi(STMTREP *stmt, HEAP_OBJ_REP *hor, CODEREP *cr)
{
  Is_True(stmt->Stmtrep_id() != 0, ("invalid stmtrep id"));
  CHI_NODE *chi = Find_stmt_hor_chi(stmt, hor->Heap_obj());
  Is_True_Ret(chi != NULL, ("Update_stmt_hor_chi not find chi"), NULL);
  CHOR *opnd = (CHOR*)chi->OPND();
  Is_True(opnd->first->Heap_obj() == hor->Heap_obj() &&
          opnd->second == cr, ("chi opnd mismatch"));
  if (opnd->first != hor)
    opnd->first = hor;
  CHOR *res = (CHOR *)chi->RESULT();
  HEAP_OBJ_REP *rhor = res->first;
  Is_True(rhor && rhor != hor && rhor->Heap_obj() == hor->Heap_obj(),
          ("Heap_obj mismatch"));
  rhor->Gen_name(stmt);
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, "Update_stmt_hor_chi updates "));
  Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               rhor->Print(TFile));
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, "<-"));
  Is_Trace_cmd(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
               hor->Print(TFile));
  Is_Trace(Tracing() || Get_Trace(TP_VSA, VSA_VSYM_TRACE_FLAG),
           (TFile, " on sr%d\n", stmt->Stmtrep_id()));
  return rhor;
}

// =============================================================================
// Perform_heap_vsym_analysis(CFG* cfg, MEM_POOL* def_bb_pool, INT max_iter)
// Perform iterative heap vsym analysis
// =============================================================================
void
VSA::Perform_heap_vsym_analysis(CFG* cfg, MEM_POOL* def_bb_pool, INT max_iter)
{
  Is_True(this->Comp_unit()->Cfg() == cfg, ("wrong cfg"));
  BOOL trace = Get_Trace(TP_VSA, VSA_VSYM_DUMP_FLAG);
  Is_Trace(trace,
           (TFile, "============================================================\n"));
  Is_Trace(trace,
           (TFile, "CFG dump before HEAP VSYM ANALYSIS for %s.\n",
                   Dna()->Fname()));
  Is_Trace_cmd(trace, cfg->Print(TFile));
  Is_Trace(trace,
           (TFile, "============================================================\n"));

  HEAP_VSYM_ANALYSIS hv(this->Comp_unit(), def_bb_pool);
  if (VSA_HVA_Delay_Ho_Rename) {
    hv.Perform_with_delayed_ho_renaming(max_iter);
  }
  else {
    hv.Perform(max_iter);
  }

  Is_Trace(trace,
           (TFile, "============================================================\n"));
  Is_Trace(trace,
           (TFile, "CFG dump after HEAP VSYM ANALYSIS for %s.\n",
                   Dna()->Fname()));
  Is_Trace_cmd(trace, Print_hor(TFile));
  Is_Trace(trace,
           (TFile, "============================================================\n"));
  Is_Trace(trace,
           (TFile, "HEAP OBJECT dump after HEAP VSYM ANALYSIS for %s.\n",
                   Dna()->Fname()));
  Is_Trace_cmd(trace, Print_ho_list(TFile));
  Is_Trace(trace,
           (TFile, "============================================================\n"));
  Is_Trace(trace,
           (TFile, "VSYM OBJECT dump after HEAP VSYM ANALYSIS for %s.\n",
                   Dna()->Fname()));
  Is_Trace_cmd(trace, Print_vo_list(TFile));
  Is_Trace(trace,
           (TFile, "============================================================\n"));
}

