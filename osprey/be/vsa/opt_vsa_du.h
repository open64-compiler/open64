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
// opt_vsa_du.h
//
// interface for VSA D-U chain
// ==================================================================
#ifndef opt_vsa_du_INCLUDED
#define opt_vsa_du_INCLUDED

// ==================================================================
// D-U chain for VSA
// Create and manage VSA D-U info
// ==================================================================

#include "defs.h"
#include "vsa_defs.h"
#include "block_allocator.h"
#include <ext/hash_map>

// ==================================================================
// instead of the traditional D-U chain represented by linked list,
// we represent D-U chain in a tree.
// for uses without control dependency, they are connected by next
// chain. for uses with control dependency, they are connected by
// child chain.
// 
// S1: sym1v1 <- ...
// S2: if (sym1v1 ...) {
// S3:   <-- sym1v1
// S4:   <-- sym1v1
//     }
//     else {
// S5:   <-- sym1v1
// S6:   <-- sym1v1
//     }
// S7:   <-- sym1v1
// 
//    next     next
// S1 ----> S2 ----> S8
//          | child      next
//          +------> S3 ----> S4
//          | child      next
//          +------> S5 ----> S6
//
// to simplify the tree storage, we convert it into a binary tree:
// left child is the next chain and right child is the child chain.
//          S1
//         /
//        S2
//       /  \
//      S8   S3
//          /  \
//         S4   S5
//             /
//            S6

// VSA_DU_MEM_UNIT
// VSA_Du_Mem_High and VSA_Du_Mem_Low is MB
#define VSA_DU_MEM_UNIT (1024 * 1024)

// DU_INFO_CONTAINER
// Use the container to save D-U info
typedef BLOCK_ALLOCATOR<4096, 16> DU_INFO_CONTAINER;

// USE_KIND
// the kind of stmt/phi where the var/vor is used
enum USE_KIND {
  USE_BY_PHI   = 0,        // the var/vor is used in phi
  USE_BY_STMT  = 1,        // the var/vor is used in stmt
  USE_BY_CHI   = 2,        // the var/vor is used in chi
  USE_BY_MU    = 4,        // the var/vor is used in mu
  USE_KIND_MAX = 7,        // max 7 USE_KIND: STMT | CHI | MU
};

// USE_LOCATION
// the detailed location where the var/vor is used
enum USE_LOCATION {
  USE_NONE    = 0x00,      // the var/vor is not used here
  USE_IN_NODE = 0x01,      // the var/vor is used in this node
  USE_IN_IVAR = 0x02,      // the var/vor is used in ivar base
  USE_IN_DIV  = 0x04,      // the var/vor is used in divisor
};

// USE_KIND_BITS, USE_KIND_MASK
enum {
  USE_KIND_BITS = 3,      // low 3 bit in _use is for USE_KIND
  USE_KIND_MASK = 0x7,    // USE_KIND kind = (_use & USE_KIND_MASK);
  USE_PTR_MASK = ~(7ULL), // CODEREP* ptr = (_use & USE_PTR_MASK);
};

// NULL_ID
// invalid D-U index
enum {
  NULL_ID = DU_INFO_CONTAINER::IDX_INVALID
};

// DU_INFO
// representation for D-U info
class DU_INFO {
  friend class DNA_DU_INFO;

private:
  uintptr_t _use;           // STMTREP* or PHI_NODE* with lower 2 bits for USE_KIND
  UINT32    _next    : 29;  // index to next use in the same level
  UINT32    _lhs_use :  3;  // how var/vor is used in lhs/result
  UINT32    _child   : 29;  // index to child use in the next level with more control flow
  UINT32    _rhs_use :  3;  // how var/vor is used in rhs/opnd

  // get the pointer to use stmt/phi
  template<typename T>
  T* Get_use() const {
    Is_True(!Is_null(), ("invalid use"));
    const uintptr_t mask = ((((uintptr_t)-1) >> USE_KIND_BITS) << USE_KIND_BITS);
    return (T*)(_use & mask);
  }

public:
  // constructor
  DU_INFO() : _use(0),_next(NULL_ID), _lhs_use(USE_NONE),
              _child(NULL_ID), _rhs_use(USE_NONE) { }

  // check if the DU_INFO is null
  BOOL Is_null() const {
    return _use == 0;
  }

  // get USE_KIND
  USE_KIND Kind() const {
    Is_True(!Is_null(), ("invalid use"));
    return (USE_KIND)(_use & USE_KIND_MASK);
  }

  // get STMTREP* where the var/vor is used
  STMTREP *Get_stmt() const {
    Is_True(Kind() != USE_BY_PHI,
            ("invalid kind"));
    return Get_use<STMTREP>();
  }

  // get PHI_NODE where the var/vor is used as phi opnd
  PHI_NODE *Get_phi() const {
    Is_True(Kind() == USE_BY_PHI,
            ("invalid kind"));
    return Get_use<PHI_NODE>();
  }

  // get BB where the USE is in
  BB_NODE *Bb() const {
    Is_True(!Is_null(), ("invalid use"));
    if (Kind() == USE_BY_PHI) {
      return Get_phi()->Bb();
    }
    else {
      return Get_stmt()->Bb();
    }
  }

  // get next use in the same level without control dependency
  UINT32 Next() const {
    return _next;
  }

  // get child use in sub level which have control dependency
  UINT32 Child() const {
    return _child;
  }

  // set the use info for STMTREP
  void Set_use(STMTREP* sr, USE_KIND k) {
    Is_True(k != USE_BY_PHI,
            ("def by phi"));
    Is_True(((uintptr_t)sr & USE_KIND_MASK) == 0,
            ("sr not aligned"));
    _use = (uintptr_t)sr | k;
  }

  // set the use info for PHI_NODE
  void Set_use(PHI_NODE* phi) {
    Is_True(((uintptr_t)phi & USE_KIND_MASK) == 0,
            ("phi not aligned"));
    _use = (uintptr_t)phi | USE_BY_PHI;
  }

  // set next use
  void Set_next(UINT32 next) {
    _next = next;
  }

  // set child use
  void Set_child(UINT32 child) {
    _child = child;
  }

  // dump use info without next/child
  void Dump_use(FILE* fp) const;

  // dump while use info with next/child
  void Dump(FILE* fp) const;
};  // DU_INFO

// DNA_DU_INFO
// manage D-U for a function
class DNA_DU_INFO {
  friend class IPSA_DU_MANAGER;
  friend class DU_INFO_ITER;

private:
  DNA_DU_INFO      *_prev;       // pointer to previous DNA DU
  DNA_DU_INFO      *_next;       // pointer to next DNA DU
  DU_INFO_CONTAINER _du_info;    // DU container
  COMP_UNIT        *_comp_unit;  // pointer to comp_unit

  DNA_DU_INFO(const DNA_DU_INFO&);            // disable copy ctor
  DNA_DU_INFO& operator=(const DNA_DU_INFO&); // disable assignment

public:
  // get DU_INFO entry for given D-U index
  const DU_INFO* Get_use(UINT32 idx) const {
    return _du_info.Get_ptr<DU_INFO>(idx);
  }

  // get function index for this D-U
  UINT32 Dna_idx() const {
    return _comp_unit->Dna()->Dna_idx();
  }

  // get function name for this D-U
  const char *Fname() const {
    return _comp_unit->Dna()->Fname();
  }

private:
  // constructor
  DNA_DU_INFO(BALLOC_STAT *stat, COMP_UNIT *cu)
    : _du_info(stat), _prev(NULL), _next(NULL), _comp_unit(cu) { }

  // lock the D-U buffer to avoid it's GC'ed
  void   Lock()             { _du_info.Lock();              }

  // unlock the D-U buffer so that it can be GC'ed
  void   Unlock()           { _du_info.Unlock();            }

  // check if D-U buffer is locked
  BOOL   Is_locked() const  { return _du_info.Is_locked();  }

  // total memory used by D-U for this function
  UINT64 Total_size() const { return _du_info.Total_size(); }

  // build D-U from IR
  void   Build();

  // dump given du entry
  void   Dump(FILE *fp, UINT32 du, UINT32 level) const;

  // dump all du entries
  void   Dump(FILE *fp) const;
};  // DNA_DU_INFO

// IPSA_DU_MANAGER
// manager to manage all DNA_DU_INFO instances
class IPSA_DU_MANAGER : BALLOC_STAT {
  typedef __gnu_cxx::hash_map<UINT32, DNA_DU_INFO*> DNA_DU_MAP;

private:
  DNA_DU_MAP  _map;       // dna_idx -> DNA_DU_INFO map
  DNA_DU_INFO *_head;     // circular link list for GC

  IPSA_DU_MANAGER(const IPSA_DU_MANAGER&);             // disable copy ctor
  IPSA_DU_MANAGER &operator=(const IPSA_DU_MANAGER&);  // disable assignment

  // unique instance
  static IPSA_DU_MANAGER _du_mgr;

private:
  // constructor
  IPSA_DU_MANAGER() : _head(NULL) { }

  // check memory usage and do GC if necessary
  void Check_watermark();

  // move du to circular list head
  void Move_to_head(DNA_DU_INFO *du) {
    if (_head == NULL) {
      Is_True(du->_prev == NULL && du->_next == NULL, ("dlist corrupted"));
      // set _head and form the circle
      _head = du;
      _head->_prev = _head;
      _head->_next = _head;
    }
    else if (_head != du) {
      Is_True(_head->_prev != NULL && _head->_next != NULL, ("dlist corrupted"));
      if (du->_next != NULL) {
        // remove du from link list
        Is_True(du->_prev != NULL, ("dlist corrupted"));
        du->_prev->_next = du->_next;
        du->_next->_prev = du->_prev;
      }
      // move original head to du's next
      du->_next = _head;
      du->_prev = _head->_prev;
      _head->_prev = du;
      // set new du
      _head = du;
    }
  }

private:
  //get du_info for dna. either get from cache or create a new one
  DNA_DU_INFO *Get_du(DNA_NODE *dna) {
    // check cache
    DNA_DU_INFO* &du = _map[dna->Dna_idx()];
    if (du == NULL) {
      // not cached, create a new one
      du = new DNA_DU_INFO(this, dna->Comp_unit());
      Is_True(du != NULL, ("du oom?"));
      // build du from IR
      du->Build();
      Is_Trace(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG),
               (TFile, "----DU MGR create DU %s: size=%lld total=%lld\n",
                dna->Fname(), du->Total_size(), Total_size()));
      // free unused D-U info if D-U memory reaches high watermark
      if (_head && Total_size() >= VSA_Du_Mem_High * VSA_DU_MEM_UNIT)
        Check_watermark();
    }
    // move this to circular list head so it's freed late (LRU)
    Move_to_head(du);
    Is_Trace(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG),
             (TFile, "----DU MGR lock DU %s: size=%lld total=%lld\n",
              dna->Fname(), du->Total_size(), Total_size()));
    // lock the buffer
    du->Lock();
    return du;
  }

  // release the du_info. unlock the buffer and cache it
  void Release_du(DNA_DU_INFO *du) {
    Is_True(du && du->Is_locked(), ("invalid du"));
    Is_True(_map[du->Dna_idx()] == du, ("invalid du"));
    // unlock the buffer
    du->Unlock();
    Is_Trace(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG),
             (TFile, "----DU MGR unlock DU %s: size=%lld total=%lld\n",
              du->Fname(), du->Total_size(), Total_size()));
  }

  // delete the du_info and not cache it any more
  void Delete_du(DNA_DU_INFO *du) {
    Is_True(du && du->Is_locked(), ("invalid du"));
    Is_True(_map[du->Dna_idx()] == du, ("invalid du"));
    // unlock the buffer
    du->Unlock();
    // remove from circular list
    if (du == _head) {
      _head = du->_next;
      if (du == _head) {
        // only 1 item in the list
        _head = NULL;
        du->_next = NULL;
      }
    }
    if (du->_next) {
      Is_True(du->_prev, ("du list corrupted?"));
      du->_next->_prev = du->_prev;
      du->_prev->_next = du->_next;
    }
    // remove from map
    _map.erase(du->Dna_idx());
    // for tracing
    const char *fname = du->Fname();
    UINT64 size = du->Total_size();
    // delete the D-U object
    delete du;
    Is_Trace(Get_Trace(TP_VSA, VSA_DU_MGR_FLAG),
             (TFile, "----DU MGR delete DU %s: size=%lld total=%lld\n",
              fname, size, Total_size()));
  }

  // dump all existing du_info managed
  void Dump(FILE *fp) const;

public:
  // get the du_info for dna
  static DNA_DU_INFO *Get(DNA_NODE *dna) {
    return _du_mgr.Get_du(dna);
  }

  // release the du_info. the data may be cached for later use
  static void Release(DNA_DU_INFO *du) {
    _du_mgr.Release_du(du);
  }

  // delete du_info directly. not cache it any more
  static void Delete(DNA_DU_INFO *du) {
    _du_mgr.Delete_du(du);
  }
};  // IPSA_DU_MANAGER

// DU_INFO_ITER
// iterator to walk the D-U chain
class DU_INFO_ITER {
private:
  const DNA_DU_INFO *_container;  // D-U container
  const DU_INFO     *_cur;        // current use info
  std::stack<UINT32> _stack;      // stack to keep child for use visited

  DU_INFO_ITER(const DU_INFO_ITER&);            // disable copy ctor
  DU_INFO_ITER& operator=(const DU_INFO_ITER&); // disable assignment

public:
  // constructor to iterate use of var
  DU_INFO_ITER(const DNA_DU_INFO *info, CODEREP *var) : _container(info) {
    Is_True(var->Kind() == CK_VAR, ("only var has D-U"));
    UINT32 du = var->Use_chain();
    _cur = du != NULL_ID ? _container->Get_use(du) : NULL;
  }

  // constructor to iterate use of vor
  DU_INFO_ITER(const DNA_DU_INFO *info, VSYM_OBJ_REP *vor) : _container(info) {
    UINT32 du = vor->Use_chain();
    _cur = du != NULL_ID ? _container->Get_use(du) : NULL;
  }

  // check if iterator reaches end
  BOOL Empty() const {
    return _cur == NULL && _stack.empty();
  }

  // return current use and move to next use
  const DU_INFO *Next() {
    if (_cur == NULL)
      return NULL;
    const DU_INFO *ret = _cur;
    UINT32 next = _cur->Next();
    UINT32 child = _cur->Child();
    if (next != NULL_ID) {
      if (child != NULL_ID)
        _stack.push(child);  // push child to stack
      _cur = _container->Get_use(next);
    }
    else if (child != NULL_ID) {
      _cur = _container->Get_use(child);
    }
    else if (! _stack.empty()) {
      _cur = _container->Get_use(_stack.top());
      _stack.pop();
    }
    else {
      _cur = NULL;
    }
    return ret;
  }

};  // DU_INFO_ITER

#endif /* opt_vsa_du_INCLUDED */
