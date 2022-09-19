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

// ====================================================================
//
// Module: opt_vsa_eh.h
//
// Part 1: infrastructure enhancement to support EH
//  - Add exception object type table to DNA_NODE to save type of all
//    possible exception objects thrown by this function. The initial
//    data comes from exception specification on function declaration
//    or empty.
//  - Add EH-table to COMP_UNIT for each function. Each entry of the
//    table contains the CALL stmt which may throws exceptions, the
//    OPT_CHI of the handler BB which catches exceptions from the call,
//    the BB_REGION information, flags and masks for types which may
//    be thrown from the call.
//  - Update DNA_NODE exception specification according to EH-table.
//  - In IPSA::Link, bottom-up propagate exception specification along
//    with the call graph
//  - After IPSA Link phase, update the COMP_UNIT EH-table with updated
//    DNA_NODE exception specification.
//
// Part 2: enhance our general checkers to work with EH
//  - For all existing general checkers
//    - when the backward U-D traversal reaches the entry CHI of a
//      handler, follow the predecessors on EH-table and match the CHI
//      node on OPT_CHI and MU node on call to continue the traversal.
//  - If the CR with the same AUX occurs on the CHI list of the call,
//    the traversal will step into the call according to the type of
//    object been thrown. Otherwise the pred of call stmt is checked.
//
// Part 3: CMU SEI CERT ERR rules
//  - Follow the exception path from call to handler or handler to call
//    to check if the process of exceptio object matches with the rule
//
// ====================================================================

#ifndef opt_vsa_eh_INCLUDED
#define opt_vsa_eh_INCLUDED        "opt_vsa_eh.h"

#include "mempool_allocator.h"
#include <vector>
#include <ext/hash_set>
#include "symtab.h"

class ST;
class STMTREP;
class VSA;
class RNA_NODE;
class DNA_NODE;
class IPSA;

#define EH_CLEANUP_TI   ((ST*)1)
#define EH_CATCHALL_TI  ((ST*)2)
#define EH_FINALLY_TI   ((ST*)3)

// ----------------------------------------------------------------------------
// EH_PATH_FLAG
//   PATH flag for inter-procedure exception specification propagation
// ----------------------------------------------------------------------------
enum EH_PATH_FLAG {
};

typedef std::pair<uint32_t, STR_IDX> FS_PAIR;
typedef mempool_allocator<FS_PAIR> EH_TYPE_ALLOCATOR;
typedef std::vector<FS_PAIR, EH_TYPE_ALLOCATOR> EH_TYPE_VECTOR;

typedef std::pair<FS_PAIR, IDTYPE> FS_BB_PAIR;
typedef mempool_allocator<FS_BB_PAIR> FS_BB_ALLOCATOR;
typedef std::vector<FS_BB_PAIR, FS_BB_ALLOCATOR> FS_BB_VECTOR;

// ----------------------------------------------------------------------------
// EH_PATH
//   Present a path from a call which may throw exception to its handler
//   there are a little difference between eh in c++ and eh in java
//   for java, a handler don't represent the catch body, it is just a filter
//     we should traverse the handler's successor to find the real catch symbol and catch body
// ----------------------------------------------------------------------------
class EH_PATH {
private:
  STMTREP       *_throw;      // a CALL stmtrep which throws exception
  STMTREP       *_handler;    // a OPT_CHI stmtrep at the entry of a handler
  BB_REGION     *_rinfo;      // associated region info on the call
  FS_BB_VECTOR   _fs_bb_vec;  // <tcval, bb> vector, saved the real catch symbol and it's catch body
  UINT32         _flag : 8;   // eh path flags
  UINT32         _mask :24;   // typeinfo mask, max 24 typeinfo supported

private:
  void Find_catch_symbol_and_body(STMTREP *handler, EH_TYPE_VECTOR *filter_info, COMP_UNIT *cu);
  void Find_catch_symbol_and_body(BB_NODE *bb, EH_TYPE_VECTOR *filter_info, COMP_UNIT *cu, __gnu_cxx::hash_set<IDTYPE> &visited_bb);

public:
  EH_PATH(STMTREP* call, STMTREP* handler, BB_REGION* rinfo, EH_TYPE_VECTOR *filter_info, COMP_UNIT *cu, MEM_POOL *pool)
   : _throw(call), _handler(handler), _rinfo(rinfo), _flag(0), _mask(0),
   _fs_bb_vec(0, std::make_pair(std::make_pair(0, 0), 0), FS_BB_VECTOR::allocator_type(pool)) {
    if (filter_info->size() != 0)
      Find_catch_symbol_and_body(handler, filter_info, cu);
  }

  STMTREP*                    Throw_stmt() const    { return _throw;   }
  STMTREP*                    Handler_stmt() const  { return _handler; }
  BB_REGION*                  Region_info() const   { return _rinfo;   }
  FS_BB_VECTOR*               Fs_bb_vector()        { return &_fs_bb_vec; }
  void                        Print(FILE* fp = stdout) const;
};

typedef mempool_allocator<EH_PATH*> EH_PATH_ALLOCATOR;
typedef std::vector<EH_PATH*, EH_PATH_ALLOCATOR> EH_PATH_VECTOR;

// ----------------------------------------------------------------------------
// EH_TABLE
//   Present all exception path in the function
// ----------------------------------------------------------------------------
class EH_TABLE {
private:
  COMP_UNIT      *_cu;
  EH_PATH_VECTOR  _paths;
  EH_TYPE_VECTOR  _typeinfo;
  EH_TYPE_VECTOR  _eh_spec;      // method throws exception table, saved throw exception name
  EH_TYPE_VECTOR  _filter_info;  // method catch exception table, saved catch exception name

private:
  void Init_eh_info();

public:
  EH_TABLE(COMP_UNIT* cu)
   : _cu(cu),
     _paths(0, NULL, EH_PATH_VECTOR::allocator_type(cu->Mem_pool())),
     _typeinfo(0, std::make_pair(0, 0), EH_TYPE_VECTOR::allocator_type(cu->Mem_pool())),
     _eh_spec(0, std::make_pair(0, 0), EH_TYPE_VECTOR::allocator_type(cu->Mem_pool())),
     _filter_info(0, std::make_pair(0, 0), EH_TYPE_VECTOR::allocator_type(cu->Mem_pool()))
  {
    Init_eh_info();
  }

  COMP_UNIT*      Comp_unit() const { return _cu; }
  EH_PATH_VECTOR* Path_list()       { return &_paths; }
  EH_TYPE_VECTOR* Type_list()       { return &_typeinfo; }
  EH_TYPE_VECTOR* Spec_list()       { return &_eh_spec; }
  EH_TYPE_VECTOR* Filter_list()     { return &_filter_info; }

  EH_PATH* Create_eh_path(STMTREP* call, STMTREP* opt_chi, BB_REGION* rinfo)
  {
    EH_PATH* node = CXX_NEW(EH_PATH(call, opt_chi, rinfo, Filter_list(), Comp_unit(), Comp_unit()->Mem_pool()),
                             Comp_unit()->Mem_pool());
    _paths.push_back(node);
    return node;
  }

  EH_PATH* Find_handler(STMTREP* call)
  {
    EH_PATH_VECTOR::const_iterator it;
    for (it = _paths.begin(); it != _paths.end(); ++it) {
      if (call == (*it)->Throw_stmt())
        return *it;
    }
    return NULL;
  }

  FS_PAIR Type_info(INT filter) const;
  BOOL    Is_eh_type_match(uint32_t file_idx, STR_IDX eh_type, INT filter) const;
  INT     Get_type_filter(uint32_t file_idx, STR_IDX eh_type) const;

  BOOL    Add_eh_type(uint32_t file_idx, STR_IDX ti);
  BOOL    Throw_eh_type(uint32_t file_idx, STR_IDX ti) const;

public:
  void Print(FILE* fp = stdout) const;
  void Print_region(BB_REGION* region, FILE* fp = stdout) const;
};

// ----------------------------------------------------------------------------
// EH_PATH_ITER
//   Iterator to traverse all calls which will reach the handler when an
//   exception is thrown
// ----------------------------------------------------------------------------
class EH_PATH_ITER {
private:
  STMTREP *_handler;
  EH_PATH_VECTOR::const_iterator _iter;
  EH_PATH_VECTOR::const_iterator _end;

  EH_PATH_ITER(const EH_PATH_ITER&);
  EH_PATH_ITER& operator=(const EH_PATH_ITER&);

public:
  EH_PATH_ITER(EH_TABLE* table, STMTREP* handler)
   : _handler(handler)
  {
    _end = table->Path_list()->end();
    for (_iter = table->Path_list()->begin(); _iter != _end; ++_iter) {
      if (handler == (*_iter)->Handler_stmt())
        return;
    }
  }

  BOOL Is_empty()
  {
    return _iter == _end || (*_iter)->Handler_stmt() != _handler;
  }

  EH_PATH* Next()
  {
    if (Is_empty())
      return NULL;
    EH_PATH* path = *_iter;
    ++ _iter;
    return path;
  }

};

#endif /* opt_vsa_eh_INCLUDED */

