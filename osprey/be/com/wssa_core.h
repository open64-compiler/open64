/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//===================================================================
//
// Module: wssa_core.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Core data structures for WHIRL SSA
//
// Exported classes:
//  WSSA::PHI_NODE, WSSA::CHI_NODE, WSSA::MU_NODE
//
//===================================================================

#ifndef wssa_core_INCLUDED
#define wssa_core_INCLUDED

#include "wssa_defs.h"
#include "wssa_utils.h"
#include "errors.h"
#include <vector>
using std::vector;
#include <ext/hash_map>
using __gnu_cxx::hash_map;


namespace WSSA {

//===================================================================
// traits for wssa nodes
//   phi node: number of res = 1, number of opnd is not fixed
//   chi node: number of res = 1, number of opnd = 1
//   mu  node: number of res = 0, number of opnd = 1
//===================================================================
class PHI_NODE_TRAITS {
public:
  enum { NODE_KIND = WSSA_PHI };
  enum { RES_COUNT = 1, OPND_COUNT = -1 };
};

class CHI_NODE_TRAITS {
public:
  enum { NODE_KIND = WSSA_CHI };
  enum { RES_COUNT = 1, OPND_COUNT = 1 };
};

class MU_NODE_TRAITS {
public:
  enum { NODE_KIND = WSSA_MU  };
  enum { RES_COUNT = 0, OPND_COUNT = 1 };
};

//===================================================================
// SSA_NODE_BASE
//   base class for SSA nodes: phi, chi and mu node
//   members:
//     _next: the next SSA node attached to the same WHIRL node
//     _opnds: collection of both res and opnds
//       phi: _opnds[0] is res, _opnds[1] is number of opnds,
//            _opnds[2..k+1] is opnds 0..k-1
//       chi: _opnds[0] is res, _opnds[1] is opnd
//       mu : _opnds[0] is opnd
//===================================================================
template<typename _Ttraits>
class SSA_NODE_BASE {
private:
  WSSA_NODE_IDX _next;  // index for the next node on the list
  INT32 _opnds[0];      // variant length
                        // phi: 1 for res + 1 for num of opnds + num
                        // chi: 1 for res + 1 for opnd
                        // mu:  1 for opnd

private:
  // convert operand index to location in _opnds array
  INT32 opnd_idx_to_store_loc(INT32 idx) const {
    Is_True(idx >= 0 && idx < Opnd_count(), ("bad opnd index"));
    if (_Ttraits::OPND_COUNT != -1)
      return _Ttraits::RES_COUNT + idx;
    else
      return _Ttraits::RES_COUNT + 1 /* num of opnds */ + idx;
  }

  // no implementation for the following methods
  SSA_NODE_BASE();
  SSA_NODE_BASE(const SSA_NODE_BASE<_Ttraits>& lhs);
  SSA_NODE_BASE<_Ttraits>& operator=(const SSA_NODE_BASE<_Ttraits>& lhs);
  void* operator new(std::size_t size);
  void* operator new[] (std::size_t size);

public:
  static const WSSA_NODE_KIND NODE_KIND = (WSSA_NODE_KIND)_Ttraits::NODE_KIND;

public:
  // number of result and operands
  INT32 Res_count() const { return _Ttraits::RES_COUNT; }
  INT32 Opnd_count() const {
    if (_Ttraits::OPND_COUNT != -1)
      return _Ttraits::OPND_COUNT;
    else
      return _opnds[_Ttraits::RES_COUNT];
  }
  void Set_opnd_count(INT32 opnds) {
    Is_True(_Ttraits::OPND_COUNT == -1, ("can not set opnd count"));
    _opnds[_Ttraits::RES_COUNT] = opnds;
  }

  // result
  VER_IDX Get_res(INT32 idx) const {
    Is_True(idx >= 0 && idx < Res_count(), ("bad res index"));
    return (VER_IDX)_opnds[idx];
  }
  void Set_res(INT32 idx, VER_IDX ver) {
    Is_True(idx >= 0 && idx < Res_count(), ("bad res index"));
    _opnds[idx] = ver;
  }

  // operands
  VER_IDX Get_opnd(INT32 idx) const {
    INT32 loc = opnd_idx_to_store_loc(idx);
    return (VER_IDX)_opnds[loc];
  }
  void Set_opnd(INT32 idx, VER_IDX ver) {
    INT32 loc = opnd_idx_to_store_loc(idx);
    _opnds[loc] = ver;
  }

  // next node
  WSSA_NODE_IDX Next() const {
    return _next;
  }
  void Set_next(WSSA_NODE_IDX next) {
    _next = next;
  }

public:
  // routines for debugging
  void Print(FILE* fp, int indent) const {
    Put_indent(fp, indent);
    if (Res_count() > 0) {
      Print_ver(fp, Get_res(0));
      fprintf(fp, " = ");
    }
    fprintf(fp, "%s(", WSSA_node_name(_Ttraits::NODE_KIND)); 
    int i = 0;
    for ( ; i < Opnd_count() - 1; ++i) {
      Print_ver(fp, Get_opnd(i));
      fprintf(fp, ", ");
    }
    if (i < Opnd_count()) {
      Print_ver(fp, Get_opnd(i));
    }
    fprintf(fp, ")\n");
  }
  void Verify() const {
    WST_IDX wst_idx = Get_ver_wst(Get_opnd(0)); // always use opnd 0
    UINT32 max_ver = Get_wst_max_ver(wst_idx);
    const char* name = Get_wst_name(wst_idx);
    int i;
    for (i = 0; i < Res_count(); ++i) {
      WST_IDX res_wst = Get_ver_wst(Get_res(i));
      UINT32 res_num = Get_ver_num(Get_res(i));
      FmtAssert(res_wst == wst_idx,
                ("wst idx mismatch, res=%d, opnd0=%d",
                 res_wst, wst_idx));
      FmtAssert(res_num < max_ver,
                ("%s node, %s res(%d) > max_ver(%d)",
                _Ttraits::NODE_NAME, name, res_num, max_ver));
    }
    for (i = 0; i < Opnd_count(); ++i) {
      WST_IDX opnd_wst = Get_ver_wst(Get_opnd(i));
      UINT32 opnd_num = Get_ver_num(Get_opnd(i));
      FmtAssert(opnd_wst == wst_idx,
                ("wst idx mismatch, opnd=%d, opnd0=%d",
                 opnd_wst, wst_idx));
      FmtAssert(opnd_num < max_ver,
                ("%s node, %s opnd%d(%d) > max_ver(%d)",
                 _Ttraits::NODE_NAME, name, i, opnd_num, max_ver));
    }
  }
}; /* SSA_NODE_BASE */

//===================================================================
// WSSA nodes:
//   PHI_NODE
//   CHI_NODE
//   MU_NODE
//===================================================================
class PHI_NODE : public SSA_NODE_BASE<PHI_NODE_TRAITS> {
public:
  typedef SSA_NODE_BASE<PHI_NODE_TRAITS> SSA_NODE;

public:
  VER_IDX Res() const       { return SSA_NODE::Get_res(0); }
  void Set_res(VER_IDX ver) { SSA_NODE::Set_res(0, ver); }
  using SSA_NODE::Set_res;

  VER_IDX Opnd(int idx) const { return SSA_NODE::Get_opnd(idx); }
};

class CHI_NODE : public SSA_NODE_BASE<CHI_NODE_TRAITS> {
public:
  typedef SSA_NODE_BASE<CHI_NODE_TRAITS> SSA_NODE;

public:
  VER_IDX Res() const       { return SSA_NODE::Get_res(0); }
  void Set_res(VER_IDX ver) { SSA_NODE::Set_res(0, ver);   }
  using SSA_NODE::Set_res;

  VER_IDX Opnd() const      { return SSA_NODE::Get_opnd(0); }
  void Set_opnd(VER_IDX ver){ SSA_NODE::Set_opnd(0, ver);   }
  using SSA_NODE::Set_opnd;
};

class MU_NODE : public SSA_NODE_BASE<MU_NODE_TRAITS> {
public:
  typedef SSA_NODE_BASE<MU_NODE_TRAITS> SSA_NODE;

public:
  VER_IDX Opnd() const      { return SSA_NODE::Get_opnd(0); }
  void Set_opnd(VER_IDX ver){ SSA_NODE::Set_opnd(0, ver);   }
  using SSA_NODE::Set_opnd;
};

//===================================================================
// WSSA_NODE_ITERATOR
//  iterate the ssa nodes following the _next field
//===================================================================
template <typename _Tmgr, typename _Tnode>
class WSSA_NODE_ITERATOR {
private:
  WSSA_NODE_IDX _cur_idx;
  _Tmgr* _mgr;

public:
  WSSA_NODE_ITERATOR(_Tmgr* mgr, WSSA_NODE_IDX node_idx)
    : _mgr(mgr), _cur_idx(node_idx) { }
  // using default copy constructor

  WSSA_NODE_IDX Node_idx() const {
    return _cur_idx;
  }
  _Tnode* Get_node() {
    Is_True(_cur_idx != INVALID_IDX, ("cur node idx is invalid"));
    Is_True(_mgr != NULL, ("mgr is NULL"));
    return _mgr->template Get_node<_Tnode::NODE_KIND>(_cur_idx);
  }
  _Tnode* operator->() {
    Is_True(_cur_idx != INVALID_IDX, ("cur node idx is invalid"));
    Is_True(_mgr != NULL, ("mgr is NULL"));
    return _mgr->template Get_node<_Tnode::NODE_KIND>(_cur_idx);
  }
  _Tnode& operator*() {
    Is_True(_cur_idx != INVALID_IDX, ("cur node idx is invalid"));
    Is_True(_mgr != NULL, ("mgr is NULL"));
    return *(_mgr->template Get_node<_Tnode::NODE_KIND>(_cur_idx));
  }
  WSSA_NODE_ITERATOR& operator++() {
    Is_True(_cur_idx != INVALID_IDX, ("cur node idx is invalid"));
    Is_True(_mgr != NULL, ("mgr is NULL"));
    _Tnode* cur_node = _mgr->template Get_node<_Tnode::NODE_KIND>(_cur_idx);
    Is_True(cur_node != NULL, ("cur node is NULL"));
    _cur_idx = cur_node->Next();
  }
  bool operator==(const WSSA_NODE_ITERATOR<_Tmgr, _Tnode>& rit) {
    return (_mgr == rit._mgr) && (_cur_idx == rit._cur_idx);
  }
  bool operator!=(const WSSA_NODE_ITERATOR<_Tmgr, _Tnode>& rit) {
    return (_mgr != rit._mgr) || (_cur_idx != rit._cur_idx);
  }
  // using default operator=()
};


//===================================================================
// TABLE and MAPs
//  PHI_TABLE: collection of phi nodes
//  CHI_TABLE: collection of chi nodes
//  MU_TABLE:  collection of mu  nodes
//  LABEL_PHI_MAP: mapping from label number to first phi node 
//                 attached on the label
//  WN_PHI_MAP: mapping from WHIRL node to first phi node
//  WN_CHI_MAP: mapping from WHIRL node to first chi node
//  WN_MU_MAP:  mapping from WHIRL node to first mu  node
//===================================================================
typedef vector<PHI_NODE*> PHI_TABLE;
typedef vector<CHI_NODE*> CHI_TABLE;
typedef vector<MU_NODE*>  MU_TABLE;

typedef hash_map<WSSA_WN_IDX, WSSA_NODE_IDX> WN_PHI_MAP;
typedef hash_map<WSSA_WN_IDX, WSSA_NODE_IDX> WN_CHI_MAP;
typedef hash_map<WSSA_WN_IDX, WSSA_NODE_IDX> WN_MU_MAP;
typedef hash_map<WSSA_WN_IDX, VER_IDX>       WN_VER_MAP;

//===================================================================
// NODE_TO_TYPES
//  Get TABLE/MAP types for given SSA NODE TYPE
//===================================================================
template<WSSA_NODE_KIND _Tkind>
struct NODE_TO_TYPES;

template<>
struct NODE_TO_TYPES<WSSA_PHI> {
  typedef PHI_NODE_TRAITS NODE_TRAITS;
  typedef PHI_NODE   NODE_TYPE;
  typedef PHI_TABLE  TABLE_TYPE;
  typedef WN_PHI_MAP WN_MAP_TYPE;
};

template<>
struct NODE_TO_TYPES<WSSA_CHI> {
  typedef CHI_NODE_TRAITS NODE_TRAITS;
  typedef CHI_NODE   NODE_TYPE;
  typedef CHI_TABLE  TABLE_TYPE;
  typedef WN_CHI_MAP WN_MAP_TYPE;
};

template<>
struct NODE_TO_TYPES<WSSA_MU > {
  typedef MU_NODE_TRAITS  NODE_TRAITS;
  typedef MU_NODE    NODE_TYPE;
  typedef MU_TABLE   TABLE_TYPE;
  typedef WN_MU_MAP  WN_MAP_TYPE;
};


} /* namespace WSSA */

#endif /* wssa_core_INCLUDED */
