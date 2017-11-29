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

//====================================================================
//
// Module: wssa_du.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Interface of DU manager based on WHIRL SSA
//
// Exported classes:
//  WSSA::USE_NODE
//  WSSA::WSSA_DU_MANAGER
//  WSSA::WSSA_DU_ITERATOR
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#ifndef wssa_du_INCLUDED
#define wssa_du_INCLUDED

#include <stdio.h>
#include <vector>
#include <list>
#include <ext/hash_map>
using __gnu_cxx::hash_map;

#include "wssa_defs.h"
#include "wssa_core.h"
#include "wssa_mgr.h"

namespace WSSA {

class WSSA_DU_ITERATOR;

//===================================================================
// USE_NODE
//  The use of the version
//   _next: next use of the same version
//   _use_type: how the WHIRL uses the version
//   _use_wn: the WHIRL node uses the version
//===================================================================
class USE_NODE {
private:
  WSSA_NODE_IDX  _next;
  WSSA_NODE_KIND _use_type;
  const WN* _use_wn;

public:
  static const WSSA_NODE_KIND NODE_KIND = WSSA_UNKNOWN; // NOT used

public:
  USE_NODE(WSSA_NODE_IDX next, WSSA_NODE_KIND type, const WN* wn)
    : _next(next), _use_type(type), _use_wn(wn) { }

  WSSA_NODE_KIND Get_use_type() const    { return _use_type; }
  void Set_use_type(WSSA_NODE_KIND type) { _use_type = type; }
  const WN* Get_use_wn() const           { return _use_wn;   }
  void Set_use_wn(const WN* wn)          { _use_wn = wn;     }
  WSSA_NODE_IDX Next() const             { return _next;     }
  void Set_next(WSSA_NODE_IDX next)      { _next = next;     }

public:
  // dump and verify methods
  void Print(FILE* fp, INT indent) const;
  void Verify() const;
};

//===================================================================
// WSSA_DU_MANAGER
//  Manage the DU chain based on WHIRL SSA Manager
//  Constructor:
//    WSSA_DU_MANAGER(WHIRL_SSA_MANAGER* mgr, const WN* root)
//  Interface:
//    void Build();
//    BOOL Ver_has_use(VER_IDX ver): check if the ver has any uses
//    const_iterator begin(VER_IDX ver)
//    const_iterator end(VER_IDX ver)
//===================================================================
class WSSA_DU_MANAGER {
public:
  typedef std::vector<USE_NODE> USE_TABLE;
  typedef hash_map<UINT32 /*VER_IDX*/, UINT32 /*IDX to table*/>  USE_MAP;
  template<typename _Tmgr, typename _Tnode> friend class WSSA_NODE_ITERATOR;
  typedef WSSA_NODE_ITERATOR<WSSA_DU_MANAGER, USE_NODE> iteraor;
  typedef WSSA_NODE_ITERATOR<const WSSA_DU_MANAGER, const USE_NODE> const_iterator;

private:
  const WHIRL_SSA_MANAGER* _wssa_mgr;
  const WN* _root;
  USE_TABLE _use_table;
  USE_MAP   _use_map;

protected:
  // return the index of the first use of the version
  WSSA_NODE_IDX Ver_first_use_idx(VER_IDX ver) const {
    USE_MAP::const_iterator it = _use_map.find(ver);
    if (it != _use_map.end()) {
      UINT32 idx = it->second;
      Is_True(idx < _use_table.size(), ("use index out of bounds"));
      return idx;
    }
    else {
      return INVALID_IDX;
    }
  }

  // add wn to the use list of the version
  WSSA_NODE_IDX Add_use(VER_IDX ver, WSSA_NODE_KIND type, const WN* wn) {
    WSSA_NODE_IDX entry_idx = _use_table.size();
    WSSA_NODE_IDX first_idx = Ver_first_use_idx(ver);
    _use_table.push_back(USE_NODE(first_idx, type, wn));
    _use_map[ver] = entry_idx;
  }

  // return the node by its index
  template<WSSA_NODE_KIND>
  const USE_NODE* Get_node(WSSA_NODE_IDX idx) const {
    Is_True(idx < _use_table.size(), ("use index out of bounds"));
    return &(_use_table[idx]);
  }

  // clear all use info
  void Clear() {
    _root = NULL;
    _use_table.clear();
    _use_map.clear();
  }

  // traverse the tree and collect the use info
  void Build_rec(const WN* wn);

public:
  WSSA_DU_MANAGER(WHIRL_SSA_MANAGER* mgr, const WN* root);

  // get WHIRL SSA manager
  const WHIRL_SSA_MANAGER* WSSA_mgr() const {
    return _wssa_mgr;
  }

  // build du
  void Build();

  // return TRUE if there are uses of the version
  BOOL Ver_has_use(VER_IDX ver) const {
    Is_True(ver < _wssa_mgr->Ver_count(), ("ver idx out of bounds"));
    UINT32 idx = Ver_first_use_idx(ver);
    if (idx == INVALID_IDX) {
      return FALSE;
    }
    else {
      Is_True(idx < _use_table.size(), ("use entry out of bounds"));
      return TRUE;
    }
  }

  // return the USE_NODE
  const USE_NODE* Get_use(WSSA_NODE_IDX idx) const {
    Is_True(idx < _use_table.size(), ("use index out of bounds"));
    return &(_use_table[idx]);
  }

  // clear all use info
  // begin the iterator of the uses of the version
  const_iterator begin(VER_IDX ver) const {
    Is_True(ver < _wssa_mgr->Ver_count(), ("ver idx out of bounds"));
    WSSA_NODE_IDX idx = Ver_first_use_idx(ver);
    Is_True(idx == INVALID_IDX || idx < _use_table.size(), ("invalid use index"));
    return const_iterator(this, idx);
  }

  // end the iterator of the uses of the version
  const_iterator end(VER_IDX ver) const {
    return const_iterator(this, INVALID_IDX);
  }

public:
  WSSA_DU_ITERATOR WN_du_begin(WN* wn) const;
  WSSA_DU_ITERATOR WN_du_end(WN* wn) const;

public:
  // dump and verify methods
  void Print(FILE* fp = stdout) const;
  void Verify() const;
};

//===================================================================
// WSSA_DU_ITERATOR
//  iterate the D-U chain for the WN*
//===================================================================
class WSSA_DU_ITERATOR {
private:
  const WSSA_DU_MANAGER* _mgr;
  std::list<VER_IDX> _ver_list;
  hash_map<INTPTR /* WN */, BOOL> _wn_visited;
  VER_IDX _cur_ver;
  const USE_NODE* _cur_use;

private:
  void Next_use() {
    Is_True(_mgr != NULL, ("mgr is NULL"));

    do {
      if (_cur_use != NULL && _cur_use->Next() != INVALID_IDX) {
        _cur_use = _mgr->Get_use(_cur_use->Next());
        Is_True(_cur_use != NULL, ("use node is NULL"));
      }
      else if (_ver_list.size() > 0) {
        _cur_ver = _ver_list.front();
        _ver_list.pop_front();
        WSSA_DU_MANAGER::const_iterator it = _mgr->begin(_cur_ver);
        if (it == _mgr->end(_cur_ver)) {
          continue;
        }
        _cur_use = it.Get_node();
        Is_True(_cur_use != NULL, ("use node is NULL"));
      }
      else {
        _cur_ver = VER_INVALID;
        _cur_use = NULL;
        break;
      }

      Is_True(_cur_ver != VER_INVALID, ("invalid use ver"));
      Is_True(_cur_use != NULL, ("invalid use node"));
      WSSA_NODE_KIND use_type = _cur_use->Get_use_type();
      const WN* use_wn = _cur_use->Get_use_wn();

      if (_wn_visited[(INTPTR)use_wn] == true)
        continue;
      _wn_visited[(INTPTR)use_wn] = true;
 
      const WHIRL_SSA_MANAGER* wssa_mgr = _mgr->WSSA_mgr();
      WST_IDX wst = wssa_mgr->Get_ver_wst(_cur_ver);
      if (use_type == WSSA_PHI) {
        const PHI_NODE* node = wssa_mgr->WN_phi_node(use_wn, wst);
#ifdef Is_True_On
        int i;
        for (i = 0; i < node->Opnd_count(); ++i) {
          if (node->Opnd(i) == _cur_ver)
            break;
        }
        FmtAssert(i < node->Opnd_count(), ("invalid phi node"));
#endif
        _ver_list.push_front(node->Res());
        continue;
      }
      else if (use_type == WSSA_CHI) {
        const CHI_NODE* node = wssa_mgr->WN_chi_node(use_wn, wst);
        Is_True(node->Opnd() == _cur_ver, ("invalid chi node"));
        _ver_list.push_front(node->Res());
        break;
      }
      else {
        Is_True(use_type == WSSA_OCC || use_type == WSSA_MU, 
                ("invalid def type"));
        break;
      }
    } while(1);
#ifdef Is_True_On
    if (_cur_ver == VER_INVALID) {
      // reach the end of UD
      FmtAssert(_cur_use == NULL, ("invalid use node"));
      FmtAssert(_ver_list.size() == 0, ("version list is not empty"));
    }
    else {
      FmtAssert(_cur_use != NULL, ("invalid use node"));
      WSSA_NODE_KIND use_type = _cur_use->Get_use_type();
      FmtAssert(use_type == WSSA_OCC || use_type == WSSA_CHI ||
                use_type == WSSA_MU, ("invalid def type"));
      const WN* use_wn = _cur_use->Get_use_wn();
      const WHIRL_SSA_MANAGER* wssa_mgr = _mgr->WSSA_mgr();
      if (use_type == WSSA_OCC) {
        // use by whirl
        FmtAssert(WSSA::WN_use_ver(use_wn), ("invalud use wn"));
        FmtAssert(wssa_mgr->Get_wn_ver(use_wn) == _cur_ver, ("ver and wn mismatch"));
      }
      else if (use_type == WSSA_MU) {
        // use by mu
        FmtAssert(WSSA::WN_has_mu(use_wn), ("invalid use wn"));
        WST_IDX wst_idx = wssa_mgr->Get_ver_wst(_cur_ver);
        const MU_NODE* mu_node = wssa_mgr->WN_mu_node(use_wn, wst_idx);
        FmtAssert(mu_node != NULL && mu_node->Opnd() == _cur_ver,
                  ("ver and mu opnd mismatch"));
      }
      else if (use_type == WSSA_CHI) {
        // used by chi
        //FmtAssert(FALSE, ("TODO: reconsider the maydef-maydef chain"));
        FmtAssert(WSSA::WN_has_chi(use_wn), ("invalid use wn"));
        WST_IDX wst_idx = wssa_mgr->Get_ver_wst(_cur_ver);
        const CHI_NODE* chi_node = wssa_mgr->WN_chi_node(use_wn, wst_idx);
        FmtAssert(chi_node != NULL && chi_node->Opnd() == _cur_ver,
                  ("ver and chi opnd mismatch"));
      }
    }
#endif
  }
  void Init_def(WN* wn) {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    _cur_ver = VER_INVALID;
    _cur_use = NULL;
    if (wn != NULL) {
      // disable phi node
      Is_True(!WSSA::WN_has_phi(wn), ("invalid wn node"));
      const WHIRL_SSA_MANAGER* wssa_mgr = _mgr->WSSA_mgr();
      if (WSSA::WN_def_ver(wn)) {
        _ver_list.push_back(wssa_mgr->Get_wn_ver(wn));
      }
      if (wssa_mgr->WN_has_chi(wn)) {
        for (WHIRL_SSA_MANAGER::const_chi_iterator cit = wssa_mgr->WN_chi_begin(wn);
             cit != wssa_mgr->WN_chi_end(wn);
             ++cit) {
          _ver_list.push_back(cit->Res());
        }
      }
      FmtAssert(_ver_list.size() > 0, ("no def for wn"));
      Next_use();
    }
  }

public:
  WSSA_DU_ITERATOR(const WSSA_DU_MANAGER* mgr, WN* wn)
    : _mgr(mgr) {
    Init_def(wn);
  }
  WSSA_DU_ITERATOR(const WSSA_DU_ITERATOR& rit) :
    _mgr(rit._mgr), _ver_list(rit._ver_list), _wn_visited(rit._wn_visited),
    _cur_ver(rit._cur_ver), _cur_use(rit._cur_use) {
  }

  const WN* Use_wn() const {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid version index"));
    Is_True(_cur_use != NULL, ("invalid use node"));
    return _cur_use->Get_use_wn();
  }
  WSSA_NODE_KIND Use_type() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid version index"));
    Is_True(_cur_use != NULL, ("invalid use node"));
    return _cur_use->Get_use_type();
  }
  void Skip_cur_wst() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    const WHIRL_SSA_MANAGER* wssa_mgr = _mgr->WSSA_mgr();
    WST_IDX cur_wst = wssa_mgr->Get_ver_wst(_cur_ver);
    do {
      VER_IDX next_ver = _ver_list.front();
      WST_IDX next_wst = wssa_mgr->Get_ver_wst(next_ver);
      if (next_wst != cur_wst) {
        break;
      }
      _ver_list.pop_front();
    } while(_ver_list.size() > 0);
    if (_ver_list.size() == 0) {
      // end of the iterator
      _cur_ver = VER_INVALID;
      _cur_use = NULL;
    }
      
#ifdef Is_True_On
    for (std::list<VER_IDX>::const_iterator it = _ver_list.begin(); 
         it != _ver_list.end();
         ++it) {
      FmtAssert(cur_wst != wssa_mgr->Get_ver_wst(*it), ("errors in ver list"));
    }
#endif
  }
  WN* operator*() const {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    Is_True(_cur_use != NULL, ("invalid use node"));
    return const_cast<WN*>(_cur_use->Get_use_wn());
  }
  WSSA_DU_ITERATOR& operator++() {
    Is_True(_mgr != NULL, ("mgr is NULL"));
    Is_True(_cur_ver != VER_INVALID, ("invalid current version"));
    Is_True(_cur_use != NULL, ("current use is NULL"));
    Next_use();
    return *this;
  }
  bool operator==(const WSSA_DU_ITERATOR& rit) {
    return (_mgr == rit._mgr) && (_ver_list == rit._ver_list) && 
           (_cur_ver == rit._cur_ver) && (_cur_use == rit._cur_use);
  }
  bool operator!=(const WSSA_DU_ITERATOR& rit) {
    return (_mgr != rit._mgr) || (_ver_list != rit._ver_list) ||
           (_cur_ver != rit._cur_ver) || (_cur_use != rit._cur_use);
  }
  WSSA_DU_ITERATOR& operator=(const WSSA_DU_ITERATOR& rit) {
    _mgr = rit._mgr;
    _ver_list = rit._ver_list;
    _wn_visited = rit._wn_visited;
    _cur_ver = rit._cur_ver;
    _cur_use = rit._cur_use;
  }
};

} /* namespace WSSA */

#endif  /* wssa_du_INCLUDED */
