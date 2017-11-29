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
// Module: wn_cfg.h
//
// Revision history:
//  Nov-1 - Original Version
//
// Description:
//  Interface for WHIRL CFG
//
// Exported classes:
//  CFG_UTIL::WN_CFG
//
// SEE ALSO:
//  be/com/cfg_base.h (BB_NODE_BASE, CFG_BASE)
//
//====================================================================

#ifndef wn_cfg_INCLUDED
#define wn_cfg_INCLUDED

#include "cfg_base.h"
#include "wn.h"
#include "region_util.h"
#include <ext/hash_map>
#include <vector>
using __gnu_cxx::hash_map;
using std::vector;

namespace CFG_UTIL {

//===================================================================
// class WN_STMT_ITERATOR
//  iterator to traverse the statements in the stmt container
//  template parameters:
//    _Tcontainer: type of stmt container
//    _Tnode: type of the node
//===================================================================
template<typename _Tcontainer, typename _Tnode>
class WN_STMT_ITERATOR {

public:
  typedef _Tnode* _Tnodeptr;
  typedef _Tnode& _Tnoderef;

private:
  BOOL _fwd;
  _Tnodeptr _cur_stmt;
  _Tcontainer* _container;

public:
  WN_STMT_ITERATOR()
    : _fwd(FALSE), _cur_stmt(NULL), _container(NULL) { }
  WN_STMT_ITERATOR(BOOL fwd, _Tnodeptr cur, _Tcontainer* container)
    : _fwd(fwd), _cur_stmt(cur), _container(container) { }
  WN_STMT_ITERATOR(const WN_STMT_ITERATOR<_Tcontainer, _Tnode>& rhs)
    : _fwd(rhs._fwd), _cur_stmt(rhs._cur_stmt), _container(rhs._container) { }

public:
  _Tnodeptr operator->() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    return _cur_stmt;
  }
  _Tnoderef operator*() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    return *_cur_stmt;
  }

  WN_STMT_ITERATOR<_Tcontainer, _Tnode>& operator++() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    if (_fwd)
      _cur_stmt = _container->Next_stmt(_cur_stmt);
    else
      _cur_stmt = _container->Prev_stmt(_cur_stmt);
    return *this;
  }

  WN_STMT_ITERATOR<_Tcontainer, _Tnode>& operator--() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    if (_fwd)
      _cur_stmt = _container->Prev_stmt(_cur_stmt);
    else
      _cur_stmt = _container->Next_stmt(_cur_stmt);
    return *this;
  }

  bool operator==(const WN_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    return (_fwd == rit._fwd) &&
           (_cur_stmt == rit._cur_stmt) &&
           (_container == rit._container);
  }
  bool operator!=(const WN_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    return ! (operator==(rit));
  }
  WN_STMT_ITERATOR<_Tcontainer, _Tnode>& operator=(const WN_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    _fwd = rit._fwd;
    _cur_stmt = rit._cur_stmt;
    _container = rit._container;
  }
};


//===================================================================
// class WN_STMT_CONTAINER
//  container to maintain the WN within the bb node
//===================================================================
class WN_STMT_CONTAINER {

public:
  typedef WN* STMT;
  typedef WN_STMT_ITERATOR<WN_STMT_CONTAINER, WN> iterator;
  typedef WN_STMT_ITERATOR<const WN_STMT_CONTAINER, const WN> const_iterator;

private:
  WN* _first_stmt;
  WN* _last_stmt;
  WN* _extra_stmt;  // Only used for DO_LOOP(increment), DO_WHILE(condition)

public:
  WN_STMT_CONTAINER()
    : _first_stmt(NULL), _last_stmt(NULL), _extra_stmt(NULL) { }

public:
  // statement methods
  void Add_stmt(WN* stmt);
  WN* First_stmt() const;
  WN* Next_stmt(WN* stmt) const;
  WN* Prev_stmt(WN* stmt) const;
  WN* Last_stmt() const;
  BOOL Is_empty() const;

  // update the stmt inside the Container
  void Insert_before(WN* before, WN* stmt);
  void Insert_after(WN* after, WN* stmt);
  void Remove_stmt(WN* stmt);

  // iterators to traverse the statements
  iterator begin(WN* stmt = NULL) {
    return iterator(TRUE, stmt ? stmt : First_stmt(), this);
  }
  iterator end(WN* stmt = NULL) {
    return iterator(TRUE, stmt, this);
  }
  const_iterator begin(WN* stmt = NULL) const {
    return const_iterator(TRUE, stmt ? stmt : First_stmt(), this);
  }
  const_iterator end(WN* stmt = NULL) const {
    return const_iterator(TRUE, stmt, this);
  }
  iterator rbegin(WN* stmt = NULL) {
    return iterator(FALSE, stmt ? stmt : Last_stmt(), this);
  }
  iterator rend(WN* stmt = NULL) { 
    return iterator(FALSE, stmt, this);
  }
  const_iterator rbegin(WN* stmt = NULL) const {
    return const_iterator(FALSE, stmt ? stmt : Last_stmt(), this);
  }
  const_iterator rend(WN* stmt = NULL) const {
    return const_iterator(FALSE, stmt, this);
  }

public:
  // print and VCG dump methods
  void Print(FILE* fp, INT32 dump_flag) const;
  VCGNode* VCG_dump(MEM_POOL* mpool, VCGGraph& vcg, INT32 dump_flag) const;
};


//===================================================================
// class WN_STMT_MAPPER
//  mapper to maintain the mapping between WN and bb node
//===================================================================
class WN_STMT_MAPPER {

public:
  typedef WN* STMT;
  typedef BB_NODE_BASE<WN_STMT_CONTAINER> BB_NODE;

private:
  typedef hash_map<INTPTR, BB_NODE*> STMT_MAP;
  STMT_MAP _stmt_map;

public:
  void Connect_stmt_node(STMT stmt, BB_NODE* node);
  void Disconnect_stmt_node(STMT stmt, BB_NODE* node);
  void Disconnect_all_stmt_in_node(BB_NODE* node);
  BB_NODE* Get_stmt_node(STMT stmt) const;
};


//===================================================================
// class WN_CFG
//   class for WHIRL CFG
//   derived from CFG_BASE using WN_STMT_CONTAINER as stmt container,
//     WN_STMT_MAPPER as stmt mapper and BASIC_NODE_CONTAINER as
//     node container
//   the WN hierarchy is also maintained
//===================================================================
class WN_CFG : public CFG_BASE <WN_STMT_CONTAINER,
                           WN_STMT_MAPPER,
                           BASIC_NODE_CONTAINER<BB_NODE_BASE<WN_STMT_CONTAINER> > > {

public:
  typedef CFG_BASE<WN_STMT_CONTAINER, WN_STMT_MAPPER, 
              BASIC_NODE_CONTAINER<BB_NODE_BASE<WN_STMT_CONTAINER> > > CFG;
  typedef hash_map<INTPTR, WN*> PARENT_MAP;
  typedef hash_map<INT32, BB_NODE*> LABEL_MAP;

private:
  REGION_LEVEL _rgn_level;  // current region level
  WN* _root;
  PARENT_MAP _parent_map;
  LABEL_MAP  _label_map;

private:
  friend class WN_CFG_BUILDER;
  template<typename _Tcfg> friend class WN_CFG_BUILD_ACTION;
  template<typename _Tcfg> friend class WN_CFG_VERIFY_ACTION;
  template<typename _Tcfg> friend class PARENTMAP_BUILD_ACTION;
  template<typename _Tcfg> friend class PARENTMAP_VERIFY_VERIFIER;

  // only accessable for WN_CFG_BUILDER
  void Set_parent(WN* parent, WN* kid);
  void Remove_parent(WN* wn);
  void Set_label(INT32 label_num, BB_NODE* node);
  void Parentize_tree(WN* tree);
  void Unparentize_tree(WN* tree);

public:
  WN_CFG(MEM_POOL* mpool)
    : CFG(mpool), _rgn_level(RL_UNKNOWN), _root(NULL) { }

public:
  void Set_rgn_level(REGION_LEVEL level) { _rgn_level = level; }
  REGION_LEVEL Get_rgn_level() const     { return _rgn_level;  }

  void Set_wn_root(WN* root) { _root = root; }
  WN* Get_wn_root() const    { return _root; }

  // return the parent of kid
  WN* Get_parent(WN* kid) const;
  // return the statement ancester
  // wn must be an expression
  WN* Get_parent_stmt(WN* wn) const;
  // return the BLOCK ancester
  WN* Get_parent_block(WN* wn) const;
  // return the SCF ancester if there is one
  WN* Get_parent_scf(WN* wn) const;
  // return the REGION ancester if there is one
  WN* Get_parent_region(WN* wn) const;
  // return the bb node contains the wn
  // wn can not be BLOCK or FUNC_REGION
  BB_NODE* Get_wn_node(WN* wn) const;

  // get BB_NODE from the label number
  BB_NODE* Get_label_node(INT32 label_num) const;
  void Remove_label(INT32 label_num);

  // get the branch WN for BB has more than one successors
  WN* Get_branch_stmt(BB_NODE* node) const;

public:
  void Build();
  void Verify();

public:
  // update interface
  void Insert_before(WN* before, WN* stmt, BOOL reparentize = TRUE) {
    CFG::Insert_before(before, stmt);

    if (reparentize) {
      // maintain the parent map
      Parentize_tree(stmt);
      Set_parent(Get_parent(before), stmt);
    }
    if (WN_operator(stmt) == OPR_LABEL) {
      BB_NODE* node = CFG::Get_stmt_node(before);
      Set_label(WN_label_number(stmt), node);
    }
  }
  void Insert_after(WN* after, WN* stmt, BOOL reparentize = TRUE) {
    CFG::Insert_after(after, stmt);

    if (reparentize) {
      // maintain the parent map
      Parentize_tree(stmt);
      Set_parent(Get_parent(after), stmt);
    }
    if (WN_operator(stmt) == OPR_LABEL) {
      BB_NODE* node = CFG::Get_stmt_node(after);
      Set_label(WN_label_number(stmt), node);
    }
  }
  void Add_stmt(BB_NODE* node, WN* stmt, BOOL reparentize = TRUE) {
    CFG::Add_stmt(node, stmt);

    if (reparentize) {
      Parentize_tree(stmt);
      Is_True(WN_prev(stmt) != NULL, ("WN prev is NULL"));
      Set_parent(Get_parent(WN_prev(stmt)), stmt);
    }
    if (WN_operator(stmt) == OPR_LABEL) {
      Set_label(WN_label_number(stmt), node);
    }
  }
  void Remove_stmt(WN* stmt, BOOL reparentize = TRUE) {
    CFG::Remove_stmt(stmt); 

    if (reparentize) {
      // maintain the parent map
      //Unparentize_tree(stmt);
    }
    if (WN_operator(stmt) == OPR_LABEL) {
      Remove_label(WN_label_number(stmt));
    }
  }
  // split node at top into two nodes and return the new one
  // after_wn is the last WN in original node
  // WNs after after_wn is transferred to new_node
  BB_NODE* Split_node(BB_NODE* node, WN* after_wn) {
    BB_NODE* new_bb = CFG::Split_node(node);
    WN* first_wn = node->First_stmt();
    WN* last_wn = node->Last_stmt();
    if (last_wn == NULL) {
      Is_True(first_wn == NULL && after_wn == NULL,
              ("node is empty"));
      return new_bb;
    }
    Is_True(last_wn != NULL  && CFG::Get_stmt_node(last_wn)  == node,
            ("invalid last_wn"));
    Is_True(first_wn != NULL && CFG::Get_stmt_node(first_wn) == node,
            ("invalid first_wn"));
    WN* wn;
    for (wn = last_wn; 
         wn != WN_prev(first_wn) && wn != after_wn; 
         wn = WN_prev(wn)) {
      Remove_stmt(wn, FALSE);
    }
    Is_True(wn == after_wn || after_wn == NULL, 
            ("can not find after_wn in node"));
    if (wn == NULL) {
      Is_True(after_wn == NULL, ("after_wn should be NULL"));
      Is_True(WN_prev(first_wn) == NULL, ("prev of first_wn should be NULL"));
      wn = first_wn;
    }
    else {
      wn = WN_next(after_wn);
    }
    for (; wn != WN_next(last_wn); wn = WN_next(wn)) {
      Add_stmt(new_bb, wn, FALSE);
    }
    return new_bb;
  }
};

//===================================================================
// class WN_CFG_BUILDER
//   Build CFG and hierarchy map for WHIRL tree
//   interface:
//     void Build_CFG();
//===================================================================
class WN_CFG_BUILDER {

private:
  typedef WN_CFG::BB_NODE BB_NODE;
  typedef hash_map<INT32, BB_NODE*> LABEL_MAP;
  typedef hash_map<INT32, vector<BB_NODE*> > GOTO_MAP;

public:
  LABEL_MAP  _label_map;  // map form label_num to WN* for LABEL
  GOTO_MAP   _goto_map;   // map from label_num to WN* for GOTO
  BB_NODE* _current_bb;
  WN* _root;
  WN_CFG& _cfg;

public:
  WN_CFG_BUILDER(WN_CFG& cfg, WN* root) 
    : _current_bb(NULL), _root(root), _cfg(cfg) { }

public:
  void Build_CFG();

private:
  // build WHIRL hierarchy map
  void Build_parent_map_stmt(WN_CFG& cfg, WN* wn);
  void Build_parent_map_block(WN_CFG& cfg, WN* wn);
  // Connect labels with gotos
  void Connect_agotos();
  void Connect_goto_to_label(WN_CFG& cfg, BB_NODE* bb, WN* wn);

private:
  // handle different kinds of WHIRL node
  void Add_entry(WN_CFG& cfg, WN* wn);
  void Add_altentry(WN_CFG& cfg, WN* wn);
  void Add_if(WN_CFG& cfg, WN* wn);
  void Add_do_loop(WN_CFG& cfg, WN* wn);
  void Add_do_while(WN_CFG& cfg, WN* wn);
  void Add_while_do(WN_CFG& cfg, WN* wn);
  void Add_compgoto(WN_CFG& cfg, WN* wn);
  void Add_switch(WN_CFG& cfg, WN* wn);
  void Add_label(WN_CFG& cfg, WN* wn);
  void Add_goto(WN_CFG& cfg, WN* wn);
  void Add_xgoto(WN_CFG& cfg, WN* wn);
  void Add_agoto(WN_CFG& cfg, WN* wn);
  void Add_condgoto(WN_CFG& cfg, WN* wn);
  void Add_block(WN_CFG& cfg, WN* wn);
  void Add_region(WN_CFG& cfg, WN* wn);
  void Add_region_exit(WN_CFG& cfg, WN* wn);
  void Add_intrinsic_call(WN_CFG& cfg, WN* wn);
  void Add_stmt(WN_CFG& cfg, BB_NODE* bb, WN* wn);
};

} /* namespace CFG_UTIL */

#endif /* wn_cfg_INCLUDED */

