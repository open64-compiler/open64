/*

  Copyright (C) 2011, Hewlett-Packard Development Company, L.P. All Rights Reserved.

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
// Module: cg_cfg.h
//
// Revision history:
//  May-11-2011 - Original Version
//
// Description:
//  Interface for CG CFG
//
// Exported classes:
//  CFG_UTIL::CG_CFG
//
// SEE ALSO:
//  be/com/cfg_base.h (BB_NODE_BASE, CFG_BASE)
//
//====================================================================

#ifndef cg_cfg_INCLUDED
#define cg_cfg_INCLUDED

#include "cfg_base.h"
#include "op.h"
#include "region_util.h"
#include <ext/hash_map>
#include <vector>
#include <stack>
#include <set>
#include "bb.h"
#include "cg_region.h"
#include "cfg_util.h"

using __gnu_cxx::hash_map;
using std::vector;

namespace CFG_UTIL {

//===================================================================
// class CG_STMT_ITERATOR
//  iterator to traverse the statements in the stmt container
//  template parameters:
//    _Tcontainer: type of stmt container
//    _Tnode: type of the node
//===================================================================
template<typename _Tcontainer, typename _Tnode>
class CG_STMT_ITERATOR {

public:
  typedef _Tnode* _Tnodeptr;
  typedef _Tnode& _Tnoderef;

private:
  BOOL _fwd;
  _Tnodeptr _cur_stmt;
  _Tcontainer* _container;

public:
  CG_STMT_ITERATOR()
    : _fwd(FALSE), _cur_stmt(NULL), _container(NULL) { }
  CG_STMT_ITERATOR(BOOL fwd, _Tnodeptr cur, _Tcontainer* container)
    : _fwd(fwd), _cur_stmt(cur), _container(container) { }
  CG_STMT_ITERATOR(const CG_STMT_ITERATOR<_Tcontainer, _Tnode>& rhs)
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

  CG_STMT_ITERATOR<_Tcontainer, _Tnode>& operator++() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    if (_fwd)
      _cur_stmt = _container->Next_stmt(_cur_stmt);
    else
      _cur_stmt = _container->Prev_stmt(_cur_stmt);
    return *this;
  }

  CG_STMT_ITERATOR<_Tcontainer, _Tnode>& operator--() {
    Is_True(_cur_stmt != NULL, ("current stmt is NULL"));
    if (_fwd)
      _cur_stmt = _container->Prev_stmt(_cur_stmt);
    else
      _cur_stmt = _container->Next_stmt(_cur_stmt);
    return *this;
  }

  bool operator==(const CG_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    return (_fwd == rit._fwd) &&
           (_cur_stmt == rit._cur_stmt) &&
           (_container == rit._container);
  }
  bool operator!=(const CG_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    return ! (operator==(rit));
  }
  CG_STMT_ITERATOR<_Tcontainer, _Tnode>& operator=(const CG_STMT_ITERATOR<_Tcontainer, _Tnode>& rit) {
    _fwd = rit._fwd;
    _cur_stmt = rit._cur_stmt;
    _container = rit._container;
  }
};


//===================================================================
// class CG_STMT_CONTAINER
//  container to maintain the OP within the bb node
//===================================================================
class CG_STMT_CONTAINER {

public:
  typedef OP* STMT;
  typedef CG_STMT_ITERATOR<CG_STMT_CONTAINER, OP> iterator;
  typedef CG_STMT_ITERATOR<const CG_STMT_CONTAINER, const OP> const_iterator;

private:
  OP* _first_stmt;
  OP* _last_stmt;

public:
  CG_STMT_CONTAINER()
    : _first_stmt(NULL), _last_stmt(NULL) { }

public:
  // statement methods
  void Add_stmt(OP* stmt);
  OP* First_stmt() const;
  OP* Next_stmt(OP* stmt) const;
  OP* Prev_stmt(OP* stmt) const;
  OP* Last_stmt() const;
  BOOL Is_empty() const;

  // update the stmt inside the Container
  void Insert_before(OP* before, OP* stmt);
  void Insert_after(OP* after, OP* stmt);
  void Remove_stmt(OP* stmt);

  // iterators to traverse the statements
  iterator begin(OP* stmt = NULL) {
    return iterator(TRUE, stmt ? stmt : First_stmt(), this);
  }
  iterator end(OP* stmt = NULL) {
    return iterator(TRUE, stmt, this);
  }
  const_iterator begin(OP* stmt = NULL) const {
    return const_iterator(TRUE, stmt ? stmt : First_stmt(), this);
  }
  const_iterator end(OP* stmt = NULL) const {
    return const_iterator(TRUE, stmt, this);
  }
  iterator rbegin(OP* stmt = NULL) {
    return iterator(FALSE, stmt ? stmt : Last_stmt(), this);
  }
  iterator rend(OP* stmt = NULL) { 
    return iterator(FALSE, stmt, this);
  }
  const_iterator rbegin(OP* stmt = NULL) const {
    return const_iterator(FALSE, stmt ? stmt : Last_stmt(), this);
  }
  const_iterator rend(OP* stmt = NULL) const {
    return const_iterator(FALSE, stmt, this);
  }

public:
  // print and VCG dump methods
  void Print(FILE* fp, INT32 dump_flag) const;
  VCGNode* VCG_dump(MEM_POOL* mpool, VCGGraph& vcg, INT32 dump_flag) const;
}; // CG_STMT_CONTAINER


//===================================================================
// class CG_STMT_MAPPER
//  mapper to maintain the mapping between OP and bb node
//===================================================================
class CG_STMT_MAPPER {

public:
  typedef OP* STMT;
  typedef BB_NODE_BASE<CG_STMT_CONTAINER> BB_NODE;

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
// class CG_CFG
//   class for WHIRL CFG
//   derived from CFG_BASE using CG_STMT_CONTAINER as stmt container,
//     CG_STMT_MAPPER as stmt mapper and BASIC_NODE_CONTAINER as
//     node container
//   the OP hierarchy is also maintained
//===================================================================
class CG_CFG : public CFG_BASE <CG_STMT_CONTAINER,
                           CG_STMT_MAPPER,
                           BASIC_NODE_CONTAINER<BB_NODE_BASE<CG_STMT_CONTAINER> > > {

public:
  typedef CFG_BASE<CG_STMT_CONTAINER, CG_STMT_MAPPER, 
              BASIC_NODE_CONTAINER<BB_NODE_BASE<CG_STMT_CONTAINER> > > CFG;
  typedef hash_map<INT32, BB_NODE*> LABEL_MAP;
  typedef hash_map<INTPTR, BB*> BB_NODE_MAP;
  typedef hash_map<INTPTR, BB_NODE*> BB_MAP;
  typedef hash_map<UINT32, BB_NODE*> BB_ID_MAP;

private:
  REGION_LEVEL _rgn_level;  // current region level
  OP* _root;
  LABEL_MAP   _label_map;
  BB_NODE_MAP _bb_node_map; // BB_NODE to BB
  BB_MAP      _bb_map;      // BB to BB_NODE
  BB_ID_MAP   _bb_id_map;   // ID to BB_NODE

private:
  friend class CG_CFG_BUILDER;
  template<typename _Tcfg> friend class CG_CFG_BUILD_ACTION;
  template<typename _Tcfg> friend class CG_CFG_VERIFY_ACTION;

  // only accessable for CG_CFG_BUILDER
  void Set_label(INT32 label_num, BB_NODE* node);

  // CGSSA
  void Map_BB_and_BB_NODE(BB* bb, BB_NODE* bbn);
  bool Is_in_BB_map(BB* succ);
  BB_NODE* Get_BB_NODE_from_BB(BB* bb);

  void Add_All_Stmts_in_BB(BB_NODE*, BB*);
  void Connect_Dangling_Exits(BB_NODE* node, 
			      std::vector<bool>& visited,
			      std::vector<bool>& onstack);

public:
  CG_CFG(MEM_POOL* mpool)
    : CFG(mpool), _rgn_level(RL_UNKNOWN), _root(NULL) { }

public:
  void Set_rgn_level(REGION_LEVEL level) { _rgn_level = level; }
  REGION_LEVEL Get_rgn_level() const     { return _rgn_level;  }

  void Set_op_root(OP* root) { _root = root; }
  OP* Get_op_root() const    { return _root; }

  // return the statement ancester
  OP* Get_op(TN* tn) const;
  // return the bb node contains the op
  // op can not be BLOCK or FUNC_REGION
  BB_NODE* Get_BB(OP* op) const;
  BB* Get_CG_BB(OP* op);
  BB_NODE* Get_BB(UINT32 id) const;
  BB* Get_BB_from_BB_NODE(const BB_NODE* bbn);

  // get BB_NODE from the label number
  BB_NODE* Label_get_BB(INT32 label_num) const;
  void Remove_label(INT32 label_num);

  // get the branch OP for BB has more than one successors
  OP* Get_branch_op(BB_NODE* node) const;

public:
  void Build();
  void Verify();

public:
  // update interface
  void Insert_before(OP* before, OP* stmt) {
    CFG::Insert_before(before, stmt);
    BB* bb = Get_CG_BB(before);
    Is_True(bb != NULL, ("Given OP does not belong to any BB"));
    BB_Insert_Op_Before(bb, before, stmt);
  }
  void Insert_after(OP* after, OP* stmt) {
    CFG::Insert_after(after, stmt);
    BB* bb = Get_CG_BB(after);
    Is_True(bb != NULL, ("Given OP does not belong to any BB"));
    BB_Insert_Op_After(bb, after, stmt);
  }
  void Append_stmt(BB_NODE* node, OP* stmt) {
    BB* bb = Get_BB_from_BB_NODE(node);
    BB_Append_Op(bb, stmt);
    CFG::Add_stmt(node, stmt);
  }
  void Remove_stmt(OP* stmt) {
    BB* bb = Get_CG_BB(stmt);
    CFG::Remove_stmt(stmt); 
    Is_True(bb != NULL, ("Given OP does not belong to any BB"));
    BB_Remove_Op(bb, stmt);
  }

};

//===================================================================
// class CG_CFG_BUILDER
//   Build CFG and hierarchy map for WHIRL tree
//   interface:
//     void Build_CFG();
//===================================================================
class CG_CFG_BUILDER {

private:
  typedef CG_CFG::BB_NODE BB_NODE;
  typedef hash_map<INT32, BB_NODE*> LABEL_MAP;
  typedef hash_map<INT32, vector<BB_NODE*> > GOTO_MAP;

public:
  LABEL_MAP  _label_map;  // map form label_num to OP* for LABEL
  GOTO_MAP   _goto_map;   // map from label_num to OP* for GOTO
  BB_NODE* _current_bb;
  OP* _root;
  CG_CFG& _cfg;

public:
  CG_CFG_BUILDER(CG_CFG& cfg, OP* root) 
    : _current_bb(NULL), _root(root), _cfg(cfg) { }

public:
  void Build_CFG();

private:
  // Connect labels with gotos
  void Connect_agotos();
  void Connect_goto_to_label(CG_CFG& cfg, BB_NODE* bb, OP* op);

private:
  // handle different kinds of WHIRL node
  void Add_entry(CG_CFG& cfg, OP* op);
  void Add_altentry(CG_CFG& cfg, OP* op);
  void Add_label(CG_CFG& cfg, OP* op);
  void Add_goto(CG_CFG& cfg, OP* op);
  void Add_xgoto(CG_CFG& cfg, OP* op);
  void Add_agoto(CG_CFG& cfg, OP* op);
  void Add_condgoto(CG_CFG& cfg, OP* op);
  void Add_block(CG_CFG& cfg, OP* op);
  void Add_region(CG_CFG& cfg, OP* op);
  void Add_region_exit(CG_CFG& cfg, OP* op);
  void Add_intrinsic_call(CG_CFG& cfg, OP* op);
  void Add_stmt(CG_CFG& cfg, BB_NODE* bb, OP* op);
}; // CG_CFG_BUILDER

} /* namespace CFG_UTIL */

#endif /* cg_cfg_INCLUDED */

