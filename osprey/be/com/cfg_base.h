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
// Module: cfg_base.h
//
// Revision history:
//  Nov-1 - Original Version
//
// Description:
//  Generic base class for Control Flow Graph
//
// Exported classes:
//  CFG_UTIL::BB_NODE_BASE
//  CFG_UTIL::CFG_BASE
//
// SEE ALSO:
//
//====================================================================

#ifndef cfg_base_INCLUDED
#define cfg_base_INCLUDED

#include "defs.h"
#include "errors.h"
#include "cxx_memory.h"
#include "bitset.h"
#include "vcg.h"
#include <sstream>
#include <list>
#include <vector>
#include <bitset>
#include <algorithm>
using namespace std;

#define POS_INVALID -1

namespace CFG_UTIL {

// Forward declaration of DOM_BUILDER, DF_BUILDER, CFG_BASE, DFS_ITERATOR
// They will access the private interface of BB_NODE_BASE/CFG_BASE
template<typename _Tcfg> class DOM_BUILDER;
template<typename _Tcfg> class DF_BUILDER;
template<typename _Tcfg, typename _Twalker, BOOL _Dom> class DOM_WALKER_HELPER;
template<typename _Tcfg, BOOL _Fwd> class DFS_ITERATOR;
template<typename _Tsc, typename _Tsm, typename _Tnc> class CFG_BASE;
 
// bitwised options to control the dump content of the bb node
enum DUMP_OPTIONS {
  // cfg dump flags use the bit 0-7 or more
  DUMP_CFG   = 0x00000001, // dump cfg info, _id, _preds, _succs
  DUMP_DOM   = 0x00000002, // dump dom info, _idom, _dom_list
  DUMP_PDOM  = 0x00000004, // dump pdom info, _ipdom, _pdom_list
  DUMP_ADOM  = DUMP_DOM | DUMP_PDOM, // all dom related dump
  DUMP_DF    = 0x00000008, // dump dominance frontier
  DUMP_CD    = 0x00000010, // dump control dependency
  DUMP_ADF   = DUMP_DF | DUMP_CD,    // all df/cd related dump
  DUMP_ACFG  = 0x000000FF, // all cfg related dump
  // ir dump flags use the bit 16-23 or more
  DUMP_STMT  = 0x00010000, // dump stmt (include scf) w/o kids
  DUMP_EXPR  = 0x00020000, // dump stmt with it kids
  DUMP_AIR   = 0x00FF0000, // all IR related dump
};

//===================================================================
// class BB_NODE_SET
//  wrapper for BS
//===================================================================
class BB_NODE_SET {
private:
  MEM_POOL* _mpool;
  BS* _bs;

public:
  BB_NODE_SET(INT size, MEM_POOL* mpool) : _mpool(mpool) {
    _bs = BS_Create_Empty(size, _mpool);
  };

  // set operations
  BB_NODE_SET& Union1D(BS_ELT x) {
    Is_True(x >= 0 && x < BS_Alloc_Size(_bs) * CHAR_BIT, ("elem out of bounds"));
    _bs = BS_Union1D(_bs, x, _mpool);
    return *this;
  }
  BB_NODE_SET& UnionD(const BB_NODE_SET* bb_set) {
    Is_True(BS_Alloc_Size(_bs) >= BS_Alloc_Size(bb_set->_bs), ("elem out of bounds"));
    _bs = BS_UnionD(_bs, bb_set->_bs, _mpool);
    return *this;
  }
  BB_NODE_SET& Difference1D(BS_ELT x) {
    _bs = BS_Difference1D(_bs, x);
    return *this;
  }
  BB_NODE_SET& ClearD() {
    _bs = BS_ClearD(_bs);
    return *this;
  }

  BOOL MemberP(BS_ELT x) {
    return BS_MemberP(_bs, x);
  }

  // iteration operations
  BS_ELT First() const          { return BS_Choose(_bs);           }
  BS_ELT Next(BS_ELT cur) const { return BS_Choose_Next(_bs, cur); }

public:
  void Print(FILE* fp, int indent) const;
};

//===================================================================
// class BS_ELM_ITERATOR
//   iterators to traverse the elements in the BB_BODE_SET
//===================================================================
class BS_ELM_ITERATOR {
private:
  const BB_NODE_SET* _bb_set;
  BS_ELT _cur;

public:
  BS_ELM_ITERATOR() : _bb_set(0), _cur(BS_CHOOSE_FAILURE) {
  }
  BS_ELM_ITERATOR(const BB_NODE_SET* bb_set, BS_ELT cur)
    : _bb_set(bb_set), _cur(cur) {
  }
  // use default copy constructor and operator =

  BS_ELT operator *() {
    Is_True(_cur != BS_CHOOSE_FAILURE, ("invalid cur"));
    Is_True(_bb_set != NULL, ("invalid bb set"));
    return _cur;
  }
  BS_ELM_ITERATOR& operator++() {
    Is_True(_cur != BS_CHOOSE_FAILURE, ("invalid cur"));
    Is_True(_bb_set != NULL, ("invalid bb set"));
    _cur = _bb_set->Next(_cur);
  }
  bool operator==(const BS_ELM_ITERATOR& rit) {
    return _bb_set == rit._bb_set && _cur == rit._cur;
  }
  bool operator!=(const BS_ELM_ITERATOR& rit) {
    return _bb_set != rit._bb_set || _cur != rit._cur;
  }
};

//===================================================================
// class BB_NODE_BASE
//   template parameter:
//     _Tstmtcontainer: contains the STMTs inside the BB_NODE
//   member fields:
//     BB_NODE *_prev, *_next;
//     LIST<BB_NODE*> _preds, _succs;
//     BB_NODE *_idom, *_ipdom;
//     LIST<BB_NODE*> _dom_list, _pdom_list;
//     LIST<BB_NODE*> _df_list,  _cd_list;
//     UINT32 _id;
//     _Tstmtcontainer _stmts;
//   iterators:
//     stmt_iterator to iterate all stmts in this BB_NODE
//     bb_iterator to iterate preds/succs, dom/pdom/df/cd
//===================================================================
template<typename _Tstmtcontainer>
class BB_NODE_BASE {

public:
  typedef BB_NODE_BASE<_Tstmtcontainer> BB_NODE;
  typedef typename std::list<BB_NODE*> BB_LIST;
  typedef typename _Tstmtcontainer::STMT STMT;

  typedef typename BB_LIST::iterator bb_iterator;
  typedef typename BB_LIST::const_iterator const_bb_iterator;

  typedef typename BB_LIST::iterator dom_iterator;
  typedef typename BB_LIST::const_iterator const_dom_iterator;

  typedef typename _Tstmtcontainer::iterator stmt_iterator;
  typedef typename _Tstmtcontainer::const_iterator const_stmt_iterator;

private:
  BB_LIST _preds;  // list of predesessors
  BB_LIST _succs;  // list of successors

  BB_NODE* _idom;  // immediate dominator on CFG
  BB_NODE* _ipdom; // immediate dominator on RCFG
  BB_LIST _dom_list;   // bb list dominated immediately on CFG
  BB_LIST _pdom_list;  // bb list dominated immediately on RCFG
  BB_LIST _df_list;   // dominance frontier on CFG
  BB_LIST _cd_list;   // control dependence

  UINT32 _id;      // unique id

  _Tstmtcontainer _stmts;

public:
  BB_NODE_BASE(UINT32 id) 
    : _id(id), _idom(NULL), _ipdom(NULL) {
  }

public:
  UINT32 Get_id() const { return _id; }

  // stmt related methods
  void Add_stmt(STMT stmt) { _stmts.Add_stmt(stmt);      }
  STMT First_stmt() const  { return _stmts.First_stmt(); }
  STMT Last_stmt() const   { return _stmts.Last_stmt();  }
  BOOL Is_empty() const    { return _stmts.Is_empty();   }
  STMT Next_stmt(STMT stmt) const  { return _stmts.Next_stmt(stmt); }
  STMT Prev_stmt(STMT stmt) const  { return _stmts.Prev_stmt(stmt); }
  void Insert_before(STMT before, STMT stmt) {
    _stmts.Insert_before(before, stmt);
  }
  void Insert_after(STMT after, STMT stmt) {
    _stmts.Insert_after(after, stmt);
  }
  void Remove_stmt(STMT stmt)   { _stmts.Remove_stmt(stmt); }

  // preds/succs related methods
  void Add_pred(BB_NODE* pred) {
    bb_iterator it = std::find(_preds.begin(), _preds.end(), pred);
    FmtAssert(it == _preds.end(), ("pred has been added"));
    if (it == _preds.end())
      _preds.push_back(pred);
  }
  void Add_succ(BB_NODE* succ) {
    bb_iterator it = std::find(_succs.begin(), _succs.end(), succ);
    FmtAssert(it == _succs.end(), ("succ has been added"));
    if (it == _succs.end())
      _succs.push_back(succ);
  }
  bb_iterator Remove_pred(BB_NODE* pred) {
    bb_iterator it = std::find(_preds.begin(), _preds.end(), pred);
    FmtAssert(it != _preds.end(), ("Can not find pred"));
    return _preds.erase(it);
  }
  void Remove_all_preds() {
    _preds.clear();
  }
  bb_iterator Remove_succ(BB_NODE* succ) {
    bb_iterator it = std::find(_succs.begin(), _succs.end(), succ);
    FmtAssert(it != _preds.end(), ("Can not find succ"));
    return _succs.erase(it);
  }
  void Remove_all_succs() {
    _succs.clear();
  }
  INT32 Get_preds_count() const { return _preds.size(); }
  INT32 Get_succs_count() const { return _succs.size(); }
  const BB_NODE* Get_pred(INT index) const {
    Is_True(index >= 0 && index < _preds.size(), ("index out of bounds"));
    const_bb_iterator it = Pred_begin();
    for (int i=0; i < index; ++i)
      ++it;
    return *it;
  }
  const BB_NODE* Get_succ(INT index) const {
    Is_True(index >= 0 && index < _succs.size(), ("index out of bounds"));
    const_bb_iterator it = Succ_begin();
    for (int i=0; i < index; ++i)
      ++it;
    return *it;
  }
  INT32 Pred_pos(BB_NODE* pred) {
    INT32 pos = 0;
    for (const_bb_iterator it = Pred_begin();
         it != Pred_end();
         ++it) {
      if (*it == pred)
        return pos;
      ++pos;
    }
    return POS_INVALID;
  }
  INT32 Succ_pos(BB_NODE* succ) {
    INT32 pos = 0;
    for (const_bb_iterator it = Succ_begin();
         it != Succ_end();
         ++it) {
      if (*it == succ)
        return pos;
      ++pos;
    }
    return POS_INVALID;
  }

  // dominator and post-dominator related methods
  BB_NODE* Get_idom() const {
    Is_True(Get_preds_count() == 0 || _idom != NULL,
            ("idom is NULL. DOM is not built?"));
    return _idom;
  }
  void Set_idom(BB_NODE* node)  { _idom = node;  }
  BB_NODE* Get_ipdom() const {
    Is_True(Get_succs_count() == 0 || _ipdom != NULL,
            ("ipdom is NULL. PDOM is not built?"));
    return _ipdom;
  }
  void Set_ipdom(BB_NODE* node) { _ipdom = node; }
  BOOL Dominate(BB_NODE* node)  {
    Is_True(node != NULL, ("node is NULL"));
    do {
      if (node == this)
        return TRUE;
      else
        node = node->Get_idom();
    } while(node != NULL);
    return FALSE;
  }
  BOOL Post_dominate(BB_NODE* node) {
    Is_True(node != NULL, ("node is NULL"));
    do {
      if (node == this)
        return TRUE;
      else
        node = node->Get_ipdom();
    } while (node != NULL);
  }

  // iterators for preds/succs
  bb_iterator Pred_begin() { return _preds.begin(); }
  bb_iterator Pred_end()   { return _preds.end();   }
  const_bb_iterator Pred_begin() const { return _preds.begin(); }
  const_bb_iterator Pred_end() const { return _preds.end(); }
  bb_iterator Succ_begin() { return _succs.begin(); }
  bb_iterator Succ_end()   { return _succs.end();   }
  const_bb_iterator Succ_begin() const { return _succs.begin(); }
  const_bb_iterator Succ_end() const { return _succs.end(); }

  // iterators for dom/pdom
  dom_iterator Dom_begin()  { return _dom_list.begin(); }
  dom_iterator Dom_end()    { return _dom_list.end();   }
  const_dom_iterator Dom_begin() const { return _dom_list.begin(); }
  const_dom_iterator Dom_end() const   { return _dom_list.end();   }
  dom_iterator Pdom_begin()  { return _pdom_list.begin(); }
  dom_iterator Pdom_end()    { return _pdom_list.end();   }
  const_dom_iterator Pdom_begin() const { return _pdom_list.begin(); }
  const_dom_iterator Pdom_end() const   { return _pdom_list.end();   }

  // iterators for df/cd
  bb_iterator Df_begin()  { return _df_list.begin(); }
  bb_iterator Df_end()    { return _df_list.end();   }
  const_bb_iterator Df_begin() const { return _df_list.begin(); }
  const_bb_iterator Df_end() const   { return _df_list.end();   }
  bb_iterator Cd_begin()  { return _cd_list.begin(); }
  bb_iterator Cd_end()    { return _cd_list.end();   }
  const_bb_iterator Cd_begin() const { return _cd_list.begin(); }
  const_bb_iterator Cd_end() const   { return _cd_list.end();   }

  // iterators for stmt
  stmt_iterator Stmt_begin(STMT stmt = NULL) {
    return _stmts.begin(stmt);
  }
  stmt_iterator Stmt_end(STMT stmt = NULL) {
    return _stmts.end(stmt);
  }
  const_stmt_iterator Stmt_begin(STMT stmt = NULL) const {
    return _stmts.begin(stmt);
  }
  const_stmt_iterator Stmt_end(STMT stmt = NULL) const {
    return _stmts.end(stmt);
  }
  stmt_iterator Stmt_rbegin(STMT stmt = NULL) {
    return _stmts.rbegin(stmt);
  }
  stmt_iterator Stmt_rend(STMT stmt = NULL) {
    return _stmts.rend(stmt);
  }
  const_stmt_iterator Stmt_rbegin(STMT stmt = NULL) const {
    return _stmts.rbegin(stmt);
  }
  const_stmt_iterator Stmt_rend(STMT stmt = NULL) const { 
    return _stmts.rend(stmt);
  }

// The following section contains parameterized methods used by CFG utilities to
//   unify the processing of CFG and RCFG
private:
  // These interfaces are only used by DOM_BUILDER, DF_BUILDER and CFG_BASE
  template<typename _Tcfg> friend class DOM_BUILDER;
  template<typename _Tcfg> friend class DF_BUILDER;
  template<typename _Tcfg, BOOL _Fwd> friend class DFS_ITERATOR;
  template<typename _Tcfg, BOOL _Fwd> friend class PO_ITERATOR;
  template<typename _Tsc, typename _Tsm, typename _Tnc> friend class CFG_BASE;
  template<typename _Tcfg, typename _Twalker, BOOL _Dom> friend class DOM_WALKER_HELPER;

  // if dom is TRUE, _idom is returned
  BB_NODE* get_idom(bool dom) {
    return (dom) ? _idom : _ipdom;
  }
  // if dom is TRUE, _idom is set to node
  void set_idom(BB_NODE* node, bool dom) {
    if (dom)
      _idom = node;
    else
      _ipdom = node;
  }
  // if dom is TRUE, clear _dom_list, otherwise, clear _pdom_list
  void clear_dom_list(bool dom) {
    if (dom) {
      _dom_list.clear();
    }
    else {
      _pdom_list.clear();
    }
  }
  // if dom is TRUE, add node to _dom_list, otherwist, add to _pdom_list
  void add_to_dom_list(BB_NODE* node, bool dom) {
    if (dom)
      _dom_list.push_back(node);
    else
      _pdom_list.push_back(node);
  }
  // if df is TRUE, add node to _df_list, otherwise, add to _cd_list
  void add_to_df_list(BB_NODE* node, bool df) {
    Is_True(node != NULL, ("node is NULL"));
    if (df)
      _df_list.push_back(node);
    else
      _cd_list.push_back(node);
  }

  // iterators for preds/succs, if fwd is TRUE, _succs is visited
  bb_iterator bb_begin(BOOL fwd) { return (fwd) ? _succs.begin() : _preds.begin(); }
  bb_iterator bb_end(BOOL fwd)   { return (fwd) ? _succs.end() : _preds.end();     }
  const_bb_iterator bb_begin(BOOL fwd) const { return (fwd) ? _succs.begin() : _preds.begin(); }
  const_bb_iterator bb_end(BOOL fwd) const   { return (fwd) ? _succs.end() : _preds.end();     }

  // iterators for dom/pdom, if dom is TRUE, _dom_list is visited
  dom_iterator dom_begin(BOOL dom) { 
    if (dom) {
      return _dom_list.begin();
    }
    else {
      return _pdom_list.begin();
    }
  }
  dom_iterator dom_end(BOOL dom) {
    if (dom) {
      return _dom_list.end();
    }
    else {
      return _pdom_list.end();
    }
  }
  const_dom_iterator dom_begin(BOOL dom) const {
    if (dom) {
      return _dom_list.begin();
    }
    else {
      return _pdom_list.begin();
    }
  }
  const_dom_iterator dom_end(BOOL dom) const {
    if (dom) {
      return _dom_list.end();
    }
    else {
      return _pdom_list.end();
    }
  }

  // iterators for df/cd, if df is TRUE, _df_list is visited
  bb_iterator df_begin(BOOL df) { return (df) ? _df_list.begin() : _cd_list.begin(); }
  bb_iterator df_end(BOOL df)   { return (df) ? _df_list.end() : _cd_list.end();       }
  const_bb_iterator df_begin(BOOL df) const { return (df) ? _df_list.begin() : _cd_list.begin(); }
  const_bb_iterator df_end(BOOL df) const   { return (df) ? _df_list.end() : _cd_list.end();       }

public:
  // print and VCG dump routines
  void Print(FILE* fp, INT32 dump_flag) const {
    if (dump_flag & DUMP_ACFG) {
      fprintf(fp, "BB:%d\n", _id);
      fprintf(fp, "  Preds:");
      for (const_bb_iterator it = Pred_begin(); it != Pred_end(); ++it) {
        fprintf(fp, " %d", (*it)->_id);
      }
      fprintf(fp, "\n  Succs:");
      for (const_bb_iterator it = Succ_begin(); it != Succ_end(); ++it) {
        fprintf(fp, " %d", (*it)->_id);
      }
      fprintf(fp, "\n");
    }
    if (dump_flag & DUMP_ADOM) {
      fprintf(fp, "  Idom: %d\tIpdom: %d\n", 
        _idom ? _idom->_id : (-1), _ipdom ? _ipdom->_id : (-1));
      fprintf(fp, "  Dom list:");
      for (const_dom_iterator it = Dom_begin(); it != Dom_end(); ++it) {
        fprintf(fp, " %d", (*it)->Get_id());
      }
      fprintf(fp, "\n  Pdom list:");
      for (const_dom_iterator it = Pdom_begin(); it != Pdom_end(); ++it) {
        fprintf(fp, " %d", (*it)->Get_id());
      }
      fprintf(fp, "\n");
    }
    if (dump_flag & DUMP_ADF) {
      fprintf(fp, "  DF list:");
      for (const_bb_iterator it = Df_begin(); it != Df_end(); ++it) {
        fprintf(fp, " %d", (*it)->_id);
      }
      fprintf(fp, "\n  CD list:");
      for (const_bb_iterator it = Cd_begin(); it != Cd_end(); ++it) {
        fprintf(fp, " %d", (*it)->_id);
      }
      fprintf(fp, "\n");
    }
    _stmts.Print(fp, dump_flag);
  }

  VCGNode* VCG_dump(MEM_POOL* mpool, VCGGraph& vcg, INT32 dump_flag) const {
    char* title = (char *) MEM_POOL_Alloc(mpool, 16); // should be enough for BB:nnnnnn
    snprintf(title, 16, "BB:%d", _id);
    VCGNode* bb_node = CXX_NEW(VCGNode(title, title, Ellipse), mpool);
    vcg.addNode(*bb_node);
    if (dump_flag & DUMP_AIR) {
      VCGNode* bb_stmt = _stmts.VCG_dump(mpool, vcg, dump_flag);
      char* bs_title = (char *) MEM_POOL_Alloc(mpool, 16); // should be enough for IR:nnnnnn
      snprintf(bs_title, 16, "IR:%d", _id);
      bb_stmt->title(bs_title);
      VCGEdge* edge = CXX_NEW(VCGEdge(bb_stmt->title(), title), mpool);
      edge->lineStyle(Dotted);
      vcg.addEdge(*edge);
    }
    if (dump_flag & DUMP_ADF) {
      std::stringstream ss;
      ss << "DF list:\n";
      for (const_bb_iterator it = Df_begin(); it != Df_end(); ++it) {
        ss << (*it)->_id << " ";
      }
      ss << "\nCD list:\n";
      for (const_bb_iterator it = Cd_begin(); it != Cd_end(); ++it) {
        ss << (*it)->_id << " ";
      }
      char* df_title = (char *) MEM_POOL_Alloc(mpool, 16); // should be enough for DF:nnnnnn
      snprintf(df_title, 16, "DF:%d", _id);
      char* df_label = (char *) MEM_POOL_Alloc(mpool, ss.str().size() + 1);
      strcpy(df_label, ss.str().c_str());
      VCGNode* df_node = CXX_NEW(VCGNode(df_title, df_label), mpool);
      vcg.addNode(*df_node);
      VCGEdge* edge = CXX_NEW(VCGEdge(df_node->title(), title), mpool);
      edge->lineStyle(Dotted);
      vcg.addEdge(*edge);
    }
    return bb_node;
  }
};


//===================================================================
// BB_SET_ITERATOR
//   iterators to traverse the BB_NODE_SET but returns the BB_NODE*
//   template parameter
//     _Tcfg: the type of CFG
//===================================================================
template<typename _Tcfg>
class BB_SET_ITERATOR {
public:
  typedef typename _Tcfg::BB_NODE BB_NODE;

private:
  BS_ELT _cur;
  BB_NODE_SET* _bb_set;
  _Tcfg* _cfg;

public:
  BB_SET_ITERATOR()
    : _cur(BS_CHOOSE_FAILURE), _bb_set(NULL), _cfg(NULL) { }
  BB_SET_ITERATOR(BS_ELT cur, BB_NODE_SET* bb_set, _Tcfg* cfg)
    : _cur(cur), _bb_set(bb_set), _cfg(cfg) { }
  BB_SET_ITERATOR(const BB_SET_ITERATOR& rit)
    : _cur(rit._cur), _bb_set(rit._bb_set), _cfg(rit._cfg) { }

public:
  BB_NODE* operator->() {
    Is_True(_cur != BS_CHOOSE_FAILURE, ("invalid cur"));
    Is_True(_bb_set != NULL, ("invalid bb set"));
    Is_True(_cfg != NULL, ("invalid cfg"));
    return _cfg->Get_node((UINT32)_cur);
  }
  BB_NODE& operator*() {
    Is_True(_cur != BS_CHOOSE_FAILURE, ("invalid cur"));
    Is_True(_bb_set != NULL, ("invalid bb set"));
    Is_True(_cfg != NULL, ("invalid cfg"));
    return *(_cfg->Get_node((UINT32)_cur));
  }
  BB_SET_ITERATOR& operator++() {
    Is_True(_cur != BS_CHOOSE_FAILURE, ("invalid cur"));
    Is_True(_bb_set != NULL, ("invalid bb set"));
    Is_True(_cfg != NULL, ("invalid cfg"));
    _cur = _bb_set->Next(_cur);
    return *this;
  }
  bool operator==(const BB_SET_ITERATOR& rit) const {
    return (_cur == rit._cur) &&
           (_bb_set == rit._bb_set) &&
           (_cfg == rit._cfg);
  }
  bool operator!=(const BB_SET_ITERATOR& rit) const {
    return !(operator==(rit));
  }
  BB_SET_ITERATOR& operator=(const BB_SET_ITERATOR& rit) {
    _cur = rit._cur;
    _bb_set = rit._bb_set;
    _cfg = rit._cfg;
  }
};


//===================================================================
// DFS_ITERATOR
//   iterators to traverse the CFG in deep-first order
//   template parameter
//     _Tcfg: type of cfg
//     _Fwd: BOOL, traverse the CFG(TRUE) or RCFG(FALSE)
//       CFG: traverse from entry to exit by the succs list
//       RCFG: traverse from exit to entry by the preds list
//===================================================================
template<typename _Tcfg, BOOL _Fwd>
class DFS_ITERATOR {

public:
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef typename _Tcfg::BB_LIST BB_LIST;

private:
  BB_NODE* _current_node;
  BB_LIST  _dfs_list;
  std::vector<bool> _visited;

  // add node to _dfs_list. the node must not be visited
  void Add_node(BB_NODE* node) {
    Is_True(_visited[node->Get_id()] == false, ("node has been visited"));
    _dfs_list.insert(_dfs_list.begin(), node);
    _visited[node->Get_id()] = true;
  }

public:
  DFS_ITERATOR() : _current_node(NULL) { }
  DFS_ITERATOR(BB_NODE* node, int max_size) : _current_node(node) {
    _visited.resize(max_size, false);
    if (node != NULL) {
      for (typename BB_LIST::iterator it = _current_node->bb_begin(_Fwd);
           it != _current_node->bb_end(_Fwd);
           ++it) {
        Is_True(_current_node->Get_id() < _visited.size(), ("node id out of bounds"));
        Is_True(_visited[_current_node->Get_id()] == false,
                ("node has been visited"));
        Add_node(*it);
      }
    }
  }
  DFS_ITERATOR(const DFS_ITERATOR<_Tcfg, _Fwd>& rhs)
    : _current_node(rhs._current_node),
      _dfs_list(rhs._dfs_list),
      _visited(rhs._visited) { }

public:
  BB_NODE* operator->() { 
    Is_True(_current_node != NULL, ("current node is NULL"));
    return _current_node;
  }
  BB_NODE& operator* () {
    Is_True(_current_node != NULL, ("current node is NULL"));
    return *_current_node;
  }

  DFS_ITERATOR<_Tcfg, _Fwd>& operator++() {
    Is_True(_current_node != NULL, ("current node is NULL"));
    _visited[_current_node->Get_id()] = true;
    if (_dfs_list.size() > 0) {
      _current_node = _dfs_list.front();
      _dfs_list.pop_front();
      Is_True(_current_node->Get_id() < _visited.size(), ("node id out of bounds"));
      for (typename BB_LIST::iterator it = _current_node->bb_begin(_Fwd);
           it != _current_node->bb_end(_Fwd);
           ++it) {
        Is_True((*it)->Get_id() < _visited.size(), ("node id out of bounds"));
        if (_visited[(*it)->Get_id()] == false) {
            Add_node(*it);
        }
      }
    }
    else {
      _current_node = NULL;
    }
    return *this;
  }

  bool operator==(const DFS_ITERATOR<_Tcfg, _Fwd>& rit) {
    // since each node can be only visited once, ignore the _dfs_list;
    return (_current_node == rit._current_node);
  }

  bool operator!=(const DFS_ITERATOR<_Tcfg, _Fwd>& rit) {
    return !(operator==(rit));
  }

  DFS_ITERATOR<_Tcfg, _Fwd>& operator=(const DFS_ITERATOR<_Tcfg, _Fwd>& rit) {
    _current_node = rit._current_node;
    _dfs_list = rit._dfs_list;
    _visited = rit._visited;
  }
};



//===================================================================
// PO_ITERATOR
//   iterators to traverse the CFG in postorder
//   template parameter
//     _Tcfg: type of cfg
//     _Fwd: BOOL, traverse the CFG(TRUE) or RCFG(FALSE)
//       CFG: traverse from entry to exit by the succs list
//       RCFG: traverse from exit to entry by the preds list
//===================================================================
template<typename _Tcfg, BOOL _Fwd>
class PO_ITERATOR {

public:
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef typename _Tcfg::BB_LIST BB_LIST;

private:
  BB_NODE* _current_node;        // current node to be processed
  std::list<BB_NODE*> _ancestors;  // _ancestors of _current_node
  std::list<typename BB_LIST::iterator> _iter_list; // _ancestors' iterators pointing to the next kid
  std::vector<bool> _visited;

  void goto_next_node() {
    if (_ancestors.empty()) {
      _current_node = NULL;
      return;
    }

    BB_NODE* parent = _ancestors.front();
    typename BB_LIST::iterator iter = _iter_list.front();

    while (iter != parent->bb_end(_Fwd)) {
      if (_visited[(*iter)->Get_id()]) {
	iter++;
	continue;
      }

      Push_node(*iter); // push this kid onto the stack
      parent = _ancestors.front();
      iter = _iter_list.front();
    }

    _current_node = Pop_node();
  }

  // add a node to _ancestors. The node must not have been visited.
  void Push_node(BB_NODE* node) {
    Is_True(_visited[node->Get_id()] == false, ("node has been visited"));
    _visited[node->Get_id()] = true;

    _ancestors.push_front(node);
    _iter_list.push_front(node->bb_begin(_Fwd));
  }

  BB_NODE* Pop_node() {
    BB_NODE* node = _ancestors.front();
    _ancestors.pop_front();
    _iter_list.pop_front();

    return node;
  }

public:
  PO_ITERATOR() : _current_node(NULL) { }
  PO_ITERATOR(BB_NODE* node, int max_size) : _current_node(node) {
    _visited.resize(max_size, false);
    if (node == NULL) return;

    Push_node(node);
    goto_next_node();
  }
  PO_ITERATOR(const PO_ITERATOR<_Tcfg, _Fwd>& rhs)
    : _current_node(rhs._current_node),
      _ancestors(rhs._ancestors),
      _iter_list(rhs._iter_list),
      _visited(rhs._visited) { }

public:
  BB_NODE* operator->() { 
    Is_True(_current_node != NULL, ("current node is NULL"));
    return _current_node;
  }
  BB_NODE& operator* () {
    Is_True(_current_node != NULL, ("current node is NULL"));
    return *_current_node;
  }

  PO_ITERATOR<_Tcfg, _Fwd>& operator++() {
    Is_True(_current_node != NULL, ("current node is NULL"));
    goto_next_node();
    return *this;
  }

  bool operator==(const PO_ITERATOR<_Tcfg, _Fwd>& rit) {
    // since each node can be only visited once, ignore the _iter_list;
    return (_current_node == rit._current_node);
  }

  bool operator!=(const PO_ITERATOR<_Tcfg, _Fwd>& rit) {
    return !(operator==(rit));
  }

  PO_ITERATOR<_Tcfg, _Fwd>& operator=(const PO_ITERATOR<_Tcfg, _Fwd>& rit) {
    _current_node = rit._current_node;
    _ancestors = rit._ancestors;
    _iter_list = rit._iter_list;
    _visited = rit._visited;
  }
};



//===================================================================
// DOM_WALKER_HELPER
//   take a walker to traverse the DOM tree in deep-first order
//   template parameter:
//     _Tnode: type of bb node
//     _Twalker: the walker, which has two public methods: Visit_push() and Visit_pop
//     _Dom: BOOL, traverse the dom(TRUE) or pdom tree(FALSE)
//===================================================================
template<typename _Tcfg, typename _Twalker, BOOL _Dom>
class DOM_WALKER_HELPER {

private:
  typedef typename _Tcfg::BB_NODE BB_NODE;

private:
  _Tcfg&     _cfg;
  _Twalker& _walker;
#ifdef Is_True_On
  std::vector<bool> _visited; // each node should only be visited once
#endif

public:
  DOM_WALKER_HELPER(_Tcfg& cfg, _Twalker& walker)
    : _cfg(cfg), _walker(walker) { }

private:
  void traverse_rec(BB_NODE* node) {
#ifdef Is_True_On
    Is_True(node != NULL, ("node is NULL"));
    Is_True(_visited[node->Get_id()] == false, ("node %d already visited", node->Get_id()));
    _visited[node->Get_id()] = true;
#endif

    _walker.Visit_push(node);
    for (typename BB_NODE::dom_iterator it = node->dom_begin(_Dom);
         it != node->dom_end(_Dom);
         ++it) {
      BB_NODE* dom = *it;
      Is_True(dom->get_idom(_Dom) == node, ("idom node mismatch"));
      traverse_rec(dom);
    }
    _walker.Visit_pop(node);
  }

public:
  void Traverse() {
#ifdef Is_True_On
    if (_visited.size() > 0)
      _visited.clear();
    _visited.resize(_cfg.Get_max_id() + 1, false);
#endif
    traverse_rec(_cfg.get_start_point(_Dom));
  }
};


//===================================================================
// class CFG_BASE
//   template parameters:
//     _Tstmtcontainer: type for statement container
//     _Tstmtmapper: type for the mapping between statement and node
//     _Tnodecontainer: type for node container
//   member variables:
//     BB_NODE *_dummy_entry;
//     BB_NODE *_dummp_exits;
//     _Tstmtmapper _stmt_mapper;
//     _Tnodecontainer _node_container;
//===================================================================
template<typename _Tstmtcontainer, typename _Tstmtmapper, typename _Tnodecontainer>
class CFG_BASE {

public:
  typedef typename _Tstmtcontainer::STMT STMT;
  typedef BB_NODE_BASE<_Tstmtcontainer> BB_NODE;
  typedef typename BB_NODE::BB_LIST BB_LIST;

  typedef DFS_ITERATOR<BB_NODE, TRUE> dfs_fwd_iterator;
  typedef DFS_ITERATOR<BB_NODE, FALSE> dfs_bwd_iterator;
  typedef PO_ITERATOR<BB_NODE, TRUE> po_dfs_iterator;
  typedef PO_ITERATOR<BB_NODE, FALSE> rpo_dfs_iterator;

  typedef typename _Tnodecontainer::iterator bb_iterator;
  typedef typename _Tnodecontainer::const_iterator const_bb_iterator;

  typedef BB_SET_ITERATOR< CFG_BASE<_Tstmtcontainer, _Tstmtmapper, _Tnodecontainer> > bb_set_iterator;

private:
  BB_NODE* _dummy_entry;  // dummy entry for the CFG
  BB_NODE* _dummy_exit;   // dummy exit for the CFG
  MEM_POOL*  _cfg_pool;

private:
  _Tnodecontainer _node_container;   // contains all nodes
  _Tstmtmapper    _stmt_mapper;      // maintain the mapping from statement to bb node 

public:
  CFG_BASE(MEM_POOL* pool) : _cfg_pool(pool) {
    _dummy_entry = Create_node();
    _dummy_exit = Create_node();
  }

public:
  MEM_POOL* Mem_pool() { return _cfg_pool; }

  // node operations
  BB_NODE* Create_node()           { return _node_container.Create_node(_cfg_pool); }
  void Delete_node(BB_NODE* node)  {
    Disconnect_node(node);  // remove all edges on the node
    Disconnect_all_stmt_in_node(node);  // disconnect all stmts in the node
    _node_container.Delete_node(node, _cfg_pool);  // delete the node
  }
  UINT32 Get_max_id() const        { return _node_container.Get_max_id(); }
  BB_NODE* Get_node(UINT32 id)     { return _node_container.Get_node(id); }
  BB_NODE* Get_dummy_entry() const { return _dummy_entry; }
  BB_NODE* Get_dummy_exit() const  { return _dummy_exit;  }

  // edge operations
  void Add_entry_node(BB_NODE* entry) {
    Is_True(entry != NULL, ("entry is null"));
    Is_True(_dummy_entry != NULL, ("dummy entry is null"));
    _dummy_entry->Add_succ(entry);
    entry->Add_pred(_dummy_entry);
  }
  void Add_exit_node(BB_NODE* exit) {
    Is_True(exit != NULL, ("exit is null"));
    Is_True(_dummy_exit != NULL, ("dummy exit is null"));
    exit->Add_succ(_dummy_exit);
    _dummy_exit->Add_pred(exit);
  }
  void Connect_predsucc(BB_NODE* pred, BB_NODE* succ) {
    Is_True(pred != NULL, ("Pred is null"));
    Is_True(succ != NULL, ("Succ is null"));
    pred->Add_succ(succ);
    succ->Add_pred(pred);
  }
  void Disconnect_predsucc(BB_NODE* pred, BB_NODE* succ) {
    Is_True(pred != NULL, ("Pred is null"));
    Is_True(succ != NULL, ("Succ is null"));
    pred->Remove_succ(succ);
    succ->Remove_pred(pred);
  }
  void Disconnect_node(BB_NODE* node) {
    for (typename BB_NODE::bb_iterator it = node->Pred_begin();
         it != node->Pred_end();
         ++it) {
      (*it)->Remove_succ(node);
    }
    for (typename BB_NODE::bb_iterator it = node->Succ_begin();
         it != node->Succ_end();
         ++it) {
      (*it)->Remove_pred(node);
    }
    node->Remove_all_preds();
    node->Remove_all_succs();
  }

  // stmt to node mappings
  void Connect_stmt_node(STMT stmt, BB_NODE* node) {
    Is_True(node != NULL, ("node is NULL"));
    _stmt_mapper.Connect_stmt_node(stmt, node);
  }
  void Disconnect_stmt_node(STMT stmt, BB_NODE* node) {
    Is_True(node != NULL, ("node is NULL"));
    _stmt_mapper.Disconnect_stmt_node(stmt, node);
  }
  void Disconnect_all_stmt_in_node(BB_NODE* node) {
    Is_True(node != NULL, ("node is NULL"));
    _stmt_mapper.Disconnect_all_stmt_in_node(node);
  }
  BB_NODE* Get_stmt_node(STMT stmt) const {
    return _stmt_mapper.Get_stmt_node(stmt);
  }

  // DFS iterators
  dfs_fwd_iterator Dfs_fwd_begin() { dfs_fwd_iterator it(Get_dummy_entry(), Get_max_id()); return it;  }
  dfs_fwd_iterator Dfs_fwd_end()   { dfs_fwd_iterator it(NULL, 0); return it; }
  dfs_bwd_iterator Dfs_bwd_begin() { dfs_bwd_iterator it(Get_dummy_exit(), Get_max_id()); return it; }
  dfs_bwd_iterator Dfs_bwd_end()   { dfs_bwd_iterator it(NULL, 0); return it; }

  // Postorder iterators
  po_dfs_iterator Po_dfs_begin() { po_dfs_iterator it(Get_dummy_entry(), Get_max_id()); return it; }
  po_dfs_iterator Po_dfs_end()   { po_dfs_iterator it(NULL, 0); return it; }
  rpo_dfs_iterator Rpo_dfs_begin() { rpo_dfs_iterator it(Get_dummy_exit(), Get_max_id()); return it; }
  rpo_dfs_iterator Rpo_dfs_end()   { rpo_dfs_iterator it(NULL, 0); return it; }

  // BB iterators
  bb_iterator BB_begin() { return _node_container.begin(); }
  bb_iterator BB_end()   { return _node_container.end();   }
  const_bb_iterator BB_begin() const { return _node_container.begin(); }
  const_bb_iterator BB_end() const   { return _node_container.end();   }

  // BB Set iterators
  bb_set_iterator BB_set_begin(BB_NODE_SET* bs) {
    return bb_set_iterator(bs->First(), bs, this);
  }
  bb_set_iterator BB_set_end(BB_NODE_SET* bs) {
    return bb_set_iterator(BS_CHOOSE_FAILURE, bs, this);
  }

protected:
  // insert/delete stmt from node
  void Insert_before(STMT before, STMT stmt) {
    Is_True(before != NULL && stmt != NULL, ("insert point or stmt is null"));
    BB_NODE* node = Get_stmt_node(before);
    Is_True(node != NULL, ("can not find BB for insert point"));
    node->Insert_before(before, stmt);
    Connect_stmt_node(stmt, node);
  }
  void Insert_after(STMT after, STMT stmt) {
    Is_True(after != NULL && stmt != NULL, ("insert point or stmt is null"));
    BB_NODE* node = Get_stmt_node(after);
    Is_True(node != NULL, ("can not find BB for insert point"));
    node->Insert_after(after, stmt);
    Connect_stmt_node(stmt, node);
  }
  void Add_stmt(BB_NODE* node, STMT stmt) {
    Is_True(stmt != NULL, ("stmt is null"));
    Is_True(node != NULL, ("BB is not empty"));
    node->Add_stmt(stmt);
    Connect_stmt_node(stmt, node);
  }
  void Remove_stmt(STMT stmt) {
    Is_True(stmt != NULL, ("stmt is null"));
    BB_NODE* node = Get_stmt_node(stmt);
    Is_True(node != NULL, ("can not find BB for stmt"));
    node->Remove_stmt(stmt);
    Disconnect_stmt_node(stmt, node);
  }
  // split node at top into two nodes and return the new one
  BB_NODE* Split_node(BB_NODE* node) {
    Is_True(node != NULL, ("node is null"));
    BB_NODE* new_bb = Create_node();
    // transfer node's preds to new_bb
    for (typename BB_NODE::bb_iterator bit = node->Pred_begin();
         bit != node->Pred_end();
         ++bit) {
      Connect_predsucc(*bit, new_bb);
    }
    node->Remove_all_preds();
    return new_bb;
  }

// The following section contains parameterized methods used by CFG utilities to
//   unify the processing of CFG and RCFG
private:
  // These interfaces are only used by DOM_BUILDER and DF_BUILDER
  template<typename _Tcfg> friend class DOM_BUILDER;
  template<typename _Tcfg> friend class DF_BUILDER;
  template<typename _Tcfg, typename _Twalker, BOOL _Dom> friend class DOM_WALKER_HELPER;

  // if fwd is TRUE, return _dummy_entry
  BB_NODE* get_start_point(bool fwd) { return (fwd) ? _dummy_entry : _dummy_exit; }
  // if fwd is TRUE, return _dummy_exit
  BB_NODE* get_end_point(bool fwd)   { return (fwd) ? _dummy_exit : _dummy_entry; }

  // if dom is TRUE, clear node's dom information
  void clear_dom_info(BB_NODE* node, bool dom) {
    Is_True(node != NULL, ("node is NULL"));
    node->set_idom(NULL, dom);
    node->clear_dom_list(dom); 
  }
  // if dom is TRUE, set parent to be child's immediate dominator
  void connect_dom_node(BB_NODE* parent, BB_NODE* child, bool dom) {
    Is_True(child != NULL, ("child node is NULL"));
    child->set_idom(parent, dom);
    if (parent != NULL) {
      parent->add_to_dom_list(child, dom);
    }
  }

public:
  // Print and VCG_dump routines
  void Print(FILE* fp, INT32 dump_flag) const {
    fprintf(fp, "CFG dump:\n");
    _node_container.Print(fp, dump_flag);
  }
  void VCG_dump(const char* fname, INT32 dump_flag) const {
    MEM_POOL vcg_pool;
    MEM_POOL_Initialize(&vcg_pool, "CFG VCG pool", FALSE);
    VCGGraph vcg("CFG VCG dump");
    VCGNode** vcg_nodes = (VCGNode **) MEM_POOL_Alloc(&vcg_pool, sizeof(VCGNode*) * (Get_max_id() + 1));
    // create nodes
    for (const_bb_iterator it = BB_begin();
         it != BB_end();
         ++it) {
      const BB_NODE* node = (*it);
      VCGNode* vcg_node = node->VCG_dump(&vcg_pool, vcg, dump_flag);
      vcg_nodes[node->Get_id()] = vcg_node;
    }
    // connect preds, succs
    if (dump_flag & DUMP_ACFG) {
      for (const_bb_iterator bb_it = BB_begin();
           bb_it != BB_end();
           ++bb_it) {
        const BB_NODE* node = (*bb_it);
        if (dump_flag & DUMP_CFG) {
          for (typename BB_NODE::const_bb_iterator succ_it = node->Succ_begin();
               succ_it != node->Succ_end();
               ++succ_it) {
            const BB_NODE* succ = (*succ_it);
            VCGEdge* edge = CXX_NEW(VCGEdge(vcg_nodes[node->Get_id()]->title(),
                                            vcg_nodes[succ->Get_id()]->title()), &vcg_pool);
            edge->lineStyle(Continuous);
            vcg.addEdge(*edge);
          }
        }
        if (dump_flag & DUMP_DOM) {
          const BB_NODE* idom = node->Get_idom();
          if (idom != NULL) {
            VCGEdge* edge = CXX_NEW(VCGEdge(vcg_nodes[idom->Get_id()]->title(),
                                            vcg_nodes[node->Get_id()]->title()), &vcg_pool);
            edge->lineStyle(Dashed);
            edge->color(Black);
            vcg.addEdge(*edge);
          }
        }
        if (dump_flag & DUMP_PDOM) {
          const BB_NODE* ipdom = node->Get_ipdom();
          if (ipdom != NULL) {
            VCGEdge* edge = CXX_NEW(VCGEdge(vcg_nodes[ipdom->Get_id()]->title(),
                                            vcg_nodes[node->Get_id()]->title()), &vcg_pool);
            edge->lineStyle(Dashed);
            edge->color(Blue);
            vcg.addEdge(*edge);
          }
        }
      }
    }
    // emit vcg
    vcg.emit(fname);
    MEM_POOL_Delete(&vcg_pool);
  }
};


//===================================================================
// BASIC_NODE_CONTAINER
//   Create/Delete the nodes from MEM_POOL
//   Using array to track all the nodes allocated
// Attention:
//   id starts from 1 but the index to array start from 0
//===================================================================
template<typename _Tnode>
class BASIC_NODE_CONTAINER {

public:
  typedef _Tnode BB_NODE;
  typedef typename std::vector<BB_NODE*> BB_VECTOR;
  typedef typename BB_VECTOR::iterator iterator;
  typedef typename BB_VECTOR::const_iterator const_iterator;

private:
  BB_VECTOR _node_vector;  // to track all allocated BB_NODE
  UINT32 _max_id;

public:
  BASIC_NODE_CONTAINER() : _max_id(1) { }

public:
  // bb node methods
  BB_NODE* Create_node(MEM_POOL* pool) {
    BB_NODE* node = CXX_NEW(BB_NODE(_max_id), pool);
    Is_True(node != NULL, ("create node failed"));
    _node_vector.push_back(node);
    ++_max_id;
    return node;
  }
  void Delete_node(BB_NODE* node, MEM_POOL* pool) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(node->Get_id() <= _node_vector.size(), ("id out of bounds"));
    // TODO: reuse the vector index
    _node_vector[node->Get_id()-1] = NULL;
    CXX_DELETE(node, pool);
  }
  UINT32 Get_max_id() const {
    return _max_id;
  }
  BB_NODE* Get_node(UINT32 id) {
    Is_True(id <= _node_vector.size(), ("id out of bounds"));
    BB_NODE* node = _node_vector[id-1];
    Is_True(node->Get_id() == id, ("id mismatch"));
    return node;
  }

  // iterators
  iterator begin() { return _node_vector.begin(); }
  iterator end()   { return _node_vector.end();   }
  const_iterator begin() const { return _node_vector.begin(); }
  const_iterator end() const   { return _node_vector.end();   }

public:
  // Print and VCG_dump routines
  void Print(FILE* fp, INT32 dump_flag) const {
    for (typename BB_VECTOR::const_iterator it = _node_vector.begin();
         it != _node_vector.end();
         ++it) {
      const BB_NODE* node = (*it);
      node->Print(fp, dump_flag);
    }
  }
  void VCG_dump(MEM_POOL* mpool, VCGGraph& vcg, INT32 dump_flag) const {
    for (typename BB_VECTOR::const_iterator it = _node_vector.begin();
         it != _node_vector.end();
         ++it) {
      const BB_NODE* node = (*it);
      node->VCG_dump(mpool, vcg, dump_flag);
    }
  }
};

} /* namespace CFG_UTIL */

#endif /* cfg_base_INCLUDED */

