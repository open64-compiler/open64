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
// Module: cfg_util.h
//
// Revision history:
//  Nov-1 - Original Version
//
// Description:
//  Handle unreachable BB, calculating DOM/PDOM/DF/CD
//
// Exported classes:
//  CFG_UTIL::DOM_BUILDER
//  CFG_UTIL::DF_BUILDER
//  CFG_UTIL::CFG_CONNECTIVITY
//
// SEE ALSO:
//  be/com/cfg_base.h (BB_NODE_BASE, CFG_BASE)
//
//====================================================================

#ifndef cfg_util_INCLUDED
#define cfg_util_INCLUDED

#include "cfg_base.h"
#include "bitset.h"
#include "tracing.h"

namespace CFG_UTIL {

//===================================================================
// class DOM_BUILDER
//  Build DOM and PDOM
//  template parameters:
//    _Tcfg: type of CFG
//  constructor:
//    DOM_BUILDER(CFG_TYPE& cfg);
//  interfaces:
//    void Build_DOM(): Build dom tree
//    void Build_PDOM(): Build pdom tree
//  pre-condition:
//    CFG preds/succs
//===================================================================
template<typename _Tcfg>
class DOM_BUILDER {

private:
  typedef _Tcfg CFG_TYPE;
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef typename _Tcfg::bb_set_iterator bb_set_iterator;

private:
  class DOM_REC {

  public:
    INT32 _parent;
    INT32 _ancestor;
    INT32 _child;
    INT32 _vertex;
    INT32 _dom;
    INT32 _label;
    INT32 _semi;
    INT32 _size;
    BB_NODE_SET*   _bucket;

  public:
    DOM_REC() : _bucket(NULL) { }
    ~DOM_REC() { }
    void Init(INT32 bbs, MEM_POOL* rec_pool) {
      _bucket = CXX_NEW(BB_NODE_SET(bbs, rec_pool), rec_pool);
      _parent = 0;
      _ancestor = 0;
      _child = 0;
      _vertex = 0;
      _dom = 0;
      _label = 0;
      _semi = 0;
      _size = 0;
    }
  };
  MEM_POOL  _dom_pool;  // pool for dom builder itself
  INT32 _counter;
  DOM_REC  *_recs;
  CFG_TYPE& _cfg;

public:
  DOM_BUILDER(CFG_TYPE& cfg)
    : _cfg(cfg), _counter(0), _recs(NULL) {
    MEM_POOL_Initialize(&_dom_pool, "DOM POOL", FALSE);
    MEM_POOL_Push(&_dom_pool);
  }
  ~DOM_BUILDER() {
    MEM_POOL_Pop(&_dom_pool);
  }

private:
  INT32 Parent(INT32 n)   { return _recs[n]._parent;   }
  INT32 Ancestor(INT32 n) { return _recs[n]._ancestor; }
  INT32 Child(INT32 n)    { return _recs[n]._child;    }
  INT32 Vertex(INT32 n)   { return _recs[n]._vertex;   }
  INT32 Dom(INT32 n)      { return _recs[n]._dom;      }
  INT32 Label(INT32 n)    { return _recs[n]._label;    }
  INT32 Semi(INT32 n)     { return _recs[n]._semi;     }
  INT32 Size(INT32 n)     { return _recs[n]._size;     }
  BB_NODE_SET* Bucket(INT32 n) { return _recs[n]._bucket; }
  void Set_parent(INT32 n, INT32 p)   { _recs[n]._parent = p;   }
  void Set_ancestor(INT32 n, INT32 a) { _recs[n]._ancestor = a; }
  void Set_child(INT32 n, INT32 c)    { _recs[n]._child = c;    }
  void Set_vertex(INT32 n, INT32 v)   { _recs[n]._vertex = v;   }
  void Set_dom(INT32 n, INT32 d)      { _recs[n]._dom = d;      }
  void Set_label(INT32 n, INT32 l)    { _recs[n]._label = l;    }
  void Set_semi(INT32 n, INT32 s)     { _recs[n]._semi = s;     }
  void Set_size(INT32 n, INT32 s)     { _recs[n]._size = s;     }

  void Init() {
    INT32 bbs = _cfg.Get_max_id() + 1;
    _recs = CXX_NEW_ARRAY(DOM_REC, bbs, &_dom_pool);
    for (int i = 0; i < bbs; ++i) {
      _recs[i].Init(bbs, &_dom_pool);
    }
    _counter = 0;
  }

  // if _Fwd is true, deep-first traverse the CFG
  // otherwise, traverse RCFG
  template<bool _Fwd>
  void DFS(BB_NODE* node) {
    INT32 v = node->Get_id();

    ++_counter;
    Set_semi(v, _counter);
    Set_vertex(_counter, v);
    Set_label(v, v);
    Set_ancestor(v, 0);
    Set_child(v, 0);
    Set_size(v, 1);

    _cfg.clear_dom_info(node, _Fwd);

    typename BB_NODE::bb_iterator it;
    for (it = node->bb_begin(_Fwd); it != node->bb_end(_Fwd); ++it) {
      INT32 succ_id = (*it)->Get_id();
      if (Semi(succ_id) == 0) {
        Set_parent(succ_id, v);
        DFS<_Fwd>(*it);
      }
    }
  }
  void Compress(INT32 v) {
    Is_True(Ancestor(v) != 0, ("Ancestor(v) is 0"));
    if (Ancestor(Ancestor(v)) != 0) {
      Compress(Ancestor(v));
      if (Semi(Label(Ancestor(v))) < Semi(Label(v))) {
        Set_label(v, Label(Ancestor(v)));
      }
      Set_ancestor(v, Ancestor(Ancestor(v)));
    }
  }
  INT32 Eval(BB_NODE* node) {
    INT32 v = node->Get_id();
    if (Ancestor(v) == 0) {
      return (Label(v));
    }
    else {
      Compress(v);
      return ((Semi(Label(Ancestor(v))) >= Semi(Label(v))) ?
                 Label(v) : Label(Ancestor(v)));
    }
  }
  void Link(BB_NODE* bbv, BB_NODE* bbw) {
    INT32 v = bbv->Get_id();
    INT32 w = bbw->Get_id();
    INT32 s = w;

    while (Semi(Label(w)) < Semi(Label(Child(s)))) {
      if (Size(s) + Size(Child(Child(s))) >= 2*Size(Child(s))) {
        Set_ancestor(Child(s), s);
        Set_child(s, Child(Child(s)));
      }
      else {
        Set_size(Child(s), Size(s));
        Set_ancestor(s, Child(s));
        s = Child(s);
      }
    }

    Set_label(s, Label(w)); 
    Set_size(v, Size(v) + Size(w));

    if (Size(v) < 2*Size(w)) {
      INT32 tmps = s;
      s = Child(v);
      Set_child(v, tmps);
    }

    while (s != 0) {
      Set_ancestor(s, v);
      s = Child(s);
    }
  }

  // if _Fwd is true, compute the dom tree
  // otherwise, compute the pdom tree
  template<bool _Fwd>
  void Compute_dom_tree() {
    // step 1: dfs
    DFS<_Fwd> (_cfg.get_start_point(_Fwd));

    // initialize vertex 0
    Set_size(0, 0);
    Set_label(0, 0);
    Set_semi(0, 0);

    for (INT32 i = _counter; i >= 2 ; i--) {
      INT32 w = Vertex(i);
      Is_True(w!=0, ("Vertex(i) is 0"));
      BB_NODE* bbw = _cfg.Get_node(w);

      // step 2
      typename BB_NODE::bb_iterator it;
      for (it = bbw->bb_begin(!_Fwd); it != bbw->bb_end(!_Fwd); ++it) {
        INT32 u = Eval(*it);
        if (Semi(u) < Semi(w)) {
          Set_semi(w, Semi(u));
        }
      }

      Bucket(Vertex(Semi(w)))->Union1D(bbw->Get_id());
      Link(_cfg.Get_node(Parent(w)),  bbw);

      // step 3
      // traverse bucket
      BB_NODE* bbv;
      BB_NODE_SET* bs = Bucket(Parent(w));
      for (bb_set_iterator it = _cfg.BB_set_begin(bs);
           it != _cfg.BB_set_end(bs);
           ++it) {
        INT32 u = Eval(&(*it));
        INT32 v = it->Get_id();;
        Bucket(Parent(w))->Difference1D(v);
        Set_dom(v, (Semi(u) < Semi(v)) ? u : Parent(w));
      }
    }

    // step 4
    for (INT32 j = 2; j <= _counter; ++j) {
      INT32 w = Vertex(j);
      if (Dom(w) != Vertex(Semi(w))) {
        Set_dom(w, Dom(Dom(w)));
      }
    }
  }

  // if _Fwd is TRUE, build dom tree
  // othersize, build pdom tree
  template<bool _Fwd>
  void Build_dom_tree() {
    Init();
    Compute_dom_tree<_Fwd>();
    BB_NODE* start_node = _cfg.get_start_point(_Fwd);

    typename CFG_TYPE::bb_iterator it;
    for (it = _cfg.BB_begin();
         it != _cfg.BB_end();
         ++it) {
      INT32 v = (*it)->Get_id();
      BB_NODE* node = _cfg.Get_node(v);
      if (Dom(v) != 0) {
        // Set idom, dom_bbs
        BB_NODE* dom = _cfg.Get_node(Dom(v));
        _cfg.connect_dom_node(dom, node, _Fwd);
      }
      else {
        if (node == start_node || Label(node->Get_id()) == 0) {
           // node is entry or not visited during DFS
           _cfg.connect_dom_node(NULL, node, _Fwd);
        }
        else {
           FmtAssert(FALSE, ("node %d doesn't have a %sdom", node->Get_id(), _Fwd ? "" : "p"));
        }
      }
    }
  }

public:
  void Build_DOM() {
    Build_dom_tree<true>();
  }
  void Build_PDOM() {
    Build_dom_tree<false>();
  }
};


//===================================================================
// DF_BUILDER
//   Build DF and CD for CFG
//   template parameter:
//     _Tcfg: CFG type
//   constructor:
//     DF_BUILDER(CFG_TYPE& cfg);
//   public method:
//     void Build_DF(): Build DF for cfg
//     void Build_CD(): Build CD for cfg
//   pre-condition:
//     CFG preds/succs and DOM/PDOM available
//===================================================================
template<typename _Tcfg>
class DF_BUILDER {

private:
  typedef _Tcfg CFG_TYPE;
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef typename _Tcfg::bb_set_iterator bb_set_iterator;

private:
  class DF_REC {
  public:
    BB_NODE_SET*   _df_set;
    DF_REC() : _df_set(NULL) { }
    void Init(INT32 bbs, MEM_POOL* rec_pool) {
      _df_set = CXX_NEW(BB_NODE_SET(bbs, rec_pool), rec_pool);
    }
  };
  MEM_POOL  _df_pool;   // pool for df builder itself
  DF_REC  *_recs;
  CFG_TYPE& _cfg;

public:
  DF_BUILDER(CFG_TYPE& cfg)
    : _cfg(cfg), _recs(NULL) {
    MEM_POOL_Initialize(&_df_pool, "DF POOL", FALSE);
    MEM_POOL_Push(&_df_pool);
  }
  ~DF_BUILDER() {
    MEM_POOL_Pop(&_df_pool);
  }

private:
  BB_NODE_SET* DF_set(INT32 v) { return _recs[v]._df_set; }
  void Init() {
    INT32 bbs = _cfg.Get_max_id() + 1;
    _recs = CXX_NEW_ARRAY(DF_REC, bbs, &_df_pool);
    for (int i = 0; i < bbs; ++i) {
      _recs[i].Init(bbs, &_df_pool);
    }
  }

  // if _Fwd is TRUE, compute the dominance frontier recursively
  // otherwise, compute the control dependence recursively
  template<bool _Fwd>
  void Compute_DF_rec(BB_NODE* node) {
    // bottom up traversal on dom tree
    typename BB_NODE::dom_iterator xit;
    for (xit = node->dom_begin(_Fwd); xit != node->dom_end(_Fwd); ++xit) {
      Is_True((*xit)->get_idom(_Fwd) == node, ("idom node mismatch"));
      Compute_DF_rec<_Fwd>(*xit);
    }

    // start from empty set
    BB_NODE_SET* node_df = DF_set(node->Get_id());
    typename BB_NODE::bb_iterator bit;
    for (bit = node->bb_begin(_Fwd); bit != node->bb_end(_Fwd); ++bit) {
      if ((*bit)->get_idom(_Fwd) != node) {
        node_df->Union1D((*bit)->Get_id());
      }
    }
    // traverse the dominated blocks
    typename BB_NODE::dom_iterator yit;
    for (yit = node->dom_begin(_Fwd); yit != node->dom_end(_Fwd); ++yit) {
      Is_True((*yit)->get_idom(_Fwd) == node, ("idom node mismatch"));
      BB_NODE_SET* ydf = DF_set((*yit)->Get_id());
      // for each element of Dominance-Frontier of yit
      for (bb_set_iterator zit = _cfg.BB_set_begin(ydf);
           zit != _cfg.BB_set_end(ydf);
           ++zit) {
        if (zit->get_idom(_Fwd) != node) {
          node_df->Union1D(zit->Get_id());
        }
      }
    }
    // convert bs to dom_list
    for (bb_set_iterator dit = _cfg.BB_set_begin(node_df);
         dit != _cfg.BB_set_end(node_df);
         ++dit) {
      node->add_to_df_list(&(*dit), _Fwd);
    }
  }

  // if _Fwd is TRUE, compute the dominance frontier
  // otherwise, compute the control dependence
  template<bool _Fwd>
  void Compute_DF() {
    Init();
    Compute_DF_rec<_Fwd>(_cfg.get_start_point(_Fwd));
  }

public:
  void Build_DF() { Compute_DF<true>();  }
  void Build_CD() { Compute_DF<false>(); }

};


//===================================================================
// CFG_CONNECTIVITY
//   Identify unreached and not exit block on CFG
//   template parameter:
//     _Tcfg: CFG type
//   constructor:
//     CFG_CONNECTIVITY(CFG_TYPE& cfg);
//   public method:
//     Perform(): Identify the unreachable and not exit blocks
//   pre-condition:
//     CFG preds/succs
//===================================================================
template<typename _Tcfg>
class CFG_CONNECTIVITY {

private:
  typedef _Tcfg CFG_TYPE;
  typedef typename _Tcfg::BB_NODE BB_NODE;

private:
  typedef std::vector<bool> VISITED_ARRAY;
  _Tcfg& _cfg;
  BOOL _trace;

public:
  CFG_CONNECTIVITY(_Tcfg& cfg, BOOL trace = FALSE) 
    : _cfg(cfg), _trace(trace) { }

private:
  void Init(VISITED_ARRAY& visited) {
    if (visited.size() > 0)
      visited.clear();
    visited.resize(_cfg.Get_max_id() + 1, false);
  }

  void Mark_node_reachable(BB_NODE* node, VISITED_ARRAY& visited) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(node->Get_id() <= _cfg.Get_max_id(), ("node id out of bounds"));
    if (visited[node->Get_id()] == true)
      return;
    visited[node->Get_id()] = true;
    if (_trace) {
      fprintf(TFile, "WCFG: BB:%d reachable\n", node->Get_id());
    }
    BOOL no_exit = TRUE;
    for (typename BB_NODE::bb_iterator it = node->Succ_begin();
         it != node->Succ_end();
         ++it) {
      if (*it != node) {
        if (no_exit == TRUE)
          no_exit = FALSE;
        Mark_node_reachable(*it, visited);
      }
    }
    if (no_exit && node != _cfg.Get_dummy_exit()) {
      // if node doesn't have succ, connect to dummy exit
      if (_trace) {
        fprintf(TFile, "WCFG: BB:%d is reachable but no other succ, connect to dummy exit\n", node->Get_id());
      }
      _cfg.Add_exit_node(node);
    }
  }

  void Mark_node_exit(BB_NODE* node, VISITED_ARRAY& visited) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(node->Get_id() <= _cfg.Get_max_id(), ("node id out of bounds"));
    if (visited[node->Get_id()] == true)
      return;
    visited[node->Get_id()] = true;
    if (_trace) {
      fprintf(TFile, "WCFG: BB:%d has exit\n", node->Get_id());
    }
    for (typename BB_NODE::bb_iterator it = node->Pred_begin();
         it != node->Pred_end();
         ++it) {
      Mark_node_exit(*it, visited);
    }
  }

  // if node is unreachable, disconnect it from the cfg
  void Disconnect_unreached(VISITED_ARRAY& visited) {
    for (typename CFG_TYPE::bb_iterator it = _cfg.BB_begin();
         it != _cfg.BB_end();
         ++it) {
      if (visited[(*it)->Get_id()] == false) {
        if (_trace) {
          fprintf(TFile, "WCFG: BB:%d not reachable, disconnect from CFG\n", (*it)->Get_id());
        }
        _cfg.Disconnect_node(*it);
      }
    }
  }

  // if node can not reach exit, connect it with the dummy exit
  void Connect_noexit(BB_NODE* node, VISITED_ARRAY& visited, VISITED_ARRAY& instack, VISITED_ARRAY& will_exit) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(node->Get_id() <= _cfg.Get_max_id(), ("node id out of bounds"));
    int succ_visited = 0;
    if (visited[node->Get_id()] == false) {
      visited[node->Get_id()] = true;
      instack[node->Get_id()] = true;
      for (typename BB_NODE::const_bb_iterator it = node->Succ_begin();
           it != node->Succ_end();
           ++it) {
        if (instack[(*it)->Get_id()] == false) {
          Connect_noexit(*it, visited, instack, will_exit);
          ++succ_visited;
        }
      }
      instack[node->Get_id()] = false;
      if (succ_visited == 0 && will_exit[node->Get_id()] == false &&
          node != _cfg.Get_dummy_exit()) {
        if (_trace) {
          fprintf(TFile, "WCFG: BB:%d has no exit, connect to dummy exit\n", node->Get_id());
        }
        _cfg.Add_exit_node(node);
        will_exit[node->Get_id()] = true;
      }
    }
  }

  // handle not reachable node
  void Process_not_reached() {
    VISITED_ARRAY visited;
    Init(visited);
    Mark_node_reachable(_cfg.Get_dummy_entry(), visited);
    Disconnect_unreached(visited);
  }

  // handle not exit node
  void Process_not_exit() {
    VISITED_ARRAY will_exit;
    Init(will_exit);
    Mark_node_exit(_cfg.Get_dummy_exit(), will_exit);
    VISITED_ARRAY instack;
    VISITED_ARRAY visited;
    Init(instack);
    Init(visited);
    Connect_noexit(_cfg.Get_dummy_entry(), visited, instack, will_exit); 
  }

public:
  void Perform() {
    Process_not_reached();
    Process_not_exit();
  }
};

} /* namespace CFG_UTIL */

#endif /* cfg_util_INCLUDED */

