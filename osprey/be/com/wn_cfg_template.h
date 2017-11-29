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
// Module: wn_cfg_template.h
//
// Revision history:
//  Jan-16-2011 - Original Version
//
// Description:
//  Template method for traversal of WN_TREE to build PARENTMAP 
//  or CFG
//
// Exported classes:
//  CFG_UTIL::WN_CFG
//
// SEE ALSO:
//  be/com/wn_cfg.h   (WN_CFG)
//  be/com/cfg_base.h (BB_NODE_BASE, CFG_BASE)
//
//====================================================================

#ifndef wn_cfg_template_INCLUDED
#define wn_cfg_template_INCLUDED

#include "cfg_base.h"
#include "wn.h"
#include "region_util.h"
#include <ext/hash_map>
#include <ext/hash_set>
#include <vector>
using __gnu_cxx::hash_map;
using __gnu_cxx::hash_set;
using std::vector;

namespace CFG_UTIL {

//===================================================================
// class PARENTMAP_ACTION_BASE
//  base class to build or verify the parent map
//===================================================================
template<typename _Tcfg>
class PARENTMAP_ACTION_BASE {
public:
  void Initialize() {
  }

  void Finalize() {
  }
};

//===================================================================
// class PARENTMAP_BUILD_ACTION
//  class to build the parent map
//===================================================================
template<typename _Tcfg>
class PARENTMAP_BUILD_ACTION : public PARENTMAP_ACTION_BASE<_Tcfg> {
private:
  _Tcfg& _cfg;

public:
  PARENTMAP_BUILD_ACTION(_Tcfg& cfg) : _cfg(cfg) {
  }

public:
  void Process(WN* parent, WN* kid) {
    _cfg.Set_parent(parent, kid);
  }
};

//===================================================================
// class PARENTMAP_VERIFY_ACTION
//  class to verfy the parent map
//===================================================================
template<typename _Tcfg>
class PARENTMAP_VERIFY_ACTION : public PARENTMAP_ACTION_BASE<_Tcfg> {
private:
  _Tcfg& _cfg;

public:
  PARENTMAP_VERIFY_ACTION(_Tcfg& cfg) : _cfg(cfg) {
  }

public:
  void Process(WN* parent, WN* kid) const {
    Is_True(_cfg.Get_parent(kid) == parent, 
            ("PARENTMAP VERIFY: parent and kid mismatch"));
  }
};

//===================================================================
// class WN_TREE_TRAVERSE
//  template class to traverse the WN TREE
//===================================================================
template<typename _Ttraverser>
class WN_TREE_TRAVERSE {
private:
  _Ttraverser& _traverser;

public:
  WN_TREE_TRAVERSE(_Ttraverser& traverser) : _traverser(traverser) {
  }

private:
  void Traverse_rec(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) != OPERATOR_UNKNOWN,
            ("invalid WHIRL node"));

    if (WN_operator(wn) == OPR_BLOCK) {
      for (WN* item = WN_first(wn); item != NULL; item = WN_next(item)) {
        _traverser.Process(wn, item);
        Traverse_rec(item);
      }
    }
    else {
      for (INT i = 0; i < WN_kid_count(wn); ++i) {
        _traverser.Process(wn, WN_kid(wn, i));
        Traverse_rec(WN_kid(wn, i));
      }
    }
  }

public:
  void Traverse(WN* wn) {
    Is_True(wn != NULL &&
            (WN_operator(wn) == OPR_FUNC_ENTRY ||
             WN_operator(wn) == OPR_REGION),
            ("invalid WHIRL node"));

    _traverser.Initialize();
    Traverse_rec(wn);
    _traverser.Finalize();
  }
};

//===================================================================
// class WN_CFG_ACTION_BASE
//  base class to build or verify the WHIRL CFG
//===================================================================
template<typename _Tcfg>
class WN_CFG_ACTION_BASE {
public:
  typedef typename _Tcfg::BB_NODE BB_NODE;

protected:
  _Tcfg& _cfg;

public:
  WN_CFG_ACTION_BASE(_Tcfg& cfg) : _cfg(cfg) {
  }

public:
  REGION_LEVEL Get_rgn_level() {
    return _cfg.Get_rgn_level();
  }
 
  BB_NODE* Get_dummy_entry() {
    return _cfg.Get_dummy_entry();
  }

  BB_NODE* Get_dummy_exit() {
    return _cfg.Get_dummy_exit();
  }
};

//===================================================================
// class WN_CFG_BUILD_ACTION
//  class to build the WHIRL CFG
//===================================================================
template<typename _Tcfg>
class WN_CFG_BUILD_ACTION : public WN_CFG_ACTION_BASE<_Tcfg> {
public:
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef WN_CFG_ACTION_BASE<_Tcfg> ACTION_BASE;

private:
  typedef hash_map<INT32, vector<BB_NODE*> > GOTO_MAP;
  typedef hash_map<INT32, BB_NODE*> LABEL_MAP;
  GOTO_MAP   _goto_map;   // map from label_num to WN* for GOTO
  LABEL_MAP  _label_map; 

public:
  WN_CFG_BUILD_ACTION(_Tcfg& cfg) : WN_CFG_ACTION_BASE<_Tcfg> (cfg) {
  }

  void Initialize() {
  }

  void Finalize() {
#ifdef Is_True_On
    Is_True(_goto_map.empty(), ("found unresolved GOTOs"));
    for (typename LABEL_MAP::iterator it = _label_map.begin();
         it != _label_map.end();
         ++it) {
      WN* stmt = it->second->First_stmt();
      Is_True(stmt != NULL && WN_operator(stmt) == OPR_LABEL,
              ("first stmt is not LABEL"));
      Is_True(WN_label_number(stmt) == it->first,
              ("LABEL number mismatch"));
    }
#endif    
  }

public:
  void Process_root(WN* root) {
    ACTION_BASE::_cfg.Set_wn_root(root);
  }

  void Process_rgn_level(REGION_LEVEL level) {
    ACTION_BASE::_cfg.Set_rgn_level(level);
  }

  void Process_stmt(BB_NODE* node, WN* stmt) {
    Is_True(node != NULL, ("invalid WCFG node"));
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));

    node->Add_stmt(stmt);
    ACTION_BASE::_cfg.Connect_stmt_node(stmt, node);
  }

  void Process_predsucc(BB_NODE* pred, BB_NODE* succ) {
    Is_True(pred != NULL && succ != NULL,
            ("invalid pred or succ node"));

    ACTION_BASE::_cfg.Connect_predsucc(pred, succ);
  }

  void Process_entry_node(BB_NODE* entry) {
    Is_True(entry != NULL, ("invalid node"));

    ACTION_BASE::_cfg.Add_entry_node(entry);
  }

  void Process_exit_node(BB_NODE* exit) {
    Is_True(exit != NULL, ("invalid node"));

    ACTION_BASE::_cfg.Add_exit_node(exit);
  }

  void Process_goto(BB_NODE* node, WN* stmt) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(stmt != NULL &&
            (WN_operator(stmt) == OPR_GOTO ||
             WN_operator(stmt) == OPR_CASEGOTO ||
             WN_operator(stmt) == OPR_TRUEBR ||
             WN_operator(stmt) == OPR_FALSEBR ||
             WN_operator(stmt) == OPR_REGION_EXIT),
            ("WN is not goto or casegoto"));
  
    typename LABEL_MAP::iterator lit = _label_map.find(WN_label_number(stmt));
    if (lit != _label_map.end()) {
      ACTION_BASE::_cfg.Connect_predsucc(node, lit->second);
    }
    else {
      vector<BB_NODE*>& gotos = _goto_map[WN_label_number(stmt)];
      gotos.push_back(node);
    }
  }

  void Process_label(BB_NODE* node, WN* stmt) {
    Is_True(node != NULL, ("node is NULL"));
    Is_True(stmt != NULL && WN_operator(stmt) == OPR_LABEL,
            ("stmt is not LABEL"));

    FmtAssert(_label_map.find(WN_label_number(stmt)) == _label_map.end(),
              ("found duplicated labels"));

    _label_map[WN_label_number(stmt)] = node;
    ACTION_BASE::_cfg.Set_label(WN_label_number(stmt), node);

    typename GOTO_MAP::iterator vit = _goto_map.find(WN_label_number(stmt));
    if (vit != _goto_map.end()) {
      vector<BB_NODE*>& gotos = vit->second;
      for (typename vector<BB_NODE*>::iterator nit = gotos.begin();
           nit != gotos.end();
           ++nit) {
        ACTION_BASE::_cfg.Connect_predsucc(*nit, node);
      }
      _goto_map.erase(vit);
    }
  }

  BB_NODE* Create_node(BB_NODE* node, WN* stmt) {
    return ACTION_BASE::_cfg.Create_node();
  }

  BOOL Need_new_node(BB_NODE* node, WN* stmt) {
    Is_True(node != NULL, ("node is NULL"));

    return !node->Is_empty();
  }

};

//===================================================================
// class WN_CFG_VERIFY_ACTION
//  class to verify the WHIRL CFG
//===================================================================
template<typename _Tcfg>
class WN_CFG_VERIFY_ACTION : public  WN_CFG_ACTION_BASE<_Tcfg> {
public:
  typedef typename _Tcfg::BB_NODE BB_NODE;
  typedef WN_CFG_ACTION_BASE<_Tcfg> ACTION_BASE;

private:
  BB_NODE* _verify_node;
  WN*      _verify_stmt;
  typedef hash_set<INTPTR> NODE_SET;
  vector<NODE_SET> _verify_preds;
  vector<NODE_SET> _verify_succs;

private:
  BOOL Is_stmt_in_node(BB_NODE* node, WN* stmt) {
    if (node == _verify_node && node->Next_stmt(_verify_stmt) == stmt) {
      _verify_stmt = stmt;
      return TRUE;
    }
    if (node->First_stmt() == stmt) {
      _verify_node = node;
      _verify_stmt = stmt;
      return TRUE;
    }
    return FALSE;
  }

  BOOL Is_stmt_goto_label(WN* stmt, WN* label) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    Is_True(label != NULL && WN_operator(label) != OPERATOR_UNKNOWN,
            ("invalid label"));

    OPERATOR opr = WN_operator(stmt);
    if (opr != OPR_GOTO &&
        opr != OPR_CASEGOTO &&
        opr != OPR_TRUEBR &&
        opr != OPR_FALSEBR &&
        opr != OPR_REGION_EXIT &&
        opr != OPR_SWITCH &&
        opr != OPR_COMPGOTO)
      return FALSE;
    if (WN_operator(label) != OPR_LABEL)
      return FALSE;

    if (opr == OPR_GOTO || opr == OPR_CASEGOTO ||
        opr == OPR_TRUEBR || opr == OPR_FALSEBR ||
        opr == OPR_REGION_EXIT) {
      return WN_label_number(stmt) == WN_label_number(label);
    }
    else {
      // OPR_SWITCH and OPR_COMPGOTO
      for (WN* goto_wn = WN_first(WN_switch_table(stmt));
           goto_wn != NULL;
           goto_wn = WN_next(goto_wn)) {
        if (WN_label_number(goto_wn) == WN_label_number(label))
          return TRUE;
      }
      if (WN_kid_count(stmt) > 2 &&
          WN_label_number(WN_switch_default(stmt)) == WN_label_number(label))
        return TRUE;
      else
        return FALSE;
    }
  }

  BOOL Is_kid_of_stmt(WN* stmt, WN* kid) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    Is_True(kid != NULL && WN_operator(kid) != OPERATOR_UNKNOWN,
            ("invalid label"));

    if (WN_operator(stmt) != OPR_SWITCH &&
        WN_operator(stmt) != OPR_COMPGOTO)
      return FALSE;
    if (WN_operator(kid) != OPR_GOTO &&
        WN_operator(kid) != OPR_CASEGOTO)
      return FALSE;

    for (WN* goto_wn = WN_first(WN_switch_table(stmt));
         goto_wn != NULL;
         goto_wn = WN_next(goto_wn)) {
      if (goto_wn == kid)
        return TRUE;
    }
    if (WN_kid_count(stmt) > 2 &&
        WN_switch_default(stmt) == kid)
      return TRUE;
    else
      return FALSE;
  }

  BOOL Is_stmt_adjacent(WN* prev, WN* next) {
    Is_True(prev != NULL && WN_operator(prev) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    Is_True(next != NULL && WN_operator(next) != OPERATOR_UNKNOWN,
            ("invalid label"));

    while (prev != NULL && WN_next(prev) == NULL) {
      prev = ACTION_BASE::_cfg.Get_parent(prev);
    }
    while (next != NULL && WN_prev(next) == NULL) {
      next = ACTION_BASE::_cfg.Get_parent(next);
    }

    if (prev == NULL || next == NULL)
      return FALSE;

    if (WN_next(prev) == next) {
      Is_True(WN_prev(next) == prev, ("invalid prev and next"));
      return TRUE;
    }
    else {
      return FALSE;
    }
  }

public:
  WN_CFG_VERIFY_ACTION(_Tcfg& cfg) : WN_CFG_ACTION_BASE<_Tcfg> (cfg) {
  }

  void Initialize() {
    _verify_node = NULL;
    _verify_stmt = NULL;
    _verify_preds.resize(ACTION_BASE::_cfg.Get_max_id(), NODE_SET());
    _verify_succs.resize(ACTION_BASE::_cfg.Get_max_id(), NODE_SET());
  }

  void Finalize() {
    BB_NODE* dummy_exit = ACTION_BASE::Get_dummy_exit();
    for (typename _Tcfg::dfs_fwd_iterator bit = ACTION_BASE::_cfg.Dfs_fwd_begin();
         bit != ACTION_BASE::_cfg.Dfs_fwd_end();
         ++bit) {
      UINT32 bb_id = bit->Get_id();
      if (bit->Get_preds_count() != _verify_preds[bb_id].size()) {
        if (dummy_exit != &(*bit)) { // we possibly add edges to dummy exit
          Is_True(FALSE, ("WCFG VERIFY: missing edge"));
        }
      }
      if (bit->Get_succs_count() != _verify_succs[bb_id].size()) {
        if (bit->Succ_pos(dummy_exit) == POS_INVALID ||
            bit->Get_succs_count() != _verify_succs[bb_id].size() + 1) {
          Is_True(FALSE, ("WCFG VERIFY: missing edge"));
        }
      }
    }
  }

public:
  void Process_root(WN* root) {
    Is_True(ACTION_BASE::_cfg.Get_wn_root() == root, 
            ("WCFG VERIFY: root mismatch"));
    Is_True(ACTION_BASE::_cfg.Get_dummy_entry()->First_stmt() == root,
            ("WCFG VERIFY: root in dummy entry mismatch"));
  }

  void Process_rgn_level(REGION_LEVEL level) {
    Is_True(ACTION_BASE::_cfg.Get_rgn_level() == level, 
            ("WCFG VERIFY: region level mismatch"));
  }

  void Process_stmt(BB_NODE* node, WN* stmt) {
    Is_True(node != NULL, ("invalid WCFG node"));
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    Is_True(ACTION_BASE::_cfg.Get_wn_node(stmt) == node, 
            ("WCFG VERIFY: stmt to node mapping mismatch"));
    Is_True(Is_stmt_in_node(node, stmt),
            ("WCFG VERIFY: node to stmt mapping mismatch"));
  }

  void Process_predsucc(BB_NODE* pred, BB_NODE* succ) {
    Is_True(pred != NULL && succ != NULL, ("invalid WCFG node"));

    INT32 succ_pos = pred->Succ_pos(succ);
    INT32 pred_pos = succ->Pred_pos(pred);
    NODE_SET& succ_set = _verify_succs[pred->Get_id()];
    NODE_SET& pred_set = _verify_preds[succ->Get_id()];

    Is_True(succ_pos != POS_INVALID &&
            pred->Get_succ(succ_pos) == succ,
            ("WCFG VERIFY: can not find succ"));
    Is_True(pred_pos != POS_INVALID &&
            succ->Get_pred(pred_pos) == pred,
            ("WCFG VERIFY: can not find pred"));
    Is_True(succ_set.find((INTPTR)succ) == succ_set.end(),
            ("WCFG VERIFY: duplicated succ"));
    Is_True(pred_set.find((INTPTR)pred) == pred_set.end(),
            ("WCFG VERIFY: duplicated pred"));

    succ_set.insert((INTPTR)succ);
    pred_set.insert((INTPTR)pred);
  }

  void Process_entry_node(BB_NODE* entry) {
    Is_True(entry != NULL, ("invalid entry node"));

    BB_NODE* dummy_entry = ACTION_BASE::Get_dummy_entry();
    INT32 pos = dummy_entry->Succ_pos(entry);

    Is_True(entry->Get_preds_count() == 1,
            ("WCFG VERIFY: entry can only have 1 predecessor"));
    Is_True(entry->Get_pred(0) == dummy_entry,
            ("WCFG VERIFY: predecessor of entry node must be dummy entry"));
    Is_True(pos != POS_INVALID &&
            dummy_entry->Get_succ(pos) == entry,
            ("WCFG VERIFY: entry node is not connected to dummy entry"));

    Process_predsucc(dummy_entry, entry);
  }

  void Process_exit_node(BB_NODE* exit) {
    Is_True(exit != NULL, ("invalid exit node"));

    BB_NODE* dummy_exit = ACTION_BASE::Get_dummy_exit();
    INT32 pos = dummy_exit->Pred_pos(exit);

    Is_True(exit->Get_succs_count() == 1,
            ("WCFG VERIFY: exit can only have 1 successor"));
    Is_True(exit->Get_succ(0) == dummy_exit,
            ("WCFG VERIFY: successor of exit node must be dummy exit"));
    Is_True(pos != POS_INVALID &&
              dummy_exit->Get_pred(pos) == exit,
              ("WCFG VERIFY: exit node is not connected to dummy exit"));

    Process_predsucc(exit, dummy_exit);
  }

  void Process_goto(BB_NODE* node, WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    Is_True(node != NULL, ("node is NULL"));
    Is_True((WN_operator(stmt) == OPR_GOTO ||
             WN_operator(stmt) == OPR_CASEGOTO ||
             WN_operator(stmt) == OPR_TRUEBR ||
             WN_operator(stmt) == OPR_FALSEBR ||
             WN_operator(stmt) == OPR_REGION_EXIT),
            ("stmt is not goto or casegoto"));
    Is_True((node->Last_stmt() == stmt ||
             Is_kid_of_stmt(node->Last_stmt(), stmt)),
            ("WCFG VERIFY: stmt is not the last of node"));

    INT32 label_number = WN_label_number(stmt);
    BB_NODE* succ = ACTION_BASE::_cfg.Get_label_node(label_number);
    WN* label_wn = succ->First_stmt();

    Is_True(succ != NULL,
            ("WCFG VERIFY: can not find label"));
    Is_True((label_wn != NULL && 
             WN_operator(label_wn) == OPR_LABEL &&
             WN_label_number(label_wn) == WN_label_number(stmt)),
            ("WCFG VERIFY: invalid label for goto"));

    Process_predsucc(node, succ);
  }

  void Process_label(BB_NODE* node, WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) == OPR_LABEL,
            ("stmt is not label"));
    Is_True(node != NULL, ("node is NULL"));

    WN* label_wn = node->First_stmt();
    Is_True(label_wn == stmt,
            ("WCFG VERIFY: invalid label for node"));

    if (WN_Label_Is_Handler_Begin(stmt) ||
        LABEL_target_of_goto_outer_block(WN_label_number(stmt))) {
      BB_NODE* dummy_entry = ACTION_BASE::Get_dummy_entry();
      INT32 succ_pos = dummy_entry->Succ_pos(node);
      INT32 pred_pos = node->Pred_pos(dummy_entry);

      Is_True((succ_pos != POS_INVALID &&
               dummy_entry->Get_succ(succ_pos) == node),
              ("WCFG VERIFY: handler node is not connected to dummy entry"));
      Is_True((pred_pos != POS_INVALID &&
               node->Get_pred(pred_pos) == dummy_entry),
              ("WCFG VERIFY: handler node is not connected to dummy entry"));
      return; 
    }

    for (typename BB_NODE::bb_iterator bit = node->Pred_begin();
         bit != node->Pred_end();
         ++bit) {
      WN* last_stmt = (*bit)->Last_stmt();
      switch (WN_operator(last_stmt)) {
      case OPR_GOTO:
      case OPR_CASEGOTO:
      case OPR_REGION_EXIT:
        Is_True(WN_label_number(last_stmt) == WN_label_number(stmt),
                ("WCFG VERIFY: invalid label for goto"));
        break;
      case OPR_TRUEBR:
      case OPR_FALSEBR:
        if (WN_label_number(last_stmt) != WN_label_number(stmt)) {
          Is_True(Is_stmt_adjacent(last_stmt, stmt),
                  ("WCFG VERIFY: invalid fall through node"));
        }
        break;
      case OPR_COMPGOTO:
      case OPR_SWITCH:
        Is_True(Is_stmt_goto_label(last_stmt, stmt), 
                ("WCFG VERIFY: invalid label for goto"));
        break;
      default:
        Is_True(Is_stmt_adjacent(last_stmt, stmt),
                ("WCFG VERIFY: invalid fall through node"));
        break;
      }
    }
  }

  BB_NODE* Create_node(BB_NODE* node, WN* stmt) {
    if (stmt == NULL) {
      Is_True(node != NULL, ("invalid node"));
      UINT32 bb_id = node->Get_id();
      for (typename BB_NODE::bb_iterator bit = node->Succ_begin();
           bit != node->Succ_end();
           ++bit) {
        BB_NODE* succ = *bit;
        if (_verify_succs[bb_id].find((INTPTR)succ) != _verify_succs[bb_id].end())
          continue;
        if (succ->Is_empty())
          return succ;
      }
      Is_True(FALSE, ("WCFG VERIFY: can not verify node for stmt"));
      return NULL;
    }
    else {
      while (WN_operator(stmt) == OPR_REGION) {
        stmt = WN_first(WN_region_body(stmt));
        FmtAssert(stmt != NULL, ("stmt is NULL"));
      }
      Is_True(WN_operator(stmt) != OPR_BLOCK, ("invalid stmt"));

      BB_NODE* new_node = ACTION_BASE::_cfg.Get_wn_node(stmt);
      Is_True(new_node != NULL && new_node->First_stmt() == stmt,
              ("WCFG VERIFY: invalid first stmt for node"));
      return new_node;
    }
  }

  BOOL Need_new_node(BB_NODE* node, WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN, 
            ("stmt is NULL"));
    Is_True(node != NULL, ("node is NULL"));

    while (WN_operator(stmt) == OPR_REGION) {
      stmt = WN_first(WN_region_body(stmt));
      Is_True(stmt != NULL, ("stmt is NULL"));
    }
    Is_True(WN_operator(stmt) != OPR_BLOCK, ("invalid stmt"));

    return node->First_stmt() != stmt;
  }

};

//===================================================================
// class WN_CFG_TRAVERSE
//  helper class to traverse the WHIRL to build/verify CFG
//===================================================================
template<typename _Taction>
class WN_CFG_TRAVERSE {
public:
  typedef typename _Taction::BB_NODE BB_NODE;

private:
  _Taction& _action;
  BB_NODE* _current_bb; 

public:
  WN_CFG_TRAVERSE(_Taction& action)
    : _action(action), _current_bb(NULL) {
  }

  void Traverse(WN* tree) {
    Is_True(tree != NULL && WN_operator(tree) != OPERATOR_UNKNOWN, 
            ("tree is NULL"));

    if (WN_operator(tree) == OPR_REGION) {
      FmtAssert(FALSE, ("TODO: build CFG for region"));
    }
    Is_True(WN_operator(tree) == OPR_FUNC_ENTRY, 
            ("tree is not FUNC_ENTRY"));

    _action.Initialize();

    _action.Process_root(tree);
    RID *rid = REGION_get_rid(tree);
    _action.Process_rgn_level((REGION_LEVEL)(RID_level(rid) + 1));

    Is_True(_current_bb == NULL, ("invalud current node"));

    Process_entry(tree);

    _action.Finalize();
  }

public:
  //===================================================================
  // Process_entry: handle FUNC_ENTRY
  //   FUNC_ENTRY --> ignore
  //     [kid0..n-3] IDNAME --> ignore
  //     [kidn-3] PRAGMA --> ignore
  //     [kidn-2] PRAGMA --> ignore
  //     [kidn-1] BODY --> BB0
  //===================================================================
  void Process_entry(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_FUNC_ENTRY,
            ("WN is not FUNC_ENTRY"));
    Is_True(_current_bb == NULL,
            ("current BB is not NULL"));

    WN* func_body = WN_func_body(wn);
    Is_True(func_body != NULL && WN_operator(func_body) == OPR_BLOCK,
            ("invalid BLOCK of function body"));

    _current_bb = _action.Create_node(NULL, WN_first(func_body));
    _action.Process_entry_node(_current_bb);
    Process_stmt(_action.Get_dummy_entry(), wn);
    Process_block(func_body);
  }

  //===================================================================
  // Process_altentry: handle ALTENTRY
  //   STMT0 --> BB0 (must be GOTO/RETURN)
  //   ALTENTRY --> BB1 (BB1 don't have other preds than dummy_entry
  //     [kid0..n-1] IDNAME --> ignore
  //===================================================================
  void Process_altentry(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_ALTENTRY,
            ("WN is not ALTENTRY"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));

    _current_bb = _action.Create_node(NULL, wn);
    _action.Process_entry_node(_current_bb);
    Process_stmt(_current_bb, wn);
  }

  //===================================================================
  // Process_if: handle IF
  //   STMT0 --> BB0
  //   OPR_IF --> BB0
  //     [kid0] COND --> BB0
  //     [kid1] THEN --> BB1
  //     [kid2] ELSE --> BB2
  //   STMT1 --> BB3
  //===================================================================
  void Process_if(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_IF,
            ("WN is not IF"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));

    // put OPR_IF into BB0
    BB_NODE* if_bb = _current_bb;
    Process_stmt(if_bb, wn);

    // put THEN block into BB1
    WN* then_block = WN_then(wn);
    Is_True(then_block != NULL && WN_operator(then_block) == OPR_BLOCK,
            ("invalid BLOCK of IF then"));

    _current_bb = _action.Create_node(if_bb, WN_first(then_block));
    _action.Process_predsucc(if_bb, _current_bb);
    Process_block(then_block);
    BB_NODE* then_bb = _current_bb;

    // put ELSE block into BB2
    WN* else_block = WN_else(wn);
    Is_True(else_block != NULL && WN_operator(else_block) == OPR_BLOCK,
            ("invalid BLOCK of IF else"));

    _current_bb = _action.Create_node(if_bb, WN_first(else_block));
    _action.Process_predsucc(if_bb, _current_bb);
    Process_block(else_block);
    BB_NODE* else_bb = _current_bb;

    // connect THEN/ELSE block with BB3
    _current_bb = _action.Create_node(then_bb != NULL ? then_bb : else_bb, 
                                      WN_next(wn));
    if (then_bb != NULL)
      _action.Process_predsucc(then_bb, _current_bb);
    if (else_bb != NULL)
      _action.Process_predsucc(else_bb, _current_bb);
  }

  //===================================================================
  // Process_do_loop: handle DO_LOOP
  //   STMT0 --> BB0
  //   OPR_DO_LOOP --> BB0 <include INIT>
  //     [kid0] IDNAME --> ignore
  //     [kid1] INIT --> BB0
  //     [kid2] COMP --> BB1
  //     [kid3] INCR --> BB2* (put into _extra field)
  //     [kid4] BODY --> BB2
  //     [kid5] LOOP_INFO --> ignore
  //   STMT1 --> BB3
  //===================================================================
  void Process_do_loop(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_DO_LOOP, 
            ("WN is not DO_LOOP"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put INIT into BB0
    BB_NODE* init_bb = _current_bb;
    Process_stmt(init_bb, wn);
  
    // put COMP into BB1
    _current_bb = _action.Create_node(init_bb, WN_end(wn));
    _action.Process_predsucc(init_bb, _current_bb);
    Process_stmt(_current_bb, WN_end(wn));
    BB_NODE* cmp_bb = _current_bb;
  
    // put BODY/INCR into BB2
    WN* body_block = WN_do_body(wn);
    Is_True(body_block != NULL && WN_operator(body_block) == OPR_BLOCK,
            ("invalid BLOCK for DO_LOOP body"));

    _current_bb = _action.Create_node(cmp_bb, WN_first(body_block));
    _action.Process_predsucc(cmp_bb, _current_bb);
    Process_block(body_block);
    Process_stmt(_current_bb, WN_step(wn));
    _action.Process_predsucc(_current_bb, cmp_bb);
  
    // connect with BB3
    _current_bb = _action.Create_node(cmp_bb, WN_next(wn));
    _action.Process_predsucc(cmp_bb, _current_bb);
  }

  //===================================================================
  // Process_do_while: handle DO_WHILE
  //   STMT0 --> BB0
  //   DO_WHILE
  //     [kid0] COMP --> BB1* (put into _extra field)
  //     [kid1] BODY --> BB1
  //   STMT1 --> BB2
  //===================================================================
  void Process_do_while(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_DO_WHILE, 
            ("WN is not DO_WHILE"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    WN* body_block = WN_while_body(wn);
    Is_True(body_block != NULL && WN_operator(body_block) == OPR_BLOCK,
            ("invalid BLOCK for DO_WHILE body"));

    // put BODY into BB1
    if (_action.Need_new_node(_current_bb, WN_first(body_block)) /*!_current_bb->Is_empty()*/) {
      // create new BB if current BB isn't empty
      BB_NODE* last_bb = _current_bb;
      _current_bb = _action.Create_node(last_bb, WN_first(body_block));
      _action.Process_predsucc(last_bb, _current_bb);
    }
    BB_NODE* body_entry = _current_bb;
    Process_block(body_block);
    BB_NODE* body_exit = _current_bb;
  
    if (_current_bb == NULL) {
      // there is a GOTO at the end of body
      _current_bb = _action.Create_node(NULL, wn);
    }
    else if (_current_bb->Get_succs_count() > 0) {
      // can not put the COMP to BB1 in this case
      _current_bb = _action.Create_node(body_exit, wn);
      _action.Process_predsucc(body_exit, _current_bb);
    }
    Process_stmt(_current_bb, wn);
    _action.Process_predsucc(_current_bb, body_entry);
  
    // connect BB2 and BB3
    if (WN_next(wn) != NULL) {
      BB_NODE* cmp_bb = _current_bb;
      _current_bb = _action.Create_node(cmp_bb, WN_next(wn));
      _action.Process_predsucc(cmp_bb, _current_bb);
    }
  }

  //===================================================================
  // Process_while_do: handle WHILE_DO
  //   STMT0 --> BB0
  //   WHILE_DO --> BB1
  //     [kid0] COMP --> BB1
  //     [kid1] BODY --> BB2
  //   STMT1 --> BB3
  //===================================================================
  void Process_while_do(WN* wn) {
  Is_True(wn != NULL && WN_operator(wn) == OPR_WHILE_DO, 
          ("WN is not WHILE_DO"));
  Is_True(_current_bb != NULL, ("Current BB is NULL"));

  // put COMP into BB1
  if (_action.Need_new_node(_current_bb, wn) /*!_current_bb->Is_empty()*/) {
    // create new BB if current BB isn't empty
    BB_NODE* last_bb = _current_bb;
    _current_bb = _action.Create_node(last_bb, wn);
    _action.Process_predsucc(last_bb, _current_bb);
  }
  BB_NODE* cmp_bb = _current_bb;
  Process_stmt(cmp_bb, wn);

  // put BODY into BB2
  WN* body_block = WN_while_body(wn);
  Is_True(body_block != NULL && WN_operator(body_block) == OPR_BLOCK,
          ("invalid BLOCK for WHILE_DO body"));

  _current_bb = _action.Create_node(cmp_bb, WN_first(body_block));
  _action.Process_predsucc(cmp_bb, _current_bb);
  Process_block(body_block);
  if (_current_bb != NULL) {
    // there is a goto at the end of body
    _action.Process_predsucc(_current_bb, cmp_bb);
  }

  // connect BB1 and BB3
  _current_bb = _action.Create_node(cmp_bb, WN_next(wn));
  _action.Process_predsucc(cmp_bb, _current_bb);
}

  //===================================================================
  // Process_compgoto: handle COMP_GOTO
  //   STMT0 --> BB0
  //   COMPGOTO --> BB0
  //     [kid0] LDID --> BB0
  //     [kid1] JUMPs --> BB0
  //     [kid2] GOTO --> BB0
  //   STMT1 --> BB1
  //===================================================================
  void Process_compgoto(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_COMPGOTO, 
            ("Wn is not COMPGOTO"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put all kids into BB0
    Process_stmt(_current_bb, wn);
    WN* goto_wn = WN_first(WN_switch_table(wn));
    hash_set<INT32> label_set;
    while (goto_wn != NULL) {
      if (label_set.find(WN_label_number(goto_wn)) == label_set.end()) {
        label_set.insert(WN_label_number(goto_wn));
        _action.Process_goto(_current_bb, goto_wn);
      }
      goto_wn = WN_next(goto_wn);
    }
    goto_wn = WN_kid_count(wn) > 2 ? WN_switch_default(wn) : NULL;
    if (goto_wn != NULL &&
        label_set.find(WN_label_number(goto_wn)) == label_set.end())
      _action.Process_goto(_current_bb, goto_wn);
  
    // create BB1
    if (WN_next(wn) != NULL)
      _current_bb = _action.Create_node(NULL, WN_next(wn));
    else
      _current_bb = NULL;    // no fall-through for compgoto
  }

  //===================================================================
  // Process_switch: handle SWITCH
  //   STMT0 --> BB0
  //   SWITCH --> BB0
  //     [kid0] LDID --> BB0
  //     [kid1] CASEGOTOs --> BB0
  //     [kid2] GOTO --> BB0
  //   STMT1 --> BB1
  //===================================================================
  void Process_switch(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_SWITCH, 
            ("Wn is not SWITCH"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put all kids into BB0
    Process_stmt(_current_bb, wn);
    WN* casegoto_wn = WN_first(WN_switch_table(wn));
    hash_set<INT32> label_set;
    while (casegoto_wn != NULL) {
      if (label_set.find(WN_label_number(casegoto_wn)) == label_set.end()) {
        label_set.insert(WN_label_number(casegoto_wn));
        _action.Process_goto(_current_bb, casegoto_wn);
      }
      casegoto_wn = WN_next(casegoto_wn);
    }
    casegoto_wn = WN_kid_count(wn) > 2 ? WN_switch_default(wn) : NULL;
    if (casegoto_wn != NULL &&
        label_set.find(WN_label_number(casegoto_wn)) == label_set.end())
      _action.Process_goto(_current_bb, casegoto_wn);
  
    // create BB1
    if (WN_next(wn) != NULL)
      _current_bb = _action.Create_node(NULL, WN_next(wn));
    else
      _current_bb = NULL;    // no fall-through for switch
  }
  
  //===================================================================
  // Process_label: handle LABEL
  //   STMT0 --> BB0
  //   LABEL --> BB1
  //   STMT1 --> BB1
  //===================================================================
  void Process_label(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_LABEL, 
            ("WN is not LABEL"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    BB_NODE* last_bb = _current_bb;
    if (_action.Need_new_node(_current_bb, wn) /*!_current_bb->Is_empty()*/) {
      // create new bb if current BB isn't empty
      _current_bb = _action.Create_node(last_bb, wn);
    }
  
    Process_stmt(_current_bb, wn);
    _action.Process_label(_current_bb, wn);
  
    // connect last_bb with current_bb
    if (WN_Label_Is_Handler_Begin(wn) ||
        LABEL_target_of_goto_outer_block(WN_label_number(wn))) {
      _action.Process_predsucc(_action.Get_dummy_entry(), _current_bb);
    }
    if (last_bb != _current_bb)
      _action.Process_predsucc(last_bb, _current_bb);
  }
  
  //===================================================================
  // Process_goto: handle GOTO
  //   STMT0 --> BB0
  //   GOTO  --> BB0
  //   STMT1 --> BB1
  //===================================================================
  void Process_goto(WN* wn) {
    Is_True(WN_operator(wn) == OPR_GOTO, ("WN is not goto"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put GOTO into BB0
    Process_stmt(_current_bb, wn);
    _action.Process_goto(_current_bb, wn);
  
    // create BB1
    if (WN_next(wn) != NULL)
      _current_bb = _action.Create_node(NULL, WN_next(wn));
    else
      _current_bb = NULL;    // no fall-through for goto
  }
  
  //===================================================================
  // Process_goto: handle XGOTO
  //===================================================================
  void Process_xgoto(WN* wn) {
    FmtAssert(FALSE, ("TODO: handle XGOTO"));
  }

  //===================================================================
  // Process_goto: handle AGOTO
  //===================================================================
  void Process_agoto(WN* wn) {
    FmtAssert(FALSE, ("TODO: handle AGOTO"));
  }

  //===================================================================
  // Process_condgoto: handle conditional goto, TRUEBR, FALSEBR
  //   STMT0 --> BB0
  //   CONDGOTO  --> BB0
  //   STMT1 --> BB1
  //===================================================================
  void Process_condgoto(WN* wn) {
    Is_True(wn != NULL &&
            (WN_operator(wn) == OPR_TRUEBR || WN_operator(wn) == OPR_FALSEBR),
            ("WN is not TRUEBR or FALSEBR"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put CONDGOTO into BB0
    BB_NODE* condgoto_bb = _current_bb;
    Process_stmt(condgoto_bb, wn);

    WN* next = WN_next(wn);
    if (next == NULL || WN_operator(next) != OPR_LABEL ||
        WN_label_number(wn) != WN_label_number(next)) {
      _action.Process_goto(condgoto_bb, wn);
    }
  
    // create and connect BB1
    if (WN_next(wn) != NULL) {
      _current_bb = _action.Create_node(condgoto_bb, WN_next(wn));
      _action.Process_predsucc(condgoto_bb, _current_bb);
    }
  }
  
  //===================================================================
  // Process_block: handle the BLOCK
  //   traverse the block and take actions for different WHIRL node
  //   STMT1 --> handle stmt1 based on stmt1's operator
  //   STMT2 --> handle stmt2 based on stmt2's operator
  //   STMT3 --> handle stmt3 based on stmt3's operator
  //===================================================================
  void Process_block(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_BLOCK, 
            ("wn is not BLOCK"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    for (WN* stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt)) {
      OPERATOR opr = WN_operator(stmt);
      switch (opr) {
      case OPR_IF:
        Process_if(stmt); break;
      case OPR_DO_LOOP:
        Process_do_loop(stmt); break;
      case OPR_DO_WHILE:
        Process_do_while(stmt); break;
      case OPR_WHILE_DO:
        Process_while_do(stmt); break;
      case OPR_COMPGOTO:
        Process_compgoto(stmt); break;
      case OPR_SWITCH:
        Process_switch(stmt); break;
      case OPR_ALTENTRY:
        Process_altentry(stmt); break;
      case OPR_LABEL:
        Process_label(stmt); break;
      case OPR_GOTO:
        Process_goto(stmt); break;
      case OPR_XGOTO:
        Process_xgoto(stmt); break;
      case OPR_AGOTO:
        Process_agoto(stmt); break;
      case OPR_TRUEBR:
      case OPR_FALSEBR:
        Process_condgoto(stmt); break;
      case OPR_PRAGMA:
      case OPR_XPRAGMA:
        Process_stmt(_current_bb, stmt); break;
      case OPR_INTRINSIC_CALL:
        Process_intrinsic_call(stmt); break;
      case OPR_REGION:
        Process_region(stmt); break;
      case OPR_REGION_EXIT:
        Process_region_exit(stmt); break;
      case OPR_BLOCK:
        Process_block(stmt); break;
      default:
        if (OPERATOR_is_stmt(opr)) {
          Process_stmt(_current_bb, stmt);
        }
        else {
          FmtAssert(FALSE, ("TODO: handle me: %s", OPCODE_name(WN_opcode(stmt))));
        }
      }
    }
  }
  
  //===================================================================
  // Process_region: handle REGION
  //   STMT0 --> BB0
  //   REGION --> BB1
  //     [kid0] REGION_EXITS --> ignore when it's not black box
  //                             otherwise, connect exits with labels
  //     [kid1] PRAGMAs --> ignore
  //     [kid2] BODY --> BB1
  //   STMT1 --> BB2
  //===================================================================
  void Process_region(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_REGION, 
            ("wn is not REGION"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    RID *rid = REGION_get_rid(wn);
    if (rid != NULL && RID_level(rid) >= _action.Get_rgn_level()) {
      FmtAssert(FALSE, ("TODO: handle black-box REGION"));
    }
 
    WN* region_body = WN_region_body(wn);
    Is_True(region_body != NULL && WN_operator(region_body) == OPR_BLOCK,
            ("invalid BLOCK for REGION body")); 

    if (_action.Need_new_node(_current_bb, WN_first(region_body)) /*!_current_bb->Is_empty()*/) {
      // Force new block if current BB isn't empty
      BB_NODE* last_bb = _current_bb;
      _current_bb = _action.Create_node(last_bb, WN_first(region_body));
      _action.Process_predsucc(last_bb, _current_bb);
    }
    Process_block(region_body);
    BB_NODE* rgn_body = _current_bb;
  
    // create BB2
    if (WN_next(wn) != NULL) {
      _current_bb = _action.Create_node(rgn_body, WN_next(wn));
      if (rgn_body != NULL) {
        _action.Process_predsucc(rgn_body, _current_bb);
      }
    }
  }
  
  //===================================================================
  // Process_region_exit: handle REGION_EXIT
  // STMT0 --> BB0
  // REGION_EXIT  --> BB0
  // STMT1 --> BB1
  //===================================================================
  void Process_region_exit(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_REGION_EXIT, 
            ("WN is not region_exit"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // put GOTO into BB0
    Process_stmt(_current_bb, wn);
    _action.Process_goto(_current_bb, wn);
  
    // create BB1
    if (WN_next(wn) != NULL)
      _current_bb = _action.Create_node(NULL, WN_next(wn));
    else
      _current_bb = NULL;    // no fall-through for region_exit
  }
  
  //===================================================================
  // Process_intrinsic_call: handle INTRINSIC_CALL
  // INTRINSIC_CALL --> BB0
  //   if call never returns, end current bb and connect to exit
  //===================================================================
  void Process_intrinsic_call(WN* wn) {
    Is_True(wn != NULL && WN_operator(wn) == OPR_INTRINSIC_CALL,
            ("WN is not call"));
    Is_True(_current_bb != NULL, ("Current BB is NULL"));
  
    // add call to current bb
    Process_stmt(_current_bb, wn);
    if (WN_Call_Never_Return(wn)) {
      _action.Process_predsucc(_current_bb, _action.Get_dummy_exit());
      // create BB1
      if (WN_next(wn) != NULL)
        _current_bb = _action.Create_node(NULL, WN_next(wn));
      else
        _current_bb = NULL;    // no fall-through for the intrinsic
    }
  }
  
  //===================================================================
  // Process_stmt
  //   Add stmt to bb
  //===================================================================
  void Process_stmt(BB_NODE* bb, WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN, 
            ("wn is NULL"));
  
    OPERATOR opr = WN_operator(stmt);
    if (opr == OPR_FUNC_ENTRY) {
      Is_True(bb == _action.Get_dummy_entry(), 
              ("FUNC_ENTRY can only be in dummy entry"));
    }
    if (opr == OPR_ALTENTRY) {
      Is_True(bb->Get_preds_count() == 1 &&
              *(bb->Pred_begin()) == _action.Get_dummy_entry(), 
              ("ALTENTRY can only have the dummy entry as it pred."));
    }
  
    _action.Process_stmt(bb, stmt);
  
    if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {
      _action.Process_exit_node(bb);
      // RETURN will end current bb
      if (WN_next(stmt) != NULL)
        _current_bb = _action.Create_node(NULL, WN_next(stmt));
      else
        _current_bb = NULL;    // no fall-through for return
    }
  }

};

} /* namespace CFG_UTIL */

#endif /* wn_cfg_template_INCLUDED */

