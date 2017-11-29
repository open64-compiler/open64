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
// Module: wssa_rename.h
//
// Revision history:
//  Dec-10 - Original Version
//
// Description:
//  template for SSA renaming
//
// Exported classes:
//  WSSA::WSSA_RENAME_BASE
//
// SEE ALSO:
//  be/com/wssa_mgr.h  (WHIRL_SSA_MANAGER)
//
//====================================================================

#include "wn_cfg.h"
#include "wssa_mgr.h"
#include "wssa_utils.h"
#include <vector>
#include <stack>
#include <ext/hash_set>
using namespace __gnu_cxx;

namespace WSSA {

//===================================================================
// RENAME_ACTION_BASE
//   base class of all rename actions
//   manage the rename stack
//===================================================================
template<typename _Tinterest>
class RENAME_ACTION_BASE : public _Tinterest {
private:
  typedef CFG_UTIL::WN_CFG::BB_NODE BB_NODE;
  typedef std::vector< std::stack<VER_IDX> > VERSION_STACK;
  VERSION_STACK _ver_stack;
  WHIRL_SSA_MANAGER* _ssa;
  CFG_UTIL::WN_CFG* _cfg;

protected:
  RENAME_ACTION_BASE(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : _ssa(ssa), _cfg(cfg) {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    Is_True(_cfg != NULL, ("cfg is NULL"));
  }

  WHIRL_SSA_MANAGER* Ssa() {
    return _ssa;
  } 

  CFG_UTIL::WN_CFG* Cfg() {
    return _cfg;
  }

protected:
  BOOL interest(WST_IDX wst_idx) {
    return _Tinterest::interest(_ssa, wst_idx);
  }

  BOOL interest(WN* stmt) {
    return _Tinterest::interest(_ssa, stmt);
  }

  WST_IDX create_wst_for_wn(WN* wn) {
    return _Tinterest::create_wst_for_wn(_ssa, wn);
  }

protected:
  VER_IDX top_ver(WST_IDX wst_idx) {
    Is_True(wst_idx < _ssa->WST_count(), ("wst_idx out of bounds"));
    Is_True(wst_idx < _ver_stack.size(), ("wst_idx out of bounds"));
    Is_True(interest(wst_idx),
            ("wst_idx is not interested"));
    if (_ver_stack[wst_idx].empty())
      return VER_INVALID;
    else
      return _ver_stack[wst_idx].top();
  }

  VER_IDX push_ver(WST_IDX wst_idx, VER_IDX ver_idx) {
    Is_True(wst_idx < _ssa->WST_count(), ("wst_idx out of bounds"));
    Is_True(wst_idx < _ver_stack.size(), ("wst_idx out of bounds"));
    Is_True(ver_idx < _ssa->Ver_count(), ("ver_idx out of bounds"));
    Is_True(interest(wst_idx),
            ("wst_idx is not interested"));
    _ver_stack[wst_idx].push(ver_idx);
    return ver_idx;
  }

  void pop_ver(WST_IDX wst_idx) {
    Is_True(wst_idx < _ssa->WST_count(), ("wst_idx out of bounds"));
    Is_True(wst_idx < _ver_stack.size(), ("wst_idx out of bounds"));
    Is_True(!_ver_stack[wst_idx].empty(), ("stack is empty"));
    Is_True(interest(wst_idx),
            ("wst_idx is not interested"));
    _ver_stack[wst_idx].pop();
  }

protected:
  void wn_push_ver(WN* stmt) {
    Is_True(stmt != NULL && WSSA::WN_def_ver(stmt),
            ("invalid stmt"));
    if (interest(stmt)) {
      WST_IDX wst_idx = create_wst_for_wn(stmt);
      init_wst(wst_idx);
      VER_IDX new_ver = Ssa()->New_ver(wst_idx, stmt, WSSA_OCC);
      push_ver(wst_idx, new_ver);
      Ssa()->Set_wn_ver(stmt, new_ver);
    }
  }

  void wn_pop_ver(WN* stmt) {
    Is_True(stmt != NULL && WSSA::WN_def_ver(stmt),
            ("invalid stmt"));
    if (interest(stmt)) {
      WST_IDX wst_idx = Ssa()->Get_wn_ver_wst(stmt);
      Is_True(wst_idx == create_wst_for_wn(stmt),
              ("wst_idx mismatch"));
      Is_True(top_ver(wst_idx) == Ssa()->Get_wn_ver(stmt),
              ("version mismatch"));
      pop_ver(wst_idx);
    }
  }

  void wn_use_ver(WN* stmt) {
    Is_True(stmt != NULL && WSSA::WN_use_ver(stmt),
            ("invalid stmt"));
    if (interest(stmt)) {
      WST_IDX wst_idx = create_wst_for_wn(stmt);
      init_wst(wst_idx);
      VER_IDX new_ver = top_ver(wst_idx);
      if (new_ver == VER_INVALID)
        new_ver = init_ver(wst_idx);
      Ssa()->Set_wn_ver(stmt, new_ver);
    }
  }

protected:
  void phi_push_ver(WN* stmt, PHI_NODE* phi) {
    Is_True(phi != NULL, ("phi is NULL"));
    WST_IDX wst_idx = Ssa()->Get_ver_wst(phi->Opnd(0));
#ifdef Is_True_On
    Is_True(wst_idx == Ssa()->Get_ver_wst(phi->Res()),
            ("wst_idx mismatch"));
    for (INT i = 1; i < phi->Opnd_count(); ++i) {
      Is_True(wst_idx == Ssa()->Get_ver_wst(phi->Opnd(i)),
              ("wst_idx mismatch"));
    }
#endif
    if (interest(wst_idx)) {
      VER_IDX new_ver = Ssa()->New_ver(wst_idx, stmt, WSSA_PHI);
      push_ver(wst_idx, new_ver);
      phi->Set_res(new_ver);
    }
  }

  void phi_pop_ver(PHI_NODE* phi) {
    Is_True(phi != NULL, ("phi is NULL"));
    WST_IDX wst_idx = Ssa()->Get_ver_wst(phi->Opnd(0));
    if (interest(wst_idx)) {
      Is_True(phi->Res() == top_ver(wst_idx), ("version mismatch"));
      pop_ver(wst_idx);
    }
  }

  void phi_use_ver(PHI_NODE* phi, INT pred_pos) {
    Is_True(phi != NULL, ("phi is NULL"));
    WST_IDX wst_idx = Ssa()->Get_ver_wst(phi->Opnd(0));
    if (interest(wst_idx)) {
      VER_IDX new_ver = top_ver(wst_idx);
      if (new_ver == VER_INVALID)
        new_ver = init_ver(wst_idx);
      phi->Set_opnd(pred_pos, new_ver);
    }
  }

protected:
  void chi_push_ver(WN* stmt, CHI_NODE* chi) {
    WST_IDX wst_idx = Ssa()->Get_ver_wst(chi->Opnd());
#ifdef Is_True_On
    Is_True(chi != NULL, ("chi is NULL"));
    Is_True(wst_idx == Ssa()->Get_ver_wst(chi->Res()),
            ("wst_idx mismatch"));
    const WST_Symbol_Entry& sym = Ssa()->Get_wst(wst_idx);
    Is_True(sym.Sym_type() != WST_PREG ||
            OPERATOR_is_call(WN_operator(stmt)) ||
            WN_operator(stmt) == OPR_ASM_STMT ||
            (WN_operator(stmt) == OPR_LABEL && WN_Label_Is_Handler_Begin(stmt)), 
            ("chi uses preg"));
#endif
    if (interest(wst_idx)) {
      VER_IDX opnd_ver = top_ver(wst_idx);
      if (opnd_ver == VER_INVALID)
        opnd_ver = init_ver(wst_idx);
      chi->Set_opnd(opnd_ver);
      VER_IDX res_ver = Ssa()->New_ver(wst_idx, stmt, WSSA_CHI);
      push_ver(wst_idx, res_ver);
      chi->Set_res(res_ver);
    }
  }

  void chi_pop_ver(CHI_NODE* chi) {
    WST_IDX wst_idx = Ssa()->Get_ver_wst(chi->Opnd());
    Is_True(chi != NULL, ("chi is NULL"));
    if (interest(wst_idx)) {
      Is_True(chi->Res() == top_ver(wst_idx), ("version mismatch"));
      pop_ver(wst_idx);
    }
  }

  void mu_use_ver(WN* stmt, MU_NODE* mu) {
    WST_IDX wst_idx = Ssa()->Get_ver_wst(mu->Opnd());
#ifdef Is_True_On
    Is_True(mu != NULL, ("mu is NULL"));
    const WST_Symbol_Entry& sym = Ssa()->Get_wst(wst_idx);
    Is_True(sym.Sym_type() != WST_PREG ||
            OPERATOR_is_call(WN_operator(stmt)) ||
            WN_operator(stmt) == OPR_ASM_STMT,
            ("mu uses preg"));
#endif
    if (interest(wst_idx)) {
      VER_IDX opnd_ver = top_ver(wst_idx);
      if (opnd_ver == VER_INVALID)
        opnd_ver = init_ver(wst_idx);
      mu->Set_opnd(opnd_ver);
    }
  }

protected:
  void init_stack() {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    _ver_stack.resize(_ssa->WST_count());
  }

  void init_wst(WST_IDX wst_idx) {
    Is_True(wst_idx < _ssa->WST_count(), ("wst_idx out of bounds"));
    Is_True(wst_idx <= _ver_stack.size(), ("wst_idx out of bounds"));
    Is_True(interest(wst_idx),
            ("wst_idx is not interested"));
    if (wst_idx == _ver_stack.size()) {
      _ver_stack.push_back(std::stack<VER_IDX>());
      init_ver(wst_idx);
    }
  }

  VER_IDX init_ver(WST_IDX wst_idx) {
    Is_True(wst_idx < _ssa->WST_count(), ("wst_idx out of bounds"));
    Is_True(wst_idx < _ver_stack.size(), ("wst_idx out of bounds"));
    Is_True(_ver_stack[wst_idx].empty(), ("stack is not empty"));
    Is_True(interest(wst_idx),
            ("wst_idx is not interested"));
    VER_IDX opnd_ver = _ssa->New_ver(wst_idx, NULL, WSSA_UNKNOWN);
    VER_IDX res_ver = _ssa->New_ver(wst_idx, _ssa->Root(), WSSA_CHI);
    push_ver(wst_idx, res_ver);
    CHI_NODE* entry_chi = _ssa->Create_chi();
    entry_chi->Set_res(res_ver);
    entry_chi->Set_opnd(opnd_ver);
    _ssa->Add_chi(_ssa->Root(), entry_chi);
    return res_ver;
  }

  void initialize() {
    init_stack();
    for (INT i = 0; i < Ssa()->WST_count(); ++i) {
      if (interest((WST_IDX)i)) {
        Ssa()->Set_max_ver((WST_IDX)i, WSSA_ZERO_VER);
        init_ver((WST_IDX)i);
      }
    }
  }

  void finalize() {
#ifdef Is_True_On
    for (INT i = 0; i < Ssa()->WST_count(); ++i) {
      if (interest((WST_IDX)i)) {
        Is_True(_ver_stack[i].size() == 0 || _ver_stack[i].size() == 1,
                ("stack size wrong"));
        VER_IDX ver_idx = top_ver((WST_IDX)i);
        if (ver_idx != VER_INVALID) {
          Is_True(Ssa()->Get_ver_num(ver_idx) == 1, ("ver_num wrong"));
          pop_ver((WST_IDX)i);
        }
        Is_True(top_ver((WST_IDX)i) == VER_INVALID,
                ("stack is not empty"));
      }
      else {
        Is_True(_ver_stack[(WST_IDX)i].empty(), ("stack is not empty"));
      }
    }
#endif
  }
};

//===================================================================
// PREG_INTEREST
//   only interested in preg
// NEW_PREG_INTEREST
//   only interested in newly created preg
// PREG_RENAME_ACTION
//   action class to rename preg only
//===================================================================
class PREG_INTEREST {
protected:
  BOOL interest(WHIRL_SSA_MANAGER* ssa, WST_IDX wst_idx) {
    Is_True(wst_idx < ssa->WST_count(), ("wst_idx out of bounds"));
    const WST_Symbol_Entry& sym = ssa->Get_wst(wst_idx);
    if (sym.Sym_type() == WST_PREG) // only rename PREG
      return TRUE;
    else
      return FALSE;
  }

  BOOL interest(WHIRL_SSA_MANAGER* ssa, WN* stmt) {
    OPERATOR opr = WN_operator(stmt);
    Is_True(stmt != NULL && opr != OPERATOR_UNKNOWN,
            ("invalid wn"));
    if (opr == OPR_STID || opr == OPR_STBITS ||
        opr == OPR_LDID || opr == OPR_LDBITS) {
      if (ST_class(WN_st(stmt)) == CLASS_PREG) {
        return TRUE;
      }
    }
    return FALSE;
  }

  WST_IDX create_wst_for_wn(WHIRL_SSA_MANAGER* ssa, WN* wn) {
    Is_True(wn != NULL && WSSA::WN_has_ver(wn),
            ("invalid wn"));
    Is_True(interest(ssa, wn), ("wn is not interested"));
    return ssa->Create_wst_for_wn(wn);
  }
};

class NEW_PREG_INTEREST : public PREG_INTEREST {
private:
  hash_set<UINT32 /* WST_IDX */ > _wst_set;

public:
  BOOL interest(WHIRL_SSA_MANAGER* ssa, WST_IDX wst_idx) {
    if (PREG_INTEREST::interest(ssa, wst_idx) &&
        _wst_set.find(wst_idx) != _wst_set.end())
      return TRUE;
    else
      return FALSE;
  }

  BOOL interest(WHIRL_SSA_MANAGER* ssa, WN* stmt) {
    if (PREG_INTEREST::interest(ssa, stmt)) {
      if (ssa->WN_has_ver(stmt) == FALSE)
        return TRUE;
      WST_IDX wst_idx = ssa->Get_wn_ver_wst(stmt);
      if (_wst_set.find(wst_idx) != _wst_set.end())
        return TRUE;
    }
    return FALSE;
  }

  WST_IDX create_wst_for_wn(WHIRL_SSA_MANAGER* ssa, WN* wn) {
    WST_IDX wst_idx = PREG_INTEREST::create_wst_for_wn(ssa, wn);
    _wst_set.insert(wst_idx);
    return wst_idx;
  }
};

template<typename _Tinterest>
class PREG_RENAME_ACTION_BASE : public RENAME_ACTION_BASE<_Tinterest> {
protected:
  PREG_RENAME_ACTION_BASE(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : RENAME_ACTION_BASE<_Tinterest>(ssa, cfg) {
  }

protected:
  void build_mu_list(WN* expr) {
    return;    // do nothing
  }

  void build_chi_mu_list(WN* stmt) {
    return;    // do nothing
  }

protected:
  void chi_push_ver(WN* stmt, CHI_NODE* chi) {
    return;    // do nothing
  }

  void chi_pop_ver(CHI_NODE* chi) {
    return;    // do nothing
  }

  void mu_use_ver(WN* stmt, MU_NODE* mu) {
    return;    // do nothing
  }

};

class PREG_RENAME_ACTION : public PREG_RENAME_ACTION_BASE<PREG_INTEREST> {
protected:
  PREG_RENAME_ACTION(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : PREG_RENAME_ACTION_BASE<PREG_INTEREST>(ssa, cfg) {
  }
};

class NEW_PREG_RENAME_ACTION : public PREG_RENAME_ACTION_BASE<NEW_PREG_INTEREST> {
protected:
  NEW_PREG_RENAME_ACTION(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : PREG_RENAME_ACTION_BASE<NEW_PREG_INTEREST>(ssa, cfg) {
  }
};

//===================================================================
// FULL_INTEREST
//   interested in all symbols
// FULL_RENAME_ACTION
//   action class to rename all symbols
//===================================================================
class FULL_INTEREST {
protected:
  BOOL interest(WHIRL_SSA_MANAGER* ssa, WST_IDX wst_idx) {
    return TRUE;
  }

  BOOL interest(WHIRL_SSA_MANAGER* ssa, WN* stmt) {
    return TRUE;
  }

  WST_IDX create_wst_for_wn(WHIRL_SSA_MANAGER* ssa, WN* wn) {
    Is_True(wn != NULL && WSSA::WN_has_ver(wn),
            ("invalid wn"));
    if (ST_class(WN_st(wn)) == CLASS_PREG)
      return ssa->Create_wst_for_wn(wn);
    else
      return ssa->Get_wn_ver_wst(wn);
  }
};

class FULL_RENAME_ACTION : public RENAME_ACTION_BASE<FULL_INTEREST> {
protected:
  FULL_RENAME_ACTION(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : RENAME_ACTION_BASE<FULL_INTEREST>(ssa, cfg) {
  }
};

//===================================================================
// WSSA_RENAME_BASE
//   provide functors to DOM_WALKER to rename the WHIRL SSA
//   the renaming actions are performed by _Tactions
//   template parameter:
//     _Tactions
//   public interface:
//     void Visit_push(BB_NODE* bb)
//     void Visit_pop(BB_NODE* bb)
//   _Tactions interface:
//     BOOL 
//===================================================================
template<typename _Taction>
class WSSA_RENAME_BASE : private _Taction {
public:
  typedef CFG_UTIL::WN_CFG::BB_NODE BB_NODE;
  typedef _Taction ACTION;

public:
  WSSA_RENAME_BASE(WHIRL_SSA_MANAGER* ssa, CFG_UTIL::WN_CFG* cfg)
    : _Taction(ssa, cfg) {
  }

private:
  WHIRL_SSA_MANAGER* Ssa() {
    return _Taction::Ssa();
  }

  CFG_UTIL::WN_CFG* Cfg() {
    return _Taction::Cfg();
  }

  void rename_expr(WN* expr) {
    Is_True(expr != NULL && OPERATOR_is_expression(WN_operator(expr)),
            ("invalid expr %s", OPCODE_name(WN_opcode(expr))));
    Is_True(!WSSA::WN_has_phi(expr) && !WSSA::WN_has_chi(expr),
            ("invalid expr %s", OPCODE_name(WN_opcode(expr))));
    for (int i = 0; i < WN_kid_count(expr); i++) {
      rename_expr(WN_kid(expr, i));
    }
    if (_Taction::interest(expr)) {
      if (WSSA::WN_use_ver(expr))
        _Taction::wn_use_ver(expr);
      if (WSSA::WN_has_mu(expr))
        rename_mu(expr);
    }
  }

  void push_stmt(WN* stmt) {
    Is_True(stmt != NULL && 
            (OPERATOR_is_stmt(WN_operator(stmt)) ||
             WN_operator(stmt) == OPR_FUNC_ENTRY),
            ("invalid stmt"));
    OPERATOR opr = WN_operator(stmt);
    switch (opr) {
    case OPR_FUNC_ENTRY:
      return;
    case OPR_COMPGOTO:
      rename_expr(WN_kid(stmt, 0));
      return;
    case OPR_ASM_STMT:
      for (int i = 2; i < WN_kid_count(stmt); i++) {
        rename_expr(WN_kid(stmt, i));
      }
      break;
    default:
      for (int i = 0; i < WN_kid_count(stmt); i++) {
        rename_expr(WN_kid(stmt, i));
      }
    }
    if (_Taction::interest(stmt)) {
      if (WSSA::WN_has_mu(stmt))
        rename_mu(stmt);
      if (WSSA::WN_def_ver(stmt))
        _Taction::wn_push_ver(stmt);
      if (WSSA::WN_has_chi(stmt))
        push_chi(stmt);
    }
  }

  void pop_stmt(WN* stmt) {
    Is_True(stmt != NULL && 
            (OPERATOR_is_stmt(WN_operator(stmt)) ||
             WN_operator(stmt) == OPR_FUNC_ENTRY),
            ("invalid stmt"));
    OPERATOR opr = WN_operator(stmt);
    switch (opr) {
    case OPR_FUNC_ENTRY:
    case OPR_COMPGOTO:
      return;
    case OPR_ASM_STMT:
      pop_chi(stmt);
      return;
    default:
      break;
    }
    if (_Taction::interest(stmt)) {
      if (WSSA::WN_def_ver(stmt))
        _Taction::wn_pop_ver(stmt);
      if (WSSA::WN_has_chi(stmt))
        pop_chi(stmt);
    }
  }

  void push_phi(BB_NODE* bb, WN* stmt) {
    Is_True(bb != NULL, ("invalid bb"));
    if (stmt != NULL && Ssa()->WN_has_phi(stmt)) {
      for (WHIRL_SSA_MANAGER::phi_iterator pit = Ssa()->WN_phi_begin(stmt);
           pit != Ssa()->WN_phi_end(stmt);
           ++pit) {
        _Taction::phi_push_ver(stmt, &(*pit));
      }
    }
  }

  void pop_phi(BB_NODE* bb, WN* stmt) {
    Is_True(bb != NULL, ("invalid bb"));
    if (stmt != NULL && Ssa()->WN_has_phi(stmt)) {
      for (WHIRL_SSA_MANAGER::phi_iterator pit = Ssa()->WN_phi_begin(stmt);
           pit != Ssa()->WN_phi_end(stmt);
           ++pit) {
        _Taction::phi_pop_ver(&(*pit));
      }
    }
  }

  void rename_succ_phi(BB_NODE* pred, BB_NODE* succ) {
    Is_True(pred != NULL && succ != NULL, ("pred or succ is NULL"));
    WN* stmt = succ->First_stmt();
    if (stmt != NULL && Ssa()->WN_has_phi(stmt)) {
      INT pred_pos = succ->Pred_pos(pred);
      Is_True(succ->Get_pred(pred_pos) == pred, ("pred succ mismatch"));
      for (WHIRL_SSA_MANAGER::phi_iterator pit = Ssa()->WN_phi_begin(stmt);
           pit != Ssa()->WN_phi_end(stmt);
           ++pit) {
        _Taction::phi_use_ver(&(*pit), pred_pos);
      }
    }
  }

  void push_chi(WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    if (Ssa()->WN_has_chi(stmt)) {
      for (WHIRL_SSA_MANAGER::chi_iterator cit = Ssa()->WN_chi_begin(stmt);
           cit != Ssa()->WN_chi_end(stmt);
           ++cit) {
        _Taction::chi_push_ver(stmt, &(*cit));
      }
    }
  }

  void pop_chi(WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    if (Ssa()->WN_has_chi(stmt)) {
      for (WHIRL_SSA_MANAGER::chi_iterator cit = Ssa()->WN_chi_begin(stmt);
           cit != Ssa()->WN_chi_end(stmt);
           ++cit) {
        _Taction::chi_pop_ver(&(*cit));
      }
    }
  }

  void rename_mu(WN* stmt) {
    Is_True(stmt != NULL && WN_operator(stmt) != OPERATOR_UNKNOWN,
            ("invalid stmt"));
    if (Ssa()->WN_has_mu(stmt)) {
      for (WHIRL_SSA_MANAGER::mu_iterator mit = Ssa()->WN_mu_begin(stmt);
           mit != Ssa()->WN_mu_end(stmt);
           ++mit) {
        _Taction::mu_use_ver(stmt, &(*mit));
      }
    }
  }

public:
  // initialize the renamer
  void Initialize() {
    Is_True(Ssa() != NULL, ("invalid SSA MANAGER"));
    Is_True(Cfg() != NULL, ("invalid WHIRL CFG"));
    _Taction::initialize();
  }

  void Finalize() {
    _Taction::finalize();
  }

  // callback by DOM_WALKER before visit its kids on DOM tree
  void Visit_push(BB_NODE* bb) {
    // scan the stmt list forward and
    //   rename the statement and push new version to stack
    push_phi(bb, bb->First_stmt());
    for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
         sit != bb->Stmt_end();
         ++sit) {
      push_stmt(&(*sit));
    }
    // rename phi operands in succ bbs
    for (BB_NODE::bb_iterator bit = bb->Succ_begin();
         bit != bb->Succ_end();
         ++bit) {
      rename_succ_phi(bb, *bit);
    }
  }

  // callback by DOM_WALKER after visited its kids on DOM tree
  void Visit_pop(BB_NODE* bb) {
    // scan stmt list backward and:
    //   pop up versions from stack
    for (BB_NODE::stmt_iterator sit = bb->Stmt_rbegin();
         sit != bb->Stmt_rend();
         ++sit) {
      pop_stmt(&(*sit));
    }
    pop_phi(bb, bb->First_stmt());
  }

  // rename within single BB
  void Rename_single_BB(BB_NODE* bb) {
    for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
      sit != bb->Stmt_end();
      ++sit) {
      push_stmt(&(*sit));
    }
#ifdef Is_True_On
    for (BB_NODE::stmt_iterator sit = bb->Stmt_rbegin();
         sit != bb->Stmt_rend();
         ++sit) {
      pop_stmt(&(*sit));
    }
#endif
  }
};

} /* namespace WSSA */

