// ====================================================================
//
// Copyright (C) 2011, Hewlett-Packard Development Company, L.P.
// All Rights Reserved.
//
// Open64 is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// Open64 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA  02110-1301, USA.
//
// ====================================================================
//
// Module: cgssa_build.h
// 
// Description:
//   Definitions for classes that are used in building and leaving SSA
// 
// Exported Classes:
//   CGSSA_PHI_INSERT 
//   CGSSA_RENAME 
//   CGSSA_LEAVE
//   
// ====================================================================

#ifndef cgssa_build_INCLUDED
#define cgssa_build_INCLUDED

#include "cgssa_core.h"

using namespace CFG_UTIL;

extern bool Op_has_side_effect(OP *op);

namespace CGSSA_NAME {

//====================================================================
// Definitions for classes that are used in building SSA/leaving SSA
// including Phi insertion and renaming 
//====================================================================

// What is interested in SSA build
class PREG_INTEREST {
public:
  
  // both constant and register TNs are in SSA
  // while, only register TNs can be def TN, thus they need PHI, CHI 
  BOOL interest(TN* tn, CGSSA * ssa) { 

    if (ssa->Build_Dedicated_Reg()) {
      if (TN_is_register(tn))
        return TRUE; 
    }
    else
      if (TN_is_register(tn) && !TN_is_dedicated(tn))
        return TRUE;

    return FALSE;
  }

  BOOL side_effect_op(OP* op, CGSSA * ssa) {

    if (ssa->Build_Dedicated_Reg() && 
       (OP_side_effects(op) || OP_call(op))) 
      return TRUE;

    return FALSE;  
  }

}; 

// Phi insertion class
template<typename _Tinterest>
class CGSSA_PHI_INSERT : private _Tinterest{

private:
  typedef CFG_UTIL::CG_CFG::BB_NODE BB_NODE;
  CGSSA*            _ssa;
  CFG_UTIL::CG_CFG* _cfg;

public:
  CGSSA_PHI_INSERT(CGSSA* ssa, CFG_UTIL::CG_CFG* cfg)
    : _ssa(ssa), _cfg(cfg) {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    Is_True(_cfg != NULL, ("cfg is NULL"));
  }

  CGSSA* Ssa() { return _ssa; }
  
  CFG_UTIL::CG_CFG* Cfg() { return _cfg; }

  // callback by DOM_WALKER before visit its kids on DOM tree
  void Visit_push(BB_NODE* bb) {
    for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
         sit != bb->Stmt_end();
         ++sit) {
      push_stmt(bb, &(*sit));
    }
  }

  // callback by DOM_WALKER after visited its kids on DOM tree
  void Visit_pop(BB_NODE* bb) { };

protected:
  void push_stmt(BB_NODE* bb, OP* stmt);
  void Insert_Phis_for_a_Def(BB_NODE* bb, TN* def);
};

// Rename Class
template<typename _Tinterest>
class CGSSA_RENAME: private _Tinterest {
private:
  typedef CFG_UTIL::CG_CFG::BB_NODE BB_NODE;
  typedef std::vector< std::stack<VER_ID> > VERSION_STACK;
  VERSION_STACK _ver_stack;
  CGSSA* _ssa;
  CFG_UTIL::CG_CFG* _cfg;

public:
  CGSSA* Ssa() {
    return _ssa;
  }

  CFG_UTIL::CG_CFG* Cfg() {
    return _cfg;
  }

  CGSSA_RENAME(CGSSA* ssa, CFG_UTIL::CG_CFG* cfg)
    : _ssa(ssa), _cfg(cfg) {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    Is_True(_cfg != NULL, ("cfg is NULL"));
  }

  void Initialize() {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    _ver_stack.resize(_ssa->Register_TN_count());
  }

  void Finalize() {
  }

  // callback by DOM_WALKER before visit its kids on DOM tree
  void Visit_push(BB_NODE* bb) {
    push_phi(bb);
    for (BB_NODE::stmt_iterator sit = bb->Stmt_begin();
        sit != bb->Stmt_end();
        ++sit) {
          push_stmt(&(*sit));
     }
    //rename phi operands in succ bbs
    for (BB_NODE::bb_iterator bit = bb->Succ_begin();
        bit != bb->Succ_end();
        ++bit) {
        rename_succ_phi(bb, *bit);
    }
  }

  // callback by DOM_WALKER after visited its kids on DOM tree
  void Visit_pop(BB_NODE* bb) {
    for (BB_NODE::stmt_iterator sit = bb->Stmt_rbegin();
      sit != bb->Stmt_rend();
      ++sit) {
        pop_stmt(&(*sit));
    }
    pop_phi(bb);
  }

protected:
  void push_ver(TN_NUM tn_idx, VER_ID ver_idx);
  VER_ID top_ver(TN_NUM tn_idx);
  void pop_ver(TN_NUM tn_idx);

  void push_phi(BB_NODE* bb);
  void pop_phi(BB_NODE* bb);

  void phi_push_ver(PHI_NODE* phi);
  void phi_use_ver(PHI_NODE* phi, INT pos);
  void phi_pop_ver(PHI_NODE* phi);

  void push_stmt(OP* stmt);
  void pop_stmt(OP* stmt);
  void rename_succ_phi(BB_NODE* pred, BB_NODE* succ);
  void push_dedicated_reg(OP* stmt);
  void pop_dedicated_reg(OP *stmt);

}; /* CGSSA_RENAME */

//===================================================================
// Leave SSA
//   resolves live range overlaps by introducing new TNs
//   manage the rename stack based on the existing version IDs
//===================================================================
template<typename _Tinterest>
class CGSSA_LEAVE: private _Tinterest {
private:
  typedef CFG_UTIL::CG_CFG::BB_NODE BB_NODE;
  typedef std::vector< std::stack<VER_ID> > VERSION_STACK;
  VERSION_STACK _ver_stack;
  CGSSA* _ssa;
  CFG_UTIL::CG_CFG* _cfg;

protected:
  CGSSA* Ssa() {
    return _ssa;
  }

  CFG_UTIL::CG_CFG* Cfg() {
    return _cfg;
  }

public:
  CGSSA_LEAVE(CGSSA* ssa, CFG_UTIL::CG_CFG* cfg)
    : _ssa(ssa), _cfg(cfg) {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    Is_True(_cfg != NULL, ("cfg is NULL"));
  }

  void Initialize() {
    Is_True(_ssa != NULL, ("ssa is NULL"));
    _ver_stack.resize(_ssa->Register_TN_count());
  }

  void Finalize() {
  }

  // callback by DOM_WALKER before visit its kids on DOM tree
  void Visit_push(BB_NODE* bb) {
    push_phi(bb);
    BB_NODE::stmt_iterator sit = bb->Stmt_begin();
    for (; sit != bb->Stmt_end(); ++sit)
          push_stmt(&(*sit));
  }

  // callback by DOM_WALKER after visited its kids on DOM tree
  void Visit_pop(BB_NODE* bb) {
    for (BB_NODE::stmt_iterator sit = bb->Stmt_rbegin();
      sit != bb->Stmt_rend();
      ++sit) {
        pop_stmt(&(*sit));
    }
    pop_phi(bb);
  }

protected:
  void push_ver(TN_NUM tn_idx, VER_ID ver_idx);
  VER_ID top_ver(TN_NUM tn_idx);
  void pop_ver(TN_NUM tn_idx);

  void push_phi(BB_NODE* bb);
  void pop_phi(BB_NODE* bb);
  
  void phi_push_ver(PHI_NODE* phi);
  void phi_pop_ver(PHI_NODE* phi);

  void insert_copy (PHI_NODE* phi, int idx, TN* old_tn, TN* new_tn);
  void rename_uses(VERSION* ver, TN* old_tn, TN* new_tn);
  // rename this PHI def and all its uses
  void rename_PHI(VERSION* ver);
  // rename this OP def and all its uses
  void rename_OP(VERSION* ver);
  // rename this def and all its uses
  void rename_if_overlaps(VER_ID ver_id);

  void push_stmt(OP* stmt);
  void pop_stmt(OP* stmt);

}; /* class CGSSA_LEAVE */

}  /* namespace CGSSA_NAME */
#endif /* cgssa_build_INCLUDED */

