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
// Module: cgssa_build.cxx
//
// Description:
//   Implementation for classes CGSSA_PHI_INSERT, CGSSA_RENAME, 
//   and CGSSA_LEAVE
//   
// ====================================================================

#include "cgssa_build.h"

using namespace CGSSA_NAME;

//====================================================================
// Implementation for CGSSA build/leave SSA
//====================================================================

// The entry function to build CGSSA
void
CGSSA::Build(bool dedicated_reg) {

  Clear_Tabs_();
  Build_Dedicated_Reg(dedicated_reg);

  // step1: insert phi 
  typedef CGSSA_PHI_INSERT<PREG_INTEREST> CGSSA_PHI_INSERT_PREG;
  CGSSA_PHI_INSERT_PREG inserter(this, _cfg);
  CFG_UTIL::DOM_WALKER_HELPER<CFG_UTIL::CG_CFG, CGSSA_PHI_INSERT_PREG, TRUE>
    insert_walker(*_cfg, inserter);
  insert_walker.Traverse();

  // step2: rename phase
  typedef CGSSA_RENAME<PREG_INTEREST> CGSSA_RENAME_PREG;
  CGSSA_RENAME_PREG renamer(this, _cfg);
  renamer.Initialize();
  CFG_UTIL::DOM_WALKER_HELPER<CFG_UTIL::CG_CFG, CGSSA_RENAME_PREG, TRUE>
    rename_walker(*_cfg, renamer);
  rename_walker.Traverse();
  renamer.Finalize();

}

// The entry function to leave CGSSA - remove overlapping live range
void
CGSSA::Leave() {
  typedef CGSSA_LEAVE<PREG_INTEREST> CGSSA_LEAVE_PREG;
  CGSSA_LEAVE_PREG leave(this, _cfg);
  leave.Initialize();
  CFG_UTIL::DOM_WALKER_HELPER<CFG_UTIL::CG_CFG, CGSSA_LEAVE_PREG, TRUE>
    leave_walker(*_cfg, leave);
  leave_walker.Traverse();
  leave.Finalize();
}


//====================================================================
// Implementation for CGSSA_PHI_INSERT
//====================================================================

template<typename _Tinterest>
void CGSSA_PHI_INSERT<_Tinterest>::push_stmt(BB_NODE* bb, OP* stmt) {

  // create the reference ID for each opnds
  for (int i = 0; i < OP_opnds(stmt); i++) {
    Ssa()->New_Opnd_Ref_ID(stmt, i);
  }

  // create the reference ID for each def
  // and insert phis to DF.
  // if def is a dedicated register, use base dedicated register
  // for all aliased registers. For example, (on x86) TN1 is rax, TN101, TN102 
  // are also rax with different size. We use TN1 as the base dedicated 
  // register for TN101, TN102.
  for (int i = 0; i < OP_results(stmt); i++) {
    Ssa()->New_Result_Ref_ID(stmt, i);
    TN* def = OP_result(stmt, i);
    if (_Tinterest::interest(def, Ssa())) {
      def = TNvec(Ssa()->Reg_TN_number(def));
      Insert_Phis_for_a_Def(bb, def);
    }  
  }

  // create the chi for all OPs with side effect (e.g., OP_call)
  if (_Tinterest::side_effect_op(stmt, Ssa())) {
    for (int i = REGISTER_MIN; i <= Last_Distinct_Dedicated_TN ; i++) {
      Insert_Phis_for_a_Def(bb, TNvec(i));
      CHI_NODE * chi = Ssa()->Create_Chi(i);
      Ssa()->Insert_Chi_into_OP(stmt, chi);
    }
  }
}

template<typename _Tinterest>
void CGSSA_PHI_INSERT<_Tinterest>::Insert_Phis_for_a_Def(BB_NODE* bb, TN* def) {
  for (BB_NODE::bb_iterator df_it = bb->Df_begin();
      df_it != bb->Df_end(); ++df_it) {
      BB_NODE* df_bb = *df_it;
      Is_True(df_bb != NULL && (bb == df_bb || !bb->Dominate(df_bb)),
          ("dom bb is wrong"));
      _ssa->Insert_a_Phi_into_a_DF(df_bb, def);
  }
}

//====================================================================
// Implementation for CGSSA_RENAME<_Tinterest>
//====================================================================
template<typename _Tinterest>
void CGSSA_RENAME<_Tinterest>::push_ver(TN_NUM tn_idx, VER_ID ver_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  _ver_stack[tn_idx].push(ver_idx);
}

template<typename _Tinterest>
VER_ID 
CGSSA_RENAME<_Tinterest>::top_ver(TN_NUM tn_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  if (_ver_stack[tn_idx].empty())
    return INVALID_VER;
  else
    return _ver_stack[tn_idx].top();
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::pop_ver(TN_NUM tn_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  Is_True(!_ver_stack[tn_idx].empty(), ("empty stack"));

  _ver_stack[tn_idx].pop();
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::push_phi(BB_NODE* bb) {
  Is_True(bb != NULL, ("invalid bb"));
  CGSSA::PHI_ITERATOR phi_iter = _ssa->Phi_Begin(bb);
  for (; phi_iter != _ssa->Phi_End(); ++phi_iter)
    phi_push_ver(&(*phi_iter));
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::pop_phi(BB_NODE* bb) {
  Is_True(bb != NULL, ("invalid bb"));
  CGSSA::PHI_ITERATOR phi_iter = _ssa->Phi_Begin(bb);
  for (; phi_iter != _ssa->Phi_End(); ++phi_iter)
    phi_pop_ver(&(*phi_iter));
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::phi_push_ver(PHI_NODE* phi) {
  Is_True(phi != NULL, ("phi is NULL"));
  TN_NUM phi_tn_id = phi->Get_TN_num();
  TN * phi_tn = TNvec(phi_tn_id);
  Is_True(phi_tn!= NULL, ("empty tn"));
  Is_True(_Tinterest::interest(phi_tn, Ssa()), ("special phi def"));
  
  VER_ID new_ver = Ssa()->New_Ver(phi_tn, phi);
  push_ver(phi_tn_id, new_ver);
  phi->Set_res(new_ver);
  Ssa()->Set_Ref_Ver(Ssa()->Result_Ref_ID(phi), new_ver);
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::phi_use_ver(PHI_NODE* phi, INT pos) {
  Is_True(phi != NULL, ("phi is NULL"));
  TN_NUM phi_tn_id = phi->Get_TN_num();
  TN * phi_tn = TNvec(phi_tn_id);
  Is_True(phi_tn!= NULL, ("empty tn"));
  Is_True(_Tinterest::interest(phi_tn, Ssa()), ("special phi def"));

  VER_ID new_ver = top_ver(phi_tn_id);
  if (new_ver == INVALID_VER) {
    new_ver = Ssa()->New_Ver(phi_tn);
    push_ver(phi_tn_id, new_ver);
  }  
  phi->Set_opnd(pos, new_ver); 
  REF_ID ref_id = Ssa()->Opnd_Ref_ID(phi, pos);
  Ssa()->Set_Ref_Ver(ref_id, new_ver);
  VERSION* ver_entry = Ssa()->Get_Version(new_ver);
  ver_entry->Add_Use(ref_id);
}

template<typename _Tinterest>
void 
CGSSA_RENAME<_Tinterest>::phi_pop_ver(PHI_NODE* phi) {
  Is_True(phi != NULL, ("phi is NULL"));
  TN_NUM phi_tn_id = phi->Get_TN_num();
  TN * phi_tn = TNvec(phi_tn_id);
  Is_True(phi_tn!= NULL, ("empty tn"));
  Is_True(_Tinterest::interest(phi_tn, Ssa()), ("special phi def"));
  Is_True(phi->Res() == top_ver(phi_tn_id), ("version mismatch"));

  pop_ver(phi_tn_id);
}

template<typename _Tinterest>
void
CGSSA_RENAME<_Tinterest>::push_stmt(OP* stmt) {

  // rename for opnds
  for (int i = 0; i < OP_opnds(stmt); i++) {
    TN* use = OP_opnd(stmt, i);
    VER_ID cur_ver = INVALID_VER;
    if (_Tinterest::interest(use, Ssa())) {
      // register tn case
      cur_ver = top_ver(Ssa()->Reg_TN_number(use));
    }
    // new a ver for the initial register tn and constant tn
    if (cur_ver == INVALID_VER) {
      cur_ver = Ssa()->New_Ver(use, true);
      if (_Tinterest::interest(use, Ssa()))
        push_ver(Ssa()->Reg_TN_number(use), cur_ver);  
    }  
    
    REF_ID ref_id = Ssa()->Opnd_Ref_ID(stmt, i);
    Ssa()->Set_Ref_Ver(ref_id, cur_ver);
    VERSION* ver_entry = Ssa()->Get_Version(cur_ver);
    ver_entry->Add_Use(ref_id);
  }

  bool add_mu = false;

  // for conditional def, need to add a mu 
  if (OP_cond_def(stmt)) {
    for (int i = 0; i < OP_results(stmt); i++) {
      TN* def = OP_result(stmt,i);
      VER_ID cur_ver = top_ver(Ssa()->Reg_TN_number(def));

      if (cur_ver == INVALID_VER) {
        cur_ver = Ssa()->New_Ver(def, true);
        push_ver(Ssa()->Reg_TN_number(def), cur_ver);
      }

      MU_NODE* mu = Ssa()->Create_Mu(Ssa()->Reg_TN_number(def));
      Ssa()->Insert_Mu_into_OP(stmt, mu);
      REF_ID mu_ref_id = Ssa()->Opnd_Ref_ID(mu);
      Ssa()->Set_Ref_Ver(mu_ref_id, cur_ver);
      Ssa()->Get_Version(cur_ver)->Add_Use(mu_ref_id);
      add_mu = true;
    }
  }  

  // create a new version for the defs
  for (int i = 0; i < OP_results(stmt); i++) {
    TN* def = OP_result(stmt, i);
    // add a may use for dedicated TN
    if (_Tinterest::interest(def, Ssa()) && 
      TN_is_dedicated(def) && !add_mu) {
      VER_ID top_ver_id = top_ver(Ssa()->Reg_TN_number(def));
      if (top_ver_id != INVALID_VER &&
         Ssa()->Get_Version_TN(top_ver_id) != def &&
         TN_size(Ssa()->Get_Version_TN(top_ver_id)) > TN_size(def)) {
         MU_NODE* mu = Ssa()->Create_Mu(Ssa()->Reg_TN_number(def));
         Ssa()->Insert_Mu_into_OP(stmt, mu);
         REF_ID mu_ref_id = Ssa()->Opnd_Ref_ID(mu);
         Ssa()->Set_Ref_Ver(mu_ref_id, top_ver_id);
         Ssa()->Get_Version(top_ver_id)->Add_Use(mu_ref_id);
      }
    }

    VER_ID new_ver = Ssa()->New_Ver(def, stmt, i);
    push_ver(Ssa()->Reg_TN_number(def), new_ver);
    REF_ID ref_id = Ssa()->Result_Ref_ID(stmt, i);
    Ssa()->Set_Ref_Ver(ref_id, new_ver);
  }

  // if the stmt has side effect, we need to rename the chis
  push_dedicated_reg(stmt);
}

template<typename _Tinterest>
void
CGSSA_RENAME<_Tinterest>::pop_stmt(OP* stmt) {

  pop_dedicated_reg(stmt);

  for (int i = 0; i < OP_results(stmt); i++) {
    TN* def = OP_result(stmt, i);
    if (_Tinterest::interest(def, Ssa())) {
      Is_True(top_ver(Ssa()->Reg_TN_number(def)) == Ssa()->Result_Ver(stmt, i), 
      ("version mismatch"));
      pop_ver(Ssa()->Reg_TN_number(def));
    }  
  }
}

template<typename _Tinterest>
void
CGSSA_RENAME<_Tinterest>::rename_succ_phi(BB_NODE* pred, BB_NODE* succ) {
  Is_True(pred != NULL && succ != NULL, ("pred or succ is NULL"));
  CGSSA::PHI_ITERATOR phi_iter = Ssa()->Phi_Begin(succ);
  for (; phi_iter != Ssa()->Phi_End(); ++phi_iter) {
    INT pred_pos = succ->Pred_pos(pred);
    phi_use_ver(&(*phi_iter), pred_pos);
  }
} 

template<typename _Tinterest>
void
CGSSA_RENAME<_Tinterest>::push_dedicated_reg(OP* stmt) {
  if (!_Tinterest::side_effect_op(stmt, Ssa())) return;

  // first rename for the chi opnd 
  // then push the new version of chi def
  for (int i = REGISTER_MIN; i < Last_Distinct_Dedicated_TN +1; ++i ) {
    TN* def = TNvec(i);
    VER_ID opnd_ver = top_ver(Ssa()->Reg_TN_number(def));
    if (opnd_ver == INVALID_VER) {
      opnd_ver = Ssa()->New_Ver(def, true);
      push_ver(Ssa()->Reg_TN_number(def), opnd_ver);
    }
    CHI_NODE * chi = Ssa()->Get_Chi_from_OP(stmt, i);
    // rename for the chi opnd
    REF_ID opnd_ref_id = Ssa()->Opnd_Ref_ID(chi);
    Ssa()->Set_Ref_Ver(opnd_ref_id, opnd_ver);
    chi->Set_opnd(opnd_ver);
    VERSION* ver_entry = Ssa()->Get_Version(opnd_ver);
    ver_entry->Add_Use(opnd_ref_id);

    VER_ID def_ver = Ssa()->New_Ver(def, chi);
    chi->Set_res(def_ver);
    push_ver(Ssa()->Reg_TN_number(def), def_ver);
    REF_ID res_ref_id = Ssa()->Result_Ref_ID(chi);
    Ssa()->Set_Ref_Ver(res_ref_id, def_ver);
  }
}

template<typename _Tinterest>
void
CGSSA_RENAME<_Tinterest>::pop_dedicated_reg(OP *stmt) {
  if (!_Tinterest::side_effect_op(stmt, Ssa())) return;

  for (int i = REGISTER_MIN; i < Last_Distinct_Dedicated_TN+1; ++i ) {
    TN* def = TNvec(i);
    pop_ver(Ssa()->Reg_TN_number(def));
  }
}


//====================================================================
// Implementation for CGSSA_LEAVE
// Currently CGSSA_LEAVE have similar functions, which should be 
// unified later.
//====================================================================
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::push_ver(TN_NUM tn_idx, VER_ID ver_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  _ver_stack[tn_idx].push(ver_idx);
}

template<typename _Tinterest>
VER_ID 
CGSSA_LEAVE<_Tinterest>::top_ver(TN_NUM tn_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  if (_ver_stack[tn_idx].empty())
    return INVALID_VER;
  else
    return _ver_stack[tn_idx].top();
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::pop_ver(TN_NUM tn_idx) {
  Is_True(tn_idx < _ssa->Register_TN_count() && tn_idx < _ver_stack.size(), 
    ("tn out of bounds"));
  Is_True(!_ver_stack[tn_idx].empty(), ("empty stack"));

  _ver_stack[tn_idx].pop();
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::push_phi(BB_NODE* bb) {
  Is_True(bb != NULL, ("invalid bb"));
  CGSSA::PHI_ITERATOR phi_iter = _ssa->Phi_Begin(bb);
  for (; phi_iter != _ssa->Phi_End(); ++phi_iter)
    phi_push_ver(&(*phi_iter));
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::pop_phi(BB_NODE* bb) {
  Is_True(bb != NULL, ("invalid bb"));
  CGSSA::PHI_ITERATOR phi_iter = _ssa->Phi_Begin(bb);
  for (; phi_iter != _ssa->Phi_End(); ++phi_iter)
    phi_pop_ver(&(*phi_iter));
}

// rename this PHI and all its uses
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::phi_push_ver(PHI_NODE* phi) {
  Is_True(phi != NULL, ("phi is NULL"));
  TN_NUM phi_tn_id = phi->Get_TN_num();
  TN * phi_tn = TNvec(phi_tn_id);
  Is_True(phi_tn!= NULL, ("empty tn"));
  Is_True(_Tinterest::interest(phi_tn, Ssa()), ("special phi def"));
  
  for (int i = 0; i < phi->Opnd_count(); ++i ) {
    VER_ID ver_id = phi->Get_opnd(i);
    rename_if_overlaps(ver_id);
  }

  // push the PHI version onto the stack
  VER_ID ver_id = Ssa()->Result_Ver(phi);
  push_ver(phi_tn_id, ver_id);
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::phi_pop_ver(PHI_NODE* phi) {
  Is_True(phi != NULL, ("phi is NULL"));
  TN_NUM phi_tn_id = phi->Get_TN_num();
  TN * phi_tn = TNvec(phi_tn_id);
  Is_True(phi_tn!= NULL, ("empty tn"));
  Is_True(_Tinterest::interest(phi_tn, Ssa()), ("special phi def"));
  Is_True(phi->Res() == top_ver(phi_tn_id), ("version mismatch"));

  pop_ver(phi_tn_id);
}

// insert a copy from old_tn to new_tn to the idx'th predecessor of 
// the basic block to which phi belongs
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::insert_copy (PHI_NODE* phi, int idx, TN* old_tn, TN* new_tn) {
  CG_CFG::BB_NODE* this_BB = _cfg->Get_BB(phi->BB());
  const CG_CFG::BB_NODE* pred_BB = this_BB->Get_pred(idx);

  OPS copy_OPs; OPS_Init(&copy_OPs);
  Exp_COPY (old_tn, new_tn, &copy_OPs /* , TRUE: Need this? */);
  BB* pred_bb = _cfg->Get_BB_from_BB_NODE(pred_BB);
  OP *point = BB_last_op(pred_bb);
  if (OP_br(point))
    BB_Insert_Ops_Before(pred_bb, point, &copy_OPs);
  else
    BB_Append_Ops(pred_bb, &copy_OPs);
}

// change the VERSION's uses from old_tn to new_tn
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::rename_uses(VERSION* ver, TN* old_tn, TN* new_tn) {
  VERSION::use_iterator use_iter = ver->Use_Begin();
  for (; use_iter != ver->Use_End(); ++use_iter) {
    REF_ID use = *use_iter;
    INTPTR stmt; INT idx; bool is_OP;
    Ssa()->Get_Stmt(use, &stmt, &idx, &is_OP);

    if (is_OP) {
      Set_OP_opnd((OP*)stmt, idx, new_tn);
    }
    else {
      // for a PHI using this operand, a copy should be generated 
      // from the new TN to the old TN in the corresponding predecessor
      // of the BB where the PHI belongs.
      insert_copy((PHI_NODE*)stmt, idx, old_tn, new_tn);
    }
  }
}

// rename this PHI def and all its uses
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::rename_PHI(VERSION* ver) {
  TN* old_tn = ver->Get_TN();
  TN* new_tn = Dup_TN(old_tn); // Do we wanna copy TN_GLOBAL_REG flag ?

  // change SSA related data
  ver->Set_TN(new_tn);

  // generate a COPY (from old_tn to new_tn) for each of the PHI's sources
  // at the end of the corresponding redecessor basic block
  VER_ID ver_id = ver->Get_Ver();
  PHI_NODE* phi = Ssa()->Get_Phi_Def(ver_id);
  phi->Set_TN_num(TN_number(new_tn));
  CG_CFG::BB_NODE* this_BB = _cfg->Get_BB(phi->BB());
  for (int idx=0; idx<phi->Opnd_count(); idx++) {
    insert_copy(phi, idx, old_tn, new_tn);
  }

  rename_uses(ver, old_tn, new_tn);
}

// rename this OP def and all its uses
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::rename_OP(VERSION* ver) {
  TN* old_tn = ver->Get_TN();
  TN* new_tn = Dup_TN(old_tn); // Do we wanna copy TN_GLOBAL_REG flag ?

  // change SSA related data
  ver->Set_TN(new_tn);

  // change the OP's result
  VER_ID ver_id = ver->Get_Ver();
  OP* op; INT idx;
  Ssa()->Get_Occ_Def(ver_id, &op, &idx);
  Set_OP_result(op, idx, new_tn);

  rename_uses(ver, old_tn, new_tn);
}

// rename this def and all its uses
template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::rename_if_overlaps(VER_ID ver_id) {
  // Only PHI and OP can overlap their live ranges.
  VERSION_KIND ver_kind = Ssa()->Def_kind(ver_id);
  if (ver_kind != VERSION_PHI_DEF && ver_kind != VERSION_OCC_DEF)
    return;

  // No overlap if the given ver_id matches that on the top of the stack
  VERSION* ver = Ssa()->Get_Version(ver_id);
  TN* tn = ver->Get_TN();
  VER_ID cur_ver = top_ver(TN_number(tn));
  if (ver_id == cur_ver)
    return;

  if (ver_kind == VERSION_OCC_DEF)
    rename_OP(ver);
  else
    rename_PHI(ver);
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::push_stmt(OP* stmt){
  for (int i = 0; i < OP_opnds(stmt); i++) {
    VER_ID ver_id = Ssa()->Opnd_Ver(stmt, i);
    rename_if_overlaps(ver_id);
  }

  // push the defs
  for (int i = 0; i < OP_results(stmt); i++) {
    VER_ID ver_id = Ssa()->Result_Ver(stmt, i);
    TN* def = OP_result(stmt, i);
    if (_Tinterest::interest(def, Ssa()))
      push_ver(TN_number(def), ver_id);
  }
}

template<typename _Tinterest>
void 
CGSSA_LEAVE<_Tinterest>::pop_stmt(OP* stmt){
  for (int i = 0; i < OP_results(stmt); i++) {
    TN* def = OP_result(stmt, i);
    Is_True(_Tinterest::interest(def, Ssa()), ("special def"));
    Is_True(top_ver(TN_number(def)) == Ssa()->Result_Ver(stmt, i), 
      ("version mismatch"));
    pop_ver(TN_number(def));
  }
}

