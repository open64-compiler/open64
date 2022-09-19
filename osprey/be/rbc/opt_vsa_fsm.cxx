/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

#include "defs.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_cfg.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_vsa.h"
#include "opt_vsa_util.h"
#include "cxx_memory.h"
#include "report.h"
#include "erbe.h"
#include <ext/hash_set>
#include "opt_vsa_rbc.h"
#include "config_vsa.h"
#include "class_hierarchy.h"

using __gnu_cxx::hash_set;



// =============================================================================
//
// FSM::Find_state returns the state ID by the state name
//
// =============================================================================
IDTYPE
FSM::Find_state(STRING state)
{
  for (INT i = 0; i < State_vec()->size(); i++) {
    if (strcmp((*State_vec())[i], state) == 0)
      return i;
  }
  return FSM_STATE_INVALID;
}


// =============================================================================
//
// FSM::Print prints out the FSM in ascii form
//
// =============================================================================
void
FSM::Print(FILE *fp) 
{
  INT i;
  fprintf(fp, "State List::\n");
  for (i = 0; i < State_vec()->size(); i++) {
    fprintf(fp, "(%d : %s) ", i, (*State_vec())[i]);
  }

  fprintf(fp, "\nTransition List::\n");
  for (i = 0; i < Ts_vec()->size(); i++) {
    TRANSIT *tr = (*Ts_vec())[i];
    fprintf(fp, "Transition #%d, (State:%d, Action:%s, Nstate:%d)\n",
            i, tr->State(), tr->Is_default() ? "(default)" : tr->Action(), tr->Nstate());
  }
}


// =============================================================================
//
// FSM::Add_transition build FSM with sequence of trnasitions from one state to
//          another.  It first create an object of TRANSIT class with all these
//          input parameters and push_back into the Ts_vec().
//
// =============================================================================
void
FSM::Add_transition(IDTYPE state, STRING action, CODEREP *key, CODEREP *cond,
                    IDTYPE nstate, STRING_VEC *errcode, INT32 msg_id)
{
  TRANSIT *ts = CXX_NEW(TRANSIT(state, action, key, cond, nstate, errcode, msg_id, FALSE), Mem_pool());
  Ts_vec()->push_back(ts);
}


// =============================================================================
//
// FSM::Set_default_action fill up a Transition from a state to final state,
//      it is used as an error report transition because all intermediate
//      states now have a transition to final state.
//
// =============================================================================
void
FSM::Set_default_action(STRING state, STRING_VEC *errcode, INT32 msg_id)
{
  IDTYPE   i = Get_state(state);
  TRANSIT *ts = CXX_NEW(TRANSIT(i, NULL, NULL, NULL, Final_state(), errcode, msg_id, TRUE), Mem_pool());
  Ts_vec()->push_back(ts);
}


// =============================================================================
//
// FSM::Get_default_action return default transition for a state
//
// =============================================================================
TRANSIT*
FSM::Get_default_action(IDTYPE state, INT *idx)
{
  TRANSIT *ret = NULL;
  for (INT i = 0; i < Ts_vec()->size(); i++) {
    TRANSIT *ts = (*Ts_vec())[i];
    // not default action, skip
    if (ts->Action() != NULL && !ts->Is_default())
      continue;
    // state does not apply, skip
    if (ts->State() != state)
      continue;
    // found
    ret = ts;
    *idx = i;
    break;
  }
  return ret;
}


IDTYPE
FSM::Get_state(STRING state)
{
  IDTYPE i = Find_state(state);
  if (i != FSM_STATE_INVALID) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "%sFSM::Get_state for %s, get State:%d\n",SBar,state,i));
    return i;
  }

  // enter the State_vec since it's not found in the existing state list
  State_vec()->push_back(state);
  i = State_vec()->size() - 1;
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sFSM::Get_state for %s, get State:%d\n",SBar,state,i));
  return i;
}

// =============================================================================
//
// FSM::Verify validate whether The FSM is invalid if any of the state
//         transition is not defined
//
// =============================================================================
BOOL
FSM::Verify(void) const
{
  return TRUE;
}


// =============================================================================
//
// FB_LIST::Find returns the fsm_base that has the same name
//
// =============================================================================
FSM_BASE*
FB_LIST::Find(STRING name) 
{
  FSM_BASE    *fsm_base;
  FB_LIST_ITER fb_list_iter;
  FOR_ALL_ELEM (fsm_base, fb_list_iter, Init(this)) {
    if (strcmp(fsm_base->Fsm_name(), name) == 0) {
      return fsm_base;
    }
  }
  return NULL;
}

// =============================================================================
//
// FB_LIST::Count returns the number of elements in this FB_LIST
//
// =============================================================================
INT
FB_LIST::Count(void)
{
  FSM_BASE    *fsm_base;
  FB_LIST_ITER fb_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (fsm_base, fb_list_iter, Init(this)) {
    ++count;
  }
  return count;
}


// =============================================================================
//
// Print function for tracing Fsm Base Object lifecycle
//
// =============================================================================
void
FB_LIST::Print(FILE *fp) 
{
  FSM_BASE    *fsm_base;
  FB_LIST_ITER fb_list_iter;
  FOR_ALL_ELEM (fsm_base, fb_list_iter, Init(this)) {
    fsm_base->Print(fp);
  }
}

// =============================================================================
//
// FO_LIST::Find returns the fsm_obj that has the same name
//
// =============================================================================
FSM_OBJ*
FO_LIST::Find(STRING name) 
{
  FSM_OBJ     *fsm_obj;
  FO_LIST_ITER fo_list_iter;
  FOR_ALL_ELEM (fsm_obj, fo_list_iter, Init(this)) {
    if (strcmp(fsm_obj->Fsm_name(), name) == 0) {
      return fsm_obj;
    }
  }
  return NULL;
}


// =============================================================================
//
// FO_LIST::Count returns the number of elements in this FO_LIST
//
// =============================================================================
INT
FO_LIST::Count(void)
{
  FSM_OBJ     *fsm_obj;
  FO_LIST_ITER fo_list_iter;
  INT count = 0;
  FOR_ALL_ELEM (fsm_obj, fo_list_iter, Init(this)) {
    ++count;
  }
  return count;
}


// =============================================================================
//
// Print function for tracing Fsm Object lifecycle
//
// =============================================================================
void
FO_LIST::Print(FILE *fp) 
{
  FSM_OBJ     *fsm_obj;
  FO_LIST_ITER fo_list_iter;
  FOR_ALL_ELEM (fsm_obj, fo_list_iter, Init(this)) {
    fsm_obj->Print(fp);
  }
}

// =============================================================================
//
// VSA::Allocate_fsm_obj - the management function for FSM_OBJ_REP cannot be
//      defined in the header file since Rbc() is not visible in that scope
//
// =============================================================================
FSM_OBJ_REP*
VSA::Allocate_fsm_obj( BB_NODE *bb, STRING fsm_name, MEM_POOL *def_bbs_pool)
{
  FSM_OBJ  *fsm_obj = Find(fsm_name);
  if (fsm_obj == NULL) {
    FSM_BASE *fsm_base;
    fsm_base = Ipsa()->Rbc()->Find_fsm_base(fsm_name);
    fsm_obj = CXX_NEW(FSM_OBJ(fsm_base), Mem_pool());
    Fsm_obj_list()->Append(fsm_obj);
    Is_Trace(Tracing(), (TFile, "VSA::Allocate_fsm_obj for %s:\n", fsm_name));

    fsm_obj->Set_stack(CXX_NEW( STACK<FOR_PAIR>(Mem_pool()), Mem_pool() ));
    // create entry_chi does not have a relevant semantics for fsm_base
    FSM_OBJ_REP *entry_chi = Allocate_fsm_obj(fsm_obj); // entry_chi version
    if (Is_root_entry())
      entry_chi->Set_initial_state();  // Default is FSM_STATE_INVALID

    entry_chi->Set_srcpos_node((STMTREP*)NULL, NULL, PATHINFO_NONE);
    entry_chi->Set_attr(ROR_DEF_BY_CHI);
    fsm_obj->Push(entry_chi, 0);    // push entry chi to stack
    fsm_obj->Set_entry_chi(entry_chi);
  }
  if (bb) fsm_obj->Prepend_def_bbs(bb, def_bbs_pool);
  return Allocate_fsm_obj( fsm_obj );
}


// =============================================================================
//
// FSM_OBJ_REP::Gen_name, similar to the Gen_name function for HEAP_OBJ_REP
//            AND VSYM_OBJ_REP, except it does not create new version number.
//            Instead, it wraps Process based on the action in the stmt.
//
// =============================================================================
IDTYPE
FSM_OBJ_REP::Gen_name(STMTREP* stmt)
{
  Is_True(this != NULL, ("FSM_OBJ_REP:Gen_name - NULL this"));
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sVSA::Gen_name: at stmtrep_id:%d\n", SBar, (stmt)?stmt->Stmtrep_id() : 0));

  IDTYPE i = Fsm_obj()->Gen_version();
  this->Set_version(i);
  this->Set_state_unknown();
  this->Fsm_obj()->Push(this, stmt);
  return i;
}


// =============================================================================
//
//  VSA::Identify_fsm_insts_stmt - do Identify_fsm_insts at statement level
//  Instance = insts
//
// =============================================================================
void
VSA::Identify_fsm_insts_stmt(STMTREP *stmt, BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  OPERATOR opr = stmt->Opr();
  if (opr == OPR_FALSEBR || opr == OPR_TRUEBR) {
    // bind FSMs that has 'if' statement as its transition
    Is_Trace(Tracing(),
             (TFile,
              "VSA::Identify_fsm_insts_stmt: trying to match transition in \"if\" statement.\n"));
    FB_LIST *fsm_list = Rbc()->Fsm_base_list();
    FB_LIST_ITER fb_list_iter;
    FSM_BASE *fsm_base;
    FOR_ALL_ELEM(fsm_base, fb_list_iter, Init(fsm_list)) {
      FSM *fsm = fsm_base->Fsm();
      if (!fsm->Is_set(FSM_ATTR_IF_TRANSIT))
        continue;
      Add_new_for(stmt, bb, def_bbs_pool, fsm_base->Fsm_name(), "if");
    }
    return;
  }
  else if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {
    // check if current function is bound to FSM that has 'return' as its transition
    // then bind them to the 'return' statement
    Is_Trace(Tracing(),
             (TFile,
              "VSA::Identify_fsm_insts_stmt: trying to match transition in \"return\" statement.\n"));
    FB_LIST *fsm_list = Rbc()->Fsm_base_list();
    FB_LIST_ITER fb_list_iter;
    FSM_BASE *fsm_base;
    FOR_ALL_ELEM(fsm_base, fb_list_iter, Init(fsm_list)) {
      FSM *fsm = fsm_base->Fsm();
      // add 'return' fsm_obj_rep only if there's already other fsm_obj_rep identified
      // of the same fsm, avoid adding 'return' fsm_obj_rep to all functions.
      if (!fsm->Is_set(FSM_ATTR_RET_TRANSIT) || Find(fsm_base->Fsm_name()) == NULL)
        continue;
      Add_new_for(stmt, bb, def_bbs_pool, fsm_base->Fsm_name(), "return");
    }
    return;
  }
  else if (opr == OPR_STID) {
    CODEREP *rhs = stmt->Rhs();
    CODEREP *lhs = stmt->Lhs();
    FOR_ARRAY *for_array = Cr_2_for_array(rhs);
    if (for_array != NULL) {
      Is_Trace(Tracing(),
               (TFile,
                "VSA::Identify_fsm_insts_stmt: Add left hand side to map for assignment.\n"));
      for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++)
        Enter_cr_for_array_map(lhs, (*for_array)[fa_idx]);
    }
  }
  // else if (opr == OPR_ISTORE) {
  //   CODEREP *rhs = stmt->Rhs();
  //   CODEREP *lhs = stmt->Lhs();
  //   FOR_ARRAY *for_array = Cr_2_for_array(rhs);
  //   if (for_array != NULL) {
  //     Is_Trace(Tracing(),
  //              (TFile,
  //               "VSA::Identify_fsm_insts_stmt: Add left hand side to map for assignment.\n"));
  //     for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++)
  //       Enter_cr_for_array_map(lhs, (*for_array)[fa_idx]);
  //   }
  //   VSYM_OBJ_REP *vor = Cr_2_vor(rhs);
  //   if (vor != NULL) {
  //     for_array = Vor_2_for_array(vor);
  //     if (for_array != NULL) {
  //       Is_Trace(Tracing(),
  //                (TFile,
  //                 "VSA::Identify_fsm_insts_stmt: Add left hand side to map for assignment.\n"));
  //       for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++)
  //         Enter_cr_for_array_map(lhs, (*for_array)[fa_idx]);
  //     }
  //   }
  // }

  // deal with calls only here after
  if (!OPERATOR_is_call(opr))
    return;

  RNA_NODE *rna = Dna()->Get_callsite_rna(stmt);
  if (rna == NULL)
    return;

  if (rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
    for (INT idx = INVALID_VAR_IDX; idx < rna->Arg_list()->size(); idx++) {
      CODEREP *tgt = (idx == INVALID_VAR_IDX) ?
        Comp_unit()->Find_return_value(stmt) :
        rna->Get_arg(idx);
      SRC_VECTOR *srcs = Ipsa()->Get_assign_src(rna, tgt);
      if (srcs == NULL)
        continue;
      for (INT i = 0; i < srcs->size(); i++) {
        CODEREP *src = rna->Get_arg((*srcs)[i]);
        if (src == NULL || src == tgt)
          continue;
        FOR_ARRAY *for_array = Cr_2_for_array(src);
        if (for_array == NULL)
          continue;
        Is_Trace(Tracing(),
                 (TFile,
                  "VSA::Identify_fsm_insts_stmt: Add left hand side cr%d to map for implicit assignment from cr%d.\n",
                  tgt->Coderep_id(), src->Coderep_id()));
        for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++)
          Enter_cr_for_array_map(tgt, (*for_array)[fa_idx]);
      }
    } // end rna->Arg_list()
  }

  CALLEE_VECTOR callee_list = rna->Callee_list();
  STRING_VEC visited_fsm;
  for (CALLEE_VECTOR::const_iterator callee_iter = callee_list.begin();
       callee_iter != callee_list.end(); callee_iter++) {
    DNA_NODE *callee = Ipsa()->Get_dna(callee_iter->Callee());
    if (callee == NULL)
      continue;
    if (callee->Is_set_rbc_flag(DNA_FSM_MODELLED)) {
      STRING str_act = callee->Fname();
      Is_Trace(Tracing(),
               (TFile,
                "VSA::Identify_fsm_insts_stmt: trying to bind callee:\"%s\" to an FSM.\n",
                str_act));
      DNODE_VECTOR *rbc_nodes = Rbc()->Get_rbc_nodes(callee);
      if (rbc_nodes == NULL)
        continue;
      for (DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
           rbc_iter != rbc_nodes->end();  rbc_iter++) {
        DNA_NODE *rbc_callee = *rbc_iter;
        if (rbc_callee == NULL)
          continue;
        STRING_VEC *fsm_list = rbc_callee->Fsm_list();
        if (fsm_list == NULL)
          continue;
        for (INT i = 0; i < fsm_list->size(); i++) {
          STRING fsm_name = (*fsm_list)[i];
          BOOL visited = FALSE;
          for (INT i = 0; i < visited_fsm.size(); i++) {
            if (strcmp(fsm_name, visited_fsm[i]) == 0) {
              visited = TRUE;
              break;
            }
          }
          if (visited)
            continue;
          visited_fsm.push_back(fsm_name);
          Is_Trace(Tracing(),
                   (TFile,
                    "VSA::Identify_fsm_insts_stmt: searching FSM:\"%s\" transitions.\n",
                    fsm_name));
          FSM_BASE *fsm_base = Rbc()->Find_fsm_base(fsm_name);
          if (fsm_base == NULL)
            continue;
          FSM *fsm = fsm_base->Fsm();
          Add_new_for(stmt, bb, def_bbs_pool, fsm_name, callee->Fname(), callee);
        } // end for fsm_list
      } // end for rbc_nodes
    }
  } // end for callee_list
}


// =============================================================================
//
// VSA::Is_action_match: match fsm defined transation with stmt rep transation
//
// =============================================================================
FSM_MATCH_KIND
VSA::Is_action_match(STMTREP *sr, const char* ft_def, const char* ft_value)
{
  if (strcmp(ft_def, ft_value) == 0) {
    return TS_EXACT_MATCH;
  }
  if (OPERATOR_is_call(sr->Opr())) {
    if (Is_candidate_action(sr, ft_def, ft_value)) {
      return TS_CAND_MATCH;
    }
    RNA_NODE *rna = Dna()->Get_callsite_rna(sr);
    if (rna && Rbc()->Find_rna_tag_info(Ipsa(), ft_def, rna)) {
      return TS_TAG_MATCH;
    }
  }
  return TS_NOT_MATCH;
}

// =============================================================================
//
// VSA::Add_new_for: Allocate new hor, and enter to map
//
// =============================================================================
void
VSA::Add_new_for(STMTREP *sr, BB_NODE *bb, MEM_POOL *def_bbs_pool,
                 STRING fsm_name, const char *ft_action, DNA_NODE *callee)
{
  FSM_BASE *fsm_base = Rbc()->Find_fsm_base(fsm_name);
  FSM *fsm = fsm_base->Fsm();
  for (INT i = 0; i < fsm->Ts_vec()->size(); i++) {
    TRANSIT *ts = (*fsm->Ts_vec())[i];
    STRING action = ts->Action();
    if (action == NULL || ts->Is_default())
      continue;
    FSM_MATCH_KIND mk = Is_action_match(sr, action, ft_action);
    if (mk == TS_NOT_MATCH)
      continue;
    FSM_OBJ_REP *fsm_obj_rep = Allocate_fsm_obj(bb, fsm_name, def_bbs_pool);
    CODEREP *key = NULL;
    if (OPERATOR_is_call(sr->Opr())) {
      RBC_CONTEXT rbc_ctx(Dna(), callee, fsm->Dna(), Dna()->Get_callsite_rna(sr), Loc_pool());
      TAG_INFO *tag_info = Rbc()->Find_dna_tag_info(action, callee);
      if (tag_info) {
        rbc_ctx.Set_tag_info(tag_info);
      }
      CONTEXT_SWITCH context(fsm->Dna());
      Rbc()->Rbc_init();
      key = (CODEREP*)Rbc()->Eval__exp(rbc_ctx, ts->Key());
      if (Rbc()->Rbc_result_ignore()) {
        key = NULL;
        Is_Trace(Tracing(), (TFile, "VSA::Add_new_for: KEY evaluation result ignored\n"));
      }
      Rbc()->Rbc_init();
      UINT64 retv = Rbc()->Eval__exp(rbc_ctx, ts->Cond());
      if (retv == 0 || Rbc()->Rbc_result_ignore()) {
        Is_Trace(Tracing(), (TFile, "VSA::Add_new_for: COND evaluation result ignored\n"));
        continue;
      }
    }
    else if (sr->Opr() == OPR_RETURN || sr->Opr() == OPR_RETURN_VAL) {
      STMTREP *prev_stmt = sr->Prev();
      if (prev_stmt != NULL && prev_stmt->Opr() == OPR_STID)
        key = prev_stmt->Lhs();
    }
    fsm_obj_rep->Set_attr(ROR_DEF_BY_TRANSIT);
    fsm_obj_rep->Set_transit(ts);
    fsm_obj_rep->Set_srcpos_node(sr, Dna(), PATHINFO_TRANSIT);
    fsm_obj_rep->Set_stmt_def(sr, Dna());
    fsm_obj_rep->Set_key(key);
    if (OPERATOR_is_call(sr->Opr())) {
      CODEREP *ret = Comp_unit()->Find_return_value(sr);
      if (ret != NULL)
        Enter_cr_for_array_map(ret, fsm_obj_rep);
    }
    if (key != NULL)
      Enter_cr_for_array_map(key, fsm_obj_rep);

    Is_Trace(Tracing(),
             (TFile,
              "VSA::Add_new_for: New FSM_OBJ_REP for FSM:\"%s\", transit #%d \"%s\", on stmt%d\n",
              fsm_name, i, ft_action, sr->Stmtrep_id()));
    Enter_sr_for_array_map(sr, fsm_obj_rep);
  }
  return;
}

// =============================================================================
//
// VSA::Is_candidate_action : return true if action2 is child class's virtual
// function
//
// =============================================================================
BOOL
VSA::Is_candidate_action(STMTREP *call, const char *action1, const char *action2) {
  CONTEXT_SWITCH ctx(Dna());
  if(strcmp(action1, action2) == 0) {
    return TRUE;
  }

  if(!OPERATOR_is_call(call->Opr()) ||
     !PU_java_lang(Get_Current_PU())){
    return FALSE;
  }
  BOOL ret = FALSE;
  if(Ipsa()->Glob_cha()) {
    CODEREP *callcr = call->Rhs();
    INT32 ofst = 0;
    if(call->Call_flags() & WN_CALL_IS_VIRTUAL) {
      CODEREP *ild = callcr->Opnd(callcr->Kid_count() -1);
      if (!Get_virtual_call_ofst(ild, ofst)) {
        Is_True(FALSE, ("Cannot get virtual call offset from virtual-call ILOAD"));
        return FALSE;
      }
    } else if(call->Call_flags() & WN_CALL_IS_INTERFACE) {
      TY_IDX ty_idx = TY_IDX_ZERO;
      if(!Get_intrfc_call_info(callcr, ofst, ty_idx)) {
        return FALSE;
      }
    } else {
      // direct call
      ofst = Ipsa()->Glob_cha()->Find_function_offset(action2, Loc_pool());
    }
    ret = Ipsa()->Glob_cha()->Is_candidate_call(action1, action2, ofst);
  }
  if(ret) {
    Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
             (TFile, "VSA::Identify_fsm_insts action %s is candidates of action %s\n", action2, action1));
  }
  return ret;
}

// =============================================================================
//
// VSA::Identify_fsm_insts traverse the CFG in Dom_tree_order and identify
//      function calls that has associated with any FSM and based on the action
//      it assocated with it to create a FSM instance as it's result.  This
//      instance will get attached to the RHS of the call statement.
//
// =============================================================================
void
VSA::Identify_fsm_insts(BB_NODE *bb, MEM_POOL *def_bbs_pool)
{
  Is_Trace(Get_Trace(TP_WOPT2, VSA_DUMP_FLAG),
           (TFile, "%sVSA::Identify_fsm_insts processing BB=%d\n", SBar, bb->Id()));
  // iterate through each statement in this bb
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Identify_fsm_insts_stmt(stmt, bb, def_bbs_pool);
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Identify_fsm_insts(dom_bb, def_bbs_pool);
}


// =============================================================================
//
//   Generate a Fsm_obj_rep for the result of a phi function.
//
// =============================================================================
FSM_OBJ_REP*
VSA::Gen_phi(PHI_NODE *phi, FSM_OBJ *fsm_obj)
{
  Is_True(fsm_obj != NULL, ("VSA::Gen_phi: could not find the fsm_obj by ID"));
  FSM_OBJ_REP *phi_result = Allocate_fsm_obj(fsm_obj);
  phi_result->Set_attr(ROR_DEF_BY_PHI);
  phi_result->Set_phi_def(phi);

  Is_Trace(Tracing(), (TFile, "VSA::Gen_phi: fsmID:%d, VerID:%d\n", fsm_obj->Id(), phi_result->State()));
  
  phi->Set_result((CODEREP *)phi_result);  // hack to keep the result

  return phi_result; 
}


// =============================================================================
//
//  VSA::Place_fo_phi_node places phi node for fsm_object
//
// =============================================================================
void
VSA::Place_fo_phi_node(void)
{
  PHILIST_MAP  *bb_ro_philist = Bb_fo_philist();
  FSM_OBJ      *fsm_obj;
  FO_LIST      *fsm_obj_list = Fsm_obj_list();
  FO_LIST_ITER  fo_list_iter;
  FSM_OBJ_REP  *forrep;
  Place_ro_phi_node(bb_ro_philist, fsm_obj, fsm_obj_list, &fo_list_iter, forrep);
}


// =============================================================================
//
// VSA::Perform_fsm_analysis find all FSM actions, perform phi insertion, then
//      apply the typical SSA rename function
//
// =============================================================================
void
VSA::Perform_fsm_analysis(CFG *cfg, MEM_POOL *def_bbs_pool)
{
  if (Dna()->Non_functional() || Dna()->Is_in_rbc_file())
    return;
  BB_NODE *bb;
  CFG_ITER cfg_iter;

  // Precondition: Init_DNA phase has created FSM according to user defined rule
  Is_Trace(Tracing(),
           (TFile, "%sVSA::Perform_fsm_analysis: Identify FSM for %s\n%s", SBar, Dna()->Fname(), SBar) );
  Identify_fsm_insts(cfg->Entry_bb(), def_bbs_pool);

  if (Count_fo_list() != 0) {

    // For every BBs, setup the Phi_list.
    // FOR_ALL_ELEM (bb, cfg_iter, Init(cfg))
    //   Enter_bb_fo_philist(bb, CXX_NEW( PHI_LIST(bb), Mem_pool() ));

    // Placement of fsm_obj phi functions
    // Is_Trace(Tracing(),
    //          (TFile, "%sVSA::Perform_fsm_analysis: Place fo PHI\n%s", SBar, SBar) );
    // Place_fo_phi_node();

    Is_Trace(Tracing(),
             (TFile, "%sAfter VSA::Perform_fsm_analysis: Dump FSM object list\n%s", SBar, SBar) );
    Is_Trace_cmd(Tracing(), Print_fo_list(TFile));

    // dump IR with for
    Is_Trace_cmd(Tracing(), Print_obj("FSM Object", TFile));
  }
}
