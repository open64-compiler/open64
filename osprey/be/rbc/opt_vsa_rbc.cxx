//-*-c++-*-

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

// =============================================================================
// =============================================================================
//
// Module: opt_vsa_rbc.cxx
//
// =============================================================================
// =============================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "glob.h"       // for Cur_PU_Name
#include "mempool.h"
#include "tracing.h"    /* for TFile */
#include "stab.h"
#include "irbdata.h"
#include "cxx_memory.h"
#include "be_symtab.h"
#include "erbe.h"

#include "opt_defs.h"
#include "opt_config.h"
#include "opt_cfg.h"
#include "opt_dbg.h"
#include "opt_dna.h"
#include "opt_estr.h"
#include "opt_etable.h"
#include "opt_main.h"
#include "bb_node_set.h"
#include "opt_util.h"
#include "opt_vsa_util.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "opt_sym.h"
#include "opt_alias_rule.h"
#include "opt_cvtl_rule.h"
#include "opt_lftr2.h"
#include "config_vsa.h"
#include "report.h"    // for vsa report
#include "opt_vsa.h"
#include "opt_vra.h"
#include "j_class_hierarchy_bldr.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_report.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_var_def.h"
#include "opt_vsa_tag_prop.h"
#include <fcntl.h>
#include "../../include/gnu/demangle.h"

#define DEF_RBC_OP(eval_op, init_ptr, eval_ptr, fname)  { eval_op, init_ptr, eval_ptr, fname },
RBC_BASE::RBC_OP_DESC RBC_BASE::_eval_op_table[] = {
  #include "rbc_eval_op.inc"
};
#undef DEF_RBC_OP

const char*
RBC_BASE::Rbc_op_name(RBC_OP op)
{
  Is_True_Ret(op < sizeof(_eval_op_table)/sizeof(RBC_OP_DESC), ("rbc op overflow"), "");
  return _eval_op_table[op]._name;
}

// =============================================================================
//
// RBC_BASE::Switch_plist
//
// =============================================================================
void
RBC_BASE::Switch_plist(void)
{
  SRCPOS_HANDLE_VEC *tmp = _true_plist;
  _true_plist = _false_plist;
  _false_plist = tmp;
}


// =============================================================================
//
// COMPLEXITY hack: RBC_BASE::Builtin_func_map_init
//
// =============================================================================
void
RBC_BASE::Rule_fix_cost_init(void)
{
  // cert-c rules
  _rbc_fix_cost_map["ARR38-C"] = 3;
  _rbc_fix_cost_map["ENV32-C"] = 3;
  _rbc_fix_cost_map["ENV33-C"] = 3;
  _rbc_fix_cost_map["ERR33-C"] = 3;
  _rbc_fix_cost_map["FIO02-C"] = 3;
  _rbc_fix_cost_map["FIO30-C"] = 3;
  _rbc_fix_cost_map["FIO34-C"] = 3;
  _rbc_fix_cost_map["FIO37-C"] = 3;
  _rbc_fix_cost_map["FIO42-C"] = 3;
  _rbc_fix_cost_map["FIO45-C"] = 5;
  _rbc_fix_cost_map["FIO47-C"] = 1;
  _rbc_fix_cost_map["MEM35-C"] = 5;
  _rbc_fix_cost_map["MSC30-C"] = 1;
  _rbc_fix_cost_map["MSC32-C"] = 1;
  _rbc_fix_cost_map["MSC33-C"] = 1;
  _rbc_fix_cost_map["MSC37-C"] = 1;
  _rbc_fix_cost_map["MSC41-C"] = 3;
  _rbc_fix_cost_map["POS30-C"] = 3;
  _rbc_fix_cost_map["POS34-C"] = 3;
  _rbc_fix_cost_map["POS35-C"] = 3;
  _rbc_fix_cost_map["POS37-C"] = 1;
  _rbc_fix_cost_map["POS54-C"] = 3;
  _rbc_fix_cost_map["SIG30-C"] = 3;
  _rbc_fix_cost_map["SIG31-C"] = 5;
  _rbc_fix_cost_map["STR02-C"] = 3;
  _rbc_fix_cost_map["STR31-C"] = 3;
  _rbc_fix_cost_map["STR32-C"] = 3;
  _rbc_fix_cost_map["STR38-C"] = 1;
  _rbc_fix_cost_map["MEM55-CPP"] = 3;
  _rbc_fix_cost_map["MSC51-CPP"] = 3;
  _rbc_fix_cost_map["MSC54-CPP"] = 5;
  _rbc_fix_cost_map["STR50-CPP"] = 3;

  // cert-j rules
  _rbc_fix_cost_map["DCL00-J"] = 3;
  _rbc_fix_cost_map["ENV01-J"] = 3;
  _rbc_fix_cost_map["ENV03-J"] = 1;
  _rbc_fix_cost_map["ENV06-J"] = 1;
  _rbc_fix_cost_map["ERR00-J"] = 3;
  _rbc_fix_cost_map["ERR08-J"] = 3;
  _rbc_fix_cost_map["EXP02-J"] = 1;
  _rbc_fix_cost_map["FIO02-J"] = 3;
  _rbc_fix_cost_map["FIO05-J"] = 1;
  _rbc_fix_cost_map["FIO08-J"] = 3;
  _rbc_fix_cost_map["FIO14-J"] = 3;
  _rbc_fix_cost_map["FIO16-J"] = 3;
  _rbc_fix_cost_map["FIO52-J"] = 3;
  _rbc_fix_cost_map["IDS00-J"] = 3;
  _rbc_fix_cost_map["IDS01-J"] = 3;
  _rbc_fix_cost_map["IDS03-J"] = 3;
  _rbc_fix_cost_map["IDS04-J"] = 5;
  _rbc_fix_cost_map["IDS06-J"] = 3;
  _rbc_fix_cost_map["IDS07-J"] = 3;
  _rbc_fix_cost_map["IDS11-J"] = 3;
  _rbc_fix_cost_map["IDS15-J"] = 5;
  _rbc_fix_cost_map["IDS16-J"] = 3;
  _rbc_fix_cost_map["IDS17-J"] = 3;
  _rbc_fix_cost_map["IDS51-J"] = 3;
  _rbc_fix_cost_map["IDS53-J"] = 3;
  _rbc_fix_cost_map["IDS54-J"] = 3;
  _rbc_fix_cost_map["JNI01-J"] = 1;
  _rbc_fix_cost_map["MET06-J"] = 1;
  _rbc_fix_cost_map["MSC02-J"] = 3;
  _rbc_fix_cost_map["MSC03-J"] = 3;
  _rbc_fix_cost_map["MSC61-J"] = 1;
  _rbc_fix_cost_map["MSC62-J"] = 1;
  _rbc_fix_cost_map["OBJ01-J"] = 3;
  _rbc_fix_cost_map["OBJ05-J"] = 3;
  _rbc_fix_cost_map["OBJ07-J"] = 3;
  _rbc_fix_cost_map["OBJ09-J"] = 1;
  _rbc_fix_cost_map["OBJ11-J"] = 3;
  _rbc_fix_cost_map["OBJ13-J"] = 1;
  _rbc_fix_cost_map["SEC01-J"] = 1;
  _rbc_fix_cost_map["SEC02-J"] = 3;
  _rbc_fix_cost_map["SEC03-J"] = 3;
  _rbc_fix_cost_map["SEC04-J"] = 3;
  _rbc_fix_cost_map["SEC05-J"] = 3;
  _rbc_fix_cost_map["SEC06-J"] = 3;
  _rbc_fix_cost_map["SEC07-J"] = 1;
  _rbc_fix_cost_map["SER01-J"] = 1;
  _rbc_fix_cost_map["SER05-J"] = 3;
}

// =============================================================================
//
// RBC_BASE::Builtin_func_map_init
//
// =============================================================================
void
RBC_BASE::Builtin_func_map_init(void)
{
  for (int op_idx = 0; op_idx < sizeof(_eval_op_table)/sizeof(RBC_OP_DESC); op_idx++) {
    RBC_OP_DESC *desc = &(_eval_op_table[op_idx]);
    _eval_name_op_map[desc->_name] = desc->_eval_op;
  }
  // new map
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_ARG] = &RBC_BASE::Eval__get_arg;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_RET] = &RBC_BASE::Eval__get_ret;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_ARGCNT] = &RBC_BASE::Eval__get_argcnt;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_MEM_SIZE] = &RBC_BASE::Eval__get_mem_size;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_VALUE] = &RBC_BASE::Eval__get_value;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_ELEM_COUNT] = &RBC_BASE::Eval__get_elem_count;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_THIS_POINTER] = &RBC_BASE::Eval__get_this_pointer;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_TYPE_NAME] = &RBC_BASE::Eval__get_type_name;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_GET_TYPE_KIND] = &RBC_BASE::Eval__get_type_kind;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_PARM_TAINTED] = &RBC_BASE::Eval__set_parm_tainted;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_IMPLICIT_ASSIGN] = &RBC_BASE::Eval__set_implicit_assign;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_PARM_DEREF] = &RBC_BASE::Eval__set_parm_deref;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_PARM_MOD] = &RBC_BASE::Eval__set_parm_mod;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_PARM_BASE_AND_FLDNM] = &RBC_BASE::Eval__set_parm_base_and_fld_name;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_MAY_SLEEP] = &RBC_BASE::Eval__set_func_may_sleep;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_ATOMIC_REGION_BEGIN] = &RBC_BASE::Eval__set_atomic_region_begin;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_ATOMIC_REGION_END] = &RBC_BASE::Eval__set_atomic_region_end;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_ATOMIC] = &RBC_BASE::Eval__set_func_atomic;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_SHUTDOWN] = &RBC_BASE::Eval__set_func_shutdown;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_COLL_APPEND] = &RBC_BASE::Eval__set_func_coll_append;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_COLL_REMOVE] = &RBC_BASE::Eval__set_func_coll_remove;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_FUNC_COLL_GET] = &RBC_BASE::Eval__set_func_coll_get;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_STR_EQ] = &RBC_BASE::Eval__is_str_eq;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_STR_SUB] = &RBC_BASE::Eval__is_str_sub;

  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DECLARE_MALLOC_SIMILAR] = &RBC_BASE::Eval__declare_malloc_similar;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DECLARE_FREE_SIMILAR] = &RBC_BASE::Eval__declare_free_similar;

  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_TAG] = &RBC_BASE::Eval__set_tag;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_UNSET_TAG] = &RBC_BASE::Eval__unset_tag;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_TAG_SET] = &RBC_BASE::Eval__is_tag_set;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_OR_TAG] = &RBC_BASE::Eval__or_tag;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_COPY_TAG] = &RBC_BASE::Eval__copy_tag;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_EVAL_TAG] = &RBC_BASE::Eval__eval_tag;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_SET_TAG_CONST_DEFVAL] = &RBC_BASE::Eval__set_tag_const_defval;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DECL_TAG_EQUAL] = &RBC_BASE::Eval__decl_tag_equal;

  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_PRE_SANITIZED] = &RBC_BASE::Eval__pre_sanitized;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_PRE_CALL] = &RBC_BASE::Eval__pre_call;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_PRE_CHECK_VAR_VALUE] = &RBC_BASE::Eval__pre_check_var_value;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_POST_CHECK_VAR_VALUE] = &RBC_BASE::Eval__post_check_var_value;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_POST_CALL] = &RBC_BASE::Eval__post_call;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_PARM_IS_DEF_BY_FUNC] = &RBC_BASE::Eval__parm_is_def_by_func;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_FUNC_MAY_ENTER_RECURSION] = &RBC_BASE::Eval__func_may_enter_recursion;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_FUNC_MAY_NOT_RETURN] = &RBC_BASE::Eval__func_may_not_return;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_FUNC_IS_ASYNCHRONOUS_SAFE] = &RBC_BASE::Eval__func_is_asynchronous_safe;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_FUNC_PERFORMS_SANITIZE] = &RBC_BASE::Eval__func_performs_sanitize;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_AUTOMATIC_VARIABLE] = &RBC_BASE::Eval__is_automatic_variable;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_DYNAMICALLY_ALLOCATED_IF_COPIED] = &RBC_BASE::Eval__is_dynamically_allocated_if_copied;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_COMPATIBLE_PARM_TYPE] = &RBC_BASE::Eval__is_compatible_parm_type;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_PARM_TAINTED] = &RBC_BASE::Eval__is_parm_tainted;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DO_NOT_GET_CALLED] = &RBC_BASE::Eval__do_not_get_called;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DO_NOT_ACCESS_SHARED_OBJ] = &RBC_BASE::Eval__do_not_access_shared_obj;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_DO_NOT_CALL_SLEEP_IN_ATM] = &RBC_BASE::Eval__do_not_call_sleep_in_atm;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IMPLICIT_CALL] = &RBC_BASE::Eval__implicit_call;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_CALL_SUPER] = &RBC_BASE::Eval__call_super;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_FUNC_INVOKED_BY_SUBCLASS] = &RBC_BASE::Eval__func_invoked_by_subclass;
  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_IS_OBJ_METH_OVERRIDE] = &RBC_BASE::Eval__is_obj_meth_override;

  _builtin_func_map_new[RBC_ENGINE::EXEC_KIND_JNI_MODEL_PRAGMA] = &RBC_BASE::Eval__jni_model_pragma;
}


// =============================================================================
// Functions called init_dna phase
// =============================================================================

// =============================================================================
//
// RBC_BASE::Init__fsm_use
//
// =============================================================================
void
RBC_BASE::Init__fsm_use(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_use(CONST STRING fsm_name)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_use don't have first arg"));
  char* fsm_name = Find_const_char(enclosing_dna, arg1);
  Is_True(fsm_name != NULL, ("Can't get const string in Fsm_use"));
  enclosing_dna->Set_rbc_flag(DNA_FSM_MODELLED);
  enclosing_dna->Add_fsm_list(Clone_string((STRING)fsm_name, Mem_pool()));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__fsm_build_begin
//
// =============================================================================
void
RBC_BASE::Init__fsm_build_begin(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_build_begin(CONST STRING fsm_name)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_build_begin don't have first arg"));
  char* fsm_name = Find_const_char(enclosing_dna, arg1);
  Is_True(fsm_name != NULL, ("Can't get const string in Fsm_build_begin"));
  Is_True(Current_fsm_base() == NULL, ("Calling New_fsm_base for nested FSM creation"));
  FSM_BASE *fsm_base = New_fsm_base(Clone_string(fsm_name, Mem_pool()));
  Set_current_fsm_base(fsm_base);
  fsm_base->Fsm()->Set_dna(enclosing_dna);
  Is_Trace(Tracing(), (TFile, "Process_model_decl_func: Fsm build begin, fsm : %s.\n", fsm_name));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__fsm_new_start_state
//
// =============================================================================
void
RBC_BASE::Init__fsm_new_start_state(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_new_start_state(CONST STRING state_name)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_new_start_state don't have first arg"));
  char* state_name = Find_const_char(enclosing_dna, arg1);
  Is_True(state_name != NULL, ("Can't get const string in Fsm_new_start_state"));
  Is_True(Current_fsm_base() != NULL, ("Not in the context of a FSM builder"));
  Current_fsm_base()->Fsm()->Set_start(Clone_string(state_name, Mem_pool()));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Eval__fsm_new_final_state
//
// =============================================================================
void
RBC_BASE::Init__fsm_new_final_state(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_new_final_state(CONST STRING state_name)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_new_final_state don't have first arg"));
  char* state_name = Find_const_char(enclosing_dna, arg1);
  Is_True(state_name != NULL, ("Can't get const string in Fsm_new_final_state"));
  Is_True(Current_fsm_base() != NULL, ("Not in the context of a FSM builder"));
  Current_fsm_base()->Fsm()->Set_final(Clone_string(state_name, Mem_pool()));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__fsm_add_transition
//
// =============================================================================
void
RBC_BASE::Init__fsm_add_transition(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_add_transition(CONST STRING state, CONST STRING action, OBJECT key,
  //                           BOOL cond, CONST STRING next_state, CONST STRING errcode)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_add_transition don't have first arg"));
  char* state = Find_const_char(enclosing_dna, arg1);
  Is_True(state != NULL, ("Can't get const string in Fsm_add_transition"));
  IDTYPE   statenum = Current_fsm_base()->Fsm()->Get_state(state);
  CODEREP *arg2   = model_call->Rhs()->Find_nth_arg(1 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg2 != NULL, ("Fsm_add_transition don't have second arg"));
  char* action = Find_const_char(enclosing_dna, arg2);
  Is_True(action != NULL, ("Can't get const string in Fsm_add_transition"));
  CODEREP *key = model_call->Rhs()->Find_nth_arg(2 + Rbc_parm_offset(enclosing_dna));
  CODEREP *cond = model_call->Rhs()->Find_nth_arg(3 + Rbc_parm_offset(enclosing_dna));

  CODEREP *arg5   = model_call->Rhs()->Find_nth_arg(4 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg5 != NULL, ("Fsm_add_transition don't have 5th arg"));
  char* nstate = Find_const_char(enclosing_dna, arg5);
  Is_True(nstate != NULL, ("Can't get const string in Fsm_add_transition"));
  IDTYPE   nstatenum = Current_fsm_base()->Fsm()->Get_state(nstate);
  CODEREP *arg6   = model_call->Rhs()->Find_nth_arg(5 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg6 != NULL, ("Fsm_add_transition don't have 6th arg"));
  char* errcode = Find_const_char(enclosing_dna, arg6);
  if (errcode != NULL) {
    if (strlen(errcode) == 0)
      errcode = NULL;
    else
      errcode = Clone_string(errcode, Mem_pool());
  }
  else
    Is_True(arg6->Kind() == CK_CONST && arg6->Const_val() == 0,
            ("Invalid errcode in Fsm_add_transition"));

  STRING_VEC *errcode_vec = Build_vec_from_string(errcode, Mem_pool());
  CODEREP *arg7 = model_call->Rhs()->Find_nth_arg(6 + Rbc_parm_offset(enclosing_dna));
  INT32 msg_id = PATHINFO_TRANSIT;
  if (arg7 != NULL && arg7->Kind() == CK_CONST) {
    msg_id = (INT32)arg7->Const_val();
    Is_True(msg_id < 0 || msg_id >= PATHINFO_MAX, ("Message ID conflict with builtin in"));
  }
  else
    Is_True(FALSE, ("Invalid msg_id"));

  Is_True(Current_fsm_base() != NULL, ("Not in the context of a FSM builder"));
  Current_fsm_base()->Fsm()->Add_transition(statenum, action, key, cond, nstatenum, errcode_vec, msg_id);
  if (strcmp(action, "if") == 0 || strcmp(action, "if:test") == 0) {
    Current_fsm_base()->Fsm()->Set_flag(FSM_ATTR_IF_TRANSIT);
  }
  else if (strcmp(action, "return") == 0) {
    Current_fsm_base()->Fsm()->Set_flag(FSM_ATTR_RET_TRANSIT);
  }
  model->Clear_flag(RBC_SE_MODEL);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__fsm_set_default_action
//
// =============================================================================
void
RBC_BASE::Init__fsm_set_default_action(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_set_default_action(CONST STRING default_action, CONST STRING errcode)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_set_default_action don't have first arg"));
  char* def_act = Find_const_char(enclosing_dna, arg1);
  Is_True(def_act != NULL, ("Can't get const string in Fsm_set_default_action"));

  CODEREP *arg2   = model_call->Rhs()->Find_nth_arg(1 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg2 != NULL, ("Fsm_set_default_action don't have second arg"));
  char* errcode = Find_const_char(enclosing_dna, arg2);
  if (errcode != NULL) {
    if (strlen(errcode) == 0)
      errcode = NULL;
    else
      errcode = Clone_string(errcode, Mem_pool());
  }
  else
    Is_True(arg2->Kind() == CK_CONST && arg2->Const_val() == 0,
            ("Invalid errcode in FSM_set_default_action\n"));

  STRING_VEC *errcode_vec = Build_vec_from_string(errcode, Mem_pool());
  CODEREP *arg3 = model_call->Rhs()->Find_nth_arg(2 + Rbc_parm_offset(enclosing_dna));
  INT32 msg_id = PATHINFO_TRANSIT;
  if (arg3 != NULL && arg3->Kind() == CK_CONST) {
    msg_id = (INT32)arg3->Const_val();
    Is_True(msg_id < 0 || msg_id >= PATHINFO_MAX, ("Message ID conflict with builtin in"));
  }
  else
    Is_True(FALSE, ("Invalid msg_id"));

  Is_True(Current_fsm_base() != NULL, ("Not in the context of a FSM builder"));
  Current_fsm_base()->Fsm()->Set_default_action(Clone_string(def_act, Mem_pool()), errcode_vec, msg_id);
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__fsm_build_end
//
// =============================================================================
void
RBC_BASE::Init__fsm_build_end(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // BOOL   Fsm_build_end(CONST STRING fsm_name)
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(enclosing_dna));
  Is_True(arg1 != NULL, ("Fsm_set_default_action don't have first arg"));
  char* fsm_name = Find_const_char(enclosing_dna, arg1);
  Is_True(fsm_name != NULL, ("Can't get const string in Fsm_set_default_action"));
  Is_True(Current_fsm_base() != NULL, ("Not in the context of a FSM builder"));
  Is_True(strcmp(fsm_name, Current_fsm_base()->Fsm_name()) == 0,
          ("Fsm_build_end not in the context %s", Current_fsm_base()->Fsm_name()));

  // verify the FSM
  Current_fsm_base()->Fsm()->Verify();
  Is_Trace(Tracing(), (TFile, "Process_model_decl_func: Fsm build end, fsm : %s.\n", fsm_name));
  Is_Trace_cmd(Tracing(), Current_fsm_base()->Fsm()->Print(TFile));
  Reset_current_fsm_base();
  model->Clear_flag(RBC_SE_MODEL);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__if
// BOOL   If(BOOL v, BOOL true_exp, BOOL false_exp)
// Note: restrict IF only used for tag op, as to evaluate condition, it need symbolic
//       eval, mvsa_model phase is too early to do the evaluate
//       also the condition can not contain tag checking api, as tag is not set up yet
// =============================================================================
void
RBC_BASE::Init__if(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__not
//
// =============================================================================
void
RBC_BASE::Init__not(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  STMTREP *arg0 = Get_rbc_nth_call(model_call, 0 + Rbc_parm_offset(enclosing_dna));
  if (arg0 == NULL) {
    return;
  }
  Is_True_Ret(arg0->Opr() == OPR_CALL, ("Init__not: bad arg0 call"));

  RNA_NODE *rna = enclosing_dna->Get_callsite_rna(arg0);
  if (rna == NULL) return;

  RBC_OP arg0_op = rna->Rbc_op();
  INIT_FUNC func = Init_func(arg0_op);
  if (func != NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Init__not init func for %s\n",
                         Rbc_op_name(arg0_op)));
    (this->*func)(ipsa, enclosing_dna, model, arg0);
  }
}

// =============================================================================
//
// RBC_BASE::Init__is_tag_set
// BOOL   Is_tag_set(OBJECT obj, const STRING tag)
//
// =============================================================================
void
RBC_BASE::Init__is_tag_set(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  UINT32 obj_idx = 0;  // first parameter in rbc api
  UINT32 tag_idx = 1;  // second parameter in rbc api
  // autoly set parm flags for obj to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, obj_idx, REF_ILOAD);
  TAG_BASE *tag_base = Create_tag_base(enclosing_dna, model_call, tag_idx);
  if (tag_base) {
    tag_base->Set_flag(RSC_TAG_USED);
  }
}

// =============================================================================
//
// RBC_BASE::Init__is_tag_attr_set
// BOOL   Is_tag_attr_set(OBJECT v, const STRING tag, const STRING attr)
//
// =============================================================================
void
RBC_BASE::Init__is_tag_attr_set(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  UINT32 obj_idx = 0;  // first parameter in rbc api
  UINT32 tag_idx = 1;  // second parameter in rbc api
  // autoly set parm flags for obj to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ILOAD);
  TAG_BASE *tag_base = Create_tag_base(enclosing_dna, model_call, tag_idx);
  if (tag_base) {
    tag_base->Set_flag(RSC_TAG_USED);
  }
}

// =============================================================================
//
// RBC_BASE::Init__set_tag
// BOOL   Set_tag(OBJECT obj, RBC_STRING tag)
//
// =============================================================================
void
RBC_BASE::Init__set_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  UINT32 obj_idx = 0;  // first parameter in rbc api
  UINT32 tag_idx = 1;  // second parameter in rbc api
  // autoly set parm flags for obj to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE);
  Create_tag_base(enclosing_dna, model_call, tag_idx);
}


// =============================================================================
//
// RBC_BASE::Init__or_tag
// BOOL   Or_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
void
RBC_BASE::Init__or_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  // autoly set parm flags for tgt/src to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE|REF_ILOAD);
  Set_obj_parm_flag(enclosing_dna, model_call, 1, REF_ILOAD);

  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__merge_tag
// BOOL   Merge_tag(OBJECT tgt, OBJECT src1, OBJECT src2)
//
// =============================================================================
void
RBC_BASE::Init__merge_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  // autoly set parm flags for tgt/src to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE|REF_ILOAD);
  Set_obj_parm_flag(enclosing_dna, model_call, 1, REF_ILOAD);
  Set_obj_parm_flag(enclosing_dna, model_call, 2, REF_ILOAD);

  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__copy_tag
// BOOL   Copy_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
void
RBC_BASE::Init__copy_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  // autoly set parm flags for tgt/src to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE);
  Set_obj_parm_flag(enclosing_dna, model_call, 1, REF_ILOAD);

  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__eval_tag
// BOOL   Eval_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
void
RBC_BASE::Init__eval_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  // autoly set parm flags for tgt/src to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE);
  Set_obj_parm_flag(enclosing_dna, model_call, 1, REF_ILOAD);

  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__set_tag_const_defval
// BOOL   Set_tag_const_defval(const STRING tag, UINT64 value)
//
// =============================================================================
void
RBC_BASE::Init__set_tag_const_defval(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  UINT32 tag_idx = 0;  // first parameter in rbc api
  TAG_BASE *tag_base = Create_tag_base(enclosing_dna, model_call, tag_idx);
  Is_True_Ret(tag_base, ("RBC_BASE::Init__set_tag_const_defval::Create_tag_base failed"));

  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(enclosing_dna));
  Is_True_Ret(arg1->Kind() == CK_CONST, ("not const value in Set_tag_const_defval\n"));
  INT64 value = arg1->Const_val();
  Is_True_Ret(value >= RBC_ENGINE::TAG_KEEP &&
              value <= RBC_ENGINE::TAG_UNSET,
              ("invalid default value in Set_tag_const_defval\n"));

  TAG_CONF_INFO *info = Get_tag_conf_info(tag_base->Id());
  if(info == NULL) {
    TAG_CONF_INFO new_info;
    new_info.Set_cst_val((TAG_VAL)value);
    Enter_tag_conf_map(tag_base->Id(), new_info);
  } else {
    info->Set_cst_val((TAG_VAL)value);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Set_tag_const_defval(\"%s\", %lld)\n", tag_base->Tag_name(), value));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_tag_input_defval
// BOOL   Set_tag_input_defval(const STRING tag, UINT64 value)
//
// =============================================================================
void
RBC_BASE::Init__set_tag_input_defval(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  UINT32 tag_idx = 0;  // first parameter in rbc api
  TAG_BASE *tag_base = Create_tag_base(enclosing_dna, model_call, tag_idx);
  Is_True_Ret(tag_base, ("RBC_BASE::Init__set_tag_input_defval Create_tag_base failed"));
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(enclosing_dna));
  Is_True_Ret(arg1->Kind() == CK_CONST, ("not const value in Set_tag_const_defval\n"));
  INT64 value = arg1->Const_val();
  Is_True(value >= RBC_ENGINE::TAG_KEEP &&
          value <= RBC_ENGINE::TAG_UNSET,
          ("invalid default value in Set_tag_const_defval\n"));

  TAG_CONF_INFO * info = Get_tag_conf_info(tag_base->Id());
  if(info == NULL) {
    TAG_CONF_INFO new_info;
    new_info.Set_input_val((TAG_VAL)value);
    Enter_tag_conf_map(tag_base->Id(), new_info);
  } else {
    info->Set_input_val((TAG_VAL)value);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Set_tag_input_defval(\"%s\", %lld)\n", tag_base->Tag_name(), value));
  model->Clear_flag(RBC_SE_MODEL);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_tag_attr
// BOOL   Set_tag_attr(OBJECT tgt, OBJRCT src, const STRING tag, const STRING attr)
//
// =============================================================================
void
RBC_BASE::Init__set_tag_attr(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;
  // autoly set parm flags for tgt/src to create vsym for tag binding
  Set_obj_parm_flag(enclosing_dna, model_call, 0, REF_ISTORE);
  Set_obj_parm_flag(enclosing_dna, model_call, 1, REF_ILOAD);
  TAG_BASE *tag_base = Create_tag_base(enclosing_dna, model_call, 2);
  // set tag used flag in init phase, as Eval__set_tag_attr only performed in tag propagation
  // which is to late to mark tag use flag
  if (tag_base) {
    tag_base->Set_flag(RSC_TAG_USED);
  }

  CODEREP *arg3 = model_call->Rhs()->Find_nth_arg(3 + Rbc_parm_offset(enclosing_dna));
  char *attr_name = Find_const_char(enclosing_dna, arg3);
  Is_True(attr_name != NULL, ("RBC ERROR: Set_tag_attr: null attr\n"));
  Add_tag_attr(attr_name);
  enclosing_dna->Set_rbc_flag(DNA_RBC_TAG_OP);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  model->Set_flag(RBC_SE_TAG_OP);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_tag_for_all_parm
//     Set_tag_for_all_parm(STRING tag)
//
// =============================================================================
void
RBC_BASE::Init__set_tag_for_all_parm(IPSA *ipsa, DNA_NODE *enclosing_dna,
                                     RNA_NODE *model, STMTREP *model_call)
{
  if (!VSA_Enable_TAG)
    return;

  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(enclosing_dna));
  char *tag_name = Find_const_char(enclosing_dna, arg1);
  Is_True_Ret(tag_name != NULL, ("RBC ERROR: Set_tag_for_all_parm: null tag\n"));
  // set tag for each parameter of enclosing_dna
  for (INT i = PDV_INIT_ID; i < enclosing_dna->Parm_list()->size(); i++) {
    // autoly set parm flags to create vsym for tag binding
    ST_IDX parm_st = enclosing_dna->Get_param_stidx(i);
    if (parm_st != ST_IDX_ZERO && TY_kind(ST_type(parm_st)) == KIND_POINTER) {
      enclosing_dna->Set_parm_flag(i, REF_ISTORE);
    }
  }
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_class_sensitive
// BOOL   Set_class_sensitive() - mark sensitive flag on class type
//
// =============================================================================
void
RBC_BASE::Init__set_class_sensitive(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  Is_True_Ret(PU_is_constructor(Get_Current_PU()),
              ("RBC ERROR: Set_class_sensitive should only be marked on constructor\n"));
  CONTEXT_SWITCH context(enclosing_dna);
  VAR_NODE *func_parm = (*enclosing_dna->Parm_list())[1];
  ST *this_st = ST_ptr(func_parm->St_idx());
  if (this_st != NULL && ST_is_this_ptr(this_st)) {
    TY_IDX ty_idx = ST_type(this_st);
    if(TY_kind(ty_idx) == KIND_POINTER) {
      TY_IDX class_ty = TY_pointed(ty_idx);
      Set_TY_is_sensitive_class(class_ty);
      Is_Trace(Tracing(),
               (TFile, "RBC: Set_class_sensitive, set class %s sensitive\n",
               TY_name(class_ty)));
    } else {
      Is_True_Ret(Tracing(),
                  ("RBC: Set_class_sensitive, dna %s first parameter is not a pointer",
                   enclosing_dna->Fname()));
    }
  } else {
    Is_True_Ret(Tracing(), 
      ("RBC: Set_class_sensitive, constructor %s has no this ptr",
       enclosing_dna->Fname()));
  }
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_ty_is_mutex
// BOOL   Set_ty_is_mutex() - mark mutex flag on type
//
// =============================================================================
void
RBC_BASE::Init__set_ty_is_mutex(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // TO BE IMPLEMENTED
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_ty_is_atomic
// BOOL   Set_ty_is_atomic() - mark atomic flag on type
//
// =============================================================================
void
RBC_BASE::Init__set_ty_is_atomic(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // TO BE IMPLEMENTED
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_is_thread
// BOOL   Set_ty_is_thread() - mark thread flag on type
//
// =============================================================================
void
RBC_BASE::Init__set_ty_is_thread(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  // TO BE IMPLEMENTED
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_func_tag
// BOOL   Set_func_tag(RBC_CONST RBC_STRING tag_fname, TAGOBJ_VARARG)
//
// =============================================================================
void
RBC_BASE::Init__set_func_tag(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  CODEREP *call_rhs = model_call->Rhs();
  Is_Trace_cmd(Tracing(), model_call->Print(TFile));
  UINT32 arg_ofst = Rbc_parm_offset(enclosing_dna);
  UINT32 arg_idx = arg_ofst;
  CODEREP *arg1 = call_rhs->Find_nth_arg(arg_idx++);
  Is_True(arg1 != NULL, ("Set_func_tag missing 1st arg tag_fname"));
  char* tag_fname = Find_const_char(enclosing_dna, arg1);
  Is_True(tag_fname != NULL, ("Set_func_tag can not find function name string"));
  if (tag_fname == NULL) {
    Is_Trace(Tracing(),
             (TFile, "RBC ERROR: Set_func_tag[%s]: not set tagged function name\n",
              enclosing_dna->Fname()));
    return;
  }
  if (Find_dna_tag_info(tag_fname, enclosing_dna) == NULL) {
    INT32 arg_cnt = call_rhs->Kid_count();
    TAG_OBJS* tagobjs = NULL;
    if (arg_idx < arg_cnt) {
      tagobjs = CXX_NEW(TAG_OBJS(mempool_allocator<CODEREP *>(Mem_pool())), Mem_pool());
    }
    Enter_dna_tag_info(tag_fname, enclosing_dna, tagobjs);
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Set_func_tag[%s]: add tag_fname %s with tag_obj: ",
                         enclosing_dna->Fname(), tag_fname));
    for (; arg_idx < arg_cnt; arg_idx++) {
      CODEREP *arg_n = call_rhs->Find_nth_arg(arg_idx);
      tagobjs->push_back(arg_n);
      Is_Trace(Tracing(), (TFile, "cr%d ",arg_n->Coderep_id()));
    }
    Is_Trace(Tracing(), (TFile, "\n"));
  } else {
    Is_True(FALSE, ("RBC ERROR: Set_func_tag[%s]: duplicate call for tag %s\n",
                    enclosing_dna->Fname(), tag_fname));
    Is_Trace(Tracing(),
             (TFile, "RBC ERROR: Set_func_tag[%s]: duplicate call for tag %s\n",
              enclosing_dna->Fname(), tag_fname));
  }
  enclosing_dna->Set_rbc_flag(DNA_RBC_FUNC_TAG);
}

// =============================================================================
//
// RBC_BASE::Init__do_not_call
// BOOL   Do_not_call(RBC_CONST RBC_STRING fname)
//
// =============================================================================
void
RBC_BASE::Init__do_not_call(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  enclosing_dna->Set_rbc_flag(DNA_RBC_ASSERT_DNA);
  model->Set_flag(RBC_SE_ASSERT_DNA);
  model->Clear_flag(RBC_SE_ASSERT);
  return;
}


// =============================================================================
//
// RBC_BASE::Init__is_parm_type_addr_passed
// BOOL   Is_parm_type_addr_passed(RBC_CONST RBC_STRING typename)
//
// =============================================================================
void
RBC_BASE::Init__is_parm_type_addr_passed(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  enclosing_dna->Set_rbc_flag(DNA_RBC_ASSERT_DNA);
  model->Set_flag(RBC_SE_ASSERT_DNA);
  model->Clear_flag(RBC_SE_ASSERT);
  return;
}

// =============================================================================
//
// RBC_BASE::Init__set_parm_mod
// BOOL   Set_parm_mod(OBJECT obj)
//   Resolve the phase-ordering issue of mvsa_model later than propagation
//   Trying to extract parm_idx from rbc call and set flag on rbc callee
// =============================================================================
void
RBC_BASE::Init__set_parm_mod(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  IDTYPE obj_parm_idx = Extract_get_arg_idx(enclosing_dna, model_call, 0);
  if(obj_parm_idx > INVALID_VAR_IDX &&
     obj_parm_idx < enclosing_dna->Parm_list()->size()) {
    enclosing_dna->Set_parm_flag(obj_parm_idx,  REF_ISTORE);
    Is_Trace(Tracing(),
      (TFile, "RBC_BASE::Init__set_parm_mod: set REF_ISTORE for %s parm(%d)\n",
       enclosing_dna->Fname(), obj_parm_idx));
  }
}

// =============================================================================
//
// RBC_BASE::Init__set_parm_deref
// BOOL   Set_parm_deref(OBJECT obj)
//   Resolve the phase-ordering issue of mvsa_model later than propagation
//   Trying to extract parm_idx from rbc call and set flag on rbc callee
// =============================================================================
void
RBC_BASE::Init__set_parm_deref(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  IDTYPE obj_parm_idx = Extract_get_arg_idx(enclosing_dna, model_call, 0);
  if(obj_parm_idx > INVALID_VAR_IDX &&
     obj_parm_idx < enclosing_dna->Parm_list()->size()) {
    enclosing_dna->Set_parm_flag(obj_parm_idx,  REF_ILOAD);
    Is_Trace(Tracing(),
      (TFile, "RBC_BASE::Init__set_parm_deref: set REF_ILOAD for %s parm(%d)\n",
       enclosing_dna->Fname(), obj_parm_idx));
  }
}

// =============================================================================
//
// RBC_BASE::Init__set_func_thread
// BOOL Set_func_thread(BOOL is_multi_thread)
// =============================================================================
void
RBC_BASE::Init__set_func_thread(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(enclosing_dna));
  Is_True_Ret(arg1->Kind() == CK_CONST, ("RBC_BASE:Init__set_func_thread arg1 not const\n"));
  BOOL is_multi_thread = (BOOL)arg1->Const_val();
  ipsa->Mark_thread_entry(enclosing_dna->Dna_idx(), is_multi_thread);
  // No need to eval __mvsa_model
  model->Clear_flag(RBC_SE_MODEL);
}

// =============================================================================
//
// RBC_BASE::Extract_get_arg_idx
// Extract the index value used in rbc.Get_arg(index)
// =============================================================================
IDTYPE
RBC_BASE::Extract_get_arg_idx(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 model_idx)
{
  IDTYPE parm_idx = INVALID_VAR_IDX;
  STMTREP *get_arg_call = Get_rbc_nth_call(model_call, model_idx + Rbc_parm_ofst_adjust(enclosing_dna));
  if (get_arg_call == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Extract_get_arg_idx: not call at idx(%d)\n", model_idx));
    return INVALID_VAR_IDX;
  }
  VSA *vsa = enclosing_dna->Comp_unit()->Vsa();
  Is_True_Ret(vsa, ("RBC_BASE::Extract_get_arg_idx: vsa not created"), INVALID_VAR_IDX);
  RNA_NODE *rna = vsa->Sr_2_rna(get_arg_call);
  if (rna == NULL) {
    return parm_idx;
  } else if (rna->Rbc_op() == RBC_OP_GET_ARG) {
    CODEREP *parm_cr = get_arg_call->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(enclosing_dna));
    if (parm_cr->Kind() == CK_CONST) {
      parm_idx = parm_cr->Const_val();
      // if the function has this pointer, inc parm_idx
      ST *this_st = Get_this_symbol(enclosing_dna);
      if (this_st != NULL) {
        parm_idx ++;
      }
    }
  } else if (rna->Rbc_op() == RBC_OP_GET_THIS_POINTER) {
    ST *func_parm_st = Get_this_symbol(enclosing_dna);
    if (func_parm_st) {
      parm_idx = 1;
    } else {
      Is_Trace(Tracing(), (TFile, "Extract_get_arg_idx: unable to find this pointer"));
    }
  }
  return parm_idx;
}

// =============================================================================
//
// RBC_BASE::Set_obj_parm_flag
// Set object parm flag
// =============================================================================
void
RBC_BASE::Set_obj_parm_flag(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 model_idx, UINT32 parm_flag)
{
  IDTYPE obj_idx = Extract_get_arg_idx(enclosing_dna, model_call, model_idx);
  ST_IDX parm_st = enclosing_dna->Get_param_stidx(obj_idx);
  if (parm_st != ST_IDX_ZERO && TY_kind(ST_type(parm_st)) == KIND_POINTER) {
    enclosing_dna->Set_parm_flag(obj_idx, parm_flag);
  } else if (obj_idx >= enclosing_dna->Parm_list()->size() &&
             TY_is_varargs(PU_prototype(*enclosing_dna->Pu()))) {
    ST* st = New_ST(CURRENT_SYMTAB);
    ST_Init(st, Save_Str("_temp_vararg_rbc"), CLASS_VAR,
            SCLASS_FORMAL, EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
    VAR_NODE *vnd = CXX_NEW(VAR_NODE(ST_st_idx(st), NULL),
                            enclosing_dna->Mem_pool());
    VNODE_VECTOR *parm_list = enclosing_dna->Parm_list();
    for (int parm_idx = parm_list->size(); parm_idx <= obj_idx; parm_idx++) {
      parm_list->push_back(vnd);
    }
    enclosing_dna->Set_parm_flag(obj_idx, parm_flag);
  }
}

TAG_BASE *
RBC_BASE::Create_tag_base(DNA_NODE *enclosing_dna, STMTREP *model_call, UINT32 tag_idx)
{
  CODEREP *tag = model_call->Rhs()->Find_nth_arg(tag_idx + Rbc_parm_ofst_adjust(enclosing_dna));
  char *tag_name = Find_const_char(enclosing_dna, tag);
  Is_True_Ret(tag_name != NULL, ("RBC ERROR: Create_tag_base: null tag\n"), NULL);
  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if (tag_base == NULL)
    tag_base = New_tag_base(Clone_string((STRING) tag_name, Mem_pool()));
  return tag_base;
}

FSM_BASE*
RBC_BASE::New_fsm_base(STRING name)
{
  FSM_BASE *fsm_base;
  fsm_base = CXX_NEW(FSM_BASE(New_fsm_base_id(), name), Mem_pool());
  fsm_base->Set_kind(RSC_KIND_FSM);
  FSM *fsm = CXX_NEW(FSM(Mem_pool()), Mem_pool());
  fsm_base->Set_fsm(fsm);
  _fsm_base_list->Append(fsm_base);
  return fsm_base;
}

TAG_BASE*
RBC_BASE::New_tag_base(STRING name)
{
  TAG_BASE *tag_base;
  tag_base = CXX_NEW(TAG_BASE(New_tag_base_id(), name), Mem_pool());
  _tag_base_list->Append(tag_base);
  Is_Trace(Tracing(), (TFile, "RBC New_tag_base: add new tag : %s(%d).\n", name, tag_base->Id()));
  return tag_base;
}


// =============================================================================
//
// RBC_BASE::Eval__bool_exp
//
// =============================================================================
BOOL
RBC_BASE::Eval__bool_exp(RBC_CONTEXT &rbc_ctx, CODEREP *boolexp)
{
  RBC_EVAL_SKIP();
  Is_True_Rbc(boolexp != NULL && boolexp->Kind() == CK_OP,
              ("RBC ERROR: invalid boolexp.\n"));
  BOOL path_from_left = FALSE;
  BOOL ret = TRUE;
  UINT64 lhs_ui64val = Eval__exp(rbc_ctx, boolexp->Opnd(0));
  if (Plist_true()->size() > 0 || Plist_false()->size() > 0)
    path_from_left = TRUE;
  UINT64 rhs_ui64val = Eval__exp(rbc_ctx, boolexp->Opnd(1));

  OPCODE  cmp_op = boolexp->Op();
  switch(OPCODE_operator(cmp_op)) {
  case OPR_LT:
    ret = (lhs_ui64val < rhs_ui64val);
    break;
  case OPR_LE:
    ret = (lhs_ui64val <= rhs_ui64val);
    break;
  case OPR_GT:
    ret = (lhs_ui64val > rhs_ui64val);
    break;
  case OPR_GE:
    ret = (lhs_ui64val >= rhs_ui64val);
    break;
  case OPR_EQ:
    ret = (lhs_ui64val == rhs_ui64val);
    break;
  case OPR_NE:
    ret = (lhs_ui64val != rhs_ui64val);
    break;
  default:
    Is_True_Rbc(false, ("RBC ERROR: OPCODE:%d not implement yet in bool exp.\n", cmp_op));
    break;
  }
  // switch path lists if needed
  if (path_from_left) {
    if (ret != (lhs_ui64val != 0))
      Switch_plist();
  }
  else if (ret != (rhs_ui64val != 0))
    Switch_plist();

  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__bool_exp %s(%lld, %lld) = %d\n",
                       OPCODE_name(cmp_op), lhs_ui64val, rhs_ui64val, ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__exp
//
// =============================================================================
UINT64
RBC_BASE::Eval__exp(RBC_CONTEXT &rbc_ctx, CODEREP *x)
{
  RBC_EVAL_SKIP();
  Is_True_Rbc(x != NULL, ("RBC ERROR: null x passed to exp.\n"));
  UINT64 ret = 0;
  switch (x->Kind()) {
  case CK_CONST:
    ret = x->Const_val();
    return ret;

  case CK_VAR: {
    // comment below code as API Rbc_apply_rule will change context
    // unable to get the Aux id from current callee context
    // below code are used to get java constant char, in java
    // the const data is not a CK_CONST, but a normal var coderep
    // if continue search defstmt, the const can't be find
    if (Is_const(rbc_ctx.Rbc_node(), x)) {
      ret = (UINT64)Find_const_char(rbc_ctx.Rbc_node(), x);
      return ret;
    }

    STMTREP *defstmt = x->Defstmt();
    Is_True_Rbc(defstmt != NULL, ("RBC ERROR: null defstmt in exp.\n"));
    if (defstmt->Opr() == OPR_CALL)
      ret = Eval__builtin_function(rbc_ctx, defstmt);
    else if (defstmt->Opr() == OPR_STID)
      ret = Eval__exp(rbc_ctx, defstmt->Rhs());
    else if (defstmt->Opr() == OPR_INTRINSIC_CALL &&
             defstmt->Rhs()->Intrinsic() == INTRN_CHECK_CAST)
      ret = Eval__exp(rbc_ctx, defstmt->Rhs()->Opnd(1)->Find_actual_arg());
    else {
      Is_True_Rbc(false, ("RBC ERROR: OPR:%d not implement yet in exp.\n", defstmt->Opr()));
    }
    return ret;
  }
  case CK_OP: {
    if (x->Opr() == OPR_CVTL || x->Opr() == OPR_CVT) {
      ret = Eval__exp(rbc_ctx, x->Opnd(0));
    }
    else if (OPERATOR_is_compare(x->Opr())) {
      ret = Eval__bool_exp(rbc_ctx, x);
    }
    else if (x->Opr() == OPR_BAND || x->Opr() == OPR_BIOR) {
      UINT64 left, right;
      left = Eval__exp(rbc_ctx, x->Opnd(0));
      right = Eval__exp(rbc_ctx, x->Opnd(1));
      if (x->Opr() == OPR_BAND)
        ret = left & right;
      else
        ret = left | right;
    }
    else if (x->Opr() == OPR_BNOT) {
      UINT64 var = Eval__exp(rbc_ctx, x->Opnd(0));
      ret = ~var;
    }
    else if (x->Opr() == OPR_ADD) {
      UINT64 left, right;
      left = Eval__exp(rbc_ctx, x->Opnd(0));
      right = Eval__exp(rbc_ctx, x->Opnd(1));
      ret = left + right;
    }
    else {
      Is_True_Rbc(false, ("RBC ERROR: OPR:%d not implement yet in exp.\n", x->Opr()));
    }
    return ret;
  }
  case CK_LDA: {
    ST *lda_st = x->Lda_base_st();
    if (ST_class(lda_st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(lda_st)) == MTYPE_STR)
      ret = (UINT64)Index_to_char_array(TCON_str_idx(ST_tcon_val(lda_st)));
    else {
      Is_True_Rbc(false,
                  ("RBC ERROR: LDA cr%d is not a const string in exp.\n", x->Coderep_id()));
    }
    return ret;
  }
  default:
    Is_True_Rbc(false, ("RBC ERROR: CK_KIND:%d not implement yet in exp.\n", x->Kind()));
    break;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__builtin_function
//
// =============================================================================
UINT64
RBC_BASE::Eval__builtin_function(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  Is_True_Rbc((stmt != NULL && stmt->Opr() == OPR_CALL && OPERATOR_has_sym(stmt->Opr())),
              ("RBC ERROR: expect a function call in the operand 0.\n") );

  RNA_NODE *rna = rbc_ctx.Rbc_node()->Get_callsite_rna(stmt);
  BUILTIN_FUNC func = Builtin_func(rna->Rbc_op());
  if (func == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: builtin function:\"%s\" not found.\n", Rbc_op_name(rna->Rbc_op())));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return 0;
  }
  Is_Trace(Tracing(), (TFile, "RBC: calling %s\n", Rbc_op_name(rna->Rbc_op())));
  UINT64 ret = (this->*func)(rbc_ctx, stmt);
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__not
//
// =============================================================================
UINT64
RBC_BASE::Eval__not(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Not(BOOL v)
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  UINT64 v = Eval__exp(rbc_ctx, arg0);
  ret = (v == 0) ? 1 : 0;

  // swith true/false plist as result true/false is exchanged
  Switch_plist();
  Is_Trace(Tracing(), (TFile, "RBC::Not(%lld) = %lld\n", v, ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__or
// BOOL Or(BOOL v1, BOOL v2)
// =============================================================================
UINT64
RBC_BASE::Eval__or(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  BOOL v1 = (BOOL) Eval__exp(rbc_ctx, arg0);
  if((!Rbc_result_ignore()) && (v1 == TRUE)) {
    ret = TRUE;
    Is_Trace(Tracing(), (TFile, "RBC::Or(%d, v2) = %lld\n", v1, ret));
  } else {
    // if v1 evaluate failed, continue evaluate v2
    BOOL v1_skipped = FALSE;
    while(Rbc_eval_skip()) {
      Rbc_eval_certainty()->pop_back();
      v1_skipped = TRUE;
    }
    CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
    BOOL v2 = (BOOL) Eval__exp(rbc_ctx, arg1);
    BOOL v2_skipped = FALSE;
    ret = v2;
    if (!v1_skipped) {
      while(Rbc_eval_skip()) {
        Rbc_eval_certainty()->pop_back();
        v2_skipped = TRUE;
      }
      if (v2_skipped) {
        ret = v1;
      }
    }
    Is_Trace(Tracing(), (TFile, "RBC::Or(%d%s, %d%s) = %lld\n",
                         v1, v1_skipped ? "[ignore]" : "",
                         v2, v2_skipped ? "[ignore]" : "", ret));
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__and
// BOOL And(BOOL v1, BOOL v2)
// =============================================================================
UINT64
RBC_BASE::Eval__and(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  BOOL v1 = (BOOL) Eval__exp(rbc_ctx, arg0);
  if(v1 == FALSE) {
    ret = FALSE;
    Is_Trace(Tracing(), (TFile, "RBC::And(%d, v2) = %lld\n", v1, ret));
  } else {
    CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
    BOOL v2 =(BOOL) Eval__exp(rbc_ctx, arg1);
    ret = v2;
    Is_Trace(Tracing(), (TFile, "RBC::And(%d, %d) = %lld\n", v1, v2, ret));
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__if
// BOOL If(BOOL cond, BOOL true_exp, BOOL false_exp)
// =============================================================================
UINT64
RBC_BASE::Eval__if(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  UINT64 cond = Eval__exp(rbc_ctx, arg0);

  if(cond) {
    CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
    ret = Eval__exp(rbc_ctx, arg1);
  } else {
    CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
    ret = Eval__exp(rbc_ctx, arg2);
  }

  Is_Trace(Tracing(), (TFile, "RBC: If(%lld) { %s executed with ret %lld }\n",
                       cond, cond ? "true_exp" : "false_exp", ret ));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__pre_sanitized & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__pre_sanitized(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Pre_sanitized(void *v)
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa_ctx = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null v passed to Pre_sanitized.\n"));
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(arg, call_stmt, dna, spos_pool, vsa_ctx), spos_pool);
  ret = Pre_sanitized(vsa_ctx, call_stmt, arg, srcpos_h);
  if (ret == 0)
    Plist_false()->push_back(srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Pre_sanitized(v:cr%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Pre_sanitized(VSA *vsa_ctx, STMTREP *stmt, CODEREP *v, SRCPOS_HANDLE *srcpos_h)
{
  Is_True_Rbc(v != NULL, ("RBC ERROR: invalid v passed to Pre_sanitized.\n"));
  BOOL ret = TRUE;
  CONTEXT_SWITCH caller_ctx(vsa_ctx->Dna());
  Is_Trace(Tracing(), (TFile, "%sRBC: Pre_sanitized Evaluation Begin\n%s", SBar, SBar));

  VAR_DEF_HELPER helper(v, stmt, vsa_ctx->Comp_unit());
  CHECK_OBJ check_obj(v, stmt);
  vsa_ctx->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  if (def_info_vec.size() == 0) {
    Is_Trace(Tracing(),
      (TFile, "RBC_BASE::Pre_sanitized can't find coderep define, sr %d, cr%d\n", stmt->Stmtrep_id(), v->Coderep_id()));
  }
  DNA_NODE *def_dna = NULL;
  CODEREP *def_cr = NULL;
  STMTREP *def_stmt = NULL;
  for(int i = 0; i < def_info_vec.size() && ret; i++) {
    def_dna = def_info_vec[i]->Dna();
    def_cr = def_info_vec[i]->Coderep();
    Is_True_Rbc(def_dna && def_cr, ("RBC ERROR: Pre santize invalid def_dna, def_cr"));
    CONTEXT_SWITCH def_ctx(def_dna);
    if(Is_const(def_dna, def_cr)) {
      Is_Trace(Tracing(), (TFile, "RBC Pre_sanitized: def is a const mark sanitized:Y\n"));
      ret = TRUE;
    } else if (def_cr->Kind() == CK_IVAR) {
      Is_Trace(Tracing(), (TFile, "RBC Pre_sanitized: def is ivar mark sanitized:N\n"));
      ret = FALSE;
    } else if(def_cr->Kind() == CK_VAR) {
      def_stmt = def_cr->Defstmt();
      if(def_dna->Is_param(def_cr) != INVALID_VAR_IDX) {
        Is_Trace(Tracing(), (TFile, "RBC Pre_sanitized: def is an input parameter mark sanitized:N\n"));
        Rbc_eval_certainty()->push_back(REC_MAYBE);
        ret = FALSE;
      } else if(def_stmt) {
        switch(def_stmt->Opr()) {
        case OPR_OPT_CHI:
          Is_Trace(Tracing(), (TFile, "RBC Pre_sanitized: def is a entry chi mark sanitized:N\n"));
          Rbc_eval_certainty()->push_back(REC_MAYBE);
          ret = FALSE;
          break;
        case OPR_INTRINSIC_CALL:
          if(def_stmt->Rhs()->Intrinsic() == INTRN_ALLOC_OBJ ||
             (def_stmt->Call_flags() & WN_CALL_IS_CONSTRUCTOR)) {
            Is_Trace(Tracing(),
                     (TFile, "RBC Pre_sanitized: def stmt in %s is alloc_obj mark sanitized:Y\n",
                      def_dna->Fname()));
            ret = TRUE;
          } else {
            Is_Trace(Tracing(),
                     (TFile, "RBC Pre_sanitized: def stmt in %s is not alloc_obj mark sanitized:N\n",
                      def_dna->Fname()));
            ret = FALSE;
          }
          break;
        case OPR_CALL:
        case OPR_ICALL:
        {
          // if def by an unresolved call, check current dna
          // sanitized or if the call marked sanitize
          RNA_NODE *rna = def_dna->Get_callsite_rna(def_stmt);
          if(rna->Uniq_callee() != INVALID_RNA_PU_IDX &&
             vsa_ctx->Ipsa()->Get_dna(rna->Uniq_callee())->Is_set(DNA_SANITIZE_DATA)) {
            Is_Trace(Tracing(),
                     (TFile, "RBC Pre_sanitized: def stmt callee %s is sanitize fun mark sanitized:Y\n",
                      vsa_ctx->Ipsa()->Get_dna(rna->Uniq_callee())->Fname()));
            ret = TRUE;
          }
          else if(def_dna->Is_set(DNA_SANITIZE_DATA)) {
            Is_Trace(Tracing(),
                     (TFile, "RBC Pre_sanitized: def dna %s is sanitize fun mark sanitized:Y\n",
                      def_dna->Fname()));
            ret = TRUE;
          } else {
            Is_Trace(Tracing(),
                     (TFile, "RBC Pre_sanitized: def dna %s is not sanitize fun mark sanitized:N\n",
                      def_dna->Fname()));
            ret = FALSE;
          }
          break;
        }
        default:
          Is_Trace(Tracing(),
                   (TFile, "RBC Pre_sanitized: def by %s not supported opr mark sanitized:Y\n",
                    def_dna->Fname()));
          ret = TRUE;
          break;
        }
      } else {
        Is_Trace(Tracing(), (TFile, "RBC Pre_sanitized: def is NULL, mark sanitized:Y\n"));
        ret = TRUE;
      }
    } else {
      Is_Trace(Tracing(), (TFile, "RBC ERROR: Pre_sanitized not supported def_cr, def dna: %s, cr: cr%d\n", def_dna->Fname(), def_cr->Coderep_id()));
      ret = TRUE;
    }
  }
  if(!ret && def_dna && def_stmt)
    srcpos_h->Append_data(def_stmt, def_dna, PATHINFO_RBC);

  Is_Trace(Tracing(), (TFile, "%sRBC: Pre_sanitized Evaluation End\n%s", SBar, SBar));
  return ret;
}


BOOL
RBC_BASE::Is_in_if_stmt(VSA *vsa, CODEREP *v, CODEREP *value, OPERATOR opr, STMTREP *stmt, BOOL vonly)
{
  BOOL ret = FALSE;
  Is_True_Rbc(v != NULL, ("RBC ERROR: null v in Is_in_if_stmt.\n"));
  if (stmt->Opr() == OPR_FALSEBR || stmt->Opr() == OPR_TRUEBR ) {
    CODEREP *cmp = stmt->Rhs();
    // TODO: it is quite possible that cmp is LDID, workaround for now
    if (cmp->Kind() != CK_OP) {
      Is_Trace(Tracing(), (TFile, "RBC: Failed to find compare op in if stmt, skipped.\n"));
      Rbc_eval_certainty()->push_back(REC_SKIP);
      return TRUE;
    }
    if (OPERATOR_is_compare(cmp->Opr())) {
      CODEREP *rhs = cmp->Opnd(1);
      CODEREP *lhs = cmp->Opnd(0);
      Is_Trace(Tracing(), (TFile,
                           "---------- v:cr%d opr:%d value:cr%d lhs:cr%d rhs:cr%d\n",
                           v->Coderep_id(), opr,
                           value == NULL || (INT32)(INTPTR)value == -1 ? -1 : value->Coderep_id(),
                           lhs->Coderep_id(), rhs->Coderep_id()));
      // set lhs, rhs according to compare opr
      if (vonly) {
        // check only if v appear in if statement
        if (Check_cr_eq(vsa, lhs, v) || Check_cr_eq(vsa, rhs, v))
          ret = TRUE;
      }
      else {
        // when opr == OPERATOR_UNKNOWN, we don't care about compare operator
        if (opr == OPERATOR_UNKNOWN) {
          if ((Check_cr_eq(vsa, lhs, v) && Check_cr_eq(vsa, rhs, value)) ||
              (Check_cr_eq(vsa, rhs, v) && Check_cr_eq(vsa, lhs, value)))
            ret = TRUE;
        }
        else if (cmp->Opr() == opr) {
          if (Check_cr_eq(vsa, lhs, v) && Check_cr_eq(vsa, rhs, value))
            ret = TRUE;
          else if (opr == OPR_EQ || opr == OPR_NE)
            if (Check_cr_eq(vsa, rhs, v) && Check_cr_eq(vsa, lhs, value))
              ret = TRUE;
        }
        else if ((opr == OPR_NE && cmp->Opr() == OPR_EQ) ||
                 (opr == OPR_EQ && cmp->Opr() == OPR_NE)) {
          if ((Check_cr_eq(vsa, lhs, v) && Check_cr_eq(vsa, rhs, value)) ||
              (Check_cr_eq(vsa, rhs, v) && Check_cr_eq(vsa, lhs, value)))
            ret = TRUE;
        }
        else if ((opr == OPR_GE && cmp->Opr() == OPR_LT) ||
                 (opr == OPR_GT && cmp->Opr() == OPR_LE) ||
                 (opr == OPR_LE && cmp->Opr() == OPR_GT) ||
                 (opr == OPR_LT && cmp->Opr() == OPR_GE))
          if (Check_cr_eq(vsa, rhs, v) && Check_cr_eq(vsa, lhs, value))
            ret = TRUE;
      }
    }
  }
  return ret;
}


BOOL
RBC_BASE::Check_cr_eq(VSA *vsa, CODEREP *stmt_v, CODEREP *comp_v)
{
  BOOL ret = FALSE;
  Is_True_Rbc(stmt_v != NULL, ("RBC ERROR: null stmt_v passed to Check_cr_eq.\n"));

  if (comp_v == NULL) {
    if ((stmt_v->Kind() == CK_CONST && stmt_v->Const_val() == 0) ||
        (stmt_v->Kind() == CK_RCONST && stmt_v->Const_fval() == 0))
      ret = TRUE;
  } // end comp_v NULL
  else if ((INT32)(INTPTR)comp_v == -1) {
    if (stmt_v->Kind() == CK_CONST && stmt_v->Const_val() == (INT64)-1)
      ret = TRUE;
  } // end comp_v -1
  else if (comp_v == stmt_v) {
    ret = TRUE;
  } // end same CODEREP
  else if (comp_v->Kind() == CK_CONST) {
    if (stmt_v->Kind() == CK_CONST && stmt_v->Const_val() == comp_v->Const_val())
      ret = TRUE;
  } // end CK_CONST
  else if (comp_v->Kind() == CK_IVAR) {
    if (stmt_v->Kind() == CK_IVAR) {
      if (stmt_v->I_field_id() == comp_v->I_field_id()) {
        CODEREP *stmt_base = stmt_v->Ilod_base() ? stmt_v->Ilod_base() : stmt_v->Istr_base();
        CODEREP *comp_base = comp_v->Ilod_base() ? comp_v->Ilod_base() : comp_v->Istr_base();
        ret = Check_cr_eq(vsa, stmt_base, comp_base);
      }
    }
    else if (stmt_v->Kind() == CK_VAR) {
      STMTREP *comp_defstmt = comp_v->Get_defstmt();
      if (comp_defstmt != NULL) {
        ret = Check_cr_eq(vsa, stmt_v, comp_defstmt->Rhs());
      }
    }
  } // end CK_IVAR
  else if (comp_v->Kind() == CK_VAR) {
    IDTYPE comp_v_id = comp_v->Aux_id();
    if (stmt_v->Kind() == CK_VAR) {
      IDTYPE stmt_v_id = stmt_v->Aux_id();
      CODEREP *rhs = stmt_v;
      if (stmt_v_id != comp_v_id) {
        STMTREP *defstmt = rhs->Defstmt();
        while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
          if (defstmt->Rhs() == NULL)
            break;
          rhs = defstmt->Rhs();
          if (rhs->Kind() == CK_VAR && defstmt != rhs->Defstmt()) {
            AUX_STAB_ENTRY *sym = vsa->Opt_stab()->Aux_stab_entry(rhs->Aux_id());
            ST *st = sym->St();
            if (st != NULL && ST_sclass(st) != SCLASS_REG) {
              stmt_v_id = rhs->Aux_id();
              defstmt = rhs->Defstmt();
            }
            else
              break;
          }
          else
            break;
        }
      }
      rhs = comp_v;
      if (stmt_v_id != comp_v_id) {
        STMTREP *defstmt = rhs->Defstmt();
        while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
          if (defstmt->Rhs() == NULL)
            break;
          rhs = defstmt->Rhs();
          if (rhs->Kind() == CK_VAR && defstmt != rhs->Defstmt()) {
            AUX_STAB_ENTRY *sym = vsa->Opt_stab()->Aux_stab_entry(rhs->Aux_id());
            ST *st = sym->St();
            if (st != NULL && ST_sclass(st) != SCLASS_REG) {
              comp_v_id = rhs->Aux_id();
              defstmt = rhs->Defstmt();
            }
            else
              break;
          }
          else
            break;
        }
      }
      if (stmt_v_id == comp_v_id)
        ret = TRUE;
    }
    else if (stmt_v->Kind() == CK_OP) {
      if (stmt_v->Opr() == OPR_CVTL || stmt_v->Opr() == OPR_CVT) {
        ret = Check_cr_eq(vsa, stmt_v->Opnd(0), comp_v);
      }
    }
    else if (stmt_v->Kind() == CK_LDA) {
      IDTYPE stmt_v_id = stmt_v->Lda_aux_id();
      if (stmt_v_id == comp_v_id)
        ret = TRUE;
    }
    else if (stmt_v->Kind() == CK_IVAR) {
      STMTREP *defstmt = stmt_v->Get_defstmt();
      if (defstmt != NULL) {
        ret = Check_cr_eq(vsa, defstmt->Rhs(), comp_v);
      }
    }
  } // end CK_VAR
  else if (comp_v->Kind() == CK_OP) {
    if (comp_v->Opr() == OPR_CVT || comp_v->Opr() == OPR_CVTL) {
      ret = Check_cr_eq(vsa, stmt_v, comp_v->Opnd(0));
    }
  } // end CK_OP

  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__pre_call & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__pre_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Pre_call(char *fname);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  UINT64 ret = Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(ret != 0, ("RBC ERROR: null fname passed to Pre_call.\n"));
  char *fname = (char*)ret;
  DNA_NODE *callee = rbc_ctx.Callee();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *latter = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  vector<IDTYPE> path;
  path.clear();
  hash_set<IDTYPE> calls_visited;
  calls_visited.clear();
  calls_visited.insert(dna->Dna_idx());
  CONTEXT_SWITCH context(dna);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, latter, dna, spos_pool), spos_pool);
  srcpos_h->Set_orig_stname(callee->Fname());
  ret = Is_called_previously(vsa, fname, latter, latter->Bb(), &path, calls_visited, srcpos_h);
  Is_True_Rbc(path.size() == 0, ("RBC ERROR: non-empty path in Pre_call.\n"));
  Is_Trace(Tracing(), (TFile, "RBC: Pre_call(\"%s\"): %lld\n", fname, ret));
  return ret;
}


BOOL
RBC_BASE::Is_called_previously(VSA *vsa_ctx, char *fname, STMTREP *latter, BB_NODE *bb,
                               vector<IDTYPE> *path, hash_set<IDTYPE> &calls_visited, SRCPOS_HANDLE *srcpos_h)
{
  RBC_EVAL_SKIP();
  BOOL ret = FALSE;
  if(srcpos_h != NULL && srcpos_h->Cur_node() &&
     srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Is_called_previously(%s) due to spos node cnt(%d) for func(%s)\n",
                         fname, srcpos_h->Children_count(), vsa_ctx->Dna()->Fname()));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return TRUE;
  }
  for (vector<IDTYPE>::iterator iter = path->begin(); iter != path->end(); iter++) {
    // if visited, it should be a loop, let other path to determine
    // for example, let 'BB0' decides
    // BB0 -> BB1 -> ... call fname -> BBn
    //         ^                        |
    //         |                        |
    //         --------------------------
    if (*iter == bb->Id())
      return TRUE;
  }
  path->push_back(bb->Id());
  srcpos_h->Append_data(bb, vsa_ctx->Dna(), PATHINFO_BRANCH);

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ visit BB%d\n", bb->Id()));
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    OPERATOR opr = stmt->Opr();
    if (opr == OPR_CALL) {
      ST *st = stmt->St();
      if (strcmp(fname, ST_name(st)) == 0) {
        ret = TRUE;
        path->pop_back();
        srcpos_h->Remove_last();
        return ret;
      }
    }

    if (stmt == latter)
      break;
  }

  DNA_NODE *dna = vsa_ctx->Dna();
  RNODE_VECTOR *clby_list = dna->Clby_list();
  if (bb->Pred() == NULL) {
    // to the entry of current function
    if (clby_list->size() == 1) {
      SRCPOS_HANDLE *sh = srcpos_h->Clone();
      Plist_false()->push_back(sh);
    }
    else {
      // go through clby_list
      INT idx = 0;
      INT parent_idx = srcpos_h->Cur_idx();
      srcpos_h->Add_children(clby_list->size());
      SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
      for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
        RNA_NODE *rna = (*clby_list)[i];
        if (rna == NULL)
          continue;
        DNA_NODE *caller = vsa_ctx->Ipsa()->Get_dna(rna->Caller_idx());
        if (caller == NULL)
          continue;
        if (caller->Non_functional())
          continue;
        if (calls_visited.find(caller->Dna_idx()) != calls_visited.end())
          continue;
        calls_visited.insert(caller->Dna_idx());
        srcpos_h->Set_cur_node(cur_node, idx);
        srcpos_h->Append_data(rna->Callstmt(), caller, PATHINFO_DNA_CALLSITE);
        idx++;
        vector<IDTYPE> caller_path;
        caller_path.clear();
        CONTEXT_SWITCH context(caller);
        ret = Is_called_previously(caller->Comp_unit()->Vsa(), fname, rna->Callstmt(),
                                   rna->Callstmt()->Bb(), &caller_path, calls_visited, srcpos_h);
        if (ret == FALSE)
          break;
      }
      srcpos_h->Reset_cur_node(cur_node, parent_idx);
    }
  }

  BB_NODE *pred_bb;
  BB_LIST_ITER pred_bb_iter;
  FOR_ALL_ELEM(pred_bb, pred_bb_iter, Init(bb->Pred())) {
    ret = Is_called_previously(vsa_ctx, fname, pred_bb->Last_stmtrep(),
                               pred_bb, path, calls_visited, srcpos_h);
    if (ret == FALSE)
      break;
  }
  path->pop_back();
  srcpos_h->Remove_last();
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_errno_cleared_before
//     if errno is set to zero in the same function
//     right before a call to errno-setting-function
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_errno_cleared_before(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_errno_cleared_before(void)
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CONTEXT_SWITCH context(dna);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
  srcpos_h->Set_orig_stname(callee->Fname());
  hash_set<IDTYPE> visited_bb;
  UINT64 ret = Is_errno_cleared_before(call_stmt->Prev(), call_stmt->Bb(), NULL,
                                       dna, visited_bb, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_errno_cleared_before(void): %lld\n", ret));
  return ret;
}


BOOL
RBC_BASE::Is_var_errno(CODEREP *var, VSA *vsa)
{
  BOOL ret = FALSE;
  if (var == NULL)
    return ret;

  CODEREP *cr = var;
  if (cr->Kind() == CK_IVAR)
    cr = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
  if (cr->Kind() == CK_VAR) {
    STMTREP *def_stmt = cr->Defstmt();
    if (def_stmt != NULL && def_stmt->Opr() == OPR_STID) {
      cr = def_stmt->Rhs();
      if (cr != NULL && cr->Kind() == CK_VAR &&
          vsa->Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_return_preg()) {
        def_stmt = cr->Defstmt();
        if (def_stmt->Opr() == OPR_CALL) {
          ST *st = def_stmt->St();
          if (st != NULL && strncmp(ST_name(st), "__errno_location", 16) == 0)
            ret = TRUE;
        }
      }
    }
  }
  return ret;
}


BOOL
RBC_BASE::Is_errno_cleared_before(STMTREP *stmt, BB_NODE *bb, BB_NODE *succ, DNA_NODE *dna,
                                  hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (bb == NULL || dna == NULL || srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return ret;
  visited_bb.insert(bb->Id());

  VSA *vsa = dna->Comp_unit()->Vsa();
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  BOOL start = FALSE;

  FOR_ALL_NODE_REVERSE(cur, stmt_iter, Init()) {
    if (!start)
      if (cur == stmt)
        start = TRUE;
      else
        continue;

    OPERATOR opr = cur->Opr();
    if (opr == OPR_ISTORE) {
      // check stmt: errno = 0;
      CODEREP *lhs = cur->Lhs();
      CODEREP *rhs = cur->Rhs();
      if (Is_var_errno(lhs, vsa)) {
        if (rhs->Kind() == CK_CONST && rhs->Const_val() == 0)
          ret = TRUE;
        return ret;
      }
    } // end if OPR_ISTORE
    else if (OPERATOR_is_call(opr)) {
      // if it is another errno-setting-function
      RNA_NODE *rna = dna->Get_callsite_rna(cur);
      BOOL found = FALSE;
      if (rna != NULL) {
        for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
             iter != rna->Callee_list().end(); iter++) {
          DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
          if (callee == NULL)
            continue;
          if (callee->Is_set(DNA_ERRNO_SETTING)) {
            found = TRUE;
            break;
          }
          else {
            DNODE_VECTOR *rbc_nodes = Get_rbc_nodes(callee);
            if (rbc_nodes != NULL) {
              for (DNODE_VECTOR::iterator rbc_iter = rbc_nodes->begin();
                   rbc_iter != rbc_nodes->end(); rbc_iter++) {
                DNA_NODE *rbc_dna = *rbc_iter;
                if (rbc_dna == NULL)
                  continue;
                if (rbc_dna->Is_set(DNA_ERRNO_SETTING)) {
                  found = TRUE;
                  break;
                }
              }
            }
          }
        }
        if (found) {
          srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
          Plist_false()->push_back(srcpos_h->Clone()->Clone());
          srcpos_h->Remove_last();
          return ret;
        }
      }
    } // end if OPERATOR_is_call
    else if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
      CODEREP *cmp = cur->Rhs();
      if (cmp->Kind() == CK_OP && OPERATOR_is_compare(cmp->Opr())) {
        CODEREP *rhs = cmp->Opnd(1);
        CODEREP *lhs = cmp->Opnd(0);
        CODEREP *var = NULL;
        CODEREP *value = NULL;
        if (rhs->Kind() == CK_CONST) {
          value = rhs;
          var = lhs;
        }
        else if (lhs->Kind() == CK_CONST) {
          value = lhs;
          var = rhs;
        }
        if (Is_var_errno(var, vsa)) {
          if (value->Kind() == CK_CONST && value->Const_val() == 0) {
            if (cmp->Opr() == OPR_EQ) {
              // True branch set true
              if (opr == OPR_TRUEBR) {
                // OPR_TRUEBR
                if (succ != NULL && cur->Label_number() == succ->Labnam()) {
                  ret = TRUE;
                }
                else {
                  srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
                  Plist_false()->push_back(srcpos_h->Clone()->Clone());
                  srcpos_h->Remove_last();
                }
              }
              else {
                // OPR_FALSEBR
                if (succ != NULL && cur->Label_number() != succ->Labnam()) {
                  ret = TRUE;
                }
                else {
                  srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
                  Plist_false()->push_back(srcpos_h->Clone()->Clone());
                  srcpos_h->Remove_last();
                }
              }
            } // end OPR_EQ
            else if (cmp->Opr() == OPR_NE) {
              // False branch set true
              if (opr == OPR_FALSEBR) {
                // OPR_FALSEBR
                if (succ != NULL && cur->Label_number() == succ->Labnam()) {
                  ret = TRUE;
                }
                else {
                  srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
                  Plist_false()->push_back(srcpos_h->Clone()->Clone());
                  srcpos_h->Remove_last();
                }
              }
              else {
                // OPR_TRUEBR
                if (succ != NULL && cur->Label_number() != succ->Labnam()) {
                  ret = TRUE;
                }
                else {
                  srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
                  Plist_false()->push_back(srcpos_h->Clone()->Clone());
                  srcpos_h->Remove_last();
                }
              }
            } // end OPR_NE
          }
          return ret;
        } // end if Is_var_errno
      }
    } // end if OPR_TRUEBR || OPR_FALSEBR
  }

  BB_NODE *prev_bb;
  BB_LIST_ITER prev_bb_iter;
  if (bb->Pred() != NULL) {
    // assume all prev paths have been set
    ret = TRUE;
  }
  else {
    srcpos_h->Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
    Plist_false()->push_back(srcpos_h->Clone()->Clone());
    srcpos_h->Remove_last();
  }
  FOR_ALL_ELEM(prev_bb, prev_bb_iter, Init(bb->Pred())) {
    if (!Is_errno_cleared_before(prev_bb->Last_stmtrep(), prev_bb, bb, dna, visited_bb, srcpos_h))
      ret = FALSE;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Is_var_used_in_stmt
//    CODEREP::Contains can not tell when copy propagation selects temp var
//    but return var is used in function call, so Check_cr_eq is called
//
// =============================================================================
BOOL
RBC_BASE::Is_var_used_in_stmt(CODEREP *v, STMTREP *stmt, VSA *vsa)
{
  CODEREP *rhs = stmt->Rhs();
  if (rhs == NULL)
    return FALSE;
  if (rhs->Contains(v))
    return TRUE;
  if (rhs->Kind() == CK_OP) {
    for (INT i = 0; i < rhs->Kid_count(); i++)
      if (Check_cr_eq(vsa, v, rhs->Get_opnd(i)))
        return TRUE;
  }
  CODEREP *lhs = stmt->Lhs();
  if (lhs != NULL) {
    if (lhs->Kind() == CK_IVAR &&
        lhs->Istr_base() != NULL &&
        lhs->Istr_base()->Contains(v))
      return TRUE;
  }
  return FALSE;
}

// =============================================================================
//
// RBC_BASE::Eval__pre_check_var_value
//   check if variable v is checked before it is used
//   BOOL Pre_check_var_value(void *v, char *opr, void *value);
//
// =============================================================================
UINT64
RBC_BASE::Eval__pre_check_var_value(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Pre_check_var_value.\n"));
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *v = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  // process compare value
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *)Eval__exp(rbc_ctx, arg2);
  // process compare opr
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  ret = Eval__exp(rbc_ctx, arg1);
  OPERATOR cmp_op = Get_opr_from_char((const char*)ret);
  CONTEXT_SWITCH context(dna);
  BB_NODE *bb = call_stmt->Bb();
  hash_set<IDTYPE> visited;
  ret = Pre_check_var(vsa, bb, v, cmp_op, value, visited);
  Is_Trace(Tracing(), (TFile, "RBC: Pre_check_var_value(v:cr%d, opr:%d, value:cr%d): %lld\n",
                       v->Coderep_id(), cmp_op,
                       value == NULL || (INT32)(INTPTR)value == -1 ? -1 : value->Coderep_id(), ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Pre_check_var
//    go through the post dominance frontiers of bb, and check if 'v opr value' is
//    validated in the last statement of cd bbs.
//
// =============================================================================
BOOL
RBC_BASE::Pre_check_var(VSA *vsa, BB_NODE *bb, CODEREP *v, OPERATOR opr, CODEREP *value,
                        hash_set<IDTYPE>& visited)
{
  BB_NODE *cd;
  BB_NODE_SET_ITER cd_iter;
  FOR_ALL_ELEM(cd, cd_iter, Init(bb->Rcfg_dom_frontier())) {
    if (visited.find(cd->Id()) != visited.end())
      return FALSE;
    visited.insert(cd->Id());

    Is_True(cd->Succ() != NULL,
            ("cd bb does not have successors"));
    Is_True(cd->Succ()->Multiple_bbs(),
            ("succ of cc should be multiple bbs"));
    STMTREP *last_stmt = cd->Last_stmtrep();
    if (last_stmt != NULL) {
      if (Is_var_used_in_stmt(v, last_stmt, vsa)) {
        if (Is_in_if_stmt(vsa, v, value, opr, last_stmt, FALSE)) {
          return TRUE;
        }
      }
    }
    if (Pre_check_var(vsa, cd, v, opr, value, visited)) {
      return TRUE;
    }
  }
  return FALSE;
}

// =============================================================================
//
// RBC_BASE::Post_check_var
//    ensure 'v opr value' is validated in if statement afterwards
//    when 'v' is used in a statement, if the statement is not an 'if',
//    return FALSE, when 'v' is no longer in used, also return FALSE.
//
//    direction can be 0, 1 and -1. 0 means no fixed direction yet. 1 means
//    only search caller and -1 means only search callee
//
// =============================================================================
BOOL
RBC_BASE::Post_check_var(STMTREP *from, BB_NODE* from_bb, CODEREP *v, CODEREP *value, OPERATOR opr, INT direction,
                         BOOL vonly, vector<IDTYPE> *path, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h)
{
  BOOL start = FALSE;
  BOOL ret = FALSE;
  if (srcpos_h != NULL && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Post_check_var due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         srcpos_h->Children_count(), dna->Fname()));
    return TRUE;
  }
  for (vector<IDTYPE>::iterator iter = path->begin(); iter != path->end(); iter++) {
    if (*iter == from_bb->Id())
      return TRUE;
  }
  path->push_back(from_bb->Id());
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ visit BB%d\n", from_bb->Id()));
  VSA *vsa = dna->Comp_unit()->Vsa();
  STMTREP_ITER stmt_iter(from_bb->Stmtlist());
  STMTREP *stmt;

  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (!start)
      if (stmt == from)
        start = TRUE;
      else
        continue;
    // if v is used in stmt
    if (Is_var_used_in_stmt(v, stmt, vsa)) {
      // an 'if' stmt, OK
      ret = Is_in_if_stmt(vsa, v, value, opr, stmt, vonly);
      if (ret) {
        path->pop_back();
        return ret;
      }
      else {
        // not an 'if' stmt, if it's a call, go callee
        if (direction <= 0 && OPERATOR_is_call(stmt->Opr())) {
          RNA_NODE *rna = dna->Get_callsite_rna(stmt);
          if (rna != NULL) {
            INT idx = 0;
            INT parent_idx = srcpos_h->Cur_idx();
            srcpos_h->Add_children(rna->Callee_list().size());
            SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
            for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
                 iter != rna->Callee_list().end(); iter++) {
              DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
              if (callee == NULL)
                continue;
              if (callee->Non_functional())
                continue;
              CODEREP *new_arg = NULL;
              BOOL found_new_arg = Find_arg_in_callee(vsa, v, rna, callee, &new_arg);
              if (new_arg == NULL)
                continue;
              srcpos_h->Set_cur_node(cur_node, idx);
              srcpos_h->Append_data(stmt, dna, PATHINFO_DNA_CALLSITE);
              idx++;
              BB_NODE *callee_bb = callee->Comp_unit()->Cfg()->Entry_bb();
              STMTREP *callee_stmt = callee_bb->First_stmtrep();
              vector<IDTYPE> new_path;
              CONTEXT_SWITCH context(callee);
              Is_Trace(Tracing(), (TFile, "RBC: Post_check_var: callee(%s)\n", callee->Fname()));
              ret = Post_check_var(callee_stmt, callee_bb, new_arg, value, opr, vonly, -1, &new_path, callee, srcpos_h);
              if (!ret) {
                path->pop_back();
                return ret;
              }
            } // end for callee
            srcpos_h->Reset_cur_node(cur_node, parent_idx);
          }// end if rna != NULL
        }
        else if (direction >= 0 &&
                 stmt->Opr() == OPR_STID &&
                 vsa->Opt_stab()->Aux_stab_entry(stmt->Lhs()->Aux_id())->Is_return_preg() &&
                 stmt->Next() != NULL &&
                 stmt->Next()->Opr() == OPR_RETURN) {
          RNODE_VECTOR *clby_list = dna->Clby_list();
          Is_True(clby_list != NULL, ("clby list is null"));
          if (clby_list->size() == VAR_INIT_ID) {
            path->pop_back();
            // no ERR33 if target is system library
            return Is_target_system_library();
          }
          else {
            INT idx = 0;
            INT parent_idx = srcpos_h->Cur_idx();
            srcpos_h->Add_children(clby_list->size());
            SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
            for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
              RNA_NODE *rna = (*clby_list)[i];
              if (rna->Is_back_edge())
                continue;
              DNA_NODE *caller = vsa->Ipsa()->Get_dna(rna->Caller_idx());
              if (caller == NULL || caller->Non_functional())
                continue;
              STMTREP *callstmt = rna->Callstmt();
              Is_True(callstmt && OPERATOR_is_call(callstmt->Opr()),
                      ("invalid call stmt"));

              CONTEXT_SWITCH context(caller);
              srcpos_h->Set_cur_node(cur_node, idx);
              srcpos_h->Append_data(callstmt, caller, PATHINFO_DNA_CALLSITE);
              idx++;

              CODEREP *retv = caller->Comp_unit()->Find_return_value(callstmt);
              if (retv == NULL) {
                // not checked
                path->pop_back();
                Plist_false()->push_back(srcpos_h->Clone());
                return FALSE;
              }
              Is_True(retv->Kind() == CK_VAR &&
                      caller->Comp_unit()->Opt_stab()->
                        Aux_stab_entry(retv->Aux_id())->Is_return_preg(),
                      ("not return preg"));
              STMTREP *next = callstmt->Next();
              if (next == NULL || next->Opr() != OPR_STID) {
                // not assign to another preg/var, treat as unchecked
                path->pop_back();
                srcpos_h->Append_data(next, dna, PATHINFO_RBC);
                Plist_false()->push_back(srcpos_h->Clone());
                return FALSE;
              }
              vector<IDTYPE> new_path;
              Is_Trace(Tracing(),
                       (TFile, "RBC: Post_check_var: caller(%s)\n", caller->Fname()));
              ret = Post_check_var(callstmt, callstmt->Bb(), next->Lhs(), value,
                                   opr, vonly, 1, &new_path, caller, srcpos_h);
              if (!ret) {
                path->pop_back();
                return FALSE;
              }
            }
            srcpos_h->Reset_cur_node(cur_node, parent_idx);
          }
        }
        path->pop_back();
        if (!ret) {
          srcpos_h->Append_data(stmt, dna, PATHINFO_RBC);
          Plist_false()->push_back(srcpos_h->Clone());
        }
        return ret;
      }
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_size = 0;
  INT i =0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (from_bb->Succ() != NULL) {
    succ_size = from_bb->Succ()->Len();
    if (succ_size > 1) {
      srcpos_h->Add_children(succ_size);
      cur_node = srcpos_h->Cur_node();
    }
    ret = TRUE;
  }
  else {
    if (!ret) {
      STMTREP *last_stmt = Get_last_stmt(from_bb);
      if (last_stmt != NULL)
        srcpos_h->Append_data(last_stmt, dna, PATHINFO_RBC);
      Plist_false()->push_back(srcpos_h->Clone());
    }
  }
  STMTREP *last_stmt = from_bb->Last_stmtrep();
  BOOL found = FALSE;
  BOOL cond = FALSE;
  if (last_stmt != NULL && (last_stmt->Opr() == OPR_TRUEBR ||
                            last_stmt->Opr() == OPR_FALSEBR)) {
    CODEREP *cmp = last_stmt->Rhs();
    if (cmp->Kind() == CK_OP && (cmp->Opr() == OPR_EQ ||
                                 cmp->Opr() == OPR_NE)) {
      CODEREP *rhs = cmp->Opnd(1);
      CODEREP *lhs = cmp->Opnd(0);
      CODEREP *v = NULL;
      UINT64 cv = 0;
      if (rhs->Kind() == CK_CONST) {
        cv = rhs->Const_val();
        v = lhs;
      }
      else if (lhs->Kind() == CK_CONST) {
        cv = lhs->Const_val();
        v = rhs;
      }
      if (v != NULL) {
        VAR_DEF_HELPER helper(v, last_stmt, vsa->Comp_unit());
        CHECK_OBJ check_obj(v, last_stmt);
        vsa->Var_def_trav_helper(&helper, check_obj);
        DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
        if (def_info_vec.size() == 1) {
          CODEREP *def_cr = def_info_vec[0]->Coderep();
          if (def_cr != NULL && def_cr->Kind() == CK_CONST) {
            UINT64 def_cr_value = def_cr->Const_val();
            if (cmp->Opr() == OPR_EQ)
              cond = (cv == def_cr_value);
            else
              cond = (cv != def_cr_value);
            found = TRUE;
            Is_Trace(Tracing(), (TFile, "RBC: BR(%d) %d = %lld opr(%d) %lld\n",
                                 last_stmt->Opr(), cond, cv, cmp->Opr(), def_cr_value));
          } // end if def_cr
        } // end def_info.size()
      } // end if v
    } // end if CK_OP
  } // end last_stmt
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(from_bb->Succ())) {
    if (succ_size > 1) {
      if (found) {
        if (last_stmt->Opr() == OPR_FALSEBR) {
          if (!cond) {
            // skip false branch target
            if (last_stmt->Label_number() != succ_bb->Labnam()) {
              continue;
            }
          }
          else {
            if (last_stmt->Label_number() == succ_bb->Labnam()) {
              continue;
            }
          }
        }
        else if (last_stmt->Opr() == OPR_TRUEBR) {
          if (cond) {
            if (last_stmt->Label_number() != succ_bb->Labnam()) {
              continue;
            }
          }
          else {
            if (last_stmt->Label_number() == succ_bb->Labnam()) {
              continue;
            }
          }
        }
      } // found
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (!Post_check_var(succ_bb->First_stmtrep(), succ_bb, v, value, opr, vonly, direction, path, dna, srcpos_h)) {
      ret = FALSE;
      break;
    }
  }
  path->pop_back();
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ reset visited BB%d\n", from_bb->Id()));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__post_check_var_value
//
// =============================================================================
UINT64
RBC_BASE::Eval__post_check_var_value(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // check if variable v is checked before it is used
  // BOOL Post_check_var_value(void *v, char *opr, void *value);
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Check_var_value.\n"));
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *v = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  if (v == NULL) {
    ret = 0;
    Is_Trace(Tracing(), (TFile, "RBC: return value of \"%s\" is not assigned to a variable.\n",
                         callee->Fname()));
    SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(v, call_stmt, dna, spos_pool), spos_pool);
    srcpos_h->Set_orig_stname(callee->Fname());
    Plist_false()->push_back(srcpos_h);
    return ret;
  }
  // process compare value
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *)Eval__exp(rbc_ctx, arg2);
  // process compare opr
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  ret = Eval__exp(rbc_ctx, arg1);
  OPERATOR cmp_op = Get_opr_from_char((const char*)ret);
  vector<IDTYPE> path;
  path.clear();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(v, call_stmt, dna, spos_pool, vsa), spos_pool);
  // when function call is used in compare statement directly,
  // there's no return variable, use function name in this case
  // i.e. if (scanf("%s", buf) == -1)
  // use function name for all cases.
  srcpos_h->Set_orig_stname(callee->Fname());
  CONTEXT_SWITCH context(dna);
  ret = Post_check_var(call_stmt, call_stmt->Bb(), v, value, cmp_op, FALSE, 0, &path, dna, srcpos_h);
  Is_True_Rbc(path.size() == 0, ("RBC ERROR: non-empty path in Post_check_var_value.\n"));
  Is_Trace(Tracing(), (TFile, "RBC: Post_check_var_value(v:cr%d, opr:%d, value:cr%d): %lld\n",
                       v->Coderep_id(), cmp_op,
                       value == NULL || (INT32)(INTPTR)value == -1 ? -1 : value->Coderep_id(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Post_check_func
//    ensure 'v' is validated via function 'fname' with 'value',
//    when 'v' is used in a statement, if the statement is not
//    a call to 'fname', return FALSE, when 'v' is not longer in used,
//    return TRUE.
//
// =============================================================================
BOOL
RBC_BASE::Post_check_func(STMTREP *from, CODEREP *v, char *fname, char *value, DNA_NODE *dna,
                          hash_set<IDTYPE> &visited, SRCPOS_HANDLE *srcpos_h)
{
  BOOL start = FALSE;
  BOOL ret = TRUE;
  if (from == NULL)
    return ret;
  BB_NODE *from_bb = from->Bb();
  if (srcpos_h != NULL && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Post_check_func due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         srcpos_h->Children_count(), dna->Fname()));
    return ret;
  }
  if (visited.find(from_bb->Id()) != visited.end()) {
    return ret;
  }
  visited.insert(from_bb->Id());
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ visit BB%d\n", from_bb->Id()));
  VSA *vsa = dna->Comp_unit()->Vsa();
  CODEREP *chk = v;
  STMTREP *stmt;
  STMTREP_ITER stmt_iter(from_bb->Stmtlist());
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (!start) {
      if (stmt == from) {
        start = TRUE;
        if (from_bb->Ifmerge()) {
          PHI_LIST *phi_list = from_bb->Phi_list();
          PHI_NODE *phi;
          PHI_LIST_ITER phi_iter;
          BOOL found = FALSE;
          FOR_ALL_ELEM(phi, phi_iter, Init(phi_list)) {
            if (phi->Live()) {
              PHI_OPND_ITER phi_opnd_iter(phi);
              CODEREP *opnd;
              FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
                if (chk == opnd) {
                  chk = phi->RESULT();
                  found = TRUE;
                  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ switch check cr%d to cr%d\n",
                                       opnd->Coderep_id(), chk->Coderep_id()));
                  break;
                }
              }
              if (found)
                break;
            }
          }
        }
      }
      else
        continue;
    }
    if (Is_var_used_in_stmt(chk, stmt, vsa)) {
      Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ cr%d used in stmt%d\n",
                           chk->Coderep_id(), stmt->Stmtrep_id()));
      srcpos_h->Append_data(stmt, dna, PATHINFO_RBC);
      if (stmt->Opr() == OPR_CALL && strcmp(ST_name(stmt->St()), fname) == 0) {
        if (value != NULL) {
          // the following is hard-coded now,
          // should be symbolic evaluation on addional condition
          // like: arg1 of fname is v && arg2 of fname is value
          CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(1);
          if (arg2 == NULL ||
              arg2->Kind() != CK_CONST ||
              arg2->Const_val() != (UINT64)(*value)) {
            ret = FALSE;
          }
        }
      }
      else {
        ret = FALSE;
      }
      if (!ret) {
        // report error
        Plist_false()->push_back(srcpos_h->Clone());
      }
      return ret;
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_size = 0;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (from_bb->Succ() != NULL) {
    succ_size = from_bb->Succ()->Len();
    if (succ_size > 1) {
      srcpos_h->Add_children(succ_size);
      cur_node = srcpos_h->Cur_node();
    }
  }
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(from_bb->Succ())) {
    if (succ_size > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (!Post_check_func(succ_bb->First_stmtrep(), chk, fname, value, dna, visited, srcpos_h)) {
      ret = FALSE;
      break;
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__post_check_var_func
//
// =============================================================================
UINT64
RBC_BASE::Eval__post_check_var_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // validate variable v via function call fname before it is used
  // BOOL Post_check_var_func(void *v, char *fname, char *value)
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Post_check_var_func\n"));
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *v = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (v == NULL) {
    Is_Trace(Tracing(),
             (TFile, "RBC: Post_check_var_func(v, func, value): 'v' is no longer used.\n"));
    return ret;
  }
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *fname = (char*)Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  char *value = (char*)Eval__exp(rbc_ctx, arg2);
  hash_set<IDTYPE> visited;
  visited.clear();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(v, call_stmt, dna, spos_pool, vsa), spos_pool);
  if (srcpos_h->Orig_stname() == NULL) {
    srcpos_h->Set_orig_stname(callee->Fname());
  }
  CONTEXT_SWITCH context(dna);
  STMTREP *chk_stmt = call_stmt->Next();
  if (chk_stmt == NULL) {
    BB_NODE *next_bb = call_stmt->Bb()->Next();
    if (next_bb != NULL)
      chk_stmt = next_bb->First_stmtrep();
  }
  ret = Post_check_func(chk_stmt, v, fname, value, dna, visited, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Post_check_var_func(v:cr%id, func:%s, value:%s): %lld\n",
                       v->Coderep_id(), fname, value == NULL? "NULL":value, ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__post_call & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__post_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Post_call(char *fname);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  UINT64 ret = Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(ret != 0, ("RBC ERROR: null fname passed to Post_call.\n"));
  char *fname = Vsa_demangle((char *)ret);
  STMTREP *from = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  DNA_NODE *callee = rbc_ctx.Callee();
  hash_set<IDTYPE> visited;
  visited.clear();
  CONTEXT_SWITCH context(dna);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, from, dna, spos_pool), spos_pool);
  srcpos_h->Set_orig_stname(callee->Fname());
  ret = Will_call_later(vsa, fname, from, from->Bb(), visited, srcpos_h);
  Is_True_Rbc(visited.size() == 0, ("RBC ERROR: non-empty path in Post_call.\n"));
  Is_Trace(Tracing(), (TFile, "RBC: Post_call(\"%s\"): %lld\n", fname, ret));
  if (fname)
    free(fname);
  return ret;
}


BOOL
RBC_BASE::Will_call_later(VSA *vsa_ctx, char *fname, STMTREP *from, BB_NODE *bb,
                          hash_set<IDTYPE>& visited, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  BOOL start = FALSE;
  if (srcpos_h != NULL && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Will_call_later due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         srcpos_h->Children_count(), vsa_ctx->Dna()->Fname()));
    return TRUE;
  }

  if (visited.find(bb->Id()) != visited.end())
    return TRUE;
  visited.insert(bb->Id());
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Will_call_later:visit BB%d\n", bb->Id()));

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (!start) {
      if (stmt == from)
        start = TRUE;
      else
        continue;
    }

    OPERATOR opr = stmt->Opr();
    if (stmt->Opr() == OPR_CALL) {
      char *callee_name = Vsa_demangle(ST_name(stmt->St()));
      BOOL found = strcmp(fname, callee_name) == 0;
      if (callee_name)
        free(callee_name);
      if (found) {
        ret = TRUE;
        Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Will_call_later:found! reset visit BB%d\n", bb->Id()));
        visited.erase(bb->Id());
        return ret;
      }
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_size = 0;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (bb->Succ() != NULL) {
    succ_size = bb->Succ()->Len();
    if (succ_size > 1) {
      srcpos_h->Add_children(succ_size);
      cur_node = srcpos_h->Cur_node();
    }
  }
  else {
    if (!ret) {
      STMTREP *last_stmt = Get_last_stmt(bb);
      if (last_stmt != NULL)
        srcpos_h->Append_data(last_stmt, vsa_ctx->Dna(), PATHINFO_RBC);
      SRCPOS_HANDLE *sh = srcpos_h->Clone();
      Plist_false()->push_back(sh);
    }
  }

  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_size > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, vsa_ctx->Dna(), PATHINFO_BRANCH);
      i++;
    }
    ret = Will_call_later(vsa_ctx, fname, succ_bb->First_stmtrep(), succ_bb, visited, srcpos_h);
    if (ret == FALSE)
      break;
  }
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ Will_call_later:reset visit BB%d\n", bb->Id()));
  visited.erase(bb->Id());
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_func_exec_successful
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_func_exec_successful(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_func_exec_successful(char *fname, char *opr, int value)
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  STRING fname = (STRING)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  STRING opr_str = (STRING)Eval__exp(rbc_ctx, arg1);
  OPERATOR cmp_op = Get_opr_from_char(opr_str);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  INT value = (INT)Eval__exp(rbc_ctx, arg2);
  STMTREP *src_stmt = rbc_ctx.Stmt();
  if (src_stmt != NULL && (src_stmt->Opr() == OPR_FALSEBR ||
                           src_stmt->Opr() == OPR_TRUEBR)) {
    CODEREP *cmp = src_stmt->Rhs();
    CODEREP *var_cr = NULL;
    CODEREP *val_cr = NULL;
    // indicating codes like: 0 < ret
    BOOL reverted = FALSE;
    if (cmp->Kind() == CK_OP && OPERATOR_is_compare(cmp->Opr())) {
      CODEREP *rhs = cmp->Opnd(1);
      CODEREP *lhs = cmp->Opnd(0);
      if (lhs->Kind() == CK_CONST) {
        var_cr = rhs;
        val_cr = lhs;
        reverted = TRUE;
      }
      else if (rhs->Kind() == CK_CONST) {
        var_cr = lhs;
        val_cr = rhs;
      }
      else {
        Rbc_eval_certainty()->push_back(REC_SKIP);
        Is_Trace(Tracing(),
                 (TFile,
                  "RBC::Is_func_exec_successful: none of the compare operand is constant, skip\n"));
        return ret;
      }
      if (var_cr->Kind() == CK_OP &&
          (var_cr->Opr() == OPR_CVT || var_cr->Opr() == OPR_CVTL)) {
        var_cr = var_cr->Opnd(0);
      }
      if (Is_var_retv_of_func(dna, var_cr, src_stmt, fname)) {
        if (cmp->Opr() == cmp_op) {
          if (val_cr != NULL &&
              val_cr->Kind() == CK_CONST &&
              (INT)val_cr->Const_val() == value) {
            ret = TRUE;
          }
        }
        else if ((cmp->Opr() == OPR_EQ && cmp_op == OPR_NE) ||
                 (cmp->Opr() == OPR_NE && cmp_op == OPR_EQ)) {
          if (val_cr != NULL &&
              val_cr->Kind() == CK_CONST &&
              (INT)val_cr->Const_val() != value) {
            ret = TRUE;
          }
        }
        else {
          Rbc_eval_certainty()->push_back(REC_SKIP);
          Is_Trace(Tracing(),
                   (TFile,
                    "RBC::Is_func_exec_successful: TODO: other comparison not yet implemented\n"));
        }
      }
      else {
        Rbc_eval_certainty()->push_back(REC_SKIP);
        Is_Trace(Tracing(),
                 (TFile,
                  "RBC::Is_func_exec_successful: not return value of the function, skip\n"));

      }
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_func_exec_successful(fname:%s, opr:%s, value:%d): %lld %s\n",
                       fname, opr_str, value, ret, Rbc_result_ignore() ? "ignored" : ""));
  return ret;
}


BOOL
RBC_BASE::Is_var_retv_of_func(DNA_NODE *dna, CODEREP *cr, STMTREP *stmt, STRING fname)
{
  BOOL ret = FALSE;
  CONTEXT_SWITCH context(dna);
  VAR_DEF_HELPER helper(cr, stmt, dna->Comp_unit());
  CHECK_OBJ check_obj(cr, stmt);
  dna->Comp_unit()->Vsa()->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  for (INT i = 0; i < def_info_vec.size(); i++) {
    DNA_NODE *def_dna = def_info_vec[i]->Dna();
    CODEREP *def_cr = def_info_vec[i]->Coderep();
    STMTREP *def_stmt = NULL;
    if (def_cr != NULL && def_cr->Kind() == CK_VAR)
      def_stmt = def_cr->Defstmt();
    if (def_stmt != NULL && OPERATOR_is_call(def_stmt->Opr())) {
      CONTEXT_SWITCH def_ctx(def_dna);
      if (strcmp(ST_name(def_stmt->St()), fname) == 0) {
        ret = TRUE;
        break;
      }
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_return_checked_properly
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_return_checked_properly(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_return_checked_properly(char *fname, char *opr, int value)
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  STRING fname = (STRING)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  STRING opr_str = (STRING)Eval__exp(rbc_ctx, arg1);
  OPERATOR cmp_op = Get_opr_from_char(opr_str);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  INT value = (INT)Eval__exp(rbc_ctx, arg2);
  STMTREP *src_stmt = rbc_ctx.Stmt();
  if (src_stmt != NULL &&
      (src_stmt->Opr() == OPR_FALSEBR ||
       src_stmt->Opr() == OPR_TRUEBR)) {
    CODEREP *cmp = src_stmt->Rhs();
    CODEREP *var_cr = NULL;
    CODEREP *val_cr = NULL;
    // indicating codes like: 0 < ret
    BOOL reverted = FALSE;
    if (cmp->Kind() == CK_OP &&
        OPERATOR_is_compare(cmp->Opr())) {
      CODEREP *rhs = cmp->Opnd(1);
      CODEREP *lhs = cmp->Opnd(0);
      INT chk_val = 0;
      if (lhs->Kind() == CK_CONST) {
        var_cr = rhs;
        val_cr = lhs;
        reverted = TRUE;
      }
      else if (rhs->Kind() == CK_CONST) {
        var_cr = lhs;
        val_cr = rhs;
      }
      if (var_cr != NULL && val_cr != NULL) {
        if (val_cr->Kind() == CK_CONST) {
          chk_val = (INT)val_cr->Const_val();
        }
        if (var_cr->Kind() == CK_OP &&
            (var_cr->Opr() == OPR_CVT ||
             var_cr->Opr() == OPR_CVTL)) {
          var_cr = var_cr->Opnd(0);
        }
        if (Is_var_retv_of_func(dna, var_cr, src_stmt, fname) &&
            chk_val == value) {
          if (cmp->Opr() == cmp_op) {
            ret = TRUE;
          }
          else if (cmp_op == OPR_NE) {
            if (cmp->Opr() == OPR_EQ) {
              ret = TRUE;
            }
          }
          else if (cmp_op == OPR_EQ) {
            if (cmp->Opr() == OPR_NE) {
              ret = TRUE;
            }
          }
          else if (cmp_op == OPR_GE) {
            if (cmp->Opr() == OPR_LT) {
              ret = TRUE;
            }
          }
          else if (cmp_op == OPR_GT) {
            if (cmp->Opr() == OPR_LE) {
              ret = TRUE;
            }
          }
          else if (cmp_op == OPR_LE) {
            if (cmp->Opr() == OPR_GT) {
              ret = TRUE;
            }
          }
          else if (cmp_op == OPR_LT) {
            if (cmp->Opr() == OPR_GE) {
              ret = TRUE;
            }
          }
          else {
            Rbc_eval_certainty()->push_back(REC_SKIP);
            Is_Trace(Tracing(),
                     (TFile,
                      "RBC::Is_return_checked_properly: TODO: other comparison not yet implemented\n"));
          }
        }
      }
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_return_checked_properly(fname:%s, opr:%s, value:%d): %lld %s\n",
                       fname, opr_str, value, ret, Rbc_result_ignore() ? "ignored" : ""));
  return ret;
}


// =============================================================================
//
// Eval__is_var_used_after: check if var is used in statements after
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_var_used_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_var_used_after(void *v)
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CONTEXT_SWITCH context(dna);
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(var, call_stmt, dna, spos_pool, vsa), spos_pool);
  ret = Is_var_used_after(var, call_stmt->Next(), dna, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_var_used_after(cr(%d)): %lld\n",
                       var == NULL ? -1 : var->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_var_used_after(CODEREP *var, STMTREP *stmt, DNA_NODE *dna,
                            SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || stmt == NULL || dna == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;

  hash_set<IDTYPE> visited_bb;
  ret = Is_var_used_in_bb(var, stmt, stmt->Bb(), dna, visited_bb, srcpos_h);
  if (!ret) {
    hash_set<IDTYPE> visited_rna;
    ret = Is_var_used_in_caller(var, dna, visited_rna, srcpos_h);
  }
  return ret;
}


BOOL
RBC_BASE::Is_var_used_in_bb(CODEREP *var, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                            hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || stmt == NULL || bb == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return ret;
  visited_bb.insert(bb->Id());

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  BOOL start = FALSE;

  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    if (!start)
      if (cur == stmt)
        start = TRUE;
      else
        continue;

    // when var is passed as a parameter of a call, it is considered to be
    // a use, there's no need to check in callees
    if (Is_var_used_in_stmt(var, cur, dna->Comp_unit()->Vsa())) {
      srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
      Plist_true()->push_back(srcpos_h->Clone()->Clone());
      ret = TRUE;
      srcpos_h->Remove_last();
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_len = 1;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (bb->Succ() != NULL) {
    succ_len = bb->Succ()->Len();
    if (succ_len > 1) {
      srcpos_h->Add_children(succ_len);
      cur_node = srcpos_h->Cur_node();
    }
  }
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_len > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (Is_var_used_in_bb(var, succ_bb->First_stmtrep(), succ_bb, dna,
                          visited_bb, srcpos_h))
      ret = TRUE;
  }
  return ret;
}


BOOL
RBC_BASE::Is_var_used_in_caller(CODEREP *var, DNA_NODE *dna,
                                hash_set<IDTYPE> &visited_rna,
                                SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || dna == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;

  if (var->Kind() == CK_VAR) {
    CODEREP *rhs = var;
    STMTREP *defstmt = var->Defstmt();
    while(defstmt != NULL && defstmt->Opr() == OPR_STID) {
      if (defstmt->Rhs() == NULL ||
          defstmt->Rhs()->Kind() != CK_VAR ||
          defstmt == defstmt->Rhs()->Defstmt())
        break;
      rhs = defstmt->Rhs();
      defstmt = rhs->Defstmt();
    }
    IDTYPE parm_idx = dna->Is_param(rhs);
    if (parm_idx != INVALID_VAR_IDX) {
      RNODE_VECTOR *clby_list = dna->Clby_list();
      VSA *vsa = dna->Comp_unit()->Vsa();
      if (clby_list->size() > 1) {
        INT idx = 0;
        INT parent_idx = srcpos_h->Cur_idx();
        srcpos_h->Add_children(clby_list->size());
        SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
        for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
          RNA_NODE *rna = (*clby_list)[i];
          if (rna == NULL)
            continue;
          if (visited_rna.find(rna->Rna_idx()) != visited_rna.end())
            continue;
          visited_rna.insert(rna->Rna_idx());
          DNA_NODE *caller = vsa->Ipsa()->Get_dna(rna->Caller_idx());
          if (caller == NULL || caller->Non_functional())
            continue;
          STMTREP *callstmt = rna->Callstmt();
          {
            CONTEXT_SWITCH caller_ctx(caller);
            srcpos_h->Set_cur_node(cur_node, idx);
            srcpos_h->Append_data(callstmt, caller, PATHINFO_DNA_CALLRETURN);
            idx++;
            CODEREP *arg = rna->Get_arg(parm_idx);
            hash_set<IDTYPE> visited_bb;
            if (Is_var_used_in_bb(arg, callstmt->Next(), callstmt->Bb(), caller,
                                  visited_bb, srcpos_h))
              ret = TRUE;
            else if (Is_var_used_in_caller(arg, caller, visited_rna, srcpos_h))
              ret = TRUE;
          }
        } // for clby_list
        srcpos_h->Reset_cur_node(cur_node, parent_idx);
      }
    } // if parm_idx
  }
  return ret;
}


// =============================================================================
//
// Eval__is_var_defined_after: check if var is defined in statements after
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_var_defined_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_var_defined_after(void *v)
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(var, call_stmt, dna, spos_pool, vsa), spos_pool);
  ret = Is_var_defined_after(var, call_stmt->Next(), dna, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_var_defined_after(cr(%d)): %lld\n",
                       var == NULL ? -1 : var->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_var_defined_after(CODEREP *var, STMTREP *stmt, DNA_NODE *dna,
                               SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || stmt == NULL || dna == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;

  hash_set<IDTYPE> visited_bb;
  ret = Is_var_defined_in_bb(var, stmt, stmt->Bb(), dna, visited_bb, srcpos_h);
  if (!ret) {
    hash_set<IDTYPE> visited_rna;
    ret = Is_var_defined_in_caller(var, dna, visited_rna, srcpos_h);
  }
  return ret;
}


BOOL
RBC_BASE::Is_var_defined_in_bb(CODEREP *var, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                               hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || stmt == NULL || bb == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return ret;
  visited_bb.insert(bb->Id());

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  BOOL start = FALSE;

  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    if (!start)
      if (cur == stmt)
        start = TRUE;
      else
        continue;

    OPERATOR opr = cur->Opr();
    CODEREP *lhs = cur->Lhs();
    if (opr == OPR_STID) {
      CODEREP *rhs = cur->Rhs();
      if (rhs->Kind() == CK_VAR) {
        VSA *vsa = dna->Comp_unit()->Vsa();
        if (vsa->Opt_stab()->Aux_stab_entry(rhs->Aux_id())->Is_preg()) {
          continue;
        }
      }
      if (lhs != NULL && lhs->Contains(var)) {
        srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
        Plist_true()->push_back(srcpos_h->Clone()->Clone());
        ret = TRUE;
        srcpos_h->Remove_last();
      }
    } // end OPR_STID
    else if (opr == OPR_ISTORE) {
      if (lhs != NULL) {
        CODEREP *base = lhs->Ilod_base() ? lhs->Ilod_base() : lhs->Istr_base();
        if (base != NULL && base->Contains(var)) {
          srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
          Plist_true()->push_back(srcpos_h->Clone()->Clone());
          ret = TRUE;
          srcpos_h->Remove_last();
        }
      }
    } // end OPR_ISTORE
    else if (OPERATOR_is_call(opr)) {
      // TODO, var is passed as parameter & defined in callee
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_len = 1;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (bb->Succ() != NULL) {
    succ_len = bb->Succ()->Len();
    if (succ_len > 1) {
      srcpos_h->Add_children(succ_len);
      cur_node = srcpos_h->Cur_node();
    }
  }
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_len > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (Is_var_defined_in_bb(var, succ_bb->First_stmtrep(), succ_bb, dna,
                             visited_bb, srcpos_h))
      ret = TRUE;
  }
  return ret;
}


BOOL
RBC_BASE::Is_var_defined_in_caller(CODEREP *var, DNA_NODE *dna,
                                   hash_set<IDTYPE> &visited_rna,
                                   SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (var == NULL || dna == NULL)
    return ret;
  if (srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;

  // if var is returned to caller
  BOOL found = FALSE;
  VSA *vsa = dna->Comp_unit()->Vsa();
  for (INT i = PDV_INIT_ID; i < dna->Retv_list()->size(); i++) {
    PDV_NODE *pdv = (*dna->Retv_list())[i];
    if ((pdv->Kind() & BY_RETURNSTMT) == 0)
      continue;
    if (Check_cr_eq(vsa, var, pdv->Stmt()->Lhs())) {
      found = TRUE;
      break;
    }
  }

  if (found) {
    // go callers
    RNODE_VECTOR *clby_list = dna->Clby_list();
    if (clby_list->size() > 1) {
      INT idx = 0;
      INT parent_idx = srcpos_h->Cur_idx();
      srcpos_h->Add_children(clby_list->size());
      SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
      for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
        RNA_NODE *rna = (*clby_list)[i];
        if (rna == NULL)
          continue;
        if (visited_rna.find(rna->Rna_idx()) != visited_rna.end())
          continue;
        visited_rna.insert(rna->Rna_idx());
        DNA_NODE *caller = vsa->Ipsa()->Get_dna(rna->Caller_idx());
        if (caller == NULL || caller->Non_functional())
          continue;
        STMTREP *callstmt = rna->Callstmt();
        {
          CONTEXT_SWITCH caller_ctx(caller);
          srcpos_h->Set_cur_node(cur_node, idx);
          srcpos_h->Append_data(callstmt, caller, PATHINFO_DNA_CALLRETURN);
          idx++;
          // var in caller
          CODEREP *retv = Get_ret(caller->Comp_unit()->Vsa(), rna);
          hash_set<IDTYPE> visited_bb;
          // continue check
          if (Is_var_defined_in_bb(retv, callstmt->Next(), callstmt->Bb(), caller,
                                   visited_bb, srcpos_h))
            ret = TRUE;
          else if (Is_var_defined_in_caller(retv, caller, visited_rna, srcpos_h))
            ret = TRUE;
        }
      } // end for clby_list
      srcpos_h->Reset_cur_node(cur_node, parent_idx);
    }
  }
  return ret;
}


// =============================================================================
//
// Eval__is_var_invalid_and_used_after: check if var is invalidated (call
// the same function again) then used later
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_var_invalid_and_used_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_var_invalid_and_used_after(void *v)
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(var, call_stmt, dna, spos_pool, vsa),
                                    spos_pool);
  {
    FULL_TRAV_CONTEXT trav_ctx(spos_pool);
    trav_ctx.Push_frame(call_stmt->Bb(), call_stmt->Next(), rbc_ctx.Rna(), TD_NONE,
                        vsa->Comp_unit());
    INV_USED_INFO chk_info(var, dna->Comp_unit(), rbc_ctx.Callee()->Fname());
    CONTEXT_SWITCH context(dna);
    ret = Is_var_invalid_and_used_in_bb(chk_info, call_stmt->Next(), call_stmt->Bb(),
                                        trav_ctx, srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_var_invalid_and_used_after(cr(%d)): %lld\n",
                       var == NULL ? -1 : var->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_var_invalid_and_used_in_bb(INV_USED_INFO &chk_info, STMTREP *stmt, BB_NODE *bb,
                                        FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (!chk_info.Is_good() || stmt == NULL || bb == NULL || srcpos_h == NULL || srcpos_h->Reach_check_limit())
    trav_ctx.Set_skip();
  if (trav_ctx.Skip())
    return ret;
  if (trav_ctx.Visited(bb))
    return ret;

  VSA *vsa = trav_ctx.Cur_vsa();
  DNA_NODE *dna = trav_ctx.Cur_dna();
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  BOOL start = FALSE;

  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    if (!start)
      if (cur == stmt)
        start = TRUE;
      else
        continue;

    OPERATOR opr = cur->Opr();
    if (chk_info.Invalid()) {
      if (chk_info.Comp_unit() == dna->Comp_unit()) {
        Is_Trace(Tracing(), (TFile, "RBC: STMT(%d) starts Is_var_used_after\n", cur->Stmtrep_id()));
        ret = Is_var_used_after(chk_info.Cr(), cur, dna, srcpos_h);
        Is_Trace(Tracing(), (TFile, "RBC: STMT(%d) done Is_var_used_after: %d\n", cur->Stmtrep_id(), ret));
        chk_info.Set_result(ret);
        trav_ctx.Reset_visited(bb);
        return ret;
      }
      else if (opr == OPR_STID &&
               vsa->Opt_stab()->Aux_stab_entry(cur->Lhs()->Aux_id())->Is_return_preg() &&
               cur->Next() != NULL &&
               cur->Next()->Opr() == OPR_RETURN) {
        // go caller
        if (Is_var_invalid_and_used_in_caller(chk_info, cur->Lhs(), trav_ctx, srcpos_h)) {
          ret = chk_info.Result();
          Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: func(%s) done going caller in STMT(%d)\n",
                               dna->Fname(), cur->Stmtrep_id()));
          trav_ctx.Reset_visited(bb);
          return ret;
        }
      } // end opr == OPR_STID
    }
    else {
      if (OPERATOR_is_call(opr) && OPERATOR_has_sym(opr)) {
        // invalidated call
        char *callee_name = ST_name(cur->St());
        if (strcmp(callee_name, chk_info.Fname()) == 0) {
          srcpos_h->Append_data(cur, dna, PATHINFO_DNA_CALLSITE);
          chk_info.Set_invalid(TRUE);
          Is_Trace(Tracing(), (TFile, "RBC: STMT(%d) set cr(%d) in func(%s) invalid\n",
                               cur->Stmtrep_id(), chk_info.Cr()->Coderep_id(),
                               chk_info.Comp_unit()->Dna()->Fname()));
        }
        else {
          // go callee
          if (Is_var_invalid_and_used_in_callee(chk_info, cur, trav_ctx, srcpos_h)) {
            ret = chk_info.Result();
            Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: func(%s) done going callee in STMT(%d)\n",
                                 dna->Fname(), cur->Stmtrep_id()));
            trav_ctx.Reset_visited(bb);
            return ret;
          }
        }
      } // end OPERATOR_is_call(opr)
      else if (opr == OPR_STID &&
               vsa->Opt_stab()->Aux_stab_entry(cur->Lhs()->Aux_id())->Is_return_preg() &&
               cur->Next() != NULL &&
               cur->Next()->Opr() == OPR_RETURN) {
        // go caller
        if (Is_var_invalid_and_used_in_caller(chk_info, cur->Lhs(), trav_ctx, srcpos_h)) {
          ret = chk_info.Result();
          Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: func(%s) done going caller in STMT(%d)\n",
                               dna->Fname(), cur->Stmtrep_id()));
          trav_ctx.Reset_visited(bb);
          return ret;
        }
      } // end opr == OPR_STID
    }
  } // end FOR_ALL_NODE stmt

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_len = 1;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (bb->Succ() != NULL) {
    succ_len = bb->Succ()->Len();
    if (succ_len > 1) {
      srcpos_h->Add_children(succ_len);
      cur_node = srcpos_h->Cur_node();
    }
  }
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_len > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (Is_var_invalid_and_used_in_bb(chk_info, succ_bb->First_stmtrep(), succ_bb, trav_ctx, srcpos_h)) {
      ret = TRUE;
      break;
    }
  }
  trav_ctx.Reset_visited(bb);
  return ret;
}


BOOL
RBC_BASE::Is_var_invalid_and_used_in_callee(INV_USED_INFO &chk_info, STMTREP *call_stmt,
                                            FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h)
{
  BOOL found = FALSE;
  if (!chk_info.Is_good() || srcpos_h == NULL || srcpos_h->Reach_check_limit())
    trav_ctx.Set_skip();
  if (trav_ctx.Skip())
    return found;

  // we are still in caller context here
  VSA *vsa = trav_ctx.Cur_vsa();
  DNA_NODE *dna = trav_ctx.Cur_dna();
  RNA_NODE *rna = dna->Get_callsite_rna(call_stmt);
  if (trav_ctx.Visited(rna, TD_DOWN))
    return found;

  // record call site in path
  INT idx = 0;
  INT parent_idx = srcpos_h->Cur_idx();
  srcpos_h->Add_children(rna->Callee_list().size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
       iter != rna->Callee_list().end(); iter++) {
    DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
    if (callee == NULL)
      continue;
    // set up path when stepping into callee
    srcpos_h->Set_cur_node(cur_node, idx);
    srcpos_h->Append_data(call_stmt, dna, PATHINFO_DNA_CALLSITE);
    idx++;
    if (!callee->Non_functional()) {
      BB_NODE *bb = callee->Comp_unit()->Cfg()->Entry_bb();
      STMTREP *stmt = bb->First_stmtrep();
      Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Try caller(%s) --> callee(%s)\n",
                           dna->Fname(), callee->Fname()));
      found = TRUE;
      trav_ctx.Push_frame(call_stmt->Bb(), call_stmt->Next(), rna, TD_DOWN, callee->Comp_unit());
      INV_USED_INFO callee_chk_info(chk_info.Cr(), chk_info.Comp_unit(), chk_info.Fname());
      callee_chk_info.Set_invalid(chk_info.Invalid());
      CONTEXT_SWITCH context(callee);
      if (Is_var_invalid_and_used_in_bb(callee_chk_info, stmt, bb, trav_ctx, srcpos_h))
        chk_info.Set_result(TRUE);
      trav_ctx.Pop_frame(TD_DOWN);
      Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Done caller(%s) --> callee(%s)\n",
                           dna->Fname(), callee->Fname()));
    } // end not callee none functional
    else {
    } // end callee none functional
  } // end for callee list
  srcpos_h->Reset_cur_node(cur_node, parent_idx);
  trav_ctx.Reset_visited(rna, TD_DOWN);
  return found;
}


BOOL
RBC_BASE::Is_var_invalid_and_used_in_caller(INV_USED_INFO &chk_info, CODEREP *ret_cr,
                                            FULL_TRAV_CONTEXT &trav_ctx, SRCPOS_HANDLE *srcpos_h)
{
  BOOL found = FALSE;
  if (!chk_info.Is_good() || srcpos_h == NULL || srcpos_h->Reach_check_limit())
    trav_ctx.Set_skip();
  if (trav_ctx.Skip())
    return found;

  // we are still in callee context here
  VSA *vsa = trav_ctx.Cur_vsa();
  DNA_NODE *dna = trav_ctx.Cur_dna();
  Is_Trace(Tracing(), (TFile, "%sCONTEXT TRAV: Find caller frame\n", SBar));
  FULL_TRAV_FRAME *frame = trav_ctx.Pop_frame(TD_DOWN);
  RNA_NODE *prev_rna = NULL;
  if (frame != NULL) {
    // we are in callee from a caller, now go back to caller
    // a -> ( b' -> ) b, done visiting b, need to go back to a, b' is implicit call
    // ^              |
    // |              |
    // ----------------
    // frame --> b
    // trav_ctx.top() --> b' or a
    prev_rna = frame->Rna();
    // if we are from an implicit call, pop one more frame
    FULL_TRAV_FRAME *implicit_frame = NULL;
    if (prev_rna->Is_flag_set(RBC_SE_IMPLICIT_CALL)) {
      implicit_frame = trav_ctx.Pop_frame(TD_DOWN);
      if (implicit_frame == NULL)
        return found;
      prev_rna = implicit_frame->Rna();
    }
    // frame --> b
    // implicit_frame --> b' or NULL
    // trav_ctx.top() --> a
    // restore frame, duplicate caller frame & push caller frame
    // we can not simply pop frame b as we might not done with b traversal
    // pop out frame b will lost visited info of b
    TRAV_DIRECTION dir = TD_DOWN;
    FULL_TRAV_FRAME *caller_frame = trav_ctx.Top_frame(dir);
    if (caller_frame == NULL) {
      dir = TD_UP;
      caller_frame = trav_ctx.Top_frame(dir);
    }
    if (caller_frame == NULL) {
      dir = TD_NONE;
      caller_frame = trav_ctx.Top_frame(dir);
    }
    // frame --> b
    // implicit_frame --> b' or NULL
    // caller_frame --> a
    // trav_ctx.top() --> a
    if (implicit_frame != NULL)
      trav_ctx.Push_frame(implicit_frame);
    trav_ctx.Push_frame(frame);
    Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Found caller frame\n"));
    Is_Trace(Tracing(),
             (TFile,
              "%sCONTEXT TRAV: 'a' -> 'b', 'b' reaches a return, go back to caller 'a'\n", SBar));
    // frame --> b
    // caller_frame --> a
    // trav_ctx.top() --> b
    if (trav_ctx.Visited(prev_rna, TD_UP))
      return found;
    if (implicit_frame != NULL)
      frame = implicit_frame;
    // we need continue traversal info for frame a, which is stored in callee frame b or b'
    STMTREP *stmt = frame->Stmt();
    BB_NODE *bb = frame->Bb();
    DNA_NODE *caller = caller_frame->Comp_unit()->Dna();
    Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Try callee(%s) --> caller(%s)\n",
                         dna->Fname(), caller->Fname()));
    found = TRUE;
    trav_ctx.Push_frame(trav_ctx.Duplicate(caller_frame, TD_UP));
    // a -> ( b' -> ) b -> a'
    // frame --> b
    // implicit_frame --> b' or NULL
    // caller_frame --> a
    // trav_ctx.top() --> a'
    srcpos_h->Append_data(prev_rna->Callstmt(), caller, PATHINFO_DNA_CALLRETURN);
    INV_USED_INFO caller_chk_info(chk_info.Cr(), chk_info.Comp_unit(), chk_info.Fname());
    caller_chk_info.Set_invalid(chk_info.Invalid());
    // we re-enter this bb by going back, reset here
    trav_ctx.Reset_visited(bb);
    CONTEXT_SWITCH context(caller);
    // update var to caller coderep if returned
    if (chk_info.Comp_unit() == dna->Comp_unit() && Check_cr_eq(vsa, chk_info.Cr(), ret_cr)) {
      caller_chk_info.Set_cr(Get_ret(caller->Comp_unit()->Vsa(), frame->Rna()));
      caller_chk_info.Set_cu(caller->Comp_unit());
    }
    if (Is_var_invalid_and_used_in_bb(caller_chk_info, stmt, bb, trav_ctx, srcpos_h))
      chk_info.Set_result(TRUE);
    trav_ctx.Pop_frame(TD_UP);
    // a -> ( b' -> ) b
    // trav_ctx.top() --> b
    Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Done callee(%s) --> caller(%s)\n",
                         dna->Fname(), caller->Fname()));
    trav_ctx.Reset_visited(prev_rna, TD_UP);
  } // end frame != NULL
  else {
    // we've done current function, try to go back to its caller
    // first check if there's a caller frame it should go back to, for example
    // a calls b calls c, then done c returns to b (b' in frame), done b, to returns to a
    // a -> b -> c -> b', done visiting b', need to go back to a
    // ^              |
    // |              |
    // ----------------
    UINT32 call_depth = trav_ctx.Stack_size();
    FULL_TRAV_CONTEXT tmp_ctx(trav_ctx.Mem_pool());
    FULL_TRAV_FRAME *caller_frame = NULL;
    frame = trav_ctx.Pop_frame(TD_UP);
    FULL_TRAV_FRAME *tmp_frame = frame;
    COMP_UNIT *callee_cu = NULL;
    if (frame != NULL) {
      tmp_ctx.Push_frame(frame);
      // find the caller frame a that calls frame b
      // frame --> b'
      // trav_ctx.top() --> c
      callee_cu = frame->Comp_unit();
      while (caller_frame == NULL) {
        frame = trav_ctx.Pop_frame(TD_UP);
        if (frame == NULL)
          frame = trav_ctx.Pop_frame(TD_DOWN);
        if (frame == NULL)
          break;
        tmp_ctx.Push_frame(frame);
        if (frame->Comp_unit() == callee_cu && frame->Direction() == TD_DOWN) {
          caller_frame = trav_ctx.Top_frame(TD_DOWN);
          if (caller_frame == NULL)
            caller_frame = trav_ctx.Top_frame(TD_UP);
          prev_rna = frame->Rna();
        }
      }
      // frame --> NULL
      // caller_frame --> a
      // trav_ctx.top() --> a
      while (tmp_ctx.Stack_size() > 0) {
        frame = tmp_ctx.Pop_frame(TD_UP);
        if (frame == NULL)
          frame = tmp_ctx.Pop_frame(TD_DOWN);
        trav_ctx.Push_frame(frame);
      }
      // frame --> b'
      // caller_frame --> a
      // trav_ctx.top() --> b'
      Is_True(trav_ctx.Stack_size() == call_depth, ("CONTEXT TRAV: wrong stack size\n"));
    }
    if (caller_frame != NULL) {
      // a -> b -> c -> b', go back to a
      Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Found caller frame\n"));
      Is_Trace(Tracing(),
               (TFile,
                "%sCONTEXT TRAV: a -> b -> c -> b', b' reaches return, go back to caller a\n", SBar));
      frame = tmp_frame;
      // frame --> b'
      if (trav_ctx.Visited(prev_rna, TD_UP))
        return found;
      STMTREP *stmt = frame->Stmt();
      BB_NODE *bb = frame->Bb();
      DNA_NODE *caller = caller_frame->Comp_unit()->Dna();
      Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Try callee(%s) --> caller(%s)\n",
                           dna->Fname(), caller->Fname()));
      found = TRUE;
      trav_ctx.Push_frame(trav_ctx.Duplicate(caller_frame, TD_UP));
      // a -> b -> c -> b' -> a'
      srcpos_h->Append_data(prev_rna->Callstmt(), caller, PATHINFO_DNA_CALLRETURN);
      INV_USED_INFO caller_chk_info(chk_info.Cr(), chk_info.Comp_unit(), chk_info.Fname());
      caller_chk_info.Set_invalid(chk_info.Invalid());
      // we re-enter this bb by going back, reset here
      trav_ctx.Reset_visited(bb);
      CONTEXT_SWITCH context(caller);
      // update var to caller coderep if returned
      if (chk_info.Comp_unit() == dna->Comp_unit() && Check_cr_eq(vsa, chk_info.Cr(), ret_cr)) {
        caller_chk_info.Set_cr(Get_ret(caller->Comp_unit()->Vsa(), frame->Rna()));
        caller_chk_info.Set_cu(caller->Comp_unit());
      }
      if (Is_var_invalid_and_used_in_bb(caller_chk_info, stmt, bb, trav_ctx, srcpos_h))
        chk_info.Set_result(TRUE);
      trav_ctx.Pop_frame(TD_UP);
      // a -> b -> c -> b'
      // trav_ctx.top() --> b'
      Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Done callee(%s) --> caller(%s)\n",
                           dna->Fname(), caller->Fname()));
      trav_ctx.Reset_visited(prev_rna, TD_UP);
    } // end caller_frame != NULL
    else {
      // there's no caller frame 'a', now try to go to its possible callers
      Is_Trace(Tracing(),
               (TFile,
                "%sCONTEXT TRAV: no caller frame, try all callers\n", SBar));
      RNODE_VECTOR *clby_list = dna->Clby_list();
      INT idx = 0;
      INT parent_idx = srcpos_h->Cur_idx();
      srcpos_h->Add_children(clby_list->size());
      SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
      for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
        RNA_NODE *caller_rna = (*clby_list)[i];
        if (caller_rna == NULL)
          continue;
        if (trav_ctx.Visited(caller_rna, TD_UP))
          continue;
        DNA_NODE *caller = vsa->Ipsa()->Get_dna(caller_rna->Caller_idx());
        if (caller == NULL || caller->Non_functional())
          continue;
        STMTREP *call_stmt = caller_rna->Callstmt();
        srcpos_h->Set_cur_node(cur_node, idx);
        srcpos_h->Append_data(call_stmt, caller, PATHINFO_DNA_CALLRETURN);
        idx++;
        BB_NODE *bb = call_stmt->Bb();
        Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Try callee(%s) --> caller(%s)\n",
                             dna->Fname(), caller->Fname()));
        found = TRUE;
        trav_ctx.Push_frame(NULL, NULL, caller_rna, TD_UP, caller->Comp_unit());
        INV_USED_INFO caller_chk_info(chk_info.Cr(), chk_info.Comp_unit(), chk_info.Fname());
        caller_chk_info.Set_invalid(chk_info.Invalid());
        CONTEXT_SWITCH context(caller);
        // update var to caller coderep if returned
        if (chk_info.Comp_unit() == dna->Comp_unit() && Check_cr_eq(vsa, chk_info.Cr(), ret_cr)) {
          caller_chk_info.Set_cr(Get_ret(caller->Comp_unit()->Vsa(), caller_rna));
          caller_chk_info.Set_cu(caller->Comp_unit());
        }
        if (Is_var_invalid_and_used_in_bb(caller_chk_info, call_stmt->Next(), bb, trav_ctx, srcpos_h))
          chk_info.Set_result(TRUE);
        trav_ctx.Pop_frame(TD_UP);
        Is_Trace(Tracing(), (TFile, "CONTEXT TRAV: Done callee(%s) --> caller(%s)\n",
                             dna->Fname(), caller->Fname()));
        trav_ctx.Reset_visited(caller_rna, TD_UP);
      } // end for clby_list
      srcpos_h->Reset_cur_node(cur_node, parent_idx);
    } // end caller_frame == NULL
  } // end frame == NULL
  return found;
}


// =============================================================================
//
// RBC_BASE::Eval__is_errno_checked_after
//     if errno is checked right after a call to errno-settint-function
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_errno_checked_after(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_errno_checked_after(void)
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  DNA_NODE *callee = rbc_ctx.Callee();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CONTEXT_SWITCH context(dna);
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *ret_cr = Get_ret(vsa, rna);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool),
                                    spos_pool);
  srcpos_h->Set_orig_stname(callee->Fname());
  hash_set<IDTYPE> visited_bb;
  UINT64 ret = Is_errno_checked_after(ret_cr, call_stmt->Next(), call_stmt->Bb(), dna,
                                      visited_bb, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_errno_checked_after(void): %lld\n", ret));
  return ret;
}


BOOL
RBC_BASE::Is_errno_checked_after(CODEREP *cr, STMTREP *stmt, BB_NODE *bb, DNA_NODE *dna,
                                 hash_set<IDTYPE> &visited_bb, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (bb == NULL || dna == NULL || srcpos_h == NULL || srcpos_h->Reach_check_limit())
    return ret;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return ret;
  visited_bb.insert(bb->Id());

  VSA *vsa = dna->Comp_unit()->Vsa();
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  BOOL start = FALSE;

  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    if (!start)
      if (cur == stmt)
        start = TRUE;
      else
        continue;

    OPERATOR opr = cur->Opr();
    if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
      CODEREP *cmp = cur->Rhs();
      if (cmp->Kind() == CK_OP && OPERATOR_is_compare(cmp->Opr())) {
        CODEREP *rhs = cmp->Opnd(1);
        CODEREP *lhs = cmp->Opnd(0);
        CODEREP *var = NULL;
        CODEREP *value = NULL;
        if (rhs->Kind() == CK_CONST) {
          value = rhs;
          var = lhs;
        }
        else if (lhs->Kind() == CK_CONST) {
          value = lhs;
          var = rhs;
        }
        if (Is_var_errno(var, vsa)) {
          if (value->Kind() == CK_CONST && value->Const_val() == 0) {
            ret = TRUE;
            return ret;
          }
          else {
            srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
            Plist_false()->push_back(srcpos_h->Clone()->Clone());
            srcpos_h->Remove_last();
            return ret;
          }
        } // end Is_var_errno
      }
    } // end if OPR_TRUEBR || OPR_FALSEBR
    else if (OPERATOR_is_call(opr) || opr == OPR_RETURN) {
      if (OPERATOR_has_sym(opr)) {
        ST *st = cur->St();
        if (st != NULL && strncmp(ST_name(st), "__errno_location", 16) == 0)
          continue;
      }
      srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
      Plist_false()->push_back(srcpos_h->Clone()->Clone());
      srcpos_h->Remove_last();
      return ret;
    } // end OPERATOR_is_call
    if (cr != NULL && Is_var_used_in_stmt(cr, cur, vsa)) {
      srcpos_h->Append_data(cur, dna, PATHINFO_RBC);
      Plist_false()->push_back(srcpos_h->Clone()->Clone());
      srcpos_h->Remove_last();
      return ret;
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_size = 0;
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  if (bb->Succ() != NULL) {
    succ_size = bb->Succ()->Len();
    if (succ_size > 1) {
      srcpos_h->Add_children(succ_size);
      cur_node = srcpos_h->Cur_node();
    }
    // assume all succ paths have been checked
    ret = TRUE;
  }
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_size > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      i++;
    }
    if (!Is_errno_checked_after(cr, succ_bb->First_stmtrep(), succ_bb, dna,
                                visited_bb, srcpos_h))
      ret = FALSE;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__parm_is_def_by_func
//
// =============================================================================
UINT64
RBC_BASE::Eval__parm_is_def_by_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Parm_is_def_by_func(void *parm, const char *fname)
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  UINT64 ret = FALSE;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *parm = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(parm, call_stmt, dna, spos_pool), spos_pool);
  if (parm == NULL || parm->Kind() != CK_VAR) {
    Plist_false()->push_back(srcpos_h);
    return FALSE;
  }
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 func = Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(func != 0, ("RBC ERROR: null fname passed to Parm_is_def_by_func.\n"));
  STRING fname = Vsa_demangle((char*)func);
  STMTREP *defstmt = parm->Defstmt();
  while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
    srcpos_h->Append_data(defstmt, dna, PATHINFO_COPY);
    CODEREP *rhs = defstmt->Rhs();
    if (rhs == NULL || rhs->Kind() != CK_VAR ||
        (defstmt = rhs->Defstmt()) == NULL)
      break;
  }
  if (defstmt) {
    srcpos_h->Append_data(defstmt, dna, PATHINFO_COPY);
    if (OPERATOR_is_call(defstmt->Opr())) {
      STRING callee_name = Vsa_demangle(ST_name(dna->File_idx(), defstmt->St()));
      int comp_res = strcmp(fname, callee_name);
      if (comp_res == 0)
        ret = TRUE;
      if (callee_name != NULL)
        free(callee_name);
    }
  }
  if (!ret)
    Plist_false()->push_back(srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Parm_is_def_by_func(cr%d, \"%s\"): %lld\n",
                       parm->Coderep_id(), fname, ret));
  if (fname != NULL)
    free(fname);
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_called_by check if function of vsa_ctx is called by fname
//   TODO: with inline, rule attached dna (source dna) may not exist,
//         we'll need to deal with that someway later
//
// BOOL Is_called_by(char *fname)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_called_by(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *fname = (char*)Eval__exp(rbc_ctx, arg0);
  if (fname != NULL)
    ret = (strcmp(fname, dna->Fname()) == 0);
  Is_Trace(Tracing(), (TFile, "RBC: Is_called_by(\"%s\"): %lld\n", fname == NULL ? "" : fname, ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_called_in_thread check if context function is called in
// thread work function
//
// BOOL Is_called_in_thread(void)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_called_in_thread(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  IPSA *ipsa = rbc_ctx.Ipsa();
  DNA_NODE *caller = rbc_ctx.Caller();
  CONTEXT_SWITCH caller_ctx(caller);
  RNA_NODE *rna = rbc_ctx.Rna();
  if (caller->Exec_in_thread()) {
    SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(NULL, rna->Callstmt(), caller,
                                                rbc_ctx.Spos_pool()),
                                  rbc_ctx.Spos_pool());
    sp_h->Set_orig_stname(rbc_ctx.Callee()->Fname());
    hash_set<IDTYPE> visited_dna;
    if (Is_called_in_dna_with_path(ipsa, caller, DNA_THREAD_ENTRY, sp_h, &visited_dna)) {
      ret = TRUE;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_called_in_thread(%s) returns %lld\n", rbc_ctx.Callee()->Fname(), ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_called_in_isr check if context function is called in
// signal handler function
//
// BOOL Is_called_in_isr(void)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_called_in_isr(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  IPSA *ipsa = rbc_ctx.Ipsa();
  DNA_NODE *caller = rbc_ctx.Caller();
  CONTEXT_SWITCH caller_ctx(caller);
  RNA_NODE *rna = rbc_ctx.Rna();
  if (caller->Exec_in_isr()) {
    SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(NULL, rna->Callstmt(), caller,
                                                rbc_ctx.Spos_pool()),
                                  rbc_ctx.Spos_pool());
    sp_h->Set_orig_stname(rbc_ctx.Callee()->Fname());
    hash_set<IDTYPE> visited_dna;
    if (Is_called_in_dna_with_path(ipsa, caller, DNA_ISR_ENTRY, sp_h, &visited_dna)) {
      ret = TRUE;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_called_in_isr(%s) returns %lld\n", rbc_ctx.Callee()->Fname(), ret));
  return ret;
}

BOOL
RBC_BASE::Is_called_in_dna_with_path(IPSA *ipsa, DNA_NODE *cur_dna, UINT32 dna_flag,
                                     SRCPOS_HANDLE *sp_h, hash_set<IDTYPE> *visited_dna)
{
  BOOL ret = FALSE;
  if (visited_dna->find(cur_dna->Dna_idx()) != visited_dna->end()) {
    return ret;
  }
  visited_dna->insert(cur_dna->Dna_idx());
  if (sp_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Is_called_in_dna_with_path spos reach limit(%d) processing func(%s)\n",
                                sp_h->Children_count(), cur_dna->Fname()));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return ret;
  }

  CONTEXT_SWITCH cur_ctx(cur_dna);
  if (cur_dna->Is_set_rbc_flag(dna_flag)) {
    sp_h->Append_data(cur_dna->St(), NULL, cur_dna, PATHINFO_ST_DECLARE);
    SRCPOS_HANDLE *cloned_sp = sp_h->Clone()->Clone();
    Plist_true()->push_back(cloned_sp);
    return TRUE;
  }
  sp_h->Add_children(cur_dna->Clby_list()->size());
  SRCPOS_TREENODE *cur_node = sp_h->Cur_node();
  INT parent_idx = sp_h->Cur_idx();
  RNODE_VECTOR *clby_list = cur_dna->Clby_list();
  for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
    sp_h->Set_cur_node(cur_node, i - 1);
    RNA_NODE *caller_rna = (*clby_list)[i];
    if (caller_rna == NULL) {
      continue;
    }
    DNA_NODE *caller = ipsa->Get_dna(caller_rna->Caller_idx());
    if (caller == NULL || caller->Non_functional()) {
      continue;
    }
    sp_h->Append_data(caller_rna->Callstmt(), caller, PATHINFO_DNA_CALLSITE);
    if (Is_called_in_dna_with_path(ipsa, caller, dna_flag, sp_h, visited_dna)) {
      ret = TRUE;
    }
  }
  sp_h->Reset_cur_node(cur_node, parent_idx);
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_called_in_loop check if context function is called in loop
//
// BOOL Is_called_in_loop(void)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_called_in_loop(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  IPSA *ipsa = rbc_ctx.Ipsa();
  DNA_NODE *caller = rbc_ctx.Caller();
  CONTEXT_SWITCH caller_ctx(caller);
  RNA_NODE *rna = rbc_ctx.Rna();
  if (rna->Is_flag_set(RNA_CALLED_IN_LOOP)) {
    SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(NULL, rna->Callstmt(), caller,
                                                rbc_ctx.Spos_pool()),
                                  rbc_ctx.Spos_pool());
    sp_h->Set_orig_stname(rbc_ctx.Callee()->Fname());
    hash_set<IDTYPE> visited_dna;
    if (Is_called_in_loop(rna, caller, sp_h, &visited_dna)) {
      ret = TRUE;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_called_in_loop(%s) returns %lld\n", rbc_ctx.Callee()->Fname(), ret));
  return ret;
}

BOOL
RBC_BASE::Is_called_in_loop(RNA_NODE *rna, DNA_NODE *cur_dna,
                            SRCPOS_HANDLE *sp_h, hash_set<IDTYPE> *visited_dna)
{
  BOOL ret = FALSE;
  if (visited_dna->find(cur_dna->Dna_idx()) != visited_dna->end()) {
    return ret;
  }
  visited_dna->insert(cur_dna->Dna_idx());
  if (sp_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Is_called_in_loop spos reach limit(%d) processing func(%s)\n",
                                sp_h->Children_count(), cur_dna->Fname()));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return ret;
  }

  CONTEXT_SWITCH ctx(cur_dna);
  BOOL is_stmt_in_loop = FALSE;
  BB_NODE *bb = rna->Callstmt()->Bb();
  while (bb != NULL) {
    if (bb->Loop() != NULL) {
      is_stmt_in_loop = TRUE;
      sp_h->Append_data(bb, cur_dna, PATHINFO_BRANCH);
      break;
    }
    bb = bb->Idom();
  }
  if (is_stmt_in_loop) {
    SRCPOS_HANDLE *cloned_sp = sp_h->Clone()->Clone();
    Plist_true()->push_back(cloned_sp);
    return TRUE;
  } else if (cur_dna->Is_set(DNA_CALLED_IN_LOOP)) {
    sp_h->Add_children(cur_dna->Clby_list()->size());
    SRCPOS_TREENODE *cur_node = sp_h->Cur_node();
    INT parent_idx = sp_h->Cur_idx();
    RNODE_VECTOR *clby_list = cur_dna->Clby_list();
    IPSA *ipsa = cur_dna->Comp_unit()->Vsa()->Ipsa();
    for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
      sp_h->Set_cur_node(cur_node, i - 1);
      RNA_NODE *caller_rna = (*clby_list)[i];
      if (caller_rna == NULL) {
        continue;
      }
      DNA_NODE *caller = ipsa->Get_dna(caller_rna->Caller_idx());
      if (caller == NULL || caller->Non_functional()) {
        continue;
      }
      sp_h->Append_data(caller_rna->Callstmt(), caller, PATHINFO_DNA_CALLSITE);
      if (Is_called_in_loop(caller_rna, caller, sp_h, visited_dna)) {
        ret = TRUE;
      }
    }
    sp_h->Reset_cur_node(cur_node, parent_idx);
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_memory_overlap check if tgt memory with size would
// overlap with src memory
//
// BOOL Is_memory_overlap(void *tgt, int size, void *src)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_memory_overlap(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 size = Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *src = (CODEREP*)Eval__exp(rbc_ctx, arg2);
  ret = Is_memory_overlap(tgt, size, src, call_stmt, dna);
  if (ret) {
    SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(tgt, call_stmt, dna, spos_pool, vsa), spos_pool);
    Plist_true()->push_back(srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_memory_overlap(cr%d, %lld, cr%d): %lld\n",
                       tgt == NULL ? -1 : tgt->Coderep_id(), size,
                       src == NULL ? -1 : src->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_memory_overlap(CODEREP *tgt, UINT64 size, CODEREP *src,
                            STMTREP *stmt, DNA_NODE *dna)
{
  UINT64 ret = FALSE;
  if (dna == NULL || tgt == NULL || src == NULL || stmt == NULL)
    return ret;

  CONTEXT_SWITCH context(dna);
  CODEREP *tgt_base = NULL;
  UINT64 tgt_start = 0;
  CODEREP *src_base = NULL;
  UINT64 src_start = 0;
  OPERATOR opr = stmt->Opr();
  if (opr == OPR_MSTORE) {
    if (tgt->Kind() == CK_IVAR && tgt->Opr() == OPR_MLOAD) {
      tgt_base = tgt->Ilod_base() ? tgt->Ilod_base() : tgt->Istr_base();
      tgt_start = tgt->Offset();
    }
    if (src->Kind() == CK_IVAR && src->Opr() == OPR_MLOAD) {
      src_base = src->Ilod_base() ? src->Ilod_base() : src->Istr_base();
      src_start = src->Offset();
    }
  }
  else if (OPERATOR_is_call(opr)) {
    if (tgt->Kind() == CK_LDA) {
      tgt_base = tgt;
    }
    else if (tgt->Kind() == CK_OP)
    {
      if (tgt->Opnd(0) != NULL && tgt->Opnd(0)->Kind() == CK_LDA)
        tgt_base = tgt->Opnd(0);
      if (tgt->Opnd(1) != NULL && tgt->Opnd(1)->Kind() == CK_CONST)
        tgt_start = tgt->Opnd(1)->Const_val();
    }
    if (src->Kind() == CK_LDA) {
      src_base = src;
    }
    else if (src->Kind() == CK_OP) {
      if (src->Opnd(0) != NULL && src->Opnd(0)->Kind() == CK_LDA)
        src_base = src->Opnd(0);
      if (src->Opnd(1) != NULL && src->Opnd(1)->Kind() == CK_CONST)
        src_start = src->Opnd(1)->Const_val();
    }
  }
  if ((tgt_base != NULL) && (tgt_base == src_base)) {
    if (((tgt_start <= src_start) && (tgt_start + size - 1 >= src_start)) ||
        ((tgt_start > src_start) && (src_start + size - 1 >= tgt_start))) {
      ret = TRUE;
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_memory_big_enough check if tgt memory is big enough
// to hold element size with element count
//
// BOOL Is_memory_big_enough(void *tgt, int elem_sz, int elem_cnt)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_memory_big_enough(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 size = Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  UINT64 count = Eval__exp(rbc_ctx, arg2);
  ret = Is_memory_big_enough(tgt, size, count, call_stmt, dna);
  if (!ret) {
    SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(tgt, call_stmt, dna, spos_pool, vsa), spos_pool);
    Plist_false()->push_back(srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_memory_big_enough(cr%d, %lld, %lld): %lld\n",
                       tgt == NULL ? -1 : tgt->Coderep_id(), size, count, ret));
  return ret;
}


BOOL
RBC_BASE::Is_memory_big_enough(CODEREP *tgt, UINT64 elem_sz, UINT64 elem_cnt,
                               STMTREP *stmt, DNA_NODE *dna)
{
  UINT64 ret = TRUE;
  if (dna == NULL || tgt == NULL || stmt == NULL || elem_sz == 0 || elem_cnt == 0)
    return ret;

  CONTEXT_SWITCH context(dna);
  UINT64 tgt_sz = 0;
  BOOL sz_known = Get_mem_size(dna->Comp_unit()->Vsa(), tgt, tgt_sz);
  if (sz_known) {
    if (tgt_sz < elem_sz * elem_cnt)
      ret = FALSE;
  }
  return ret;
}


BOOL
RBC_BASE::Get_mem_size(VSA *vsa, CODEREP *cr, UINT64 &size)
{
  BOOL ret = FALSE;
  switch (cr->Kind()) {
  case CK_LDA:
    {
      ST *base_st = cr->Lda_base_st();
      TY_IDX base_ty = ST_type(base_st);
      size = TY_size(base_ty);
      ret = TRUE;
    }
    break;
  case CK_VAR:
    {
      HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(cr);
      if (hor != NULL) {
        if (hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI)
          break;
        CODEREP *bs = hor->Heap_obj()->Byte_size();
        if (bs != NULL && bs->Kind() == CK_CONST) {
          size = (UINT64)bs->Const_val();
          ret = TRUE;
        }
      }
      if (!ret) {
        STMTREP *def_stmt = cr->Defstmt();
        if (def_stmt != NULL) {
          OPERATOR opr = def_stmt->Opr();
          if (opr == OPR_STID) {
            CODEREP *rhs = def_stmt->Rhs();
            if (rhs != NULL) {
              ret = Get_mem_size(vsa, rhs, size);
            }
          } // end OPR_STID
          else if (opr == OPR_CALL) {
            RNA_NODE *rna = vsa->Dna()->Get_callsite_rna(def_stmt);
            // give up on recursive call or try at most once?
            if (rna != NULL && !rna->Is_back_edge()) {
              const CALLEE_VECTOR& callee_list = rna->Callee_list();
              for (CALLEE_VECTOR::const_iterator iter = callee_list.begin();
                   iter != callee_list.end(); iter++) {
                DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
                if (callee == NULL)
                  continue;
                if (callee->Non_functional())
                  continue;
                for (INT i = PDV_INIT_ID; i < callee->Retv_list()->size(); i++) {
                  PDV_NODE *pdv = (*callee->Retv_list())[i];
                  if ((pdv->Kind() & BY_RETURNSTMT) == 0)
                    continue;
                  CODEREP *ret_cr = pdv->Stmt()->Lhs();
                  if (ret_cr != NULL) {
                    CONTEXT_SWITCH context(callee);
                    ret = Get_mem_size(callee->Comp_unit()->Vsa(), ret_cr, size);
                  }
                  if (ret)
                    break;
                }
                if (ret)
                  break;
              } // end for callee
            }
          } // end OPR_CALL
        }
      }
      // TODO return reg or others
    }
    break;
  case CK_IVAR:
    {
      HEAP_OBJ_REP *hor = vsa->Cr_2_heap_obj(cr);
      if (hor != NULL) {
        if (hor->Attr() == ROR_DEF_BY_VARPHI ||
            hor->Attr() == ROR_DEF_BY_VORPHI)
          break;
        CODEREP *bs = hor->Heap_obj()->Byte_size();
        if (bs != NULL && bs->Kind() == CK_CONST) {
          size = (UINT64)bs->Const_val();
          ret = TRUE;
        }
      }
      if (!ret) {
        CODEREP *base_cr = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
        ret = Get_mem_size(vsa, base_cr, size);
      }
    }
    break;
  case CK_OP:
    {
      if (cr->Opr() == OPR_ADD) {
        CODEREP *arg0 = cr->Opnd(0);
        CODEREP *arg1 = cr->Opnd(1);
        if (arg1->Kind() != CK_CONST)
          break;
        UINT64 offset = (UINT64)arg1->Const_val();
        if (arg0->Kind() == CK_LDA) {
          TY_IDX lda_ty = ST_type(arg0->Lda_base_st());
          if (Is_Structure_Type(lda_ty)) {
            // deal with structure base + offset
            FLD_ITER fld_iter = Make_fld_iter(TY_fld(lda_ty));
            do {
              FLD_HANDLE fld(fld_iter);
              if (!fld.Is_Null() && FLD_ofst(fld) == offset) {
                size = TY_size(FLD_type(fld));
                ret = TRUE;
                break;
              }
            } while (!FLD_last_field(fld_iter++));
          }
          else {
            // array + idx
            UINT64 base_sz = 0;
            ret = Get_mem_size(vsa, arg0, base_sz);
            if (ret) {
              size = base_sz - offset;
            }
          }
        } // end CK_LDA
        else if (arg0->Kind() == CK_VAR) {
          // structure
          TY_IDX ty = arg0->Lod_ty();
          if (TY_kind(ty) == KIND_POINTER && Is_Structure_Type(TY_pointed(ty))) {
            FLD_ITER fld_iter = Make_fld_iter(TY_fld(TY_pointed(ty)));
            do {
              FLD_HANDLE fld(fld_iter);
              if (!fld.Is_Null() && FLD_ofst(fld) == offset) {
                size = TY_size(FLD_type(fld));
                ret = TRUE;
                break;
              }
            } while (!FLD_last_field(fld_iter++));
          }
        } // end CK_VAR
      } // end OPR_ADD
      else if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
        ret = Get_mem_size(vsa, cr->Opnd(0), size);
      } // end OPR_CVT
    }
    break;
  default:
    break;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__func_may_enter_recursion & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__func_may_enter_recursion(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Func_may_enter_recursion(void *func);
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Func_may_enter_recursion.\n"));
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Func_may_enter_recursion.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = rbc_ctx.Ipsa()->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Func_may_enter_recursion.\n"));
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
  vector<DNA_NODE *> visited;
  visited.clear();
  ret = Func_may_enter_recursion(arg_dna, srcpos_h, &visited);
  if (ret != 0) {
    srcpos_h->Set_orig_stname(arg_dna->Fname());
    Plist_true()->push_back(srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Func_may_enter_recursion(\"%s\"): %lld\n", arg_dna->Fname(), ret));
  return ret;
}


BOOL
RBC_BASE::Func_may_enter_recursion(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited)
{
  CONTEXT_SWITCH context(dna);
  for (vector<DNA_NODE*>::iterator iter = visited->begin(); iter != visited->end(); iter++) {
    if (*iter == dna) {
      return TRUE;
    }
  }
  visited->push_back(dna);
  Is_Trace(Tracing(), (TFile, "RBC: checking callees of %s\n", dna->Fname()));
  RNA_NODE *call_rna = NULL;
  DNA_NODE *callee = NULL;
  for (CALLEE_ITER iter(dna->Comp_unit()->Vsa()->Ipsa(), dna); !iter.Is_end(); iter.Next()) {
    call_rna = iter.Current_callsite();
    callee = iter.Current();
    if (callee != NULL && !callee->Non_functional()) {
      srcpos_h->Append_data(call_rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
      if (Func_may_enter_recursion(callee, srcpos_h, visited)) {
        return TRUE;
      }
      srcpos_h->Remove_last();
    }
  }
  visited->pop_back();
  return FALSE;
}

// =============================================================================
//
// RBC_BASE::Is_Pdom_bb
// return TRUE if bb pdom check_bb, otherwise FALSE.
// Helper function for Eval__is_return_value_checked
//
// =============================================================================
BOOL
RBC_BASE::Is_Pdom_bb(BB_NODE *bb, BB_NODE *check_bb)
{
  BB_NODE     *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Pdom_bbs()))
    if (dom_bb == check_bb || Is_Pdom_bb(dom_bb, check_bb))
      return TRUE;
  return FALSE;
}

// =============================================================================
//
// RBC_BASE::Is_value_checked
// return TRUE if the BB (where stmt is defined) is Pdom_bbs of exit_bb, otherwise FALSE.
// Helper function for Eval__is_return_value_checked
//
// =============================================================================
BOOL
RBC_BASE::Is_value_checked(BB_NODE *exit_bb, STMTREP *stmt, DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h) {

  CODEREP *cr = stmt->Rhs();
  if (cr && cr->Is_flag_set(CF_DEF_BY_PHI)) {
    return TRUE;
  } else {
    if (OPERATOR_is_call(stmt->Opr())) {
      srcpos_h->Append_data(stmt, dna, PATHINFO_DNA_CALLSITE);
      return FALSE;
    }
    if (cr && cr->Kind() == CK_VAR) {
      while (stmt != NULL && stmt->Opr() == OPR_STID) {
        CODEREP *rhs = stmt->Rhs();
        if (rhs != NULL && rhs->Kind() == CK_VAR &&
            stmt != rhs->Defstmt()) {
          stmt = rhs->Defstmt();
          if (!rhs->Is_flag_set(CF_DEF_BY_CHI))
            srcpos_h->Append_data(stmt, dna, PATHINFO_COPY);
        } else {
          cr = rhs;
          break;
        }
        if (exit_bb != stmt->Bb() &&
            !Is_Pdom_bb(exit_bb, stmt->Bb()))
          return TRUE;
      }
      if (stmt != NULL &&
          Is_value_checked(exit_bb, stmt, dna, srcpos_h))
        return TRUE;
    }
  }
  return FALSE;
}

// =============================================================================
//
// RBC_BASE::Eval__is_return_value_checked check if value is checked before it is returned
// BOOL Is_return_value_checked()
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_return_value_checked(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_return_value_checked: Enter.\n"));
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Callee();
  // if real source code callee not defined, just return
  if (dna == rbc_ctx.Rbc_node())
    return 1;
  // add callsite srcpos
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, rbc_ctx.Rna()->Callstmt(),
                                                  rbc_ctx.Caller(), spos_pool), spos_pool);
  CODEREP *cr = NULL;
  EXITBB_ITER iter(dna->Comp_unit()->Cfg());
  FOR_ALL_ITEM(iter, Init()) {
    BB_NODE *exit_bb = iter.Cur_exit_bb();
    STMTREP *stmt = exit_bb->Last_stmtrep();
    if (stmt == NULL) continue;
    if (stmt != NULL &&
        (stmt->Opr() == OPR_RETURN ||
         stmt->Opr() == OPR_RETURN_VAL))
      stmt = stmt->Prev();
    if (stmt != NULL && stmt->Opr() == OPR_STID) {
      srcpos_h->Append_data(stmt, dna, PATHINFO_DNA_CALLRETURN);
      if (Is_value_checked(exit_bb, stmt, dna, srcpos_h))
        ret = 1;
    }
  }
  if (ret == 0) {
    Plist_false()->push_back(srcpos_h);
  }

  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__func_may_not_return & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__func_may_not_return(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Func_may_not_return(void *func);
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Func_may_not_return.\n"));
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Func_may_not_return.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = rbc_ctx.Ipsa()->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Func_may_not_return.\n"));
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
  vector<DNA_NODE *> visited;
  visited.clear();
  ret = Func_may_not_return(arg_dna, srcpos_h, &visited);
  if (ret != 0) {
    srcpos_h->Set_orig_stname(arg_dna->Fname());
    Plist_true()->push_back(srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Func_may_not_return(\"%s\"): %lld\n", arg_dna->Fname(), ret));
  return ret;
}


BOOL
RBC_BASE::Func_may_not_return(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited)
{
  CONTEXT_SWITCH context(dna);
  for (vector<DNA_NODE*>::iterator iter = visited->begin(); iter != visited->end(); iter++) {
    if (*iter == dna)
      return FALSE;
  }
  visited->push_back(dna);
  Is_Trace(Tracing(), (TFile, "RBC: checking callees of %s\n", dna->Fname()));
  RNODE_VECTOR *rna_list = dna->Call_list();
  RNA_NODE *call_rna = NULL;
  DNA_NODE *callee = NULL;
  for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
    call_rna = (*rna_list)[i];
    callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(call_rna->Uniq_callee());
    if (callee != NULL && !callee->Non_functional()) {
      srcpos_h->Append_data(call_rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
      if (Func_may_not_return(callee, srcpos_h, visited)) {
        visited->pop_back();
        return TRUE;
      }
      srcpos_h->Remove_last();
    }
    else {
      ST *callee_st = call_rna->Callee_st();
      if (callee_st != NULL) {
        if (strcmp(ST_name(callee_st), "exit") == 0 ||
            strcmp(ST_name(callee_st), "longjmp") == 0 ||
            strcmp(ST_name(callee_st), "_Exit") == 0) {
          srcpos_h->Append_data(call_rna->Callstmt(), dna, PATHINFO_RBC);
          visited->pop_back();
          return TRUE;
        }
      }
    }
  }
  visited->pop_back();
  return FALSE;
}


// =============================================================================
//
// RBC_BASE::Eval__func_is_asynchronous_safe & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__func_is_asynchronous_safe(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Func_is_asynchronous_safe(void *func);
  RBC_EVAL_SKIP();
  UINT64 ret = 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Func_is_asynchronous_safe.\n"));
  // signal handler can be -1, 0, 1
  if (arg->Kind() == CK_CONST || arg->Kind() != CK_LDA)
    return ret;
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Func_is_asynchronous_safe.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = rbc_ctx.Ipsa()->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Func_is_asynchronous_safe.\n"));
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
  vector<DNA_NODE *> visited;
  visited.clear();
  ret = Is_func_asynchronous_safe(arg_dna, srcpos_h, &visited);
  if (ret == 0) {
    srcpos_h->Set_orig_stname(arg_dna->Fname());
    Plist_false()->push_back(srcpos_h->Clone());
  }
  Is_Trace(Tracing(), (TFile, "RBC: Func_is_asynchronous_safe(\"%s\"):%lld\n", arg_dna->Fname(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_func_asynchronous_safe(DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h, vector<DNA_NODE*> *visited)
{
  BOOL ret = TRUE;
  CONTEXT_SWITCH context(dna);
  for (vector<DNA_NODE*>::iterator iter = visited->begin(); iter != visited->end(); iter++) {
    if (*iter == dna)
      return ret;
  }
  visited->push_back(dna);
  RNODE_VECTOR *rna_list = dna->Call_list();
  for (INT i = VAR_INIT_ID; i < rna_list->size(); i++) {
    RNA_NODE *rna = (*rna_list)[i];
    DNA_NODE *callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(rna->Uniq_callee());
    char *callee_name = NULL;
    if (callee == NULL) {
      if (rna->Callee_st() == NULL)
        break;
      callee_name = ST_name(rna->Callee_st());
    }
    else {
      if (callee->Call_list()->size() == 1) break;
      if (callee->Call_list()->size() > 1 &&
          !callee->Non_functional()) {
        srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
        if (!Is_func_asynchronous_safe(callee, srcpos_h, visited)) {
          ret = FALSE;
          break;
        }
      }
      callee_name = callee->Fname();
    }
    if (callee_name != NULL &&
        !(strcmp(callee_name, "abort") == 0 ||
          strcmp(callee_name, "_Exit") == 0 ||
          strcmp(callee_name, "quick_exit") == 0 ||
          strcmp(callee_name, "signal") == 0)) {
      srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_RBC);
      ret = FALSE;
      break;
    }
  }
  visited->pop_back();
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__func_performs_sanitize
//
// =============================================================================
UINT64
RBC_BASE::Eval__func_performs_sanitize(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_flag(DNA_SANITIZE_DATA);
  Is_Trace(Tracing(), (TFile, "RBC: Set DNA_SANITIZE_DATA on \"%s\"\n", callee->Fname()));
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__is_automatic_variable
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_automatic_variable(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_automatic_variable(void *v);
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null v passed to Is_automatic_variable.\n"));
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CONTEXT_SWITCH context(dna);
  ST *st = Get_cr_st(vsa, arg);
  if (st != NULL) {
    if (ST_sclass(st) == SCLASS_AUTO) {
      SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
      srcpos_h->Append_data(st, NULL,  // or call_stmt->Bb()?
                            dna, PATHINFO_ST_DECLARE);
      srcpos_h->Set_orig_stname(ST_name(st));
      Plist_true()->push_back(srcpos_h);
      ret = 1;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_automatic_variable(v:cr%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}


ST*
RBC_BASE::Get_cr_st(VSA *vsa_ctx, CODEREP *cr)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Get_cr_st.\n"));
  ST *ret = NULL;
  if (cr->Kind() == CK_VAR) {
    ret = vsa_ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
  }
  else if (cr->Kind() == CK_LDA) {
    ret = cr->Lda_base_st();
  }
  else if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
    if (base != NULL)
      if (base->Kind() == CK_VAR)
        ret = vsa_ctx->Opt_stab()->Aux_stab_entry(base->Aux_id())->St();
      else if (base->Kind() == CK_LDA) {
        ret = base->Lda_base_st();
      }
  }
  else if (cr->Kind() == CK_OP) {
    // try to use one of the opnds
    for (INT actual = 0; actual < cr->Kid_count(); ++ actual) {
      if (cr->Opr() == OPR_ICALL && actual == cr->Kid_count() - 1)
        break;
      CODEREP *opnd = cr->Opnd(actual);
      if (opnd->Kind() != CK_CONST) {
        ret = Get_cr_st(vsa_ctx, opnd);
        if (ret != NULL) {
          TY *ty = Ty_ptr(vsa_ctx->Dna()->File_idx(), ST_type(ret));
          if (ty != NULL && TY_kind(*ty) == KIND_SCALAR)
            continue;
        }
        break;
      }
    }
  }
  else {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: CK_KIND:%d not implemented yet in Get_cr_st.\n",
                         cr->Kind()));
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_dynamically_allocated_if_copied & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_dynamically_allocated_if_copied(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_dynamically_allocated_if_copied(void *v);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  // the variable may have been deleted if not used later
  if (arg == NULL)
    return 1;
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(arg, call_stmt, dna, spos_pool), spos_pool);
  VSA *vsa = rbc_ctx.Caller_vsa();
  UINT64 ret = Is_dynamically_allocated_if_copied(vsa, arg, srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_dynamically_allocated_if_copied(v:cr%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}


BOOL
RBC_BASE::Is_dynamically_allocated_if_copied(VSA *vsa_ctx, CODEREP *v, SRCPOS_HANDLE *srcpos_h)
{
  Is_True_Rbc(v != NULL, ("RBC ERROR: null v passed to Is_dynamically_allocated_if_copied.\n"));
  BOOL ret = TRUE;
  DNA_NODE *dna = vsa_ctx->Dna();
  RNODE_VECTOR *rna_list = dna->Call_list();
  RNA_NODE *rna = NULL;
  CONTEXT_SWITCH context(dna);
  for (INT rna_idx = VAR_INIT_ID; rna_idx < rna_list->size(); rna_idx++) {
    BOOL as_parm = FALSE;
    rna = (*rna_list)[rna_idx];
    Is_Trace(Tracing(), (TFile, "RBC: scanning RNA_NODE(%d) for cr%d\n",
                         rna->Rna_idx(), v->Coderep_id()));
    for (INT parm = VAR_INIT_ID; parm < rna->Arg_list()->size(); parm++) {
      if (Check_cr_eq(vsa_ctx, v, rna->Get_arg(parm))) {
        Is_Trace(Tracing(), (TFile, "RBC: cr%d is a parameter in RNA_NODE(%d) call:%d\n",
                             v->Coderep_id(), rna->Rna_idx(),
                             rna->Callstmt()->Opr()));
        if (rna->Callstmt()->Opr() == OPR_INTRINSIC_CALL) {
          INTRINSIC intrn = rna->Callstmt()->Rhs()->Intrinsic();
          if (intrn == INTRN_STRCPY ||
              intrn == INTRN_STRNCPY ||
              intrn == INTRN_MEMCPY) {
            srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_PARM);
            as_parm = TRUE;
          }
        }
        else if (rna->Callstmt()->Opr() == OPR_CALL) {
          ST *st = rna->Callee_st();
          if (st != NULL &&
              (strcmp(ST_name(st), "strncpy") == 0 ||
               strcmp(ST_name(st), "strcpy") == 0 ||
               strcmp(ST_name(st), "memcpy") == 0 ||
               strcmp(ST_name(st), "strcat") == 0 ||
               strcmp(ST_name(st), "strncat") == 0)) {
            srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_PARM);
            as_parm = TRUE;
          }
        }
        break;
      }
    }
    if (as_parm) {
      CODEREP *arg1 = rna->Get_arg(1);
      HEAP_OBJ_REP* hor = vsa_ctx->Cr_2_heap_obj(arg1);
      if (hor == NULL ||
          hor->Attr() == ROR_DEF_BY_LDA) {
        ST *st = NULL;
        if (arg1->Kind() == CK_OP) {
          for (INT actual = 0; actual < arg1->Kid_count(); ++actual) {
            CODEREP *opnd = arg1->Opnd(actual);
            const char* var_name = Generate_var_name(vsa_ctx, opnd);
            if (!Vsa_check_sym_ignore(var_name)) {
              st = Get_cr_st(vsa_ctx, opnd);
              break;
            }
          }
        }
        else {
          st = Get_cr_st(vsa_ctx, arg1);
        }
        if (st != NULL) {
          srcpos_h->Append_data(st, NULL, // or rna->Callstmt()->Bb()?
                                dna, PATHINFO_ST_DECLARE);
          srcpos_h->Set_orig_stname(ST_name(st));
          Plist_false()->push_back(srcpos_h);
        }
        ret = FALSE;
        break;
      }
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_compatible_parm_type & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_compatible_parm_type(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_compatible_parm_type(int i)
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  int parm = (int)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(parm != 0, ("RBC ERROR: 0 passed to Is_compatible_parm_type.\n"));
  CODEREP *arg = rna->Get_arg(parm);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null arg in Is_compatible_parm_type.\n"));
  if (arg->Kind() == CK_CONST) {
    Is_Trace(Tracing(), (TFile, "RBC: const in Is_compatible_parm_type, skipped.\n"));
    return 1;
  }
  if (callee == NULL) {
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return 1;
  }
  Is_True_Rbc(parm > 0 && parm < callee->Parm_list()->size(),
              ("RBC ERROR: parameter:%d out of parameter lists of \"%s\"\n", parm, callee->Fname()));
  VAR_NODE *func_parm = (*callee->Parm_list())[parm];
  ST *func_parm_st = callee->St_ptr(func_parm->St_idx());
  TY *parm_ty = Ty_ptr(callee->File_idx(), ST_type(func_parm_st));
  UINT64 ret = 0;
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CONTEXT_SWITCH context(dna);
  TY *arg_ty = Get_cr_ty(vsa, arg);
  ST *arg_st = Get_cr_st(vsa, arg);
  if (arg_st != NULL && Vsa_check_sym_ignore(ST_name(arg_st))) {
    STPATH *stp = dna->Get_stpath(call_stmt, arg);
    if (stp != NULL)
      arg_ty = Ty_ptr(dna->File_idx(), ST_type(stp->St_idx()));
  }
  if (arg_ty != NULL && TY_kind(*arg_ty) == KIND_STRUCT) {
    if (arg->object_ty() != TY_IDX_ZERO) {
      arg_ty = Ty_ptr(dna->File_idx(), arg->object_ty());
    }
  }
  if (parm_ty != NULL && arg_ty != NULL && !Is_compatible_type(parm_ty, arg_ty, arg)) {
    SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(arg, call_stmt, dna, spos_pool, vsa), spos_pool);
    Plist_false()->push_back(srcpos_h);
  }
  else {
    ret = 1;
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_compatible_parm_type(i:%d): %lld\n", parm, ret));
  return ret;
}


TY*
RBC_BASE::Get_cr_ty(VSA *vsa_ctx, CODEREP *cr)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Get_cr_ty.\n"));
  TY *cr_ty = NULL;
  ST *cr_st = NULL;
  if (cr->Kind() == CK_VAR) {
    if (vsa_ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_preg()) {
      // special handling for preg cr
      hash_set<IDTYPE> visited;
      TY_IDX ty = vsa_ctx->Find_preg_type(cr, visited);
      return ty != TY_IDX_ZERO ? Ty_ptr(vsa_ctx->Dna()->File_idx(), ty)
                               : NULL;
    }
    else {
      cr_st = vsa_ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
      return Ty_ptr(vsa_ctx->Dna()->File_idx(), ST_type(cr_st));
    }
  }
  else if (cr->Kind() == CK_LDA) {
    TY_IDX ty_idx = cr->Lda_ty();
    if (TY_kind(ty_idx) != KIND_POINTER)
      return NULL;
    TY_IDX ty_ptr = TY_pointed(ty_idx);
    if (TY_kind(ty_ptr) == KIND_STRUCT) {
      UINT field_id = cr->Afield_id();
      if (field_id != 0) {
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(ty_ptr, field_id, cur_field_id);
        if (!fld.Is_Null()) {
          ty_idx = FLD_type(fld);
        }
        else {
          return NULL;
        }
      }
    }
    return Ty_ptr(vsa_ctx->Dna()->File_idx(), ty_idx);
  }
  else if (cr->Kind() == CK_IVAR) {
    CODEREP *base = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
    if (base == NULL) {
      return NULL;
    }
    else {
      if (base->Kind() == CK_VAR)
        cr_st = vsa_ctx->Opt_stab()->Aux_stab_entry(base->Aux_id())->St();
      else if (base->Kind() == CK_LDA)
        cr_st = base->Lda_base_st();
      else
        return NULL;
      cr_ty = Ty_ptr(vsa_ctx->Dna()->File_idx(), ST_type(cr_st));
      UINT fld_id = cr->I_field_id();
      TY_IDX ilod_ty = cr->Ilod_ty();
      if (Is_Structure_Type(ilod_ty) && fld_id > 0) {
        UINT curr_id = 0;
        FLD_HANDLE fld_handle = FLD_get_to_field(ilod_ty, fld_id, curr_id);
        if (!fld_handle.Is_Null())
          return Ty_ptr(vsa_ctx->Dna()->File_idx(), FLD_type(fld_handle));
      }
      else if (TY_kind(*cr_ty) == KIND_POINTER)
        return Ty_ptr(vsa_ctx->Dna()->File_idx(), TY_pointed(*cr_ty));
    }
  }
  else if (cr->Kind() == CK_OP) {
    // TODO
  }
  else {
    Is_True_Rbc(false, ("RBC ERROR: CK_KIND:%d not implemented yet in Get_cr_ty.\n",
                        cr->Kind()));
  }
  return NULL;
}


BOOL
RBC_BASE::Is_compatible_type(TY *base, TY *v, CODEREP *var)
{
  Is_True_Rbc(base != NULL, ("RBC ERROR: null base passed to Is_compatible_type.\n"));
  Is_True_Rbc(v != NULL, ("RBC ERROR: null v passed to Is_compatible_type.\n"));
  Is_True_Rbc(var != NULL, ("RBC ERROR: null var passed to Is_compatible_type.\n"));
  BOOL ret = FALSE;
  if (base == v)
    return TRUE;

  BOOL is_ptr_or_arr = FALSE;
  TYPE_ID base_mtype = MTYPE_UNKNOWN;
  if (TY_kind(*base) == KIND_POINTER) {
    base_mtype = TY_mtype(TY_pointed(*base));
    is_ptr_or_arr = TRUE;
  }
  else if (TY_kind(*base) == KIND_ARRAY) {
    base_mtype = TY_mtype(TY_etype(*base));
    is_ptr_or_arr = TRUE;
  }

  TYPE_ID v_mtype = MTYPE_UNKNOWN;
  if (is_ptr_or_arr) {
    if (TY_kind(*v) == KIND_POINTER) {
      if (Is_Structure_Type(TY_pointed(*v))) {
        FLD_ITER fld_iter = Make_fld_iter(TY_fld(TY_pointed(*v)));
        UINT64 offset = var->Offset();
        do {
          FLD_HANDLE fld(fld_iter);
          if (!fld.Is_Null() && FLD_ofst(fld) == offset) {
            TY_IDX ftype = FLD_type(fld);
            if (TY_kind(ftype) == KIND_ARRAY) {
              v_mtype = TY_mtype(TY_etype(ftype));
            }
            else {
              v_mtype = TY_mtype(ftype);
            }
            break;
          }
        } while (!FLD_last_field(fld_iter++));
      }
      else {
        if (TY_kind(TY_pointed(*v)) == KIND_ARRAY)
          v_mtype = TY_mtype(TY_etype(TY_pointed(*v)));
        else
          v_mtype = TY_mtype(TY_pointed(*v));
      }
    }
    else if (TY_kind(*v) == KIND_ARRAY) {
      v_mtype = TY_mtype(TY_etype(*v));
    }
    if (base_mtype != MTYPE_UNKNOWN && base_mtype == v_mtype)
      ret = TRUE;
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Is_cr_ret check cr is stored return value
//
// =============================================================================
BOOL
RBC_BASE::Is_cr_ret(VSA *vsa_ctx, RNA_NODE *caller_rna, CODEREP *cr)
{
  if (!cr)
    return FALSE;
  RBC_EVAL_SKIP();
  CODEREP *ret = Get_ret(vsa_ctx, caller_rna);
  return ret ? (ret == cr) : FALSE;
}

// =============================================================================
//
// RBC_BASE::Set_tag
// Helper function for Eval__set_tag and Eval__unset_tag
//
// =============================================================================
BOOL
RBC_BASE::Set_tag(BOOL value, DNA_NODE *callee, IDTYPE parm_idx, STRING tag_name, BOOL is_ret)
{
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Set_tag[%d]: null tag\n", value));

  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if (!tag_base)
    tag_base = New_tag_base(Clone_string((STRING) tag_name, Mem_pool()));

  if(is_ret) {
    callee->Set_ret_tag(tag_base->Id(), value);
  } else {
    Is_True_Rbc(parm_idx != INVALID_VAR_IDX,
                ("RBC ERROR: Set_tag[%d]: not return or parameter\n", value));
    if(parm_idx < callee->Parm_list()->size()) {
      callee->Set_parm_tag(parm_idx, tag_base->Id(), value);
    } else {
      Is_Trace(Tracing(), (TFile, "RBC: Set_tag[%d]: mismatched callee\n", value));
      return FALSE;
    }
  }
  callee->Set_rbc_flag(DNA_RBC_TAG_CREATE);
  tag_base->Set_flag(RSC_TAG_USED);
  Is_Trace(Tracing(), (TFile, "RBC: Set_tag[%d](%d, \"%s:%d\") on func(\"%s\")\n",
                       value, parm_idx, tag_name, tag_base->Id(), callee->Fname()));
  return TRUE;
}
// =============================================================================
//
// RBC_BASE::Eval__set_tag
// BOOL   Set_tag(OBJECT obj, const STRING tag)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!(VSA_Enable_TAG))
    return TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if(obj == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__set_tag: set tag object may be optimized\n"));
    return TRUE;
  }

  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  DNA_NODE *callee = rbc_ctx.Callee();
  char *tag_name = Find_const_char(rbc_node, arg2);
  IDTYPE parm_idx = rna->Get_arg_with_cr(obj);
  BOOL is_ret = FALSE;
  if(Is_cr_ret(vsa, rna, obj)) {
    is_ret = TRUE;
  }
  UINT64 ret = Set_tag(TRUE, callee, parm_idx, tag_name, is_ret);
  if (ret) {
    DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
    if (rbc_callee != NULL) {
      rbc_callee->Set_rbc_flag(DNA_RBC_TAG_CREATE);
    }
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__unset_tag
// BOOL   Unset_tag(Object obj, const STRING tag)
//
// =============================================================================
UINT64
RBC_BASE::Eval__unset_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if(obj == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__unset_tag: set tag object may be optimized\n"));
    return TRUE;
  }

  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg2);
  IDTYPE parm_idx = rna->Get_arg_with_cr(obj);
  BOOL is_ret = FALSE;
  if(Is_cr_ret(vsa, rna, obj)) {
    is_ret = TRUE;
  }
  return Set_tag(TRUE, callee, parm_idx, tag_name, is_ret);
}


// =============================================================================
//
// RBC_BASE::Eval__set_tag_for_all_parm
//     BOOL   Set_tag_for_all_parm(STRING tag)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_tag_for_all_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{ 
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg1);
  // set tag for each parameter of callee
  for (INT i = PDV_INIT_ID; i < callee->Parm_list()->size(); i++) {
    if (!Set_tag(TRUE, callee, i, tag_name, FALSE)) {
      Is_Trace(Tracing(),
        (TFile, "RBC ERROR: Set_tag_for_all_parm: failed to set tag(%s) for parm(%d)\n",
         tag_name, i));
    }
  }
  return TRUE;
}


// =============================================================================
//
// RBC_BASE::Eval__is_tag_set
// BOOL   Is_tag_set(OBJECT v, const STRING tag)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_tag_set(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG) {
     Rbc_eval_certainty()->push_back(REC_SKIP);
     return 1;
  }
  BOOL ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *v = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(v != NULL, ("RBC ERROR: Is_tag_set: null v\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg1);
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Is_tag_set: null tag\n"));

  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if (tag_base != NULL) {
    CONTEXT_SWITCH caller_ctx(dna);
    // sp_h needs to be allocated in rbc_ctx Pool to be used in RBC::Report_error later
    SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(v, call_stmt, dna, rbc_ctx.Spos_pool()),
                                  rbc_ctx.Spos_pool());
    ret = Check_tag(dna, call_stmt, v, CHECK_BY_TAG, tag_base, sp_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_tag_set(cr(%d), \"%s\"): %d\n", v->Coderep_id(), tag_name, ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_tag_attr_set_for_all_parm
//     BOOL   Is_tag_attr_set_for_all_parm(STRING tag, STRING attr)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_tag_attr_set_for_all_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG) {
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return TRUE;
  }

  BOOL ret = TRUE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg1);
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Is_tag_attr_set_for_all_parm: null tag\n"));
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *attr_name = Find_const_char(rbc_node, arg2);
  Is_True_Rbc(attr_name != NULL, ("RBC ERROR: Is_tag_attr_set_for_all_parm: null attr\n"));

  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if(!tag_base) {
    tag_base = New_tag_base(Clone_string((STRING) tag_name, Mem_pool()));
  }
  IDTYPE attr_id = Add_tag_attr(attr_name);
  if (tag_base != NULL && attr_id != TAG_INVALID_ID) {
    CONTEXT_SWITCH caller_ctx(dna);
    // check tag for each rna argument
    for (INT i = VAR_INIT_ID; i <= rna->Arg_cnt(); i++) {
      CODEREP *parm = rna->Get_arg(i);
      if (parm == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC ERROR: Is_tag_attr_set_for_all_parm: parm(%d) is NULL\n", i));
        continue;
      }
      // sp_h needs to be allocated in rbc_ctx Pool to be used in RBC::Report_error later
      SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(parm, call_stmt, dna, rbc_ctx.Spos_pool()),
                                    rbc_ctx.Spos_pool());
      if (!Check_tag(dna, call_stmt, parm, CHECK_BY_TAG_ATTR, tag_base, sp_h, attr_id)) {
        ret = FALSE;
        Is_Trace(Tracing(), (TFile, "RBC: Is_tag_attr_set_for_all_parm(tag(%s), attr(%s), parm(%d)): %d\n",
                             tag_name, attr_name, i, ret));
      }
    }
  }
  else {
    Is_Trace(Tracing(), (TFile, "RBC: Is_tag_attr_set_for_all_parm: null tag_base or invalid attr_id\n"));
  }

  Is_Trace(Tracing(), (TFile, "RBC: Is_tag_attr_set_for_all_parm(tag(%s), attr(%s)): %d\n", tag_name, attr_name, ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__or_tag
// BOOL   Or_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
UINT64
RBC_BASE::Eval__or_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return 1;
  DNA_NODE *caller = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  UINT64 ret = 0;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(caller));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(caller));
  CODEREP *src = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if(tgt && src) {
    CONTEXT_SWITCH caller_ctx(caller);
    if (VSA_Enable_TAG_OLD) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *r_tor_list = vsa->Find_tor_list_from_cr(call_stmt, src, tag_kind);
      if (r_tor_list != NULL) {
        ret = vsa->Find_tgt_and_merge(call_stmt, tgt, r_tor_list);
        if (ret) {
          // try to merge tags to its base if there is
          CODEREP *base_cr = Find_ilod_base(tgt);
          if (base_cr != NULL && base_cr != tgt) {
            ret = vsa->Find_tgt_and_merge(call_stmt, base_cr, r_tor_list);
          }
        }
      }
    } else {
      TAG_PROP *tag_prop = vsa->Tag_prop();
      Is_True_Ret(tag_prop, ("vsa TAG_PROP not set"), FALSE);
      if (Is_cr_ret(vsa, rna, tgt)) {
        tgt = vsa->Comp_unit()->Find_return_value(call_stmt);
      }
      tag_prop->Bind_or_pending_tag<TO_DEF_BY_OR>(call_stmt->Bb(), call_stmt, tgt, src, tgt);
    }
    Is_Trace(Tracing(), (TFile, "RBC: Or_tag(cr(%d), cr(%d)): %lld\n",
                        tgt->Coderep_id(), src->Coderep_id(), ret));
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__merge_tag
// BOOL   Merge_tag(OBJECT tgt, OBJECT src1, OBJECT src2)
//
// =============================================================================
UINT64
RBC_BASE::Eval__merge_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return 1;
  DNA_NODE *caller = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  UINT64 ret = 0;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(caller));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(caller));
  CODEREP *src1 = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(caller));
  CODEREP *src2 = (CODEREP*)Eval__exp(rbc_ctx, arg2);
  if(tgt && src1 && src2) {
    CONTEXT_SWITCH caller_ctx(caller);
    if (VSA_Enable_TAG_OLD) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *src1_list = vsa->Find_tor_list_from_cr(call_stmt, src1, tag_kind);
      if (src1_list != NULL) {
        ret = vsa->Find_tgt_and_merge(call_stmt, tgt, src1_list);
        if (ret) {
          // try to merge tags to its base if there is
          CODEREP *base_cr = Find_ilod_base(tgt);
          if (base_cr != NULL && base_cr != tgt) {
            ret = vsa->Find_tgt_and_merge(call_stmt, base_cr, src1_list);
          }
        }
      }
      TOR_LIST_OLD *src2_list = vsa->Find_tor_list_from_cr(call_stmt, src2, tag_kind);
      if (src2_list != NULL) {
        ret = vsa->Find_tgt_and_merge(call_stmt, tgt, src2_list);
        if (ret) {
          // try to merge tags to its base if there is
          CODEREP *base_cr = Find_ilod_base(tgt);
          if (base_cr != NULL && base_cr != tgt) {
            ret = vsa->Find_tgt_and_merge(call_stmt, base_cr, src2_list);
          }
        }
      }
    } else {
      TAG_PROP *tag_prop = vsa->Tag_prop();
      Is_True_Ret(tag_prop, ("vsa TAG_PROP not set"), FALSE);
      if (Is_cr_ret(vsa, rna, tgt)) {
        tgt = vsa->Comp_unit()->Find_return_value(call_stmt);
      }
      tag_prop->Bind_or_pending_tag<TO_DEF_BY_OR>(call_stmt->Bb(), call_stmt, tgt, src1, src2);
    }
    Is_Trace(Tracing(), (TFile, "RBC: Merge_tag(cr(%d), cr(%d), cr(%d)): %lld\n",
                        tgt->Coderep_id(), src1->Coderep_id(), src2->Coderep_id(), ret));
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__copy_tag
// BOOL   Copy_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
UINT64
RBC_BASE::Eval__copy_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return 1;
  DNA_NODE *caller = rbc_ctx.Caller();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  UINT64 ret = 0;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(caller));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(caller));
  CODEREP *src = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if(tgt && src) {
    CONTEXT_SWITCH caller_ctx(caller);
    if (VSA_Enable_TAG_OLD) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *tor_list = vsa->Find_tor_list_from_cr(call_stmt, src, tag_kind);
      if (tor_list != NULL) {
        Is_Trace(Tracing(), (TFile, "RBC: Copy_tag bind to cr(%d)\n", tgt->Coderep_id()));
        ret = vsa->Bind_tor_list_to_cr(call_stmt, tgt, tor_list, TO_DEF_BY_COPY);
      }
    } else {
      TAG_PROP *tag_prop = vsa->Tag_prop();
      if (Is_cr_ret(vsa, rna, tgt)) {
        tgt = vsa->Comp_unit()->Find_return_value(call_stmt);
      }
      tag_prop->Bind_or_pending_tag<TO_DEF_BY_COPY>(call_stmt->Bb(), call_stmt, tgt, src);
    }
    Is_Trace(Tracing(), (TFile, "RBC: Copy_tag(cr(%d), cr(%d)): %lld\n",
                        tgt->Coderep_id(), src->Coderep_id(), ret));
  }
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__eval_tag
// BOOL   Eval_tag(OBJECT tgt, OBJECT src)
//
// =============================================================================
UINT64
RBC_BASE::Eval__eval_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return TRUE;
  DNA_NODE *caller = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  UINT64 ret = 0;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(caller));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(caller));
  CODEREP *src = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if(tgt && src) {
    CONTEXT_SWITCH caller_ctx(caller);
    if (VSA_Enable_TAG_OLD) {
      TAGOKIND tag_kind;
      TOR_LIST_OLD *tor_list = vsa->Find_tor_list_from_cr(call_stmt, src, tag_kind);
      if (tor_list != NULL) {
        if (Is_cr_ret(vsa, rna, tgt)) {
          CODEREP *ret_cr = vsa->Comp_unit()->Find_return_value(call_stmt);
          if(ret_cr) {
            ret = vsa->Bind_tor_list_to_cr(call_stmt, ret_cr, tor_list, TO_DEF_BY_SE);
          }
        } else {
          ret = vsa->Bind_tor_list_to_cr(call_stmt, tgt, tor_list, TO_DEF_BY_SE);
        }
      }
    } else {
      TAG_PROP *tag_prop = vsa->Tag_prop();
      if (Is_cr_ret(vsa, rna, tgt)) {
        tgt = vsa->Comp_unit()->Find_return_value(call_stmt);
      }
      tag_prop->Bind_or_pending_tag<TO_DEF_BY_SE>(call_stmt->Bb(), call_stmt, tgt, src);
    }
    Is_Trace(Tracing(), (TFile, "RBC: Eval_tag(cr(%d), cr(%d)): %lld\n",
                         tgt->Coderep_id(), src->Coderep_id(), ret));
  }
  return TRUE;
}


// ============================================================================
//
// RBC_BASE::Eval__set_tag_const_defval
// BOOL   Set_tag_const_defval(const STRING tag, BOOL value)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_tag_const_defval(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG)
    return 1;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg0);
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Set_tag_const_defval: null tag\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  BOOL value = (BOOL)Eval__exp(rbc_ctx, arg1);

  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if (tag_base == NULL)
    tag_base = New_tag_base(Clone_string((STRING) tag_name, Mem_pool()));
  // do the const side effect record
  callee->Set_rbc_flag(DNA_RBC_TAG_OP);
  Is_Trace(Tracing(), (TFile, "RBC: Set_tag_const_defval(\"%s\", %d)\n", tag_name, value));
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__decl_tag_equal
// BOOL   Decl_tag_equal(const STRING tag1, const STRING tag2)
// TODO
//
// =============================================================================
UINT64
RBC_BASE::Eval__decl_tag_equal(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *tag1_name = Find_const_char(rbc_node, arg0);
  Is_True_Rbc(tag1_name != NULL, ("RBC ERROR: Decl_tag_equal: null tag1\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *tag2_name = Find_const_char(rbc_node, arg1);
  Is_True_Rbc(tag2_name != NULL, ("RBC ERROR: Tag_declare equal: null tag2\n"));
  // do the equal record

  Is_Trace(Tracing(), (TFile, "RBC: Decl_tag_equal(\"%s\", \"%s\")\n", tag1_name, tag2_name));
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__set_tag_attr
// BOOL   Set_tag_attr(OBJECT tgt, OBJRCT src, const STRING tag, const STRING attr)
//        if need not to use tag, just pass ""
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_tag_attr(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  RNA_NODE *rna = rbc_ctx.Rna();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *tgt = (CODEREP *) Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *src = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  if(tgt == NULL || src == NULL) {
     Is_Trace(Tracing(),
              (TFile,
               "RBC: tgt/src maybe return value of \"%s\"is not assigned to a variable.\n",
               callee->Fname()));
     return ret;
  }
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg2);
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Set_tag_attr: null tag\n"));
  CODEREP *arg3 = stmt->Rhs()->Find_nth_arg(3 + Rbc_parm_ofst_adjust(dna));
  char *attr_name = Find_const_char(rbc_node, arg3);
  Is_True_Rbc(attr_name != NULL, ("RBC ERROR: Set_tag_attr: null attr\n"));
  IDTYPE attr_id = Add_tag_attr(attr_name);
  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if (attr_id != TAG_INVALID_ID) {
    CONTEXT_SWITCH context(dna);
    if (VSA_Enable_TAG_OLD) {
      TAGOKIND tag_kind;
      // find tor list from vor chi list first
      TOR_LIST_OLD *tor_list = vsa->Find_tor_list_from_cr(call_stmt, src, tag_kind);
      if (tor_list != NULL) {
        ret = vsa->Bind_tor_list_to_cr(call_stmt, tgt, tor_list, TO_DEF_BY_TAG_ATTR, tag_base, attr_id);
      }
    } else {
      TAG_PROP *tag_prop = vsa->Tag_prop();
      Is_True_Ret(tag_prop, ("vsa TAG_PROP not set"), FALSE);
      if (Is_cr_ret(vsa, rna, tgt)) {
        tgt = vsa->Comp_unit()->Find_return_value(call_stmt);
      }
      tag_prop->Bind_or_pending_tag<TO_DEF_BY_TAG_ATTR>(call_stmt->Bb(), call_stmt, tgt, src, NULL, tag_base, attr_id);
    }
  }

  Is_Trace(Tracing(), (TFile, "RBC: Set_tag_attr(cr(%d), cr(%d), tag(%s:%d), attr(%s:%d)): %lld\n",
                       tgt->Coderep_id(), src->Coderep_id(),
                       tag_name ? tag_name : "null", tag_base ? tag_base->Id() : TAG_INVALID_ID,
                       attr_name ? attr_name : "null", attr_id,
                       ret));
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__is_tag_attr_set
// BOOL   Is_tag_attr_set(OBJECT v, const STRING tag, const STRING attr)
//        if need not to use tag, just input ""
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_tag_attr_set(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  if (!VSA_Enable_TAG) {
     Rbc_eval_certainty()->push_back(REC_SKIP);
     return 1;
  }
  BOOL ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *rbc_node = rbc_ctx.Rbc_node();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *v = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(v != NULL, ("RBC ERROR: Is_tag_attr_set: null v\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *tag_name = Find_const_char(rbc_node, arg1);
  Is_True_Rbc(tag_name != NULL, ("RBC ERROR: Is_tag_attr_set: null tag\n"));
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  char *attr_name = Find_const_char(rbc_node, arg2);
  Is_True_Rbc(attr_name != NULL, ("RBC ERROR: Is_tag_attr_set: null attr\n"));

  TAG_BASE *tag_base = Find_tag_base(tag_name);
  if(!tag_base) {
    tag_base = New_tag_base(Clone_string((STRING) tag_name, Mem_pool()));
  }
  IDTYPE attr_id = Add_tag_attr(attr_name);
  if (tag_base != NULL && attr_id != TAG_INVALID_ID) {
    CONTEXT_SWITCH ctx(dna);
    // sp_h needs to be allocated in rbc_ctx Pool to be used in RBC::Report_error later
    SRCPOS_HANDLE *sp_h = CXX_NEW(SRCPOS_HANDLE(v, call_stmt, dna, rbc_ctx.Spos_pool()),
                                  rbc_ctx.Spos_pool());
    ret = Check_tag(dna, call_stmt, v, CHECK_BY_TAG_ATTR, tag_base, sp_h, attr_id);
  } else {
    Is_Trace(Tracing(), (TFile, "RBC: Is_tag_attr_set: null tag_base or attr_id\n"));
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_tag_attr_set(cr(%d), tag(%s), attr(%s)): %d\n", v->Coderep_id(), tag_name, attr_name, ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_parm_tainted
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_parm_tainted(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_parm_tainted(Object arg);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  if (arg == NULL)
    return 0;
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null arg in Is_parm_tainted.\n"));
  CONTEXT_SWITCH caller_ctx(dna);
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool), spos_pool);
  srcpos_h->Set_orig_stname(srcpos_h->Find_orig_stname(arg, call_stmt, dna));
  hash_set<uint64_t> visited_rna_parm;
  UINT64 ret = Is_parm_tainted(vsa, rna, callee, arg, visited_rna_parm, srcpos_h);
  if(ret == 1)
    Plist_true()->push_back(srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_parm_tainted(cr:%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}

UINT64
RBC_BASE::Is_parm_tainted(VSA *vsa_ctx, RNA_NODE *caller_rna, DNA_NODE *callee, CODEREP *arg,
                          hash_set<uint64_t> & visited_rna_parm,
                          SRCPOS_HANDLE *srcpos_h)
{
  UINT64 ret = 0;
  DNA_NODE *dna = vsa_ctx->Dna();
  IDTYPE arg_idx = caller_rna->Get_arg_with_cr(arg);
  // Is_True_Rbc(arg_idx != INVALID_VAR_IDX, ("RBC ERROR: invalid arg idx\n"));
  uint64_t rna_parm = ((uint64_t)(caller_rna->Rna_idx())) << 32 | (uint64_t)arg_idx;
  if(visited_rna_parm.find(rna_parm) != visited_rna_parm.end()) {
    return ret;
  }
  visited_rna_parm.insert(rna_parm);

  // go U-D
  if (arg->Kind() == CK_VAR && dna->Is_param(arg) == INVALID_VAR_IDX) {
    STMTREP *defstmt = arg->Defstmt();
    while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
      CODEREP *rhs = defstmt->Rhs();
      if (rhs != NULL && rhs->Kind() == CK_VAR && defstmt != rhs->Defstmt()) {
        defstmt = rhs->Defstmt();
      }
      else {
        arg = rhs;
        break;
      }
    }
  }
  if(arg->Kind() == CK_IVAR)
    arg = arg->Ilod_base();
  // if it's an input paramter and not set in this function
  if (arg->Kind() == CK_VAR && dna->Is_param(arg) != INVALID_VAR_IDX) {
    if (dna->Clby_list()->size() == 1) {
      ret = 1;
      if(arg->Defstmt()) {
        srcpos_h->Append_data(arg->Defstmt(), dna, PATHINFO_RBC);
      }
      // srcpos_h->Set_orig_stname(srcpos_h->Find_orig_stname(arg, caller_rna->Callstmt(), dna, vsa_ctx));
      Is_Trace(Tracing(), (TFile, "RBC: arg(%d) is tainted as parameter of %s\n",
                           arg->Coderep_id(), dna->Fname()));
    }
    else {
      // go through clby_list
      INT idx = 0;
      srcpos_h->Add_children(dna->Clby_list()->size());
      SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
      for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
        RNA_NODE *rna = (*dna->Clby_list())[i];
        CODEREP *param = rna->Get_arg(dna->Is_param(arg));
        if (param == NULL)
          continue;
        DNA_NODE *caller_dna = vsa_ctx->Ipsa()->Get_dna(rna->Caller_idx());
        if (caller_dna == NULL)
          continue;
        srcpos_h->Set_cur_node(cur_node, idx);
        srcpos_h->Append_data(rna->Callstmt(), caller_dna, PATHINFO_DNA_CALLSITE);
        idx++;
        CONTEXT_SWITCH context(caller_dna);
        Is_Trace(Tracing(), (TFile, "RBC: goto caller %s for cr%d via RNA_NODE(%d)\n",
                             caller_dna->Fname(), param->Coderep_id(), rna->Rna_idx()));
        ret = Is_parm_tainted(caller_dna->Comp_unit()->Vsa(), rna, callee, param, visited_rna_parm, srcpos_h);
        if (ret)
          break;
      }
    }
  } else if(caller_rna->Is_set_arg_flag(arg_idx, ARG_TAINTED)) {
    Is_Trace(Tracing(), (TFile, "RBC: arg(%d) is tainted in RNA_NODE(%d)\n",
                         arg->Coderep_id(), caller_rna->Rna_idx()));
    srcpos_h->Set_orig_stname(srcpos_h->Find_orig_stname(arg, caller_rna->Callstmt(), dna));
    srcpos_h->Append_data(caller_rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
    ret = 1;
  }
  else {
    // search function calls that set tainted on parameters
    // process implicit assign
    STMTREP *call_stmt = caller_rna->Callstmt();
    SRCPOS cur_line = call_stmt->Linenum();
    // inlined function, find origin line number
    if (call_stmt->Next() != NULL && call_stmt->Next()->Opr() == OPR_PRAGMA &&
        call_stmt->Next()->Orig_wn() != NULL &&
        WN_pragma(call_stmt->Next()->Orig_wn()) == WN_PRAGMA_INLINE_BODY_END) {
      cur_line = call_stmt->Next()->Linenum();
    }
    RNODE_VECTOR *rna_list = dna->Call_list();
    for (INT i = VAR_INIT_ID; !ret && i < rna_list->size(); i++) {
      RNA_NODE *rna = (*rna_list)[i];
      SRCPOS iter_line = rna->Callstmt()->Linenum();
      if (SRCPOS_linenum(iter_line) >= SRCPOS_linenum(cur_line))
        continue;
      for (INT p = VAR_INIT_ID; !ret && p < rna->Arg_list()->size(); p++) {
        CODEREP *arg_p = rna->Get_arg(p);
        if (Check_cr_eq(vsa_ctx, arg, arg_p)) {
          if(Is_parm_tainted(vsa_ctx, rna, callee, arg_p, visited_rna_parm, srcpos_h)) {
            ret = 1;
            break;
          }
          if(rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
            SRC_VECTOR *srcs = vsa_ctx->Ipsa()->Get_assign_src(rna, arg_p);
            if(srcs) {
              INT idx = 0;
              INT parent_idx = srcpos_h->Cur_idx();
              srcpos_h->Add_children(srcs->size());
              SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
              for(int src_idx = 0 ; src_idx < srcs->size(); src_idx++) {
                IDTYPE src_id = (*srcs)[src_idx];
                CODEREP *src = rna->Get_arg(src_id);
                srcpos_h->Set_cur_node(cur_node, idx);
                srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_COPY);
                idx++;
                Is_Trace(Tracing(), (TFile, "RBC: try implicit assignment in RNA_NODE(%d) for cr%d\n",
                                     rna->Rna_idx(), src == NULL ? -1 : src->Coderep_id()));
                if(Is_parm_tainted(vsa_ctx, rna, callee, src, visited_rna_parm, srcpos_h)) {
                  ret = 1;
                  break;
                }
              } // end for srcs
              if (!ret)
                srcpos_h->Reset_cur_node(cur_node, parent_idx);
            }
          }
        }
      } // end for rna arg_list
    } // end for rna_list
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_std_output
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_std_output(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Is_std_output(Object v);
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (arg != NULL) {
    CONTEXT_SWITCH context(dna);
    ST *st = Get_cr_st(vsa, arg);
    if (st && (strcmp(ST_name(st), "stdout") == 0 ||
               strcmp(ST_name(st), "stderr") == 0))
      ret = TRUE;
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_std_output(cr%d): %lld\n",
                       arg == NULL ? -1 : arg->Coderep_id(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__jni_model_pragma
//
// =============================================================================
UINT64
RBC_BASE::Eval__jni_model_pragma(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  DNA_NODE *callee = rbc_ctx.Callee();
  Is_True(callee != NULL, ("JNI: no uniq callee for jni"));
  callee->Set_rbc_flag(DNA_JNI_MODEL);
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__valueof
// java::lang::Long* java::lang::Long::valueOf(long long)
// =============================================================================
UINT64
RBC_BASE::Eval__valueof(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  // should use Rbc_parm_offset, as this is only called internally
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_offset(dna));
  return Eval__exp(rbc_ctx, arg0);
}

// =============================================================================
//
// RBC_BASE::Eval__set_parm_deref
// BOOL Set_parm_deref(Object obj)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_parm_deref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee, ("RBC ERROR: Set_parm_deref: null callee.\n"));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  if(obj) {
    Is_True_Rbc(obj, ("RBC ERROR: Set_parm_deref: null obj"));
    IDTYPE parm_idx = rna->Get_arg_with_cr(obj);
    Is_True_Rbc(parm_idx != INVALID_VAR_IDX, ("RBC ERROR: Set_parm_deref: invalid parm idx\n"));
    rna->Set_arg_flag(parm_idx, rna->Get_arg_flags(parm_idx)|REF_ILOAD);
    if(parm_idx < callee->Parm_list()->size()) {
       // TODO: remove rna flag later, and make sure all check side use dna flag instead
      callee->Set_parm_flag(parm_idx, callee->Parm_flags(parm_idx) | REF_ILOAD);
      DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
      // mark flag on rbc_callee to identify the flag is from rbc
      // functional callee and rbc_callee can exists at the same time
      if (rbc_callee && rbc_callee != callee && parm_idx < rbc_callee->Parm_list()->size()) {
        rbc_callee->Set_parm_flag(parm_idx, rbc_callee->Parm_flags(parm_idx) | REF_ILOAD);
      }
      return TRUE;
    } else {
      Is_Trace(Tracing(), (TFile, "RBC Set_parm_deref: mismatched callee\n"));
      return FALSE;
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC Set_parm_deref: null obj\n"));
    return FALSE;
  }
}


// =============================================================================
//
// RBC_BASE::Eval__set_parm_mod
// BOOL Set_parm_mod(Object obj)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_parm_mod(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee, ("RBC ERROR: Set_parm_mod: null callee.\n"));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  if(obj) {
    IDTYPE parm_idx = rna->Get_arg_with_cr(obj);
    Is_True_Rbc(parm_idx != INVALID_VAR_IDX, ("RBC ERROR: Set_parm_mod: invalid parm idx\n"));
    rna->Set_arg_flag(parm_idx, rna->Get_arg_flags(parm_idx)|REF_ISTORE);
    if(parm_idx < callee->Parm_list()->size()) {
      callee->Set_parm_flag(parm_idx, callee->Parm_flags(parm_idx) | REF_ISTORE);
      DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
      // mark flag on rbc_callee to identify the flag is from rbc
      // functional callee and rbc_callee can exists at the same time
      if (rbc_callee && rbc_callee != callee && parm_idx < rbc_callee->Parm_list()->size()) {
        rbc_callee->Set_parm_flag(parm_idx, rbc_callee->Parm_flags(parm_idx) | REF_ISTORE);
      }
      return TRUE;
    } else {
      Is_Trace(Tracing(), (TFile, "RBC Set_parm_mod: mismatched callee\n"));
      return FALSE;
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC Set_parm_mod: null obj\n"));
    return FALSE;
  }
}

// =============================================================================
//
// RBC_BASE::Eval__set_parm_base_and_fld_name
// BOOL Set_parm_base_and_fld_name(Object base, Object fld_name)
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_parm_base_and_fld_name(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee, ("RBC ERROR: Set_parm_base_and_fld_name: null callee.\n"));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *base = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *fld_cr = (CODEREP *)Eval__exp(rbc_ctx, arg1);
  if(base && fld_cr) {
    IDTYPE parm_base_idx = rna->Get_arg_with_cr(base);
    IDTYPE parm_field_idx = rna->Get_arg_with_cr(fld_cr);
    Is_True_Rbc(parm_base_idx != INVALID_VAR_IDX && parm_field_idx != INVALID_VAR_IDX,
                ("RBC ERROR: Set_parm_base_and_fld_name: invalid parm idx\n"));
    INT parm_cnt = callee->Parm_list()->size();
    if(parm_base_idx < parm_cnt && parm_field_idx < parm_cnt) {
      callee->Set_parm_flag(parm_base_idx, callee->Parm_flags(parm_base_idx) | REF_BASE);
      callee->Set_parm_flag(parm_field_idx, callee->Parm_flags(parm_field_idx) | REF_FLDNM);
      return TRUE;
    } else {
      Is_Trace(Tracing(), (TFile, "RBC Set_parm_base_and_fld_name: mismatched callee\n"));
      return FALSE;
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC Set_parm_base_and_fld_name: null base or fld\n"));
    return FALSE;
  }
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_may_sleep
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_may_sleep(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (arg->Kind() == CK_CONST) {
    // hard code 0x10u for ___GFP_WAIT || bus_dmamap_load_mbuf(..., 0)
    if ((arg->Const_val() | 0x10u) ||
        (arg->Const_val() == 0)){
      callee->Set_flag(DNA_MAY_SLEEP);
      Is_Trace(Tracing(), (TFile, "RBC: Set DNA_MAY_SLEEP on \"%s\"\n", callee->Fname()));
    }
  }
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__set_atomic_region_begin
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_atomic_region_begin(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_flag(DNA_ATOMIC_REGION_BEGIN);
  Is_Trace(Tracing(), (TFile, "RBC: Set DNA_ATOMIC_REGION_BEGIN on \"%s\"\n", callee->Fname()));
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__set_atomic_region_end
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_atomic_region_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_flag(DNA_ATOMIC_REGION_END);
  Is_Trace(Tracing(), (TFile, "RBC: Set DNA_ATOMIC_REGION_END on \"%s\"\n", callee->Fname()));
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__set_func_atomic
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_atomic(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_flag(DNA_ATOMIC_FUNC);
  Is_Trace(Tracing(), (TFile, "RBC: Set DNA_ATOMIC_FUNC on \"%s\"\n", callee->Fname()));
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__set_shutdownhook
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_shutdown(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  rbc_ctx.Ipsa()->Enter_shutdown(rbc_ctx.Rna()->Caller_idx());
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_errno_setting
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_errno_setting(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *callee = rbc_ctx.Callee();
  callee->Set_flag(DNA_ERRNO_SETTING);
  Is_Trace(Tracing(), (TFile, "RBC: Set DNA_ERRNO_SETTING on \"%s\"\n", callee->Fname()));
  return 1;
}

// =============================================================================
//
// RBC_BASE::Set_func_container_init
// Set_func_container_init(OBJEC obj, OBJECT value)
// value can be null
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_container_init(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(obj != NULL, ("RBC ERROR: null obj for Set_func_container_init"));
  
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_CONTAINER_INIT) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Set_func_coll_append
// Set_func_coll_append(OBJEC obj, OBJECT value)
// =============================================================================
UINT64
RBC_BASE::Set_func_coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(obj != NULL && value != NULL,
              ("RBC ERROR: null obj or value passed to set_func_coll_append"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj in set_func_coll_append"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj in set_func_coll_append"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_COLL_APPEND) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
// RBC_BASE::Eval__set_func_coll_append
// Set_func_coll_append(OBJEC obj, OBJECT value)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_append(rbc_ctx, stmt, FALSE);
}

// =============================================================================
// RBC_BASE::Eval__set_func_coll_append_ref
// Set_func_coll_append_ref(OBJEC obj, OBJECT *value)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_append_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_append(rbc_ctx, stmt, TRUE);
}

// =============================================================================
// RBC_BASE::Eval__set_func_coll_insert
// Set_func_coll_insert(OBJEC obj, OBJECT pos, OBJECT cnt, OBJECT value)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_insert(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *pos = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CODEREP *arg3 = stmt->Rhs()->Find_nth_arg(3 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg3);

  Is_True_Rbc(obj != NULL && pos != NULL && value != NULL,
              ("RBC ERROR: null obj/pos/value passed to set_func_coll_insert"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj in set_func_coll_insert"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj in set_func_coll_insert"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_CONTAINER_OP) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
// RBC_BASE::Eval__set_func_coll_end
// Set_func_coll_end(OBJECT obj)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);

  Is_True_Rbc(obj != NULL,
              ("RBC ERROR: null obj/pos/value passed to Set_func_coll_end"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj in Set_func_coll_end"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj in Set_func_coll_end"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_CONTAINER_OP) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_coll_remove
// set_func_coll_remove(OBJECT v, OBJECT idx)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_remove(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *idx = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(obj != NULL && idx != NULL,
              ("RBC ERROR: null obj or value passed to set_func_coll_remove"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
              ("RBC ERROR: invalid obj or value idx in set_func_coll_remove"));
  callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_COLL_REMOVE) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Set_func_coll_get
// set_func_coll_get(OBJECT v, OBJECT idx)
// =============================================================================
UINT64
RBC_BASE::Set_func_coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *idx = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(obj != NULL && idx != NULL,
              ("RBC ERROR: null obj or value passed to set_func_coll_get"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX,
              ("RBC ERROR: invalid obj or value idx in set_func_coll_get"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (obj_idx < callee->Parm_list()->size()) {
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  } else {
    // c++ model using mangled name as prototype
    Is_Trace(Tracing(), (TFile, "RBC: Set_func_coll_get: mismatched callee\n"));
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_COLL_GET) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_coll_get
// set_func_coll_get(OBJECT v, OBJECT idx)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_get(rbc_ctx, stmt, FALSE);
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_coll_get_ref
// &set_func_coll_get_ref(OBJECT v, OBJECT idx)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_get_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_get(rbc_ctx, stmt, TRUE);
}

// =============================================================================
//
// RBC_BASE::Set_func_coll_back
// Set_func_coll_back(OBJECT v)
// =============================================================================
UINT64
RBC_BASE::Set_func_coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj or value idx in set_func_coll_get"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj or value idx in set_func_coll_get"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_COLL_BACK) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_coll_back
// Set_func_coll_back(OBJECT v)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_back(rbc_ctx, stmt, FALSE);
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_coll_back_ref
// &Set_func_coll_back_ref(OBJECT v)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_coll_back_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_coll_back(rbc_ctx, stmt, TRUE);
}

// =============================================================================
//
// RBC_BASE::Set_func_map_put
// Set_func_map_put(OBJEC obj, OBJECT key, OBJECT value)
// =============================================================================
UINT64
RBC_BASE::Set_func_map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *key = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(obj != NULL && key != NULL && value != NULL,
              ("RBC ERROR: null obj or value passed to Set_func_map_put"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj in Set_func_map_put"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj in Set_func_map_put"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_MAP_PUT) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_map_put
// Set_func_map_put(OBJEC obj, OBJECT key, OBJECT value)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_map_put(rbc_ctx, stmt, FALSE);
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_map_put_ref
// Set_func_map_put_ref(OBJEC obj, OBJECT key, OBJECT *value)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_map_put_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_map_put(rbc_ctx, stmt, TRUE);
}

// =============================================================================
//
// RBC_BASE::Set_func_map_get
// Set_func_map_get(OBJEC obj, OBJECT key)
// =============================================================================
UINT64
RBC_BASE::Set_func_map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);

  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *key = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(obj != NULL && key != NULL,
              ("RBC ERROR: null obj or key passed to set_func_map_get"));
  IDTYPE obj_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < rna->Arg_list()->size(),
              ("RBC ERROR: invalid obj in set_func_map_get"));
  rna->Set_arg_flag(obj_idx, rna->Get_arg_flags(obj_idx) | REF_BASE);
  if (rna->Callstmt()->Opr() != OPR_INTRINSIC_CALL) {
    Is_True_Rbc(obj_idx > INVALID_VAR_IDX && obj_idx < callee->Parm_list()->size(),
                ("RBC ERROR: invalid obj in set_func_map_get"));
    callee->Set_parm_flag(obj_idx, callee->Parm_flags(obj_idx) | REF_BASE);
  }
  Is_Trace(Tracing(), (TFile,
                       "RBC: Set DNA_RBC_SE_EVAL/RNA(RBC_SE_MAP_GET) on \"%s\"\n",
                       callee->Fname()));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_map_get
// Set_func_map_get(OBJEC obj, OBJECT key)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_map_get(rbc_ctx, stmt, FALSE);
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_map_get
// &Set_func_map_get(OBJEC obj, OBJECT key)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_map_get_ref(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  return Set_func_map_get(rbc_ctx, stmt, TRUE);
}

// =============================================================================
//
// RBC_BASE::Eval__set_func_str_get
// Set_func_str_get(OBJECT str, OBJECT idx)
// =============================================================================
UINT64
RBC_BASE::Eval__set_func_str_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  rna->Set_flag(RBC_SE_CONTAINER_OP);
  return TRUE;
}

UINT64
RBC_BASE::Eval__set_class_sensitive(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  Is_True_Rbc(FALSE, ("RBC ERROR: Should not be called, only trigger in Init__set_class_sensitive\n"));
  return TRUE;
}

// =============================================================================
//
// RBC_BASE::Eval__hard_coded_password
//
// =============================================================================
UINT64
RBC_BASE::Eval__hard_coded_password(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP*)Eval__exp(rbc_ctx, arg1);
  if (var != NULL && value != NULL) {
    CONTEXT_SWITCH context(dna);
    const char *var_name = Generate_var_name(vsa, var);
    BOOL found = FALSE;
    if (strcmp(var_name, "password") == 0) {
      found = TRUE;
    }
    else {
      STPATH *stp = dna->Get_stpath(rna->Callstmt(), var);
      if (stp != NULL) {
        var_name = stp->St_name();
        if (strcmp(var_name, "password") == 0) {
          found = TRUE;
        }
      }
    }
    if (found) {
      if (value->Kind() == CK_LDA) {
        ST *value_st = value->Lda_base_st();
        if (ST_class(value_st) == CLASS_CONST &&
            TCON_ty(ST_tcon_val(value_st)) == MTYPE_STR)
          ret = 1;
      }
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Hard_coded_password(cr%d, cr%d): %lld\n",
                       var == NULL ? -1 : var->Coderep_id(),
                       value == NULL ? -1 : value->Coderep_id(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_compression_extraction_safe
// bool Is_compression_extraction_safe(OBJECT obj)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_compression_extraction_safe(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  STMTREP *call_sr = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (obj != NULL) {
    CONTEXT_SWITCH ctx(dna);
    BB_NODE *bb = call_sr->Bb();
    BB_LOOP *loop = Find_bb_loop(vsa->Cfg(), bb);
    if (loop != NULL) {
      Is_Trace(Tracing(),
        (TFile, "RBC: BB_LOOP related with the object coderep, body bb: %d, obj: cr%d\n",
          loop->Header() ? loop->Header()->Id() : -1, obj->Coderep_id()));
      if (loop->Iv() != NULL && loop->Iv() == obj) {
        // if the obj is induction variable, treat de-compress action is safe
        Is_Trace(Tracing(), (TFile, "RBC: The obj is induction variable, obj: cr%d\n", obj->Coderep_id()));
      } else {
        BB_NODE *tbb;
        BB_NODE_SET_ITER iter;
        CODEREP *inc_cr = NULL;
        FOR_ALL_ELEM (tbb, iter, Init(loop->True_body_set())) {
          inc_cr = Find_self_inc_include_cr(vsa->Comp_unit()->Opt_stab(), tbb, obj);
          if (inc_cr != NULL) {
            break;
          }
        }
        // can't find a self increment variable that include the obj cr
        if (inc_cr == NULL) {
          Is_Trace(Tracing(),
            (TFile, "RBC: Can't find self increment variable include obj, obj: cr%d\n",
              obj->Coderep_id()));
        } else {
          Is_Trace(Tracing(),
            (TFile, "RBC: Find self increment variable include obj, inc var: cr%d, obj: cr%d\n",
              inc_cr->Coderep_id(), obj->Coderep_id()));
          ret = 1;
        }
      }
    } else {
      Is_Trace(Tracing(), (TFile, "RBC: Can't find related bb loop, bb: %d.\n", bb->Id()));
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_compression_extraction_safe(cr%d): %lld\n",
                        obj == NULL ? -1 : obj->Coderep_id(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Find_bb_loop find the bb related BB_LOOP
//    all BB_LOOP were normalized to do-while form
//
// =============================================================================
BB_LOOP*
RBC_BASE::Find_bb_loop(CFG *cfg, BB_NODE *bb)
{
  BB_LOOP *loops = cfg->Loops();
  if (loops == NULL) {
    return NULL;
  }
  BB_LOOP *loop = NULL;
  BB_LOOP_ITER iter(loops);
  FOR_ALL_NODE(loop, iter, Init()) {
    // find the deepest BB_LOOP
    if (loop->True_body_set()->MemberP(bb)) {
      BB_LOOP *parent = loop;
      while (parent->Child() != NULL) {
        BB_LOOP_ITER child_iter(parent->Child());
        BB_LOOP *child;
        BOOL found = FALSE;
        FOR_ALL_NODE(child, child_iter, Init()) {
          if (child->True_body_set()->MemberP(bb)) {
            parent = child;
            found = TRUE;
            break;
          }
        }
        if (!found) {
          return parent;
        }
      }
      return parent;
    }
  }
  return NULL;
}


// =============================================================================
//
// RBC_BASE::Find_self_inc_include_cr find a self increment variable that inclue
//    the cr
//
// =============================================================================
CODEREP *
RBC_BASE::Find_self_inc_include_cr(OPT_STAB *opt_stab, BB_NODE *bb, CODEREP *cr)
{
  STMTREP *stmt;
  STMTREP_ITER iter(bb->Stmtlist());
  FOR_ALL_NODE (stmt, iter, Init()) {
    // find STID or ISTORE
    if (stmt->Opr() != OPR_STID && stmt->Opr() != OPR_ISTORE) {
      continue;
    }
    CODEREP *rhs = stmt->Rhs();
    // find OPR_ADD
    if (rhs->Kind() != CK_OP || rhs->Opr() != OPR_ADD) {
      continue;
    }
    CODEREP *op0 = rhs->Opnd(0);
    CODEREP *op1 = rhs->Opnd(1);
    if (op0->Kind() == CK_OP && (op0->Opr() == OPR_CVT || op0->Opr() == OPR_CVTL)) {
      op0 = op0->Opnd(0);
    }
    if (op1->Kind() == CK_OP && (op1->Opr() == OPR_CVT || op1->Opr() == OPR_CVTL)) {
      op1 = op1->Opnd(0);
    }
    CODEREP *another_cr = NULL;
    if (cr == op0) {
      another_cr = op1;
    } else if (cr == op1) {
      another_cr = op0;
    } else {
      continue;
    }
    CODEREP *lhs = stmt->Lhs();
    if (lhs->Kind() == CK_VAR && another_cr->Kind() == CK_VAR) {
      if (lhs->Field_id() == another_cr->Field_id()) {
        ST *lhs_st = opt_stab->Aux_stab_entry(lhs->Aux_id())->St();
        ST *another_st = opt_stab->Aux_stab_entry(another_cr->Aux_id())->St();
        if (ST_st_idx(lhs_st) == ST_st_idx(another_st)) {
          return lhs;
        }
      }
    } else if (lhs->Kind() == CK_IVAR && another_cr->Kind() == CK_IVAR) {
      if (lhs->I_field_id() == another_cr->I_field_id() &&
        lhs->Ilod_base() && lhs->Ilod_base()->Kind() == CK_VAR &&
        another_cr->Ilod_base() && another_cr->Ilod_base()->Kind() == CK_VAR) {
        ST *lhs_base_st = opt_stab->Aux_stab_entry(lhs->Ilod_base()->Aux_id())->St();
        ST *another_base_st = opt_stab->Aux_stab_entry(another_cr->Ilod_base()->Aux_id())->St();
        if (ST_st_idx(lhs_base_st) == ST_st_idx(another_base_st)) {
          return lhs;
        }
      }
    }
  }
  return NULL;
}


// =============================================================================
//
// RBC_BASE::Eval__is_null_term_str
// bool Is_null_term_str(void *var, uint64 len)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_null_term_str(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 len = Eval__exp(rbc_ctx, arg1);
  if (var != NULL) {
    CONTEXT_SWITCH context(dna);
    TY *ty = Get_cr_ty(vsa, var);
    TYPE_ID mtype = MTYPE_UNKNOWN;
    if (ty != NULL) {
      if (TY_kind(*ty) == KIND_POINTER) {
        TY_IDX ptr_ty = TY_pointed(*ty);
        if (TY_kind(ptr_ty) == KIND_ARRAY) {
          mtype = TY_mtype(TY_etype(ptr_ty));
        }
        else if (TY_kind(ptr_ty) == KIND_SCALAR) {
          mtype = TY_mtype(ptr_ty);
        }
      } // end KIND_POINTER
      else if (TY_kind(*ty) == KIND_ARRAY) {
        mtype = TY_mtype(TY_etype(*ty));
      } // end KIND_ARRAY
      if (mtype == MTYPE_I1) {
        UINT64 size = Get_strlen(vsa, var, rbc_ctx.Stmt(), rbc_ctx.Mem_pool());
        if (size + 1 < len)
          ret = TRUE;
      }
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_null_term_str(cr%d, %lld) = %lld\n",
                       var ? var->Coderep_id() : -1, len, ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_cst_str_eq
// bool Is_const_str_eq(const char *cst1, const char *cst2)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_cst_str_eq(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  const char *cst1 = (char*) Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  const char *cst2 = (char*) Eval__exp(rbc_ctx, arg1);
  if(cst1 != NULL && cst2 != NULL) {
    ret = !strcmp(cst1, cst2);
  } else if(cst1 == NULL && cst2 == NULL) {
    ret = TRUE; // "equal" for both null
  }

  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, spos_pool, vsa), spos_pool);
  srcpos_h->Set_orig_stname(cst1 ? cst1 : "null");
  Plist_true()->push_back(srcpos_h);
  Is_Trace(Tracing(), (TFile, "RBC: Is_const_str_eq(\"%s\", \"%s\") = %lld\n",
                       cst1 ? cst1 : "null", cst2 ? cst2 : "null", ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_str_eq
// bool Is_str_eq(char *var, const char *cst)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_str_eq(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(var != NULL, ("RBC ERROR: null var in Is_str_eq\n"));
  Is_True_Rbc(var->Kind() == CK_LDA || var->Kind() == CK_VAR || var->Kind() == CK_IVAR,
    ("RBC ERROR: invalid var kind in Is_str_eq\n"));
  char *cst = NULL;
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa, call_stmt, var, str_set, rbc_ctx.Mem_pool());
  if (found) {
    CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
    cst = (char*) Eval__exp(rbc_ctx, arg1);
    Is_True_Rbc(cst != NULL, ("RBC ERROR: null cst in Is_str_eq\n"));
    STR_SET::iterator str_it;
    for (str_it = str_set.begin(); str_it != str_set.end(); str_it++) {
      if (strcmp(*str_it, cst) == 0) {
        ret = TRUE;
        break;
      }
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC: Is_str_eq: can't find string, cr%d\n",
                         var->Coderep_id()));
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_str_eq(cr%d, \"%s\") returns %lld.\n",
                       var->Coderep_id(), cst ? cst : "NULL", ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_str_sub
// bool Is_str_sub(char *var, const char *sub)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_str_sub(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(var != NULL, ("RBC ERROR: null var in Is_str_sub\n"));
  Is_True_Rbc(var->Kind() == CK_LDA || var->Kind() == CK_VAR || var->Kind() == CK_IVAR,
    ("RBC ERROR: invalid var kind in Is_str_sub\n"));
  char *sub = NULL;
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa, call_stmt, var, str_set, rbc_ctx.Mem_pool());
  if (found) {
    CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
    sub = (char*) Eval__exp(rbc_ctx, arg1);
    Is_True_Rbc(sub != NULL, ("RBC ERROR: null cst in Is_str_sub\n"));
    STR_SET::iterator str_it;
    for (str_it = str_set.begin(); str_it != str_set.end(); str_it++) {
      if (strstr(*str_it, sub) != NULL) {
        ret = TRUE;
        break;
      }
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC: Is_str_sub: can't find string, cr%d\n", var->Coderep_id()));
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_str_sub(cr%d, \"%s\"), compare result: %lld.\n",
                       var->Coderep_id(), sub ? sub : "NULL", ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_str_match
// bool Is_str_match(char *var, const char *pattern)
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_str_match(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  BOOL ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *var = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(var != NULL, ("RBC ERROR: null var in Is_str_match\n"));
  Is_True_Rbc(var->Kind() == CK_LDA || var->Kind() == CK_VAR || var->Kind() == CK_IVAR,
              ("RBC ERROR: invalid var kind in Is_str_match\n"));

  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *pattern = (char *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(pattern, ("RBC ERROR: null pattern string in Is_str_match"));

  ret = Is_str_match(vsa, rna, var, pattern, rbc_ctx.Mem_pool());
  return ret;
}

BOOL
RBC_BASE::Is_str_match(VSA *vsa_ctx, RNA_NODE *caller_rna, CODEREP *var, char *pattern, MEM_POOL *pool)
{
  BOOL ret = FALSE;
  if(!pattern) {
    return ret;
  }
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa_ctx, caller_rna->Callstmt(), var, str_set, pool);
  if(found) {
    regex_t reg_exp;
    regmatch_t dummy[1];
    int reti = 0;
    reti = regcomp(&reg_exp, pattern, REG_EXTENDED | REG_NOSUB | REG_NEWLINE);
    if (reti) {
      Is_Trace(Tracing(), (TFile, "RBC: Is_str_match: Compile regular expression error.\n"));
      regfree(&reg_exp);
      return ret;
    }
    STR_SET::iterator str_it;
    for (str_it = str_set.begin(); str_it != str_set.end(); str_it++) {
      char *cmp_str = *str_it;
      Is_Trace(Tracing(), (TFile, "RBC: Is_str_match: Trying match String %s with pattern %s.\n",
                          cmp_str, pattern));

      reti = regexec(&reg_exp, cmp_str, 1, dummy, 0);
      if (!reti) {
        ret = TRUE;
        Is_Trace(Tracing(), (TFile, "RBC: Is_str_match: String '%s' match with '%s'.\n", cmp_str, pattern));
        break;
      } else {
        if (reti == REG_NOMATCH) {
          Is_Trace(Tracing(), (TFile, "RBC: Is_str_match String '%s' not match with '%s'.\n", cmp_str, pattern));
        } else {
          char msgbuf[128];
          regerror(reti, &reg_exp, msgbuf, sizeof(msgbuf));
          Is_Trace(Tracing(), (TFile, "RBC: Is_str_match: Regular expression match failed, %s.\n", msgbuf));
        }
      }
    }
    regfree(&reg_exp);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_str_match(cr%d, \"%s\") returns %d.\n",
                       var->Coderep_id(), pattern, ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_parm_constant
// BOOL Is_parm_constant(OBJECT obj)
// @obj: Get_arg
// =============================================================================
UINT64
RBC_BASE::Eval__is_parm_constant(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_parm_constant: Enter.\n"));
  // Get args from stmt
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(rbc_ctx.Caller()));
  CODEREP *obj = (CODEREP*) Eval__exp(rbc_ctx, arg0);
  if (obj == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_parm_constant: Obj is null.\n"));
    return 0;
  }
  if (obj->Kind() == CK_CONST) {
    Is_Trace(Tracing(),
             (TFile,
              "RBC_BASE::Is_parm_constant: Find sensitive cr def by constant, sr: sr%d, cr: cr%d, const value: %lld.\n",
              stmt->Stmtrep_id(), obj->Coderep_id(), obj->Const_val()));
    return 1;
  }
  return 0;
}

// =============================================================================
//
// RBC_BASE::Eval__is_parm_plain_old_func
// BOOL Is_parm_plain_old_func(OBJECT obj)
// @obj: Get_arg
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_parm_plain_old_func(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Is_parm_plain_old_func.\n"));
  // signal handler can be -1, 0, 1
  if (arg->Kind() == CK_CONST || arg->Kind() != CK_LDA)
    return ret;
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Is_parm_plain_old_func.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = ipsa->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Is_parm_plain_old_func.\n"));
  // check if fun name is C++ mangled name or PU has eh region
  if (PU_has_region(*(arg_dna->Pu()))) {
    ret = 1;
  } else {
    char *fname = arg_dna->Fname();
    if (fname != NULL && fname[0] == '_' && fname[1] == 'Z') {
      char *dfname = Vsa_demangle(fname);
      if (strcmp(fname, dfname) != 0)
        ret = 1;
      free(dfname);
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: Is_parm_plain_old_func(\"%s\"):%lld\n", arg_dna->Fname(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__is_init_by_const_str
// BOOL Is_init_by_const_str(OBJECT obj)
// @obj: Get_arg
// =============================================================================
UINT64
RBC_BASE::Eval__is_init_by_const_str(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_init_by_const_str: Enter.\n"));
  // Get args from stmt
  DNA_NODE *dna = rbc_ctx.Caller();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*) Eval__exp(rbc_ctx, arg0);
  if (obj == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_init_by_const_str: Obj is null.\n"));
    return 0;
  }
  vector<SRCPOS_HANDLE *> sp_vector;
  Find_var_init_by_const_str(dna, call_stmt, obj, sp_vector);
  if (sp_vector.size() > 0) {
    for (INT i = 0; i < sp_vector.size(); i++) {
      Plist_false()->push_back(sp_vector[i]);
    }
  }
  return 0;
}

void
RBC_BASE::Find_var_init_by_const_str(DNA_NODE *dna, STMTREP *sr, CODEREP *cr, vector<SRCPOS_HANDLE *> &sp_vector)
{
  VSA *vsa_ctx = dna->Comp_unit()->Vsa();
  CONTEXT_SWITCH ctx(dna);
  VAR_DEF_HELPER helper(cr, sr, vsa_ctx->Comp_unit(), FOR_GENERAL);
  helper.Set_follow_ctr_ud(TRUE);
  helper.Set_def_srcpos_cand_on(TRUE);
  helper.Set_srcpos_on(TRUE);
  CHECK_OBJ check_obj(cr, sr);
  vsa_ctx->Var_def_trav_helper(&helper, check_obj);
  SRCPOS_HANDLE *sp_h = helper.Srcpos();
  char *buf = (char *) MEM_POOL_Alloc(Mem_pool(), VSA_VAR_NAME_MAX_LEN);
  STRING_BUFFER sbuf(buf, VSA_VAR_NAME_MAX_LEN);
  const char *name = SRCPOS_HANDLE::Find_cr_stname(&sbuf, cr, sr, dna);
  if (name != NULL) {
    sp_h->Set_orig_stname(name);
  }
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  for (INT i = 0; i < def_info_vec.size(); i++) {
    DEF_INFO *def_info = def_info_vec[i];
    DNA_NODE *def_dna = def_info->Dna();
    CODEREP *def_cr = def_info->Coderep();
    Is_True_Ret(def_dna != NULL && def_cr != NULL,
      ("RBC_BASE::Find_var_init_by_const_str: def dna is null or def cr is null, def dna: %p, def cr: %p.",
        def_dna, def_cr));
    SRCPOS_TREENODE *spos_node = def_info->Spos_node();
    IDTYPE spos_id = def_info->Spos_id();
    Is_True_Ret(spos_node != NULL, ("RBC_BASE::Find_var_init_by_const_str: spos node is NULL."));
    char *str = NULL;
    {
      CONTEXT_SWITCH def_ctx(def_dna);
      str = Find_const_char(def_dna, def_cr);
    }
    if (str != NULL) {
      sp_h->Reset_cur_node(spos_node, spos_id);
      Is_Trace(Tracing(),
        (TFile, "RBC_BASE::Is_init_by_const_str: Find sensitive cr def by const string, sr: sr%d, cr: cr%d, variable name: %s.",
          sr->Stmtrep_id(), cr->Coderep_id(), name == NULL ? "" : name));
      sp_vector.push_back(sp_h->Clone(Mem_pool()));
    }
  }
}

// =============================================================================
//
// RBC_BASE::Eval__implicit_call
// BOOL Implicit_call(OBJECT obj, char *sig, OBJECT parm1)
// call obj->sig(parm1)
// @obj: is from caller, to get the class name
// @sig: function signature
// @parm1: parameter passed to function
// =============================================================================
UINT64
RBC_BASE::Eval__implicit_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  // Get args from stmt
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*) Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(obj != NULL, ("RBC ERROR: null obj passed to implicit call"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char* sig_tmp = (char *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(sig_tmp, ("RBC_BASE::Eval__implicit_call: Can't find sig"));
  char *sig = (char *) malloc(strlen(sig_tmp) + 1);
  strcpy(sig, sig_tmp);
  // param1 is directly get from callee context, no need to eval
  CODEREP *parm1 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));

  // Connect callee with the implicit dna:
  // Create a new RNA, add it to callee's call list
  // add implicit dna to rna callee
  Is_True_Rbc(callee && callee->Non_functional(), ("callee should be non-function node"));
  callee->Set_rbc_flag(DNA_RBC_SE_EVAL);
  RNA_NODE *rna = NULL;
  // get class name from object type, get implicit call dna/st by class name + sig
  {
    CONTEXT_SWITCH caller_ctx(dna);
    TY_IDX obj_ty = obj->object_ty();
    Is_True_Rbc(obj_ty &&
                TY_kind(obj_ty) == KIND_POINTER &&
                TY_kind(TY_pointed(obj_ty)) == KIND_STRUCT,
                ("RBC_BASE:implicit_call obj type should be ptr to a class"));
    TY_IDX pointed = TY_pointed(obj_ty);
    char *cls_name = TY_name(pointed);
    // VIRFUNC_INFO_VEC *meth_vec = CXX_NEW(VIRFUNC_INFO_VEC(VIRFUNC_INFO_VEC::allocator_type(pool)), pool);
    std::vector<VIRFUNC_INFO *> meth_vec;
    ipsa->Glob_cha()->Find_candidate_functions_in_subclasses(cls_name, sig, meth_vec);
    std::vector<VIRFUNC_INFO *>::iterator iter;
    for (iter = meth_vec.begin(); iter != meth_vec.end(); ++iter) {
      VIRFUNC_INFO *fun = *iter;
      ST *fun_st = St_ptr(fun->File_idx(), fun->Fun_st());
      DNA_NODE *implicit_dna = ipsa->Get_dna(fun->File_idx(), fun_st);
      if (implicit_dna) {
        if (!rna)
          rna = ipsa->New_rna(callee, stmt);
        rna->Add_callee(implicit_dna->Dna_idx(), implicit_dna->Flags());
        implicit_dna->Inc_ref_cnt(rna);
        Is_Trace(Tracing(), (TFile, "RBC_BASE:Eval__implicit_call find implicit callee, caller : %d, callee : %d.\n", callee->Dna_idx(), implicit_dna->Dna_idx()));
      }
    }
  }
  if (rna) {
    rna->Set_flag(RBC_SE_IMPLICIT_CALL);
    rna->Enter_arg_list(parm1, ipsa);
    if (!callee->Non_functional())
      callee->Comp_unit()->Vsa()->Enter_sr_rna_map(stmt, rna);
  } else {
      Is_Trace(Tracing(), (TFile, "RBC_BASE:Eval__implicit_call can't find imlicit callee.\n"));
  }
  if (sig)
    free(sig);
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__call_super
// Call_super()
//
// =============================================================================
UINT64
RBC_BASE::Eval__call_super(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CONTEXT_SWITCH caller(dna);
  if(call_stmt->Opr() == OPR_CALL && OPERATOR_has_sym(call_stmt->Opr())) {
    CODEREP *obj = call_stmt->Rhs()->Opnd(0);
    TY_IDX obj_ty = obj->object_ty();
    Is_True_Rbc(TY_kind(obj_ty) == KIND_POINTER, ("RBC_BASE:Call_super obj type should be ptr"));
    TY_IDX pointed = TY_pointed(obj_ty);
    ST *call_st = call_stmt->St();
    const char *fun = ST_name(call_st);
    CLASS_HIERARCHY *cha = ipsa->Glob_cha();
    STRING_BUFFER buf(strlen(fun)+1);
    const char *class_name = cha->Extract_class_name(fun, &buf);
    if(cha && class_name) {
      return cha->Is_base_class(class_name, TY_name(pointed)) ? 1 : 0;
    }
  }
  return 0;
}

// =============================================================================
//
// RBC_BASE::Eval__func_invoked_by_subclass
// Func_invoked_by_subclass(String class_name)
//
// =============================================================================
UINT64
RBC_BASE::Eval__func_invoked_by_subclass(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char* class_name = (char *)Eval__exp(rbc_ctx, arg0);
  CONTEXT_SWITCH caller(dna);
  const char *caller_name = dna->Fname();
  CLASS_HIERARCHY *cha = ipsa->Glob_cha();
  STRING_BUFFER buf(strlen(caller_name) + 1);
  const char *caller_cls_name = cha->Extract_class_name(caller_name, &buf);
  if(caller_cls_name != NULL) {
    return cha->Is_base_class(class_name, caller_cls_name);
  } else {
    return 0;
  }
  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__is_obj_meth_override
// Is_obj_meth_override(OBJECT obj, const String meth signature)
// Check if obj's method with given signature override base class's method
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_obj_meth_override(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  IPSA *ipsa = rbc_ctx.Ipsa();
  Is_Trace(Tracing(), (TFile, "%sRBC_BASE:Eval__is_obj_meth_override in %s\n%s",
                       DBar, dna->Fname(), DBar));
  UINT64 ret = FALSE;
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(obj != NULL, ("RBC ERROR: null obj passed to Is_obj_meth_override\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  char *sig_tmp = (char *) Eval__exp(rbc_ctx, arg1);
  Is_True_Rbc(sig_tmp, ("RBC_BASE::Eval__is_obj_meth_override: Can't find meth signature"));
  char *sig = (char *) malloc(strlen(sig_tmp) + 1);
  strcpy(sig, sig_tmp);
  // U-D traverse to find the obj def type
  Is_Trace(Tracing(),
           (TFile, "%sRBC_BASE:Eval__is_obj_meth_override: Find var def for obj:\n%s",
            SBar, SBar));
  CONTEXT_SWITCH caller(dna);
  VAR_DEF_HELPER helper(obj, call_stmt, vsa->Comp_unit());
  CHECK_OBJ check_obj(obj, call_stmt);
  vsa->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  Is_Trace(Tracing(),
           (TFile, "%sRBC_BASE:Eval__is_obj_meth_override:End of Find var def for obj:\n%s",
            SBar, SBar));

  DNA_NODE *def_dna = NULL;
  CODEREP *def_cr = NULL;
  STMTREP *def_stmt = NULL;
  // there's no guarantee that we can find defs for a cr, especially for Java.
  if (def_info_vec.size() == 0) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__is_obj_meth_override: no def info found for cr%d\n",
                         obj->Coderep_id()));
    if (sig)
      free(sig);
    return ret;
  }
  char msg[512];
  for(int i = 0; i < def_info_vec.size(); i++) {
    def_dna = def_info_vec[i]->Dna();
    def_cr = def_info_vec[i]->Coderep();
    Is_True_Rbc(def_dna && def_cr,
                ("RBC ERROR: Eval__is_obj_meth_override invalid def_dna, def_cr"));
    CONTEXT_SWITCH def_ctx(def_dna);
    CLASS_HIERARCHY *cha = ipsa->Glob_cha();
    if(def_cr->Kind() == CK_OP) {
      if(def_cr->Opr() == OPR_INTRINSIC_CALL &&
         def_cr->Intrinsic() == INTRN_ALLOC_OBJ) {
        Is_True_Rbc(def_cr->Opnd(0)->Kind() == CK_IVAR &&
                    def_cr->Opnd(0)->Opr() == OPR_PARM &&
                    def_cr->Opnd(0)->Ilod_base()->Kind() == CK_LDA,
                    ("RBC ERROR: bad first kid in INTRN_ALLOC_OBJ"));
        CODEREP *cls_cr = def_cr->Opnd(0)->Ilod_base();
        ST *st = cls_cr->Lda_base_st();
        if(st && ST_is_class_symbol(st)) {
          TY_IDX def_ty_idx = ST_class_symbol_ty_idx(ST_st_idx(st));
          char *cls_name = TY_name(def_ty_idx);
          Is_Trace(Tracing(), (TFile, "**Found class: %s\n", cls_name));
          VIRFUNC_INFO *vfunc = cha->Get_meth_by_sig(cls_name, sig);
          if(vfunc) {
            // meth is defined in class, check if parent also define the method
            INT32 off = vfunc->Ofst();
            C_STR_VEC *parents = cha->Get_parents(cls_name);
            if(parents != NULL) {
              for(int i = 0; i < parents->size() && !ret; i++) {
                C_STR parent = (*parents)[i];
                // java doesn't have multi vptr, set vptr_ofst to 0
                VIRFUNC_INFO *pvfunc = cha->Get_vtable_entry(parent, off, 0);
                if(pvfunc) {
                  Is_Trace(Tracing(),
                           (TFile, "**Parent %s define method %s, Mark override: Y\n",
                            parent, sig));
                  char *fname = Vsa_demangle(ST_name(vfunc->File_idx(), vfunc->Fun_st()));
                  sprintf(msg, "Object's method %s override base class's method", fname);
                  if(fname) {
                    free(fname);
                  }
                  ret = TRUE;
                  break;
                } else {
                  Is_Trace(Tracing(),
                           (TFile, "**Parent %s not define method %s, Mark override: N\n",
                            parent, sig));
                }
              }
            } else {
              Is_Trace(Tracing(),
                       (TFile, "**No parents for class %s, Mark override: N\n",
                        cls_name));
            }
          } else {
            Is_Trace(Tracing(),
                     (TFile, "**Unable to find vtable for class %s sig %s, Mark override: N\n",
                      cls_name, sig));
          }
        }
      } else {
        if(def_dna->Is_param(def_cr)) {
          Is_Trace(Tracing(),
                   (TFile, "**Obj is define by input entry chi, Mark override: Y, May\n"));
          sprintf(msg, "Object is defined by input parameter, may override method %s", sig);
          ret = TRUE;
        } else {
          // not def as we expected, unknown
          Is_Trace(Tracing(), (TFile, "RBC ERROR: method override cr%d is not param.\n",
                               def_cr->Coderep_id()));
        }
      }
    } else {
      Is_Trace(Tracing(), (TFile, "RBC ERROR: method override cr%d kind-%d not supported yet.\n",
                           def_cr->Coderep_id(), def_cr->Kind()));
    }
    if(ret) {
      CONTEXT_SWITCH caller(dna);
      SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(obj, call_stmt, dna, spos_pool, vsa), spos_pool);
      srcpos_h->Add_message(msg);
      Plist_false()->push_back(srcpos_h);
    }
  }
  Is_Trace(Tracing(),
           (TFile, "%sRBC_BASE::Eval__is_obj_meth_override RET %lld\n%s",
            DBar, ret, DBar));
  if (sig)
    free(sig);
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__do_not_get_called & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__do_not_get_called(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = FALSE;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Do_not_get_called.\n"));
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(call_stmt->Rhs(), call_stmt, dna, spos_pool), spos_pool);
  srcpos_h->Set_orig_stname(callee->Fname());
  Plist_false()->push_back(srcpos_h);
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__do_not_call & helper functions
//   rule evaluated on dna
// BOOL Do_not_call(CONST STRING function_name)
// =============================================================================
UINT64
RBC_BASE::Eval__do_not_call(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *enclosing_dna = rbc_ctx.Callee();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(enclosing_dna));
  Is_True_Rbc(arg1 != NULL, ("Do_not_call not set function name"));
  char* fname = Find_const_char(enclosing_dna, arg1);
  Is_True_Rbc(fname != NULL, ("Do_not_call not set const function name"));

  CONTEXT_SWITCH ctx(enclosing_dna);
  STMTREP *entry_stmt = enclosing_dna->Comp_unit()->Vsa()->Get_entry_chi_stmt();
  Is_True_Rbc(entry_stmt, ("null entry chi stmt for function %s", enclosing_dna->Fname()));
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, entry_stmt, enclosing_dna,
                                                  rbc_ctx.Spos_pool()),
                                    rbc_ctx.Spos_pool());
  char *demangle_fname = Vsa_demangle((char *)fname);
  STRING cloned_fname = Clone_string(demangle_fname, rbc_ctx.Mem_pool());
  srcpos_h->Set_orig_stname(cloned_fname);
  if (demangle_fname) {
    free(demangle_fname);
  }
  hash_set<IDTYPE> visited_dna;
  ret = Do_not_call(enclosing_dna, fname, srcpos_h, &visited_dna);
  Is_Trace(Tracing(), (TFile, "Eval__do_not_call(%s) returns %lld\n", fname, ret));
  return ret;
}

BOOL
RBC_BASE::Do_not_call(DNA_NODE *dna, char *fname, SRCPOS_HANDLE *srcpos_h, hash_set<IDTYPE> *visited_dna)
{
  Is_True_Rbc(dna, ("Do_not_call: null dna"));
  BOOL ret = TRUE;
  if (visited_dna->find(dna->Dna_idx()) != visited_dna->end()) {
    return ret;
  }
  if (srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip Do_not_call(%s) spos reach limit(%d) processing func(%s)\n",
                         fname, srcpos_h->Children_count(), dna->Fname()));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return ret;
  }
  visited_dna->insert(dna->Dna_idx());
  CONTEXT_SWITCH ctx(dna);
  srcpos_h->Add_children(dna->Call_list()->size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  INT parent_idx = srcpos_h->Cur_idx();
  for (INT i = VAR_INIT_ID; i < dna->Call_list()->size(); ++i) {
    srcpos_h->Set_cur_node(cur_node, i - 1);
    RNA_NODE *rna = (*dna->Call_list())[i];
    Is_True(rna && rna->Callstmt(), ("rna or callstmt invalid"));
    STMTREP *stmt = rna->Callstmt();
    if (stmt->Opr() == OPR_CALL && strcmp(ST_name(stmt->St()), fname) == 0) {
      srcpos_h->Append_data(stmt, dna, PATHINFO_DNA_CALLSITE);
      Plist_false()->push_back(srcpos_h->Clone()->Clone());
      ret = FALSE;
      continue;
    }
    const CALLEE_VECTOR& callee_list = rna->Callee_list();
    for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
      DNA_NODE *callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(iter->Callee());
      if (callee && !callee->Non_functional()) {
        srcpos_h->Append_data(stmt, dna, PATHINFO_DNA_CALLSITE);
        if (strcmp(callee->Fname(), fname) == 0) {
          Plist_false()->push_back(srcpos_h->Clone()->Clone());
          ret = FALSE;
          break;
        } else {
          if (!Do_not_call(callee, fname, srcpos_h, visited_dna)) {
            ret = FALSE;
          }
        }
      }
    }
  }
  srcpos_h->Reset_cur_node(cur_node, parent_idx);
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__is_parm_type_addr_passed check if parameter with type_name is
// passed by address
//   Is_parm_type_addr_passed(const char *type_name)
//
//
// =============================================================================
UINT64
RBC_BASE::Eval__is_parm_type_addr_passed(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = TRUE;
  DNA_NODE *enclosing_dna = rbc_ctx.Callee();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(enclosing_dna));
  Is_True_Rbc(arg1 != NULL, ("Is_type_addr_passed not set type name"));
  char* type_name = Find_const_char(enclosing_dna, arg1);
  Is_True_Rbc(type_name != NULL, ("Is_type_addr_passed not set const type name"));

  CONTEXT_SWITCH ctx(enclosing_dna);
  STMTREP *entry_stmt = enclosing_dna->Comp_unit()->Vsa()->Get_entry_chi_stmt();
  Is_True_Rbc(entry_stmt, ("null entry chi stmt for function %s", enclosing_dna->Fname()));

  for (INT i = VAR_INIT_ID; i < enclosing_dna->Parm_list()->size(); i++) {
    VAR_NODE* parm_node = enclosing_dna->Parm_list()->at(i);
    Is_True_Rbc(parm_node, ("null parm_node"));
    ST_IDX parm_st = parm_node->St_idx();
    TY_IDX parm_type = ST_type(parm_st);
    BOOL is_pointer = FALSE;
    while (TY_kind(parm_type) == KIND_POINTER) {
      is_pointer = TRUE;
      parm_type = TY_pointed(parm_type);
    }
    char *parm_ty_name = TY_name(parm_type);
    if (strcmp(type_name, parm_ty_name) == 0) {
      SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, entry_stmt, enclosing_dna,
                                                      rbc_ctx.Spos_pool()),
                                        rbc_ctx.Spos_pool());
      srcpos_h->Set_orig_stname(ST_name(parm_st));
      if (is_pointer) {
        Plist_true()->push_back(srcpos_h);
      } else {
        Plist_false()->push_back(srcpos_h);
        ret = FALSE;
      }
    }
  }
  Is_Trace(Tracing(), (TFile, "Eval__is_type_addr_passed(%s) returns %lld\n", type_name, ret));
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__do_not_access_shared_obj & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__do_not_access_shared_obj(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Do_not_access_shared_obj(void *func)
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Do_not_access_shared_obj.\n"));
  // signal handler can be -1, 0, 1
  if (arg->Kind() == CK_CONST || arg->Kind() != CK_LDA)
    return 1;
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Do_not_access_shared_obj.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = ipsa->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Do_not_access_shared_obj.\n"));
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(arg, call_stmt, dna, spos_pool, vsa), spos_pool);
  vector<IDTYPE> path;
  path.clear();
  UINT64 ret = 1;
  {
    CONTEXT_SWITCH context(arg_dna);
    ret = Do_not_access_shared_obj(arg_dna->Comp_unit()->Vsa(), arg_dna->Comp_unit()->Cfg()->Entry_bb(), srcpos_h, &path);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Do_not_access_shared_obj(\"%s\"): %lld\n",
                       arg_dna->Fname(), ret));
  return ret;
}


BOOL
RBC_BASE::Do_not_access_shared_obj(VSA *vsa_ctx, BB_NODE *bb, SRCPOS_HANDLE *srcpos_h, vector<IDTYPE> *path)
{
  Is_True_Rbc(bb != NULL, ("RBC ERROR: null bb passed to Do_not_access_shared_obj.\n"));
  Is_True_Rbc(srcpos_h != NULL, ("RBC ERROR: null srcpos_h passed to Do_not_access_shared_obj.\n"));
  BOOL ret = TRUE;
  for (vector<IDTYPE>::iterator iter = path->begin(); iter != path->end(); iter++) {
    if (*iter == bb->Id())
      return ret;
  }
  Is_Trace(Tracing(), (TFile,
                       "@@@@@@@@@@ visit BB%d\n", bb->Id()));
  path->push_back(bb->Id());
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    Is_Trace(Tracing(), (TFile,
                         "---------- visit stmt%d\n",
                         stmt->Stmtrep_id()));
    if (Contains_shared_obj(vsa_ctx, stmt, srcpos_h)) {
      ret = FALSE;
      return ret;
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (!Do_not_access_shared_obj(vsa_ctx, succ_bb, srcpos_h, path)) {
      ret = FALSE;
      break;
    }
  }
  path->pop_back();
  return ret;
}


BOOL
RBC_BASE::Contains_shared_obj(VSA *vsa_ctx, STMTREP *stmt, SRCPOS_HANDLE *srcpos_h)
{
  Is_True_Rbc(stmt != NULL, ("RBC ERROR: null stmt passed to Contains_shared_obj.\n"));
  BOOL ret = FALSE;
  srcpos_h->Append_data(stmt, vsa_ctx->Dna(), PATHINFO_CD_BB);
  OPERATOR opr = stmt->Opr();
  if (opr == OPR_PRAGMA || opr == OPR_GOTO ||
      opr == OPR_GOTO || opr == OPR_LABEL ||
      opr == OPR_RETURN || opr == OPR_RETURN_VAL ||
      opr == OPR_OPT_CHI) {
    ret = FALSE;
  }
  else if (opr == OPR_STID) {
    CODEREP *lhs = stmt->Lhs();
    if (Contains_shared_obj(vsa_ctx, lhs, srcpos_h))
      ret = TRUE;
    else {
      CODEREP *rhs = stmt->Rhs();
      if (Contains_shared_obj(vsa_ctx, rhs, srcpos_h))
        ret = TRUE;
    }
  }
  else if (opr == OPR_MSTORE) {
    CODEREP *lhs = stmt->Lhs()->Istr_base();
    if (Contains_shared_obj(vsa_ctx, lhs, srcpos_h))
      ret = TRUE;
    else {
      CODEREP *rhs = stmt->Rhs();
      if (Contains_shared_obj(vsa_ctx, rhs, srcpos_h))
        ret = TRUE;
    }
  }
  else if (OPERATOR_is_call(opr)) {
    char *fname = stmt->St() ? ST_name(stmt->St())
                             : NULL;
    if (fname && Is_lib_func(fname)) {
      ret = FALSE;
    }
    else {
      CODEREP *rhs = stmt->Rhs();
      for (INT actual = 0; actual < rhs->Kid_count(); ++ actual) {
        if (opr == OPR_ICALL && actual == rhs->Kid_count() - 1)
          break;
        Is_True_Rbc(rhs->Opnd(actual)->Kind() == CK_IVAR &&
                    rhs->Opnd(actual)->Opr() == OPR_PARM,
                    ("RBC ERROR: wrong param cr in Contains_shared_obj.\n"));
        CODEREP* opnd = rhs->Opnd(actual)->Ilod_base();
        if (Contains_shared_obj(vsa_ctx, opnd, srcpos_h)) {
          ret = TRUE;
          break;
        }
      }
    }
  }
  else if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
    if (OPERATOR_is_compare(stmt->Rhs()->Opr())) {
      CODEREP *lhs = stmt->Rhs()->Opnd(0);
      if (Contains_shared_obj(vsa_ctx, lhs, srcpos_h))
        ret = TRUE;
      else {
        CODEREP *rhs = stmt->Rhs()->Opnd(1);
        if (Contains_shared_obj(vsa_ctx, rhs, srcpos_h))
          ret = TRUE;
      }
    }
  }
  else
    Is_Trace(Tracing(),
             (TFile,
              "RBC ERROR: OPERATOR:%d not implemented yet in Contains_shared_obj.\n",
              opr));
  srcpos_h->Remove_last();
  return ret;
}


BOOL
RBC_BASE::Contains_shared_obj(VSA *vsa_ctx, CODEREP *cr, SRCPOS_HANDLE *srcpos_h)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Contains_shared_obj.\n"));
  BOOL ret = TRUE;
  ST *st = Get_cr_st(vsa_ctx, cr);
  if (st)
    srcpos_h->Append_data(st, NULL, vsa_ctx->Dna(), PATHINFO_ST_DECLARE);

  if (cr->Is_var_volatile())
    ret = FALSE;
  else if (cr->Kind() == CK_CONST)
    ret = FALSE;
  else if (st && (ST_sclass(st) == SCLASS_AUTO ||
                  ST_sclass(st) == SCLASS_REG))
    ret = FALSE;
  else if (st && (ST_class(st) == CLASS_FUNC ||
                  ST_class(st) == CLASS_NAME ||
                  ST_class(st) == CLASS_CONST))
    ret = FALSE;
  else if (st && (strcmp(ST_name(st), "stdin") == 0 ||
                  strcmp(ST_name(st), "stdout") == 0 ||
                  strcmp(ST_name(st), "stderr") ==0))
    ret = FALSE;

  if (ret)
    Plist_false()->push_back(srcpos_h->Clone());

  if (st)
    srcpos_h->Remove_last();
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__do_not_call_sleep_in_atm & helper functions
//
// =============================================================================
UINT64
RBC_BASE::Eval__do_not_call_sleep_in_atm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Do_not_call_sleep_in_atm
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *spos_pool = rbc_ctx.Spos_pool();
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(call_stmt->Rhs(), call_stmt, dna, spos_pool), spos_pool);
  vector<UINT64> path;
  path.clear();
  UINT64 ret = Do_not_call_sleep_in_atm(vsa, call_stmt, call_stmt->Bb(), srcpos_h, &path);
  Is_Trace(Tracing(), (TFile, "RBC: Do_not_call_sleep_in_atm: %lld\n", ret));
  return ret;
}


BOOL
RBC_BASE::Do_not_call_sleep_in_atm(VSA *vsa_ctx, STMTREP *from, BB_NODE *bb,
                                   SRCPOS_HANDLE *srcpos_h, vector<UINT64> *path)
{
  BOOL ret = TRUE;
  BOOL start = FALSE;

  UINT64 key = (UINT64)vsa_ctx->Dna()->Dna_idx() << 32 | bb->Id();
  for (vector<UINT64>::iterator iter = path->begin(); iter != path->end(); iter++) {
    if (*iter == key)
      return TRUE;
  }
  path->push_back(key);

  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  Is_Trace(Tracing(), (TFile, "@@@@@@@@@@ visit \"%s\" BB%d\n",
                       vsa_ctx->Dna()->Fname(), bb->Id()));
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (!start)
      if (stmt == from)
        start = TRUE;
      else
        continue;

    OPERATOR opr = stmt->Opr();
    if (OPERATOR_is_call(opr)) {
      ST *st = NULL;
      if (OPERATOR_has_sym(opr))
        st = stmt->St();
      if (st != NULL) {
        // check atomic region ends
        DNA_NODE *callee = vsa_ctx->Ipsa()->Get_dna(vsa_ctx->Dna()->File_idx(), st);
        if (callee != NULL) {
          if (callee->Is_set(DNA_ATOMIC_REGION_END)) {
            path->pop_back();
            srcpos_h->Remove_last();
            return TRUE;
          }
          else if (callee->Is_set(DNA_MAY_SLEEP)) {
            srcpos_h->Append_data(stmt, vsa_ctx->Dna(), PATHINFO_RBC);
            CONTEXT_SWITCH context(vsa_ctx->Dna());
            SRCPOS_HANDLE *sh = srcpos_h->Clone();
            sh->Set_orig_stname(Vsa_demangle(ST_name(st)));
            Plist_false()->push_back(sh);
            srcpos_h->Remove_last();
            ret = FALSE;
          }
          else if (!callee->Is_set(DNA_ATOMIC_REGION_BEGIN)) {
            srcpos_h->Append_data(stmt, vsa_ctx->Dna(), PATHINFO_DNA_CALLSITE);
            BB_NODE *entry = callee->Comp_unit()->Cfg()->Entry_bb();
            ret = Do_not_call_sleep_in_atm(callee->Comp_unit()->Vsa(),
                                           entry->First_stmtrep(), entry, srcpos_h, path);
            srcpos_h->Remove_last();
            if (callee->Is_set(DNA_ATOMIC_FUNC)) {
              path->pop_back();
              return ret;
            }
          }
        }
      }
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (!Do_not_call_sleep_in_atm(vsa_ctx, succ_bb->First_stmtrep(), succ_bb, srcpos_h, path)) {
      ret = FALSE;
      break;
    }
  }
  path->pop_back();
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__declare_malloc_similar
//
// =============================================================================
UINT64
RBC_BASE::Eval__declare_malloc_similar(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  int parm_idx = Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(parm_idx < callee->Parm_list()->size(),
              ("RBC ERROR: parm_idx %d out of range for %s\n", parm_idx, callee->Fname()));
  call_stmt->Set_call_flags(call_stmt->Call_flags() | WN_CALL_DOES_MEM_ALLOC);
  callee->Set_flag(DNA_MEMORY_ALLOCATION);
  CODEREP *retv = vsa->Comp_unit()->Find_return_value(call_stmt);
  if (retv != NULL)
    retv->Set_value_malloc();
  Is_Trace(Tracing(), (TFile, "RBC: set malloc flag for %s, arg #%d, cr%d\n",
                       callee->Fname(), parm_idx, retv == NULL ? -1 : retv->Coderep_id()));
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__declare_free_similar
//
// =============================================================================
UINT64
RBC_BASE::Eval__declare_free_similar(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  DNA_NODE *callee = rbc_ctx.Callee();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  Is_True_Rbc(callee != NULL, ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                               dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  int parm_idx = Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(parm_idx < callee->Parm_list()->size(),
              ("RBC ERROR: parm_idx %d out of range for %s\n", parm_idx, callee->Fname()));
  call_stmt->Set_call_flags(call_stmt->Call_flags() | WN_CALL_DOES_MEM_FREE);
  BOOL is_ptr;
  CODEREP *actual_arg = rna->Get_arg(parm_idx)->Find_actual_arg();
  IDTYPE_SET visited_set;
  IDTYPE arg_param = dna->Find_param_references(actual_arg, &is_ptr, IN_NONE, visited_set, TRUE);
  if (actual_arg->Kind() == CK_VAR && arg_param != INVALID_VAR_IDX) {
    callee->Set_flag(DNA_MEMORY_DEALLOCATION);
    callee->Set_parm_flag(parm_idx, REF_FREED);
  }
  IDTYPE arg_glob = dna->Find_glob_references(actual_arg, &is_ptr, IN_NONE);
  if (arg_glob != INVALID_VAR_IDX) {
    callee->Set_flag(DNA_MEMORY_DEALLOCATION);
    callee->Set_glob_flag(arg_glob, REF_FREED);
  }
  Is_Trace(Tracing(), (TFile, "RBC: set free flag for %s, arg #%d\n", callee->Fname(), parm_idx));
  return 1;
}


// =============================================================================
//
// RBC_BASE::Eval__get_this_pointer
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_this_pointer(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // void *Get_this_pointer(void);
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  CODEREP *arg = NULL;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
  TAG_INFO *tag_info = rbc_ctx.Tag_info();
  if (tag_info) {
    Is_Trace(Tracing(), (TFile, "RBC: Get_this_pointer: Apply on %s tag_obj[1] ", callee->Fname()));
    CODEREP* tag_ret = Get_tag_obj(rbc_ctx, tag_info, 1);
    Is_Trace(Tracing(), (TFile, "RBC: Get_this_pointer: cr%d\n",
                          tag_ret == NULL ? -1 : tag_ret->Coderep_id()));
    return (UINT64)tag_ret;
  }
  // offset java "this" pointer
  if (rna->Arg_cnt() > 0) {
    Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Get_this_pointer.\n"));
    ST *func_parm_st = Get_this_symbol(callee);
    if(func_parm_st != NULL) {
      ret = ret + 1;
    } else {
      DevWarn("Get_this_pointer can't find parm symbol 'this', fname: %s.\n", callee->Fname());
    }
    arg = rna->Get_arg(ret);
    ret = (UINT64)arg;
  }
  else {
    Is_True_Rbc(FALSE, ("RBC ERROR: invalid call of Get_this_pointer.\n"));
  }
  Is_Trace(Tracing(), (TFile, "RBC: Get_this_pointer: cr%d\n", arg == NULL ? -1 : arg->Coderep_id()));
  return ret;
}

ST*
RBC_BASE::Get_this_symbol(DNA_NODE *dna)
{
  CONTEXT_SWITCH context(dna);
  if (dna->Parm_list()->size() < 2) {
    return NULL;
  }
  VAR_NODE *func_parm = (*dna->Parm_list())[1];
  ST *func_parm_st = ST_ptr(func_parm->St_idx());
  if (func_parm_st != NULL && ST_is_this_ptr(func_parm_st)) {
    return func_parm_st;
  } else {
    return NULL;
  }
}


// =============================================================================
//
// RBC_BASE::Eval__get_arg
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_arg(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // void *Get_arg(int i);
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();

  CODEREP *arg = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null arguement found for Get_arg.\n"));
  if (arg->Kind() == CK_VAR || arg->Kind() == CK_CONST)
    ret = Eval__exp(rbc_ctx, arg);
  else
    return ret;

  Is_True_Rbc(ret != 0, ("RBC ERROR: there's no argument %d in given function.\n", ret));

  TAG_INFO *tag_info = rbc_ctx.Tag_info();
  if (tag_info) {
    Is_Trace(Tracing(), (TFile, "RBC: Get_arg: Apply on %s tag_obj[%lld] ", callee->Fname(), ret));
    CODEREP* tag_ret = Get_tag_obj(rbc_ctx, tag_info, ret);
    Is_Trace(Tracing(), (TFile, "RBC: Get_arg: cr%d\n",
                          tag_ret == NULL ? -1 : tag_ret->Coderep_id()));
    return (UINT64)tag_ret;
  }

  // offset java "this" pointer for other parameters
  if (PU_java_lang(*dna->Pu()) && ret < rna->Arg_cnt()) {
    Is_True_Rbc(callee != NULL, ("RBC ERROR: null callee in Get_arg.\n"));
    CONTEXT_SWITCH context(callee);
    VAR_NODE *func_parm = (*callee->Parm_list())[1];
    ST *func_parm_st = ST_ptr(func_parm->St_idx());
    if (func_parm_st != NULL && ST_is_this_ptr(func_parm_st))
      ret = ret + 1;
  }
  arg = rna->Get_arg(ret);
  Is_Trace(Tracing(), (TFile, "RBC: Get_arg(%lld): cr%d\n", ret, arg == NULL ? -1 : arg->Coderep_id()));
  ret = (UINT64)arg;
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_ret
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_ret(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // void *Get_ret(void);
  // return value of the function call
  RBC_EVAL_SKIP();
  VSA *vsa = rbc_ctx.Caller_vsa();
  RNA_NODE *rna = rbc_ctx.Rna();
  DNA_NODE *callee = rbc_ctx.Callee();
  DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
  TAG_INFO *tag_info = rbc_ctx.Tag_info();
  if (tag_info) {
    Is_Trace(Tracing(), (TFile, "RBC: Get_ret: Apply on tag_obj[0] "));
    CODEREP* tag_ret = Get_tag_obj(rbc_ctx, tag_info, 0);
    Is_Trace(Tracing(), (TFile, "RBC: Get_ret: cr%d\n",
                          tag_ret == NULL ? -1 : tag_ret->Coderep_id()));
    return (UINT64)tag_ret;
  }
  CODEREP *ret = Get_ret(vsa, rna);
  Is_Trace(Tracing(), (TFile, "RBC: Get_ret: cr%d\n",
                       ret == NULL ? -1 : ret->Coderep_id()));
  return (UINT64)ret;
}

CODEREP *
RBC_BASE::Get_ret(VSA *vsa_ctx, RNA_NODE *caller_rna)
{
  CONTEXT_SWITCH context(vsa_ctx->Dna());
  CODEREP *ret = vsa_ctx->Comp_unit()->Find_return_value(caller_rna->Callstmt());
  if (ret != NULL) {
    STMTREP *rstmt = caller_rna->Callstmt()->Next();
    if (rstmt != NULL && rstmt->Opr() == OPR_STID && rstmt->Rhs() == ret) {
      ret = rstmt->Lhs();
      rstmt = rstmt->Next();
      if (rstmt != NULL &&
          (rstmt->Opr() == OPR_STID || rstmt->Opr() == OPR_ISTORE) &&
          rstmt->Rhs() == ret)
        ret = rstmt->Lhs();
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_argcnt
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_argcnt(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // int Get_argcnt(void *func);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  IPSA *ipsa = rbc_ctx.Ipsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Get_argcnt.\n"));
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND: %d not implemented yet in Get_argcnt.\n", arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = ipsa->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Get_argcnt.\n"));
  UINT64 ret = arg_dna->Parm_list()->size() - 1;
  Is_Trace(Tracing(), (TFile, "RBC: Get_argcnt(cr%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_mem_size
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_mem_size(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // int Get_mem_size(void *expr);
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (arg == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC: null expr passed to Get_mem_size.\n"));
    ret = 0;
    Rbc_eval_certainty()->push_back(REC_UNKNOWN);
  }
  else {
    CONTEXT_SWITCH context(dna);
    ret = Get_mem_size(vsa, arg);
  }
  if (Rbc_result_ignore()) {
    Rbc_eval_certainty()->push_back(REC_SKIP);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Get_mem_size(cr%d): %lld %s\n",
                               arg ? arg->Coderep_id() : -1 ,
                               ret, Rbc_result_ignore() ? "ignored" : ""));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_value
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_value(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // int Get_value(void *expr);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null expr passed to Get_value.\n"));
  CONTEXT_SWITCH context(dna);
  UINT64 ret = Get_value(vsa, arg);
  if (Rbc_result_ignore()) {
    Rbc_eval_certainty()->push_back(REC_SKIP);
  }
  Is_Trace(Tracing(), (TFile, "RBC: Get_value(cr%d): %lld %s\n",
                       arg->Coderep_id(), ret, Rbc_result_ignore() ? "ignored":""));
  return ret;
}


// =============================================================================
//
// RBC_BASE::Generate_var_name
//
// =============================================================================
const char*
RBC_BASE::Generate_var_name(VSA *vsa_ctx, CODEREP *cr)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Generate_var_name.\n"));
  const char *ret = NULL;
  if (cr->Kind() == CK_VAR) {
    ret = ST_name(vsa_ctx->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St());
  }
  else if (cr->Kind() == CK_LDA) {
    ret = ST_name(vsa_ctx->Opt_stab()->Aux_stab_entry(cr->Lda_aux_id())->St());
  }
  else if (cr->Kind() == CK_IVAR) {
    ret = Generate_var_name(vsa_ctx, cr->Ilod_base());
  }
  else if (cr->Kind() == CK_CONST) {
    char *buf = (char*)malloc(SIZE_MAX_STR);
    memset(buf, 0, SIZE_MAX_STR);
    snprintf(buf, SIZE_MAX_STR, "SIZE:%lld", cr->Const_val());
    ret = buf;
  }
  else if (cr->Kind() == CK_OP) {
    if (cr->Opnd(0) != NULL)
      ret = Generate_var_name(vsa_ctx, cr->Opnd(0));
    if ((ret == NULL || Vsa_check_sym_ignore(ret)) && cr->Opnd(1) != NULL)
      ret = Generate_var_name(vsa_ctx, cr->Opnd(1));
  }
  else {
    ret = "";
    Is_Trace(Tracing(),
             (TFile, "RBC ERROR: CK_KIND:%d not implemented yet in Generate_var_name.\n",
              cr->Kind()));
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_elem_count
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_elem_count(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // int Get_elem_count(void *v);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null v passed to Get_elem_count.\n"));
  hash_set<IDTYPE> visited_bb;
  hash_set<IDTYPE> visited_rna;
  UINT64 ret = Get_elem_count(vsa, arg, visited_bb, visited_rna);

  // we skip lib func like std:: for now
  STRING caller_name = dna->Fname();
  BOOL skip = Is_lib_func(caller_name);
  if (skip)
    Rbc_eval_certainty()->push_back(REC_SKIP);

  Is_Trace(Tracing(), (TFile, "RBC: Get_elem_count(cr%d): %lld\n", arg->Coderep_id(), ret));
  return ret;
}


UINT64
RBC_BASE::Get_elem_count(VSA *vsa_ctx, CODEREP *cr,
                         hash_set<IDTYPE>& visited_bb, hash_set<IDTYPE>& visited_rna)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Get_elem_count.\n"));
  DNA_NODE *dna = vsa_ctx->Dna();
  CONTEXT_SWITCH context(dna);
  BOOL found = FALSE;
  UINT64 ret = 1;
  CODEREP *rhs = cr;
  if (cr->Kind() == CK_VAR) {
    STMTREP *defstmt = cr->Defstmt();
    HEAP_OBJ_REP *hor = vsa_ctx->Cr_2_heap_obj(cr);
    if (hor != NULL) {
      CODEREP *size = hor->Rsc_obj()->Byte_size();
      if (size != NULL && size->Kind() == CK_CONST) {
        ret = size->Const_val();
        return ret;
      }
    }
    while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
      rhs = defstmt->Rhs();
      if (rhs != NULL && rhs->Kind() == CK_VAR && defstmt != rhs->Defstmt())
        defstmt = rhs->Defstmt();
      else
        break;
    }
    if (rhs->Kind() == CK_VAR) {
      if (rhs->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI))) {
        PHI_NODE     *phi = rhs->Defphi();
        if (visited_bb.find(phi->Bb()->Id()) != visited_bb.end())
          return ret;
        visited_bb.insert(phi->Bb()->Id());
        Is_Trace(Tracing(), (TFile, "RBC: processing PHI cr%d\n", rhs->Coderep_id()));
        PHI_OPND_ITER phi_opnd_iter(phi);
        CODEREP      *opnd;
        FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
          Is_Trace(Tracing(), (TFile, "RBC: processing opnd cr%d\n", opnd->Coderep_id()));
          UINT64 tmp = Get_elem_count(vsa_ctx, opnd, visited_bb, visited_rna);
          if (ret == 1)
            ret = tmp;
          else if (tmp > 1 && tmp < ret)
            ret = tmp;
          found = TRUE;
        }
        Is_Trace(Tracing(), (TFile, "RBC: processing PHI cr%d done\n", rhs->Coderep_id()));
      }
      else if (rhs->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI))) {
        // Go through caller to find the value
        for (INT i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
          RNA_NODE *rna = (*dna->Clby_list())[i];
          if (visited_rna.find(rna->Rna_idx()) != visited_rna.end())
            continue;
          visited_rna.insert(rna->Rna_idx());
          CODEREP *arg = rna->Get_arg(dna->Is_param(rhs));
          if (arg == NULL)
            continue;
          DNA_NODE *caller_dna = vsa_ctx->Ipsa()->Get_dna(rna->Caller_idx());
          if (caller_dna == NULL)
            continue;
          if (caller_dna == dna)
            continue;
          VSA *vsa = caller_dna->Comp_unit()->Vsa();
          CONTEXT_SWITCH caller_ctx(caller_dna);
          Is_Trace(Tracing(), (TFile, "RBC: searching in func(\"%s\") for:\n ", caller_dna->Fname()));
          Is_Trace_cmd(Tracing(), arg->Print(TFile));
          hash_set<IDTYPE> caller_visited_bb;
          UINT64 tmp = Get_elem_count(vsa, arg, caller_visited_bb, visited_rna);
          if (ret == 1)
            ret = tmp;
          else if (tmp > 1 && tmp < ret)
            ret = tmp;
          found = TRUE;
        }
      }
    }
  }
  if (!found) {
    TY *ty = Get_cr_ty(vsa_ctx, rhs);
    if (ty != NULL)
      if (TY_kind(*ty) == KIND_POINTER) {
        UINT64 pt_size = TY_size(TY_pointed(*ty));
        ret = (pt_size == 0) ? 0 : TY_size(*ty) / pt_size;
      }
      else if (TY_kind(*ty) == KIND_ARRAY) {
        UINT64 elem_size = TY_size(TY_etype(*ty));
        ret = (elem_size == 0) ? 0 : TY_size(*ty) / elem_size;
      }
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__get_strlen
//
// Get_strlen(OBJECT obj)
// =============================================================================
UINT64
RBC_BASE::Eval__get_strlen(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 len = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  if (obj) {
    CONTEXT_SWITCH caller_ctx(dna);
    len = Get_strlen(rbc_ctx.Caller_vsa(), obj, rbc_ctx.Stmt(), rbc_ctx.Mem_pool());
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Get_strlen(cr%d)=%lld %s\n",
                       obj ? obj->Coderep_id() : 0, len, Rbc_result_ignore() ? "ignored" : ""));
  return len;
}


UINT64
RBC_BASE::Get_strlen(VSA *vsa, CODEREP *cr, STMTREP *stmt, MEM_POOL *pool)
{
  UINT64 len = 0;
  if (cr == NULL || stmt == NULL)
    return len;
  DNA_NODE *dna = vsa->Dna();
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa, stmt, cr, str_set, pool);
  if (!found) {
    len = Get_initv_strlen(dna, cr);
    if (len == 0) {
      // try vsym U-D to find str definition for lenth
      VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
      VSYM_OBJ_REP *vor = vsa->Find_vor_mu_vor(stmt, cr, &any_vfr);
      if (vor != NULL) {
        STMTREP *defstmt = NULL;
        if (vor->Attr() == ROR_DEF_BY_ISTORE ||
            vor->Attr() == ROR_DEF_BY_COPY ||
            vor->Attr() == ROR_DEF_BY_CHI)
          defstmt = vor->Defstmt();
        while (defstmt != NULL) {
          RNA_NODE *rna = dna->Get_callsite_rna(defstmt);
          // check def via implicit assignment
          if (rna != NULL && rna->Is_flag_set(RBC_SE_IMPLICIT_ASSIGN)) {
            SRC_VECTOR *srcs = vsa->Ipsa()->Get_assign_src(rna, cr);
            if (srcs != NULL) {
              for (INT i = 0; i < srcs->size(); i++) {
                CODEREP *src = rna->Get_arg((*srcs)[i]);
                if (src != NULL && src != cr)
                  len += Get_initv_strlen(dna, src);
              }
            }
          } // end IMPLICIT ASSIGN
          else if (defstmt->Opr() == OPR_MSTORE) {
            CODEREP *rhs = defstmt->Rhs();
            if (rhs != NULL && rhs->Kind() == CK_IVAR) {
              rhs = rhs->Ilod_base();
              if (rhs != NULL && rhs->Kind() == CK_LDA) {
                ST *lda_st = rhs->Lda_base_st();
                if (ST_class(lda_st) == CLASS_CONST &&
                    TCON_ty(ST_tcon_val(lda_st)) == MTYPE_STR)
                  len = strlen(Index_to_char_array(TCON_str_idx(ST_tcon_val(lda_st))));
              }
            }
          } // end OPR_MSTORE
          // found some string, stop
          if (len != 0)
            break;
          // not found, go vor chi opnd
          CVOR *opnd = vsa->Find_vor_chi_opnd(defstmt, vor);
          if (opnd == NULL)
            break;
          vor = opnd->first;
          if (vor != NULL &&
              (vor->Attr() == ROR_DEF_BY_ISTORE ||
               vor->Attr() == ROR_DEF_BY_COPY ||
               vor->Attr() == ROR_DEF_BY_CHI) &&
              defstmt != vor->Defstmt())
            defstmt = vor->Defstmt();
          else
            break;
        } // end while defstmt != NULL
      }
    }
    if (len == 0)
      Rbc_eval_certainty()->push_back(REC_SKIP);
  } else {
    if (str_set.size() == 1) {
      char *str = *(str_set.begin());
      len = str ? strlen(str) : 0;
    } else {
      // multiple const string found, not processing it for now
      // can return a coderep with range
      Rbc_eval_certainty()->push_back(REC_SKIP);
    }
  }
  return len;
}


UINT64
RBC_BASE::Get_initv_strlen(DNA_NODE *dna, CODEREP *cr)
{
  UINT64 ret = 0;
  ST_IDX st_idx = ST_IDX_ZERO;
  ST *st = NULL;
  if (cr->Kind() == CK_LDA) {
    st = cr->Lda_base_st();
  }
  else if (cr->Kind() == CK_VAR) {
    st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
  }
  // check if global initialized
  if (st != NULL &&
      (ST_sclass(st) == SCLASS_DGLOBAL ||
       ST_sclass(st) == SCLASS_FSTATIC ||
       ST_sclass(st) == SCLASS_PSTATIC) &&
      ST_is_initialized(st) &&
      !ST_init_value_zero(st)) {
    if (ST_class(st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(st)) == MTYPE_STR) {
      char *tmp = Index_to_char_array(TCON_str_idx(ST_tcon_val(st)));
      ret = strlen(tmp);
    }
    else {
      st_idx = ST_st_idx(st);
      if (ST_IDX_level(st_idx) == GLOBAL_SYMTAB) {
        const INITO *inito = ST_inito(dna->File_idx(), st_idx);
        INITV_IDX blk = INITO_val(*inito);
        INITVKIND kind = INITV_kind(blk);
        if (blk != INITO_IDX_ZERO) {
          if (kind == INITVKIND_BLOCK) {
            INITV_IDX initv_idx = INITV_blk(blk);
            while (initv_idx != INITO_IDX_ZERO) {
              if (INITV_kind(initv_idx) == INITVKIND_VAL) {
                UINT32 val = TCON_uval(INITV_tc_val(initv_idx));
                if (val == 0)
                  break;
                ret++;
              }
              initv_idx = INITV_next(initv_idx);
            } // end while
          } // end INITV BLOCK
          else if (kind == INITVKIND_SYMOFF) {
            ST_IDX initv_st_idx = INITV_st(blk);
            ST *initv_st = &St_Table[initv_st_idx];
            if (ST_class(initv_st) == CLASS_CONST &&
                TCON_ty(ST_tcon_val(initv_st)) == MTYPE_STR) {
              char *tmp = Index_to_char_array(TCON_str_idx(ST_tcon_val(initv_st)));
              ret = strlen(tmp);
            }
          } // end INITV SYMOFF
        }
      } // end GLOBAL_SYMTAB
    }
  } // end initialized
  else {
    if (cr->Kind() == CK_VAR) {
      STMTREP *def_stmt = cr->Defstmt();
      if (def_stmt != NULL) {
        OPERATOR opr = def_stmt->Opr();
        if (opr == OPR_STID) {
          CODEREP *rhs = def_stmt->Rhs();
          if (rhs != NULL && rhs != cr) {
            ret = Get_initv_strlen(dna, rhs);
          }
        } // end OPR_STID
        else if (opr == OPR_CALL) {
          RNA_NODE *rna = dna->Get_callsite_rna(def_stmt);
          // give up on recursive call or try at most once?
          if (rna != NULL && !rna->Is_back_edge()) {
            const CALLEE_VECTOR& callee_list = rna->Callee_list();
            for (CALLEE_VECTOR::const_iterator iter = callee_list.begin();
                 iter != callee_list.end(); iter++) {
              BOOL found = FALSE;
              DNA_NODE *callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(iter->Callee());
              if (callee == NULL)
                continue;
              if (callee->Non_functional())
                continue;
              for (INT i = PDV_INIT_ID; i < callee->Retv_list()->size(); i++) {
                PDV_NODE *pdv = (*callee->Retv_list())[i];
                if ((pdv->Kind() & BY_RETURNSTMT) == 0)
                  continue;
                CODEREP *ret_cr = pdv->Stmt()->Lhs();
                if (ret_cr != NULL) {
                  CONTEXT_SWITCH context(callee);
                  ret = Get_initv_strlen(callee, ret_cr);
                }
                if (ret != 0) {
                  found = TRUE;
                  break;
                }
              }
              if (found)
                break;
            } // end for callee
          }
        } // end OPR_CALL
      } // end def_stmt != NULL
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__get_type_name
// Get_type_name(Object obj)
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_type_name(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(obj, ("RBC ERROR: null obj passed to Get_type_name\n"));
  CONTEXT_SWITCH caller_ctx(dna);
  TY_IDX ty_idx = obj->object_ty();
  if(PU_java_lang(*dna->Pu()) && TY_kind(ty_idx) == KIND_POINTER) {
    // java object are all pointer type, get real type from pointed ty
    ty_idx = TY_pointed(ty_idx);
  }
  char *name = Clone_string(TY_name(ty_idx), rbc_ctx.Mem_pool());
  Is_Trace(Tracing(), (TFile, "RBC: Get_type_name() = %s\n", name));
  return (UINT64)name;
}

// =============================================================================
//
// RBC_BASE::Eval__get_type_kind
// Get_type_kind(Object obj)
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_type_kind(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(obj, ("RBC ERROR: null obj passed to Get_type_kind\n"));
  CONTEXT_SWITCH caller_ctx(dna);
  TY_IDX ty_idx = obj->object_ty();
  if(PU_java_lang(*dna->Pu()) && TY_kind(ty_idx) == KIND_POINTER) {
    // java object are all pointer type, get real type from pointed ty
    ty_idx = TY_pointed(ty_idx);
  }
  switch(TY_kind(ty_idx)) {
    case KIND_SCALAR:
      return RBC_ENGINE::TYPE_KIND_PRIMITIVE;
    case KIND_STRUCT:
      if(TY_is_array_class(ty_idx)) {
        return RBC_ENGINE::TYPE_KIND_ARRAY;
      } else {
        return RBC_ENGINE::TYPE_KIND_CLASS;
      }
    case KIND_ARRAY:
      return RBC_ENGINE::TYPE_KIND_ARRAY;
  }
  return RBC_ENGINE::TYPE_KIND_INVALID;
}

// =============================================================================
//
// RBC_BASE::Eval__get_max_stack_size: return the max stack size of given func
//   int Get_max_stack_size(void *func)
//
//
// =============================================================================
UINT64
RBC_BASE::Eval__get_max_stack_size(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  RBC_EVAL_SKIP();
  UINT64 ret = 0;
  DNA_NODE *dna = rbc_ctx.Caller();
  IPSA *ipsa = rbc_ctx.Ipsa();
  MEM_POOL *loc_pool = rbc_ctx.Mem_pool();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *arg = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(arg != NULL, ("RBC ERROR: null func passed to Get_max_stack_size.\n"));
  Is_True_Rbc(arg->Kind() == CK_LDA,
              ("RBC ERROR: CK_KIND(%d) not implemented yet in Get_max_stack_size.\n",
               arg->Kind()));
  UINT32 file_idx = dna->File_idx();
  DNA_NODE *arg_dna = ipsa->Get_dna(file_idx, arg->Lda_base_st());
  Is_True_Rbc(arg_dna != NULL, ("RBC ERROR: null function information in Get_max_stack_size.\n"));
  // bottom up traversal evaluating max value,
  // dna index starting from 1, we add one more to avoid OOB access
  UINT64 dna_count = ipsa->_post_order_dnode.size() + 1;
  // values record max value of all callee, indexes record rna/dna index of max value
  UINT64 *values = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, loc_pool);
  UINT64 *indexes = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, loc_pool);
  for (int i = 0; i < dna_count; i++) {
    values[i] = 0;
    indexes[i] = 0;
  }
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> dna_iter(ipsa); !dna_iter.Is_end(); dna_iter.Next()) {
    DNA_NODE *func = dna_iter.Current();
    Is_True_Rbc(func->Dna_idx() < dna_count, ("RBC ERROR: FUNC INDEX(%d) out of BOUND(%lld)\n",
                                              func->Dna_idx(), dna_count));
    UINT64 max_callee_value = 0;
    UINT64 max_callee_index = 0;
    // find max value of "func"'s functional callee
    for (CALLEE_ITER callee_iter(ipsa, func); !callee_iter.Is_end(); callee_iter.Next()) {
      RNA_NODE *callee_rna = callee_iter.Current_callsite();
      DNA_NODE *cur_callee = callee_iter.Current();
      if (cur_callee != NULL && !cur_callee->Non_functional()) {
        // all callees must have been processed in bottom up traversal
        // so their index must be fine and won't have OOB access,
        // and we don't need to check here
        if (values[cur_callee->Dna_idx()] > max_callee_value) {
          max_callee_value = values[cur_callee->Dna_idx()];
          // rna & dna index, used to print out error message later
          max_callee_index = (UINT64)callee_rna->Rna_idx() << 32 | (UINT64)cur_callee->Dna_idx();
        }
      }
    }

    // update max value & rna/dna index of current function
    values[func->Dna_idx()] = max_callee_value + func->Stack_size();
    indexes[func->Dna_idx()] = max_callee_index;
    if (arg_dna->Dna_idx() == func->Dna_idx()) {
      ret = values[func->Dna_idx()];
      break;
    }
  }
  // build up srcpos infor
  SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(NULL, call_stmt, dna, rbc_ctx.Spos_pool()),
                                    rbc_ctx.Spos_pool());
  srcpos_h->Append_data(arg_dna->St(), NULL, arg_dna, PATHINFO_ST_DECLARE);
  int len = strlen(arg_dna->Fname()) + SIZE_MAX_STR;
  char *var_name = (char*)CXX_NEW_ARRAY(BOOL, len, loc_pool);
  snprintf(var_name, len, "%s(%lld)", arg_dna->Fname(), values[arg_dna->Dna_idx()]);
  srcpos_h->Set_orig_stname(var_name);
  Is_Trace(Tracing(), (TFile, "STACK SIZE(%lld): \"%s\"", values[arg_dna->Dna_idx()], arg_dna->Fname()));
  DNA_NODE *cur_callee = arg_dna;
  while (cur_callee != NULL && indexes[cur_callee->Dna_idx()] != 0) {
    IDTYPE rna_idx = (IDTYPE)(indexes[cur_callee->Dna_idx()] >> 32 & 0xffffffff);
    RNA_NODE *callee_rna = ipsa->Get_rna(rna_idx);
    if (callee_rna == NULL)
      break;
    srcpos_h->Append_data(callee_rna->Callstmt(), cur_callee, PATHINFO_DNA_CALLSITE);
    IDTYPE dna_idx = (IDTYPE)(indexes[cur_callee->Dna_idx()] & 0xffffffff);
    cur_callee = ipsa->Get_dna(dna_idx);
    Is_Trace(Tracing(), (TFile, " -> \"%s\"", cur_callee->Fname()));
  }
  Plist_true()->push_back(srcpos_h);

  return ret;
}

// =============================================================================
//
// RBC_BASE::Eval__set_parm_tainted
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_parm_tainted(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL set_parm_tainted(CODEREP* obj);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  IDTYPE arg_idx = rna->Get_arg_with_cr(obj);
  Is_True_Rbc(arg_idx != INVALID_VAR_IDX, ("RBC ERROR: invalid arg idx"));
  rna->Set_arg_flag(arg_idx, ARG_TAINTED);
  Is_Trace(Tracing(), (TFile, "RBC: set arg(%d) of RNA_NODE(%d) req sanitize.\n",
                       arg_idx, rna->Rna_idx()));

  return 1;
}

// =============================================================================
//
// RBC_BASE::Eval__set_implicit_assign
//
// =============================================================================
UINT64
RBC_BASE::Eval__set_implicit_assign(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // BOOL Set_implicit_assign(void *tgt, void *src);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  RNA_NODE *rna = rbc_ctx.Rna();
  IPSA *ipsa = rbc_ctx.Ipsa();
  // return value uses arglist[0]
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *tgt = (CODEREP*)Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *src = (CODEREP*)Eval__exp(rbc_ctx, arg1);

  if(tgt != NULL && src != NULL) {
    int src_num = rna->Get_arg_with_cr(src);
    int tgt_num = rna->Get_arg_with_cr(tgt);
    rna->Set_flag(RBC_SE_IMPLICIT_ASSIGN);
    ipsa->Enter_rbc_assign(rna->Rna_idx(), tgt_num, src_num);
    if (tgt_num != INVALID_VAR_IDX) {
      Is_Trace(Tracing(), (TFile, "RBC: RNA_NODE(%d): arg(%d) is defined by arg(%d).\n",
                           rna->Rna_idx(), tgt_num, src_num));
    }
    else {
      Is_Trace(Tracing(), (TFile, "RBC: RNA_NODE(%d): ret is defined by arg(%d).\n",
                           rna->Rna_idx(), src_num));
    }
  }

  return 1;
}

// =============================================================================
//
// RBC_BASE::Get_mem_size
//
// =============================================================================
UINT64
RBC_BASE::Get_mem_size(VSA *vsa_ctx, CODEREP *cr)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Get_mem_size.\n"));
  UINT64 mem_sz = 0;  // default to 0, in case this function could not find it
  Rbc_eval_certainty()->push_back(REC_UNKNOWN);

  // The cr should contain pointer wither allocated by malloc or
  // point to a local via LDA operator
  switch (cr->Kind()) {
  case CK_LDA:
    {
      ST      *base_st = cr->Lda_base_st();
      TY_IDX   base_ty = ST_type(base_st);
      mem_sz = TY_size(base_ty);
      Rbc_eval_certainty()->pop_back();
    }
    break;
  case CK_VAR:
    // gather the size from the heap_obj if it is allocated locally
    // TODO TODO TODO: from iparam, a side effect of a call
    {
      HEAP_OBJ_REP *hor = vsa_ctx->Cr_2_heap_obj(cr);
      if (hor != NULL) {
        CODEREP *bs = hor->Heap_obj()->Byte_size();
        if (bs != NULL && bs->Kind() == CK_CONST) {
          mem_sz = bs->Const_val();
        } else {
          // heap size is unknown, keep certainty REC_UNKNOWN
          break;
        }
      }
      TY_IDX ty = cr->Lod_ty();
      UINT64 mem_sz1 = 0;
      if (TY_kind(ty) == KIND_POINTER)
        mem_sz1 = TY_size(TY_pointed(ty));
      else
        mem_sz1 = TY_size(ty);
      Rbc_eval_certainty()->pop_back();
      if (mem_sz < mem_sz1) {
        mem_sz = mem_sz1;
        if (cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI)))
          Rbc_eval_certainty()->push_back(REC_MAYBE);
      }
      // ST *arg_st = Get_cr_st(vsa_ctx, cr);
      // if (arg_st != NULL && Vsa_check_sym_ignore(ST_name(arg_st))) {
      //   TY *arg_ty = Get_cr_ty(vsa_ctx, cr);
      //   STPATH *stp = vsa_ctx->Dna()->Get_stpath(cr->Defstmt(), cr);
      //   if (stp != NULL)
      //     arg_ty = Ty_ptr(vsa_ctx->Dna()->File_idx(), ST_type(stp->St_idx()));
      //   if (TY_kind(*arg_ty) == KIND_POINTER)
      //     mem_sz = TY_size(TY_pointed(*arg_ty));
      //   else
      //     mem_sz = TY_size(*arg_ty);
      //   Rbc_eval_certainty()->pop_back();
      // }
    }
    break;
  case CK_OP:
    // deal with structure fields
    {
      if (cr->Opr() == OPR_ADD) {
        CODEREP *arg0 = cr->Opnd(0)->Find_actual_arg();
        ST *st0 = Get_cr_st(vsa_ctx, arg0);
        CODEREP *arg1 = cr->Opnd(1)->Find_actual_arg();
        if (arg1->Kind() != CK_CONST)
          break;
        UINT64 offset = arg1->Const_val();
        if (arg0->Kind() == CK_LDA) {
          if (Is_Structure_Type(ST_type(st0))) {
            FLD_ITER fld_iter = Make_fld_iter(TY_fld(ST_type(st0)));
            do {
              FLD_HANDLE fld(fld_iter);
              if (!fld.Is_Null() && FLD_ofst(fld) == offset) {
                mem_sz = TY_size(FLD_type(fld));
                Rbc_eval_certainty()->pop_back();
              }
            } while (!FLD_last_field(fld_iter++));
          }
          else {
            Rbc_eval_certainty()->pop_back();
            mem_sz = Get_mem_size(vsa_ctx, arg0) - arg1->Const_val();
          }
        }
        else if (arg0->Kind() == CK_VAR) {
          if (TY_kind(ST_type(st0)) == KIND_POINTER &&
              Is_Structure_Type(TY_pointed(ST_type(st0)))) {
            FLD_ITER fld_iter = Make_fld_iter(TY_fld(TY_pointed(ST_type(st0))));
            do {
              FLD_HANDLE fld(fld_iter);
              if (!fld.Is_Null() && FLD_ofst(fld) == offset) {
                mem_sz = TY_size(FLD_type(fld));
                Rbc_eval_certainty()->pop_back();
              }
            } while (!FLD_last_field(fld_iter++));
          }
        }
      }
      else if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
        Rbc_eval_certainty()->pop_back();
        mem_sz = Get_mem_size(vsa_ctx, cr->Opnd(0));
      }
      else
        Is_Trace(Tracing(), (TFile,
                             "RBC ERROR: OPR_KIND:%d not implemented yet in Get_mem_size.\n",
                             cr->Opr()));
    }
    break;
  default:
    Is_Trace(Tracing(), (TFile,
                         "RBC ERROR: CK_KIND:%d not implemented yet in Get_mem_size.\n",
                         cr->Kind()));
    break;

  } // process cr

  return mem_sz;
}


// =============================================================================
//
// RBC_BASE::Get_value
//
// =============================================================================
UINT64
RBC_BASE::Get_value(VSA *vsa_ctx, CODEREP *cr)
{
  Is_True_Rbc(cr != NULL, ("RBC ERROR: null cr passed to Get_value.\n"));
  UINT64 cr_value = 0;  // default to 0, in case this function could not find it
  Rbc_eval_certainty()->push_back(REC_UNKNOWN);

  // collect copy_limit from the 3rd arg
  switch (cr->Kind()) {
  case CK_CONST:
    cr_value = cr->Const_val();
    Rbc_eval_certainty()->pop_back();
    break;
  case CK_VAR: {
    // could be defined by caller thru iparam or a side effect of a call
    // TODO TODO TODO: this is interesting case we need to handle
    STMTREP *defstmt = cr->Defstmt();
    if (defstmt != NULL && defstmt->Rhs() != NULL) {
      if (defstmt->Opr() == OPR_CALL) {
        if (cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_CHI))) {
          Rbc_eval_certainty()->pop_back();
          cr_value = Get_value(vsa_ctx, cr->Defchi()->OPND());
          break;
        }
        else if (strcmp(ST_name(defstmt->St()), "wcslen") == 0) {
          CODEREP *arg = defstmt->Rhs()->Opnd(0)->Find_actual_arg();
          TY *ty = Get_cr_ty(vsa_ctx, arg);
          if (ty != NULL) {
            if (TY_kind(*ty) == KIND_POINTER) {
              UINT64 pt_size = TY_size(TY_pointed(*ty));
              cr_value = (pt_size == 0) ? 0 : TY_size(*ty) / pt_size;
            }
            else if (TY_kind(*ty) == KIND_ARRAY) {
              UINT64 elem_size = TY_size(TY_etype(*ty));
              cr_value = (elem_size == 0) ? 0 : TY_size(*ty) / elem_size;
            }
            else
              cr_value = 1;
            Rbc_eval_certainty()->pop_back();
            break;
          }
        }
      }
      else if (defstmt->Opr() == OPR_INTRINSIC_CALL) {
        CODEREP *rhs = defstmt->Rhs();
        if (rhs->Intrinsic() == INTRN_STRLEN) {
          Rbc_eval_certainty()->pop_back();
          cr_value = Get_strlen(vsa_ctx, rhs->Opnd(0)->Find_actual_arg(), defstmt, vsa_ctx->Loc_pool());
          if (cr_value != 0)
            break;
        }
      }
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, defstmt->Rhs());
    }
    break;
  }
  case CK_OP: {
    if (cr->Opr() == OPR_ADD) {
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, cr->Opnd(0));
      cr_value += Get_value(vsa_ctx, cr->Opnd(1));
    }
    else if (cr->Opr() == OPR_SUB) {
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, cr->Opnd(0));
      cr_value -= Get_value(vsa_ctx, cr->Opnd(1));
    }
    else if (cr->Opr() == OPR_MPY) {
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, cr->Opnd(0));
      if (cr_value == 0) {
        Rbc_eval_certainty()->push_back(REC_MAYBE);
        cr_value = 1;
      }
      cr_value *= Get_value(vsa_ctx, cr->Opnd(1));
    }
    else if (cr->Opr() == OPR_DIV) {
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, cr->Opnd(1));
      if (cr_value == 0)
        Rbc_eval_certainty()->push_back(REC_SKIP);
      else
        cr_value = Get_value(vsa_ctx, cr->Opnd(0))/cr_value;
    }
    else if (cr->Opr() == OPR_INTRINSIC_CALL) {
      if (cr->Intrinsic() == INTRN_STRLEN) {
        Rbc_eval_certainty()->pop_back();
        cr_value = Get_value(vsa_ctx, cr->Opnd(0)->Find_actual_arg());
      }
    }
    else if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
      Rbc_eval_certainty()->pop_back();
      cr_value = Get_value(vsa_ctx, cr->Opnd(0));
    }
    else
      Is_Trace(Tracing(), (TFile,
                           "RBC ERROR: OPR_KIND:%d not implemented yet in Get_value.\n",
                           cr->Opr()));
    break;
  }
  case CK_LDA: {
    ST *lda_st = cr->Lda_base_st();
    if (ST_class(lda_st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(lda_st)) == MTYPE_STR) {
      cr_value = strlen(Index_to_char_array(TCON_str_idx(ST_tcon_val(lda_st))));
      Rbc_eval_certainty()->pop_back();
    }
    else {
      TY *ty = Get_cr_ty(vsa_ctx, cr);
      if (ty != NULL) {
        cr_value = TY_size(*ty);
        Rbc_eval_certainty()->pop_back();
      }
    }
    break;
  }
  case CK_IVAR: {
    CODEREP *base = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
    Rbc_eval_certainty()->pop_back();
    cr_value = Get_value(vsa_ctx, base);
    break;
  }
  default:
    Is_Trace(Tracing(), (TFile,
                         "RBC ERROR: CK_KIND: %d not implemented yet in Get_value.\n",
                         cr->Kind()));
    break;
  } // process cr

  return cr_value;
}


// =============================================================================
//
// RBC_BASE::Is_equal
//
// =============================================================================
BOOL
RBC_BASE::Is_equal(const STRING name, const char * as)
{
  Is_True(name != NULL && as != NULL, ("RBC ERROR: invalid parameters in Is_equal.\n"));
  if (name == NULL && as == NULL)
    return TRUE;
  else if (name == NULL || as == NULL)
    return FALSE;
  BOOL ret = FALSE;
  STRING fname = Prune_func_name(name);
  Is_True(fname != NULL, ("RBC ERROR: null fname in Is_equal for \"%s\".\n", name));
  if (fname == NULL)
    return FALSE;
  if (strcmp(fname, as) == 0)
    ret = TRUE;
  if (fname)
    free(fname);
  return ret;
}


// =============================================================================
//
// RBC_BASE::Get_rbc_nth_call : Return the call statement at rna's offset
//
// =============================================================================
STMTREP *
RBC_BASE::Get_rbc_nth_call(STMTREP *call_stmt, UINT32 offset)
{
  OPERATOR opr = call_stmt->Opr();

  Is_True_Ret(opr == OPR_CALL,
              ("RBC ERROR: Get_rbc_nth_call: called for non direct call.\n"), NULL);

  CODEREP *arg1 = call_stmt->Rhs()->Find_nth_arg(offset);
  Is_True_Ret(arg1, ("RBC ERROR: Get_rbc_nth_call: ill formed arg at offset%d.\n", offset), NULL);

  STMTREP *func = NULL;
  if (arg1->Kind() == CK_OP &&
      (arg1->Opr() == OPR_EQ || arg1->Opr() == OPR_NE)) {
    arg1 = arg1->Opnd(0);
  }
  if (arg1 && arg1->Kind() == CK_VAR) {
    func = arg1->Defstmt();
    opr = func->Opr();
    while (opr == OPR_STID) {
      CODEREP *rhs = func->Rhs();
      if (rhs == NULL || rhs->Kind() != CK_VAR)
        break;
      func = rhs->Defstmt();
      opr = func->Opr();
    }
  }
  if (func && func->Opr() != OPR_CALL) {
    return NULL;
  }
  return func;
}

// =============================================================================
//
// RBC_BASE::Process_model_decl_func : Setup appropriate _rbc_flag and related
//       info: All DNA with model decl attribute will contain _rbc_flag:
//       DNA_RBC_MODEL Fsm_use("fsm_name") stash away the "fsm_name" in the
//       enclosing_dna
//
// =============================================================================
void
RBC_BASE::Process_model_decl_func(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  STMTREP *model_arg0 = Get_rbc_nth_call(model_call, Rbc_parm_offset(enclosing_dna));
  if (model_arg0 == NULL) return;

  RNA_NODE *model_rna = enclosing_dna->Get_callsite_rna(model_arg0);
  if (model_rna == NULL) return;

  RBC_OP model_op = model_rna->Rbc_op();
  INIT_FUNC func = Init_func(model_op);
  if(func != NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Process_model_decl_func init func for %s\n",
                         Rbc_op_name(model_op)));
    (this->*func)(ipsa, enclosing_dna, model, model_arg0);
  }
}

void
RBC_BASE::Process_rbc_assert_func(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  STMTREP *assert_cond = Get_rbc_nth_call(model_call, 0 + Rbc_parm_offset(enclosing_dna));
  if (assert_cond == NULL) {
    return;
  }
  Is_True_Ret(assert_cond->Opr() == OPR_CALL, ("Process_rbc_assert_func: bad assert cond call"));

  RNA_NODE *assert_rna = enclosing_dna->Get_callsite_rna(assert_cond);
  if (assert_rna == NULL) return;

  RBC_OP assert_op = assert_rna->Rbc_op();
  INIT_FUNC func = Init_func(assert_op);
  if (func != NULL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Process_rbc_assert_func init func for %s\n",
                         Rbc_op_name(assert_op)));
    (this->*func)(ipsa, enclosing_dna, model, assert_cond);
  }
}

STRING_VEC*
RBC_BASE::Build_vec_from_string(STRING str, MEM_POOL *pool)
{
  STRING_VEC *ret = CXX_NEW(STRING_VEC(STRING_VEC::allocator_type(pool)), pool);
  ret->clear();
  if (str != NULL) {
    STRING code = str;
    STRING tmp = NULL;
    while (code != NULL) {
      int len = strlen(code);
      tmp = strstr(code, " ");
      if (tmp != NULL) {
        strncpy(tmp, "\000\000", 1);
        int sub_len = strlen(code);
        ret->push_back(Clone_string(code, pool));
        if (sub_len < len)
          code = tmp + 1;
        else
          Is_Trace(Tracing(), (TFile, "RBC ERROR: str length invalid in Build_vec_from_string.\n"));
      }
      else {
        ret->push_back(Clone_string(code, pool));
        break;
      }
    }
  }
  return ret;
}

void
RBC_BASE::Propagate_fsm_use(IPSA *ipsa, DNA_NODE *dna)
{
  CONTEXT_SWITCH ctx(dna);
  VIRFUNC_INFO_VEC* cand_funs = ipsa->Glob_cha()->Find_candidate_functions(dna->Fname(), Loc_pool());
  if(cand_funs && cand_funs->size() >= 1) {
    VIRFUNC_INFO_VEC::iterator it;
    for (it = cand_funs->begin(); it != cand_funs->end(); ++it) {
      VIRFUNC_INFO* func = *it;
      Is_True(func != NULL, ("null vfunc entry in class hierarchy"));
      ST *fun_st = St_ptr(func->File_idx(), func->Fun_st());
      Is_True(ST_class(fun_st) == CLASS_FUNC,
              ("Find vfun: found virtual fun st is not a function symbol"));
      DNA_NODE *callee = ipsa->Get_dna(func->File_idx(), fun_st);
      Propagate_fsm_list(callee, dna, Mem_pool());
    }
  }
}

void
RBC_BASE::Propagate_fsm_list(DNA_NODE *tgt_dna, DNA_NODE *src_dna, MEM_POOL *pool)
{
  if (tgt_dna == NULL || src_dna == NULL || tgt_dna == src_dna)
    return;
  STRING_VEC *src_fsm_list = src_dna->Fsm_list();
  if (src_fsm_list == NULL)
    return;
  STRING_VEC *tgt_fsm_list = tgt_dna->Fsm_list();
  tgt_dna->Set_rbc_flag(DNA_FSM_MODELLED);
  for (INT i = 0; i < src_fsm_list->size(); i++) {
    STRING fsm_name = (*src_fsm_list)[i];
    BOOL needed = TRUE;
    if (tgt_fsm_list != NULL) {
      for (INT j = 0; j < tgt_fsm_list->size(); j++) {
        if (strcmp(fsm_name, (*tgt_fsm_list)[i]) == 0) {
          needed = FALSE;
          break;
        }
      }
    }
    if (needed) {
      if (strcmp(tgt_dna->Fname(), src_dna->Fname()) != 0) {
        // propagate fsm use rbc dna to class hierarchy dna,
        // unlike Add_rbc_node(), set class hierarchy dna's rbc ref dna to itself,
        // and do not change fsm use rbc dna's ref.
        DNODE_VECTOR *dna_list = Get_rbc_nodes(tgt_dna);
        if (dna_list == NULL) {
          dna_list = CXX_NEW(DNODE_VECTOR(DNODE_VECTOR::allocator_type(_mem_pool)), _mem_pool);
          _func_rbc_map[tgt_dna->Dna_idx()] = dna_list;
          tgt_dna->Set_rbc_ref_idx(tgt_dna->Dna_idx());
        }
        for (DNODE_VECTOR::const_iterator iter = dna_list->begin();
             iter != dna_list->end(); iter++) {
          if (*iter == src_dna)
            return;
        }
        dna_list->push_back(src_dna);
      }
      tgt_dna->Add_fsm_list(Clone_string(fsm_name, pool));
      Is_Trace(Tracing(),
               (TFile, "RBC_BASE::Propagate_fsm_list propagate fsm %s from %s to %s\n",
                fsm_name, src_dna->Fname(), tgt_dna->Fname()));
    }
  }
  return;
}

// =============================================================================
//
// RBC_BASE::RBC_BASE
//
// =============================================================================
RBC_BASE::RBC_BASE(MEM_POOL *gpool, MEM_POOL *loc_pool):
  _mem_pool(gpool), _loc_pool(loc_pool),
  _rules_map(128, NULL, gpool, FALSE),
  _rule_except_map(128, __gnu_cxx::hash<const char*>(), streq(), NAME_RB_ALLOCATOR(gpool)),
  _builtin_func_map_new(128, __gnu_cxx::hash<UINT32>(), __gnu_cxx::equal_to<UINT32>(), NEW_BUILTIN_ALLOCATOR(gpool)),
  _eval_name_op_map(31, __gnu_cxx::hash<const char*>(), streq(), EVAL_NAME_OP_ALLOCATOR(gpool)),
  _rbc_phase(RBC_DEFAULT_PHASE), _tag_attr_map(32, __gnu_cxx::hash<const char*>(), streq(), NAME_ID_ALLOCATOR(gpool)),
  _func_dna_map(128, __gnu_cxx::hash<const char*>(), streq(), NAME_ID_ALLOCATOR(gpool)),
  _func_rbc_map(128, __gnu_cxx::hash<IDTYPE>(), __gnu_cxx::equal_to<IDTYPE>(), IDX_DNODES_ALLOCATOR(gpool)),
  _func_rbc_ops_map(128, __gnu_cxx::hash<IDTYPE>(), __gnu_cxx::equal_to<IDTYPE>(), IDX_RBC_OPS_ALLOCATOR(gpool)),
  _tag_conf_map(128, __gnu_cxx::hash<IDTYPE>(), __gnu_cxx::equal_to<IDTYPE>(), TAG_ID_CONF_ALLOCATOR(gpool)),
  _annot_rbc_map(128, __gnu_cxx::hash<const char*>(), streq(), NAME_RB_ALLOCATOR(gpool)),
  _rule_set_map(128, __gnu_cxx::hash<const char*>(), streq(), RULE_NAME_SET_ALLOCATOR(gpool)),
  _dna_tag_map(31, __gnu_cxx::hash<IDTYPE>(), __gnu_cxx::equal_to<IDTYPE>(), DAN_TAG_INFOS_ALLOCATOR(gpool))
{
  Rule_fix_cost_init();  // COMPLEXITY hack
  Builtin_func_map_init();
  _eval_cert = CXX_NEW(RBC_EVAL_CERTAINTY_VEC(RBC_EVAL_CERTAINTY_VEC::allocator_type(gpool)), gpool);
  _true_plist = CXX_NEW(SRCPOS_HANDLE_VEC(SRCPOS_HANDLE_VEC::allocator_type(gpool)), gpool);
  _false_plist = CXX_NEW(SRCPOS_HANDLE_VEC(SRCPOS_HANDLE_VEC::allocator_type(gpool)), gpool);
  _tracing = Get_Trace(TP_WOPT2, VSA_DUMP_FLAG) || Get_Trace(TP_WOPT2, FSM_RBC_DUMP_FLAG);
  _rules_map.Init();
  _last_rule_id = 0;
  _last_fsm_base_id = 0;
  _last_tag_base_id = TAG_START_ID;
  _last_tag_attr_id = TAG_START_ID;
  _fsm_base_list = CXX_NEW(FB_LIST, _mem_pool);
  _tag_base_list = CXX_NEW(TB_LIST, _mem_pool);
  _current_fsm_base = NULL;
  _adjust_ofst = FALSE;
  _builtin_checks = RBCT_NONE;
  _tag_attr_vec = CXX_NEW(NAME_VEC(NAME_VEC::allocator_type(gpool)), gpool);
  for (int i = 0; i < TAG_START_ID; i++) {
    _tag_attr_vec->push_back("");
  }
  if (VSA_Builtin_Recursion)
    Set_builtin_check(RBCT_RECURSION);
}


RBC_BASE::~RBC_BASE()
{
  _eval_cert->clear();
  _true_plist->clear();
  _false_plist->clear();
}


// =============================================================================
//
// RBC_BASE::Add_rbc_node add to function => rbc DNODE_VECTOR map
//     with dna's index, if rbc_dna is already in the DNODE_VECTOR, skip.
//     we assume dna is a source one, but it can be a rbc dna as well when
//     we have multiple rules written in different files for a same function
//     that have no user source codes like libc functions,
//     in this case, reuse the rbc reference index if there already is
//
// =============================================================================
void
RBC_BASE::Add_rbc_node(DNA_NODE *dna, DNA_NODE *rbc_dna)
{
  if (dna == NULL || rbc_dna == NULL)
    return;
  DNODE_VECTOR *dna_list = Get_rbc_nodes(dna);
  if (dna_list == NULL) {
    if (dna->Non_functional()) {
      // dna is a rbc dna as well, reuse dna_list from rbc_dna
      dna_list = Get_rbc_nodes(rbc_dna);
      if (dna_list != NULL) {
        // add dna to the list
        dna_list->push_back(dna);
        Add_rbc_ops(rbc_dna, dna);
        dna->Set_rbc_ref_idx(rbc_dna->Get_rbc_ref_idx());
        Is_Trace(Tracing(), (TFile, "Add DNA_NODE(%d) for %s with INDEX DNA_NODE(%d)\n",
                             dna->Dna_idx(), dna->Fname(), dna->Get_rbc_ref_idx()));
        return;
      }
    }

    // new vector, add to map
    dna_list = CXX_NEW(DNODE_VECTOR(DNODE_VECTOR::allocator_type(_mem_pool)), _mem_pool);
    _func_rbc_map[dna->Dna_idx()] = dna_list;
    dna->Set_rbc_ref_idx(dna->Dna_idx());
  }
  for (DNODE_VECTOR::const_iterator iter = dna_list->begin();
       iter != dna_list->end(); iter++) {
    // already in vector, skip
    if (*iter == rbc_dna)
      return;
  }
  dna_list->push_back(rbc_dna);
  Add_rbc_ops(dna, rbc_dna);
  rbc_dna->Set_rbc_ref_idx(dna->Get_rbc_ref_idx());
  Is_Trace(Tracing(), (TFile, "Add DNA_NODE(%d) for %s with INDEX DNA_NODE(%d)\n",
                       rbc_dna->Dna_idx(), rbc_dna->Fname(), rbc_dna->Get_rbc_ref_idx()));
}

// =============================================================================
//
// RBC_BASE::Add_rbc_ops add to function => rbc ops map
//   add rbc ops in rbc_dna to dna
//
// =============================================================================
void
RBC_BASE::Add_rbc_ops(DNA_NODE *dna, DNA_NODE *rbc_dna)
{
  if (dna == NULL || rbc_dna == NULL)
    return;
  if (!rbc_dna->Non_functional())
    return;

  RBC_OP_SET *op_set = Get_rbc_ops(dna);
  if (op_set == NULL) {
    op_set = CXX_NEW(RBC_OP_SET(3, __gnu_cxx::hash<UINT32>(),
                                __gnu_cxx::equal_to<UINT32>(),
                                RBC_OP_SET::allocator_type(_mem_pool)),
                     _mem_pool);
    _func_rbc_ops_map[dna->Dna_idx()] = op_set;
  }
  RNODE_VECTOR *call_list = rbc_dna->Call_list();
  for (INT i = VAR_INIT_ID; i < call_list->size(); ++i) {
    RNA_NODE *rna = (*call_list)[i];
    Is_True(rna != NULL, ("invalid rna"));
    if (rna && rna->Rbc_op() != RBC_OP_NONE) {
      op_set->insert(rna->Rbc_op());
    }
  }
}

// =============================================================================
//
// RBC_BASE::Rbc_init
//
// =============================================================================
void
RBC_BASE::Rbc_init(void)
{
  _eval_cert->clear();
  _eval_cert->push_back(REC_DEFINITE);
  _true_plist->clear();
  _false_plist->clear();
}


// =============================================================================
//
// RBC_BASE::Prune_func_name
//
// =============================================================================
char *
RBC_BASE::Prune_func_name(const char *name)
{
  Is_True(name != NULL, ("RBC ERROR: null name passed to Prune_func_name.\n"));
  if (name == NULL)
    return NULL;
  char *ret = Vsa_demangle(name);
  char *tmp = NULL;
  char *pch = strstr(ret, "(");
  if (pch && strlen(pch) > 1)
    strncpy(pch, "\000\000", 1);
  tmp = (char*)malloc(strlen(ret) + 1);
  memset(tmp, 0, strlen(ret) + 1);
  strcpy(tmp, ret);
  free(ret);
  ret = tmp;
  tmp = NULL;
  pch = strrchr(ret, ':');
  if (pch == NULL || strlen(pch + 1) == 0)
    pch = strrchr(ret, '.');
  if (pch && strlen(pch + 1) > 0) {
    tmp = (char*)malloc(strlen(pch));
    memset(tmp, 0, strlen(pch));
    strcpy(tmp, pch + 1);
    free(ret);
    ret = tmp;
    tmp = NULL;
  }
  pch = strstr(ret, "<");
  if (pch == NULL)
    return ret;
  if (strlen(pch) > 1)
    strncpy(pch, "\000\000", 1);
  tmp = (char*)malloc(strlen(ret) + 1);
  memset(tmp, 0, strlen(ret) + 1);
  strcpy(tmp, ret);
  free(ret);
  ret = tmp;
  tmp = NULL;
  return ret;
}


// =============================================================================
//
// RBC_BASE::Is_const
//
// =============================================================================

BOOL
RBC_BASE::Is_const(DNA_NODE *dna, CODEREP *cr)
{
  switch(cr->Kind()) {
    case CK_CONST:
      return TRUE;
    case CK_VAR:
    {
      ST* st = dna->Comp_unit()->Opt_stab()->St(cr->Aux_id());
      if (st && ST_is_class_const_data(st)) {
        return TRUE;
      }
    }
    default:
      return FALSE;
  }
}


// =============================================================================
//
// RBC_BASE::Eval__java_Property_getProperty: suppose we are in the context of dna,
//     return the value of given property
//
// =============================================================================
char*
RBC_BASE::Eval__java_Property_getProperty(DNA_NODE *dna, STMTREP *call_stmt)
{
  if (!PU_java_lang(Get_Current_PU()))
    return NULL;
  if (dna == NULL || call_stmt == NULL)
    return NULL;
  VSA *vsa = dna->Comp_unit()->Vsa();
  RNA_NODE *rna = dna->Get_callsite_rna(call_stmt);
  if (rna == NULL)
    return NULL;
  // evaluate Java getProperty
  const CALLEE_VECTOR& callee_list = rna->Callee_list();
  for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
    DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
    if (callee == NULL)
      continue;
    if (strcmp(callee->Fname(), "_ZN4java4util10Properties11getPropertyEJPNS_4lang6StringES4_S4_") == 0) {
      // property string
      CODEREP *arg1 = call_stmt->Rhs()->Opnd(1)->Find_actual_arg();
      char *prop_str = Find_const_char(dna, arg1);
      if (prop_str == NULL)
        continue;
      // default string
      CODEREP *arg2 = call_stmt->Rhs()->Opnd(2)->Find_actual_arg();
      char *df_str = Find_const_char(dna, arg2);
      // java.util.Properties(this) class
      CODEREP *prop_cr = call_stmt->Rhs()->Opnd(0)->Find_actual_arg();
      // evaluate property file path via prop_cr
      char *path = Eval__java_Property_load(dna, call_stmt, prop_cr);
      if (path != NULL) {
        FILE *pfile = fopen(path, "r");
        free(path);
        if (pfile != NULL) {
          char line[1024];
          while (fgets(line, 1024, pfile) != NULL) {
            char *pch = strstr(line, prop_str);
            if (pch != NULL) {
              line[strlen(line)-1] = '\0';
              pch = strstr(line, "=");
              pch = pch + 1;
              size_t len = strlen(pch) + 1;
              char *ret = (char*)malloc(len);
              memset(ret, 0, len);
              strcpy(ret, pch);
              Is_Trace(Tracing(), (TFile, "RBC: Java.Util.Properties.getProperty(\"%s\"): \"%s\"\n", prop_str, ret));
              return ret;
            }
          }
        }
        else {
          Is_Trace(Tracing(), (TFile, "RBC: Java.Util.Properties.getProperty failed to open property file\n"));
        }
        fclose(pfile);
      }
    }
  }
  return NULL;
}


// =============================================================================
//
// RBC_BASE::Eval__java_Property_load: suppose we are in the context of dna,
//     return the path of the property file
//
// =============================================================================
char*
RBC_BASE::Eval__java_Property_load(DNA_NODE *dna, STMTREP *call_stmt, CODEREP *cr)
{
  if (!PU_java_lang(Get_Current_PU()))
    return NULL;
  if (dna == NULL || call_stmt == NULL || cr == NULL)
    return NULL;
  BB_NODE *bb = call_stmt->Bb();
  STMTREP *load_stmt = NULL;
  char *file = NULL;
  while (bb != NULL) {
    load_stmt = bb->Last_stmtrep();
    if (load_stmt != NULL && load_stmt->Opr() == OPR_ICALL) {
      RNA_NODE *rna = dna->Get_callsite_rna(load_stmt);
      if (rna != NULL) {
        const CALLEE_VECTOR& callee_list = rna->Callee_list();
        for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
          DNA_NODE *callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(iter->Callee());
          if (callee == NULL)
            continue;
          if (strcmp(callee->Fname(), "_ZN4java4util10Properties4loadEJvPNS_2io11InputStreamE") == 0) {
            // InputStream of load()
            CODEREP *last_call_cr = load_stmt->Rhs()->Opnd(1);
            if (last_call_cr != NULL)
              last_call_cr = last_call_cr->Find_actual_arg();
            while (last_call_cr != NULL && last_call_cr->Kind() == CK_VAR) {
              STMTREP *defstmt = last_call_cr->Defstmt();
              if (defstmt != NULL && defstmt->Opr() == OPR_STID && defstmt->Rhs() != NULL)
                last_call_cr = defstmt->Rhs();
              else
                break;
            }
            // ClassLoader::getResourceAsStream initialized InputStream
            if (last_call_cr != NULL && last_call_cr->Kind() == CK_VAR) {
              STMTREP *defstmt = last_call_cr->Defstmt();
              if (defstmt != NULL && defstmt->Opr() == OPR_ICALL) {
                CODEREP *arg1 = defstmt->Rhs()->Opnd(1)->Find_actual_arg();
                // property file name from getResourceAsStream
                file = Find_const_char(dna, arg1);
              }
              if (file != NULL) {
                Is_Trace(Tracing(), (TFile, "RBC: Java.Util.Properties.load(\"%s\")\n", file));
                // evaluate the path for property file
                // path of .class file
                const char *wn_file = WHIRL_FILE_MANAGER::Get()->Get_file(dna->File_idx()).File_name();
                // evaluate package name
                char *dfname = Vsa_demangle(dna->Fname());
                char *pch = strstr(dfname, "(");
                if (pch && strlen(pch) > 1)
                  strncpy(pch, "\000\000", 1);
                // remove function name & class name from package name
                pch = strrchr(dfname, '.');
                if (pch && strlen(pch) > 1)
                  strncpy(pch, "\000\000", 1);
                pch = strrchr(dfname, '.');
                if (pch && strlen(pch) > 1)
                  strncpy(pch, "\000\000", 1);
                // package name to path format
                while ((pch = strstr(dfname, ".")) != NULL) {
                  strncpy(pch, "/", 1);
                }
                // path - package name + property file name
                size_t len = strlen(wn_file) + strlen(file) + 1;
                char *path = (char*)malloc(len);
                memset(path, 0, len);
                strcpy(path, wn_file);
                pch = strstr(path, dfname);
                if (dfname != NULL)
                  free(dfname);
                if (pch && (len - (pch - path)) > strlen(file)) {
                  strncpy(pch, file, strlen(file));
                  strncpy(pch+strlen(file), "\000\000", 1);
                  Is_Trace(Tracing(), (TFile, "RBC: Java.Util.Properties.load(\"%s\")\n", path));
                  return path;
                }
              }
            }
            return NULL;
          }
        }
      }
    }
    bb = bb->Prev();
  }
  return NULL;
}


// =============================================================================
//
// RBC_BASE::Find_const_char_cross: cross procedure find const string def
// a const
// @parm: str_vec - stores found constant str vector
// @ret: return TRUE if found const, otherwise FALSE
// =============================================================================
BOOL
RBC_BASE::Find_const_char_cross(VSA *vsa_ctx, STMTREP *stmt, CODEREP *cr, STR_SET& str_set, MEM_POOL *pool)
{
  BOOL found = FALSE;
  CONTEXT_SWITCH cur_ctx(vsa_ctx->Dna());
  // no trace, no srcpos
  VAR_DEF_HELPER helper(cr, stmt, vsa_ctx->Comp_unit(), FOR_GENERAL);
  helper.Set_follow_ctr_ud(TRUE);
  CHECK_OBJ check_obj(cr, stmt);
  vsa_ctx->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  DEF_INFO_VEC::iterator def_it;
  for (def_it = def_info_vec.begin(); def_it != def_info_vec.end(); def_it++) {
    DEF_INFO *def_info = *def_it;
    DNA_NODE *def_dna = def_info->Dna();
    CODEREP  *def_cr = def_info->Coderep();
    STMTREP  *def_stmt = def_info->Stmtrep();
    Is_True_Rbc(def_dna && def_cr, ("RBC_BASE: null def dna/cr"));
    CONTEXT_SWITCH def_ctx(def_dna);
    char *str = Find_const_char(def_dna, def_cr);
    char *prop_str = NULL;
    if (str == NULL && def_stmt && def_stmt->Opr() == OPR_ICALL) {
      prop_str = Eval__java_Property_getProperty(def_dna, def_stmt);
      str = prop_str;
    }
    if (str) {
      char *clone_str = Clone_string(str, pool);
      str_set.insert(clone_str);
    }
    if (prop_str) {
      free(prop_str);
    }
  }
  return (str_set.size() > 0) ? TRUE : FALSE;
}

// =============================================================================
//
// RBC_BASE::Find_const_char
// should copy the returned string, if want to use string after context switch
//
// =============================================================================
char*
RBC_BASE::Find_const_char(DNA_NODE *dna, CODEREP *cr)
{
  Is_True(cr != NULL, ("RBC ERROR: null cr passed to Find_const_char.\n"));
  if (cr == NULL)
    return NULL;
  char *ret = NULL;
  if (PU_java_lang(*dna->Pu())) {
    // Java constant string process
    if (cr->Kind() != CK_VAR && cr->Kind() != CK_IVAR)
      return NULL;
    if (cr->Kind() == CK_IVAR) {
      cr = cr->Ilod_base();
      if (cr == NULL || cr->Kind() != CK_VAR) {
        // maybe some cases should be handle later
        return NULL;
      }
    }
    UINT32 offset = cr->Offset()/Pointer_Size;
    ST *st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
    while (!ST_is_class_const_data(st)) {
      if (cr->Kind() != CK_VAR)
        break;
      if (cr->Defstmt() == NULL)
        break;
      cr = cr->Defstmt()->Rhs();
      if (cr == NULL)
        break;
      offset = cr->Offset()/Pointer_Size;
      if (cr->Kind() == CK_VAR)
        st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
      else if (cr->Kind() == CK_LDA)
        st = dna->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Lda_aux_id())->St();
    }
    ret = Get_class_const_value(st, offset);
  }
  else if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    if (cr->Kind() == CK_IVAR)
      cr = cr->Ilod_base();
    if (cr == NULL) {
      return NULL;
    }
    // C/C++ constant string process
    if (cr->Kind() != CK_LDA || ST_class(cr->Lda_base_st()) != CLASS_CONST ||
        TCON_ty(ST_tcon_val(cr->Lda_base_st())) != MTYPE_STR)
      return NULL;
    ret = Index_to_char_array(TCON_str_idx(ST_tcon_val(cr->Lda_base_st())));
  }
  else {
    Is_True(false,
            ("RBC: unsupport source language:%d\n", PU_src_lang(Get_Current_PU())));
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Register_rules: it adds loaded rules to RBC_BASE,
//
// =============================================================================
void
RBC_BASE::Register_rules(IPSA *ipsa, DNA_NODE *func,
                         vector<std::pair<char*, IDTYPE> > *links)
{
  CONTEXT_SWITCH context(func);

  RNODE_VECTOR *rna_list = func->Call_list();
  for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
    RNA_NODE *rna = (*rna_list)[i];
    if (!(rna->Is_flag_set(RBC_SE_ASSERT) || rna->Is_flag_set(RBC_SE_ASSERT_DNA)) ||
        rna->Is_rbc_op(RBC_OP_RBC_APPLY_RULE))
      continue;

    CODEREP *vcall = rna->Callstmt()->Rhs();
    // offset "this" parameter
    UINT32 offset = 0;
    if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
      offset = 1;
    }
    CODEREP *boolexp = vcall->Opnd(0 + offset)->Ilod_base();

    // #0 process source, implicit
    int source = PU_src_lang(Get_Current_PU());

    // #1 process rule name, required
    CODEREP *rname = vcall->Get_opnd(1 + offset);
    Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
    if (rname == NULL) {
      Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
      return;
    }
    char *name = Find_const_char(func, rname);
    Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
    if (name == NULL) {
      Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
      return;
    }

    // add rule to map
    RULE *rule = CXX_NEW(RULE(_last_rule_id, func->Dna_idx(), boolexp, name, source), _mem_pool);
    _last_rule_id++;
    Enter_rna_rule_map(rna->Rna_idx(), rule);

    // link rule
    for (vector<std::pair<char*, IDTYPE> >::const_iterator iter = links->begin();
         iter != links->end(); iter++)
    {
      char *expected = iter->first;
      UINT32 rna_idx = iter->second;
      if (strcmp(name, expected) == 0) {
        RULE *tmp = Get_rule(rna_idx);
        if (tmp == NULL) {
          Enter_rna_rule_map(rna_idx, rule);
        }
        else {
          Is_Trace(Tracing(),
                   (TFile,
                    "RBC ERROR: multiple implementations of rule \"%s\", only the first met one is attached, should not use Rbc_apply_rule\n",
                    expected));
        }
      }
    }
  }
  return;
}


// =============================================================================
//
// RBC_BASE::Get_param_list_types put type pointers of parameter lists of
//     function 'dna' into 'tlist'
//
// =============================================================================
void
RBC_BASE::Get_param_list_types(DNA_NODE *dna, vector<TY*> &tlist)
{
  tlist.clear();
  if (dna == NULL)
    return;

  CONTEXT_SWITCH ctx(dna);
  for (INT i = PDV_INIT_ID; i < dna->Parm_list()->size(); i++) {
    ST_IDX st_idx = (*dna->Parm_list())[i]->St_idx();
    TY *ty = Ty_ptr(dna->File_idx(), ST_type(ST_ptr(st_idx)));
    tlist.push_back(ty);
  }
}

// =============================================================================
//
// RBC_BASE::Use_fuzzy_name: detect a function need to use fuzzy name or original
//       name, passed parameter must be demangled function name,
//        and without parameter
//
// =============================================================================
BOOL
RBC_BASE::Use_fuzzy_name(char *demangled_func_name)
{
  if (demangled_func_name == NULL) {
    return FALSE;
  }
  char *la_pos = strchr(demangled_func_name, '<');
  // left angle position
  if (la_pos == NULL) {
    return FALSE;
  }
  char *last_pos = demangled_func_name + strlen(demangled_func_name);
  // previous char that is not space
  char *prev = last_pos - 1;
  // escape space before '<'
  while (prev >= demangled_func_name && *prev == ' ') {
    prev--;
  }
  const char *op_str = "operator";
  // template
  if (prev - demangled_func_name + 1 < strlen(op_str)) {
    return TRUE;
  }
  // detect operator < or operator <<
  if (strncmp(prev - strlen(op_str) + 1, op_str, strlen(op_str)) != 0) {
    // not found operator
    return TRUE;
  }
  // found operator
  if (*(la_pos + 1) == '<') {
    // operator <<
    la_pos += 1;
  }
  la_pos = strchr(la_pos, '<');
  return la_pos != NULL;
}

static const char *std_namespace_ignore_pattern[] = {
  "^__[[:digit:]]+::",
  "^__cxx[[:digit:]]+::",
};

// =============================================================================
//
// RBC_BASE::Convert_to_fuzzy_name: convert demangled name to fuzzy name,
//       the returned c string must be allocated in mem pool
//
// =============================================================================
char *
RBC_BASE::Convert_to_fuzzy_name(char *demangled_name, MEM_POOL *pool)
{
  char *op_pos = strstr(demangled_name, "operator");
  char *end_pos = demangled_name + strlen(demangled_name);
  char *ignore1 = NULL;
  char *ignore2 = NULL;
  if (op_pos != NULL) {
    op_pos += strlen("operator");
    while (op_pos < end_pos && *op_pos == ' ') {
      op_pos += 1;
    }
    if (op_pos < end_pos && *op_pos == '<') {
      ignore1 = op_pos;
      op_pos += 1;
      if (op_pos < end_pos && *op_pos == '<') {
        ignore2 = op_pos;
      }
    }
  }
  std::stack<char *> angle_pos_stack;
  std::string fuzzy_name_str("__XVSA_FUZZY_NAME::");
  // std namespace
  if (strncmp(demangled_name, "std::", strlen("std::")) == 0) {
    fuzzy_name_str.append("std::");
    demangled_name += strlen("std::");
    INT pattern_len = sizeof(std_namespace_ignore_pattern) / sizeof(const char *);
    regex_t **reg_exp_arr = Compile_pattern_arr(std_namespace_ignore_pattern, pattern_len, TRUE, pool);
    for (INT i = 0; i < pattern_len; i++) {
      regex_t *reg_exp = reg_exp_arr[i];
      if (reg_exp == NULL) {
        continue;
      }
      regmatch_t match_r[1];
      INT reti = (INT) regexec(reg_exp, demangled_name, 1, match_r, 0);
      if (!reti) {
        demangled_name += match_r[0].rm_eo;
      }
    }
  }
  for (char *p = demangled_name; p < demangled_name + strlen(demangled_name); p++) {
    if (p == ignore1 || p == ignore2) {
      continue;
    }
    if (*p == '<') {
      angle_pos_stack.push(p);
    } else if (!angle_pos_stack.empty() && *p == '>') {
      angle_pos_stack.pop();
    } else if (angle_pos_stack.empty()) {
      fuzzy_name_str.append(1, *p);
    }
  }
  char *fuzzy_name = (char *) MEM_POOL_Alloc(pool, fuzzy_name_str.size() + 1);
  strncpy(fuzzy_name, fuzzy_name_str.c_str(), fuzzy_name_str.size() + 1);
  return fuzzy_name;
}

// =============================================================================
//
// RBC_BASE::Init_dna_fuzzy_name_map: extract fuzzy name if needed, and cache,
//       if can find fuzzy name, need to use fuzzy name later for linking rbc
//        dna node and original dna node
//
// =============================================================================
void
RBC_BASE::Init_dna_fuzzy_name_map(IPSA *ipsa, ID_STR_MAP &dna_fuzzy_name_map)
{
  DNODE_VECTOR::const_iterator iter = ipsa->_post_order_dnode.begin();
  for (; iter != ipsa->_post_order_dnode.end(); iter++) {
    DNA_NODE *dna = *iter;
    if (strncmp(dna->Fname(), "_Z", 2) != 0) {
      continue;
    }
    if (!PU_cxx_lang(*(dna->Pu())) && !PU_c_lang(*(dna->Pu()))) {
      continue;
    }
    // now func name is mangled name
    char *dfname_without_param = cplus_demangle(dna->Fname(), DMGL_ANSI|DMGL_TYPES);
    BOOL use_fuzzy = Use_fuzzy_name(dfname_without_param);
    if (!use_fuzzy) {
      continue;
    }
    char *fuzzy_name = Convert_to_fuzzy_name(dfname_without_param, Loc_pool());
    dna_fuzzy_name_map[dna->Dna_idx()] = fuzzy_name;
    free(dfname_without_param);
  }
}

// =============================================================================
//
// RBC_BASE::Link_dna_for_rbc: it may have both source dna & rule dna for a
//     function, add rule dna in src dna's rbc_node and propoget rbc flags
//
// =============================================================================
void
RBC_BASE::Link_dna_for_rbc(IPSA *ipsa)
{
  Is_Trace(Tracing(), (TFile, "%sRBC: Link_dna_for_rbc:\n%s", DBar, DBar));

  hash_map<IDTYPE, DNA_NODE*> rbc_functional_map; // rbc dna map to functional dna
  // dna id => fuzzy name (for template method or some c++ methods have different mangle name)
  ID_STR_MAP dna_fuzzy_name_map(128, __gnu_cxx::hash<IDTYPE>(), __gnu_cxx::equal_to<IDTYPE>(), ID_STR_ALLOCATOR(Loc_pool()));
  Init_dna_fuzzy_name_map(ipsa, dna_fuzzy_name_map);
  UINT32 prop_flag = DNA_RBC_MODEL|DNA_RBC_ASSERT|DNA_RBC_TAG_OP|DNA_FSM_MODELLED|DNA_RBC_SE_EVAL|DNA_RBC_TAG_CREATE|DNA_RBC_ASSERT_DNA|DNA_THREAD_ENTRY|DNA_RBC_FUNC_TAG|DNA_RBC_GLOBAL_USED|DNA_RBC_DEFINE_CALL;
  DNODE_VECTOR::const_iterator iter = ipsa->_post_order_dnode.begin();
  for (; iter != ipsa->_post_order_dnode.end(); iter++) {
    DNA_NODE *dna = *iter;
    // a rbc dna, add to name => rbc_dna map
    if (dna->Non_functional()) {
      Add_rbc_node(dna, dna);
    }
    // set RNA_HAS_FUNCTIONAL for dna's callers
    INT i;
    for (i = VAR_INIT_ID; i < dna->Clby_list()->size(); i++) {
      RNA_NODE *rna = (*dna->Clby_list())[i];
      rna->Set_flag(dna->Non_functional() ? RNA_HAS_RBC_NODES : RNA_HAS_FUNCTIONAL);
    }
    char *dna_fuzzy_name = NULL;
    if (dna_fuzzy_name_map.find(dna->Dna_idx()) != dna_fuzzy_name_map.end()) {
      dna_fuzzy_name = dna_fuzzy_name_map[dna->Dna_idx()];
    }
    DNODE_VECTOR::const_iterator iter2 = iter + 1;
    for (; iter2 != ipsa->_post_order_dnode.end(); iter2++) {
      DNA_NODE *func = *iter2;
      char *func_fuzzy_name = NULL;
      if (dna_fuzzy_name_map.find(func->Dna_idx()) != dna_fuzzy_name_map.end()) {
        func_fuzzy_name = dna_fuzzy_name_map[func->Dna_idx()];
      }
      // same dna
      if (dna == func) {
        continue;
      }
      // one use fuzzy name, and another is not, can't link, just continue
      if ((dna_fuzzy_name == NULL && func_fuzzy_name != NULL) ||
            (dna_fuzzy_name != NULL && func_fuzzy_name == NULL)) {
        continue;
      }
      BOOL fuzzy_match = FALSE;
      if (dna_fuzzy_name != NULL) {
        if (strcmp(dna_fuzzy_name, func_fuzzy_name) == 0) {
          fuzzy_match = TRUE;
        } else {
          // use fuzzy name, but not match
          continue;
        }
      } else if (strcmp(dna->Fname(), func->Fname()) != 0) {
        // dna original name not match
        continue;
      }
      // name matched
      // for C source, compare parameter lists as well
      if (!fuzzy_match && (PU_c_lang(*dna->Pu()) || PU_c_lang(*func->Pu()))) {
        INT parm_size = dna->Parm_list()->size();
        // parameter numbers mismatch, skip
        if (func->Parm_list()->size() != parm_size) {
          Is_Trace(Tracing(),
                    (TFile,
                    "DNA_NODE(%d) & DNA_NODE(%d) match failed, diff parameter numbers\n",
                    dna->Dna_idx(), func->Dna_idx()));
          continue;
        }
        // need to compare each parameter type match as well
        vector<TY*> dna_parm_types, func_parm_types;
        // get parameter types of dna
        Get_param_list_types(dna, dna_parm_types);
        // get parameter types of func
        Get_param_list_types(func, func_parm_types);
        // compare types
        BOOL diff = FALSE;
        for (int i = 0; i < dna_parm_types.size(); i++) {
          TY *dna_ty = dna_parm_types[i];
          TY *func_ty = func_parm_types[i];
          if (dna_ty == NULL || func_ty == NULL ||
              dna_ty->kind != func_ty->kind) {
            diff = TRUE;
            Is_Trace(Tracing(),
                      (TFile,
                      "DNA_NODE(%d) & DNA_NODE(%d) match failed, diff type in parameter(%d)\n",
                      dna->Dna_idx(), func->Dna_idx(), i));
            break;
          }
        }
        // different signature, skip
        if (diff)
          continue;
      }
      if (!dna->Non_functional() || Get_rbc_nodes(dna)) {
        rbc_functional_map.insert(make_pair(func->Dna_idx(), dna));
      } else if (!func->Non_functional() || Get_rbc_nodes(func)) {
        rbc_functional_map.insert(make_pair(dna->Dna_idx(), func));
      }
      // try to propagate rbc flags to functional dna if there is
      if (!func->Non_functional() && dna->Is_set_rbc_flag(prop_flag)) {
        UINT32 flags = dna->Rbc_flags() & prop_flag;
        func->Set_rbc_flag(flags);
        Add_rbc_node(func, dna);
        Is_Trace(Tracing(), (TFile, "with FLAGS(0x%x)\n", flags));
        if(dna->Is_set_rbc_flag(DNA_FSM_MODELLED)) {
          Propagate_fsm_list(func, dna, Mem_pool());
        }
      }
      else if (func->Is_set_rbc_flag(prop_flag)) {
        UINT32 flags = func->Rbc_flags() & prop_flag;
        dna->Set_rbc_flag(flags);
        Add_rbc_node(dna, func);
        Is_Trace(Tracing(), (TFile, "with FLAGS(0x%x)\n", flags));
        if(func->Is_set_rbc_flag(DNA_FSM_MODELLED)) {
          Propagate_fsm_list(dna, func, Mem_pool());
        }
      }
    } // iter2
  } // iter

  // link dna with tagged function name's rule
  DNA_TAG_MAP::iterator dna_tag_iter;
  for (dna_tag_iter = _dna_tag_map.begin(); dna_tag_iter != _dna_tag_map.end(); dna_tag_iter++) {
    DNA_NODE *dna = ipsa->Get_dna(dna_tag_iter->first);
    TAG_INFOS *tag_infos = dna_tag_iter->second;
    Is_True(dna, ("no dna at index %d", dna_tag_iter->first));
    Is_True(tag_infos, ("null tag_infos for dna%s", dna->Fname()));
    if (tag_infos == NULL || dna == NULL) {
      continue;
    }

    hash_map<IDTYPE, DNA_NODE*>::iterator functional_iter = rbc_functional_map.find(dna->Dna_idx());
    if (functional_iter != rbc_functional_map.end()) {
      // switch to functional dna
      dna = functional_iter->second;
    }

    DNODE_VECTOR *dna_rbc_nodes = Get_rbc_nodes(dna);
    Is_True(dna_rbc_nodes, ("no rbc nodes for dna(%d):%s", dna->Dna_idx(), dna->Fname()));

    TAG_INFOS::iterator tag_info_iter;
    for (tag_info_iter = tag_infos->begin(); tag_info_iter != tag_infos->end(); tag_info_iter++) {
      const char *tag_name = (*tag_info_iter)->_tag_name;
      Is_Trace(Tracing(), (TFile, "Add rbc nodes with tag %s for dna %s : ", tag_name, dna->Fname()));
      IDTYPE tag_dnaidx = Get_dna_idx_by_name((char *)tag_name);
      if (tag_dnaidx == 0) {
        Is_Trace(Tracing(), (TFile, "no dna for tag %s\n", tag_name));
        continue;
      } else {
        DNA_NODE *tag_dna = ipsa->Get_dna(tag_dnaidx);
        Is_True(tag_dna, ("null tag dna"));
        if (tag_dna != NULL) {
          // shell we get rbc_nodes from tag_dna's first rbc node if functional dna does not exists?
          functional_iter = rbc_functional_map.find(tag_dna->Dna_idx());
          if (functional_iter != rbc_functional_map.end()) {
            // switch to functional dna
            tag_dna = functional_iter->second;
          }
          DNODE_VECTOR *tag_rbc_nodes = Get_rbc_nodes(tag_dna);
          if (tag_rbc_nodes) {
            for (DNODE_VECTOR::iterator rbc_iter = tag_rbc_nodes->begin();
                 rbc_iter != tag_rbc_nodes->end(); rbc_iter++) {
              DNA_NODE *rbc_dna = *rbc_iter;
              if (rbc_dna == NULL) {
                continue;
              }
              dna_rbc_nodes->push_back(rbc_dna);
              Is_Trace(Tracing(), (TFile, "Add DNA_NODE(%d) for %s with INDEX DNA_NODE(%d)\n",
                                   rbc_dna->Dna_idx(), rbc_dna->Fname(), dna->Dna_idx()));
            }
          }
          // propagate fsm for functions with same tag
          if(tag_dna->Is_set_rbc_flag(DNA_FSM_MODELLED)) {
            Propagate_fsm_list(dna, tag_dna, Mem_pool());
          }
        }
      }
    }
  }

  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> dna_iter(ipsa); !dna_iter.Is_end(); dna_iter.Next()) {
    // propagate fsm use from base class to child virtual function
    DNA_NODE *dna = dna_iter.Current();
    if(dna->Is_set_rbc_flag(DNA_FSM_MODELLED)) {
      Propagate_fsm_use(ipsa, dna);
    }
    // link annoation rule with dna
    if(dna->Has_annots()) {
      Link_rule_for_annot(ipsa, dna);
    }
  }
}


// =============================================================================
//
// RBC_BASE::Process_rbc_rule_exception
//    void Rbc_rule_exception(char *rule_name, bool condition)
//
// =============================================================================
void
RBC_BASE::Process_rbc_rule_exception(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL ||
      !dna->Is_set_rbc_flag(DNA_RBC_RULE_CFG) ||
      !rna->Is_rbc_op(RBC_OP_RBC_RULE_EXCEPTION))
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process rule name, required
  CODEREP *rname = vcall->Get_opnd(0 + offset);
  Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (rname == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }
  char *name = Find_const_char(dna, rname);
  Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (name == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }
  name = Clone_string(name, Mem_pool());

  // #2 process exception condition
  CODEREP *boolexp = vcall->Opnd(1 + offset)->Ilod_base();

  // add rule exception to map
  RULE_BODY *except = CXX_NEW(RULE_BODY(dna->Dna_idx(), boolexp), _mem_pool);
  Enter_rule_except_map(name, except);
}


// =============================================================================
//
// RBC_BASE::Process_rbc_same_as_func
//    void Rbc_same_as_func(char *func_name)
//
// =============================================================================
void
RBC_BASE::Process_rbc_same_as_func(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL)
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process function name
  CODEREP *arg1 = vcall->Get_opnd(0 + offset);
  Is_True(arg1 != NULL, ("RBC ERROR: func name is null"));
  if (arg1 == NULL) {
    return;
  }
  STRING func_name = Find_const_char(dna, arg1);
  Is_True(func_name != NULL, ("Rbc_annotate: empty func name"));
  if (func_name == NULL) {
    return;
  }

  // #2 find rbc node list of func name,
  //    copy its rbc nodes to current dna's rbc node list as well
  IDTYPE apl_rbc_idx = Get_dna_idx_by_name(func_name);
  DNA_NODE *apl_rbc_callee = ipsa->Get_dna(apl_rbc_idx);
  if (apl_rbc_callee != NULL) {
    DNODE_VECTOR *apl_rbc_nodes = Get_rbc_nodes(apl_rbc_callee);
    if (apl_rbc_nodes != NULL) {
      DNODE_VECTOR *ref_dna_list = Get_rbc_nodes(dna);
      if (ref_dna_list != NULL) {
        for (DNODE_VECTOR::const_iterator iter = apl_rbc_nodes->begin();
             iter != apl_rbc_nodes->end(); iter++) {
          DNA_NODE *rbc_dna = *iter;
          if (rbc_dna == NULL)
            continue;
          // set flags as well
          if (rbc_dna->Is_set_rbc_flag(DNA_RBC_ASSERT)) {
            dna->Set_rbc_flag(DNA_RBC_ASSERT);
            rna->Set_flag(RBC_SE_ASSERT);
          }
          if (rbc_dna->Is_set_rbc_flag(DNA_RBC_MODEL)) {
            dna->Set_rbc_flag(DNA_RBC_MODEL);
            rna->Set_flag(RBC_SE_MODEL);
          }
          ref_dna_list->push_back(rbc_dna);
          Is_Trace(Tracing(), (TFile, "Apply DNA_NODE(%d): %s to DNA_NODE(%d): %s\n",
                               rbc_dna->Dna_idx(), rbc_dna->Fname(),
                               dna->Get_rbc_ref_idx(), dna->Fname()));
        }
      }
      else {
        Is_Trace(Tracing(), (TFile, "RBC ERROR: NULL rbc node list for %s\n",
                             dna->Fname()));
      }
    }
  }
  else {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: %s has no rules to apply to %s\n",
                         func_name, dna->Fname()));
  }
}

// =============================================================================
//
// RBC_BASE::Process_rbc_rule_set :
//     void Rbc_set_rule_set(const char *rule_name, const char *rule_set)
//
// =============================================================================
void
RBC_BASE::Process_rbc_rule_set(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL)
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process rule name
  CODEREP *arg1 = vcall->Get_opnd(0 + offset);
  Is_True(arg1 != NULL, ("RBC ERROR: rule name is null"));
  if (arg1 == NULL) {
    return;
  }
  const char* rule_name = Find_const_char(dna, arg1);
  Is_True(rule_name != NULL, ("Rbc_set_rule_set: empty rule_name"));
  if (rule_name == NULL) {
    return;
  }

  // #2 process rule set name
  CODEREP *arg2 = vcall->Get_opnd(1 + offset);
  Is_True(arg2 != NULL, ("RBC ERROR: rule name is null"));
  if (arg2 == NULL) {
    return;
  }
  const char* rule_set = Find_const_char(dna, arg2);
  Is_True(rule_set != NULL, ("Rbc_set_rule_set: empty rule_set"));
  if (rule_set == NULL) {
    return;
  }
  RULE_NAME_SET_MAP::iterator rule_it = _rule_set_map.find(rule_name);
  if (rule_it != _rule_set_map.end()) {
    if (strcmp(rule_set, rule_it->second) != 0) {
      Is_True(FALSE,
              ("RBC ERROR: add rule[%s] to [%s]: already add to rule_set %s",
               rule_name, rule_set, rule_it->second));
      Is_Trace(Tracing(),
               (TFile,"Rbc_set_rule_set: add rule[%s] to rule_set[%s]: conflict with rule_set [%s]\n",
                rule_name, rule_set, rule_it->second));
    }
  } else {
    rule_name = Clone_string((char *)rule_name, Mem_pool());
    rule_set  = Clone_string((char *)rule_set, Mem_pool());
    _rule_set_map.insert(make_pair(rule_name, rule_set));
    Is_Trace(Tracing(),
              (TFile, "Rbc_set_rule_set: add rule[%s] to rule_set[%s]\n",
               rule_name, rule_set));
  }
}
// =============================================================================
//
// RBC_BASE::Process_rbc_annotate : add to _annot_rbc_map only
//     void Rbc_annotate(char *aname, bool expr)
//
// =============================================================================
void
RBC_BASE::Process_rbc_annotate(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL)
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process annotation name
  CODEREP *arg1 = vcall->Get_opnd(0 + offset);
  Is_True(arg1 != NULL, ("RBC ERROR: annotation name is null"));
  if (arg1 == NULL) {
    return;
  }
  STRING annot_name = Find_const_char(dna, arg1);
  Is_True(annot_name != NULL, ("Rbc_annotate: empty aname"));
  if (annot_name == NULL) {
    return;
  }
  annot_name = Clone_string(annot_name, Mem_pool());

  // #2 process rbc evaluation
  CODEREP *expr = vcall->Opnd(1 + offset)->Ilod_base();
  Is_True(expr != NULL, ("Rbc_annotate: null expr"));
  if (expr == NULL) {
    return;
  }
  // find expr def call cr
  if (expr->Kind() != CK_VAR)
    return;
  STMTREP *defstmt = expr->Defstmt();
  while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
    if (defstmt->Rhs() == NULL)
      break;
    expr = defstmt->Rhs();
    if (expr->Kind() == CK_VAR && defstmt != expr->Defstmt())
      defstmt = expr->Defstmt();
    else
      break;
  }

  // add annotation to map
  RULE_BODY *rb = CXX_NEW(RULE_BODY(dna->Dna_idx(), expr), _mem_pool);
  Enter_annot_rbc_map(annot_name, rb);
}


// =============================================================================
//
// RBC_BASE::Process_rbc_instr_range(ipsa, dna, rna)
//    Process __builtin_xvsa_range(int var, int lower, int upper),
//    add value range annotaion to VSA.
//    It's instrumentations on source codes, handle the call similar to
//    mvsa_model without setting DNA_NONE_FUNCTIONAL flag, which will skip
//    checks for the instrumented functions.
//
// =============================================================================
void
RBC_BASE::Process_rbc_instr_range(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL)
    return;

  VSA *vsa = dna->Comp_unit()->Vsa();
  if (vsa == NULL)
    vsa = CXX_NEW(VSA(dna->Comp_unit(), ipsa, ipsa->Mem_pool(), ipsa->Loc_pool()),
                  ipsa->Mem_pool());
  CODEREP *arg0 = model_call->Rhs()->Find_nth_arg(0);
  // TODO: for arg0, we may need to deal with
  // #1. after propagation, arg0 is replaced by some const CODEREP
  Is_True(arg0 != NULL, ("Invalid var."));
  if (arg0 == NULL)
    return;
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(1);
  Is_True(arg1 != NULL && arg1->Kind() == CK_CONST, ("Invalid lower bound."));
  if (arg1 == NULL || arg1->Kind() != CK_CONST)
    return;
  INT64 lower = (INT64)arg1->Const_val();
  CODEREP *arg2 = model_call->Rhs()->Find_nth_arg(2);
  Is_True(arg2 != NULL && arg2->Kind() == CK_CONST, ("Invalid upper bound."));
  if (arg2 == NULL || arg2->Kind() != CK_CONST)
    return;
  INT64 upper = (INT64)arg2->Const_val();
  vsa->Enter_cr_vr_map(arg0->Coderep_id(), lower, upper);
}


// =============================================================================
//
// RBC_BASE::Process_rbc_instr_ne(ipsa, dna, rna)
//    Process __builtin_xvsa_ne(int var, int value),
//    add value range annotaion to VSA.
//    It's instrumentations on source codes, handle the call similar to
//    mvsa_model without setting DNA_NONE_FUNCTIONAL flag, which will skip
//    checks for the instrumented functions.
//
// =============================================================================
void
RBC_BASE::Process_rbc_instr_ne(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL)
    return;

  VSA *vsa = dna->Comp_unit()->Vsa();
  if (vsa == NULL)
    vsa = CXX_NEW(VSA(dna->Comp_unit(), ipsa, ipsa->Mem_pool(), ipsa->Loc_pool()),
                  ipsa->Mem_pool());
  CODEREP *arg0 = model_call->Rhs()->Find_nth_arg(0);
  // TODO: for arg0, we may need to deal with
  // #1. after propagation, arg0 is replaced by some const CODEREP
  Is_True(arg0 != NULL, ("Invalid var."));
  if (arg0 == NULL)
    return;
  CODEREP *arg1 = model_call->Rhs()->Find_nth_arg(1);
  Is_True(arg1 != NULL && arg1->Kind() == CK_CONST, ("Invalid value."));
  if (arg1 == NULL || arg1->Kind() != CK_CONST)
    return;
  INT64 val = (INT64)arg1->Const_val();
  vsa->Enter_cr_vr_map(arg0->Coderep_id(), val, val);
}


// =============================================================================
//
// RBC_BASE::Process_rbc_instr_compare(ipsa, dna, rna)
//    Process __builtin_xvsa_compare(int var, char *opr, int value),
//    add value range annotaion to VSA.
//    It's instrumentations on source codes, handle the call similar to
//    mvsa_model without setting DNA_NONE_FUNCTIONAL flag, which will skip
//    checks for the instrumented functions.
//
// =============================================================================
void
RBC_BASE::Process_rbc_instr_compare(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *callstmt)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || callstmt == NULL)
    return;

  VSA *vsa = dna->Comp_unit()->Vsa();
  if (vsa == NULL)
    vsa = CXX_NEW(VSA(dna->Comp_unit(), ipsa, ipsa->Mem_pool(), ipsa->Loc_pool()),
                  ipsa->Mem_pool());
  CODEREP *arg0 = callstmt->Rhs()->Find_nth_arg(0);
  // TODO: for arg0, we may need to deal with
  // #1. after propagation, arg0 is replaced by some const CODEREP
  Is_True(arg0 != NULL, ("Invalid var."));
  if (arg0 == NULL)
    return;
  CODEREP *arg1 = callstmt->Rhs()->Find_nth_arg(1);
  Is_True(arg1 != NULL, ("Invalid opr"));
  if (arg1 == NULL)
    return;
  const char *opr = Find_const_char(dna, arg1);
  Is_True(opr != NULL, ("Invalid opr"));
  if (opr == NULL)
    return;
  OPERATOR cmp_op = Get_opr_from_char(opr);
  CODEREP *arg2 = callstmt->Rhs()->Find_nth_arg(2);
  Is_True(arg2 != NULL && arg2->Kind() == CK_CONST, ("Invalid value."));
  if (arg2 == NULL || arg2->Kind() != CK_CONST)
    return;
  INT64 val = (INT64)arg2->Const_val();
  switch(cmp_op) {
  case OPR_LT:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), INT64_MIN, val);
    break;
  case OPR_LE:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), INT64_MIN, val + 1);
    break;
  case OPR_GT:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), val + 1, INT64_MAX);
    break;
  case OPR_GE:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), val, INT64_MAX);
    break;
  case OPR_EQ:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), val, val + 1);
    break;
  case OPR_NE:
    vsa->Enter_cr_vr_map(arg0->Coderep_id(), val, val);
    break;
  default:
    break;
  }
}


// =============================================================================
//
// RBC_BASE::Process_rbc_disable_rule, must be called after rules loaded
//    void Rbc_disable_rule(char *rule_name)
//
// =============================================================================
void
RBC_BASE::Process_rbc_disable_rule(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL ||
      !dna->Is_set_rbc_flag(DNA_RBC_RULE_CFG) ||
      !rna->Is_rbc_op(RBC_OP_RBC_DISABLE_RULE))
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process rule name, required
  CODEREP *rname = vcall->Get_opnd(0 + offset);
  Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (rname == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }
  char *name = Find_const_char(dna, rname);
  Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (name == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }

  // search in rules loaded and set enable flag to false
  for (ID_MAP_ITER<RULE*, IDTYPE> iter(_rules_map); !iter.Is_end(); iter.Next()) {
    RULE *rule = iter.Node();
    char *rname = rule->Get_rule_name();
    if (strcmp(name, rname) == 0) {
      rule->Set_enable(FALSE);
      Is_Trace(Tracing(), (TFile, "RBC: disable rule(\"%s\") for RULE(%d)\n", rname, rule->Get_id()));
    }
  }
}


// =============================================================================
//
// RBC_BASE::Process_rbc_set_builtin
//    void Rbc_enable_builtin(char *rule_name)
//
// =============================================================================
void
RBC_BASE::Process_rbc_set_builtin(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, STMTREP *model_call)
{
  if (ipsa == NULL || dna == NULL || rna == NULL || model_call == NULL ||
      !dna->Is_set_rbc_flag(DNA_RBC_RULE_CFG) ||
      !rna->Is_rbc_op(RBC_OP_RBC_ENABLE_BUILTIN))
    return;

  CODEREP *vcall = model_call->Rhs();
  // offset "this" parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process rule name, required
  CODEREP *rname = vcall->Get_opnd(0 + offset);
  Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (rname == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }
  char *name = Find_const_char(dna, rname);
  Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (name == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }

  if (Is_equal(name, "CRF"))
    Set_builtin_check(RBCT_RECURSION);
  else
    Is_Trace(Tracing(), (TFile, "RBC ERROR: there's no builtin rule named \"%s\"\n", name));
}


// =============================================================================
//
// RBC_BASE::Process_rbc_exec_path perform checks on all execution paths
//
//    void For_all_exec_path(bool cond, char *rule_name)
//        param 'cond' is the assertion user would like to write,
//        param 'rule_name' is the rule name user would use,
//
//    We might have hundreds of paths violate user’s condition,
//    reporting all will generate huge output, so our APIs
//    provide evaluations only on max/min values, so that there won't be
//    too many issues to report
//
//    Here, we do bottom up traversal to do evaluation and report paths from
//    root entry to leaf, a leaf is a function without callee or all it's
//    callees are non-functional
//
// =============================================================================
void
RBC_BASE::Process_rbc_exec_path(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, MEM_POOL *pool)
{
  if (ipsa == NULL || dna == NULL || rna == NULL ||
      !dna->Is_set_rbc_flag(DNA_RBC_RULE_CFG) ||
      !rna->Is_flag_set(RBC_SE_EXEC_PATH))
    return;

  CODEREP *vcall = rna->Callstmt()->Rhs();
  // offset "this" parameter, C++ APIs has 'rbc' this pointer as its first parameter,
  // Java APIs are all static so they don't have 'RBC_ENGINE' this pointer as its first parameter
  UINT32 offset = 0;
  if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
    offset = 1;
  }

  // #1 process parameter 'rule_name' of 'For_all_exec_path'
  CODEREP *rname = vcall->Get_opnd(1 + offset);
  Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (rname == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }
  char *name = Find_const_char(dna, rname);
  Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
  if (name == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
    return;
  }

  // #2 process parameter 'cond' of 'For_all_exec_path',
  // it is expected to be a bool expression,
  // it may be in the form of "API compare_op CONST" or "CONST compare_op API"
  CODEREP *boolexp = vcall->Opnd(0 + offset)->Ilod_base();
  Is_True(boolexp != NULL && boolexp->Kind() == CK_OP,
          ("RBC ERROR: expect bool expr in the first parameter.\n"));
  if (boolexp == NULL || boolexp->Kind() != CK_OP) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: expect bool expr in the first parameter.\n"));
    return;
  }
  CODEREP *lhs = boolexp->Opnd(0);
  CODEREP *rhs = boolexp->Opnd(1);
  CODEREP *func_cr = lhs;
  OPCODE cmp_op = boolexp->Op();
  Is_True(OPERATOR_is_compare(boolexp->Opr()),
          ("RBC ERROR: expect bool expr in the first parameter.\n"));
  if (!OPERATOR_is_compare(boolexp->Opr())) {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: expect bool expr in the first parameter.\n"));
    return;
  }
  BOOL const_on_left = FALSE;
  // identify if the bool expr is in form of "API > CONST" or "CONST > API"
  if (lhs->Kind() == CK_CONST) {
    const_on_left = TRUE;
    func_cr = rhs;
  }
  else if (rhs->Kind() != CK_CONST)
    // have no const in the bool expr, "API cmp API" is not supported now
    return;
  // find defstmt of 'cond', identify the api used in 'cond'
  STMTREP *defstmt = NULL;
  if (func_cr->Kind() == CK_VAR)
    defstmt = func_cr->Defstmt();
  while(defstmt != NULL && defstmt->Opr() == OPR_STID) {
    if (defstmt->Rhs() == NULL)
      break;
    func_cr = defstmt->Rhs();
    if (func_cr->Kind() == CK_VAR && defstmt != func_cr->Defstmt())
      defstmt = func_cr->Defstmt();
    else
      break;
  }
  if (defstmt == NULL || defstmt->Opr() != OPR_CALL)
    return;
  // API name
  char *fname = Prune_func_name(ST_name(defstmt->St()));
  // used eval_kind later on for different APIs
  PATH_EVAL_KIND eval_kind = PATH_EVAL_KIND_NONE;
  if (Is_equal(fname, "Get_max_call_depth"))
    eval_kind = PATH_MAX_CALL_DEPTH;
  else if (Is_equal(fname, "Get_max_stack_size"))
    eval_kind = PATH_MAX_STACK_SIZE;
  if (fname)
    free(fname);

  if (eval_kind == PATH_EVAL_KIND_NONE)
    // not recognized API, just return
    return;

  // bottom up traversal evaluating max value,
  // dna index starting from 1, we add one more to avoid OOB access
  UINT64 dna_count = ipsa->_post_order_dnode.size() + 1;
  // values record max value of all callee, indexes record rna/dna index of max value
  UINT64 *values = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  UINT64 *indexes = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  for (int i = 0; i < dna_count; i++) {
    values[i] = 0;
    indexes[i] = 0;
  }
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> dna_iter(ipsa); !dna_iter.Is_end(); dna_iter.Next()) {
    DNA_NODE *func = dna_iter.Current();
    Is_True(func->Dna_idx() < dna_count, ("RBC ERROR: FUNC INDEX(%d) out of BOUND(%lld)\n",
                                          func->Dna_idx(), dna_count));
    if (func->Dna_idx() >= dna_count) {
      Is_Trace(Tracing(), (TFile, "RBC ERROR: FUNC INDEX(%d) out of BOUND(%lld)\n", func->Dna_idx(), dna_count));
      return;
    }
    UINT64 max_callee_value = 0;
    UINT64 max_callee_index = 0;
    // find max value of "func"'s functional callee
    for (CALLEE_ITER callee_iter(ipsa, func); !callee_iter.Is_end(); callee_iter.Next()) {
      RNA_NODE *callee_rna = callee_iter.Current_callsite();
      DNA_NODE *callee = callee_iter.Current();
      if (callee != NULL && !callee->Non_functional()) {
        // all callees must have been processed in bottom up traversal
        // so their index must be fine and won't have OOB access,
        // and we don't need to check here
        if (values[callee->Dna_idx()] > max_callee_value) {
          max_callee_value = values[callee->Dna_idx()];
          // rna & dna index, used to print out error message later
          max_callee_index = (UINT64)callee_rna->Rna_idx() << 32 | (UINT64)callee->Dna_idx();
        }
      }
    }
    // current function's value of given API
    UINT64 self_value = 0;
    if (eval_kind == PATH_MAX_CALL_DEPTH) {
      // call depth
      self_value = 1;
    }
    else if (eval_kind == PATH_MAX_STACK_SIZE) {
      // stack size
      self_value = func->Stack_size();
    }
    // update max value & rna/dna index of current function
    values[func->Dna_idx()] = max_callee_value + self_value;
    indexes[func->Dna_idx()] = max_callee_index;
  }

  // evaluate the condition and print out errors
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func == NULL || func->Non_functional() || !func->Is_root_entry())
      continue;

    // eval the condition
    UINT64 lhs_ui64val = 0;
    UINT64 rhs_ui64val = 0;
    BOOL ret = TRUE;
    if (const_on_left) {
      // condition in "const > API" form
      lhs_ui64val = lhs->Const_val();
      rhs_ui64val = values[func->Dna_idx()];
    }
    else {
      // condition in "API > const" form
      lhs_ui64val = values[func->Dna_idx()];
      rhs_ui64val = rhs->Const_val();
    }
    UINT64_COMPARER comparer(OPCODE_operator(cmp_op));
    ret = comparer(lhs_ui64val, rhs_ui64val);
    if (ret)
      continue;

    // exceed limit, generate path info from root to leaf & report error
    // "root -> foo() -> bar() -> ... -> leaf()"
    CONTEXT_SWITCH context(func);
    // message, MAX_DEPTH or MAX_STACK_SIZE, to print out in var field
    char *var_name = (char*)CXX_NEW_ARRAY(BOOL, SIZE_MAX_STR, pool);
    if (eval_kind == PATH_MAX_CALL_DEPTH) {
      snprintf(var_name, SIZE_MAX_STR, "CALL DEPTH:%lld", values[func->Dna_idx()]);
      Is_Trace(Tracing(), (TFile, "CALL DEPTH(%lld): \"%s\"", values[func->Dna_idx()], func->Fname()));
    }
    else if (eval_kind == PATH_MAX_STACK_SIZE) {
      snprintf(var_name, SIZE_MAX_STR, "STACK SIZE:%lld", values[func->Dna_idx()]);
      Is_Trace(Tracing(), (TFile, "STACK SIZE(%lld): \"%s\"", values[func->Dna_idx()], func->Fname()));
    }
    SRCPOS_HANDLE srcpos_h(func, pool);
    // st_pos for root info
    SRCPOS st_pos = ST_Srcpos(*func->St());
    if (st_pos == 0)
      st_pos = func->Comp_unit()->Cfg()->Entry_spos();
    srcpos_h.Append_data(func->St(), NULL, func, PATHINFO_ST_DECLARE);
    srcpos_h.Set_orig_stname(var_name);
    VSA *vsa = func->Comp_unit()->Vsa();
    DNA_NODE *callee = func;
    // path info for all max value callees
    while (callee != NULL && indexes[callee->Dna_idx()] != 0) {
      IDTYPE rna_idx = (IDTYPE)(indexes[callee->Dna_idx()] >> 32 & 0xffffffff);
      RNA_NODE *callee_rna = ipsa->Get_rna(rna_idx);
      if (callee_rna == NULL)
        break;
      srcpos_h.Append_data(callee_rna->Callstmt(), callee, PATHINFO_DNA_CALLSITE);
      IDTYPE dna_idx = (IDTYPE)(indexes[callee->Dna_idx()] & 0xffffffff);
      callee = ipsa->Get_dna(dna_idx);
      if (callee == NULL)
        break;
      Is_Trace(Tracing(), (TFile, " -> \"%s\"", callee->Fname()));
    }
    Is_Trace(Tracing(), (TFile, "\n"));
    Report_rbc_error(vsa, st_pos, name, FALSE, &srcpos_h);
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Process_rbc_exec_path: DNA COUNT(%lld)\n", dna_count));
}

// =============================================================================
//
// RBC_BASE::Process_rbc_for_all_func: Bind the exec_expr to all dna
//    BOOL For_all_func(BOOL exec_expr)
//      @parm exec_expr: evaluate expression
//    Evaluate given expression on all function
// exec_expr only support Rbc_assert for now
//=============================================================================
void
RBC_BASE::Process_rbc_for_all_func(IPSA *ipsa, DNA_NODE *enclosing_dna, RNA_NODE *model, STMTREP *model_call)
{
  Is_Trace(Tracing(),
           (TFile, "RBC_BASE::Process_rbc_for_all_func bind rule DNA_NODE(%d) to all dna\n",
            enclosing_dna->Dna_idx()));

  STMTREP *assert_call = Get_rbc_nth_call(model_call, 0 + Rbc_parm_offset(enclosing_dna));
  if (assert_call == NULL || assert_call->Opr() != OPR_CALL) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE:: unable to get assert_call\n"));
    return;
  }
  RNA_NODE *assert_rna = enclosing_dna->Get_callsite_rna(assert_call);
  if (assert_rna && Is_rbc_assert(assert_rna)) {
    // Link all functional dna with enclosing dna
    // all functional dna should be created when processing rule file
    for (INT dna_idx = IPSA::DNA_INIT_IDX; dna_idx < ipsa->Dna_count(); dna_idx++) {
      DNA_NODE *dna = ipsa->Get_dna(dna_idx);
      if (dna == NULL || dna->Non_functional()) {
        continue;
      }
      dna->Set_rbc_flag(enclosing_dna->Rbc_flags() & (DNA_RBC_ASSERT_DNA | DNA_RBC_ASSERT));
      Add_rbc_node(dna, enclosing_dna);
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC_BASE:For_all_func first arg not Rbc_assert\n"));
  }
}

// =============================================================================
//
// RBC_BASE::Link_rule_for_rbc: perform rule linking when we have
//      Rbc_apply_rule calls
//
// =============================================================================
void
RBC_BASE::Link_rule_for_rbc(IPSA *ipsa)
{
  Is_Trace(Tracing(), (TFile, "%sRBC: Link_rule_for_rbc:\n%s", DBar, DBar));

  // collect rules in Rbc_apply_rule calls
  vector<std::pair<char*, IDTYPE> > rules_to_link;
  rules_to_link.clear();
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (!func->Non_functional() || !func->Is_set_rbc_flag(DNA_RBC_RULE_CFG))
      continue;
    CONTEXT_SWITCH context(func);

    RNODE_VECTOR *rna_list = func->Call_list();
    for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
      RNA_NODE *rna = (*rna_list)[i];
      if (!rna->Is_rbc_op(RBC_OP_RBC_APPLY_RULE))
        continue;

      CODEREP *vcall = rna->Callstmt()->Rhs();
      // offset "this" parameter
      UINT32 offset = 0;
      if (PU_c_lang(Get_Current_PU()) || PU_cxx_lang(Get_Current_PU())) {
        offset = 1;
      }

      // #1 process rule name, required
      CODEREP *rname = vcall->Get_opnd(0 + offset);
      Is_True(rname != NULL, ("RBC ERROR: rule name is empty.\n"));
      if (rname == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
        return;
      }
      char *name = Find_const_char(func, rname);
      Is_True(name != NULL, ("RBC ERROR: rule name is empty.\n"));
      if (name == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC ERROR: rule name is empty.\n"));
        return;
      }
      rules_to_link.push_back(std::make_pair(name, rna->Rna_idx()));
    }
  }

  // register rules and link
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional() && func->Is_set_rbc_flag(DNA_RBC_ASSERT))
      Register_rules(ipsa, func, &rules_to_link);
  }

  // process rule configurations: declare, disable, exception
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (!func->Non_functional() || !func->Is_set_rbc_flag(DNA_RBC_RULE_CFG))
      continue;
    CONTEXT_SWITCH context(func);

    RNODE_VECTOR *rna_list = func->Call_list();
    for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
      RNA_NODE *rna = (*rna_list)[i];
      switch (rna->Rbc_op()) {
        case RBC_OP_RBC_DISABLE_RULE:
          Process_rbc_disable_rule(ipsa, func, rna, rna->Callstmt());
          break;
        case RBC_OP_RBC_RULE_EXCEPTION:
          Process_rbc_rule_exception(ipsa, func, rna, rna->Callstmt());
          break;
        case RBC_OP_RBC_ENABLE_BUILTIN:
          Process_rbc_set_builtin(ipsa, func, rna, rna->Callstmt());
        break;
        case RBC_OP_RBC_SAME_AS_FUNC:
          Process_rbc_same_as_func(ipsa, func, rna, rna->Callstmt());
        break;
        default:
          continue;
      }
    }
  }
}

// =============================================================================
//
// RBC_BASE::Link_rule_for_annot: perform rule linking for dna's with annotation
//   Apply annotation's model_decl rule to dna by call Process_model_decl_func
//   to perform Init__xxx functions(mark side-effects in INIT_DNA phase)
//
// =============================================================================
void
RBC_BASE::Link_rule_for_annot(IPSA *ipsa, DNA_NODE *dna)
{
  vector<STRING> annot_names;
  if(!dna->Get_annots(annot_names))
    return;

  for(vector<STRING>::iterator it = annot_names.begin();
      it != annot_names.end(); it++) {
    STRING annot_name = *it;
    if(annot_name == NULL)
      continue;
    RULE_BODY_VEC *rb_vec = Get_annot_rbc(annot_name);
    if (rb_vec == NULL)
      continue;
    for (INT i = 0; i < rb_vec->size(); i++) {
      RULE_BODY *rb = (*rb_vec)[i];
      if(rb == NULL)
        continue;
      DNA_NODE *rbc_dna = rb->Get_rbc_dna(ipsa);
      if (rbc_dna == NULL) {
        Is_Trace(Tracing(),
          (TFile, "RBC_BASE::Link_rule_for_annotate: null rbc_dna for ANNOT(\"%s\") IDX(%d)\n",
           annot_name, i));
        continue;
      }
      CONTEXT_SWITCH context(rbc_dna);
      STMTREP *rule_sr = rb->Get_rbc_sr();
      if(rule_sr == NULL || !OPERATOR_is_call(rule_sr->Opr()))
        continue;
      RNA_NODE *rna = rbc_dna->Get_callsite_rna(rule_sr);
      if (rna && Is_rbc_model_decl(rna)) {
        Process_model_decl_func(ipsa, dna, rna, rna->Callstmt());
      }
    }
  }
}

TAG_INFO*
RBC_BASE::Find_rna_tag_info(IPSA *ipsa, const char *tag_name, RNA_NODE *rna)
{
  const CALLEE_VECTOR& callee_list = rna->Callee_list();
  CALLEE_VECTOR::const_iterator iter;
  DNA_NODE *callee = NULL;
  for (iter = callee_list.begin(); iter != callee_list.end(); ++iter) {
    callee = ipsa->Get_dna(iter->Callee());
    if (callee) {
      TAG_INFO *tag_info = Find_dna_tag_info(tag_name, callee);
      if (tag_info) {
        return tag_info;
      }
    }
  }
  return NULL;
}

CODEREP *
RBC_BASE::Get_tag_obj(VSA *vsa, const char *tag_name, RNA_NODE *caller_rna, UINT32 index)
{
  IPSA *ipsa = vsa->Ipsa();
  TAG_INFO *tag_info = Find_rna_tag_info(ipsa, tag_name, caller_rna);
  if (tag_info) {
    Rbc_init();
    CXX_MEM_POOL temp_pool("Get_tag_obj pool", FALSE);
    DNA_NODE *model_dna = ipsa->Get_dna(tag_info->Dna_idx());
    RBC_CONTEXT rbc_ctx(vsa->Dna(), model_dna, model_dna, caller_rna, temp_pool());
    CODEREP *caller_cr = Get_tag_obj(rbc_ctx, tag_info, index);
    return caller_cr;
  }
  return NULL;
}

CODEREP *
RBC_BASE::Get_tag_obj(RBC_CONTEXT &rbc_ctx, TAG_INFO *tag_info, UINT32 index)
{
  RBC_EVAL_SKIP();
  if (tag_info && tag_info->Tag_objs() && index < tag_info->Tag_objs()->size()) {
    CODEREP *tag_obj = tag_info->Tag_objs()->at(index);
    DNA_NODE *model_dna = rbc_ctx.Ipsa()->Get_dna(tag_info->Dna_idx());
    Is_True_Rbc(model_dna, ("RBC ERROR: null tag model dna"));
    CONTEXT_SWITCH model_ctx(model_dna);
    if (tag_obj->Kind() == CK_CONST && tag_obj->Const_val() == 0) {
      Is_Trace(Tracing(), (TFile, "not set at rbc model %s", model_dna->Fname()));
      Rbc_eval_certainty()->push_back(REC_SKIP);
      return NULL;
    }
    // adjust to tag model dna to eval tag_obj
    DNA_NODE *saved_callee = rbc_ctx.Rbc_node();
    TAG_INFO *saved_tag_info = rbc_ctx.Tag_info();
    rbc_ctx.Set_rbc_node(model_dna);
    rbc_ctx.Set_tag_info(NULL);
    CODEREP *tag_ret = (CODEREP *)Eval__exp(rbc_ctx, tag_obj);
    // switch back
    rbc_ctx.Set_rbc_node(saved_callee);
    rbc_ctx.Set_tag_info(saved_tag_info);
    return tag_ret;
  } else {
    Is_Trace(Tracing(), (TFile, "Get_tag_obj(%s) at index %d out of bound\n", tag_info->Tag_name(), index));
    Rbc_eval_certainty()->push_back(REC_SKIP);
    return NULL;
  }
}
// =============================================================================
//
// RBC_BASE::Build_rbc_func_name_map: build map
//          thus can link rbc node with candidate
//          key: function name, value: dna id
//
// =============================================================================
void
RBC_BASE::Build_rbc_func_name_map(IPSA *ipsa)
{
  INT dna_count = ipsa->Dna_count();
  for (INT i = IPSA::DNA_INIT_IDX; i < dna_count; ++i ) {
    DNA_NODE *func = ipsa->Get_dna(i);
    if (!func->Is_set_rbc_flag(DNA_RBC_ASSERT|DNA_RBC_MODEL))
      continue;
    CONTEXT_SWITCH context(func);
    VIRFUNC_INFO_VEC *candidates = ipsa->Glob_cha()->Find_candidate_functions(func->Fname(), Loc_pool());
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Build_rbc_func_name_map: Add rbc function's all candidates, func: %s, candidate number: %ld\n",
                            func->Fname(), candidates ? candidates->size():0));
    if (candidates) {
      VIRFUNC_INFO_VEC_ITER iter;
      for (iter = candidates->begin(); iter != candidates->end(); ++iter) {
        VIRFUNC_INFO *func_info = *iter;
        char *func_name = ST_name(func_info->File_idx(), func_info->Fun_st());
        _func_dna_map[func_name] = func->Dna_idx();
      }
    }
  }
}


// =============================================================================
//
// RBC_BASE::Process_rbc_builtin : process the list of RBC builtin declarative
//                                 functions including:
//             rbc.Rbc_apply_rule(Rule_name);
//             rbc.Rbc_assert(Predicate, Error_num);
//             rbc.Model_decl(Declarative_functions);
//             rbc.Get_arg(arg_num)
//             rbc.Get_mem_size(symbol_name)
//             rbc.Get_value(symbol_name)
//
// =============================================================================
void
RBC_BASE::Process_rbc_builtin(IPSA *ipsa, DNA_NODE *dna, RNA_NODE *rna, ST *st)
{
  if (rna == NULL) return;
  // Evaluate RBC built in APIs,
  // mark DNA side effects and some of RNA attributes here
  switch(rna->Rbc_op()) {
    case RBC_OP_RBC_ASSERT:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_ASSERT);
      rna->Set_flag(RBC_SE_ASSERT);
      Process_rbc_assert_func(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_MODEL_DECL:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_MODEL);
      rna->Set_flag(RBC_SE_MODEL);
      Process_model_decl_func(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_RBC_APPLY_RULE:
      // it must be an assert, set flags here
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_ASSERT | DNA_RBC_RULE_CFG);
      rna->Set_flag(RBC_SE_ASSERT);
      break;
    case RBC_OP_RBC_SAME_AS_FUNC:
      // do not set RBC assert / model flags here as we won't know what there will be,
      // real rules will be evaluated via rbc nodes copied from apply source,
      // we can propagate RBC flags to user source node anyway.
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_RULE_CFG);
      break;
    case RBC_OP_RBC_SET_RULE_SET:
      Process_rbc_rule_set(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_RBC_RULE_EXCEPTION:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_RULE_CFG);
      break;
    case RBC_OP_RBC_DISABLE_RULE:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_RULE_CFG);
      break;
    case RBC_OP_RBC_ENABLE_BUILTIN:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_RULE_CFG);
      break;
    case RBC_OP_FOR_ALL_EXEC_PATH:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_RULE_CFG);
      rna->Set_flag(RBC_SE_EXEC_PATH);
      break;
    case RBC_OP_FOR_ALL_FUNC:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      Process_rbc_for_all_func(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_RBC_ANNOTATE:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      Process_rbc_annotate(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_XVSA_RANGE:
      // it's in source code annotation, so
      // do not set DNA_NONE_FUNCTIONAL flag here
      Process_rbc_instr_range(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_XVSA_NE:
      // it's in source code annotation, so
      // do not set DNA_NONE_FUNCTIONAL flag here
      Process_rbc_instr_ne(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_XVSA_COMPARE:
      // it's in source code annotation, so
      // do not set DNA_NONE_FUNCTIONAL flag here
      Process_rbc_instr_compare(ipsa, dna, rna, rna->Callstmt());
      break;
    case RBC_OP_RBC_GLOBAL_USED:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_GLOBAL_USED);
      rna->Set_flag(RBC_SE_GLOBAL_USED);
      break;
    case RBC_OP_RBC_DEFINE_CALL:
      dna->Set_flag(DNA_NONE_FUNCTIONAL);
      dna->Set_rbc_flag(DNA_RBC_DEFINE_CALL);
      rna->Set_flag(RBC_SE_DEFINE_CALL);
      break;
    default:
      // do nothing
      break;
  }
}

// =============================================================================
//
// RBC_BASE::Eval__mvsa_model
//
// =============================================================================
void
RBC_BASE::Eval__mvsa_model(DNA_NODE *caller, RNA_NODE *caller_rna, MEM_POOL *pool)
{
  Is_True_Ret(caller_rna && caller_rna->Is_flag_set(RBC_SE_MODEL),
              ("RBC_BASE::Eval__mvsa_model: not model rna"));
  const CALLEE_VECTOR& callee_list = caller_rna->Callee_list();
  CALLEE_VECTOR::const_iterator end = callee_list.end();
  IPSA *ipsa = caller->Comp_unit()->Vsa()->Ipsa();
  for (CALLEE_VECTOR::const_iterator it = callee_list.begin();
       it != end; ++it) {
    DNA_NODE *callee = ipsa->Get_dna(it->Callee());
    if (callee) {
      // fetch its rbc nodes list first
      DNODE_VECTOR *rbc_nodes = ipsa->Rbc()->Get_rbc_nodes(callee);
      if (rbc_nodes == NULL)
        continue;
      DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
      for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
        DNA_NODE *rbc_callee = *rbc_iter;
        // evaluate if there's a model in the rbc node
        if (rbc_callee != NULL && rbc_callee->Is_set_rbc_flag(DNA_RBC_MODEL)) {
          Is_Trace(Tracing(), (TFile,
                               "RBC_BASE::Eval__mvsa_model: evaluate RBC model for %s via RBC NODE(%d)\n",
                               callee->Fname(), rbc_callee->Dna_idx()));
          // the context management is tricky because RNA uses caller's context
          // while the model itself is in the callee's context
          Is_True(Rbc_parm_offset(caller) == Rbc_parm_offset(rbc_callee),
                  ("RBC ERROR: imcompatible language type of RBC APIs and sources.\n"));
          if (Rbc_parm_offset(caller) != Rbc_parm_offset(rbc_callee))
            return;

          CONTEXT_SWITCH context(rbc_callee);

          RNODE_VECTOR *rna_list = rbc_callee->Call_list();
          for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
            if (!(*rna_list)[i]->Is_flag_set(RBC_SE_MODEL))
              continue;

            CODEREP *vcall = (*rna_list)[i]->Callstmt()->Rhs();
            INT actual_count = vcall->Opr() == OPR_ICALL ? vcall->Kid_count() - 1
                                                         : vcall->Kid_count();
            // ignore callsite if parameter count mismatch
            if (Rbc_parm_offset(caller) >= actual_count)
              continue;

            CODEREP *stmt = vcall->Opnd(0 + Rbc_parm_offset(caller))->Ilod_base();

            Rbc_init();
            RBC_CONTEXT rbc_ctx(caller, callee, rbc_callee, caller_rna, pool);
            if (callee->Is_set_rbc_flag(DNA_RBC_FUNC_TAG)) {
              TAG_INFO *tag_info = Find_dna_tag_info(rbc_callee->Fname(), callee);
              if (tag_info) {
                rbc_ctx.Set_tag_info(tag_info);
              }
            }
            UINT64 retv = Eval__exp(rbc_ctx, stmt);
            if (retv == 0) {
              Is_Trace(Tracing(), (TFile, "RBC ERROR: failed modeling: %s\n", callee->Fname()));
            }
          }
        }
      }
      if(callee->Has_annots()) {
        Is_Trace(Tracing(), (TFile,
                             "VSA::Handle call: evaluate RBC annotation for %s\n",
                             callee->Fname()));
        ipsa->Rbc()->Eval__annot_model(caller, caller_rna, callee, pool);
      }
    }
  }
}

// =============================================================================
//
// RBC_BASE::Eval__annot_model
//
// =============================================================================
void
RBC_BASE::Eval__annot_model(DNA_NODE *caller, RNA_NODE *caller_rna, DNA_NODE *callee, MEM_POOL *pool)
{
  if (callee == NULL)
    return;

  vector<STRING> annot_names;
  if(!callee->Get_annots(annot_names))
    return;
  for(vector<STRING>::iterator it = annot_names.begin();
      it != annot_names.end(); it++) {
    STRING annot_name = *it;
    if(annot_name == NULL)
      continue;
    RULE_BODY_VEC *rb_vec = Get_annot_rbc(annot_name);
    if (rb_vec == NULL)
      continue;

    VSA *vsa = caller->Comp_unit()->Vsa();
    for (INT i = 0; i < rb_vec->size(); i++) {
      RULE_BODY *rb = (*rb_vec)[i];
      if (rb == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_model: null RULE_BODY for ANNOTATION(\"%s\") INDEX(%d)\n",
                            annot_name, i));
        continue;
      }
      DNA_NODE *rbc_dna = rb->Get_rbc_dna(vsa->Ipsa());
      if (rbc_dna == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_model: null rbc_dna for ANNOTATION(\"%s\") INDEX(%d)\n",
                            annot_name, i));
        continue;
      }
      CONTEXT_SWITCH context(rbc_dna);
      STMTREP *rule_sr = rb->Get_rbc_sr();
      if (rule_sr == NULL || !OPERATOR_is_call(rule_sr->Opr()))
        continue;
      RNA_NODE *rule_rna = rbc_dna->Get_callsite_rna(rule_sr);
      // evaluate only Model_decl
      if (rule_rna && Is_rbc_model_decl(rule_rna)) {
        CODEREP *vcall = rule_sr->Rhs();
        CODEREP *stmt = vcall->Opnd(0 + Rbc_parm_offset(caller))->Ilod_base();

        Rbc_init();
        RBC_CONTEXT rbc_ctx(caller, callee, rbc_dna, caller_rna, pool);
        UINT64 retv = Eval__exp(rbc_ctx, stmt);
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_model: eval ANNOTATION(\"%s\") for FUNC(\"%s\"): %lld\n",
                            annot_name, callee->Fname(), retv));
      }
    } // end for rb_vec
  } // end of annot
}

void
RBC_BASE::Eval__mvsa_assert(DNA_NODE *caller, RNA_NODE *caller_rna, RNA_FLAGS assert_flag)
{
  Is_True_Ret(caller, ("RBC_BASE::Eval__mvsa_assert: null caller"));
  IPSA *ipsa = caller->Comp_unit()->Vsa()->Ipsa();
  if (assert_flag == RBC_SE_ASSERT_DNA) {
    Is_True_Ret(caller->Is_set_rbc_flag(DNA_RBC_ASSERT_DNA),
                ("RBC_BASE::Eval__mvsa_assert func %s do not have assert dna rule", caller->Fname()));
    // fetch its rbc nodes list first
    DNODE_VECTOR *rbc_nodes = ipsa->Rbc()->Get_rbc_nodes(caller);
    if (rbc_nodes != NULL) {
      DNODE_VECTOR::const_iterator iter = rbc_nodes->begin();
      for (; iter != rbc_nodes->end(); iter++) {
        DNA_NODE *rbc_callee = *iter;
        // evaluate if there's an assert in the rbc node
        if (rbc_callee != NULL && rbc_callee->Is_set_rbc_flag(DNA_RBC_ASSERT_DNA)) {
          Is_Trace(Tracing(), (TFile,
                                "RBC_BASE::Eval__mvsa_assert: evaluate RBC assert for %s via DNA node(%d)\n",
                                caller->Fname(), rbc_callee->Dna_idx()));
          ipsa->Rbc()->Eval__mvsa_assert(caller, rbc_callee, NULL, caller, assert_flag);
        }
      }
    }
  } else if (assert_flag == RBC_SE_ASSERT) {
    Is_True_Ret(caller_rna, ("RBC_BASE::Eval__mvsa_assert: null caller rna"));
    Is_True_Ret(caller_rna->Is_flag_set(RBC_SE_ASSERT), ("RBC_BASE::Eval__mvsa_assert: rna has no assert"));
    const CALLEE_VECTOR& callee_list = caller_rna->Callee_list();
    for(CALLEE_VECTOR::const_iterator iter = callee_list.begin();
        iter != callee_list.end(); iter++) {
      DNA_NODE *callee = ipsa->Get_dna(iter->Callee());
      if (callee == NULL) {
        continue;
      }
      // fetch its rbc nodes list first
      DNODE_VECTOR *rbc_nodes = ipsa->Rbc()->Get_rbc_nodes(callee);
      if (rbc_nodes == NULL) {
        continue;
      }

      DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
      for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
        DNA_NODE *rbc_callee = *rbc_iter;
        // evaluate if there's an assert in the rbc node
        if (rbc_callee != NULL && rbc_callee->Is_set_rbc_flag(DNA_RBC_ASSERT)) {
          Is_Trace(Tracing(), (TFile,
                                "RBC_BASE::Eval__mvsa_assert: evaluate RBC assert for %s via RBC NODE(%d)\n",
                                callee->Fname(), rbc_callee->Dna_idx()));
          ipsa->Rbc()->Eval__mvsa_assert(callee, rbc_callee, caller_rna, caller, assert_flag);
        }
      }
      if (callee->Has_annots()) {
        Is_Trace(Tracing(), (TFile,
                              "RBC_BASE::Eval__mvsa_assert: evaluate RBC annotation for %s\n",
                              callee->Fname()));
        ipsa->Rbc()->Eval__annot_assert(caller, caller_rna, callee);
      }
    }
  }
}

// =============================================================================
//
// RBC_BASE::Eval__mvsa_assert
//
// =============================================================================
void
RBC_BASE::Eval__mvsa_assert(DNA_NODE *callee, DNA_NODE *rbc_callee, RNA_NODE *caller_rna, DNA_NODE *caller, RNA_FLAGS assert_flag)
{
  VSA *vsa = caller->Comp_unit()->Vsa();
  RNODE_VECTOR *rna_list = rbc_callee->Call_list();
  for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
    RNA_NODE *rna = (*rna_list)[i];
    if (!rna->Is_flag_set(assert_flag))
      continue;

    // check if rule is enabled, skip if not
    RULE *rule = Get_rule(rna->Rna_idx());
    if (rule == NULL) {
      Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__mvsa_assert: null RULE for RNA_NODE(%d)\n",
                           rna->Rna_idx()));
      continue;
    }
    STRING rule_name = rule->Get_rule_name();
    if (!rule->Is_enable()) {
      Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__mvsa_assert: skip RULE(%d):\"%s\"\n",
                           rule->Get_id(), rule_name));
      continue;
    }
    CODEREP *boolexp = (CODEREP*)rule->Get_bool_expr();
    // In rbc apply rule, the real rule dna is stored in the RULE *rule,
    // so we need to switch to the right context.
    rbc_callee = vsa->Ipsa()->Get_dna(rule->Get_dna_idx());
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__mvsa_assert: eval RULE(%d):\"%s\" for FUNC(\"%s\")\n",
                         rule->Get_id(), rule_name, callee->Fname()));
    rule->Set_scanned(TRUE);
    Eval__base_assert(caller, caller_rna, callee, rbc_callee, rule_name, boolexp);
  } // end for rna_list
}


// =============================================================================
//
// RBC_BASE::Eval__annot_assert
//
// =============================================================================
void
RBC_BASE::Eval__annot_assert(DNA_NODE *caller, RNA_NODE *rna, DNA_NODE *callee)
{
  if (callee == NULL)
    return;

  vector<STRING> annot_names;
  if (!callee->Get_annots(annot_names))
    return;
  for(vector<STRING>::iterator it = annot_names.begin(); it != annot_names.end(); it++) {
    STRING annot_name = *it;
    RULE_BODY_VEC *rb_vec = Get_annot_rbc(annot_name);
    if (rb_vec == NULL)
      continue;

    VSA *vsa = caller->Comp_unit()->Vsa();
    for (INT i = 0; i < rb_vec->size(); i++) {
      RULE_BODY *rb = (*rb_vec)[i];
      if (rb == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_assert: null RULE_BODY for ANNOTATION(\"%s\") INDEX(%d)\n",
                            annot_name, i));
        continue;
      }
      DNA_NODE *rbc_dna = rb->Get_rbc_dna(vsa->Ipsa());
      if (rbc_dna == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_assert: null rbc_dna for ANNOTATION(\"%s\") INDEX(%d)\n",
                            annot_name, i));
        continue;
      }
      CONTEXT_SWITCH context(rbc_dna);
      STMTREP *rule_sr = rb->Get_rbc_sr();
      if (rule_sr == NULL || !OPERATOR_is_call(rule_sr->Opr()))
        continue;
      RNA_NODE *rule_rna = rbc_dna->Get_callsite_rna(rule_sr);
      // evaluate only Rbc_assert
      if (rule_rna && Is_rbc_assert(rule_rna)) {
        CODEREP *vcall = rule_sr->Rhs();
        CODEREP *boolexp = vcall->Opnd(0 + Rbc_parm_offset(caller))->Ilod_base();
        CODEREP *rname_cr = vcall->Opnd(1 + Rbc_parm_offset(caller))->Ilod_base();
        STRING rule_name = Find_const_char(rbc_dna, rname_cr);
        Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__annot_assert: eval ANNOTATION(\"%s\") for FUNC(\"%s\")\n",
                            annot_name, callee->Fname()));
        Eval__base_assert(caller, rna, callee, rbc_dna, rule_name, boolexp);
      }
    } // end for rb_vec
  }
}


// =============================================================================
//
// RBC_BASE::Eval__base_assert
//
// =============================================================================
void
RBC_BASE::Eval__base_assert(DNA_NODE *caller, RNA_NODE *caller_rna, DNA_NODE *callee,
                            DNA_NODE *rbc_callee, STRING rule_name, CODEREP *boolexp)
{
  // we should be in rbc_callee's context since we will symbolic executate
  // statements from rules/rbc_callee
  if (caller == NULL || callee == NULL ||
      rbc_callee == NULL || rule_name == NULL || boolexp == NULL)
    return;
  if (caller_rna == NULL && !callee->Is_set_rbc_flag(DNA_RBC_ASSERT_DNA)) {
    return;
  }

  Is_True(Rbc_parm_offset(caller) == Rbc_parm_offset(rbc_callee),
          ("RBC ERROR: imcompatible language type of RBC APIs and sources.\n"));
  if (Rbc_parm_offset(caller) != Rbc_parm_offset(rbc_callee))
    return;

  VSA *vsa = caller->Comp_unit()->Vsa();
  // check exception lists first
  if (VSA_RBC_Exception) {
    BOOL skip = FALSE;
    RULE_BODY_VEC *re_vec = Get_rule_exceptions(rule_name);
    if (re_vec != NULL) {
      for (INT i = 0; i < re_vec->size(); i++) {
        RULE_BODY *except = (*re_vec)[i];
        if (except == NULL) {
          Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__base_assert: null RULE_BODY for RULE(\"%s\") EXCEPTION(%d)\n",
                               rule_name, i));
          continue;
        }
        DNA_NODE *except_dna = vsa->Ipsa()->Get_dna(except->Get_dna_idx());
        if (except_dna == NULL) {
          Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__base_assert: null except_dna for RULE(\"%s\") EXCEPTION(%d)\n",
                               rule_name, i));
          continue;
        }
        // if the exception applies on given callee
        if (strcmp(except_dna->Fname(), callee->Fname()) == 0) {
          CONTEXT_SWITCH except_context(except_dna);
          Rbc_init();
          RBC_CONTEXT except_ctx(caller, callee, except_dna, caller_rna, vsa->Loc_pool());
          UINT64 retv = Eval__exp(except_ctx, except->Get_rbc_expr());
          if (retv != 0) {
            Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__base_assert: skip RULE(\"%s\") due to exceptions on FUNC(\"%s\")\n",
                                 rule_name, callee->Fname()));
            skip = TRUE;
            break;
          }
        }
      } // end for re_vec
      if (skip)
        return;
    }
  } // end if VSA_RBC_Exception

  // do symbolic evaluation here for boolexp
  CONTEXT_SWITCH context(rbc_callee);
  Rbc_init();
  RBC_CONTEXT rbc_ctx(caller, callee, rbc_callee, caller_rna, vsa->Loc_pool());
  if (callee->Is_set_rbc_flag(DNA_RBC_FUNC_TAG)) {
    TAG_INFO *tag_info = Find_dna_tag_info(rbc_callee->Fname(), callee);
    if (tag_info) {
      rbc_ctx.Set_tag_info(tag_info);
    }
  }
  UINT64 retv = Eval__exp(rbc_ctx, boolexp);
  if (retv != 0)
    return;
  if (Rbc_eval_skip())
    return;
  // report error, need caller's context
  CONTEXT_SWITCH caller_context(caller);
  STMTREP *rp_stmt = caller_rna ? caller_rna->Callstmt() : vsa->Get_entry_chi_stmt();
  Is_True(rp_stmt, ("null report stmt"));
  if (rp_stmt == NULL) {
    Is_Trace(Tracing(), (TFile, "RBC Eval__base_assert skip: report stmt is null\n"));
    return;
  }
  if (Plist_false()->size() != 0) {
    for (INT i = 0; i < Plist_false()->size(); i++) {
      SRCPOS_HANDLE *srcpos_h = (*Plist_false())[i];
      Report_rbc_error(vsa, rp_stmt, rule_name, Rbc_result_maybe(), srcpos_h);
    }
  }
  else {
    // for those no path list has been generated, use callee function name,
    // because we don't actually know which parameter is involved.
    SRCPOS_HANDLE srcpos_h(NULL, rp_stmt, caller, vsa->Loc_pool(), vsa);
    srcpos_h.Append_data(rp_stmt, caller, PATHINFO_VUL_SPOT);
    srcpos_h.Set_orig_stname(callee->Fname());
    Report_rbc_error(vsa, rp_stmt, rule_name, Rbc_result_maybe(), &srcpos_h);
  }
}

// =============================================================================
//
// RBC_BASE::Eval__mvsa_tag_op
//
// =============================================================================
void
RBC_BASE::Eval__mvsa_tag_op(DNA_NODE *caller, RNA_NODE *caller_rna, MEM_POOL *pool)
{
  Is_True_Ret(caller_rna->Is_flag_set(RBC_SE_TAG_OP), ("rna is not tag op"));
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Eval__mvsa_tag_op:\n"));
  const CALLEE_VECTOR& callee_list = caller_rna->Callee_list();
  IPSA *ipsa = caller->Comp_unit()->Vsa()->Ipsa();
  for (CALLEE_VECTOR::const_iterator iter = callee_list.begin(); iter != callee_list.end(); iter++) {
    DNA_NODE *callee = ipsa->Get_dna(iter->Callee());
    if (callee == NULL) {
      continue;
    }
    DNODE_VECTOR *rbc_nodes = Get_rbc_nodes(callee);
    if (rbc_nodes == NULL) {
      continue;
    }
    DNODE_VECTOR::const_iterator rbc_iter = rbc_nodes->begin();
    for (; rbc_iter != rbc_nodes->end(); rbc_iter++) {
      DNA_NODE *rbc_callee = *rbc_iter;
      if (rbc_callee == NULL || !rbc_callee->Is_set_rbc_flag(DNA_RBC_TAG_OP)) {
        continue;
      }
      // the context management is tricky because RNA uses caller's context
      // while the model itself is in the callee's context
      Is_True(Rbc_parm_offset(caller) == Rbc_parm_offset(rbc_callee),
              ("RBC ERROR: imcompatible language type of RBC APIs and sources.\n"));
      if (Rbc_parm_offset(caller) != Rbc_parm_offset(rbc_callee))
        continue;

      CONTEXT_SWITCH context(rbc_callee);

      RNODE_VECTOR *rna_list = rbc_callee->Call_list();
      for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
        if (!(*rna_list)[i]->Is_flag_set(RBC_SE_TAG_OP))
          continue;

        CODEREP *vcall = (*rna_list)[i]->Callstmt()->Rhs();
        CODEREP *stmt = vcall->Opnd(0 + Rbc_parm_offset(caller))->Ilod_base();

        Rbc_init();
        RBC_CONTEXT rbc_ctx(caller, callee, rbc_callee, caller_rna, pool);
        UINT64 retv = Eval__exp(rbc_ctx, stmt);
        if (retv == 0) {
          Is_Trace(Tracing(), (TFile, "RBC ERROR: failed tag op: %s\n", callee->Fname()));
        }
      }
    }
  }
}

// =============================================================================
//
// RBC_BASE::Eval__mvsa_container_op - Symbolic execution entry for container op
//
// =============================================================================
EVAL_RET
RBC_BASE::Eval__mvsa_container_op(DNA_NODE *caller, RNA_NODE *caller_rna, MEM_POOL *pool, void *objs, BOOL is_forward)
{
  CALLEE_VECTOR callee_list = caller_rna->Callee_list();
  BOOL container_op = FALSE;
  EVAL_RET retv(BOOL_T, TRUE);
  IPSA *ipsa = caller->Comp_unit()->Vsa()->Ipsa();
  for (CALLEE_VECTOR::const_iterator iter = callee_list.begin();
       iter != callee_list.end(); iter++) {
    DNA_NODE *callee = ipsa->Get_dna(iter->Callee());
    if (callee && callee->Is_set_rbc_flag(DNA_RBC_SE_EVAL)) {
      DNODE_VECTOR *rbc_nodes = Get_rbc_nodes(callee);
      Is_True(rbc_nodes != NULL, ("Rbc nodes expected but found none!\n"));
      if (rbc_nodes == NULL)
        continue;
      Is_Trace(Tracing(), (TFile, "RBC_BASE:processing mvsa_container_op\n"));
      DNODE_VECTOR::const_iterator iter = rbc_nodes->begin();
      for (; iter != rbc_nodes->end(); iter++) {
        DNA_NODE *rbc_callee = *iter;
        // the context management is tricky because RNA uses caller's context
        // while the model itself is in the callee's context
        Is_True_Ret_Rbc_Invalid(
          Rbc_parm_offset(caller) == Rbc_parm_offset(rbc_callee),
          ("RBC ERROR: imcompatible language type of RBC APIs and sources.\n"));

        CONTEXT_SWITCH context(rbc_callee);
        RNODE_VECTOR *rna_list = rbc_callee->Call_list();
        for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
          RNA_NODE *rna = (*rna_list)[i];
          Rbc_init();
          RBC_CONTEXT rbc_ctx(caller, callee, rbc_callee, caller_rna, pool);
          BOOL default_op = FALSE;
          switch (rna->Rbc_op()) {
            case RBC_OP_SET_FUNC_CONTAINER_INIT:
              retv = Eval__container_init(rbc_ctx, rna->Callstmt(), objs, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_APPEND:
              retv = Eval__coll_append(rbc_ctx, rna->Callstmt(), objs, FALSE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_APPEND_REF:
              retv = Eval__coll_append(rbc_ctx, rna->Callstmt(), objs, TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_INSERT:
              retv = Eval__coll_insert(rbc_ctx, rna->Callstmt(), objs, TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_REMOVE:
              retv = Eval__coll_remove(rbc_ctx, rna->Callstmt(), objs, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_GET:
              retv = Eval__coll_get(rbc_ctx, rna->Callstmt(), FALSE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_GET_REF:
              retv = Eval__coll_get(rbc_ctx, rna->Callstmt(), TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_BACK:
              retv = Eval__coll_back(rbc_ctx, rna->Callstmt(), FALSE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_BACK_REF:
              retv = Eval__coll_back(rbc_ctx, rna->Callstmt(), TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_COLL_END:
              retv = Eval__coll_end(rbc_ctx, rna->Callstmt(), is_forward);
              break;
            case RBC_OP_SET_FUNC_MAP_GET:
              retv = Eval__map_get(rbc_ctx, rna->Callstmt(), FALSE, is_forward);
              break;
            case RBC_OP_SET_FUNC_MAP_GET_REF:
              retv = Eval__map_get(rbc_ctx, rna->Callstmt(), TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_MAP_PUT:
              retv = Eval__map_put(rbc_ctx, rna->Callstmt(), objs, FALSE, is_forward);
              break;
            case RBC_OP_SET_FUNC_MAP_PUT_REF:
              retv = Eval__map_put(rbc_ctx, rna->Callstmt(), objs, TRUE, is_forward);
              break;
            case RBC_OP_SET_FUNC_STR_GET:
              retv = Eval__str_get(rbc_ctx, rna->Callstmt(), is_forward);
              break;
            default:
              default_op = TRUE;
              continue;
          }
          if (!default_op) {
            Is_True_Ret_Rbc_Invalid(
              !container_op,
              ("RBC ERROR: only allow one container SE annoate on each call"));
            container_op = TRUE;
            if (retv == EVAL_RET_INVALID) {
              Is_Trace(Tracing(), (TFile, "RBC ERROR: failed container op: %s\n", callee->Fname()));
            }
            return retv;
          }
        } // for_all(rbc_callee's rna lists)
      } // for all rbc_nodes
    } // if(callee && callee is SE_EVAL)
  } // for_all(caller_rna's callee_lists)
  return retv;
}


// =============================================================================
//
// RBC_BASE::Eval__assume_parm
//    Assumptions specified on function parameters
//    Use parameter coderep id to insert into value range map
//
// Assume_parm(int i, UINT64 lower, UINT64 upper)
//
// =============================================================================
UINT64
RBC_BASE::Eval__assume_parm(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  IDTYPE parm_idx = (IDTYPE)Eval__exp(rbc_ctx, arg0);
  DNA_NODE *callee = rbc_ctx.Callee();
  CODEREP *parm = callee->Get_param_cr(parm_idx);
  Is_True_Rbc(parm != NULL, ("RBC ERROR: invaled parameter idx\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 lower = Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  UINT64 upper = Eval__exp(rbc_ctx, arg2);
  VSA *callee_vsa = callee->Comp_unit()->Vsa();
  callee_vsa->Enter_cr_vr_map(parm->Coderep_id(), lower, upper);
  DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
  callee->Set_flag(DNA_HAS_ASSUMPTION);
  rbc_callee->Set_flag(DNA_HAS_ASSUMPTION);
  return TRUE;
}


// =============================================================================
//
// RBC_BASE::Eval__assume_var
//    Assumptions specified on line for var,
//    If no user source code, the assumptions would not be attached
//    Use var coderep id to insert into value range map
//
// Assume_var(CONST STRING var, UINT64 line, UINT64 lower, UINT64 upper)
//
// =============================================================================
UINT64
RBC_BASE::Eval__assume_var(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  DNA_NODE *callee = rbc_ctx.Callee();
  if (callee->Non_functional()) {
    Is_Trace(Tracing(),
             (TFile, "RBC: No source of %s, no assumption attached\n",
              callee->Fname()));
    return TRUE;
  }
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  char *vname = (char*)Eval__exp(rbc_ctx, arg0);
  Is_True_Rbc(vname != NULL, ("RBC ERROR: null variable name\n"));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT32 linenum = (UINT32)Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  UINT64 lower = Eval__exp(rbc_ctx, arg2);
  CODEREP *arg3 = stmt->Rhs()->Find_nth_arg(3 + Rbc_parm_ofst_adjust(dna));
  UINT64 upper = Eval__exp(rbc_ctx, arg3);
  // find coderep id in linenum of vname
  CODEREP *cr = NULL;
  {
    CONTEXT_SWITCH ctx(callee);
    cr = Find_cr_with_name_in_line(vname, linenum, callee->Comp_unit()->Cfg()->Entry_bb(), callee);
  }
  VSA *callee_vsa = callee->Comp_unit()->Vsa();
  if (cr != NULL) {
    callee_vsa->Enter_cr_vr_map(cr->Coderep_id(), lower, upper);
    DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
    callee->Set_flag(DNA_HAS_ASSUMPTION);
    rbc_callee->Set_flag(DNA_HAS_ASSUMPTION);
  }
  else {
    Is_Trace(Tracing(),
             (TFile,
              "RBC ERROR: failed to find cr in line %d of name %s\n",
              linenum, vname));
  }
  return TRUE;
}


// =============================================================================
//
// RBC_BASE::Find_cr_with_name
//
// =============================================================================
CODEREP*
RBC_BASE::Find_cr_with_name(const char* name, CODEREP *cr, STMTREP *stmt, DNA_NODE *dna)
{
  CODEREP *ret = NULL;
  if (cr == NULL)
    return ret;
  switch (cr->Kind()) {
  case CK_CONST:
  case CK_RCONST:
    break;
  case CK_LDA:
  case CK_VAR: {
    STRING_BUFFER sbuf(VSA_VAR_NAME_MAX_LEN);
    const char *var = SRCPOS_HANDLE::Find_cr_stname(&sbuf, cr, stmt, dna);
    if (var!= NULL && strcmp(var, name) == 0)
      ret = cr;
    break;
  }
  case CK_IVAR: {
    CODEREP *base = cr->Ilod_base() != NULL ? cr->Ilod_base() : cr->Istr_base();
    ret = Find_cr_with_name(name, base, stmt, dna);
    break;
  }
  case CK_OP: {
    for (INT actual = 0; actual < cr->Kid_count(); actual++) {
      CODEREP *opnd = cr->Opnd(actual);
      ret = Find_cr_with_name(name, opnd, stmt, dna);
      if (ret != NULL)
        break;
    }
    break;
  }
  default:
    Is_True_Rbc(FALSE, ("RBC ERROR: Find_cr_with_name: unsupported CK_KIND %d\n", cr->Kind()));
    break;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Find_cr_with_name_stmt
//
// =============================================================================
CODEREP*
RBC_BASE::Find_cr_with_name_stmt(const char* name, STMTREP *stmt, DNA_NODE *dna)
{
  CODEREP *ret = NULL;
  if (stmt->Rhs() != NULL) {
    ret = Find_cr_with_name(name, stmt->Rhs(), stmt, dna);
  }
  if (ret == NULL && stmt->Lhs() != NULL) {
    ret = Find_cr_with_name(name, stmt->Lhs(), stmt, dna);
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Find_cr_with_name_in_line
//
// =============================================================================
CODEREP*
RBC_BASE::Find_cr_with_name_in_line(const char* name, UINT32 line, BB_NODE *bb, DNA_NODE *dna)
{
  CODEREP *ret = NULL;
  // find the right line first
  STMTREP *stmt;
  STMTREP_ITER sr_iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, sr_iter, Init()) {
    SRCPOS stmt_ln = stmt->Linenum();
    if (SRCPOS_linenum(stmt_ln) == line) {
      ret = Find_cr_with_name_stmt(name, stmt, dna);
      if (ret != NULL)
        return ret;
    }
  }

  // go for dom bbs
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    ret = Find_cr_with_name_in_line(name, line, dom_bb, dna);
    if (ret != NULL)
      break;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Eval__assume_ret
//    Assumptions specified on function return values
//    Use 0 to insert into value range map
//
// Assume_ret(UINT64 lower, UINT64 upper)
//
// =============================================================================
UINT64
RBC_BASE::Eval__assume_ret(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  DNA_NODE *dna = rbc_ctx.Caller();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  UINT64 lower = Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  UINT64 upper = Eval__exp(rbc_ctx, arg1);
  DNA_NODE *callee = rbc_ctx.Callee();
  VSA *callee_vsa = callee->Comp_unit()->Vsa();
  callee_vsa->Enter_cr_vr_map(0, lower, upper);
  DNA_NODE *rbc_callee = rbc_ctx.Rbc_node();
  callee->Set_flag(DNA_HAS_ASSUMPTION);
  rbc_callee->Set_flag(DNA_HAS_ASSUMPTION);
  return TRUE;
}


template<typename T>
T
RBC_BASE::Get_value_obj(VALUE_OBJS<T> &objs, STMTREP *sr, COLL_POSITION pos, CODEREP *key, COMP_UNIT *cu, MEM_POOL *pool)
{
  Is_True_Ret(FALSE, ("Unexpected call"), NULL);
  return NULL;
}

template<>
LIST_ENTRY*
RBC_BASE::Get_value_obj(LIST_OBJS &objs, STMTREP *sr, COLL_POSITION pos, CODEREP *key, COMP_UNIT *cu, MEM_POOL *pool)
{
  if (pos == COLL_POS_IDX) {
    INT64 idx = 0;
    if(key->Kind() == CK_CONST) {
      idx = key->Const_val();
      Is_True(idx >= 0, ("RBC ERROR: negative idx for coll get"));
    } else {
      Is_Trace(Tracing(), (TFile, "TODO:LIST Get_value_obj: non-const idx\n"));
      return NULL;
    }
    if(idx < objs.Size()) {
      return objs.Value_obj(idx);
    } else {
      Is_Trace(Tracing(), (TFile, "LIST Get_value_obj: idx outof bound\n"));
    }
    return NULL;
  } else if (pos == COLL_POS_BACK || pos == COLL_POS_FRONT) {
    if (objs.Empty()) {
      Is_Trace(Tracing(), (TFile, "LIST Get_value_obj at back, list is empty\n"));
      return NULL;
    } else {
      return pos == COLL_POS_BACK ? objs.Back() : objs.Front();
    }
  } else {
    Is_True(FALSE, ("Can't reach here."));
  }
  return NULL;
}

template<>
MAP_ENTRY*
RBC_BASE::Get_value_obj(MAP_OBJS &objs, STMTREP *sr, COLL_POSITION pos, CODEREP *key, COMP_UNIT *cu, MEM_POOL *pool)
{
  CONTEXT_SWITCH ctx(cu->Dna());
  VSA *vsa = cu->Vsa();
  if (pos == COLL_POS_IDX) {
    STR_SET str_set;
    BOOL found = Find_const_char_cross(vsa, sr, key, str_set, pool);
    char *key_str = found ? *(str_set.begin()) : NULL;
    MAP_ENTRY *ret = NULL;
    for(int i = objs.Size() - 1; i >=0; i--) {
      MAP_ENTRY *obj = objs.Value_obj(i);
      if(obj->Same_key(key, key_str, cu)) {
        ret = obj;
        break;
      }
    }
    return ret;
  } else {
    Is_True(FALSE, ("IMP."));
  }
  return NULL;
}

// =============================================================================
// RBC_BASE::Eval__container_init - Symbolic execution for container init
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
//  @ret: if success, returns <BOOL_T, TRUE>
//        if fail, return EVAL_RET_INVALID
//
// set_func_container_init(OBJECT obj, OBJECT value)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__container_init(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  EVAL_RET ret = EVAL_RET_INVALID;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *) Eval__exp(rbc_ctx, arg0);
  Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for Eval__container_init"));

  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CONTEXT_SWITCH caller_ctx(dna);
  VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
  VSYM_OBJ_REP *obj_vor = vsa->Find_vor_chi_vor(call_stmt, obj, &any_vfr);
  DEF_OBJS *obj_value = NULL;
  if (obj_vor) {
    if (is_forward) {
      if (value) {
        VSYM_OBJ_REP *value_vor = vsa->Find_vor_mu_vor(call_stmt, value, &any_vfr);
        if (value_vor) {
          obj_value = vsa->Clone_value_objs(obj_vor, value_vor);
        }
      }
      if (obj_value == NULL) {
        obj_value = vsa->Create_value_objs(obj_vor, UNK_TYPE);
      }
    } else {
      Is_True_Ret(objs, ("null objs in container init"), EVAL_RET_INVALID);
      obj_value = vsa->Vor_2_value_objs(obj_vor);
      if (obj_value && obj_value->Size() > 0) {
        ((DEF_OBJS *)objs)->Copy(obj_value);
      }
    }
    if (obj_value) {
      ret.first = BOOL_T;
      ret.second = TRUE;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC:Eval__container_init returns %lld\n", ret.second));
  return ret;
}
// =============================================================================
// RBC_BASE::Eval__coll_get - Symbolic execution for collection get
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns the value objects at given idx
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_get(OBJECT obj, OBJECT idx)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();

  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_get\n"));
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *pool = rbc_ctx.Mem_pool();
  EVAL_RET ret = EVAL_RET_INVALID;
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *idx_cr = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Ret_Rbc_Invalid(idx_cr != NULL, ("RBC ERROR: null idx"));

  if (is_forward) {
    // get value objects from list vor mu, and retrive element at given index
    // if value object is found, create new value objects and bind to vor based
    // on return cr
    CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
    CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
    Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for set_func_coll_get"));
    CONTEXT_SWITCH caller_ctx(dna);
    VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
    VSYM_OBJ_REP *vor_mu = vsa->Find_vor_mu_vor(rna->Callstmt(), obj, &any_vfr);
    if (vor_mu) {
      LIST_ENTRY *entry = vsa->Get_list_entry(vor_mu, idx_cr);
      if (entry) {
        CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(rna->Callstmt());
        Is_True_Ret_Rbc_Invalid(ret_cr, ("null ret for collection get"));
        // return is preg, use FLD_K_ID
        VSYM_FLD_REP zero_vfr(FLD_K_ID, 0, 0);
        VSYM_OBJ_REP *ret_vor = vsa->Find_vor_chi_vor(rna->Callstmt(), ret_cr, &zero_vfr);
        if (ret_vor) {
          LIST_OBJS *ret_obj = (LIST_OBJS *)vsa->Create_value_objs(ret_vor, LIST_TYPE);
          ret_obj->Push_back(entry);
          ret.first = LIST_PTR;
          ret.second = (INT64)ret_obj;
        }
      }
    }
  } else {
    CXX_MEM_POOL list_objs_pool("Temp_list_objs_pool", FALSE);
    LIST_OBJS list_objs(LIST_TYPE, list_objs_pool());
    LIST_OBJS *cands = CXX_NEW(LIST_OBJS(LIST_TYPE, pool), pool);
    EVAL_RET eval_ret = Eval__container_get(vsa, rna, pool, COLL_POS_IDX, idx_cr, list_objs, *cands);
    if(eval_ret != EVAL_RET_INVALID) {
      ret.first = LIST_PTR;
      ret.second = (INT64)cands;
    }
  }
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__coll_back - Symbolic execution for collection get at back
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns the value objects at back
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_back(OBJECT v)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_back(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();

  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_back\n"));
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  MEM_POOL *pool = rbc_ctx.Mem_pool();
  EVAL_RET ret = EVAL_RET_INVALID;
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  if (is_forward) {
    // get value objects from list vor mu, and retrive element at end
    // if value object is found, create new value objects and bind to vor based
    // on return cr
    CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
    CODEREP *obj = (CODEREP *)Eval__exp(rbc_ctx, arg0);
    Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for set_func_coll_get"));
    CONTEXT_SWITCH caller_ctx(dna);
    VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
    VSYM_OBJ_REP *vor_mu = vsa->Find_vor_mu_vor(rna->Callstmt(), obj, &any_vfr);
    if (vor_mu) {
      LIST_ENTRY *entry = vsa->Get_list_back(vor_mu);
      if (entry) {
        CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(rna->Callstmt());
        Is_True_Ret_Rbc_Invalid(ret_cr, ("null ret for collection get"));
        // return is preg, use FLD_K_ID
        VSYM_FLD_REP zero_vfr(FLD_K_ID, 0, 0);
        VSYM_OBJ_REP *ret_vor = vsa->Find_vor_chi_vor(rna->Callstmt(), ret_cr, &zero_vfr);
        if (ret_vor) {
          LIST_OBJS *ret_obj = (LIST_OBJS *)vsa->Create_value_objs(ret_vor, LIST_TYPE);
          ret_obj->Push_back(entry);
          ret.first = LIST_PTR;
          ret.second = (INT64)ret_obj;
        }
      }
    }
  } else {
    CXX_MEM_POOL list_objs_pool("Temp_list_objs_pool", FALSE);
    LIST_OBJS list_objs(LIST_TYPE, list_objs_pool());
    LIST_OBJS *cands = CXX_NEW(LIST_OBJS(LIST_TYPE, pool), pool);
    EVAL_RET eval_ret = Eval__container_get(vsa, rna, pool, COLL_POS_BACK, NULL, list_objs, *cands);
    if(eval_ret != EVAL_RET_INVALID) {
      ret.first = LIST_PTR;
      ret.second = (INT64)cands;
    }
  }
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__coll_append - Symbolic execution for collection append
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns <BOOL_T, TRUE>
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_append(OBJECT obj, OBJECT value, BOOL deref)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_append(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void* objs, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_append\n"));

  EVAL_RET ret = EVAL_RET_INVALID;
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  LIST_OBJS *lists = NULL;
  if (is_forward) {
    CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
    CODEREP *obj = (CODEREP *) Eval__exp(rbc_ctx, arg0);
    Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for Eval__coll_append"));
    CONTEXT_SWITCH caller_ctx(dna);
    VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
    CHI_NODE *chi_node = vsa->Find_vor_chi(call_stmt, obj, &any_vfr);
    if (chi_node) {
      VSYM_OBJ_REP *vor_opnd = ((CVOR*)chi_node->OPND())->first;
      VSYM_OBJ_REP *vor_res = ((CVOR*)chi_node->RESULT())->first;
      lists = (LIST_OBJS *)vsa->Clone_value_objs(vor_res, vor_opnd);
    }
    if (lists == NULL) {
      return ret;
    }
    lists->Set_type(LIST_TYPE);
  } else {
    lists = (LIST_OBJS *) objs;
  }
  Is_True_Ret_Rbc_Invalid(lists, ("RBC_ERROR: Eval__coll_append null VALUE_OBJS"));
  if (lists->Type() != LIST_TYPE) {
    // the UD of objs value contains other container operation, cancel the symbolic eval
    Is_Trace(Tracing(), (TFile, "RBC: Eval__coll_append not supported objs type %d\n", lists->Type()));
    return EVAL_RET_INVALID;
  }
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Ret_Rbc_Invalid(value != NULL, ("RBC ERROR: null value for Eval__coll_append"));
  if (deref) {
    Is_True_Ret(value->Kind() == CK_LDA, ("Deref without LDA"), EVAL_RET_INVALID);
    MU_NODE* mu = call_stmt->Mu_list()->Search_mu_node(value->Lda_aux_id());
    if (mu == NULL) {
      Is_Trace(Tracing(),
               (TFile, "RBC: Eval__coll_append not find mu for deref in call %s.\n",
                       callee->Fname()));
      return EVAL_RET_INVALID;
    }
    value = mu->OPND();
  }

  LIST_ENTRY* cand = CXX_NEW(LIST_ENTRY(0, value, call_stmt, vsa->Comp_unit()),
                             lists->Mem_pool());
  lists->Push_back(cand);
  ret.first = BOOL_T;
  ret.second = TRUE;
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__coll_insert - Symbolic execution for collection insert
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns <BOOL_T, TRUE>
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_insert(OBJECT obj, OBJECT pos, OBJECT value, OBJECT cnt)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_insert(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void *objs, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_insert\n"));
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  DNA_NODE *callee = rbc_ctx.Callee();
  EVAL_RET ret = EVAL_RET_INVALID;
  LIST_OBJS *lists = NULL;
  if (!is_forward) {
    Is_True(FALSE, ("Eval__coll_insert backward not supported yet"));
    return ret;
  }
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *) Eval__exp(rbc_ctx, arg0);
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *pos = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *cnt = (CODEREP *) Eval__exp(rbc_ctx, arg2);
  CODEREP *arg3 = stmt->Rhs()->Find_nth_arg(3 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg3);
  Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for Eval__coll_insert"));
  Is_True_Ret_Rbc_Invalid(pos != NULL, ("RBC ERROR: null value for Eval__coll_insert"));
  Is_True_Ret_Rbc_Invalid(value != NULL, ("RBC ERROR: null value for Eval__coll_insert"));
  CONTEXT_SWITCH caller_ctx(dna);
  // [1]: process obj
  VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
  CHI_NODE *chi_node = vsa->Find_vor_chi(call_stmt, obj, &any_vfr);
  if (chi_node) {
    VSYM_OBJ_REP *vor_opnd = ((CVOR*)chi_node->OPND())->first;
    VSYM_OBJ_REP *vor_res = ((CVOR*)chi_node->RESULT())->first;
    lists = (LIST_OBJS *)vsa->Clone_value_objs(vor_res, vor_opnd);
  }
  if (lists == NULL) {
    return ret;
  }
  lists->Set_type(LIST_TYPE);

  // [2]: process pos
  VSYM_OBJ_REP *pos_vor = vsa->Find_vor_mu_vor(call_stmt, pos, &any_vfr);
  LIST_ITERS *pos_values = NULL;
  if (pos_vor) {
    pos_values = (LIST_ITERS *)vsa->Vor_2_value_objs(pos_vor);
  }
  if (pos_values == NULL) {
    return ret;
  }
  Is_True_Ret_Rbc_Invalid(pos_values->Type() == LIST_ITER_TYPE &&
                          pos_values->Size() == 1,
                          ("pos_value type is not LIST_ITER_TYPE or multiple values"));

  // [3]: process cnt
  INT64 cnt_value = 1;
  if (cnt && cnt->Kind() == CK_CONST) {
    cnt_value = cnt->Const_val();
    Is_True(cnt_value > 0, ("negative insert cnt"));
  }

  // [4]: process value
  if (deref) {
    Is_True_Ret(value->Kind() == CK_LDA, ("Deref without LDA"), EVAL_RET_INVALID);
    MU_NODE* mu = call_stmt->Mu_list()->Search_mu_node(value->Lda_aux_id());
    if (mu == NULL) {
      Is_Trace(Tracing(),
               (TFile, "RBC: Eval__coll_insert not find mu for deref in call %s.\n",
                       callee->Fname()));
      return EVAL_RET_INVALID;
    }
    value = mu->OPND();
  }
  LIST_ENTRY* cand = CXX_NEW(LIST_ENTRY(0, value, call_stmt, vsa->Comp_unit()),
                             lists->Mem_pool());

  lists->Insert(pos_values->Front()->Iter(), cnt_value, cand);
  ret.first = BOOL_T;
  ret.second = TRUE;
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__coll_end - Symbolic execution for collection end
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns <BOOL_T, TRUE>
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_end(OBJECT obj)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_end(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_end\n"));
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  RNA_NODE *rna = rbc_ctx.Rna();
  EVAL_RET ret = EVAL_RET_INVALID;

  LIST_OBJS *lists = NULL;
  if (!is_forward) {
    Is_True(FALSE, ("Eval__coll_end backward not supported yet"));
    return ret;
  }
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *obj = (CODEREP *) Eval__exp(rbc_ctx, arg0);
  Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj for Eval__coll_end"));

  // [1]: process obj, get value object
  CONTEXT_SWITCH caller_ctx(dna);
  VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
  VSYM_OBJ_REP *obj_vor = vsa->Find_vor_mu_vor(rna->Callstmt(), obj, &any_vfr);
  if (obj_vor) {
    lists = (LIST_OBJS *) vsa->Vor_2_value_objs(obj_vor);
  }
  // [2]: create LIST_ITER object bind to return vor
  if (lists) {
    CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(rna->Callstmt());
    Is_True_Ret_Rbc_Invalid(ret_cr, ("null ret for Eval__coll_end"));
    VSYM_OBJ_REP *ret_vor = vsa->Find_vor_chi_vor(rna->Callstmt(), ret_cr, &any_vfr);
    if (ret_vor) {
      LIST_ITERS *iter_objs = (LIST_ITERS*)vsa->Create_value_objs(ret_vor, LIST_ITER_TYPE, 1);
      LIST_ITER *list_iter = CXX_NEW(LIST_ITER(lists->Size()), iter_objs->Mem_pool());
      iter_objs->Push_back(list_iter);
      ret.first = BOOL_T;
      ret.second = TRUE;
    }
  }
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__coll_remove -Symbolic execution for collection remove
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns <BOOL_T, TRUE>
//       if fail, return EVAL_RET_INVALID
//
// set_func_coll_remove(OBJECT v, OBJECT idx)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__coll_remove(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void* objs, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__coll_remove\n"));
  Is_True_Ret_Rbc_Invalid(objs, ("RBC_ERROR: Eval__coll_remove null VALUE_OBJS"));
  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee = rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  LIST_OBJS *lists = (LIST_OBJS*) objs;
  if (lists->Type() != LIST_TYPE) {
    // the UD of objs value contains other container operation, cancel the symbolic eval
    Is_Trace(Tracing(), (TFile, "RBC: Eval__coll_remove not supported objs type %d\n", lists->Type()));
    return EVAL_RET_INVALID;
  }

  EVAL_RET ret = EVAL_RET_INVALID;
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *idx_cr = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Ret_Rbc_Invalid(idx_cr != NULL, ("RBC ERROR: null idx passed to Eval__coll_remove"));
  if(idx_cr->Kind() == CK_CONST) {
    INT64 idx = idx_cr->Const_val();
    if(idx >= 0 && idx < lists->Size()) {
      lists->Erase(idx);
      ret.first = BOOL_T;
      ret.second = TRUE;
    } else {
      Is_Trace(Tracing(), (TFile, "RBC:Eval__coll_remove: mistached lists and index\n"));
    }
  } else {
    Is_Trace(Tracing(), (TFile, "TODO:Eval__coll_remove: non-const idx coll get\n"));
  }
  return ret;
}


// =============================================================================
// RBC_BASE::Eval__map_put - Symbolic execution for Map put
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns <BOOL_T, TRUE>
//       if fail, return EVAL_RET_INVALID
//
// set_func_map_put(OBJECT obj, OBJECT key, OBJECT value)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__map_put(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, void* objs, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__map_put\n"));
  Is_True_Ret_Rbc_Invalid(objs, ("RBC_ERROR: Eval__map_put null VALUE_OBJS"));
  MAP_OBJS *map_objs = (MAP_OBJS*)objs;
  if (map_objs->Type() != MAP_TYPE) {
    // the UD of objs value contains other container operation, cancel the symbolic eval
    Is_Trace(Tracing(), (TFile, "RBC: Eval__map_put not supported objs type %d\n", map_objs->Type()));
    return EVAL_RET_INVALID;
  }

  EVAL_RET ret = EVAL_RET_INVALID;

  DNA_NODE *dna = rbc_ctx.Caller();
  DNA_NODE *callee =rbc_ctx.Callee();
  RNA_NODE *rna = rbc_ctx.Rna();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  MEM_POOL *pool = rbc_ctx.Mem_pool();
  Is_True_Ret_Rbc_Invalid(callee != NULL,
                          ("RBC ERROR: unresolved callee for DNA_NODE:%d, RNA_NODE:%d\n",
                           dna->Dna_idx(), rna->Rna_idx()));
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *key = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + Rbc_parm_ofst_adjust(dna));
  CODEREP *value = (CODEREP *) Eval__exp(rbc_ctx, arg2);
  Is_True_Ret_Rbc_Invalid(key != NULL && value != NULL,
                          ("RBC ERROR: null key/value passed to Eval__coll_put"));
  if (deref) {
    Is_True_Ret(value->Kind() == CK_LDA, ("Deref without LDA"), EVAL_RET_INVALID);
    MU_NODE* mu = call_stmt->Mu_list()->Search_mu_node(value->Lda_aux_id());
    if (mu == NULL) {
      Is_Trace(Tracing(),
               (TFile, "RBC: Eval__map_put not find mu for deref in call %s.\n",
                       callee->Fname()));
      return EVAL_RET_INVALID;
    }
  }

  CONTEXT_SWITCH caller(dna);
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa, call_stmt, key, str_set, pool);
  // get first key
  char *key_str = found ? *(str_set.begin()) : NULL;

  // NOTE: the key didn't consider cross function yet, compare for single function for now
  for(int i = 0; i < map_objs->Size(); i++) {
    const MAP_ENTRY *obj = map_objs->Value_obj(i);
    if(obj->Same_key(key, key_str, vsa->Comp_unit())) {
      // remove old entry, and insert to last to keep insert order
      map_objs->Erase(i);
      break;
    }
  }
  // key_str memory is maintained in RBC_CONTEXT pool
  MAP_ENTRY* cand = CXX_NEW(MAP_ENTRY(key, value, call_stmt, vsa->Comp_unit(), key_str),
                            map_objs->Mem_pool());
  map_objs->Push_back(cand);
  ret.first = BOOL_T;
  ret.second = TRUE;
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__str_get - Symbolic execution for str get
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, return (CHAT_T, char value)
//
// Set_func_str_get(OBJECT str, OBJECT index)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__str_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__str_get\n"));
  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  STMTREP *call_stmt = rbc_ctx.Callstmt();
  EVAL_RET ret = EVAL_RET_INVALID;
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
  CODEREP *cr_str = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *cr_idx = (CODEREP *) Eval__exp(rbc_ctx, arg2);
  if (cr_idx->Kind() != CK_CONST) {
    return ret;
  }
  INT idx = cr_idx->Const_val();
  STR_SET str_set;
  BOOL found = Find_const_char_cross(vsa, call_stmt, cr_str, str_set, rbc_ctx.Mem_pool());
  if (found) {
    // support single const for now
    char *key_str = *(str_set.begin());
    Is_True_Ret(key_str, ("RBC_BASE::Eval__str_get null const string vaue"), EVAL_RET_INVALID);
    if (idx <= strlen(key_str)) {
      ret.first = CHAR_T;
      ret.second = (INT64) key_str[idx];
    }
  }
  return ret;
}

// =============================================================================
// RBC_BASE::Eval__map_get - Symbolic execution for map get
// @parm deref: if true, for c++ collection value pass by LDA
// @parm is_forward: if true do forward propagation for value objects
//                   if false do backward U-D traversal for collection values
// @ret: if success, returns the value objects at given idx
//       if fail, return EVAL_RET_INVALID
//
// set_func_map_get(OBJECT obj, OBJECT key)
// =============================================================================
EVAL_RET
RBC_BASE::Eval__map_get(RBC_CONTEXT &rbc_ctx, STMTREP *stmt, BOOL deref, BOOL is_forward)
{
  RBC_EVAL_SKIP_RET_INVALID();
  Is_Trace(Tracing(), (TFile, "RBC: calling Eval__map_get\n"));

  EVAL_RET ret = EVAL_RET_INVALID;

  DNA_NODE *dna = rbc_ctx.Caller();
  VSA *vsa = rbc_ctx.Caller_vsa();
  RNA_NODE *rna = rbc_ctx.Rna();
  MEM_POOL *pool = rbc_ctx.Mem_pool();
  CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + Rbc_parm_ofst_adjust(dna));
  CODEREP *key = (CODEREP *) Eval__exp(rbc_ctx, arg1);
  Is_True_Ret_Rbc_Invalid(key != NULL, ("RBC ERROR: null key passed to Eval__map_get"));
  if (deref) {
    Is_True_Ret(key->Kind() == CK_LDA, ("Deref without LDA"), EVAL_RET_INVALID);
    MU_NODE* mu = rna->Callstmt()->Mu_list()->Search_mu_node(key->Lda_aux_id());
    if (mu == NULL) {
      Is_Trace(Tracing(),
               (TFile, "RBC: Eval__map_get not find key mu for deref in call %s.\n",
                       rbc_ctx.Callee()->Fname()));
      return EVAL_RET_INVALID;
    }
    key = mu->OPND();
    // backward search one more statement to get the const def
    STMTREP *key_def = key->Defstmt();
    if (key_def && key_def->Opr() == OPR_STID) {
      key = key_def->Rhs();
    }
  }
  if (is_forward) {
    CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + Rbc_parm_ofst_adjust(dna));
    CODEREP *obj = (CODEREP *) Eval__exp(rbc_ctx, arg0);
    Is_True_Ret_Rbc_Invalid(obj != NULL, ("RBC ERROR: null obj passed to Eval__map_get"));
    CONTEXT_SWITCH caller_ctx(dna);
    VSYM_FLD_REP any_vfr(FLD_K_ANY, 0, 0);
    CHI_NODE *chi_node = vsa->Find_vor_chi(rna->Callstmt(), obj, &any_vfr);
    if (chi_node) {
      VSYM_OBJ_REP *vor_opnd = ((CVOR*)chi_node->OPND())->first;
      VSYM_OBJ_REP *vor_res = ((CVOR*)chi_node->RESULT())->first;
      MAP_OBJS *res_values = (MAP_OBJS *)vsa->Clone_value_objs(vor_res, vor_opnd);
      if (res_values) {
        res_values->Set_type(MAP_TYPE);
        CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(rna->Callstmt());
        Is_True_Ret_Rbc_Invalid(ret_cr, ("null ret for map get"));
        // return is preg, use FLD_K_ID
        VSYM_FLD_REP zero_vfr(FLD_K_ID, 0, 0);
        VSYM_OBJ_REP *ret_vor = vsa->Find_vor_chi_vor(rna->Callstmt(), ret_cr, &zero_vfr);
        if (ret_vor) {
          VSYM_OBJ_REP *next_vor = ret_vor->Next();
          Is_True_Ret_Rbc_Invalid(next_vor && next_vor->Attr() == ROR_DEF_BY_ISTORE,
                                  ("next vor is not def by istore"));
          STMTREP *def_sr = next_vor->Stmt_def();
          if (def_sr) {
            CODEREP *rhs = def_sr->Rhs();
            MAP_ENTRY *new_entry = CXX_NEW(MAP_ENTRY(key, rhs, def_sr, vsa->Comp_unit()),res_values->Mem_pool());
            // TODO: need to replace if contains existing key
            res_values->Push_back(new_entry);
          }
        }
      }
    }
  } else {
    CXX_MEM_POOL map_objs_pool("Temp_map_objs_pool", FALSE);
    MAP_OBJS map_objs(MAP_TYPE, map_objs_pool());
    MAP_OBJS *cands = CXX_NEW(MAP_OBJS(MAP_TYPE, pool), pool);
    EVAL_RET eval_ret = Eval__container_get(vsa, rna, pool, COLL_POS_IDX, key, map_objs, *cands);
    if(eval_ret != EVAL_RET_INVALID) {
      ret.first = MAP_PTR;
      ret.second = (INT64)cands;
    }
  }
  return ret;
}

template <typename T>
EVAL_RET
RBC_BASE::Eval__container_get(VSA *vsa_ctx, RNA_NODE *caller_rna, MEM_POOL *pool,
                              COLL_POSITION pos, CODEREP *key, VALUE_OBJS<T> &objs, VALUE_OBJS<T> &value_cands)
{
  CONTEXT_SWITCH caller_ctx(vsa_ctx->Dna());
  EVAL_RET ret = EVAL_RET_INVALID;
  // [Step 1] backward U-D traverse get all candidates srcpos
  STMTREP *callstmt = caller_rna->Callstmt();
  CODEREP *check_cr = vsa_ctx->Comp_unit()->Find_return_value(callstmt);
  Is_True_Ret_Rbc_Invalid(check_cr, ("RBC ERROR: null check_cr"));
  VAR_DEF_HELPER helper(check_cr, callstmt, vsa_ctx->Comp_unit(), FOR_CONTAINER_EVAL);
  CHECK_OBJ check_obj(check_cr, callstmt);
  vsa_ctx->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  if(def_info_vec.size() == 0) {
    Is_Trace(Tracing(), (TFile, "RBC:Eval__get_container: no collection U-D Chain\n"));
    return ret;
  }

  // [Step 2] iterate each candidates, forward symbolic evaluate for container ops
  //          get final container value
  SRCPOS_HANDLE *sp_h = helper.Srcpos();
  for(int cand_idx = 0; cand_idx < def_info_vec.size(); cand_idx++) {
    Is_Trace(Tracing(), (TFile, "RBC:Eval__get_container: process [%d] cand srcpos\n", cand_idx + 1));
    // adjust spos current node to candidates location
    DEF_INFO *def_info = def_info_vec[cand_idx];
    DNA_NODE *def_dna = def_info->Dna();
    CODEREP *def_cr = def_info->Coderep();
    STMTREP *def_sr = def_info->Stmtrep();
    CONTEXT_SWITCH def_ctx(def_dna);
    if (!(def_cr->Kind() == CK_OP &&
          def_cr->Opr() == OPR_INTRINSIC_CALL &&
          (def_cr->Intrinsic() == INTRN_ALLOC_OBJ ||
           (def_sr->Call_flags() & WN_CALL_IS_CONSTRUCTOR)))) {
      continue;
    }
    Is_True_Ret_Rbc_Invalid(def_info->Spos_node(), ("RBC ERROR: null spos candidate node"));
    sp_h->Reset_cur_node(def_info->Spos_node(), def_info->Spos_id());
    objs.Clear();
    // iterator through the spos handle, symbolic eval container op
    SRCPOS_TR_ITER srcpos_tr_iter(sp_h);
    EVAL_RET eval_ret(BOOL_T, TRUE);
    while (! srcpos_tr_iter.Is_empty() && eval_ret != EVAL_RET_INVALID) {
      SRCPOS_NODE vspos = srcpos_tr_iter.Next();
      if(vspos.Info() == PATHINFO_CALL_CHI && vspos.Stmt() != callstmt) {
        DNA_NODE *dna = vspos.Dna();
        STMTREP *stmt = vspos.Stmt();
        CONTEXT_SWITCH def_ctx(dna);
        VSA *vsa = dna->Comp_unit()->Vsa();
        RNA_NODE *rna = dna->Get_callsite_rna(stmt);
        if(rna != NULL && rna->Is_flag_set(RBC_SE_CONTAINER_OP)) {
          eval_ret = Eval__mvsa_container_op(dna, rna, pool, &objs, FALSE);
        } else if ((stmt->Opr() == OPR_INTRINSIC_CALL &&
                    (stmt->Rhs()->Intrinsic() == INTRN_ALLOC_OBJ ||
                     (def_sr->Call_flags() & WN_CALL_IS_CONSTRUCTOR))) ||
                   (OPERATOR_has_sym(stmt->Opr()) &&
                    PU_is_constructor(Pu_Table[ST_pu(stmt->St())]))) {
          // skip constructor
          continue;
        } else {
          Is_Trace(Tracing(), (TFile, "Eval_coll_value: not container op\n"));
          eval_ret = EVAL_RET_INVALID;
        }
      } // end of CALL_CHI
    } // end of iterate srcpos

    // [Step 3 ] for each candidate container, find value by key/index, add to return candidates
    if(eval_ret.first == BOOL_T && eval_ret.second == TRUE) {
      Is_Trace(Tracing(), (TFile, ("Container contents:\n")));
      Is_Trace_cmd(Tracing(), objs.Print(TFile));
      T obj = Get_value_obj(objs, callstmt, pos, key, vsa_ctx->Comp_unit(), pool);
      if(obj != NULL) {
        T obj_dup = obj->Clone(value_cands.Mem_pool());
        value_cands.Push_uniq(obj_dup);
        if (pos == COLL_POS_IDX) {
          Is_Trace(Tracing(), (TFile, "Found value obj for key[%d]\n", key->Coderep_id()));
        } else {
          Is_Trace(Tracing(), (TFile, "Found value obj at %s\n", pos == COLL_POS_FRONT ? "front" : "back"));
        }
        Is_Trace_cmd(Tracing(), obj->Print(TFile));
      } else {
        Is_Trace(Tracing(),
                 (TFile, "RBC:Eval__container_get: unable to find key/idx in value_objs\n"));
      }
    } else {
      Is_Trace(Tracing(), (TFile, "RBC:Eval_container_get: failed eval container op\n"));
    }
  } // end of srcpos candidates
  if(value_cands.Size() > 0) {
    ret.first = BOOL_T;
    ret.second = TRUE;
  }
  return ret;
}

// =============================================================================
//
// RBC_BASE::Report_rbc_error spotted on given spos
//
// =============================================================================
void
RBC_BASE::Report_rbc_error(VSA *vsa_ctx, SRCPOS spos, const char* rule, BOOL maybe,
                           SRCPOS_HANDLE *srcpos_h, const char* cname)
{
  Is_True(rule != NULL, ("RBC ERROR: null rule passed to Report_rbc_error.\n"));
  Is_True(spos != 0, ("RBC ERROR: SRCPOS 0 passed to Report_rbc_error.\n"));
  Is_True(srcpos_h != NULL, ("RBC ERROR: null srcpos_h passed to Report_rbc_error.\n"));
  if (rule == NULL || spos == 0 || srcpos_h == NULL)
    return;

  if (!VSA_Customized_Rule && strncmp(rule, "CUS-", 4) == 0)
    return;

  const char *output_pu_name = srcpos_h->Orig_puname();
  if (output_pu_name == NULL)
    output_pu_name = vsa_ctx->Cur_pu_name();
  const char *output_var_name = srcpos_h->Orig_stname();
  if (output_var_name == NULL || Vsa_check_sym_ignore(output_var_name))
    output_var_name = "";
  if (cname == NULL) {
    cname = Get_rule_set(rule);
  }
  char key_buf[ISKEY_MAX_KEY_LEN];
  IDTYPE keyid;
  const char* key = srcpos_h->Path_to_key(key_buf, sizeof(key_buf),
                                          output_var_name, rule, cname, keyid);
  spos = SRCPOS_NODE::Transpos(spos, vsa_ctx ? vsa_ctx->Dna()->File_idx()
                                             : File_Index);
  VSA_ISSUE issue(cname, rule, key, keyid, output_var_name, output_pu_name, spos,
                  maybe ? IC_MAYBE : IC_DEFINITELY, srcpos_h);
  issue.Set_fix_cost(Rule_fix_cost(rule));
  Vsa_error_print(&issue);
}


// =============================================================================
//
// RBC_BASE::Report_rbc_error spotted on given stmt
//
// =============================================================================
void
RBC_BASE::Report_rbc_error(VSA *vsa_ctx, STMTREP *stmt, const char* rule, BOOL maybe,
                           SRCPOS_HANDLE *srcpos_h, const char* cname)
{
  Is_True(stmt != NULL, ("RBC ERROR: null stmt passed to Report_rbc_error.\n"));
  Is_True(rule != NULL, ("RBC ERROR: null rule passed to Report_rbc_error.\n"));
  Is_True(srcpos_h != NULL, ("RBC ERROR: null srcpos_h passed to Report_rbc_error.\n"));
  if (stmt == NULL || rule == NULL || srcpos_h == NULL)
    return;

  Report_rbc_error(vsa_ctx, stmt->Linenum(), rule, maybe, srcpos_h, cname);
}

// =============================================================================
//
// RBC_BASE::Report_rbc_error: report rbc error at function entry
//
// =============================================================================
void
RBC_BASE::Report_rbc_error(DNA_NODE *dna, const char *stname, const char *rule,
                           const char *msg_id, BOOL maybe)
{
  if(dna == NULL) {
    return;
  }

  CONTEXT_SWITCH ctx(dna);
  COMP_UNIT *cu = dna->Comp_unit();
  VSA *vsa = cu->Vsa();
  SRCPOS_HANDLE sp_h(dna, vsa->Loc_pool());
  STMTREP *stmt = vsa->Get_entry_chi_stmt();
  if(stmt) {
    sp_h.Append_data(stmt, dna, PATHINFO_VUL_SPOT);
    sp_h.Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
    sp_h.Set_orig_stname(stname);
    if(msg_id != NULL) {
      sp_h.Set_msgid(msg_id);
    }
    Report_rbc_error(vsa, stmt, rule, maybe ? IC_MAYBE : IC_DEFINITELY, &sp_h);
  }
}


// =============================================================================
//
// RBC_BASE::Report_fsm_error
//
// =============================================================================
void
RBC_BASE::Report_fsm_error(VSA *vsa_ctx, FSM_TRAV_CONTEXT *fsm_ctx, STMTREP *stmt, FSM_OBJ_REP *fsm_obj_rep,
                           TRANSIT *ts, SRCPOS_HANDLE *srcpos_h, FSM_ERR_KIND kind)
{
  if (fsm_obj_rep == NULL)
    return;

  FSM *fsm = fsm_obj_rep->Fsm();
  STRING fsm_name = fsm_obj_rep->Fsm_obj()->Fsm_name();
  IDTYPE state = ts->State();
  IDTYPE nstate = ts->Nstate();
  if (kind == FSM_ERR_KIND_TRANSIT) {
    Is_True(fsm_obj_rep->Transit() == ts, ("RBC ERROR: inconsistent transit\n"));
    if (fsm_obj_rep->Transit() != ts)
      return;
    Is_Trace(Tracing(), (TFile, "RBC: FSM(\"%s\"): report error for state(%d) => state(%d) by action(\"%s\")\n",
                         fsm_name, state, nstate, ts->Action()));
  }
  else if (kind == FSM_ERR_KIND_DEFAULT) {
    Is_True(ts->Is_default(), ("RBC ERROR: not default transit\n"));
    if (!ts->Is_default())
      return;
  }
  else if (kind == FSM_ERR_KIND_DANGLING) {
    Is_True(ts->Is_default(), ("RBC ERROR: not default transit\n"));
    if (!ts->Is_default())
      return;
    Is_Trace(Tracing(), (TFile, "RBC: FSM(\"%s\"): report dangling error for state(%d)\n",
                         fsm_name, state));
  }
  else {
    Is_Trace(Tracing(), (TFile, "RBC ERROR: invalid FSM_ERR_KIND(%d)\n", kind));
    return;
  }

  // clone srcpos to reverse the path for fsm forward check
  SRCPOS_HANDLE *sh = srcpos_h->Clone();
  // STRING message = Generate_fsm_error_var(fsm, ts, state, nstate);
  // sh->Add_message(message);
  const char *var_name = NULL;
  if (sh->Orig_stname() == NULL) {
    CODEREP *key = NULL;
    if (fsm_ctx != NULL)
      key = fsm_ctx->Cur_key();
    else
      key = fsm_obj_rep->Key();
    if (key != NULL)
      var_name = sh->Find_orig_stname(key, stmt, vsa_ctx->Dna());
    if (var_name == NULL || Vsa_check_sym_ignore(var_name))
      var_name = fsm_obj_rep->Transit()->Action();
    sh->Set_orig_stname(var_name);
  }
  for (INT i = 0; i < ts->Errcode()->size(); i++) {
    const char *errcode = (*ts->Errcode())[i];
    if (strncmp(errcode, "MSR", 3) == 0) {
      if (VSA_Xsca) {
        Report_xsca_error(vsa_ctx, stmt->Linenum(), errcode, sh);
      }
    }
    else {
      // Changes as customer's require to report function name
      // for ERR33-C instead of variable name
      const char *old_name = sh->Orig_stname();
      if (strncmp(errcode, "ERR33-C", 7) == 0) {
        if (strncmp(fsm_name, "pos35c", 6) == 0) {
          sh->Set_orig_stname("open");
        }
        else if (strncmp(fsm_name, "fio45c", 6) == 0) {
          sh->Set_orig_stname("fopen");
        }
      }
      Report_rbc_error(vsa_ctx, stmt, errcode, FALSE, sh);
      sh->Set_orig_stname(old_name);
    }
  }
  // if (message)
  //   free(message);
}

// =============================================================================
//
// RBC_BASE::Report_xvsa_error
//
// =============================================================================
void
RBC_BASE::Report_xsca_error(VSA *vsa_ctx, SRCPOS spos, const char* rule,
                            SRCPOS_HANDLE *srcpos_h)
{
  Report_rbc_error(vsa_ctx, spos, rule, FALSE, srcpos_h, "SML");
}

// =============================================================================
//
// RBC_BASE::Evaluate_and_check_FSMs: it performs FSM evaluation & check, will
//     start the check by identifying transitions from start state to some
//     other state, it should through callee recursively searching for state
//     transitions, when all statements are processed and FSM is not in final
//     state, go to caller and go through statements after the call for state
//     transition recursively.
//
// =============================================================================
void
RBC_BASE::Evaluate_and_check_FSMs(IPSA *ipsa)
{
  // search transitions that transit "start" state to some nother state
  // to start evaluation & check for the FSM
  Is_Trace(Tracing(), (TFile, "%sRBC: Evaluate_and_check_FSMs\n%s", DBar, DBar));
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func->Non_functional()) continue;

    VSA *vsa = func->Comp_unit()->Vsa();
    Is_True(vsa != NULL, ("RBC ERROR: null VSA in Evaluate_and_check_FSMs.\n"));
    if (vsa == NULL) continue;

    if (vsa->Count_fo_list() != 0) {
      CONTEXT_SWITCH context(func);

      MEM_POOL fsm_analyze_pool;
      OPT_POOL_Initialize(&fsm_analyze_pool, "FSM analyze pool", FALSE, VSA_DUMP_FLAG);
      OPT_POOL_Push(&fsm_analyze_pool, SSA_DUMP_FLAG);

      Is_Trace(Tracing(), (TFile, "RBC: Evaluate in func %s\n", func->Fname()));
      Match_start_trans_and_eval(vsa, vsa->Comp_unit()->Cfg()->Entry_bb(), &fsm_analyze_pool);

      OPT_POOL_Pop(&fsm_analyze_pool, SSA_DUMP_FLAG);
      OPT_POOL_Delete(&fsm_analyze_pool, SSA_DUMP_FLAG);
    }
  }
}


// =============================================================================
//
// RBC_BASE::Match_start_trans_and_eval: go through stmts searching for
//     transition applies on start state.
//
// =============================================================================
void
RBC_BASE::Match_start_trans_and_eval(VSA *vsa, BB_NODE *bb, MEM_POOL *pool)
{
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    FOR_ARRAY *for_array = vsa->Sr_2_for_array(stmt);
    if (for_array == NULL)
      continue;
    for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++) {
      FSM_OBJ_REP *fsm_obj_rep = (*for_array)[fa_idx];
      if (fsm_obj_rep == NULL)
        continue;
      FSM *fsm = fsm_obj_rep->Fsm();
      TRANSIT *ts = fsm_obj_rep->Transit();
      if (ts == NULL || ts->Is_default())
        continue;
      IDTYPE state = ts->State();
      // not a start transit, skip
      if (state != fsm->Start_state())
        continue;
      IDTYPE next_state = ts->Nstate();
      STRING action = ts->Action();
      if (action == NULL)
        continue;
      STRING fsm_name = fsm_obj_rep->Fsm_obj()->Fsm_name();
      // assume an FSM starts from a function call always
      DNA_NODE *dna = vsa->Dna();
      RNA_NODE *rna = dna->Get_callsite_rna(stmt);
      if (rna == NULL)
        continue;
      for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
           iter != rna->Callee_list().end(); iter++) {
        DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
        if (callee == NULL)
          continue;
        if (vsa->Is_action_match(stmt, action, callee->Fname()) == TS_NOT_MATCH)
          continue;
        CODEREP *key = fsm_obj_rep->Key();
        {
          RBC_CONTEXT rbc_ctx(dna, callee, fsm->Dna(), rna, pool);
          TAG_INFO *tag_info = Find_dna_tag_info(action, callee);
          if (tag_info != NULL) {
            rbc_ctx.Set_tag_info(tag_info);
          }
          CONTEXT_SWITCH context(fsm->Dna());
          if (key == NULL) {
            Rbc_init();
            key = (CODEREP*)Eval__exp(rbc_ctx, ts->Key());
            if (Rbc_result_ignore())
              continue;
          }
          Rbc_init();
          UINT64 retv = Eval__exp(rbc_ctx, ts->Cond());
          if (retv == 0 || Rbc_result_ignore())
            continue;
        }
        // found a matched start transit, start evaluation
        SRCPOS_HANDLE *srcpos_h = CXX_NEW(SRCPOS_HANDLE(dna, pool), pool);
        srcpos_h->Append_data(stmt, dna, ts->Msg_id());
        const char *key_str = Generate_fsm_key_str(key, stmt, action, dna, srcpos_h);
        srcpos_h->Set_orig_stname(key_str);
        Is_Trace(Tracing(),
                 (TFile,
                  "%sRBC::Match_start_trans_and_eval: found FSM(%s) start transit: state(%d) => state(%d) in BB%d, STMT%d with action(%s), key(%s)\n",
                  SBar, fsm_name, state, next_state, bb->Id(), stmt->Stmtrep_id(), action,
                  key_str == NULL ? "null" : key_str));
        Is_Trace_cmd(Tracing(), vsa->Print_sr(stmt, TFile));
        fsm_obj_rep->Set_state(next_state);
        fsm_obj_rep->Set_key(key);
        FSM_TRAV_CONTEXT fsm_ctx(fsm_name, fsm, pool);
        fsm_ctx.Set_cur_key(key);
        if (ts->Errcode()->size() != 0) {
          Report_fsm_error(vsa, &fsm_ctx, stmt, fsm_obj_rep, ts, srcpos_h, FSM_ERR_KIND_TRANSIT);
        }
        // this is top frame
        fsm_ctx.Push_frame(rna, bb, stmt->Next(), key, key_str, vsa->Comp_unit(), state, TD_NONE);
        // continue FSM evaluation
        Perform_fsm_check_bb(fsm_obj_rep, stmt, bb, fsm_ctx, srcpos_h);
        if (fsm_ctx.Skip()) {
          Is_Trace(Tracing(),
                   (TFile,
                    "RBC: FSM(%s) cancel traversal in func(%s) due to stack too deep!\n",
                    fsm_name, dna->Fname()));
        }
      } // end Callee_list
      Is_Trace(Tracing(), (TFile, "%s", SBar));
    } // end for_array
  }

  // do the same for its dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Match_start_trans_and_eval(vsa, dom_bb, pool);
}


// =============================================================================
//
// RBC_BASE::Perform_fsm_check_caller: go through callers to evaluate
//     FSM state transition, if can not complete in current context.
//
// =============================================================================
BOOL
RBC_BASE::Perform_fsm_check_caller(FSM_OBJ_REP *fsm_obj_rep, FSM_TRAV_CONTEXT &fsm_ctx,
                                   SRCPOS_HANDLE *srcpos_h)
{
  if (srcpos_h != NULL && !fsm_ctx.Skip() && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip FSM(%s) due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         fsm_ctx.Fsm_name(), srcpos_h->Children_count(), fsm_ctx.Cur_dna()->Fname()));
    fsm_ctx.Set_skip();
  }
  if (fsm_ctx.Skip())
    return FALSE;
  BOOL found = FALSE;
  VSA *vsa = fsm_ctx.Cur_vsa();
  DNA_NODE *dna = vsa->Dna();
  FSM_TRAV_FRAME *frame = fsm_ctx.Pop_frame(TD_DOWN);
  RNA_NODE *prev_rna = NULL;
  if (frame != NULL) {
    // we are in callee from a caller, now go back to caller
    prev_rna = frame->Rna();
    FSM_TRAV_FRAME *second_frame = NULL;
    // from an implicit call, pop one more frame
    if (prev_rna->Is_flag_set(RBC_SE_IMPLICIT_CALL)) {
      second_frame = fsm_ctx.Pop_frame(TD_DOWN);
      Is_True(second_frame != NULL, ("RBC ERROR: implicit call frame mismatch.\n"));
      if (second_frame == NULL)
        return found;
      prev_rna = second_frame->Rna();
    }
    // restore frame and duplicate caller frame and push caller frame
    TRAV_DIRECTION dir = TD_DOWN;
    FSM_TRAV_FRAME *caller_frame = fsm_ctx.Top_frame(dir);
    if (caller_frame == NULL) {
      dir = TD_UP;
      caller_frame = fsm_ctx.Top_frame(dir);
    }
    if (caller_frame == NULL) {
      dir = TD_NONE;
      caller_frame = fsm_ctx.Top_frame(dir);
    }
    Is_True(caller_frame != NULL, ("RBC ERROR: null caller_frame\n"));
    if (second_frame != NULL)
      fsm_ctx.Push_frame(second_frame);
    fsm_ctx.Push_frame(frame);
    if (fsm_ctx.Visited(prev_rna, TD_UP))
      return found;
    // if (second_frame != NULL)
    //   fsm_ctx.Push_frame(second_frame.Duplicate(TD_UP));
    fsm_ctx.Push_frame(fsm_ctx.Duplicate(caller_frame, TD_UP));
    found = TRUE;
    if (second_frame != NULL)
      frame = second_frame;
    STMTREP *stmt = frame->Stmt();
    BB_NODE *bb = frame->Bb();
    CODEREP *key = frame->Key();
    const char *key_str = frame->Key_string();
    CODEREP *old_key = fsm_ctx.Cur_key();
    const char *old_key_str = srcpos_h->Orig_stname();
    DNA_NODE *caller = fsm_ctx.Cur_dna();
    // switch key to caller
    fsm_ctx.Set_cur_key(key);
    srcpos_h->Set_orig_stname(key_str);
    srcpos_h->Append_data(prev_rna->Callstmt(), caller, PATHINFO_DNA_CALLRETURN);
    Is_Trace(Tracing(), (TFile, "RBC: set new key for callee return: (cr%d:%s) => (cr%d:%s)\n",
                         old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                         key == NULL ? -1 : key->Coderep_id(), key_str));
    // we re-enter this bb by going back, reset here
    fsm_ctx.Reset_visited(bb);
    Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) return from callee(%s)\n", fsm_ctx.Fsm_name(), dna->Fname()));
    // printf("RBC: FSM(%s) return from callee(%s) via RNA_NODE(%d)\n", fsm_ctx.Fsm_name(), dna->Fname(), prev_rna->Rna_idx());
    CONTEXT_SWITCH context(caller);
    Perform_fsm_check_bb(fsm_obj_rep, stmt, bb, fsm_ctx, srcpos_h);
    fsm_ctx.Pop_frame(TD_UP);
    fsm_ctx.Reset_visited(prev_rna, TD_UP);
    // switch key back
    fsm_ctx.Set_cur_key(old_key);
    srcpos_h->Set_orig_stname(old_key_str);
  }
  else {
    // we've done current function, try to go back to its caller
    // first check if there's a caller frame it should go back to, for example
    // a -> b -> c -> b, done visiting 'b', need to go back to 'a'
    // ^              |
    // |              |
    // ----------------
    UINT32 call_depth = fsm_ctx.Stack_size();
    FSM_TRAV_CONTEXT tmp_ctx((STRING)".tmp_ctx", NULL, fsm_ctx.Mem_pool());
    FSM_TRAV_FRAME *caller_frame = NULL;
    frame = fsm_ctx.Pop_frame(TD_UP);
    FSM_TRAV_FRAME *tmp_frame = frame;
    COMP_UNIT *callee_cu = NULL;
    if (frame != NULL) {
      tmp_ctx.Push_frame(frame);
      // find the caller frame 'a' that calls current frame 'b'
      callee_cu = frame->Comp_unit();
      while (caller_frame == NULL) {
        frame = fsm_ctx.Pop_frame(TD_UP);
        if (frame == NULL)
          frame = fsm_ctx.Pop_frame(TD_DOWN);
        if (frame == NULL)
          break;
        tmp_ctx.Push_frame(frame);
        if (frame->Comp_unit() == callee_cu && frame->Direction() == TD_DOWN) {
          caller_frame = fsm_ctx.Top_frame(TD_DOWN);
          if (caller_frame == NULL)
            caller_frame = fsm_ctx.Top_frame(TD_UP);
          // 'a' may be the function that holds the starting transit of an FSM,
          // its frame has no direction in that case
          if (caller_frame == NULL)
            caller_frame = fsm_ctx.Top_frame(TD_NONE);
          prev_rna = frame->Rna();
        }
      }
      while (tmp_ctx.Stack_size() > 0) {
        frame = tmp_ctx.Pop_frame(TD_UP);
        if (frame == NULL)
          frame = tmp_ctx.Pop_frame(TD_DOWN);
        fsm_ctx.Push_frame(frame);
      }
      Is_True(fsm_ctx.Stack_size() == call_depth, ("RBC: FSM stack size wrong!\n"));
    }
    if (caller_frame != NULL) {
      // found a caller frame, go back to it
      Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) found caller(%s) for current func(%s)\n",
                           fsm_ctx.Fsm_name(),
                           caller_frame->Comp_unit()->Dna()->Fname(),
                           tmp_frame->Comp_unit()->Dna()->Fname()));
      frame = tmp_frame;
      if (fsm_ctx.Visited(prev_rna, TD_UP))
        return found;
      fsm_ctx.Push_frame(fsm_ctx.Duplicate(caller_frame, TD_UP));
      found = TRUE;
      STMTREP *stmt = frame->Stmt();
      BB_NODE *bb = frame->Bb();
      CODEREP *key = frame->Key();
      const char *key_str = frame->Key_string();
      CODEREP *old_key = fsm_ctx.Cur_key();
      const char *old_key_str = srcpos_h->Orig_stname();
      DNA_NODE *caller = fsm_ctx.Cur_dna();
      // switch key to caller
      fsm_ctx.Set_cur_key(key);
      srcpos_h->Set_orig_stname(key_str);
      srcpos_h->Append_data(prev_rna->Callstmt(), caller, PATHINFO_DNA_CALLRETURN);
      Is_Trace(Tracing(), (TFile, "RBC: set new key for callee return: (cr%d:%s) => (cr%d:%s)\n",
                           old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                           key == NULL ? -1 : key->Coderep_id(), key_str));
      // we re-enter this bb by going back, reset here
      fsm_ctx.Reset_visited(bb);
      Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) return from callee(%s)\n", fsm_ctx.Fsm_name(), dna->Fname()));
      CONTEXT_SWITCH context(caller);
      Perform_fsm_check_bb(fsm_obj_rep, stmt, bb, fsm_ctx, srcpos_h);
      fsm_ctx.Pop_frame(TD_UP);
      fsm_ctx.Reset_visited(prev_rna, TD_UP);
      // switch key back
      fsm_ctx.Set_cur_key(old_key);
      srcpos_h->Set_orig_stname(old_key_str);
    }
    else {
      // there's no caller frame 'a', now try to go to its possible callers
      Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) current func(%s) does not have a caller frame\n",
                           fsm_ctx.Fsm_name(), fsm_ctx.Cur_dna()->Fname()));
      RNODE_VECTOR *clby_list = vsa->Dna()->Clby_list();
      INT idx = 0;
      INT parent_idx = srcpos_h->Cur_idx();
      srcpos_h->Add_children(clby_list->size());
      SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
      for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
        BOOL found_once = FALSE;
        RNA_NODE *caller_rna = (*clby_list)[i];
        if (caller_rna == NULL)
          continue;
        if (fsm_ctx.Visited(caller_rna, TD_UP))
          continue;
        DNA_NODE *caller = vsa->Ipsa()->Get_dna(caller_rna->Caller_idx());
        if (caller == NULL)
          continue;
        if (caller->Non_functional())
          continue;
        STMTREP *call_stmt = caller_rna->Callstmt();
        srcpos_h->Set_cur_node(cur_node, idx);
        srcpos_h->Append_data(call_stmt, caller, PATHINFO_DNA_CALLRETURN);
        idx++;
        // try to switch key
        CODEREP *old_key = fsm_ctx.Cur_key();
        CODEREP *new_key = NULL;
        BOOL found_new_key = Find_fsm_key_in_caller(vsa, fsm_obj_rep, caller_rna, caller, &new_key);
        if (old_key == NULL || new_key != NULL) {
          // NULL key implies FSM uses no key, should step into caller anyway
          // or key is passed to caller, should step into it
          found_once = TRUE;
        }
        if (found_once) {
          found = TRUE;
          // got a key switching in caller
          BB_NODE *bb = call_stmt->Bb();
          const char *old_key_str = srcpos_h->Orig_stname();
          const char *new_key_str = Generate_fsm_key_str(new_key, call_stmt, NULL, caller, srcpos_h);
          if (found_new_key) {
            // set key used in caller
            fsm_ctx.Set_cur_key(new_key);
            srcpos_h->Set_orig_stname(new_key_str);
            Is_Trace(Tracing(), (TFile, "RBC: set new key for caller: (cr%d:%s) => (cr%d:%s)\n",
                                 old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                                 new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
          }
          Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) switch to caller(%s)\n", fsm_ctx.Fsm_name(), caller->Fname()));
          // we will not go back to this frame, so set bb, stmt to NULL is safe
          fsm_ctx.Push_frame(caller_rna, NULL, NULL, old_key, old_key_str,
                             caller->Comp_unit(), fsm_obj_rep->State(), TD_UP);
          CONTEXT_SWITCH caller_ctx(caller);
          Perform_fsm_check_bb(fsm_obj_rep, call_stmt->Next(), bb, fsm_ctx, srcpos_h);
          fsm_ctx.Pop_frame(TD_UP);
          // switch key back
          fsm_ctx.Set_cur_key(old_key);
          srcpos_h->Set_orig_stname(old_key_str);
        }
        fsm_ctx.Reset_visited(caller_rna, TD_UP);
      } // end for clby_list
      srcpos_h->Reset_cur_node(cur_node, parent_idx);
    }
  }
  return found;
}


// =============================================================================
//
// RBC_BASE::Perform_fsm_check_callee: go through callee to evaluate
//     FSM state transition, if transit to a call stmt
//
// =============================================================================
BOOL
RBC_BASE::Perform_fsm_check_callee(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt,
                                   FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h)
{
  if (srcpos_h != NULL && !fsm_ctx.Skip() && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip FSM(%s) due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         fsm_ctx.Fsm_name(), srcpos_h->Children_count(), fsm_ctx.Cur_dna()->Fname()));
    fsm_ctx.Set_skip();
  }
  if (fsm_ctx.Skip())
    return FALSE;
  BOOL found = FALSE;
  Is_True(stmt && OPERATOR_is_call(stmt->Opr()),
          ("RBC_BASE::Perform_fsm_check_callee not a call stmt\n"));
  if (stmt == NULL || !OPERATOR_is_call(stmt->Opr()))
    return found;
  VSA *vsa = fsm_ctx.Cur_vsa();
  RNA_NODE *rna = fsm_ctx.Cur_dna()->Get_callsite_rna(stmt);
  FSM *fsm = fsm_ctx.Fsm();
  if (fsm_ctx.Visited(rna, TD_DOWN))
    return found;
  Is_Trace(Tracing(), (TFile, "RBC::Perform_fsm_check_callee: try RNA_NODE(%d)\n",
                       rna->Rna_idx()));
  // record call site in path
  INT idx = 0;
  INT parent_idx = srcpos_h->Cur_idx();
  srcpos_h->Add_children(rna->Callee_list().size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
       iter != rna->Callee_list().end(); iter++) {
    DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
    BOOL found_once = FALSE;
    if (callee == NULL)
      continue;
    // set up path when stepping into callee
    srcpos_h->Set_cur_node(cur_node, idx);
    srcpos_h->Append_data(rna->Callstmt(), vsa->Dna(), PATHINFO_DNA_CALLSITE);
    idx++;
    if (!callee->Non_functional()) {
      // try to find if key is used in callee
      CODEREP *old_key = fsm_ctx.Cur_key();
      CODEREP *new_key = NULL;
      BOOL found_new_key = Find_fsm_key_in_callee(vsa, fsm_obj_rep, old_key, rna, callee, &new_key);
      if (old_key == NULL || new_key != NULL) {
        // NULL key implies FSM uses no key, should step into callee anyway
        // or if key is passed to callee, should step into it
        found_once = TRUE;
      }
      if (found_once) {
        found = TRUE;
        // got a key swiching in callee
        BB_NODE *bb = callee->Comp_unit()->Cfg()->Entry_bb();
        STMTREP *callee_stmt = bb->First_stmtrep();
        const char *old_key_str = srcpos_h->Orig_stname();
        const char *new_key_str = Generate_fsm_key_str(new_key, callee_stmt, NULL, callee, srcpos_h);
        if (found_new_key) {
          // set key used in callee
          fsm_ctx.Set_cur_key(new_key);
          srcpos_h->Set_orig_stname(new_key_str);
          Is_Trace(Tracing(), (TFile, "RBC: set new key for callee: (cr%d:%s) => (cr%d:%s)\n",
                               old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                               new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
        }
        Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) switch to callee(%s)\n", fsm_ctx.Fsm_name(), callee->Fname()));
        // printf("RBC: FSM(%s) switch to callee(%s) via RNA_NODE(%d)\n", fsm_ctx.Fsm_name(), callee->Fname(), rna->Rna_idx());
        fsm_ctx.Push_frame(rna, stmt->Bb(), stmt->Next(), old_key, old_key_str,
                           callee->Comp_unit(), fsm_obj_rep->State(), TD_DOWN);
        CONTEXT_SWITCH callee_ctx(callee);
        Perform_fsm_check_bb(fsm_obj_rep, callee_stmt, bb, fsm_ctx, srcpos_h);
        fsm_ctx.Pop_frame(TD_DOWN);
        // switch key back
        fsm_ctx.Set_cur_key(old_key);
        srcpos_h->Set_orig_stname(old_key_str);
      }
    } // else Non_functional
    else if (callee->Is_set_rbc_flag(DNA_RBC_SE_EVAL)) {
      RNODE_VECTOR *rna_list = callee->Call_list();
      for (INT i = VAR_INIT_ID; i < rna_list->size(); i++) {
        BOOL found_once = FALSE;
        RNA_NODE *eval_rna = (*rna_list)[i];
        if (fsm_ctx.Visited(eval_rna, TD_DOWN))
          continue;
        if (eval_rna->Is_flag_set(RBC_SE_IMPLICIT_CALL)) {
          if (eval_rna && eval_rna->Callee_cnt() > 1)
            Is_Trace(Tracing(), (TFile, "-DEVIRTJAVA RBC_BASE::FSM_eval_and_check_callee: multiple callee "
                         "while calling uniq_callee, cnt = %d, eval_rna : %d.\n",
                         eval_rna->Callee_cnt(), eval_rna->Rna_idx()));
          DNA_NODE *eval_callee = vsa->Ipsa()->Get_dna(eval_rna->Uniq_callee());

          if(eval_rna->Callee_cnt() > 1) {
            Warn_todo("RBC_BASE: support multiple callee for implicit call.");
            Is_Trace(Tracing(), (TFile, "TODO: RBC_BASE: support multiple callee for implicit call."));
            continue;
          }
          if (eval_callee == NULL) {
            Is_Trace(Tracing(), (TFile, "NULL RBC EVAL Callee"));
            continue;
          }
          VSA *eval_vsa = eval_callee->Comp_unit()->Vsa();
          // try to find if key is used in callee
          CODEREP *old_key = fsm_ctx.Cur_key();
          CODEREP *new_key = NULL;
          BOOL found_new_key = FALSE;
          DNA_NODE *init_dna = NULL;
          STMTREP *init_stmt = Get_init_stmt(vsa, stmt, stmt->Rhs()->Opnd(0), &init_dna);
          if (init_dna != NULL && init_stmt != NULL) {
            RNA_NODE *init_rna = init_dna->Get_callsite_rna(init_stmt);
            if (init_rna == NULL)
              continue;
            if (init_rna && init_rna->Callee_cnt() > 1)
              Is_Trace(Tracing(), (TFile, "-DEVIRTJAVA RBC_BASE::Perform_fsm_check_callee: multiple callee "
                                   "while calling uniq_callee, cnt = %d, init_rna : %d.\n",
                                   init_rna->Callee_cnt(), init_rna->Rna_idx()));
            DNA_NODE *init_callee = vsa->Ipsa()->Get_dna(init_rna->Uniq_callee());
            if (init_callee == NULL)
              continue;
            // var2
            found_new_key = Find_fsm_key_in_callee(vsa, fsm_obj_rep, old_key, init_rna, init_callee, &new_key);
            // this.<blabla> = var2
            hash_set<IDTYPE> visited_run_bb;
            new_key = Find_thread_init_key(init_callee->Comp_unit()->Cfg()->Entry_bb(), new_key, visited_run_bb);
            // switch key in run()
            visited_run_bb.clear();
            new_key = Find_thread_run_key(eval_vsa, eval_vsa->Cfg()->Entry_bb(),
                                          init_callee->Comp_unit()->Vsa(), new_key, visited_run_bb);
          }
          if (old_key == NULL || new_key != NULL) {
            // NULL key implies FSM uses no key, should step into callee anyway
            // of if key is passed to callee, should step into it
            found_once = TRUE;
          }
          if (found_once) {
            found = TRUE;
            // got a key switching in callee
            BB_NODE *bb = eval_callee->Comp_unit()->Cfg()->Entry_bb();
            STMTREP *callee_stmt = bb->First_stmtrep();
            const char *old_key_str = srcpos_h->Orig_stname();
            const char *new_key_str = Generate_fsm_key_str(new_key, NULL, NULL, eval_callee, srcpos_h);
            if (found_new_key) {
              // set key used in callee
              fsm_ctx.Set_cur_key(new_key);
              srcpos_h->Set_orig_stname(new_key_str);
              Is_Trace(Tracing(), (TFile, "RBC: set new key for implicit callee: (cr%d:%s) => (cr%d:%s)\n",
                                   old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                                   new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
            }
            Is_Trace(Tracing(), (TFile, "RBC: FSM(%s) switch to callee(%s)\n", fsm_ctx.Fsm_name(), eval_callee->Fname()));
            // push frame for source => callee
            fsm_ctx.Push_frame(rna, stmt->Bb(), stmt->Next(), old_key, old_key_str,
                               callee->Comp_unit(), fsm_obj_rep->State(), TD_DOWN);
            // push frame for callee => implicit callee
            fsm_ctx.Push_frame(eval_rna, stmt->Bb(), stmt->Next(), old_key, old_key_str,
                               eval_callee->Comp_unit(), fsm_obj_rep->State(), TD_DOWN);
            // srcpos_h->Append_data(callee_stmt, eval_callee, PATHINFO_DNA_CALLSITE);
            CONTEXT_SWITCH callee_ctx(eval_callee);
            Perform_fsm_check_bb(fsm_obj_rep, callee_stmt, bb, fsm_ctx, srcpos_h);
            fsm_ctx.Pop_frame(TD_DOWN);
            fsm_ctx.Pop_frame(TD_DOWN);
            // switch key back
            fsm_ctx.Set_cur_key(old_key);
            srcpos_h->Set_orig_stname(old_key_str);
          }
        }
        fsm_ctx.Reset_visited(eval_rna, TD_DOWN);
      } // end for rna_list
    } // end if DNA_RBC_SE_EVAL
  } // end for CALLEE_VECTOR
  // restore path to call site
  fsm_ctx.Reset_visited(rna, TD_DOWN);
  srcpos_h->Reset_cur_node(cur_node, parent_idx);
  return found;
}


// =============================================================================
//
// RBC_BASE::Find_arg_in_callee:
//
// =============================================================================
BOOL
RBC_BASE::Find_arg_in_callee(VSA *vsa, CODEREP *key, RNA_NODE *rna, DNA_NODE *callee, CODEREP **ret)
{
  BOOL found = FALSE;
  *ret = NULL;
  if (key == NULL)
    return found;
  for (INT i = VAR_INIT_ID; i < rna->Arg_list()->size(); i++) {
    CODEREP *arg = rna->Get_arg(i);
    // if key is passed as parameter in the call
    if (arg != NULL) {
      if (arg->Kind() == CK_OP && arg->Opr() == OPR_CVT)
        arg = arg->Opnd(0);
      if (arg == key)
        found = TRUE;
    }
    // callee parameter counter part
    if (found) {
      *ret = callee->Get_param_cr(i);
      break;
    }
  }
  return found;
}


// =============================================================================
//
// RBC_BASE::Find_fsm_key_in_callee:
//
// =============================================================================
BOOL
RBC_BASE::Find_fsm_key_in_callee(VSA *vsa, FSM_OBJ_REP *fsm_obj_rep, CODEREP *key,
                                 RNA_NODE *rna, DNA_NODE *callee, CODEREP **ret)
{
  BOOL found = FALSE;
  *ret = NULL;
  if (key == NULL)
    return found;
  for (INT i = VAR_INIT_ID; i < rna->Arg_list()->size(); i++) {
    CODEREP *arg = rna->Get_arg(i);
    // if key is passed as parameter in the call
    hash_set<IDTYPE> visited;
    visited.clear();
    if (Match_fsm_key(vsa, fsm_obj_rep, key, arg, visited) != NULL)
      found = TRUE;
    // callee parameter counter part
    if (found) {
      *ret = callee->Get_param_cr(i);
      break;
    }
  }
  return found;
}


// =============================================================================
//
// RBC_BASE::Find_fsm_key_in_caller:
//
// =============================================================================
BOOL
RBC_BASE::Find_fsm_key_in_caller(VSA *vsa, FSM_OBJ_REP *fo, RNA_NODE *rna, DNA_NODE *caller, CODEREP **ret)
{
  BOOL found = FALSE;
  *ret = NULL;
  if (fo == NULL)
    return found;
  CODEREP *key = fo->Key();
  if (key == NULL)
    return found;
  // go through return value, looking for var of the same key
  for (INT i = PDV_INIT_ID; i < vsa->Dna()->Retv_list()->size(); i++) {
    PDV_NODE *pdv = (*vsa->Dna()->Retv_list())[i];
    if ((pdv->Kind() & BY_RETURNSTMT) == 0)
      continue;
    hash_set<IDTYPE> visited;
    visited.clear();
    if (Match_fsm_key(vsa, fo, key, pdv->Stmt()->Rhs(), visited) != NULL) {
      found = TRUE;
      break;
    }
  }
  if (found) {
    STMTREP *call_stmt = rna->Callstmt();
    *ret = caller->Comp_unit()->Find_return_value(call_stmt);
    if (ret != NULL) {
      STMTREP *rstmt = call_stmt->Next();
      if (rstmt != NULL && rstmt->Opr() == OPR_STID && rstmt->Rhs() == *ret) {
        *ret = rstmt->Lhs();
        rstmt = rstmt->Next();
        if (rstmt != NULL &&
            (rstmt->Opr() == OPR_STID || rstmt->Opr() == OPR_ISTORE) &&
            rstmt->Rhs() == *ret)
          *ret = rstmt->Lhs();
      }
    }
  }
  return found;
}


// =============================================================================
//
// RBC_BASE::Match_fsm_key
//
// =============================================================================
CODEREP*
RBC_BASE::Match_fsm_key(VSA *vsa, FSM_OBJ_REP *fsm_obj_rep, CODEREP *ori_key,
                        CODEREP *new_key, hash_set<IDTYPE> &visited)
{
  if (vsa == NULL || fsm_obj_rep == NULL || ori_key == NULL || new_key == NULL)
    return NULL;
  if (ori_key == new_key) {
    Is_Trace(Tracing(), (TFile, "RBC: same keys, matched\n"));
    return new_key;
  }
  if (ori_key->Kind() == CK_OP && (ori_key->Opr() == OPR_CVT ||
                                   ori_key->Opr() == OPR_CVTL)) {
    if (Match_fsm_key(vsa, fsm_obj_rep, ori_key->Opnd(0), new_key, visited) != NULL) {
      Is_Trace(Tracing(), (TFile, "RBC: CVT/CVTL opnd(cr%d) of ori_key(cr%d) matched\n",
                           ori_key->Opnd(0)->Coderep_id(), ori_key->Coderep_id()));
      return new_key;
    }
  }
  if (new_key->Kind() == CK_CONST) {
    Is_Trace(Tracing(), (TFile, "RBC: new_key(cr%d) is a const, mismatched\n",
                         new_key->Coderep_id()));
    return NULL;
  }
  FOR_ARRAY *for_array = vsa->Cr_2_for_array(new_key);
  if (for_array != NULL) {
    for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++) {
      FSM_OBJ_REP *new_key_rep = (*for_array)[fa_idx];
      if (new_key_rep == fsm_obj_rep) {
        Is_Trace(Tracing(), (TFile, "RBC: ori_key(cr%d) new_key(cr%d) same rep, matched\n",
                             ori_key->Coderep_id(), new_key->Coderep_id()));
        return new_key;
      }
    }
  }
  if (new_key->Kind() == CK_VAR) {
    if (new_key->Is_flag_set((CR_FLAG)CF_DEF_BY_PHI)) {
      PHI_NODE *phi = new_key->Defphi();
      if (visited.find(phi->Bb()->Id()) != visited.end()) {
        Is_Trace(Tracing(), (TFile, "RBC: already visited phi BB%d, ori_key(cr%d) new_key(cr%d), mismatched\n",
                             phi->Bb()->Id(), ori_key->Coderep_id(), new_key->Coderep_id()));
        return NULL;
      }
      visited.insert(phi->Bb()->Id());
      PHI_OPND_ITER phi_opnd_iter(phi);
      CODEREP *opnd;
      Is_Trace(Tracing(), (TFile, "RBC: try PHI opnds of new_key(cr%d)\n",
                           new_key->Coderep_id()));
      FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
        if (Match_fsm_key(vsa, fsm_obj_rep, ori_key, opnd, visited) != NULL) {
          Is_Trace(Tracing(), (TFile, "RBC: PHI opnd(cr%d) of new_key(cr%d) matched\n",
                               opnd == NULL ? -1 : opnd->Coderep_id(), new_key->Coderep_id()));
          return new_key;
        }
      }
    }
    else if (new_key->Is_flag_set((CR_FLAG)CF_DEF_BY_CHI)) {
      CHI_NODE *chi = new_key->Defchi();
      CODEREP *opnd = chi != NULL ? chi->OPND() : NULL;
      Is_Trace(Tracing(), (TFile, "RBC: try CHI opnd of new_key(cr%d)\n",
                           new_key->Coderep_id()));
      if (Match_fsm_key(vsa, fsm_obj_rep, ori_key, opnd, visited) != NULL) {
        Is_Trace(Tracing(), (TFile, "RBC: CHI opnd(cr%d) of new_key(cr%d) matched\n",
                             opnd == NULL ? -1 : opnd->Coderep_id(), new_key->Coderep_id()));
        return new_key;
      }
    }
    else {
      STMTREP *defstmt = new_key->Defstmt();
      if (defstmt != NULL && defstmt->Opr() == OPR_STID) {
        CODEREP *rhs = defstmt->Rhs();
        Is_Trace(Tracing(), (TFile, "RBC: try defstmt stmt%d rhs(cr%d) of new_key(cr%d)\n",
                             defstmt->Stmtrep_id(), rhs == NULL ? -1 : rhs->Coderep_id(),
                             new_key->Coderep_id()));
        if (Match_fsm_key(vsa, fsm_obj_rep, ori_key, rhs, visited) != NULL) {
          Is_Trace(Tracing(), (TFile, "RBC: rhs(cr%d) of defstmt stmt%d, matched\n",
                               rhs == NULL ? -1 : rhs->Coderep_id(), defstmt->Stmtrep_id()));
          return new_key;
        }
        else if (rhs != NULL && rhs->Kind() == CK_IVAR && ori_key->Kind() == CK_VAR) {
          STMTREP *ori_defstmt = ori_key->Defstmt();
          if (ori_defstmt != NULL && ori_defstmt->Opr() == OPR_STID) {
            CODEREP *ori_rhs = ori_defstmt->Rhs();
            if (ori_rhs != NULL && ori_rhs->Kind() == CK_IVAR) {
              CODEREP *base = ori_rhs->Ilod_base();
              UINT ifieldid = ori_rhs->I_field_id();
              if (base != NULL && rhs->Ilod_base() == base && rhs->I_field_id() == ifieldid) {
                Is_Trace(Tracing(),
                         (TFile,
                          "RBC: ori_key(cr%d) new_key(cr%d) from same Ilod_base(cr%d) & ifieldid(%d), matched\n",
                          ori_key->Coderep_id(), new_key->Coderep_id(), base->Coderep_id(), ifieldid));
                return new_key;
              }
            }
          }
        }
      }
    }
  }
  else if (new_key->Kind() == CK_OP && (new_key->Opr() == OPR_CVT ||
                                        new_key->Opr() == OPR_CVTL)) {
    if (Match_fsm_key(vsa, fsm_obj_rep, ori_key, new_key->Opnd(0), visited) != NULL) {
      Is_Trace(Tracing(), (TFile, "RBC: CVT/CVTL opnd(cr%d) of new_key(cr%d) matched\n",
                           new_key->Opnd(0)->Coderep_id(), new_key->Coderep_id()));
      return new_key;
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC: ori_key(cr%d) new_key(cr%d), mismatched\n",
                       ori_key->Coderep_id(), new_key->Coderep_id()));
  return NULL;
}


// =============================================================================
//
// RBC_BASE::Find_thread_init_key:
//
// =============================================================================
CODEREP*
RBC_BASE::Find_thread_init_key(BB_NODE *bb, CODEREP *cr, hash_set<IDTYPE> &visited_bb)
{
  if (cr == NULL)
    return NULL;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return NULL;
  visited_bb.insert(bb->Id());

  CODEREP *ret = NULL;
  STMTREP *stmt;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (stmt->Opr() != OPR_STID && stmt->Opr() != OPR_ISTORE)
      continue;
    if (stmt->Rhs() == cr)
      ret = stmt->Lhs();
    if (ret != NULL)
      return ret;
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    ret = Find_thread_init_key(succ_bb, cr, visited_bb);
    if (ret != NULL)
      break;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Find_thread_run_key:
//
// =============================================================================
CODEREP*
RBC_BASE::Find_thread_run_key(VSA *run_vsa, BB_NODE *bb,
                              VSA *init_vsa, CODEREP *cr, hash_set<IDTYPE> &visited_bb)
{
  if (cr == NULL)
    return NULL;
  if (visited_bb.find(bb->Id()) != visited_bb.end())
    return NULL;
  visited_bb.insert(bb->Id());

  CODEREP *ret = NULL;
  STMTREP *stmt;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    if (stmt->Opr() != OPR_STID && stmt->Opr() != OPR_ISTORE)
      continue;
    CODEREP *candidate = stmt->Rhs();
    ST *can_st = Get_cr_st(run_vsa, candidate);
    ST *cr_st = Get_cr_st(init_vsa, cr);
    if (can_st != NULL && ST_is_this_ptr(can_st)) {
      if (cr_st != NULL && ST_is_this_ptr(cr_st)) {
        if (candidate->Kind() == CK_IVAR) {
          if (candidate->I_field_id() == cr->I_field_id())
            ret = candidate;
        }
        else if (candidate->Kind() == CK_VAR) {
          if (candidate->Field_id() == cr->Field_id())
            ret = candidate;
        }
      }
    }
    if (ret != NULL)
      return ret;
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    ret = Find_thread_run_key(run_vsa, succ_bb, init_vsa, cr, visited_bb);
    if (ret != NULL)
      break;
  }
  return ret;
}


STMTREP*
RBC_BASE::Get_last_stmt(BB_NODE *bb)
{
  if (bb == NULL)
    return NULL;
  STMTREP *ret = bb->Last_stmtrep();
  if (ret == NULL) {
    BB_NODE *tmp1 = bb;
    BB_NODE *tmp2 = bb->Prev();
    while (tmp2 && CFG::Fall_through(tmp2, tmp1)) {
      ret = tmp2->Last_stmtrep();
      if (ret != NULL)
        break;
      tmp1 = tmp2;
      tmp2 = tmp2->Prev();
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Perform_fsm_check_bb: go through stmt in BB to
//     evaluate FSM state transition.
//
// =============================================================================
void
RBC_BASE::Perform_fsm_check_bb(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt, BB_NODE *bb,
                               FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h)
{
  if (srcpos_h != NULL && !fsm_ctx.Skip() && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip FSM(%s) due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         fsm_ctx.Fsm_name(), srcpos_h->Children_count(), fsm_ctx.Cur_dna()->Fname()));
    fsm_ctx.Set_skip();
  }
  if (fsm_ctx.Skip())
    return;
  if (fsm_ctx.Visited(bb))
    return;
  // dna of current frame, used to validate frame push/pop later
  DNA_NODE *dna = fsm_ctx.Cur_dna();
  FSM_OBJ_REP *new_for = fsm_obj_rep;
  BOOL start = FALSE;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *cur;
  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    if (!start) {
      if (stmt == cur) {
        start = TRUE;
      }
      else {
        continue;
      }
    }
    // go for state transition in statement
    new_for = Perform_fsm_check_stmt(new_for, cur, bb, fsm_ctx, srcpos_h);

    if (new_for->State() == new_for->Fsm()->Final_state()) {
      fsm_ctx.Reset_visited(bb);
      return;
    }
    else if (OPERATOR_is_call(cur->Opr())) {
      // try to switch to callee
      if (Perform_fsm_check_callee(new_for, cur, fsm_ctx, srcpos_h)) {
        Is_True(dna == fsm_ctx.Cur_dna(), ("RBC ERROR: FSM(%s) fsm_ctx is wrong in callee rewind, cur(%s), top(%s)\n",
                                           fsm_ctx.Fsm_name(), dna->Fname(), fsm_ctx.Cur_dna()->Fname()));
        fsm_ctx.Reset_visited(bb);
        return;
      }
    }
  }

  if (bb->Succ() == NULL) {
    // last bb, should go back to caller or
    // terminate if it is root entry
    if (Perform_fsm_check_caller(new_for, fsm_ctx, srcpos_h)) {
      Is_True(dna == fsm_ctx.Cur_dna(), ("RBC ERROR: FSM(%s) fsm_ctx is wrong in caller rewind, cur(%s), top(%s)\n",
                                         fsm_ctx.Fsm_name(), dna->Fname(), fsm_ctx.Cur_dna()->Fname()));
      fsm_ctx.Reset_visited(bb);
      return;
    }
    // can not switch to caller, should be root or end of analysis
    if (Match_trans_with_hook(new_for, fsm_ctx, srcpos_h)) {
      fsm_ctx.Reset_visited(bb);
      return;
    }
    // report default error if failed to transit to final state
    INT idx = 0;
    TRANSIT *default_action = fsm_ctx.Fsm()->Get_default_action(new_for->State(), &idx);
    if (default_action != NULL && default_action->Errcode()->size() != 0) {
      Is_Trace(Tracing(), (TFile, "RBC: apply default action for state(%d)\n",
                           default_action->State()));
      STMTREP *last_stmt = Get_last_stmt(bb);
      if (last_stmt != NULL) {
        srcpos_h->Append_data(last_stmt, fsm_ctx.Cur_dna(), default_action->Msg_id());
        Report_fsm_error(fsm_ctx.Cur_vsa(), &fsm_ctx, last_stmt, new_for, default_action, srcpos_h, FSM_ERR_KIND_DEFAULT);
      }
      new_for->Set_visited();
    }
    fsm_ctx.Reset_visited(bb);
    return;
  }

  FOR_ARRAY check_list;
  check_list.clear();
  check_list.push_back(new_for);
  // do the same for its successor nodes
  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  INT32 succ_len = bb->Succ()->Len();
  INT i = 0;
  SRCPOS_TREENODE *cur_node = NULL;
  STMTREP *last_stmt = bb->Last_stmtrep();
  if (succ_len > 1) {
    srcpos_h->Add_children(succ_len);
    cur_node = srcpos_h->Cur_node();
    // add other possible transit on 'if'
    if ((strcmp(new_for->Transit()->Action(), "if") == 0) &&
        (last_stmt != NULL) &&
        (new_for->Defstmt() == last_stmt) &&
        (last_stmt->Opr() == OPR_FALSEBR || last_stmt->Opr() == OPR_TRUEBR)) {
      FOR_ARRAY *if_list = fsm_ctx.Cur_vsa()->Sr_2_for_array(last_stmt);
      if (if_list != NULL) {
        for (INT fa_idx = 0; fa_idx < if_list->size(); fa_idx++) {
          FSM_OBJ_REP *tmp = (*if_list)[fa_idx];
          if (tmp == NULL || tmp == new_for)
            continue;
          if (new_for->Fsm() != tmp->Fsm())
            continue;
          if (new_for->Transit()->State() != tmp->Transit()->State())
            continue;
          if (strcmp(tmp->Transit()->Action(), "if") == 0) {
            check_list.push_back(tmp);
            break;
          }
        }
      }
    }
  }
  CODEREP *old_key = fsm_ctx.Cur_key();
  const char *old_key_str = srcpos_h->Orig_stname();
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    if (succ_len > 1) {
      srcpos_h->Set_cur_node(cur_node, i);
      // evaluate conditions on fsm_obj_rep of "if" transit
      if ((strcmp(new_for->Transit()->Action(), "if") == 0) &&
          (last_stmt != NULL) &&
          (new_for->Defstmt() == last_stmt) &&
          (last_stmt->Opr() == OPR_FALSEBR || last_stmt->Opr() == OPR_TRUEBR)) {
        TRANSIT *ts = NULL;
        for (INT check_idx = 0; check_idx < check_list.size(); check_idx++) {
          new_for = check_list[check_idx];
          ts = new_for->Transit();
          Is_Trace(Tracing(), (TFile, "RBC: try \"if\" transit on STMT%d: ", last_stmt->Stmtrep_id()));
          Is_Trace_cmd(Tracing(), (TFile, new_for->Print(TFile)));
          Is_Trace(Tracing(), (TFile, "\n"));
          BOOL cond = TRUE;
          {
            DNA_NODE *fsm_dna = fsm_ctx.Fsm()->Dna();
            RBC_CONTEXT rbc_ctx(dna, fsm_dna, last_stmt, fsm_ctx.Cur_vsa()->Loc_pool());
            CONTEXT_SWITCH context(fsm_dna);
            Rbc_init();
            UINT64 retv = Eval__exp(rbc_ctx, ts->Cond());
            if (Rbc_result_ignore())
              continue;
            if (retv == 0)
              cond = FALSE;
          }
          if (last_stmt->Opr() == OPR_FALSEBR) {
            if (cond != TRUE) {
              if (last_stmt->Label_number() != succ_bb->Labnam())
                continue;
            }
            else {
              if (last_stmt->Label_number() == succ_bb->Labnam())
                continue;
            }
          }
          else {
            if (cond != TRUE) {
              if (last_stmt->Label_number() == succ_bb->Labnam())
                continue;
            }
            else {
              if (last_stmt->Label_number() != succ_bb->Labnam())
                continue;
            }
          }
          break;
        }
        new_for->Set_state(ts->Nstate());
        srcpos_h->Append_data(last_stmt, dna, ts->Msg_id(), FALSE);
        Is_Trace(Tracing(), (TFile, "\nRBC: FSM(\"%s\"): state(%d) => state(%d) by Action(\"if\")\n",
                             fsm_ctx.Fsm_name(), ts->State(), ts->Nstate()));
      }
      if (last_stmt != NULL && (last_stmt->Opr() == OPR_TRUEBR ||
                                last_stmt->Opr() == OPR_FALSEBR)) {
        PATH_INFO then_info = PATHINFO_THEN_TAKEN;
        PATH_INFO cond_info = PATHINFO_COND_TRUE;
        if (last_stmt->Opr() == OPR_FALSEBR) {
          if (last_stmt->Label_number() == succ_bb->Labnam()) {
            then_info = PATHINFO_ELSE_TAKEN;
            cond_info = PATHINFO_COND_FALSE;
          }
        }
        else {
          if (last_stmt->Label_number() != succ_bb->Labnam()) {
            then_info = PATHINFO_ELSE_TAKEN;
            cond_info = PATHINFO_COND_FALSE;
          }
        }
        srcpos_h->Append_data(last_stmt, dna, cond_info, FALSE);
        Is_Trace(Tracing(), (TFile, "RBC: going \"%s\" branch\n",
                             cond_info == PATHINFO_COND_TRUE ? "TRUE" : "FALSE"));
        srcpos_h->Append_data(succ_bb, dna, then_info);
        Is_Trace(Tracing(), (TFile, "RBC: going \"%s\" block\n",
                             then_info == PATHINFO_THEN_TAKEN ? "THEN" : "ELSE"));
      }
      else {
        srcpos_h->Append_data(succ_bb, dna, PATHINFO_BRANCH);
      }
      i++;
    }
    Perform_fsm_check_bb(new_for, succ_bb->First_stmtrep(), succ_bb, fsm_ctx, srcpos_h);
    fsm_ctx.Set_cur_key(old_key);
    srcpos_h->Set_orig_stname(old_key_str);
  }
  fsm_ctx.Reset_visited(bb);
}


// =============================================================================
//
// RBC_BASE::Perform_fsm_check_stmt: perform FSM state transition on stmt
//     that is binded to an FSM.
//
// =============================================================================
FSM_OBJ_REP*
RBC_BASE::Perform_fsm_check_stmt(FSM_OBJ_REP *fsm_obj_rep, STMTREP *stmt, BB_NODE *bb,
                                 FSM_TRAV_CONTEXT &fsm_ctx, SRCPOS_HANDLE *srcpos_h)
{
  if (fsm_obj_rep == NULL)
    return NULL;
  Is_True(stmt != NULL, ("RBC: null stmt passed to Perform_fsm_check_stmt.\n"));
  if (stmt == NULL)
    return fsm_obj_rep;
  Is_Trace(Tracing(), (TFile, "RBC: -------- visiting stmt(%d)\n", stmt->Stmtrep_id()));
  VSA *vsa = fsm_ctx.Cur_vsa();
  OPERATOR opr = stmt->Opr();
  FOR_ARRAY *for_array = vsa->Sr_2_for_array(stmt);
  // no FSM transits binded, transit key & continue
  if (for_array == NULL)
    return fsm_obj_rep;
  FSM_OBJ_REP *ret = NULL;
  BOOL found = FALSE;
  FSM *fsm = fsm_ctx.Fsm();
  STRING fsm_name = fsm_ctx.Fsm_name();
  TRANSIT *ts = NULL;
  IDTYPE cur_state = fsm_obj_rep->State();
  CODEREP *ori_key = fsm_ctx.Cur_key();
  CODEREP *new_key = NULL;
  STRING action = NULL;
  // iterate FSM transits binded and find match transit
  // there should be only one valid transit for given FSM current state
  for (INT fa_idx = 0; fa_idx < for_array->size(); fa_idx++) {
    ret = (*for_array)[fa_idx];
    if (ret == NULL)
      continue;
    if (fsm_obj_rep == ret) {
      Is_Trace(Tracing(), (TFile, "RBC: same fsm_obj_rep: "));
      Is_Trace_cmd(Tracing(), fsm_obj_rep->Print(TFile));
      Is_Trace(Tracing(), (TFile, " == ret: "));
      Is_Trace_cmd(Tracing(), ret->Print(TFile));
      Is_Trace(Tracing(), (TFile, ", skipped\n"));
      continue;
    }
    // different FSM
    if (ret->Fsm() != fsm)
      continue;
    ts = ret->Transit();
    if (ts == NULL)
      continue;
    action = ts->Action();
    if (action == NULL || ts->Is_default())
      continue;
    if (ret->Defstmt() == fsm_obj_rep->Defstmt()) {
      Is_Trace(Tracing(), (TFile, "RBC: same statement fsm_obj_rep: "));
      Is_Trace_cmd(Tracing(), fsm_obj_rep->Print(TFile));
      Is_Trace(Tracing(), (TFile, " ret: "));
      Is_Trace_cmd(Tracing(), ret->Print(TFile));
      Is_Trace(Tracing(), (TFile, ", skipped\n"));
      continue;
    }
    Is_Trace(Tracing(), (TFile, "RBC: trying to transit "));
    Is_Trace_cmd(Tracing(), fsm_obj_rep->Print(TFile));
    Is_Trace(Tracing(), (TFile, " => "));
    Is_Trace_cmd(Tracing(), ret->Print(TFile));
    Is_Trace(Tracing(), (TFile, "\n"));
    // not a transit that starts from current state
    if (ts->State() != cur_state) {
      Is_Trace(Tracing(), (TFile, "RBC: cur_state(%d), ret(%d => %d), skipped\n",
                           cur_state, ts->State(), ts->Nstate()));
      continue;
    }
    // valid transit on found 'ret', need to evaluate condition and match keys here after.
    // for 'if', try to match key with one of its operand
    if (opr == OPR_FALSEBR || opr == OPR_TRUEBR) {
      CODEREP *cmp = stmt->Rhs();
      if (cmp != NULL && cmp->Kind() == CK_OP && OPERATOR_is_compare(cmp->Opr())) {
        CODEREP *lhs = cmp->Opnd(0);
        CODEREP *rhs = cmp->Opnd(1);
        Is_Trace(Tracing(), (TFile, "RBC: try lhs(cr%d) of compare operand\n",
                             lhs == NULL ? -1 : lhs->Coderep_id()));
        hash_set<IDTYPE> visited;
        visited.clear();
        new_key = Match_fsm_key(vsa, fsm_obj_rep, ori_key, lhs, visited);
        if (new_key == NULL) {
          Is_Trace(Tracing(), (TFile, "RBC: try rhs(cr%d) of compare operand\n",
                               rhs == NULL ? -1 : rhs->Coderep_id()));
          visited.clear();
          new_key = Match_fsm_key(vsa, fsm_obj_rep, ori_key, rhs, visited);
        }
      }
      if (ori_key != NULL && new_key == NULL) {
        Is_Trace(Tracing(), (TFile, "RBC: no key matches, skipped\n"));
        continue;
      }
    }
    else {
      // try to match key for ret & fsm_obj_rep
      new_key = ret->Key();
      Is_Trace(Tracing(), (TFile, "RBC: trying to match key fsm_obj_rep("));
      if (ori_key == NULL) {
        Is_Trace(Tracing(), (TFile, "null)"));
      }
      else {
        Is_Trace(Tracing(), (TFile, "cr%d)", ori_key->Coderep_id()));
      }
      Is_Trace(Tracing(), (TFile, " with ret("));
      if (new_key == NULL) {
        Is_Trace(Tracing(), (TFile, "null)"));
      }
      else {
        Is_Trace(Tracing(), (TFile, "cr%d)", new_key->Coderep_id()));
      }
      Is_Trace(Tracing(), (TFile, "\n"));
      if (ori_key != NULL && new_key != NULL) {
        hash_set<IDTYPE> visited;
        visited.clear();
        new_key = Match_fsm_key(vsa, fsm_obj_rep, ori_key, new_key, visited);
        if (new_key == NULL) {
          Is_Trace(Tracing(), (TFile, "RBC: key mismatches, skipped\n"));
          continue;
        }
      }
    }
    // key matches.
    // for 'return', there's no condition evaluation
    if (opr == OPR_RETURN || opr == OPR_RETURN_VAL) {
      found = TRUE;
      break;
    }
    else if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
      // If there're two transits on a same 'if' statement,
      // like #1 function return value is tested properly &
      // #2 return value indicates function completes successfuly,
      // e.g. for fd = open(*), fd > 0 is not a proper test but fd != -1 is.
      // Transit like #1 is noted as action "if_test" and done here,
      // Transit like #2 is left later to match branch targets.
      // Note that transits change no states should not be done here to avoid infinite loop.
      if (vsa->Is_action_match(stmt, action, "if:test") != TS_NOT_MATCH) {
        DNA_NODE *fsm_dna = fsm->Dna();
        RBC_CONTEXT rbc_ctx(vsa->Dna(), fsm_dna, stmt, vsa->Loc_pool());
        CONTEXT_SWITCH context(fsm_dna);
        Rbc_init();
        UINT64 retv = Eval__exp(rbc_ctx, ts->Cond());
        if (retv == 0 || Rbc_result_ignore()) {
          Is_Trace(Tracing(), (TFile, "RBC: Action(%s) cond mismatches.\n", action));
          continue;
        }
        // Do the transit and find next transit on 'if' statement
        if (new_key != NULL) {
          fsm_ctx.Set_cur_key(new_key);
          const char *new_key_str = Generate_fsm_key_str(new_key, stmt, action, vsa->Dna(), srcpos_h);
          Is_Trace(Tracing(), (TFile, "RBC: set new key for transit: (cr%d:%s) => (cr%d:%s)\n",
                               ori_key == NULL ? -1 : ori_key->Coderep_id(), srcpos_h->Orig_stname(),
                               new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
          srcpos_h->Set_orig_stname(new_key_str);
        }
        ret->Set_visited();
        Is_Trace(Tracing(), (TFile, "RBC: Set visited on: "));
        Is_Trace_cmd(Tracing(), ret->Print(TFile));
        Is_Trace(Tracing(), (TFile, "\n"));
        ret->Set_state(ts->Nstate());
        srcpos_h->Append_data(stmt, vsa->Dna(), ts->Msg_id());
        Is_Trace(Tracing(), (TFile, "\nRBC: FSM(\"%s\"): state(%d) => state(%d) by Action(\"%s\")\n",
                             fsm_name, cur_state, ret->State(), action));
        if (ts->Errcode()->size() != 0) {
          // report error for transition with error code
          Report_fsm_error(vsa, &fsm_ctx, stmt, ret, ts, srcpos_h, FSM_ERR_KIND_TRANSIT);
        }
        // Done transit like #1, look for transit like #2,
        cur_state = ret->State();
        for (INT nfa_idx = 0; nfa_idx < for_array->size(); nfa_idx++) {
          FSM_OBJ_REP *next_for = (*for_array)[nfa_idx];
          if (next_for == NULL || next_for->Fsm() != fsm)
            continue;
          TRANSIT *next_ts = next_for->Transit();
          if (next_ts == NULL || next_ts->Is_default() || next_ts->State() != cur_state)
            continue;
          STRING next_act = next_ts->Action();
          if (next_act == NULL)
            continue;
          Is_Trace(Tracing(), (TFile, "RBC: found cur_state(%d), ret(%d => %d), next(%d => %d) on \"if\"\n",
                               cur_state, ts->State(), ts->Nstate(), next_ts->State(), next_ts->Nstate()));
          ret = next_for;
          break;
        } // end for_array
      }
      found = TRUE;
      break;
    }
    else if (OPERATOR_is_call(opr)) {
      RNA_NODE *rna = vsa->Dna()->Get_callsite_rna(stmt);
      if (rna == NULL)
        continue;
      for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
           iter != rna->Callee_list().end(); iter++) {
        DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
        if (callee == NULL)
          continue;
        if (vsa->Is_action_match(stmt, action, callee->Fname()) == TS_NOT_MATCH)
          continue;
        RBC_CONTEXT rbc_ctx(vsa->Dna(), callee, fsm->Dna(), rna, vsa->Loc_pool());
        TAG_INFO *tag_info = Find_dna_tag_info(action, callee);
        if (tag_info) {
          rbc_ctx.Set_tag_info(tag_info);
        }
        CONTEXT_SWITCH context(fsm->Dna());
        Rbc_init();
        UINT64 retv = Eval__exp(rbc_ctx, ts->Cond());
        if (retv == 0 || Rbc_result_ignore()) {
          Is_Trace(Tracing(), (TFile, "RBC: Action(%s) cond mismatches.\n", action));
          continue;
        }
        found = TRUE;
        break;
      }
      if (found)
        break;
    }
  }
  if (found) {
    if (new_key != NULL) {
      fsm_ctx.Set_cur_key(new_key);
      const char *new_key_str = Generate_fsm_key_str(new_key, stmt, action, vsa->Dna(), srcpos_h);
      Is_Trace(Tracing(), (TFile, "RBC: set new key for transit: (cr%d:%s) => (cr%d:%s)\n",
                           ori_key == NULL ? -1 : ori_key->Coderep_id(), srcpos_h->Orig_stname(),
                           new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
      srcpos_h->Set_orig_stname(new_key_str);
    }
    fsm_obj_rep->Set_visited();
    Is_Trace(Tracing(), (TFile, "RBC: Set visited on: "));
    Is_Trace_cmd(Tracing(), fsm_obj_rep->Print(TFile));
    Is_Trace(Tracing(), (TFile, "\n"));
    if (opr == OPR_TRUEBR || opr == OPR_FALSEBR) {
      // do the transit & report error in branch BB
      return ret;
    }
    ret->Set_state(ts->Nstate());
    srcpos_h->Append_data(stmt, vsa->Dna(), ts->Msg_id());
    Is_Trace(Tracing(), (TFile, "\nRBC: FSM(\"%s\"): state(%d) => state(%d) by Action(\"%s\")\n",
                         fsm_name, cur_state, ret->State(), action));
    if (ts->Errcode()->size() != 0) {
      // report error for transition with error code
      Report_fsm_error(vsa, &fsm_ctx, stmt, ret, ts, srcpos_h, FSM_ERR_KIND_TRANSIT);
    }
  }
  else {
    // skip over
    ret = fsm_obj_rep;
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Match_trans_with_hook
// @parm vsa_ctx: context of fsm_obj_rep
// @parm fsm_obj_rep: fsm that need to match transition in hook function
// Instance 1: Thread(Runnable obj)
// foo1() { PrintStream out = new PrintStream(XXX); regShutdown(out); }
// void regShutdown(out) {
//   var1 = new RunnableImpl(out);
//   var2 = new Thread(var1);
//   addShutdownHook(var2);
// }
// class RunnableImpl implement Runnable {
//    void init(out) {xxx}
//    void run()  { out.close(); }
// }
// fsm transition:
//      new PrintStream              out.close()
// init   ------>         created    ---------->  end
//
// =============================================================================
BOOL
RBC_BASE::Match_trans_with_hook(FSM_OBJ_REP *fsm_obj_rep, FSM_TRAV_CONTEXT &fsm_ctx,
                                SRCPOS_HANDLE *srcpos_h)
{
  if (srcpos_h != NULL && !fsm_ctx.Skip() && srcpos_h->Reach_check_limit()) {
    Is_Trace(Tracing(), (TFile, "RBC: skip FSM(%s) due to SRCPOS_HANDLE children(%d) for func(%s)\n",
                         fsm_ctx.Fsm_name(), srcpos_h->Children_count(), fsm_ctx.Cur_dna()->Fname()));
    fsm_ctx.Set_skip();
  }
  if (fsm_ctx.Skip())
    return FALSE;
  BOOL found = FALSE;
  if (!PU_java_lang(Get_Current_PU()) || !fsm_obj_rep) {
    return found;
  }
  // already in hook? stop
  FSM_TRAV_FRAME *frame = fsm_ctx.Top_frame(TD_NONE);
  if (frame != NULL && frame->Rna() == NULL) {
    return found;
  }
  Is_Trace(Tracing(), (TFile, ("RBC: try Match_trans_with_hook:\n")));

  FSM_OBJ_REP *new_for = fsm_obj_rep;
  VSA *vsa_ctx = fsm_ctx.Cur_vsa();
  // There's no guarantee on the execution order of shutdown hook in Java,
  // We are doomed to mess up here if there're trasitions between shutdown hooks
  INT idx = 0;
  INT parent_idx = srcpos_h->Cur_idx();
  srcpos_h->Add_children(vsa_ctx->Ipsa()->Shutdown_set().size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  DNA_IDX_SET::iterator hook_iter = vsa_ctx->Ipsa()->Shutdown_set().begin();
  DNA_IDX_SET::iterator hook_iter_end = vsa_ctx->Ipsa()->Shutdown_set().end();
  for (; hook_iter != hook_iter_end; hook_iter++) {
    DNA_NODE *hook_caller = vsa_ctx->Ipsa()->Get_dna(*hook_iter);
    STMTREP *first_stmt = hook_caller->Comp_unit()->Vsa()->Get_entry_chi_stmt();
    if(first_stmt == NULL) {
      Is_Trace(Tracing(), (TFile, "RBC: Skip hook caller %s, first stmt is NULL\n",
                           hook_caller->Fname()));
      continue;
    }
    srcpos_h->Set_cur_node(cur_node, idx);
    srcpos_h->Append_data(first_stmt, hook_caller, PATHINFO_DNA_CALLSITE);
    idx++;
    Is_Trace(Tracing(), (TFile, "RBC: Check hook caller %s\n", hook_caller->Fname()));
    CONTEXT_SWITCH hook(hook_caller);
    VSA *hook_vsa = hook_caller->Comp_unit()->Vsa();
    INT nidx = 0;
    srcpos_h->Add_children(hook_caller->Call_list()->size());
    SRCPOS_TREENODE *ncur_node = srcpos_h->Cur_node();
    for (int i = VAR_INIT_ID; i < hook_caller->Call_list()->size(); i++) {
      BOOL found_once = FALSE;
      RNA_NODE *rna = (*hook_caller->Call_list())[i];
      IDTYPE uniq_callee = rna->Uniq_callee();
      DNA_NODE *hook_dna = (uniq_callee == INVALID_RNA_PU_IDX) ?
                           NULL : vsa_ctx->Ipsa()->Get_dna(uniq_callee);
      if(hook_dna &&
         !strcmp(hook_dna->Fname(), "_ZN4java4lang7Runtime15addShutdownHookEJvPNS0_6ThreadE")) {
        STMTREP *call_stmt = rna->Callstmt();
        Is_True_Rbc(call_stmt && call_stmt->Rhs()->Kid_count() == 3, ("RBC:invalid hook function\n"));
        CODEREP *thread = call_stmt->Rhs()->Opnd(1);
        DNA_NODE *init_dna = NULL;
        STMTREP *init_stmt = Get_init_stmt(hook_vsa, call_stmt, thread, &init_dna);
        if(init_dna && init_stmt) {
          CONTEXT_SWITCH init_ctx(init_dna);
          Is_Trace(Tracing(),
                   (TFile,
                    "RBC: Find Thread construct stmt in dna:%s\n",
                    init_dna->Fname()));
          Is_Trace_cmd(Tracing(), init_dna->Comp_unit()->Vsa()->Print_sr(init_stmt, TFile));
          if(init_stmt->Rhs()->Kid_count() < 2) {
            Is_Trace(Tracing(), (TFile, "RBC: skip Thread() ctor, not supported now\n"));
            continue;
          }
          CODEREP *th_obj = init_stmt->Rhs()->Opnd(1);
          TY_IDX th_obj_ty = th_obj->object_ty();
          char *ty_name = (th_obj_ty == TY_IDX_ZERO) ||
                          (TY_kind(th_obj_ty) != KIND_POINTER) ?
                          NULL : TY_name(TY_pointed(th_obj_ty));
          if(ty_name == NULL ||
             (ty_name &&
              !vsa_ctx->Ipsa()->Glob_cha()->Is_base_class("java.lang.Runnable", ty_name))) {
            Is_Trace(Tracing(), (TFile, "RBC: Skip Thread ctor, not supported now\n"));
            continue;
          }
          DNA_NODE *run_init_dna = NULL;
          STMTREP *run_init_sr = Get_init_stmt(init_dna->Comp_unit()->Vsa(),
                                               init_stmt,
                                               th_obj,
                                               &run_init_dna);
          if(run_init_dna && run_init_sr &&
             OPERATOR_is_call(run_init_sr->Opr())) {
            CONTEXT_SWITCH obj_ctx(run_init_dna);
            Is_Trace(Tracing(),
                     (TFile,
                      "RBC: Find Runnable impl construct stmt in dna:%s\n",
                      run_init_dna->Fname()));

            DNA_NODE *runnable = Get_runnable_dna(vsa_ctx->Ipsa(), run_init_sr);
            Is_Trace(Tracing(),
                     (TFile, "RBC: Found Runnable.run dna %s\n",
                      runnable ? runnable->Fname(): "NULL\n"));

            VSA *runnable_vsa = runnable ? runnable->Comp_unit()->Vsa() : NULL;
            if(runnable_vsa && runnable_vsa->Find(fsm_ctx.Fsm_name()) != NULL) {
              // perform match on run method
              Is_Trace(Tracing(),
                       (TFile,
                        "RBC: Perfom fsm check on Runnable.run method\n"));
              // try to switch key here
              // runnable constructor parameter
              CODEREP *rcp = run_init_sr->Rhs()->Opnd(1)->Find_actual_arg();
              CODEREP *old_key = fsm_ctx.Cur_key();
              CODEREP *new_key = NULL;
              BOOL found_new_key = FALSE;
              const char *old_key_str = srcpos_h->Orig_stname();
              const char *new_key_str = Generate_fsm_key_str(new_key, NULL, NULL, runnable, srcpos_h);
              // transfer key to runnable init this.field
              // void <init>(java.io.PrintStream)
              // var1 := @parameter0: java.io.PrintStream;
              // this.<io.xc5.cert.j_fio14_4$1: java.io.PrintStream val$out> = var1;
              RNA_NODE *thread_init_rna = run_init_dna->Get_callsite_rna(run_init_sr);
              if (thread_init_rna != NULL) {
                DNA_NODE *thread_init_callee = hook_vsa->Ipsa()->Get_dna(thread_init_rna->Uniq_callee());
                if (thread_init_callee != NULL) {
                  // var1
                  found_new_key = Find_fsm_key_in_callee(run_init_dna->Comp_unit()->Vsa(), fsm_obj_rep,
                                                         rcp, thread_init_rna, thread_init_callee, &new_key);
                  // this.<blabla>
                  hash_set<IDTYPE> visited_bb;
                  new_key = Find_thread_init_key(thread_init_callee->Comp_unit()->Cfg()->Entry_bb(), new_key, visited_bb);
                  // switch key in run()
                  visited_bb.clear();
                  new_key = Find_thread_run_key(runnable_vsa, runnable_vsa->Cfg()->Entry_bb(),
                                                thread_init_callee->Comp_unit()->Vsa(), new_key, visited_bb);
                }
              }
              if (old_key == NULL || new_key != NULL) {
                // NULL key implies FSM uses no key, should step into it anyway
                // or if key is passed to callee, should step into it
                found_once = TRUE;
              }
              if (found_once) {
                found = TRUE;
                // got a key switching
                if (found_new_key) {
                  // set new key
                  fsm_ctx.Set_cur_key(new_key);
                  srcpos_h->Set_orig_stname(new_key_str);
                  Is_Trace(Tracing(), (TFile, "RBC: set new key for hooks: (cr%d:%s) => (cr%d:%s)\n",
                                       old_key == NULL ? -1 : old_key->Coderep_id(), old_key_str,
                                       new_key == NULL ? -1 : new_key->Coderep_id(), new_key_str));
                }
                STMTREP *stmt = runnable_vsa->Get_entry_chi_stmt();
                if(stmt == NULL) {
                  Is_Trace(Tracing(),
                           (TFile, "RBC: Skip check runnable dna %s, no entry chi stmt found\n",
                            runnable_vsa->Dna()->Fname()));
                  continue;
                }
                BB_NODE *entry_bb = stmt->Bb();
                srcpos_h->Set_cur_node(ncur_node, nidx);
                srcpos_h->Append_data(call_stmt, hook_caller, PATHINFO_DNA_CALLSITE);
                nidx++;
                // we will finish in shutdowm hook, so there's no need to come back to root entry
                fsm_ctx.Push_frame(NULL, NULL, NULL, NULL, NULL, runnable_vsa->Comp_unit(), new_for->State(), TD_NONE);
                Perform_fsm_check_bb(fsm_obj_rep, stmt, entry_bb, fsm_ctx, srcpos_h);
                fsm_ctx.Pop_frame(TD_NONE);
                // switch key back
                fsm_ctx.Set_cur_key(old_key);
                srcpos_h->Set_orig_stname(old_key_str);
              }
            }
          } else {
            Is_Trace(Tracing(), (TFile, "RBC HOOK: Unable to find hook Runnable construtor\n"));
          } // end if(run_init_dna && run_init_sr...)
        } else {
          Is_Trace(Tracing(), (TFile, "RBC HOOK: Unable to find hook Thread construtor\n"));
        } // end if(init_dna && init_stmt)
      } // end is shutdownhook
    } // end for all call_list
  }
  srcpos_h->Reset_cur_node(cur_node, parent_idx);
  return found;
}


// =============================================================================
//
// Get_init_stmt: find cr's constructor stmt and construct dna
//
// =============================================================================
STMTREP*
RBC_BASE::Get_init_stmt(VSA *vsa, STMTREP *stmt, CODEREP *cr, DNA_NODE** def_dna)
{
  VAR_DEF_HELPER helper(cr, stmt, vsa->Comp_unit());
  CHECK_OBJ check_obj(cr, stmt);
  vsa->Var_def_trav_helper(&helper, check_obj);
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  if (def_info_vec.size() > 0) {
    STMTREP *ret_stmt = check_obj.Stmtrep();
    *def_dna = def_info_vec[0]->Dna();
    CONTEXT_SWITCH def_ctx(*def_dna);
    STMTREP *ctor_stmt = Get_ctor_by_alloc(*def_dna, ret_stmt);
    return ctor_stmt;
  }
  return NULL;
}

// =============================================================================
//
// Get_ctor_by_alloc: get the construtor stmt from memory alloc stmt
//
// =============================================================================
STMTREP*
RBC_BASE::Get_ctor_by_alloc(DNA_NODE *dna, STMTREP *alloc_stmt)
{
  if (alloc_stmt && alloc_stmt->Opr() == OPR_INTRINSIC_CALL &&
      alloc_stmt->Rhs()->Intrinsic() == INTRN_ALLOC_OBJ) {
    CODEREP *ret_cr = dna->Comp_unit()->Find_return_value(alloc_stmt);
    if (ret_cr != NULL) {
      // search all call stmt, find the connstructor of ret_cr
      for (int i = VAR_INIT_ID; i < dna->Call_list()->size(); i++) {
        RNA_NODE *rna = (*dna->Call_list())[i];
        STMTREP *call_stmt = rna->Callstmt();
        if(call_stmt->Opr() == OPR_CALL &&
          OPERATOR_has_sym(call_stmt->Opr()) &&
          PU_is_constructor(Pu_Table[ST_pu(call_stmt->St())])) {
          CODEREP *this_ptr = call_stmt->Rhs()->Opnd(0)->Ilod_base();
          STMTREP *defstmt = this_ptr->Defstmt();
          if(defstmt->Opr() == OPR_STID &&
            defstmt->Rhs() == ret_cr) {
            return call_stmt;
          }
        }
      }
    }
  } else {
    Is_Trace(Tracing(), (TFile, "RBC Get_ctor_by_alloc: can't get constructor, alloc stmt : %d\n",
                    alloc_stmt ? alloc_stmt->Stmtrep_id() : -1));
  }
  return NULL;
}

// =============================================================================
//
// Get_runnable_dna: find Runnable.run() dna through Runnable.init() stmt
//
// =============================================================================
DNA_NODE*
RBC_BASE::Get_runnable_dna(IPSA* ipsa, STMTREP *init_stmt)
{
  Is_True_Rbc(init_stmt->Opr() == OPR_CALL &&
              OPERATOR_has_sym(init_stmt->Opr()) &&
              PU_is_constructor(Pu_Table[ST_pu(init_stmt->St())]),
              ("RBC: not thread init call stmt"));
  char *init_fun_name = ST_name(init_stmt->St());
  STRING_BUFFER buf(strlen(init_fun_name) + 1);
  const char *class_name = CLASS_HIERARCHY::Extract_class_name(init_fun_name, &buf);
  if(class_name == NULL) {
    Is_Trace(Tracing(),
             (TFile,
              "RBC HOOK: Unable to get run method class name from fun %s\n",
              init_fun_name));
    return NULL;
  }

  VIRFUNC_INFO *fun_info = ipsa->Glob_cha()->Get_meth_by_sig(class_name, "run()V");
  if(fun_info) {
    Is_Trace(Tracing(), (TFile, "RBC HOOK: Get run method fun info %s\n",
                         ST_name(fun_info->File_idx(), fun_info->Fun_st())));
    ST *fun_st = St_ptr(fun_info->File_idx(), fun_info->Fun_st());
    DNA_NODE *dna = ipsa->Get_dna(fun_info->File_idx(), fun_st);
    return dna;
  }

  Is_Trace(Tracing(), (TFile, "RBC HOOK: Unable to get run method class %s\n",
                       class_name));
  return NULL;
}


// =============================================================================
//
// Generate_fsm_error_var: string for var_name field for error report
//
// =============================================================================
STRING
RBC_BASE::Generate_fsm_error_var(FSM *fsm, TRANSIT *ts, IDTYPE state, IDTYPE nstate)
{
  STRING cur_state_str = (*fsm->State_vec())[state];
  STRING match_state_str = (*fsm->State_vec())[nstate];
  int len = strlen(cur_state_str) + strlen(match_state_str) + 5;
  STRING tmp = (STRING)malloc(len);
  strcpy(tmp, cur_state_str);
  if (ts->State() == state && ts->Nstate() == nstate)
    strcat(tmp, " => ");
  else
    strcat(tmp, " != ");
  strcat(tmp, match_state_str);
  tmp[len-1] = '\0';
  return tmp;
}

// =============================================================================
//
// Generate_fsm_key_str: string used in srcpos_h as key for an FSM
//
// =============================================================================
const char*
RBC_BASE::Generate_fsm_key_str(CODEREP *key, STMTREP *stmt, STRING act,
                               DNA_NODE *dna, SRCPOS_HANDLE *srcpos_h)
{
  if (key == NULL)
    return NULL;
  CONTEXT_SWITCH ctx(dna);
  const char *ret = srcpos_h->Find_orig_stname(key, stmt, dna, TRUE);
  if (ret == NULL || Vsa_check_sym_ignore(ret))
    ret = act;
  return ret;
}

UINT64 RBC_BASE::Eval__set_func_tag(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // TO BE IMPLEMENTED
  return 1;
}

UINT64 RBC_BASE::Eval__set_ty_is_mutex(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // TO BE IMPLEMENTED
  return 1;
}

UINT64 RBC_BASE::Eval__set_ty_is_atomic(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // TO BE IMPLEMENTED
  return 1;

}

UINT64 RBC_BASE::Eval__set_ty_is_thread(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // TO BE IMPLEMENTED
  return 1;

}
// =============================================================================
//
// RBC_BASE::Eval__exec_eval
//
// =============================================================================
UINT64
RBC_BASE::Eval__exec_eval(RBC_CONTEXT &rbc_ctx, STMTREP *stmt)
{
  // Exec_eval(UINT32 opr, UINT64 arg1, UINT64 arg2, UINT64 arg3);
  // Exec_eval(UINT32 opr, OBJECT arg1, OBJECT arg2, OBJECT arg3);
  RBC_EVAL_SKIP();
  DNA_NODE *dna = rbc_ctx.Caller();
  UINT32 parm_ofst = dna->Rbc_parm_offset();
  CODEREP *arg0 = stmt->Rhs()->Find_nth_arg(0 + parm_ofst);
  UINT32 opr = (UINT32)Eval__exp(rbc_ctx, arg0);
  BUILTIN_FUNC func = Builtin_func_new(opr);
  UINT64 ret = 0;
  if (func != NULL) {
    Is_Trace(Tracing(), (TFile, "RBC: calling %d\n", opr));
    BOOL saved_flag = Get_adjust_ofst();
    Set_adjust_ofst(TRUE);
    // Is_Trace(Tracing(), (TFile, "  Param ofst adjust: TRUE\n"));
    ret = (this->*func)(rbc_ctx, stmt);
    Set_adjust_ofst(saved_flag);
    // Is_Trace(Tracing(), (TFile, "  Param ofst reverted: %s\n", saved_flag ? "TRUE" : "FALSE"));
  }
  else {
    switch (opr) {
    case RBC_ENGINE::EXEC_KIND_CONST: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      ret = Eval__exp(rbc_ctx, arg1);
      break;
    }
    case RBC_ENGINE::EXEC_KIND_CMP_GT:
    case RBC_ENGINE::EXEC_KIND_CMP_GE:
    case RBC_ENGINE::EXEC_KIND_CMP_LT:
    case RBC_ENGINE::EXEC_KIND_CMP_LE:
    case RBC_ENGINE::EXEC_KIND_CMP_EQ:
    case RBC_ENGINE::EXEC_KIND_CMP_NE:
    case RBC_ENGINE::EXEC_KIND_ARITH_ADD:
    case RBC_ENGINE::EXEC_KIND_ARITH_SUB:
    case RBC_ENGINE::EXEC_KIND_ARITH_MPY:
    case RBC_ENGINE::EXEC_KIND_ARITH_DIV: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      UINT64 lhs_ui64val = Eval__exp(rbc_ctx, arg1);
      CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + parm_ofst);
      UINT64 rhs_ui64val = Eval__exp(rbc_ctx, arg2);
      if (opr == RBC_ENGINE::EXEC_KIND_CMP_GT)
        ret = (lhs_ui64val > rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_CMP_GE)
        ret = (lhs_ui64val >= rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_CMP_LT)
        ret = (lhs_ui64val < rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_CMP_LE)
        ret = (lhs_ui64val <= rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_CMP_EQ)
        ret = (lhs_ui64val == rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_CMP_NE)
        ret = (lhs_ui64val != rhs_ui64val);
      else if (opr == RBC_ENGINE::EXEC_KIND_ARITH_ADD) {
        // TODO: overflow checks
        ret = lhs_ui64val + rhs_ui64val;
      }
      else if (opr == RBC_ENGINE::EXEC_KIND_ARITH_SUB) {
        // TODO: underflow checks
        ret = lhs_ui64val - rhs_ui64val;
      }
      else if (opr == RBC_ENGINE::EXEC_KIND_ARITH_MPY) {
        // TODO: overflow checks
        ret = lhs_ui64val * rhs_ui64val;
      }
      else if (opr == RBC_ENGINE::EXEC_KIND_ARITH_DIV) {
        if (rhs_ui64val == 0) {
          Rbc_eval_certainty()->push_back(REC_UNKNOWN);
          Is_Trace(Tracing(), (TFile, "RBC ERROR: DBZ in Exec_eval.\n"));
        }
        else
          ret = lhs_ui64val / rhs_ui64val;
      }
      break;
    }
    case RBC_ENGINE::EXEC_KIND_NOT: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      UINT64 opnd =  Eval__exp(rbc_ctx, arg1);
      if(opnd == 0) {
        ret = 1;
      } else {
        ret = 0;
      }
      // swith true/false plist as result true/false is exchanged
      Switch_plist();
      break;
    }
    case RBC_ENGINE::EXEC_KIND_AND: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      UINT64 lhs_ui64val = Eval__exp(rbc_ctx, arg1);
      if(lhs_ui64val == 0) {
        ret = 0;
      } else {
        CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + parm_ofst);
        UINT64 rhs_ui64val = Eval__exp(rbc_ctx, arg2);
        ret = lhs_ui64val && rhs_ui64val;
      }
      break;
    }
    case RBC_ENGINE::EXEC_KIND_OR: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      UINT64 lhs_ui64val = Eval__exp(rbc_ctx, arg1);
      if(lhs_ui64val != 0) {
        ret = lhs_ui64val;
      } else {
        CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + parm_ofst);
        UINT64 rhs_ui64val = Eval__exp(rbc_ctx, arg2);
        ret = lhs_ui64val || rhs_ui64val;
      }
      break;
    }
    case RBC_ENGINE::EXEC_KIND_STR_REG_MATCH: {
      // bool str_reg_match(char *var, char *cst);
      Is_Trace(Tracing(), (TFile, "RBC eval: string regular match:\n"));
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      CODEREP *var = (CODEREP *) Eval__exp(rbc_ctx, arg1);
      Is_True_Rbc(var, ("RBC ERROR: null str passed to STR_REG_MATCH\n"));
      CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + parm_ofst);
      char *pattern = (char *) Eval__exp(rbc_ctx, arg2);
      Is_True_Rbc(pattern, ("RBC_BASE::Eval__exec_eval EXEC_KIND_STR_REG_MATCH: can't find pattern string"));
      VSA *vsa = rbc_ctx.Caller_vsa();
      RNA_NODE *caller_rna = rbc_ctx.Rna();
      ret = Is_str_match(vsa, caller_rna, var, pattern, rbc_ctx.Mem_pool());
      break;
    }
    case RBC_ENGINE::EXEC_KIND_IF: {
      CODEREP *arg1 = stmt->Rhs()->Find_nth_arg(1 + parm_ofst);
      UINT64 bool_retv = Eval__exp(rbc_ctx, arg1);
      if (bool_retv != 0) {
        CODEREP *arg2 = stmt->Rhs()->Find_nth_arg(2 + parm_ofst);
        ret = Eval__exp(rbc_ctx, arg2);
      }
      else {
        CODEREP *arg3 = stmt->Rhs()->Find_nth_arg(3 + parm_ofst);
        ret = Eval__exp(rbc_ctx, arg3);
      }
      break;
    }
    default:
      Is_Trace(Tracing(), (TFile, "RBC ERROR: builtin opr/func %d not implemented yet.\n", opr));
    }
  }
  return ret;
}


// =============================================================================
//
// RBC_BASE::Global_builtin_certc_check: entry point for certc builtin check
//    that needs to be done globally
//
// =============================================================================
void
RBC_BASE::Global_builtin_certc_check(IPSA *ipsa)
{
  Is_Trace(Tracing(), (TFile, "########## Global builtin cert C analyzing.\n"));
  // for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
  //   DNA_NODE *func = iter.Current();
  //   CONTEXT_SWITCH ctx(func);
  // }
}


// =============================================================================
//
// Builtin_recursion_check: entry point for recursion analyzing
//   DNA_RECURSION_ENTRY flag has been set up in Link() DFS walk
//
// =============================================================================
void
RBC_BASE::Builtin_recursion_check(IPSA *ipsa, MEM_POOL *pool)
{
  Is_Trace(Tracing(), (TFile, "########## Builtin recursion analyzing.\n"));
  hash_set<IDTYPE> visited;
  vector<IDTYPE> in_stack;
  in_stack.reserve(64);
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *dna = iter.Current();
    if (dna == NULL || dna->Non_functional() ||
        visited.find(dna->Dna_idx()) != visited.end())
      continue;

    if (dna->Is_set(DNA_RECURSION_ENTRY)) {
      in_stack.clear();
      SRCPOS_HANDLE srcpos_h(dna, pool);
      srcpos_h.Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
      srcpos_h.Set_orig_stname(dna->Fname());
      Print_out_recursion(NULL, dna, NULL, &srcpos_h, visited, in_stack);
    }
  }
}


// =============================================================================
//
// Print_out_recursion: find out recursion calls and report
//   CALL & ICALL are not treated differently, this may affect [M] or [D]
//
// =============================================================================
BOOL
RBC_BASE::Print_out_recursion(DNA_NODE *caller, DNA_NODE *dna, STMTREP *stmt,
                              SRCPOS_HANDLE *srcpos_h,
                              hash_set<IDTYPE> &visited, vector<IDTYPE> &in_stack)
{
  IDTYPE dna_idx = dna->Dna_idx();
  for (vector<IDTYPE>::iterator iter = in_stack.begin(); iter != in_stack.end(); iter++) {
    if (*iter == dna_idx) {
      visited.insert(dna_idx);
      Report_rbc_error(caller->Comp_unit()->Vsa(), stmt, "CRF", FALSE, srcpos_h);
      if (VSA_Xsca) {
        Report_xsca_error(caller->Comp_unit()->Vsa(), stmt->Linenum(),
                          "MSR_17_2", srcpos_h);
      }
      return TRUE;
    }
  }

  CONTEXT_SWITCH context(dna);
  in_stack.push_back(dna_idx);
  RNODE_VECTOR *rna_list = dna->Call_list();
  INT idx = 0;
  srcpos_h->Add_children(rna_list->size());
  SRCPOS_TREENODE *cur_node = srcpos_h->Cur_node();
  for (INT i = VAR_INIT_ID; i < rna_list->size(); i++) {
    RNA_NODE *rna = (*rna_list)[i];
    if (rna == NULL)
      continue;
    srcpos_h->Set_cur_node(cur_node, idx, TRUE);
    srcpos_h->Append_data(rna->Callstmt(), dna, PATHINFO_DNA_CALLSITE);
    idx++;
    for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
         iter != rna->Callee_list().end(); iter++) {
      DNA_NODE *callee = dna->Comp_unit()->Vsa()->Ipsa()->Get_dna(iter->Callee());
      IDTYPE callee_idx = callee->Dna_idx();
      if (callee == NULL || callee->Non_functional() ||
          visited.find(callee->Dna_idx()) != visited.end())
        continue;
      if (Print_out_recursion(dna, callee, rna->Callstmt(), srcpos_h,
                              visited, in_stack)) {
        return TRUE;
      }
    }
  }
  in_stack.pop_back();
  visited.insert(dna_idx);
  return FALSE;
}


// =============================================================================
//
// Builtin_certc_msc37: Check_return_for_all_exits, iterate all exit bbs,
// search for return statement, report error if return type is not void and
// exit bb doesn't return a value, except 'main'
//
// =============================================================================
void
RBC_BASE::Builtin_certc_msc37(DNA_NODE *dna)
{
  ST *fun_st = dna->St();
  if (TY_kind(TY_ret_type(ST_pu_type(fun_st))) == KIND_VOID)
    return;
  if (strcmp(dna->Fname(), "main") == 0)
    return;

  EXITBB_ITER iter(dna->Comp_unit()->Cfg());
  FOR_ALL_ITEM(iter, Init()) {
    BB_NODE *exit_bb = iter.Cur_exit_bb();
    STMTREP *stmt = exit_bb->Last_stmtrep();
    if (stmt == NULL) continue;
    if (stmt != NULL &&
        (stmt->Opr() == OPR_RETURN ||
         stmt->Opr() == OPR_RETURN_VAL))
      stmt = stmt->Prev();
    if (stmt != NULL && stmt->Opr() == OPR_STID)
      continue;
    if (stmt == NULL) {
      stmt = exit_bb->Prev()->Last_stmtrep();
    }
    if (stmt != NULL && stmt->Opr() == OPR_CALL) {
      ST *st = stmt->St();
      if (PU_has_attr_noreturn((*Pu_Table_ptr)[ST_pu(st)]))
        continue;
      char* fname = ST_name(st);
      if (Is_lib_func(fname))
        continue;
    }
    // if the last stmt is goto, it's quite likely in an infinite loop
    // so the return stmt may be unreachable and we don't report this
    if (stmt != NULL && stmt->Opr() == OPR_GOTO) {
        continue;
    }
    COMP_UNIT *cu = dna->Comp_unit();
    SRCPOS_HANDLE srcpos_h(dna, cu->Loc_pool());
    SRCPOS spos = cu->Get_end_srcpos();
    srcpos_h.Append_data(spos, NULL, dna, PATHINFO_VUL_SPOT_SO);
    Report_rbc_error(cu->Vsa(), spos, "MSC37-C", FALSE, &srcpos_h);
    if (VSA_Xsca) {
      Report_xsca_error(cu->Vsa(), spos, "MSR_17_4", &srcpos_h);
    }
  }
}


// =============================================================================
//
// Builtin_certcpp_str50: Guarantee that storage for strings has sufficient
// space for character data and the null terminator for operator "std::cin >>"
//
// =============================================================================
void
RBC_BASE::Builtin_certcpp_str50(DNA_NODE *dna)
{
  // ensure there's a "std::cin.width" call before every "std::cin >>"
  RNODE_VECTOR *rna_list = dna->Call_list();
  VSA *vsa = dna->Comp_unit()->Vsa();
  for (INT i = VAR_INIT_ID; i < rna_list->size(); i++) {
    BOOL ret = TRUE;
    RNA_NODE *rna = (*rna_list)[i];
    if (rna == NULL)
      continue;
    STMTREP *call_stmt = rna->Callstmt();
    ST *st = call_stmt->St();
    // std::cin >> call
    if (call_stmt->Opr() == OPR_CALL && st != NULL &&
        (strcmp(ST_name(st), "_ZStrsIcSt11char_traitsIcEERSt13basic_istreamIT_T0_ES6_PS3_") == 0)) {
      hash_set<IDTYPE> visited;
      ret = Builtin_certcpp_str50(call_stmt->Prev(), call_stmt->Bb(), visited);
      Is_True(visited.size() == 0, ("RBC ERROR: not empty visited BB set\n"));
    }
    if (!ret) {
      SRCPOS_HANDLE srcpos_h(NULL, call_stmt, dna, vsa->Loc_pool(), vsa);
      srcpos_h.Set_orig_stname("std::cin >>");
      Report_rbc_error(vsa, call_stmt, "STR50-CPP", FALSE, &srcpos_h);
    }
  } // end for RNA_LIST
}


BOOL
RBC_BASE::Builtin_certcpp_str50(STMTREP *stmt, BB_NODE *bb, hash_set<IDTYPE> &visited)
{
  BOOL ret = TRUE;
  BOOL start = FALSE;
  if (visited.find(bb->Id()) != visited.end()) {
    return TRUE;
  }
  visited.insert(bb->Id());
  STMTREP *cur;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  FOR_ALL_NODE_REVERSE(cur, stmt_iter, Init()) {
    if (!start) {
      if (cur == stmt)
        start = TRUE;
      else
        continue;
    }
    OPERATOR opr = cur->Opr();
    if (opr == OPR_CALL) {
      ST *st = cur->St();
      if (st != NULL) {
        if (strcmp(ST_name(st), "_ZNSt8ios_base5widthEl") == 0) {
          // std::cin.width call
          visited.erase(bb->Id());
          return TRUE;
        }
        else if (strcmp(ST_name(st), "_ZStrsIcSt11char_traitsIcEERSt13basic_istreamIT_T0_ES6_PS3_") == 0) {
          // std::cin >> call
          visited.erase(bb->Id());
          return FALSE;
        }
      } // end if st != NULL
    }
    else if (opr == OPR_PRAGMA) {
      // if it is inlined std::cin.width
      ST *wn_st = WN_st(cur->Orig_wn());
      if (wn_st != NULL && ST_class(wn_st) == CLASS_FUNC &&
          WN_pragma(cur->Orig_wn()) == WN_PRAGMA_INLINE_BODY_END) {
        const char *fname = ST_name(wn_st);
        if (strcmp(fname, "_ZNSt8ios_base5widthEl") == 0) {
          visited.erase(bb->Id());
          return TRUE;
        }
      }
    }
  }

  if (bb->Pred() == NULL) {
    visited.erase(bb->Id());
    return FALSE;
  }

  BB_NODE *prev_bb;
  BB_LIST_ITER prev_bb_iter;
  FOR_ALL_ELEM(prev_bb, prev_bb_iter, Init(bb->Pred())) {
    if (!Builtin_certcpp_str50(prev_bb->Last_stmtrep(), prev_bb, visited)) {
      ret = FALSE;
      break;
    }
  }
  visited.erase(bb->Id());
  return ret;
}


// =============================================================================
//
// Builtin_cwe390_ewa: CWE390_Error_Without_Action__empty_catch
// checks if a catch block is empty
//
// =============================================================================
void
RBC_BASE::Builtin_cwe390_ewa(DNA_NODE *dna)
{
  VSA *vsa = dna->Comp_unit()->Vsa();
  BB_NODE *bb = dna->Comp_unit()->Cfg()->Entry_bb();
  STMTREP *stmt = bb->First_stmtrep();
  hash_set<IDTYPE> visited;
  Builtin_cwe390_ewa(stmt, bb, FALSE, vsa, visited);
}


void
RBC_BASE::Builtin_cwe390_ewa(STMTREP *stmt, BB_NODE *bb, BOOL icb, VSA *vsa,
                             hash_set<IDTYPE> &visited)
{
  // do not reset visited bb because we don't need to report same catch block error
  // more than once
  if (visited.find(bb->Id()) != visited.end())
    return;
  visited.insert(bb->Id());
  BOOL in_catch_block = icb;
  STMTREP *cur;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  FOR_ALL_NODE(cur, stmt_iter, Init()) {
    OPERATOR opr = cur->Opr();
    if (opr == OPR_CALL) {
      ST *st = cur->St();
      if (st != NULL) {
        if (strcmp(ST_name(st), "__cxa_begin_catch") == 0) {
          // catch begin
          in_catch_block = TRUE;
          continue;
        }
        else if (strcmp(ST_name(st), "__cxa_end_catch") == 0 && in_catch_block) {
          if (VSA_Ecb) {
            // catch end, empty, report error
            SRCPOS_HANDLE srcpos_h(NULL, cur, vsa->Dna(), vsa->Loc_pool(), vsa);
            srcpos_h.Set_orig_stname("catch");
            Report_rbc_error(vsa, cur, "ECB", FALSE, &srcpos_h);
          }
          return;
        }
      } // end if st != NULL
    }
    if (in_catch_block) {
      // catch block not empty
      return;
    }
  }

  BB_NODE *succ_bb;
  BB_LIST_ITER succ_bb_iter;
  FOR_ALL_ELEM(succ_bb, succ_bb_iter, Init(bb->Succ())) {
    Builtin_cwe390_ewa(succ_bb->First_stmtrep(), succ_bb, in_catch_block, vsa, visited);
  }
  return;
}


// =============================================================================
//
// Builtin_ctor_init: checks if class constructor has initialized all its
// member fields
//
// =============================================================================
void
RBC_BASE::Builtin_ctor_init(DNA_NODE *dna)
{
  if (dna == NULL || !PU_is_constructor(*dna->Pu()) || !PU_cxx_lang(*dna->Pu()))
    return;

  CONTEXT_SWITCH context(dna);
  VSA *vsa = dna->Comp_unit()->Vsa();
  char *fname = Vsa_demangle(dna->Fname());
  Is_Trace(Tracing(),
           (TFile,
            "RBC_BASE::Builtin_ctor_init: verifying member fields initializing for %s\n",
            fname));
  BB_NODE *entry_bb = vsa->Comp_unit()->Cfg()->Entry_bb();
  // find coderep of 'this' pointer so as to find class fields
  DNA_NODE *this_dna = dna;
  // dna visited set to avoid recursion
  hash_set<IDTYPE> visited;
  visited.clear();
  visited.insert(dna->Dna_idx());
  CODEREP *this_cr = Find_this_cr(vsa, entry_bb, this_dna, visited);
  if (this_cr == NULL)
    return;
  hash_set<UINT> fields;
  fields.clear();
  visited.clear();
  visited.insert(dna->Dna_idx());
  // find initialized statement in constructor to record fields initialized
  Builtin_ctor_init(vsa, entry_bb, fields, visited);
  CONTEXT_SWITCH this_ctx(this_dna);
  ST *this_st = vsa->Opt_stab()->Aux_stab_entry(this_cr->Aux_id())->St();
  if (this_st != NULL) {
    TY_IDX this_ty_idx = ST_type(this_st);
    if (TY_kind(this_ty_idx) == KIND_POINTER &&
        Is_Structure_Type(TY_pointed(this_ty_idx))) {
      FLD_ITER fld_iter = Make_fld_iter(TY_fld(TY_pointed(this_ty_idx)));
      UINT fld_id = 0;
      // iterate class member field to check if it has been initialized
      do {
        FLD_HANDLE fld(fld_iter);
        // fld.Idx() is (*fld_iter).Index() in this case, so we have to maintain fld_id ourselves
        fld_id++;
        if (!fld.Is_Null()) {
          Is_Trace(Tracing(), (TFile, "---------- Checking field(%d):%s\n", fld_id, FLD_name(fld)));
          if (fields.find(fld_id) == fields.end()) {
            // for fields that are not initialized in constructor, report error
            SRCPOS_HANDLE srcpos_h(dna, vsa->Loc_pool());
            srcpos_h.Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
            const char *cls_name = TY_name(TY_pointed(this_ty_idx));
            const char *fld_name = srcpos_h.Gen_fld_stname(cls_name, TY_pointed(this_ty_idx), fld_id);
            srcpos_h.Set_orig_stname(fld_name);
            srcpos_h.Set_msgid("UIC.1");
            Report_rbc_error(vsa, ST_Srcpos(*dna->St()), "UIC", FALSE, &srcpos_h);
          }
        }
      } while (!FLD_last_field(fld_iter++));
    }
  }
  if (fname != NULL)
    free(fname);
}


// =============================================================================
//
// Find_this_cr: iterate OPR_ISTORE statements and return 'this' coderep,
// also return the context for 'this' in 'this_dna', search in all callees
// recursively if not found
//
// =============================================================================
CODEREP*
RBC_BASE::Find_this_cr(VSA *vsa, BB_NODE *bb, DNA_NODE *this_dna,
                       hash_set<IDTYPE> &visited)
{
  CODEREP *ret = NULL;
  if (bb == NULL)
    return ret;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    OPERATOR opr = stmt->Opr();
    if (opr == OPR_ISTORE) {
      CODEREP *lhs = stmt->Lhs();
      if (lhs != NULL && lhs->Kind() == CK_IVAR && lhs->Opr() == OPR_ILOAD) {
        lhs = lhs->Istr_base();
        if (lhs != NULL && lhs->Kind() == CK_VAR) {
          const char *cls_name = ST_name(vsa->Opt_stab()->Aux_stab_entry(lhs->Aux_id())->St());
          if (strcmp(cls_name, "this") == 0) {
            this_dna = vsa->Dna();
            return lhs;
          }
        }
      } // end CK_IVAR
    } else if (OPERATOR_is_call(opr)) {
      RNA_NODE *rna = vsa->Dna()->Get_callsite_rna(stmt);
      if (rna != NULL) {
        for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
             iter != rna->Callee_list().end(); iter++) {
          DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
          if (callee == NULL)
            continue;
          if (callee->Non_functional())
            continue;
          if (visited.find(callee->Dna_idx()) != visited.end())
            continue;
          visited.insert(callee->Dna_idx());
          CONTEXT_SWITCH context(callee);
          VSA *callee_vsa = callee->Comp_unit()->Vsa();
          BB_NODE *entry_bb = callee->Comp_unit()->Cfg()->Entry_bb();
          ret = Find_this_cr(callee_vsa, entry_bb, this_dna, visited);
          if (ret != NULL)
            return ret;
        }
      }
    }// end OPR_ISTORE
  } // end FOR

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    ret = Find_this_cr(vsa, dom_bb, this_dna, visited);
    if (ret != NULL)
      return ret;
  }
  return ret;
}


// =============================================================================
//
// Builtin_ctor_init: iterate OPR_ISTORE statements and record field id that
// has been initialized, recursively search in all callees
//
// =============================================================================
void
RBC_BASE::Builtin_ctor_init(VSA *vsa, BB_NODE *bb, hash_set<UINT> &fields,
                            hash_set<IDTYPE> &visited)
{
  if (bb == NULL)
    return;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  STMTREP *stmt;
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    OPERATOR opr = stmt->Opr();
    if (opr == OPR_ISTORE) {
      CODEREP *lhs = stmt->Lhs();
      if (lhs != NULL && lhs->Kind() == CK_IVAR) {
        UINT fld_id = lhs->I_field_id();
        TY_IDX ilod_ty = lhs->Ilod_ty();
        if (Is_Structure_Type(ilod_ty) && fld_id > 0) {
          UINT curr_id = 0;
          FLD_HANDLE fld = FLD_get_to_field(ilod_ty, fld_id, curr_id);
          fields.insert(fld_id);
          Is_Trace(Tracing(), (TFile, "---------- Initialized field(%d):%s in %s\n",
                               fld_id, FLD_name(fld), vsa->Dna()->Fname()));
        } // end Is_Structure_Type
      } // end CK_IVAR
    } else if (OPERATOR_is_call(opr)) {
      RNA_NODE *rna = vsa->Dna()->Get_callsite_rna(stmt);
      if (rna != NULL) {
        for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
             iter != rna->Callee_list().end(); iter++) {
          DNA_NODE *callee = vsa->Ipsa()->Get_dna(iter->Callee());
          if (callee == NULL)
            continue;
          if (callee->Non_functional())
            continue;
          if (visited.find(callee->Dna_idx()) != visited.end())
            continue;
          visited.insert(callee->Dna_idx());
          CONTEXT_SWITCH context(callee);
          VSA *callee_vsa = callee->Comp_unit()->Vsa();
          BB_NODE *entry_bb = callee->Comp_unit()->Cfg()->Entry_bb();
          Builtin_ctor_init(callee_vsa, entry_bb, fields, visited);
        }
      }
    } // end OPR_ISTORE
  } // end FOR

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs()))
    Builtin_ctor_init(vsa, dom_bb, fields, visited);
}


// =============================================================================
//
// RBC_BASE::Global_builtin_certj_check: it performs global builtin certj check
// some check just check function defination and so on we can do in front-end
// check those rule here
//
// =============================================================================
void
RBC_BASE::Global_builtin_certj_check(IPSA *ipsa)
{
  Is_Trace(Tracing(), (TFile, "########## Global builtin cert java analyzing.\n"));
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *dna = iter.Current();
    CONTEXT_SWITCH ctx(dna);
    if (!PU_java_lang(Get_Current_PU())) {
      continue;
    }
    // obj07 checks based on rbc nodes, so has to be placed here
    if (dna->Non_functional()) {
      Builtin_certj_obj07(ipsa, dna);
    }
  }
  Builtin_certj_dcl00(ipsa, NULL);
}


// =============================================================================
//
// Builtin_certj_env06: check if main function exits, only suitable for web
// application
//
// =============================================================================
void
RBC_BASE::Builtin_certj_env06(DNA_NODE *dna)
{
  // CERT ENV06-J check
  if (VSA_Check_Main_Entry) {
    Is_Trace(Tracing(), (TFile, "RBC_BASE::Builtin_certj_env06: check main function.\n"));
    if (PU_is_mainpu(Get_Current_PU())) {
      VSA *vsa = dna->Comp_unit()->Vsa();
      // SRCPOS spos = SRCPOS_NODE::Transpos(WN_Get_Linenum(vsa->Comp_unit()->Input_tree()), File_Index);
      // VSA_ISSUE issue("CERT", "RBC", "ENV06-J", "", "", vsa->Cur_pu_name(), spos,  IC_DEFINITELY, NULL);
      // Vsa_error_print(&issue);
      STMTREP *sr = vsa->Get_entry_chi_stmt();
      SRCPOS_HANDLE srcpos_h(NULL, sr, dna, vsa->Loc_pool(), vsa);
      vsa->Rbc()->Report_rbc_error(vsa, sr, "ENV06-J", IC_MAYBE, &srcpos_h);
    }
  }
}


// =============================================================================
//
// Builtin_certj_met06: check if any overridable methods in clone()
//
// =============================================================================
void
RBC_BASE::Builtin_certj_met06(IPSA *ipsa, DNA_NODE *dna)
{
  const char *check_name = "clone";
  char *fname = dna->Fname();
  Is_Trace(Tracing(),
           (TFile,
            "RBC_BASE::Builtin_certj_met06: overridable methods in clone [%s]\n",
            fname));
  if(strstr(fname, check_name)) {
    UINT32 acc_flag = ipsa->Glob_cha()->Get_meth_flag(dna->Fname());
    // skip bridge method, as they are compiler generated
    if(acc_flag & ACC_BRIDGE) {
      Is_Trace(Tracing(), (TFile, "CERTJ_MET06: skip bridge method %s\n", dna->Fname()));
      return;
    }
    STRING_BUFFER buf(strlen(fname) + 1);
    const char *fun_sig = CLASS_HIERARCHY::Extract_fun_sig(fname, &buf);
    if(fun_sig != NULL && strcmp(fun_sig, check_name) == 0) {
      VSA *vsa = dna->Comp_unit()->Vsa();
      RNODE_VECTOR *rna_list = dna->Call_list();
      for (INT i = VAR_INIT_ID; i < rna_list->size(); ++i) {
        RNA_NODE *rna = (*rna_list)[i];
        if(rna->Callee_list().size() >= 1) {
          for(CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
              it != rna->Callee_list().end(); it++) {
            DNA_NODE* callee = ipsa->Get_dna(it->Callee());
            if(callee && !callee->Non_functional()) {
              UINT32 flag = ipsa->Glob_cha()->Get_meth_flag(callee->Fname());
              if(flag != ACC_INVALID) {
                if(Is_func_overridable(flag)) {
                  Is_Trace(Tracing(),
                           (TFile,
                            "CERTJ_MET06: Func %s is not private or final Mark Overridable:Y\n",
                            callee->Fname()));
                  SRCPOS_HANDLE srcpos_h(NULL, rna->Callstmt(), dna, vsa->Loc_pool(), vsa);
                  srcpos_h.Append_data(callee->St(), NULL, callee, PATHINFO_ST_DECLARE);
                  srcpos_h.Set_orig_stname(callee->Fname());
                  vsa->Rbc()->Report_rbc_error(vsa, rna->Callstmt(), "MET06-J", IC_DEFINITELY, &srcpos_h);
                }
              } else {
                Is_Trace(Tracing(), (TFile, "CERTJ_MET06: Func %s Flag is not set\n", callee->Fname()));
              }
            }
          }
        } else {
          Is_Trace(Tracing(),
                   (TFile,
                    "CERTJ_MET06: rna %d cannot resolve candidates, Mark Overridable:N\n",
                    rna->Rna_idx()));
        }
      } // end of rna loop
    } // end of if fun sig is "clone"
  }
}

// the regular expression of sevsitive variable name
static const char *msc03_sensitive_var_pattern[] = {
  "^ip",
  "\\.ip",
  "^password",
  "\\.password",
};

// =============================================================================
//
// Builtin_certj_msc03: MSC03-J. Never hard code sensitive information
//   We check whether a variable is initialized by const string
//   This rule was implemented in 2 parts
//   Part1: user mark some parameter should be check
//   Part2: check STID/ISTORE if the variable name is sensitive
//   the implement in below is part 2
//
// =============================================================================
void
RBC_BASE::Builtin_certj_msc03(DNA_NODE *dna)
{
  BB_NODE *entry_bb = dna->Comp_unit()->Vsa()->Cfg()->Entry_bb();
  INT pattern_len = sizeof(msc03_sensitive_var_pattern) / sizeof(const char *);
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Builtin_certj_msc03: Compile regular expression pattern array.\n"));
  regex_t **reg_exp_arr = Compile_pattern_arr(msc03_sensitive_var_pattern, pattern_len, TRUE, Loc_pool());
  Builtin_certj_msc03_bb(dna, entry_bb, reg_exp_arr, pattern_len);
}

void
RBC_BASE::Builtin_certj_msc03_bb(DNA_NODE *dna, BB_NODE *bb, regex_t **reg_exp_arr, INT len)
{
  VSA *vsa_ctx = dna->Comp_unit()->Vsa();
  STMTREP *stmt;
  STMTREP_ITER iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, iter, Init()) {
    switch (stmt->Opr()) {
    case OPR_STID:
    case OPR_ISTORE:
      {
        CODEREP *lhs = stmt->Lhs();
        STRING_BUFFER sbuf(VSA_VAR_NAME_MAX_LEN);
        const char *name = SRCPOS_HANDLE::Find_cr_stname(&sbuf, lhs, stmt, dna);
        if (name == NULL) {
          break;
        }
        for (INT i = 0; i < len; i++) {
          regex_t *reg_exp = reg_exp_arr[i];
          if (reg_exp == NULL) {
            continue;
          }
          regmatch_t dummy[1];
          INT reti = (INT) regexec(reg_exp, name, 1, dummy, 0);
          // the variable is sensitive
          if (!reti) {
            Is_Trace(Tracing(),
              (TFile, "RBC_BASE::Built_in_certj_msc03_bb: Find sensitive variable, sr: sr%d, cr: cr%d, var name: %s.\n",
              stmt->Stmtrep_id(), lhs->Coderep_id(), name));
            vector<SRCPOS_HANDLE *> sp_vector;
            Find_var_init_by_const_str(dna, stmt, lhs, sp_vector);
            for (INT j = 0; j < sp_vector.size(); j++) {
              vsa_ctx->Rbc()->Report_rbc_error(vsa_ctx, stmt, "MSC03-J", IC_DEFINITELY, sp_vector[j]);
            } // for
            // match
            break;
          }
        }
      }
      break;
    default:
      break;
    }
  }

  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Builtin_certj_msc03_bb(dna, dom_bb, reg_exp_arr, len);
  }
}

// =============================================================================
//
// Builtin_certj_obj07: Sensitive classes must not let themselves be copied
//   Check sensitive class and their children
//
// =============================================================================
void
RBC_BASE::Builtin_certj_obj07(IPSA *ipsa, DNA_NODE *dna)
{
  if(!PU_is_constructor(Get_Current_PU())) {
    return;
  }
  ST *this_st = Get_this_symbol(dna);
  if(this_st != NULL) {
    TY_IDX ty_idx = ST_type(this_st);
    Is_True_Ret(TY_kind(ty_idx) == KIND_POINTER, ("RBC ERROR:this st is not pointer type\n"));
    TY_IDX class_ty = TY_pointed(ty_idx);
    Is_True_Ret(TY_kind(class_ty) == KIND_STRUCT, ("RBC ERROR: class ty is not struct type\n"));
    if(TY_is_sensitive_class(class_ty)) {
      Is_Trace(Tracing(),
               (TFile, 
                "RBC_BASE::Builtin_certj_obj07: should not copy sensitive class [%s]\n",
                 dna->Fname()));
      const char *cls_name = TY_name(class_ty);
      Check_class_canbe_copied(ipsa, cls_name, "OBJ07-J", "OBJ07J");
      // check child classes
      CLASS_SET *children = ipsa->Glob_cha()->Get_children(cls_name);
      if(children) {
        CLASS_SET_ITER iter = children->begin();
        for(; iter != children->end(); iter++) {
          Check_class_canbe_copied(ipsa, *iter, "OBJ07-J", "OBJ07J");
        }
      }
    }
  }
}


// =============================================================================
// 
// Check_class_canbe_copied: check a class canbe copied
// Algorithm:
//                            if(class is sensitive)
//                             /                 \
//                            Y                   N
//                           /                     \
//                   if(class is final)            GOOD
//                       /     \
//                      Y       N
//                     /         \
//                    GOOD    if(define copy ctor)
//                                   /      \
//                                  Y        N
//                                 /          \
//                               BAD     if(defined clone)
//                                            /    \
//                                           Y      N
//                                          /        \
//                                 foreach(clone)    BAD
//                                       |
//                if(clone is final && clone throws CloneNotSupportedException)
//                                  /         \
//                                 Y           N
//                                /             \
//                              GOOD            BAD
//
// =============================================================================
enum COPY_KIND {
  NO_COPY             = 0,    // cannot be copied
  BY_COPY_CTOR        = 1,    // copy by copy constructor
  BY_FINAL_CLONE      = 2,    // copy by final clone method
  BY_NON_FINAL_CLONE  = 3,    // copy by non-final clone method
  BY_OBJ_CLONE        = 4,    // copy by Object clone method
};

BOOL
RBC_BASE::Check_class_canbe_copied(IPSA *ipsa, const char *class_name,
                                   const char *rule, const char *msg_prefix)
{
  COPY_KIND cp_kind = NO_COPY;
  DNA_NODE *cp_dna = NULL;
  CLASS_HIERARCHY *cha = ipsa->Glob_cha();
  CLASS_INFO *cls_info = cha->Get_class_info(class_name);
  if(cls_info != NULL && cls_info->Get_aux_info() != NULL) {
    UINT32 file_idx = cls_info->File_idx();
    UINT16 acc_flag = cls_info->Get_class_acc_flag();
    if(! (acc_flag & ACC_FINAL)) {
      TY_IDX class_ty = cls_info->Get_class_ty();
      ST_IDX copy_ctor = (class_ty == TY_IDX_ZERO) ?
                          ST_IDX_ZERO : TY_copy_constructor(*Ty_ptr(file_idx, class_ty));
      if(copy_ctor != ST_IDX_ZERO) {
        cp_dna = ipsa->Get_dna(file_idx, copy_ctor);
        cp_kind =  BY_COPY_CTOR;
      } else {
        vector<JAVA_METH_INFO*> clone_methods;
        cls_info->Get_meth_by_fname("clone", clone_methods);
        if(clone_methods.size() > 0 && cp_kind == NO_COPY) {
          vector<JAVA_METH_INFO*>::iterator iter;
          for(iter = clone_methods.begin(); iter != clone_methods.end(); iter++) {
            JAVA_METH_INFO *meth_info = *iter;
            UINT32 method_flag = meth_info->Get_flag();
            // ignore bridge clone method, they are autoly generated by javac
            if(method_flag & ACC_BRIDGE) {
              continue;
            }

            ST_IDX st_idx = meth_info->St_idx();
            DNA_NODE *dna = (st_idx == ST_IDX_ZERO) ? NULL : ipsa->Get_dna(file_idx, st_idx);
            if(method_flag & ACC_FINAL) {
              if(dna == NULL) {
                continue;
              }
              // check if function throws "CloneNotSupportedException" or its subclass
              // TODO: may need to check function body only contains throw exception stmt
              const char *excp_name = "java.lang.CloneNotSupportedException";
              vector<const char *> chk_list;
              // add child exception to check list
              CLASS_SET *children = ipsa->Glob_cha()->Get_children(excp_name);
              if(children) {
                chk_list.insert(chk_list.begin(), children->begin(), children->end());
              }
              chk_list.push_back(excp_name);
              BOOL chk_res = dna->Comp_unit()->Vsa()->Check_fun_throws(dna, chk_list);
              if(!chk_res) {
                cp_dna = dna;
                cp_kind = BY_FINAL_CLONE;
              }
            } else {
              cp_dna = dna;
              cp_kind = BY_NON_FINAL_CLONE;
            }
          }
        } else {
          // not define clone, can be cloned by object's clone
          // choose constructor dna to report error
          vector<JAVA_METH_INFO*> ctors;
          cls_info->Get_meth_by_fname("<init>", ctors);
          if(ctors.size() > 0) {
            JAVA_METH_INFO *ctor = ctors[0];
            ST_IDX st_idx = ctor ? ctor->St_idx() : ST_IDX_ZERO;
            cp_kind = BY_OBJ_CLONE;
            cp_dna = (st_idx == ST_IDX_ZERO) ? NULL : ipsa->Get_dna(file_idx, st_idx);
          }
        }
      }
    }
  }
  // Report error if rule is set
  if(cp_kind != NO_COPY && rule != NULL) {
    char *msg_id = NULL;
    char comb_msg[SIZE_MAX_STR];
    if(msg_prefix != NULL) {
      Is_True_Ret(strlen(msg_prefix) + 11 < SIZE_MAX_STR, ("msg_id outof bound"), TRUE); // 11 for 10(max_len(cp_kind)) + 1(dot)
      snprintf(comb_msg, SIZE_MAX_STR, "%s.%d", msg_prefix, cp_kind);
      msg_id = comb_msg;
    }
    Report_rbc_error(cp_dna, class_name, rule, msg_id, FALSE);
  }
  BOOL ret = (cp_kind != NO_COPY);
  Is_Trace(Tracing(), (TFile, "    -class %s can be copied: %s, copy kind %d, copy by %s\n",
                       class_name, ret ? "Y":"N", cp_kind, cp_dna ? cp_dna->Fname():"<>"));
  return ret;
}

// =============================================================================
//
// Check_class_init_cycle: Check if a class has initialize cycle
// Intraclass Cycle: global is not initialized before use
//   Ex: A.a1 = A.a2;  // A.a2 not init yet
//       A.a2 = 10;
// Interclass Cycle: cross class global cycle
//   [1] cycle with same dna
//       Ex: class A : A.a1 = B.b1;
//           class B : B.b1 = A.a1;
//           A::clinit->B::clint->A::clinit
//   [2] cycle with same class
//       Ex: class A: A.a1 = B.foo();
//           class B: B.foo = A.foo();
//           A::clinit->B::foo()->A::foo()
//
// =============================================================================
BOOL
RBC_BASE::Check_class_init_cycle(DNA_NODE *check_dna, const char *check_cls,
                                 STMTREP *check_sr, GLOB_REF_INFO *ref_info,
                                 GLOB_REF_MGR *ref_mgr, BOOL cross_class, 
                                 hash_set<IDTYPE> *visited_dna,
                                 SRCPOS_HANDLE *sp_h)
{
  DNA_NODE *ref_dna = ref_info->Ref_dna();
  Is_True_Ret(ref_dna && check_sr, ("null check sr or ref_dna"), FALSE);

  Is_Trace(Tracing(),
           (TFile, "------check ref %s:%s\n",
            ref_info->Is_ref_by_var() ? "var" : "fun",
            ref_info->Glob_name()));

  CONTEXT_SWITCH ctx(ref_dna);
  VSA *ref_vsa = ref_dna->Comp_unit()->Vsa();
  STMTREP *entry_sr = ref_vsa->Get_entry_chi_stmt();
  if(ref_info->Is_ref_by_call() && entry_sr) {
    sp_h->Append_data(entry_sr, ref_dna, PATHINFO_DNA_CALLSITE);
  }
  if(check_dna == ref_dna) {
    if(cross_class) {
      // Inter class Cycle
      Is_Trace(Tracing(), 
               (TFile, "------class %s has InterClass cycle\n", check_cls));
      sp_h->Append_data(check_sr, check_dna, PATHINFO_RBC);
      Report_rbc_error(ref_vsa, check_sr, "DCL00-J", IC_DEFINITELY, sp_h);
    } else if(ref_info->Is_ref_by_var()) {
      // Intraclass: check initialize order
      vector<STMTREP *> stmts;
      ref_mgr->Get_ref_chi_stmts(ref_dna, ref_info->Ref_st(), stmts);
      BOOL init_before_check = FALSE;
      for(vector<STMTREP *>::iterator stmt_iter = stmts.begin();
          stmt_iter != stmts.end(); stmt_iter++) {
        STMTREP *sr = *stmt_iter;
        if(ref_vsa->Is_stmt_reaches(sr, check_sr)) {
          init_before_check = TRUE;
          break;
        }
      }
      if(!init_before_check) {
        Is_Trace(Tracing(),
                 (TFile, "------class %s has IntraClass cycle\n", check_cls));
        sp_h->Append_data(check_sr, ref_dna, PATHINFO_RBC);
        Report_rbc_error(ref_vsa, check_sr, "DCL00-J", IC_DEFINITELY, sp_h);
      }
    }
    return TRUE;
  } else {
    const char *fname = ref_dna->Fname();
    STRING_BUFFER buf(strlen(fname) + 1);
    const char *ref_cls = CLASS_HIERARCHY::Extract_class_name(fname, &buf);
    if(strcmp(check_cls, ref_cls) != 0) {
      INT len1 = strlen(check_cls);
      INT len2 = strlen(ref_cls);
      BOOL is_inner = FALSE;
      // do not mark cross class for inner class
      // inner class name has "$" between class names
      // Ex: Outer$Inner
      if(len1 != len2) {
        const char *lcls = len1 > len2 ? check_cls : ref_cls;
        const char *scls = len1 > len2 ? ref_cls : check_cls;
        INT scls_len = strlen(scls);
        if(strncmp(lcls, scls, scls_len) == 0 && lcls[scls_len] == '$') {
          is_inner = TRUE;
        }
      }
      if(!is_inner) {
        cross_class = TRUE;
        if(!sp_h->Orig_stname()) {
          sp_h->Set_orig_stname(ref_info->Glob_name());
        }
      }
    } else if(cross_class && entry_sr) {
      // InterClass: cycle with same class
      sp_h->Append_data(entry_sr, ref_dna, PATHINFO_RBC);
      CONTEXT_SWITCH check_ctx(check_dna);
      Is_Trace(Tracing(),
               (TFile,
                "------class %s has InterClass cycle by same class name\n",
                check_cls));
      Report_rbc_error(check_dna->Comp_unit()->Vsa(), check_sr,
                       "DCL00-J", IC_DEFINITELY, sp_h);
      return TRUE;
    }
  }

  if(visited_dna->find(ref_dna->Dna_idx()) == visited_dna->end()) {
    visited_dna->insert(ref_dna->Dna_idx());
  } else {
    return FALSE;
  }

  GLOB_REF_LIST *ref_list = ref_mgr->Get_ref_list(ref_dna);
  if(ref_list == NULL) {
    return FALSE;
  }
  INT parent_idx = sp_h->Cur_idx();
  INT cur_idx = 0;
  BOOL ret = FALSE;
  SRCPOS_TREENODE *cur_node = sp_h->Add_children(ref_list->size());
  for(GLOB_REF_LIST::iterator ref_iter = ref_list->begin();
      ref_iter != ref_list->end(); ref_iter++, cur_idx++) {
    GLOB_REF_INFO *info = *ref_iter;
    CONTEXT_SWITCH ref_ctx(info->Ctx_dna());
    sp_h->Set_cur_node(cur_node, cur_idx);
    if(info->Ref_dna() == NULL) {
      continue;
    }
    if(info->Is_ref_by_chi()) {
      continue;
    }
    if(info->Is_ref_by_var() && !sp_h->Orig_stname()) {
      sp_h->Set_orig_stname(info->Glob_name());
    }
    sp_h->Append_data(info->Ref_sr(), info->Ctx_dna(),
                      info->Is_ref_by_var() ? PATHINFO_COPY : PATHINFO_CALL_CHI);

    if(Check_class_init_cycle(check_dna, check_cls, check_sr, info,
                              ref_mgr, cross_class, visited_dna, sp_h)) {
      ret = TRUE;
    }
  }
  sp_h->Reset_cur_node(cur_node, parent_idx);
  return ret;
}

// =============================================================================
//
// Builtin_certj_dcl00: Prevent class initialization cycles
// Iterate all clinit function's glboal reference, check if any cycles among
// the globals.
//
// =============================================================================
void
RBC_BASE::Builtin_certj_dcl00(IPSA *ipsa, DNA_NODE *dna)
{
  MEM_POOL pool;
  OPT_POOL_Initialize(&pool, "Class init cycle pool", FALSE, VSA_DUMP_FLAG);
  OPT_POOL_Push(&pool, VSA_DUMP_FLAG);

  Is_Trace(Tracing(), 
           (TFile, "RBC_BASE::Builtin_certj_dcl00: check class init cycle\n"));
  // Step 1: collect all clinit functions' global ref info
  GLOB_REF_MGR *glob_mgr = CXX_NEW(GLOB_REF_MGR(&pool), &pool);
  for (DNODE_ITER<DNA_TRAV_PRE_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    dna = iter.Current();
    if(dna->Non_functional() || glob_mgr->Get_ref_list(dna) != NULL) {
      continue;
    }
    const char *fname = dna->Fname();
    STRING_BUFFER buf(strlen(fname) + 1);
    const char *fun_sig = CLASS_HIERARCHY::Extract_fun_sig(fname, &buf);
    // skip non-clinit function
    if(fun_sig == NULL || strcmp(fun_sig, CLINIT) != 0) {
      continue;
    }
    dna->Collect_global_ref_info(glob_mgr, &pool);
  }

  Is_Trace_cmd(Tracing(), glob_mgr->Print(ipsa, TFile));

  // Step 2: check global ref cycle
  DNA_GLOB_REF_MAP *ref_map = glob_mgr->Ref_map();
  for(DNA_GLOB_REF_MAP::iterator dna_iter = ref_map->begin();
      dna_iter != ref_map->end(); dna_iter++) {
    DNA_NODE *check_dna = ipsa->Get_dna(dna_iter->first);
    Is_True_Ret(check_dna, ("null dna"));
    GLOB_REF_LIST *ref_list = dna_iter->second;
    const char *fname = check_dna->Fname();
    STRING_BUFFER buf(strlen(fname) + 1);
    const char *check_cls = CLASS_HIERARCHY::Extract_class_name(fname, &buf);
    // skip non-clinit, non-class, non-ref function
    if(ref_list == NULL || check_cls == NULL || strstr(fname, CLINIT) == NULL) {
      continue;
    }
    for(GLOB_REF_LIST::iterator ref_iter = ref_list->begin();
        ref_iter != ref_list->end(); ref_iter++) {
      GLOB_REF_INFO *info = *ref_iter;
      if(info->Ref_dna() == NULL) {
        continue;
      }
      // skip global store
      if(info->Is_ref_by_chi()) {
        continue;
      }
      Is_Trace(Tracing(), (TFile, "----Check class %s\n", check_cls));
      CONTEXT_SWITCH ref_ctx(info->Ctx_dna());
      SRCPOS_HANDLE sp_h(check_dna, &pool);
      if(info->Is_ref_by_var()) {
        sp_h.Set_orig_stname(info->Glob_name());
      }
      sp_h.Append_data(info->Ref_sr(), check_dna, PATHINFO_VUL_SPOT);
      sp_h.Set_forward(TRUE);
      hash_set<IDTYPE> visited_dna;
      visited_dna.insert(check_dna->Dna_idx());
      Check_class_init_cycle(check_dna, check_cls, info->Ref_sr(),
                             info, glob_mgr, FALSE, &visited_dna, &sp_h);
    }
  }

  OPT_POOL_Pop(&pool, VSA_DUMP_FLAG);
  OPT_POOL_Delete(&pool, VSA_DUMP_FLAG);
}


BOOL
RBC_BASE::Has_arg_address_taken(DNA_NODE *dna)
{
  CXX_MEM_POOL sp_pool("SRCPOS temp pool", FALSE);
  BOOL ret = FALSE;
  if (dna == NULL)
    return ret;
  VSA *vsa = dna->Comp_unit()->Vsa();
  RNODE_VECTOR *clby_list = dna->Clby_list();
  for (INT i = VAR_INIT_ID; i < clby_list->size(); i++) {
    RNA_NODE *rna = (*clby_list)[i];
    if (rna == NULL)
      continue;
    DNA_NODE *caller = vsa->Ipsa()->Get_dna(rna->Caller_idx());
    VNODE_VECTOR *arg_list = rna->Arg_list();
    for (INT j = VAR_INIT_ID; j < arg_list->size(); j++) {
      VAR_NODE *vnd = (*arg_list)[j];
      if (vnd != NULL) {
        CODEREP *arg = vnd->Cr();
        if (arg == NULL)
          continue;
        SRCPOS_HANDLE srcpos_h(dna, sp_pool());
        srcpos_h.Append_data(dna->St(), NULL, dna, PATHINFO_ST_DECLARE);
        srcpos_h.Append_data(rna->Callstmt(), caller, PATHINFO_DNA_CALLSITE);
        ST_IDX parm_stidx = dna->Get_param_stidx(j);
        if (parm_stidx != ST_IDX_ZERO) {
          srcpos_h.Set_orig_stname(ST_name(parm_stidx));
        }
        CONTEXT_SWITCH caller_ctx(caller);
        srcpos_h.Path()->Push_mark(caller->Comp_unit());
        VAR_DEF_HELPER helper(arg, rna->Callstmt(), caller->Comp_unit(), FOR_GENERAL, TRUE, &srcpos_h);
        helper.Set_srcpos_on(TRUE);
        helper.Set_def_srcpos_cand_on(TRUE);
        CHECK_OBJ check_obj(arg, rna->Callstmt());
        caller->Comp_unit()->Vsa()->Var_def_trav_helper(&helper, check_obj);
        DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
        for (INT k = 0; k < def_info_vec.size(); k++) {
          DEF_INFO *def_info = def_info_vec[k];
          DNA_NODE *def_dna = def_info->Dna();
          CODEREP *def_cr = def_info->Coderep();
          if (def_cr != NULL && def_cr->Kind() == CK_LDA) {
            CONTEXT_SWITCH def_ctx(def_dna);
            ST *def_st = def_cr->Lda_base_st();
            if (def_st != NULL && ST_sclass(def_st) == SCLASS_AUTO) {
              helper.Srcpos()->Reset_cur_node(def_info->Spos_node(), def_info->Spos_id());
              helper.Srcpos()->Append_data(def_st, NULL, def_dna, PATHINFO_ST_DECLARE);
              // helper.Srcpos()->Set_orig_stname(ST_name(def_st));
              Report_rbc_error(vsa, ST_Srcpos(*dna->St()), "LLAT", FALSE, helper.Srcpos());
              ret = TRUE;
              break;
            }
          }
        }
        srcpos_h.Path()->Pop_mark(caller->Comp_unit(), TRUE);
        if (ret)
          break;
      }
    }
    if (ret)
      break;
  }
  Is_Trace(Tracing(), (TFile, "RBC: Has_arg_address_taken(%s) : %d\n", dna->Fname(), ret));
  return ret;
}

BOOL
RBC_BASE::Has_set_cr(BB_NODE *bb, CODEREP *cr, VSA *vsa, SRCPOS_HANDLE *srcpos_h)
{
  BOOL ret = FALSE;
  if (bb == NULL || cr == NULL)
    return ret;
  ST *st = NULL;
  if (cr->Kind() == CK_VAR)
    st = vsa->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
  if (st == NULL || ST_level(st) != GLOBAL_SYMTAB)
    return ret;
  BB_IFINFO *ifinfo = bb->Ifinfo();
  if (ifinfo != NULL) {
    BB_NODE *merged_bb = ifinfo->Merge();
    if (merged_bb != NULL) {
      Is_Trace(Tracing(), (TFile, "RBC: Has_set_cr visiting merged BB(%d)\n", merged_bb->Id()));
      PHI_LIST *phi_list = merged_bb->Phi_list();
      PHI_NODE *phi;
      PHI_LIST_ITER phi_iter;
      FOR_ALL_ELEM(phi, phi_iter, Init(phi_list)) {
        if (phi->Live()) {
          PHI_OPND_ITER phi_opnd_iter(phi);
          CODEREP *opnd;
          FOR_ALL_ELEM(opnd, phi_opnd_iter, Init()) {
            Is_Trace(Tracing(), (TFile, "RBC: Has_set_cr verifying cr(%d)\n", opnd->Coderep_id()));
            if (cr != opnd && cr->Kind() == CK_VAR &&
                opnd->Kind() == CK_VAR && cr->Aux_id() == opnd->Aux_id() &&
                opnd->Defstmt() != NULL) {
              srcpos_h->Append_data(opnd->Defstmt(), opnd, vsa->Dna(), PATHINFO_RBC);
              Report_rbc_error(vsa, opnd->Defstmt(), "TASR", FALSE, srcpos_h);
              return TRUE;
            }
          }
        }
      }
    }
  }
  return ret;
}

BOOL
RBC_BASE::Has_cmp_cr(BB_NODE *bb, VSA *vsa)
{
  CXX_MEM_POOL spos_pool("SRCPOS temp pool", FALSE);
  BOOL ret = FALSE;
  if (bb == NULL)
    return ret;
  Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_cr visiting BB(%d)\n", bb->Id()));
  STMTREP *stmt;
  STMTREP_ITER iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, iter, Init()) {
    if (stmt->Opr() == OPR_FALSEBR || stmt->Opr() == OPR_TRUEBR) {
      CODEREP *cmp = stmt->Rhs();
      if (cmp->Kind() == CK_OP) {
        if (OPERATOR_is_compare(cmp->Opr())) {
          CODEREP *rhs = cmp->Opnd(1);
          CODEREP *lhs = cmp->Opnd(0);
          if (rhs->Kind() == CK_VAR) {
            SRCPOS_HANDLE srcpos_h(rhs, stmt, vsa->Dna(), spos_pool(), vsa);
            Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_cr found compare cr(%d)\n", rhs->Coderep_id()));
            STMTREP *defstmt = rhs->Defstmt();
            if (defstmt != NULL) {
              CODEREP *def_cr = defstmt->Rhs();
              if (def_cr != NULL && def_cr->Kind() == CK_VAR &&
                  vsa->Opt_stab()->Aux_stab_entry(def_cr->Aux_id())->Is_return_preg() &&
                  defstmt->Opr() == OPR_STID) {
                defstmt = defstmt->Next();
                if (defstmt != NULL && defstmt->Lhs() != NULL) {
                  rhs = defstmt->Lhs();
                  Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_cr switch return preg to cr(%d)\n", rhs->Coderep_id()));
                }
              }
            }
            if (Has_set_cr(bb, rhs, vsa, &srcpos_h)) {
              ret = TRUE;
            }
          }
          if (lhs->Kind() == CK_VAR) {
            SRCPOS_HANDLE srcpos_h(lhs, stmt, vsa->Dna(), spos_pool(), vsa);
            Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_cr found compare cr(%d)\n", lhs->Coderep_id()));
            STMTREP *defstmt = lhs->Defstmt();
            if (defstmt != NULL) {
              CODEREP *def_cr = defstmt->Rhs();
              if (def_cr != NULL && def_cr->Kind() == CK_VAR &&
                  vsa->Opt_stab()->Aux_stab_entry(def_cr->Aux_id())->Is_return_preg() &&
                  defstmt->Opr() == OPR_STID) {
                defstmt = defstmt->Next();
                if (defstmt != NULL && defstmt->Lhs() != NULL) {
                  lhs = defstmt->Lhs();
                  Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_cr switch return preg to cr(%d)\n", lhs->Coderep_id()));
                }
              }
            }
            if (Has_set_cr(bb, lhs, vsa, &srcpos_h)) {
              ret = TRUE;
            }
          }
        }
      }
    }
  }
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    if (Has_cmp_cr(dom_bb, vsa)) {
      ret = TRUE;
    }
  }
  return ret;
}

BOOL
RBC_BASE::Has_cmp_set_race(DNA_NODE *dna)
{
  BOOL ret = FALSE;
  if (dna == NULL)
    return ret;

  VSA *vsa = dna->Comp_unit()->Vsa();
  BB_NODE *bb = dna->Comp_unit()->Cfg()->Entry_bb();
  ret = Has_cmp_cr(bb, vsa);
  Is_Trace(Tracing(), (TFile, "RBC: Has_cmp_set_race(%s): %d\n", dna->Fname(), ret));
  return ret;
}

BOOL
RBC_BASE::Is_func_overridable(UINT32 flag)
{
  if (flag & ACC_STATIC)
    return FALSE;
  if((flag & ACC_FINAL) || (flag & ACC_PRIVATE)) {
    return FALSE;
  }
  return TRUE;
}

// =============================================================================
//
// Dump_FSMs: dump all fsms we have recognised
//
// =============================================================================
void
RBC_BASE::Dump_FSMs(FILE *fp)
{
  FSM_BASE *fsm_base;
  FB_LIST_ITER fb_list_iter;
  FOR_ALL_ELEM (fsm_base, fb_list_iter, Init(Fsm_base_list())) {
    fsm_base->Print(fp);
    fprintf(fp, "\n");
    fsm_base->Fsm()->Print(fp);
  }
}


// =============================================================================
//
// Call_stack_checks: check call stack size & level
//
// =============================================================================
void
RBC_BASE::Call_stack_checks(IPSA *ipsa, UINT sz_max, UINT l_max, DNA_NODE *dna, MEM_POOL *pool)
{
  if (sz_max == 0 && l_max == 0)
    return;
  if (ipsa == NULL)
    return;

  // bottom up traversal evaluating max value,
  // dna index starting from 1, we add one more to avoid OOB access
  UINT64 dna_count = ipsa->_post_order_dnode.size() + 1;
  // css records max call stack size of all callee,
  // csl records max call stack level of all callee,
  // indexes record rna/dna index of max value
  UINT64 *css = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  UINT64 *css_indexes = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  UINT64 *csl = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  UINT64 *csl_indexes = (UINT64*)CXX_NEW_ARRAY(UINT64, dna_count, pool);
  for (int i = 0; i < dna_count; i++) {
    css[i] = 0;
    css_indexes[i] = 0;
    csl[i] = 0;
    csl_indexes[i] = 0;
  }
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> dna_iter(ipsa); !dna_iter.Is_end(); dna_iter.Next()) {
    DNA_NODE *func = dna_iter.Current();
    if (func == NULL || func->Non_functional())
      continue;
    IDTYPE func_idx = func->Dna_idx();
    if (func_idx >= dna_count) {
      Is_Trace(Tracing(), (TFile, "RBC_BASE::Call_stack_checks: FUNC INDEX(%d) out of BOUND(%lld)\n",
                           func_idx, dna_count));
      return;
    }
    UINT64 max_css = 0;
    UINT64 max_css_index = 0;
    UINT64 max_csl = 0;
    UINT64 max_csl_index = 0;
    // find max value of "func"'s functional callee
    for (CALLEE_ITER callee_iter(ipsa, func); !callee_iter.Is_end(); callee_iter.Next()) {
      RNA_NODE *callee_rna = callee_iter.Current_callsite();
      DNA_NODE *callee = callee_iter.Current();
      if (callee != NULL && !callee->Non_functional()) {
        // all callees must have been processed in bottom up traversal
        // so their index must be fine and won't have OOB access,
        // and we don't need to check here
        IDTYPE callee_idx = callee->Dna_idx();
        UINT64 callee_css = css[callee_idx];
        if (callee_css > max_css) {
          max_css = callee_css;
          // rna & dna index, used to print out error message later
          max_css_index = (UINT64)callee_rna->Rna_idx() << 32 | (UINT64)callee_idx;
        }
        UINT64 callee_csl = csl[callee_idx];
        if (callee_csl > max_csl) {
          max_csl = callee_csl;
          max_csl_index = (UINT64)callee_rna->Rna_idx() << 32 | (UINT64)callee_idx;
        }
      }
    }
    // update max value & rna/dna index of current function
    css[func_idx] = max_css + func->Stack_size();
    css_indexes[func_idx] = max_css_index;
    csl[func_idx] = max_csl + 1;
    csl_indexes[func_idx] = max_csl_index;
  }

  // evaluate the condition and print out errors
  for (DNODE_ITER<DNA_TRAV_POST_ORDER> iter(ipsa); !iter.Is_end(); iter.Next()) {
    DNA_NODE *func = iter.Current();
    if (func == NULL || func->Non_functional())
      continue;
    // check root entry if no specific dna is given
    if (dna == NULL) {
      if (!func->Is_root_entry())
        continue;
    }
    else {
      if (dna != func)
        continue;
    }
    IDTYPE func_idx = func->Dna_idx();
    // exceed limit, generate path info from root to leaf & report error
    // "root -> foo() -> bar() -> ... -> leaf()"
    VSA *vsa = func->Comp_unit()->Vsa();
    CONTEXT_SWITCH context(func);
    char *var_name = (char*)CXX_NEW_ARRAY(BOOL, SIZE_MAX_STR, pool);
    UINT64 func_css = css[func_idx];
    UINT64 func_csl = csl[func_idx];
    if (sz_max > 0 && func_css > sz_max) {
      snprintf(var_name, SIZE_MAX_STR, "STACK SIZE:%lld", func_css);
      Is_Trace(Tracing(), (TFile, "STACK SIZE(%lld): \"%s\"", func_css, func->Fname()));
      SRCPOS_HANDLE srcpos_h(func, pool);
      // st_pos for root info
      SRCPOS st_pos = ST_Srcpos(*func->St());
      if (st_pos == 0)
        st_pos = func->Comp_unit()->Cfg()->Entry_spos();
      srcpos_h.Append_data(func->St(), NULL, func, PATHINFO_ST_DECLARE);
      srcpos_h.Set_orig_stname(var_name);
      DNA_NODE *callee = func;
      // path info for all max value callees
      while (callee != NULL && css_indexes[callee->Dna_idx()] != 0) {
        IDTYPE rna_idx = (IDTYPE)(css_indexes[callee->Dna_idx()] >> 32 & 0xffffffff);
        RNA_NODE *callee_rna = ipsa->Get_rna(rna_idx);
        if (callee_rna == NULL)
          break;
        srcpos_h.Append_data(callee_rna->Callstmt(), callee, PATHINFO_DNA_CALLSITE);
        IDTYPE dna_idx = (IDTYPE)(css_indexes[callee->Dna_idx()] & 0xffffffff);
        callee = ipsa->Get_dna(dna_idx);
        if (callee == NULL)
          break;
        Is_Trace(Tracing(), (TFile, " -> \"%s\"", callee->Fname()));
      }
      Is_Trace(Tracing(), (TFile, "\n"));
      Report_rbc_error(vsa, st_pos, "CSS", FALSE, &srcpos_h);
    }
    if (l_max > 0 && func_csl > l_max) {
      snprintf(var_name, SIZE_MAX_STR, "CALL DEPTH:%lld", func_csl);
      Is_Trace(Tracing(), (TFile, "CALL DEPTH(%lld): \"%s\"", func_csl, func->Fname()));
      SRCPOS_HANDLE srcpos_h(func, pool);
      // st_pos for root info
      SRCPOS st_pos = ST_Srcpos(*func->St());
      if (st_pos == 0)
        st_pos = func->Comp_unit()->Cfg()->Entry_spos();
      srcpos_h.Append_data(func->St(), NULL, func, PATHINFO_ST_DECLARE);
      srcpos_h.Set_orig_stname(var_name);
      DNA_NODE *callee = func;
      // path info for all max value callees
      while (callee != NULL && csl_indexes[callee->Dna_idx()] != 0) {
        IDTYPE rna_idx = (IDTYPE)(csl_indexes[callee->Dna_idx()] >> 32 & 0xffffffff);
        RNA_NODE *callee_rna = ipsa->Get_rna(rna_idx);
        if (callee_rna == NULL)
          break;
        srcpos_h.Append_data(callee_rna->Callstmt(), callee, PATHINFO_DNA_CALLSITE);
        IDTYPE dna_idx = (IDTYPE)(csl_indexes[callee->Dna_idx()] & 0xffffffff);
        callee = ipsa->Get_dna(dna_idx);
        if (callee == NULL)
          break;
        Is_Trace(Tracing(), (TFile, " -> \"%s\"", callee->Fname()));
      }
      Is_Trace(Tracing(), (TFile, "\n"));
      Report_rbc_error(vsa, st_pos, "CSL", FALSE, &srcpos_h);
    }
  }
  Is_Trace(Tracing(), (TFile, "RBC_BASE::Call_stack_checks: DNA COUNT(%lld)\n", dna_count));
}
