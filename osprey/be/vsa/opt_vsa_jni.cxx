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

#include "opt_cfg.h"
#include "opt_vsa.h"
#include "opt_vsa_util.h"
#include "opt_vsa_jni.h"
#include "opt_vsa_rbc.h"
#include "opt_vsa_checker.h"
#include "opt_vsa_var_def.h"

JNI_FUNC_MAP JNI_CHECKER_HELPER::_jni_func_map;

void
JNI_CHECKER_HELPER::Init_func_map()
{
  _jni_func_map["GetObjectField"] = &JNI_CHECKER_HELPER::Check_jni_get_object_field;
  _jni_func_map["SetObjectField"] = &JNI_CHECKER_HELPER::Check_jni_set_object_field;
  _jni_func_map["GetFieldID"]     = &JNI_CHECKER_HELPER::Check_jni_get_field_id;
  _jni_func_map["GetObjectClass"] = &JNI_CHECKER_HELPER::Check_jni_get_object_class;
}

JNI_FUNC
JNI_CHECKER_HELPER::Jni_fun(char *fun_name)
{
  if(_jni_func_map.find(fun_name) != _jni_func_map.end()) {
    return _jni_func_map[fun_name];
  } else {
    return NULL;
  }
}

CHECKER_STATUS
JNI_CHECKER_HELPER::Check_jni(CHECK_OBJ &obj)
{
  if(_jni_func_map.size() == 0) {
    Init_func_map();
  }
  STMTREP *stmt = obj.Stmtrep();
  if (!(stmt->Opr() == OPR_CALL && OPERATOR_has_sym(stmt->Opr()))) {
    return CS_DONE;
  }

  char *fun_name = ST_name(stmt->St());
  JNI_FUNC func = Jni_fun(fun_name);
  Is_Trace(Ctx().Tracing(), (TFile, "JNI: calling %s: %s\n", fun_name, func ? "found" : "not found"));
  if(func) {
    return (this->*func)(obj);
  } else {
    return CS_DONE;
  }
}

CHECKER_STATUS
JNI_CHECKER_HELPER::Check_jni_set_object_field(CHECK_OBJ &obj)
{
  STMTREP *stmt = obj.Stmtrep();
  CODEREP *rhs = stmt->Rhs();
  Is_True(rhs->Kid_count() == 4, ("invalid opnd cnt"));
  if(rhs->Kid_count() != 4) {
    return CS_DONE;
  }
  CODEREP *cr = rhs->Opnd(3)->Ilod_base();
  obj.Update_var(cr, stmt);
  return CS_OP;
}

CHECKER_STATUS
JNI_CHECKER_HELPER::Check_jni_get_object_field(CHECK_OBJ &obj)
{
  STMTREP *stmt = obj.Stmtrep();
  MU_LIST *mu_list = Ctx().Vsa()->Stmt_vor_mu(stmt);
  MU_LIST_ITER mu_iter;
  MU_NODE *mnode;

  FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
    CVOR *cvor = (CVOR*)mnode->OPND();
    VSYM_OBJ_REP *vor = cvor->first;
    CODEREP *cr = cvor->second;
    if(cr == stmt->Rhs()->Opnd(1)->Ilod_base()) {
      obj.Update_vsym(vor, stmt, cr);
      Ctx().Tracker()->Push(vor->Vsym_obj()->Fld_rep_ptr());
      return CS_VSYM_UD;
    }
  }
  return CS_DONE;
}

CHECKER_STATUS
JNI_CHECKER_HELPER::Check_jni_get_field_id(CHECK_OBJ &obj)
{
  STMTREP *stmt = obj.Stmtrep();
  obj.Update_var(stmt->Rhs(), stmt);
  return CS_DONE;
}

CHECKER_STATUS
JNI_CHECKER_HELPER::Check_jni_get_object_class(CHECK_OBJ &obj)
{
  STMTREP *stmt = obj.Stmtrep();
  Is_True(stmt->Rhs()->Kid_count() == 2, ("invalid args"));
  if(stmt->Rhs()->Kid_count() == 2) {
    obj.Update_var(stmt->Rhs()->Opnd(1)->Ilod_base());
    return CS_OP;
  }
  return CS_DONE;
}

// =============================================================================
//
// VSA::Find_fld_name: find the fld name define
//
// =============================================================================
VS_FLD_KIND
VSA::Find_fld_name(RNA_NODE *rna, UINT &fld_id)
{
  if(!Ipsa()->Is_jni_call(rna)) {
    return FLD_INVALID;
  }
  VS_FLD_KIND fld_kind = FLD_INVALID;
  CODEREP *fld_cr = NULL;
  for (int i = VAR_INIT_ID; i <= rna->Arg_cnt(); ++i) {
    if (rna->Is_set_arg_flag(i, REF_FLDNM)) {
      fld_cr = rna->Get_arg(i);
      break;
    }
  }
  Is_True(fld_cr, ("didn't find REF_FLDNM"));
  if(!fld_cr) {
    return fld_kind;
  }
  // TODO: create a <fld_cr_id, vfr_id> map in vsa local pool, 
  // search vfr id first before do UD-traversal
  Is_Trace(Tracing(), (TFile, "%s", DBar));
  Is_Trace(Tracing(), (TFile, "Find field name by U-D\n"));
  Is_Trace(Tracing(), (TFile, "%s", DBar));
  VAR_DEF_HELPER helper(fld_cr, rna->Callstmt(), Comp_unit(), FOR_FLD_NAME);
  CHECK_OBJ check_obj(fld_cr, rna->Callstmt());
  Var_def_trav_helper(&helper, check_obj);
  CODEREP *local_def = helper.Local_def();
  Is_True(local_def, ("no local define found"));
  if(!local_def) {
    return fld_kind;
  }
  INT32 cr_id = local_def->Coderep_id();
  vector<const char *> fld_names;
  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  for (int i=0; i< def_info_vec.size(); i++) {
    DNA_NODE *dna = def_info_vec[i]->Dna();
    CODEREP *def_cr = def_info_vec[i]->Coderep();
    CONTEXT_SWITCH c(dna);
    if(def_cr->Kind() == CK_LDA) {
      IDTYPE fld_name_id = TCON_str_idx(ST_tcon_val(def_cr->Lda_base_st()));
      char *cand_fld_name = Tcon_str_ptr(dna->File_idx(), fld_name_id);
      if(fld_names.empty()) {
        fld_names.push_back(cand_fld_name);
        fld_kind = FLD_K_FLD_UNIQ;
      } else {
        for(int j = 0; j < fld_names.size(); j++) {
          if(strcmp(cand_fld_name, fld_names[j]) != 0) {
            fld_names.push_back(cand_fld_name);
            fld_kind = FLD_K_FLD_MULTI;
          }
        }
      }
    }
  }
  if(fld_names.empty()) {
    fld_kind = FLD_K_UNKNOWN;
    fld_names.push_back("*");
  }
  fld_id = Create_or_get_fld_id(fld_kind, fld_names, cr_id); 
  return fld_kind;
}

// =============================================================================
//
// VSA::Create_or_get_fld_id: Get an id for the fld name
// the <fld_name, id> map is stored in IPSA::_fld_name_id_map
// For kind FLD_K_UNIQ-> use FLD_NAME as the key to get/create id
// For kind FLD_K_MULTI-> use FLD_NAME1|FLD_NAME2|FLD_NAME... as the key
// FOR kind FLD_K_UNKNOWN-> use "*:coderep id" as the key, because we want to
// distinguish if the unkown flds are same based on local top define cr
//
// =============================================================================
IDTYPE
VSA::Create_or_get_fld_id(VS_FLD_KIND fld_kind, vector<const char *> &fld_names, UINT32 cr_id)
{
  UINT32 MAX_FLD_NAME_LEN = 1024;
  char *fld_rep = (char*)malloc(MAX_FLD_NAME_LEN);
  memset(fld_rep, 0, MAX_FLD_NAME_LEN);
  for(int i  =0; i < fld_names.size(); i++) {
    if(i != 0) {
      strcat(fld_rep, "|");
    }
    strcat(fld_rep, fld_names[i]);
  }
  if(fld_kind == FLD_K_UNKNOWN) {
    Is_True(fld_names.size() == 1, ("invalid fld names for unkown kind"));
    sprintf(fld_rep + strlen(fld_rep), ":%d", cr_id);
  }
  Is_True(strlen(fld_rep) < MAX_FLD_NAME_LEN, ("fld name out of bounds"));
  if(strlen(fld_rep) >= MAX_FLD_NAME_LEN) {
    free(fld_rep);
    return FLD_ID_INVALID;
  }
  IDTYPE id = Ipsa()->Fld_name_2_id(fld_rep);
  if(id != FLD_ID_INVALID) {
    return id;
  } else {
    return Ipsa()->Enter_fld_name(fld_rep);
  }
  free(fld_rep);
}


// =============================================================================
//
// VSA::Resolve_vsym_fld_name, resolve the fld name to fld id by traverse 
// U-D of class cr's type
// Step 1: Find vfrs on stmt mu and chi, add the vfrs with field name kind to worklist
// Step 2: for each vfr traverse the UD to find fld class info, unify the class/field/field_id
//         info and make a map to  <VSYM_FLD_REP *, VSYM_FLD_ANNOT *>
//
// =============================================================================
void
VSA::Resolve_vsym_fld_name(STMTREP *stmt, CODEREP *obj_cr, CODEREP *fld_cr)
{
  Is_Trace(Tracing(), (TFile, "%s", DBar));
  Is_Trace(Tracing(), (TFile, "VSA::Resolve_vsym_fld_name:\n"));
  Is_Trace_cmd(Tracing(), Print_sr(stmt, TFile));
  typedef std::set<VSYM_FLD_REP *> VFR_SET;
  VFR_SET vfrs;

  // Step 1: Find vfrs on stmt mu and chi
  // find vor mu on stmt with obj_cr
  if (Stmt_vor_mu(stmt) && !Stmt_vor_mu(stmt)->Is_Empty()) {
    MU_NODE *mnode;
    MU_LIST_ITER mu_iter;
    FOR_ALL_NODE(mnode, mu_iter, Init(Stmt_vor_mu(stmt))) {
      CVOR *cvor = (CVOR*)mnode->OPND();
      VSYM_FLD_REP *vfr = cvor->first->Vsym_obj()->Fld_rep_ptr();
      // only try to resolve FLD_UNIQ or FLD_MULTI
      if(cvor->second == obj_cr &&
         vfr->Kind() != FLD_K_ID &&
         vfr->Kind() != FLD_K_UNKNOWN) {
        vfrs.insert(vfr);
      }
    }
  }

  // find vor chi on stmt with obj_cr
  if (Stmt_vor_chi(stmt) && !Stmt_vor_chi(stmt)->Is_Empty()) {
    CHI_NODE *cnode;
    CHI_LIST_ITER chi_iter;
    FOR_ALL_NODE(cnode, chi_iter, Init(Stmt_vor_chi(stmt))) {
      CVOR *cvor = (CVOR*)cnode->RESULT();
      VSYM_FLD_REP *vfr = cvor->first->Vsym_obj()->Fld_rep_ptr();
      // only try to resolve FLD_UNIQ or FLD_MULTI
      if (cvor->second == obj_cr &&
         vfr->Kind() != FLD_K_ID &&
         vfr->Kind() != FLD_K_UNKNOWN) {
        vfrs.insert(vfr);
      }
    }
  }

  // Step2: for each vfr traverse the UD to find
  // fld class info, unify the class/field/field_id
  // info and make a map to 
  // <VSYM_FLD_REP *, VSYM_FLD_ANNOT *>
  VFR_SET::iterator it;
  VFR_SET::iterator it_end = vfrs.end();
  for(it = vfrs.begin(); it != it_end; it++) {
    VSYM_FLD_REP *vfr = (VSYM_FLD_REP*)*it;
    // skip if already generated
    if(Vfr_2_cand_map(vfr) != NULL) {
      continue;
    }

    Is_Trace(Tracing(), (TFile, "Handle vfr:"));
    Is_Trace_cmd(Tracing(), vfr->Print(Ipsa(), TFile));
    vector<char *> fld_names;
    vfr->Get_fld_names(Ipsa(), fld_names);
    Is_True(!fld_names.empty(), ("invalid field name"));
    if(fld_names.empty()) {
      // set kind to unkown if no name found
      vfr->Set_kind(FLD_K_UNKNOWN);
      continue;
    }

    // Step2.1: traverse U-D get class info
    DEF_TYS def_types;
    Find_cr_def_types(stmt, fld_cr, def_types, FOR_FLD_CLASS);

    // Step 2.2: unify class_name/fld_name/fld_id

    // Set Fld kind to FLD_K_UNKNOWN for below:
    // 1. multiple class and multiple fld
    // candidates, no idea of which <class, fld> combinations is valid
    // 2. no class type found? should we?
    if(def_types.size() == 0 || (def_types.size() > 1 && fld_names.size() > 1)) {
      vfr->Set_kind(FLD_K_UNKNOWN);
      Is_Trace(Tracing(), (TFile, "->FLD_K_UNKNOWN\n"));
      continue;
    }

    // two level loop to iterate all possbile candidates
    // -1: not set, 0: not uniq, >0: uniq
    INT32 uniq_fld_id = -1;
    for(int i = 0; i < def_types.size(); i++) {
      DNA_NODE *dna = def_types[i].first;
      TY_IDX ty_idx = def_types[i].second;
      CONTEXT_SWITCH c(dna);
      for(int j = 0; j < fld_names.size(); j++) {
        char *fld_name = fld_names[j];
        IDTYPE field_id = 0;
        FLD_HANDLE handle = FLD_get_to_field_name(ty_idx, fld_name, field_id);
        if(!handle.Is_Null()) {
          Enter_vfr_cand_map(vfr, TY_name(ty_idx), fld_name, field_id);

          Is_Trace(Tracing(), (TFile, "CAND:[%s-%s-%d] ",
            TY_name(ty_idx), fld_name, field_id));
          if(uniq_fld_id == -1) {
            uniq_fld_id = field_id;
          } else if(uniq_fld_id != field_id) {
            uniq_fld_id = 0;
          }
        } else {
          // Report VUL with field name not match with class?
        }
      }
    }
    
    if(uniq_fld_id > 0) {
      vfr->Set_kind(FLD_K_CLS_FLD_UNIQ);
      vfr->Set_fld_id(uniq_fld_id);
      Is_Trace(Tracing(), (TFile, "->FLD_K_CLS_FLD_UNIQ\n"));
    } else if(uniq_fld_id == 0) {
      vfr->Set_kind(FLD_K_CLS_FLD_MULTI);
      Is_Trace(Tracing(), (TFile, "->FLD_K_CLS_FLD_MULTI\n"));
    }

    // free memory
    for (int fld_it = 0; fld_it < fld_names.size(); fld_it++) {
      free(fld_names[fld_it]);
    }
    fld_names.clear();
  }

  Is_Trace(Tracing(), (TFile, "End of VSA::Resolve_vsym_fld_name\n"));
  Is_Trace(Tracing(), (TFile, "%s", DBar));
}


// =============================================================================
//
// VSA::Find_cr_def_types, find the cr's possible defined codereps
//
// =============================================================================
void
VSA::Find_cr_def_types(STMTREP *stmt, CODEREP *cr, DEF_TYS &def_types, VAR_DEF_KIND kind) const
{
  Is_True_Ret(kind != FOR_CONTAINER_EVAL, ("VSA::Find_cr_def_types: The kind of VAR_DEF_HELPER can't be FOR_CONTAINER_EVAL."));
  std::set<char *> cls_names;
  VAR_DEF_HELPER helper(cr, stmt, Comp_unit(), kind);
  CHECK_OBJ check_obj(cr, stmt);
  Var_def_trav_helper(&helper, check_obj);

  DEF_INFO_VEC &def_info_vec = helper.Def_info_vec();
  for(DEF_INFO_VEC::iterator it = def_info_vec.begin(); it != def_info_vec.end(); it++) {
    CODEREP *def_cr = (*it)->Coderep();
    DNA_NODE *def_dna = (*it)->Dna();
    CONTEXT_SWITCH c(def_dna);
    TY_IDX def_ty_idx = 0;
    if(def_cr->Kind() == CK_LDA) {
      ST *st = def_cr->Lda_base_st();
      if(st && ST_is_class_symbol(st)) {
        def_ty_idx = ST_class_symbol_ty_idx(ST_st_idx(st));
      }
    } else if(def_cr->Kind() == CK_VAR) {
      AUX_STAB_ENTRY *sym = def_dna->Comp_unit()->Opt_stab()->Aux_stab_entry(def_cr->Aux_id());
      if(sym && sym->Is_global() && ST_is_class_symbol(sym->St())) {
        def_ty_idx = ST_class_symbol_ty_idx(ST_st_idx(sym->St()));
      } else if(TY_kind(def_cr->Lod_ty()) == KIND_POINTER) {
        def_ty_idx = TY_pointed(def_cr->Lod_ty());
      }
    }
    if(def_ty_idx != 0) {
      if(cls_names.find(TY_name(def_ty_idx)) == cls_names.end()) {
         cls_names.insert(TY_name(def_ty_idx));
         def_types.push_back(std::make_pair(def_dna, def_ty_idx));
         Is_Trace(Tracing(), (TFile, "Find class name: %s\n", TY_name(def_ty_idx)));
      }
    }
  }  // end of for(def_crs)
  Is_True(cls_names.size() == def_types.size(), ("size not match"));
}
