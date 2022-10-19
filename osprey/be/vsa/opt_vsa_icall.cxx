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

// ====================================================================
// ====================================================================
//
// Module: opt_vsa_icall.cxx
//
// ====================================================================
//

// ====================================================================
// U-D traversal for icall promotion and devirtualization
//   Traverse U-D to find the real call target for indirect call and
// virtual call
//
// Algorithm:
//   Start from the last kid of ICALL, check if it's indirect call or
// virtual call, traverse U-D from the last kid (for indirect call) or
// first kid (this, for virtual call) to find out where they were
// assigned or created
//
// ====================================================================

#include "defs.h"
#include "erglob.h"
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vsa_util.h"
#include "opt_vsa_eh.h"
#include "opt_vsa.h"
#include "opt_dna.h"
#include "opt_addr_util.h"
#include "opt_vsa_rbc.h"

#include "opt_vsa_checker.h"
#include "class_hierarchy.h"
#include "j_class_hierarchy_bldr.h"

enum ICALL_KIND {
  IK_NONE,        // none
  IK_INDIRECT,    // indirect call
  IK_VIRTUAL,     // virtual call
  IK_INTERFACE,   // interface call
};

class ICALL_TARGET_FINDER {
private:
  MEM_POOL              *_loc_pool;           // local pool
  ICALL_TARGET_VECTOR   &_targets;            // all possible targets
  STMTREP               *_stmt;               // original ICALL stmt
  char                  *_base_ty_name;       // interface name
  INT32                  _ofst;               // offset to vtable or interface method table
  INT32                  _vptr_ofst;          // offset of vptr to this for C++ multi-inheritance
  ICALL_KIND             _kind;               // for indirect call or virtual call
  NAME_SET              *_type_cache;         // cache the de-virtualized type

  BOOL Check_ancestor(char *ty_name, CLASS_HIERARCHY *cha)
  {
    CLASS_INFO *info = cha->Get_class_info(ty_name);
    if (info == NULL) {
      return FALSE;
    }
    if (_base_ty_name == NULL || _base_ty_name[0] == '\0')
      return FALSE;

    if (_kind == IK_VIRTUAL) {
      if(strcmp(ty_name, _base_ty_name) == 0) {
        // same class return true
        return TRUE;
      }
      C_STR_VEC *parents = info->Get_parents();
      C_STR_VEC_ITER iter;
      for (iter = parents->begin(); iter != parents->end(); ++iter) {
        if (strcmp(*iter, _base_ty_name) == 0)
          return TRUE;
      }
    } else if (_kind == IK_INTERFACE) {
      CLASS_INFO *current_info = info;
      do {
        JAVA_AUX_INFO *aux_info = current_info->Get_aux_info();
        if (aux_info) {
          C_STR_VEC *interfaces = aux_info->Get_interfaces();
          C_STR_VEC_ITER iter;
          for (iter = interfaces->begin(); iter != interfaces->end(); ++iter) {
            if (strcmp(*iter, _base_ty_name) == 0)
              return TRUE;
          }
          C_STR_VEC *parents = current_info->Get_parents();
          if (parents->size() >= 1)
            current_info = cha->Get_class_info(parents->at(0));
          else
            break;
        }
      } while (current_info);
    }
    return FALSE;
  }

  BOOL Add_vir_inf_target(TRAV_CONTEXT* ctx, STR_IDX class_name_idx)
  {
    CLASS_HIERARCHY *cha = ctx->Ipsa()->Glob_cha();
    Is_True(cha, ("class hierarchy is empty"));
    IDTYPE file_idx = ctx->Dna()->File_idx();
    char *ty_name = Str_ptr(file_idx, class_name_idx);
    if (_type_cache->find(ty_name) != _type_cache->end())
      return TRUE;
    else {
      _type_cache->insert(ty_name);
    }
    if (!Check_ancestor(ty_name, cha)) {
      Is_Trace(ctx->Tracing(), 
        (TFile, "Check ancestor failed, target class %s is not ancestor for class %s.", 
        _base_ty_name == NULL ? "" : _base_ty_name, ty_name));
      return FALSE;
    }
    VIRFUNC_INFO *virtf = NULL;
    if(_kind == IK_VIRTUAL)
      virtf = cha->Get_vtable_entry(ty_name, _ofst, _vptr_ofst);
    else if(_kind == IK_INTERFACE) {
      virtf = cha->Get_interface_entry(ty_name, _base_ty_name, _ofst);
    } else
      Is_True(FALSE, ("ICL:Illegal kind"));

    if(virtf != NULL) {
      ST *fun_st = St_ptr(virtf->_file_idx, virtf->_fun_st);
      Is_True(ST_class(fun_st) == CLASS_FUNC, 
              ("ICL: found virtual fun st is not a function symbol"));
      Add_target(virtf->_file_idx, virtf->_fun_st);
      Is_Trace(ctx->Tracing(), (TFile, "##ICL: Add target %s found in %s.\n",
                                    Str_ptr(virtf->_file_idx, ST_name_idx(fun_st)), ctx->Dna()->Fname()));
      Is_Trace(ctx->Tracing(), (TFile, "  in stmt:\n"));
      return TRUE;
    } else {
      Is_Trace(ctx->Tracing(), (TFile, "ICL: unable to find icall function for class %s at ofst %d\n", ty_name, _ofst));
      return FALSE;
    }
  }

  void Add_initv_target(UINT32 file_idx, INITV_IDX initv_idx, INT64 ofst)
  {
    INITV_IDX n_initv_idx;
    BOOL found = Get_initv_from_offset(file_idx, initv_idx, ofst, &n_initv_idx);
    if (found) {
      INITV n_initv = Initv_Table[n_initv_idx];
      if (n_initv.kind == INITVKIND_SYMOFF) {
        ST_IDX cand = INITV_st(n_initv);
        if (ST_class(cand) == CLASS_FUNC) {
          Add_target(file_idx, cand);
        }
      }
    }
  }

  void Add_st_initv_target(ST *st, INT64 ofst)
  {
    Is_True(ST_is_initialized_cross(st), ("st is not initialized"));
    // find initv
    pair<UINT32, pair<ST_IDX, INITV_IDX> > initv_info = ST_has_initv_cross(st);
    UINT32 file_idx = initv_info.first;
    ST_IDX st_idx = initv_info.second.first;
    INITV_IDX initv_idx = initv_info.second.second;
    Is_True_Ret(st_idx != ST_IDX_ZERO,
                ("ST is initialized, but can't resolve."));
    Is_True_Ret(initv_idx != INITV_IDX_ZERO,
                ("ST is initialized, but can't find initv."));
    FILE_CONTEXT_SWITCH fctx(file_idx);
    Add_initv_target(file_idx, initv_idx, ofst);
  }

  void Add_st_initv_target(TRAV_CONTEXT* ctx, STMTREP *sr, ST *st, const VSA_ADDRESS_INFO& info)
  {
    Is_True(ctx->Tracker()->Size() == 1, ("only handle 1-level vfr"));
    INT64 vfr_ofst = ctx->Tracker()->Fld_rep()->Ofst();

    Is_True(ST_is_initialized_cross(st), ("st is not initialized"));
    // find initv
    pair<UINT32, pair<ST_IDX, INITV_IDX> > initv_info = ST_has_initv_cross(st);
    UINT32 file_idx = initv_info.first;
    ST_IDX st_idx = initv_info.second.first;
    INITV_IDX initv_idx = initv_info.second.second;
    Is_True_Ret(st_idx != ST_IDX_ZERO, ("ST is initialized, but can't resolve."));
    Is_True_Ret(initv_idx != INITV_IDX_ZERO, ("ST is initialized, but can't find initv."));

    // switch context
    FILE_CONTEXT_SWITCH fctx(file_idx);

    // check if array element access
    if (info.Pos_index().size() == 1) {
      CODEREP *index = info.Pos_index().front().first;
      CODEREP *scale = info.Pos_index().front().second;
      Is_True(index != NULL, ("index is null"));
      INT64 scale_val = (scale && scale->Kind() == CK_CONST) ? scale->Const_val()
                                                             : 1;
      // create a var_def_helper to find definition of index
      VAR_DEF_HELPER hlp(index, sr, ctx->Comp_unit(), FOR_GENERAL, ctx->Tracing());
      // clone ctx into helper so that only defs on current call stack is checked
      hlp.Ctx().Clone_context(ctx, TRUE, FALSE, FALSE);
      CHECK_OBJ index_obj(index, sr);
      ctx->Vsa()->Var_def_trav_helper(&hlp, index_obj);
      // check elements in def_info_vec
      DEF_INFO_VEC &def_info_vec = hlp.Def_info_vec();
      for (INT i=0; i< def_info_vec.size(); i++) {
        DNA_NODE *dna = def_info_vec[i]->Dna();
        CODEREP *def_cr = def_info_vec[i]->Coderep();
        Is_True(dna != NULL && def_cr != NULL, ("invalid def dna or cr"));
        if (def_cr->Kind() == CK_CONST) {
          INT64 ofst = def_cr->Const_val();
          if (scale_val > 1)
            ofst *= scale_val;
          ofst += info.Fix_ofst() + vfr_ofst;
          Add_initv_target(file_idx, initv_idx, ofst);
        }
      }
    }
    else {
      INT64 ofst = info.Fix_ofst() + vfr_ofst;
      Add_initv_target(file_idx, initv_idx, ofst);
    }
  }

public:
  // traversal from dereference
  enum { SUSPECT = CS_ICALL | CS_VPTR | CS_ILOD_VPTR | CS_VSYM_OBJ };
  // need to check coderep
  enum { ENTITY = TE_CODEREP };
  // need to track USE_SRCPOS
  enum { USE_SRCPOS = FALSE };
  // TODO: need to follow eh path?
  enum { FOLLOW_EH = FALSE };

public:
  // ICALL_TARGET_FINDER
  // Constructor
  ICALL_TARGET_FINDER(TRAV_CONTEXT& ctx, STMTREP *stmt, ICALL_TARGET_VECTOR& targets, MEM_POOL *pool)
   : _targets(targets), _stmt(stmt), _base_ty_name(NULL), _ofst(0), _vptr_ofst(0), _kind(IK_NONE), _loc_pool(pool) {
    _type_cache = CXX_NEW(
      NAME_SET(DEFAULT_HASH_TABLE_SIZE, __gnu_cxx::hash<NAME>(), 
      equal_str(), NAME_ALLOCATOR(_loc_pool)), _loc_pool
    );
    ctx.Set_Tracing(Get_Trace(TP_CHECKER, CHK_ICALL_TRACE_FLAG));
  }

public:
  // Checker_name
  // return the name of the checker
  const char* Checker_name() const { return "ICL"; }

  SRCPOS_HANDLE* Srcpos_handle() const { return NULL; }

  BOOL Set_check_kind(CHECKER_SUSPECT suspect) { return FALSE; }
public:
  // Check_coderep
  // call back to check coderep
  template<CODEKIND  _KIND> CHECKER_STATUS
  Check_coderep(CHECK_OBJ& obj, TRAV_CONTEXT* ctx);

  // Check_stmtrep
  // call back to check stmtrep
  template<OPERATOR _OPR> CHECKER_STATUS
  Check_stmtrep(CHECK_OBJ& obj, TRAV_CONTEXT* ctx);

  // Check_heap_obj
  CHECKER_STATUS
  Check_heap_obj(CHECK_OBJ& obj, TRAV_CONTEXT* ctx) { return CS_CONT; }

  // Check_vsym_obj
  CHECKER_STATUS
  Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx);

  // Add_target
  // check and add (file_idx, st) into target if it's not added before
  void Add_target(UINT32 file_idx, ST_IDX idx)
  {
    for (ICALL_TARGET_VECTOR::iterator iter = _targets.begin();
         iter != _targets.end(); ++ iter) {
      if (iter->first == file_idx && iter->second == idx)
        return;
    }
    _targets.push_back(std::make_pair(file_idx, idx));
  }
};

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_LDA>
//   Check LDA and add st to targets
// ====================================================================
template<> CHECKER_STATUS inline
ICALL_TARGET_FINDER::Check_coderep<CK_LDA>(CHECK_OBJ& obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_LDA, ("not lda"));
  ST* st = ctx->Opt_stab()->Aux_stab_entry(cr->Lda_aux_id())->St();
  if (st == NULL) {
    Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with null st.\n"));
    return CS_DONE;
  }
  if (!ctx->Tracker()->Empty()) {
    if (ctx->Tracker()->Size() == 1) {
      ST *st = cr->Lda_base_st();
      Is_True(st != NULL, ("invalid lda base st"));
      if (ST_level(st) == GLOBAL_SYMTAB &&
          ST_is_initialized_cross(st)) {
        INT64 ofst = cr->Offset() + ctx->Tracker()->Fld_rep()->Ofst();
        Add_st_initv_target(st, ofst);
        Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with LDA st.\n"));
      }
      else {
        Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with Local LDA w/o VOR.\n"));
      }
    }
    else {
      Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with LDA tracker not empty\n"));
    }
    return CS_DONE;
  }

  switch(_kind) {
    case IK_INDIRECT:
      if (TY_kind(ST_type(st)) != KIND_FUNCTION) {
        Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with non-function LDA.\n"));
        return CS_DONE;
      }
      Add_target(ctx->Dna()->File_idx(), ST_st_idx(st));
      Is_Trace(ctx->Tracing(), (TFile, "##ICL: Add target %s found in %s.\n",
                                      ST_name(st), ctx->Dna()->Fname()));
      Is_Trace(ctx->Tracing(), (TFile, "  in stmt:\n"));
      break;
    case IK_VIRTUAL:
    case IK_INTERFACE:
      if (ST_is_class_symbol(st)) {
        TY_IDX class_ty = ST_class_symbol_ty_idx(*st);
        Add_vir_inf_target(ctx, TY_name_idx(class_ty));
      }
      break;
    default:
      break;
  }
  Is_Trace_cmd(ctx->Tracing(), obj.Stmtrep()->Print(TFile));
  return CS_DONE;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_CONST>
//   Check const coderep
// ====================================================================
template<> CHECKER_STATUS inline
ICALL_TARGET_FINDER::Check_coderep<CK_CONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_CONST, ("not const"));
  Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with const %lld.\n", obj.Coderep()->Const_val()));
  return CS_DONE;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_RCONST>
//   Check rconst coderep
// ====================================================================
template<> CHECKER_STATUS inline
ICALL_TARGET_FINDER::Check_coderep<CK_RCONST>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  Is_True(obj.Coderep()->Kind() == CK_RCONST, ("not rconst"));
  Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with rconst.\n"));
  return CS_DONE;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_OP>
//   Check op coderep
// ====================================================================
template<> CHECKER_STATUS inline
ICALL_TARGET_FINDER::Check_coderep<CK_OP>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  SRCPOS cur_line = sr ? sr->Linenum() : 0 ;
  Is_True(cr->Kind() == CK_OP, ("not op"));
  if (cr->Opr() == OPR_ICALL) {
    Is_True(_kind == IK_NONE, ("bad kind"));
    Is_True(sr->Opr() == OPR_ICALL && sr->Rhs() == cr, ("bad sr"));
    INT32 flags = sr->Call_flags();
    if (flags & WN_CALL_IS_VIRTUAL) {
      Is_Trace(ctx->Tracing(), (TFile, "##ICL: Find virtual call candidate at line %d:\n", SRCPOS_linenum(cur_line)));
      _kind = IK_VIRTUAL;
      TY_IDX ty_idx;
      if (Get_icall_info(cr, ty_idx)) {
        _base_ty_name = TY_name(ty_idx);
      } else {
        Is_Trace(ctx->Tracing(), (TFile, "###ICL: Unable to get virtual call base type info."));
      }

      CODEREP *this_cr = cr->Opnd(0);
      CODEREP *call_addr = cr->Opnd(cr->Kid_count() - 1);
      // icall return value passed by parameter, get this from operand 1
      if (TY_return_to_param(sr->Ty())) {
        Is_True(cr->Kid_count() >= 3, ("###ICL: Invalid opnd count for ICALL with return param"));
        this_cr = cr->Opnd(1);
      }
      if (PU_java_lang(Get_Current_PU())) {
        Is_True_Ret(Is_valid_lookup_virt_op(call_addr),
                    ("##ICL: Invalid java icall expression"), CS_DONE);
        // java icall addr saved in lookup virtual intrinsic operand 0
        call_addr = call_addr->Opnd(0)->Ilod_base();
      }

      Is_True_Ret(call_addr->Kind() == CK_IVAR &&
                  call_addr->Ilod_base()->Kind() == CK_IVAR,
                  ("##ICL: ICALL unsupported address"), CS_DONE);
      _ofst = call_addr->Offset();

      Is_True_Ret(this_cr->Opr() == OPR_PARM, ("###ICL this cr is not param"), CS_DONE);
      cr = this_cr->Ilod_base();

      if (cr->Kind() == CK_IVAR)
        _vptr_ofst = cr->Offset();
    }
    else if(flags & WN_CALL_IS_INTERFACE) {
      Is_Trace(ctx->Tracing(), (TFile, "##ICL: Find interface call candidate at line %d:\n", SRCPOS_linenum(cur_line)));
      _kind = IK_INTERFACE;
      TY_IDX ty_idx = TY_IDX_ZERO;
      if(Get_intrfc_call_info(cr, _ofst, ty_idx)) {
        _base_ty_name = TY_name(ty_idx);
      } else {
        Is_Trace(ctx->Tracing(), (TFile, "##ICL: Unable to get interface info\n"));
      }
      cr = cr->Opnd(0)->Ilod_base();
    }
    else {
      _kind = IK_INDIRECT;
      Is_Trace(ctx->Tracing(), (TFile, "##ICL: Find indirect call candidate at line %d:\n", SRCPOS_linenum(cur_line)));
      //     LDID parm0
      //   PARM
      //     LDID parm1
      //   PARM
      //   ......
      //       LDID base ptr
      //     ILOAD offset
      //   PARM
      // ICALL
      // the last parm is function pointer
      cr = cr->Opnd(cr->Kid_count() - 1);
    }
    if (!_base_ty_name) {
      _base_ty_name = (char *) MEM_POOL_Alloc(_loc_pool, 1);
      _base_ty_name[0] = '\0';
    }
    Is_Trace_cmd(ctx->Tracing(), sr->Print(TFile));
  }

  Is_True(_kind == IK_VIRTUAL || _kind == IK_INDIRECT || _kind == IK_INTERFACE,
          ("bad kind"));
  VSA_ADDRESS_INFO info;
  if (ctx->Comp_unit()->Analyze_pointer_info(sr, cr, &info, FALSE) == FALSE ||
      info.Base() == NULL) {
    Is_Trace(ctx->Tracing(), (TFile, "##ICL: Done with base NULL.\n"));
    return CS_DONE;
  }
  CODEREP *base = info.Base();
  // continue with the base UD
  if (base->Kind() == CK_LDA) {
    if (ctx->Tracker()->Empty()) {
      // no vsym, check LDA directly
      obj.Update_var(base);
      return Check_coderep<CK_LDA>(obj, ctx);
    }
    ST* st = ctx->Opt_stab()->Aux_stab_entry(base->Lda_aux_id())->St();
    if (st == NULL) {
      Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with null st.\n"));
      return CS_DONE;
    }
    Is_True(TY_kind(ST_type(st)) == KIND_ARRAY ||
            TY_kind(ST_type(st)) == KIND_STRUCT, ("not aggregate type"));
    if (sr != NULL) {
      HEAP_OBJ_REP* hor = ctx->Vsa()->Cr_2_heap_obj(base);
      Is_True(hor != NULL, ("null hor"));
      BOOL maybe = FALSE;
      VSYM_OBJ_REP* vor = ctx->Tracker()->Compress(ctx->Vsa(), hor, sr, cr, TRUE, maybe);
      if (vor != NULL) {
        // Is_True(FALSE, ("base hor's def also defs the vor?"));
        obj.Update_vsym(vor, sr, cr);
        return CS_VSYM_UD;
      }
      // search mu on sr?
    }
    // search global init
    if (ST_level(st) == GLOBAL_SYMTAB &&
        ST_is_initialized_cross(st)) {
      if (ctx->Tracker()->Size() > 1) {
        // TODO: support more than 1-level vsym obj
        Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with LDA with multi-level VFR.\n"));
        return CS_DONE;
      }

      if (info.Pos_index().size() > 1 ||
          (info.Neg_index().size() +
           info.Pos_offset().size() +
           info.Neg_offset().size()) != 0) {
        Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with complex address expr.\n"));
        return CS_DONE;
      }

      Add_st_initv_target(ctx, sr, st, info);
      Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with LDA st.\n"));
      return CS_DONE;
    }
    else {
      // Local LDA without VO?
      Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with Local LDA w/o VOR.\n"));
      return CS_DONE;
    }
  }
  // only VAR and IVAR is allowed
  Is_True(base->Kind() == CK_VAR || base->Kind() == CK_IVAR, ("bad base"));
  obj.Update_var(base);
  return base->Kind() == CK_VAR ? CS_VAR_UD : CS_IVAR_UD;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_VAR>
//   Check var coderep to find icall target
// ====================================================================
template<> CHECKER_STATUS
ICALL_TARGET_FINDER::Check_coderep<CK_VAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  Is_True(cr->Kind() == CK_VAR, ("not var"));

  if (cr->Is_flag_set(CF_IS_ZERO_VERSION)) {
    Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with zero-version.\n"));
    return CS_DONE;
  }

  if (cr->Aux_id() == ctx->Opt_stab()->Default_vsym() ||
      cr->Aux_id() == ctx->Opt_stab()->Return_vsym()) {
    Is_Trace(ctx->Tracing(), (TFile, "--ICL: Done with def/ret vsym.\n"));
    return CS_DONE;
  }

  ST* st = ctx->Comp_unit()->Opt_stab()->St(cr->Aux_id());
  // for extern globals like java.system.stdout, stderr, get from type
  if (st && ST_sclass(st) == SCLASS_EXTERN && Vsa_check_var_no_sideffect(ST_name(st))) {
    TY_IDX ty = ST_type(st);
    if(TY_kind(ty) == KIND_POINTER) {
      ty = TY_pointed(ty);
    }
    if(TY_kind(ty) == KIND_STRUCT) {
      Add_vir_inf_target(ctx, TY_name_idx(ty));
    }
    return CS_DONE;
  }

  // continue with the VAR UD
  return CS_VAR_UD;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_coderep<CK_IVAR>
//   Check ivar coderep to find icall target
// ====================================================================
template<> CHECKER_STATUS
ICALL_TARGET_FINDER::Check_coderep<CK_IVAR>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  CODEREP *cr = obj.Coderep();
  STMTREP *sr = obj.Stmtrep();
  Is_True(cr->Kind() == CK_IVAR, ("not ivar"));
  if (cr->Opr() == OPR_PARM) {
    Is_True(FALSE, ("hit parm"));
    obj.Update_var(cr->Opnd(0));
    return CS_OP;
  }

  Is_True(_kind == IK_VIRTUAL || _kind == IK_INDIRECT || _kind == IK_INTERFACE, ("bad kind"));
  // continue with the IVAR vsym
  Is_Trace(ctx->Tracing(), (TFile, " -ICL: Check ivar U-D:\n"));
  Is_Trace_cmd(ctx->Tracing(), sr->Print(TFile));
  return CS_IVAR_UD;
}

template<> CHECKER_STATUS
ICALL_TARGET_FINDER::Check_stmtrep<OPR_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Callee_returns_new_heap_memory(), ("stmt is should allocate memory"));
  if ((PU_src_lang(Get_Current_PU()) & PU_CXX_LANG)) {
    CODEREP *call_return_store = ctx->Comp_unit()->Find_return_value(sr);
    STMTREP *next_stmt = NULL;
    if(call_return_store) {
      next_stmt = sr->Next();
      if(next_stmt != NULL && next_stmt->Opr() == OPR_STID &&
         next_stmt->Rhs() == call_return_store) {
        CODEREP *lhs = next_stmt->Lhs();
        ID_ST_MAP *vtbl_map = ctx->Dna()->Vtbl_map();
        if(vtbl_map->find(lhs->Coderep_id()) != vtbl_map->end()) {
          ST_IDX& vtbl_st = (*vtbl_map)[lhs->Coderep_id()];
          Is_True(ST_is_vtable(&St_Table[vtbl_st]), ("find vfun: st is not vtable"));
          if (ST_is_vtable(&St_Table[vtbl_st])) {
            TY_IDX class_ty = ST_vtable_ty_idx(vtbl_st);
            Add_vir_inf_target(ctx, TY_name_idx(class_ty));
          }
        }
        else {
          Is_Trace(ctx->Tracing(), (TFile, ("C++ failed to find vfunc")));
        }
      }
    }
    // TODO, search the next stmt for ISTORE to object at ofst 0 or constructor
  }
  Is_Trace_cmd(ctx->Tracing(), sr->Print(TFile));
  return CS_DONE;
}

// ====================================================================
// ICALL_TARGET_FINDER::Check_stmtrep<OPR_INTRINSIC_CALL>
//   Check NPD if stmtrep is INTRINSIC_CALL
// ====================================================================
template<> CHECKER_STATUS
ICALL_TARGET_FINDER::Check_stmtrep<OPR_INTRINSIC_CALL>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  STMTREP *sr = obj.Stmtrep();
  Is_True(sr->Opr() == OPR_INTRINSIC_CALL, ("not intrinsic call"));
  INTRINSIC intrn = sr->Rhs()->Intrinsic();
  if (obj.Is_var()) {
    CODEREP *cr = obj.Coderep();
    if (intrn == INTRN_CHECK_CAST) {
      CODEREP* rhs = sr->Rhs();
      Is_True(rhs->Kid_count() == 2, ("bad rhs kids"));
      Is_True(rhs->Opnd(1)->Kind() == CK_IVAR && rhs->Opnd(1)->Opr() == OPR_PARM, ("bad first kid"));
      obj.Update_var(rhs->Opnd(1)->Ilod_base());
      return CS_OP;
    } else if(intrn == INTRN_ALLOC_OBJ) {
      CODEREP *rhs = sr->Rhs();
      Is_True(rhs->Kid_count() == 1, ("bad rhs kids"));
      Is_True(rhs->Opnd(0)->Kind() == CK_IVAR && rhs->Opnd(0)->Opr() == OPR_PARM, ("bad first kid"));
      obj.Update_var(rhs->Opnd(0)->Ilod_base());
      return CS_OP;
    } else if(intrn == INTRN_INIT_CLASS) {
      CHI_NODE *cnode;
      CHI_LIST_ITER chi_iter;
      FOR_ALL_NODE(cnode, chi_iter, Init(sr->Chi_list())) {
        if (cnode->Live() && cnode->RESULT() == cr) {
          obj.Update_var(cnode->OPND());
          return CS_OP;
        }
      }
    }
  }
  // TODO: other intrinsics
  return CS_DONE;
}

template<> CHECKER_STATUS
ICALL_TARGET_FINDER::Check_stmtrep<OPR_OPT_CHI>(CHECK_OBJ &obj, TRAV_CONTEXT* ctx)
{
  return CS_CONT;
}

CHECKER_STATUS
ICALL_TARGET_FINDER::Check_vsym_obj(CHECK_OBJ &obj, TRAV_CONTEXT *ctx)
{
  if (!PU_cxx_lang(Get_Current_PU()) && !PU_c_lang(Get_Current_PU())) {
    return CS_CONT;
  }
  VSYM_OBJ_REP *vor = obj.Vor();
  Is_True_Ret(vor != NULL && !ctx->Vsa()->Is_special_vor(vor),
              ("bad vor"), CS_DONE);
  if (vor->Attr() != ROR_DEF_BY_CHI)
    return CS_CONT;
  if (vor->Stmt_def() != NULL)
    return CS_CONT;

  // try to read from global init if vor has no def
  HEAP_OBJ_REP *base_hor = vor->Vsym_obj()->Base_hor();
  if (base_hor->Attr() == ROR_DEF_BY_LDA) {
    CODEREP *base_cr = base_hor->Heap_obj()->Ho_cr();
    Is_True_Ret(base_cr && base_cr->Kind() == CK_LDA, ("null LDA ho cr"), CS_CONT);
    AUX_STAB_ENTRY *sym = ctx->Opt_stab()->Aux_stab_entry(base_cr->Lda_aux_id());
    Is_Trace_cmd(ctx->Tracing(), ctx->Opt_stab()->Print(TFile, NULL));
    if (sym && sym->Is_global() && obj.Stmtrep()) {
      Is_Trace_cmd(ctx->Tracing(), ctx->Vsa()->Print_sr(obj.Stmtrep(), TFile));
      VSYM_FLD_REP zero_fld(FLD_K_ID, 0, 0);
      MU_NODE *mu = ctx->Vsa()->Find_stmt_var_mu(obj.Stmtrep(), base_cr->Lda_base_st(), &zero_fld);
      if (mu) {
        CODEREP *glob_cr = mu->OPND();
        CODEREP *value_const = NULL;
        std::pair<UINT32, ST_IDX> symbol_st(0, ST_IDX_ZERO);
        if (ctx->Initialized_const_value(glob_cr, &value_const, symbol_st) &&
            symbol_st.second != ST_IDX_ZERO) {
          ST *fun_st = St_ptr(symbol_st.first, symbol_st.second);
          if (ST_class(fun_st) == CLASS_FUNC) {
            Is_Trace(ctx->Tracing(), (TFile, "##ICL: Add target %s\n", ST_name(symbol_st.first, symbol_st.second)));
            Add_target(symbol_st.first, symbol_st.second);
          } else {
            Is_Trace(ctx->Tracing(), (TFile, "##ICL: Skip target non-function symbol %s\n", ST_name(symbol_st.first, symbol_st.second)));
          }
        }
      }
    }
  }
  return CS_DONE;
}

// ====================================================================
// VSA::Find_icall_defs_new
//   Find icall targets by U-D traversal
// ====================================================================
BOOL
VSA::Find_icall_defs_new(STMTREP* sr, ICALL_TARGET_VECTOR& targets)
{
  Is_True(sr->Opr() == OPR_ICALL, ("bad sr"));
  COMP_UNIT* cu = Comp_unit();
  OPT_POOL_Push(_cu->Loc_pool(), -1);
  {
    TRAV_CONTEXT ctx(_cu, sr, sr->Rhs());
    ICALL_TARGET_FINDER finder(ctx, sr, targets, _cu->Loc_pool());
    CHECK_OBJ obj(sr->Rhs(), sr);
    CHECKER_STATUS sts = finder.Check_coderep<CK_OP>(obj, &ctx);
    if (sts != CS_DONE) {
      UD_TRAVELER<ICALL_TARGET_FINDER> helper(finder, ctx);
      helper.Continue_trav(obj, sts);
    }
    Is_True(ctx.Frame_empty(), ("call stack corrupted"));
  }
  OPT_POOL_Pop(_cu->Loc_pool(), -1);
  if(targets.size() > 0)
    return TRUE;
  else
    return FALSE;
}

// ====================================================================
// VSA::Find_registered_function
//   Find registered entry by U-D traversal
// ====================================================================
BOOL
VSA::Find_registered_function(STMTREP *sr, CODEREP *cr, ICALL_TARGET_VECTOR& targets)
{
  Is_True(sr && sr->Opr() == OPR_CALL, ("not call stmt"));
  Is_True(cr && (cr->Kind() == CK_VAR ||
                 (cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM)),
          ("bad func ptr cr"));

  COMP_UNIT* cu = Comp_unit();
  OPT_POOL_Push(_cu->Loc_pool(), -1);
  {
    TRAV_CONTEXT ctx(cu, sr, cr);
    ICALL_TARGET_FINDER finder(ctx, sr, targets, cu->Loc_pool());
    CHECK_OBJ obj(cr, sr);
    CHECKER_STATUS sts = (cr->Kind() == CK_VAR)
                           ? finder.Check_coderep<CK_VAR>(obj, &ctx)
                           : finder.Check_coderep<CK_IVAR>(obj, &ctx);
    if (sts != CS_DONE) {
      UD_TRAVELER<ICALL_TARGET_FINDER> helper(finder, ctx);
      helper.Continue_trav(obj, sts);
    }
    Is_True(ctx.Frame_empty(), ("call stack corrupted"));
  }
  OPT_POOL_Pop(cu->Loc_pool(), -1);
  return targets.size() > 0 ? TRUE : FALSE;
}

