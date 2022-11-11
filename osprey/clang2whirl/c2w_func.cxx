/*
  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#include <clang/AST/VTableBuilder.h>
#include "c2w_func.h"
#include "c2w_builder.h"
#include "c2w_stmt.h"
#include "c2w_verifier.h"
#include "c2w_tracer.h"
#include "c2w_expr.h"
#include "c2w_utils.h"

// clang header files
#include "clanginc.h"

using namespace clang;

// open64 header files
#include "open64inc.h"

static int _tcf_name_suffix = 0;
extern std::set<const clang::VarDecl *> _deferred_global_var_decls;
static int _ctor_name_suffix = 1;

namespace wgen {

WhirlFuncBuilder::WhirlFuncBuilder(WhirlBuilder *builder)
  : _builder(builder) {
  MEM_POOL_Initialize(&_local_pool, "FUNC local pool", FALSE);
  MEM_POOL_Push(&_local_pool);
  Is_True(Current_Map_Tab == NULL,
          ("Current_Map_Tab already initialized?"));
  Clear_call_region(); // clear call region
}

WhirlFuncBuilder::~WhirlFuncBuilder() {
  DestroyLocalSymtab();
  
  if (Current_Map_Tab) {
    WN_MAP_TAB_Delete(Current_Map_Tab);
    Current_Map_Tab = NULL;
  }
  
  MEM_POOL_Pop(&_local_pool);
  MEM_POOL_Delete(&_local_pool);
}

PU_Info *
WhirlFuncBuilder::CreatePUInfo(ST_IDX st_idx) {
  PU_Info *pu_info = CXX_NEW(PU_Info, Malloc_Mem_Pool);
  BZERO(pu_info, sizeof(PU_Info));
  Is_True(pu_info != NULL,
          ("Malloc PU_Info failed. Out of memory?"));
  
  WN_MAP_TAB_Create(&_local_pool);
  Is_True(Current_Map_Tab != NULL,
          ("Current_Map_Tab not initialized?"));
  
  PU_Info_init(pu_info);
  PU_Info_maptab(pu_info) = Current_Map_Tab;
  PU_Info_proc_sym (pu_info) = st_idx;
  
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_DEPGRAPH, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_PREFETCH, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_REGIONS, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_FEEDBACK, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_FREQ, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_AC_INTERNAL, Subsect_Missing);
  Set_PU_Info_state (pu_info, WT_ALIAS_CLASS, Subsect_Missing);
  
  return pu_info;
}

void
WhirlFuncBuilder::CreateLocalSymtab(ST_IDX st_idx) {
  Is_True(_builder->Scope().CurrentSymtab() == CURRENT_SYMTAB + 1,
          ("invalid symtab"));
  New_Scope(_builder->Scope().CurrentSymtab(), Malloc_Mem_Pool, TRUE);
  Scope_tab[CURRENT_SYMTAB].st = ST_ptr(st_idx);
}

void
WhirlFuncBuilder::DestroyLocalSymtab() {
  Is_True(_builder->Scope().CurrentSymtab() == CURRENT_SYMTAB,
          ("invalid symtab"));
  if(CURRENT_SYMTAB != GLOBAL_SYMTAB) {
    _builder->SB().RemoveLocalSymbol(CURRENT_SYMTAB);
    Delete_Scope(CURRENT_SYMTAB);
    CURRENT_SYMTAB--;
  }
}

// Exception Handling
// Contents of the array set up below:
// exc_ptr ST_IDX, filter ST_IDX, typeinfo INITO_IDX, eh_spec INITO_IDX
static void
Setup_entry_for_eh(void)
{
  const int lbnd = 0;
  const int hbnd = 3;

  ARB_HANDLE arb = New_ARB();
  ARB_Init(arb, lbnd, hbnd, 4);
  Set_ARB_flags(arb, ARB_flags(arb) | ARB_FIRST_DIMEN | ARB_LAST_DIMEN);
  STR_IDX str = Save_Str("__EH_INFO_PER_PU__");
  TY_IDX ty;
  TY_Init(New_TY(ty), (hbnd+1) * 4 , KIND_ARRAY, MTYPE_M, str); /// put attention here
  Set_TY_arb(ty, arb);
  Set_TY_etype(ty, MTYPE_TO_TY_array[MTYPE_U4]);
  ST *etable = New_ST(CURRENT_SYMTAB);
  ST_Init(etable, str, CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty);
  Set_ST_is_initialized(*etable);
  Set_ST_one_per_pu(etable);

  ST *exc_ptr_st = New_ST(CURRENT_SYMTAB);
  ST_Init(exc_ptr_st, Save_Str("__Exc_Ptr__"), CLASS_VAR, SCLASS_AUTO,
                      EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));
  Set_ST_one_per_pu(exc_ptr_st);
  INITV_IDX exc_ptr_iv = New_INITV();
  INITV_Set_VAL(Initv_Table[exc_ptr_iv], Enter_tcon(Host_To_Targ(MTYPE_U4,
                                           ST_st_idx(exc_ptr_st))), 1);

  ST *filter_st = New_ST(CURRENT_SYMTAB);

  ST_Init(filter_st, Save_Str("__Exc_Filter__"), CLASS_VAR, SCLASS_AUTO,
                     EXPORT_LOCAL, MTYPE_To_TY(Pointer_Mtype));

  Set_ST_one_per_pu(filter_st);
  INITV_IDX filter_iv = New_INITV();

  INITV_Set_VAL(Initv_Table[filter_iv], Enter_tcon(Host_To_Targ(MTYPE_U4,
                                          ST_st_idx(filter_st))), 1);
  Set_INITV_next(exc_ptr_iv, filter_iv);

  // this will be filled in later if there are type-filter entries
  INITV_IDX tinfo = New_INITV();
  INITV_Set_VAL(Initv_Table[tinfo], Enter_tcon(Host_To_Targ(MTYPE_U4, 0)), 1);
  Set_INITV_next(filter_iv, tinfo);

  // this will be filled in later if there are exception specifications
  INITV_IDX eh_spec = New_INITV();
  INITV_Set_VAL(Initv_Table[eh_spec], Enter_tcon(Host_To_Targ(MTYPE_U4, 0)), 1);
  Set_INITV_next(tinfo, eh_spec);
  Set_PU_misc_info(Get_Current_PU(),
                    New_INITO(ST_st_idx(etable), exc_ptr_iv));
}

WN *
WhirlFuncBuilder::EmitDestructorCall(const CXXDestructorDecl *decl,
                                       CXXDtorType dtor, UINT32 field_id)
{
  // add destructor to deferred list if not handled before
  GlobalDecl gd(decl, dtor);
  ST_IDX st = _builder->Get_func_st(gd);
  Is_True(st, ("bad dtor st"));

  // create call to dtor
  WN *ret = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
  WN_st_idx(ret) = st;
  WN_Set_Call_Default_Flags(ret);
  WN_Set_Call_Is_Destructor(ret);
  ST_IDX this_st = _builder->Scope().Get_this();
  TY_IDX this_ty = ST_type(this_st);
  Is_True(this_st, ("'this' symbol must have created"));
  Result dest = Result::nwSym(this_st, this_ty);
  dest.SetRef();
  dest.SetFieldId(field_id);
  WN *ld_wn = dest.GetLValue();
  WN_kid0(ret) = WGEN_CreateParm(WN_rtype(ld_wn), ld_wn, this_ty);
  WN_set_kid_count(ret, 1);

  if (emit_exceptions) {
    WhirlStmtBuilder stmt_bldr(_builder);
    ret = stmt_bldr.Setup_eh_region(ret, GetSrcPos(),
                                    false /* for_unwinding */,
                                    true /* is_dtor_or_ctor_call */);
  }
  return ret;
}

#if 0
static bool
Is_base(const CXXRecordDecl *base, const CXXRecordDecl *derived) {
  base = base->getCanonicalDecl();
  for (const CXXBaseSpecifier &base_decl : derived->bases()) {
    const CXXRecordDecl *tmp_base = base_decl.getType()->getAsCXXRecordDecl()->getCanonicalDecl();
    if (tmp_base == base)
      return true;
  }
  return false;
}
#endif

const CXXBaseSpecifier*
Find_base(const CXXRecordDecl *base, const CXXRecordDecl *derived) {
  base = base->getCanonicalDecl();
  for (const CXXBaseSpecifier &base_decl : derived->bases()) {
    const CXXRecordDecl *tmp_base = base_decl.getType()->getAsCXXRecordDecl()->getCanonicalDecl();
    if (tmp_base == base)
      return &base_decl;
#if 0
    else if (const CXXBaseSpecifier *tmp_decl = Find_base(base, tmp_base)) {
      if ((const Decl*)tmp_decl->getType()->getAsCXXRecordDecl()->getCanonicalDecl() == base)
      return tmp_decl;
    }
#endif
  }
  return NULL;
}

const CXXBaseSpecifier*
Find_vbase(const CXXRecordDecl *base, const CXXRecordDecl *derived) {
  base = base->getCanonicalDecl();
  for (const CXXBaseSpecifier &base_decl : derived->vbases()) {
    const CXXRecordDecl *tmp_base = base_decl.getType()->getAsCXXRecordDecl()->getCanonicalDecl();
    if (tmp_base == base)
      return &base_decl;
  }
  return NULL;
}

PU_Info *
WhirlFuncBuilder::EmitCXXGlobalInitialization(ST_IDX st_idx, int priority_size) {
  // function type
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);
  // void return type
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_V);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(ty_idx, tylist_idx);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
  // get pu st name
  SourceManager &src_mgr = _builder->Context()->getSourceManager();
  std::string file_name = src_mgr.getFileEntryForID(src_mgr.getMainFileID())->getName().str();
  if (file_name.empty())
    file_name = "<null>";
  for (size_t i = 0; i < file_name.size(); ++i) {
    if (!isPreprocessingNumberBody(file_name[i]))
      file_name[i] = '_';
  }
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ty_idx = (TY_IDX) pu_idx;

  // create fun name from priority_size
  STR_IDX fun_name;
  if (priority_size != 65535) {
    std::string suffix = std::to_string(priority_size);
    suffix = std::string(6 - suffix.size(), '0') + suffix;
    fun_name = Save_Str2("_GLOBAL__I_", suffix.c_str());
  } else {
    fun_name = Save_Str2("_GLOBAL__sub_I_", file_name.c_str());
  }

  ST *global_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(global_st, fun_name, CLASS_FUNC,
          SCLASS_TEXT, EXPORT_LOCAL, ty_idx);
  ST_IDX global_st_idx = ST_st_idx(global_st);

  // set ST_IS_GLB_INIT_FUNC attr
  Set_ST_is_glb_init_func(global_st);
  
  // create INITV
  ST *ctor_st = New_ST(GLOBAL_SYMTAB);
  STR_IDX initv_name = Save_Str2i("__ctors", "_", _ctor_name_suffix++);
  ST_Init(ctor_st, initv_name, CLASS_VAR, SCLASS_FSTATIC,
          EXPORT_LOCAL, Make_Pointer_Type(pu_ty_idx));
  Set_ST_is_initialized(ctor_st);

  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff(inv, &St_Table[global_st_idx], 0);
  New_INITO(ctor_st, inv);
  bool is_ctors = true;
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&  st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (ctor_st), ST_ATTR_SECTION_NAME,
                Save_Str (is_ctors ? ".ctors" : ".dtors"));

  // create pu
  CreateLocalSymtab(global_st_idx);
  PU_Info *pu_info = CreatePUInfo(global_st_idx);
  DST_INFO_IDX func_dst = _builder->DstBuilder().CreateDstForFunc(global_st_idx, rty);
  _builder->Scope().SetCurrentDst(func_dst);
  Set_PU_Info_pu_dst(pu_info, func_dst);
  WN *pu_tree = NULL;

  if (emit_exceptions)
    Setup_entry_for_eh();

  // create entry
  WN *body = WhirlBlockUtil::nwBlock();
  pu_tree = WN_CreateEntry(0, global_st_idx, body, NULL, NULL);
  WN_Set_Linenum(pu_tree, GetSrcPos());

  // call
  WN *ret = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 2);
  WN_st_idx(ret) = st_idx;
  WN_set_kid_count(ret, 2);
  WN_Set_Call_Default_Flags(ret);

  // parm
  WN *parm_1 = WN_Intconst(MTYPE_I4, 1);
  WN_kid0(ret) = WGEN_CreateParm(WN_rtype(parm_1), parm_1,
                                 MTYPE_To_TY(MTYPE_I4));
  WN *parm_2 = WN_Intconst(MTYPE_I4, priority_size);
  WN_kid1(ret) = WGEN_CreateParm(WN_rtype(parm_2), parm_2,
                                 MTYPE_To_TY(MTYPE_I4));
  if (ret) {
    if (emit_exceptions) {
      // setup EH region
      WhirlStmtBuilder stmt_bldr(_builder);
      ret = stmt_bldr.Setup_eh_region(ret, GetSrcPos(),
                                      false /* for_unwinding */,
                                      true /* is_dtor_or_ctor_call */);
    }
    WN_INSERT_BlockLast(body, ret);
  }
  WN_INSERT_BlockLast(body, WGEN_CreateReturn(GetSrcPos()));
  WhirlBlockUtil::popCurrentBlock();
  DBG_VERIFY_WHIRL_IR(pu_tree);
  Set_PU_Info_tree_ptr (pu_info, pu_tree);
  //Set_PU_Info_pu_dst(pu_info, _builder->Scope().CurrentDst());
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);

  _builder->AddPUInfo(pu_info);

  Write_PU_Info(pu_info);     

  return pu_info;
}

PU_Info *
WhirlFuncBuilder::EmitDtorPtr(const VarDecl *decl, ST_IDX &dtor_st_idx) {
  QualType type = cast<VarDecl>(decl)->getType();

  // pu type
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);
  // return type & param type
  TYLIST tylist_idx;
  TY_IDX rty = MTYPE_To_TY(MTYPE_V);
  Set_TYLIST_type(New_TYLIST(tylist_idx), rty);
  Set_TY_tylist(ty_idx, tylist_idx);
  TY_IDX pty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
  Set_TYLIST_type(New_TYLIST(tylist_idx), pty);
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
  // create pu st
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str2i("__tcf", "_", _tcf_name_suffix++), CLASS_FUNC,
           SCLASS_TEXT, EXPORT_LOCAL, ty_idx);
  dtor_st_idx = ST_st_idx(st);
  Set_ST_Srcpos(*st, GetSrcPos());

  // create pu info
  CreateLocalSymtab(dtor_st_idx);
  PU_Info *tcf_pu_info = CreatePUInfo(dtor_st_idx);

  if (emit_exceptions)
    Setup_entry_for_eh();

  // create pu dst
  DST_INFO_IDX func_dst = _builder->DstBuilder().CreateDstForFunc(dtor_st_idx, rty);
  _builder->Scope().SetCurrentDst(func_dst);
  Set_PU_Info_pu_dst(tcf_pu_info, func_dst);
  WN *pu_tree = NULL;
  WN *body = WhirlBlockUtil::nwBlock();
  pu_tree = WN_CreateEntry(1, dtor_st_idx, body, NULL, NULL);
  WN_Set_Linenum(pu_tree, GetSrcPos());
  // create ptr st
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  Is_True(symtab > GLOBAL_SYMTAB,
          ("invalid scope for function param"));
  ST *ptr = New_ST(symtab);
  ST_Init(ptr, Save_Str(".anon"), CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, pty);
  Set_ST_is_value_parm(ptr);
  Set_ST_Srcpos(*ptr, GetSrcPos());
  WN_formal(pu_tree, 0) = WN_CreateIdname(0, ST_st_idx(ptr));
  // body
  ST_IDX var_st = _builder->SB().GetST(decl);
  Is_True(var_st, ("bad var st"));

  WhirlStmtBuilder stmt_bldr(_builder);
  WN *dtor_call = stmt_bldr.Emit_cxx_destructor_call(type, var_st);
  WN_INSERT_BlockLast(body, dtor_call);

  WN_INSERT_BlockLast(body, WGEN_CreateReturn(GetSrcPos()));
  WhirlBlockUtil::popCurrentBlock();
  DBG_VERIFY_WHIRL_IR(pu_tree);
  Set_PU_Info_tree_ptr (tcf_pu_info, pu_tree);
  //Set_PU_Info_pu_dst(tcf_pu_info, _builder->Scope().CurrentDst());
  Set_PU_Info_state (tcf_pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (tcf_pu_info, WT_TREE, Subsect_InMem);
  _builder->AddPUInfo(tcf_pu_info);
  Write_PU_Info(tcf_pu_info);
  return tcf_pu_info;
}

PU_Info *
WhirlFuncBuilder::EmitCXXGlobalVarDeclInitialization(ST_IDX st_idx) {
#if 0
  Is_True(_builder->Scope().CurrentDecl() == _builder->Context()->getTranslationUnitDecl(),
          ("invalid scope"));
#endif
  CreateLocalSymtab(st_idx);
  PU_Info *pu_info = CreatePUInfo(st_idx);
  DST_INFO_IDX func_dst = _builder->DstBuilder().CreateDstForFunc(st_idx, MTYPE_To_TY(MTYPE_V));
  _builder->Scope().SetCurrentDst(func_dst);
  Set_PU_Info_pu_dst(pu_info, func_dst);
  WN *pu_tree = NULL;

  if (emit_exceptions)
    Setup_entry_for_eh();

  // create entry
  WN *body = WN_CreateBlock();
  UINT num_of_param = 2;
  pu_tree = WN_CreateEntry(num_of_param, st_idx, body, NULL, NULL);
  WN_Set_Linenum(pu_tree, GetSrcPos());
  // create __initialize_p & __priority
  TY_IDX pty = MTYPE_To_TY(MTYPE_I4);
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  Is_True(symtab > GLOBAL_SYMTAB,
          ("invalid scope for function param"));
  ST *init_p = New_ST(symtab);
  STR_IDX init_p_str = Save_Str("__initialize_p");
  ST_Init(init_p, init_p_str, CLASS_VAR, SCLASS_FORMAL,
          EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
  Set_ST_is_value_parm(init_p);
  WN_formal(pu_tree, 0) = WN_CreateIdname(0, ST_st_idx(init_p));
  ST *priority = New_ST(symtab);
  STR_IDX priority_str = Save_Str("__priority");
  ST_Init(priority, priority_str, CLASS_VAR, SCLASS_FORMAL,
          EXPORT_LOCAL, MTYPE_To_TY(MTYPE_I4));
  Set_ST_is_value_parm(priority);
  WN_formal(pu_tree, 1) = WN_CreateIdname(0, ST_st_idx(priority));

  // create __initialize_p cond wn
  WN *block = WN_CreateBlock();
  WN *cond_wn = WN_Relational(OPR_EQ, MTYPE_I4,
                              WN_Ldid(MTYPE_I4, 0, init_p, ST_type(init_p)),
                              WN_Intconst(Boolean_type, 1));
  WN *if_wn = WN_CreateIf(cond_wn, block, WN_CreateBlock());
  WN_Set_Linenum(if_wn, ST_Srcpos(st_idx));
  WN_INSERT_BlockLast(body, if_wn);

  for (std::set<const clang::VarDecl *>::iterator it = _deferred_global_var_decls.begin();
       it != _deferred_global_var_decls.end();
       ++it) {
    WN *stmt = WhirlBlockUtil::nwBlock();

    const VarDecl *decl = *it;
    const Expr *init_expr = decl->getInit();
    SRCPOS spos = SetSrcPos(getLocation(init_expr));
    bool is_constant_array = false;
    bool has_ctor_call = false;
    bool has_dtor_call = false;

    int priority_size = 65535;
    // get priority
    if (auto *ipa = decl->getAttr<InitPriorityAttr>()) {
      priority_size = ipa->getPriority();
    }

    // handle ExprWithCleanups
    if (isa<ExprWithCleanups>(init_expr)) {
      const ExprWithCleanups *cleanups = cast<ExprWithCleanups>(init_expr);
      if (isa<CXXConstructExpr>(cleanups->getSubExpr()))
        init_expr = cleanups->getSubExpr();
      else if (isa<InitListExpr>(cleanups->getSubExpr())) {
        // struct array
        const Expr *expr = cleanups->getSubExpr();
        int num_element = 0;

        if (const CXXRecordDecl *record =
              _builder->DeclBuilder().ConvertConstantArrayType(expr->getType(),
                                                               num_element)) {
          is_constant_array = true;
          has_ctor_call = true;
          Is_True(isa<InitListExpr>(expr), ("should be InitListExpr"));
          TY_IDX ty_ele = _builder->TB().ConvertType(record->getTypeForDecl());
          TY_IDX ele_pty = Make_Pointer_Type(ty_ele);
          ST_IDX var_st = _builder->SB().GetST(decl);
          Is_True(var_st, ("bad var st"));

          ST *fst = New_ST(GLOBAL_SYMTAB);
          ST_Init(fst, Save_Str2(".anon", ST_name(var_st)),
                  CLASS_VAR, SCLASS_FSTATIC,
                  EXPORT_LOCAL, ele_pty);
          Set_ST_addr_saved(ST_ptr(var_st));
          WN *stid = WN_Stid(MTYPE_U8, 0, fst, ele_pty,
                             WN_Lda(Pointer_Mtype, 0, ST_ptr(var_st)));
          WN_INSERT_BlockLast(stmt, stid);
          WN_Set_Linenum(stid, spos);

          TYPE_ID num_mty = num_element > INT32_MAX ? MTYPE_I8 : MTYPE_I4;
          TY_IDX num_ty = MTYPE_To_TY(num_mty);
          ST *num_st = MTYPE_To_PREG(num_mty);
          PREG_NUM num_ofst = Create_Preg(num_mty, ".num");
          WN *st_wn = WN_Stid(num_mty, num_ofst, num_st, num_ty,
                              WN_Intconst(num_mty, num_element -1));
          WN_INSERT_BlockLast(stmt, st_wn);
          WN_Set_Linenum(st_wn, spos);

          //UINT num_init = cast<InitListExpr>(expr)->getNumInits();
          std::vector<const Expr*> elems;
          elems.reserve(num_element);
          _builder->DeclBuilder().CollectInitListItems(cast<InitListExpr>(expr), elems);
          Is_True(elems.size() == num_element, ("init item mismatch"));
          for (UINT i = 0; i < num_element; ++i) {
            init_expr = elems[i]; // cast<InitListExpr>(expr)->getInit(i);
            if (init_expr == NULL) {
              Is_True(FALSE, ("TODO"));
              // generate mstore with 0 to target?
              continue;
            }
            Result target = Result::nwSym(ST_st_idx(fst), ele_pty);
            target.SetDeref();
            WhirlBlockUtil::pushBlock(stmt);
            WhirlStmtBuilder bldr(_builder);
            WN *init_wn = bldr.ConvertExpr(init_expr, target);
            WhirlBlockUtil::popCurrentBlock();
            if (init_wn) {
              WN_INSERT_BlockLast(stmt, init_wn);
              WN_Set_Linenum(init_wn, spos);
            }

            WN *ld = WN_Ldid(MTYPE_U8, 0, fst, ele_pty);
            WN *add_wn = WN_Add(WN_rtype(ld), ld, WN_Intconst(MTYPE_U8, TY_size(ty_ele)));
            WN *st_wn = WN_Stid(WN_rtype(add_wn), 0, fst, ele_pty, add_wn);
            WN_INSERT_BlockLast(stmt, st_wn);
            WN_Set_Linenum(st_wn, spos);

            WN *ldid_wn = WN_Ldid(num_mty, num_ofst, num_st, num_ty);
            WN *sub_wn = WN_Sub(TY_mtype(num_ty), ldid_wn,
                                WN_Intconst(WN_rtype(ldid_wn), 1));
            st_wn = WN_Stid(num_mty, num_ofst, num_st, num_ty, sub_wn);
            WN_INSERT_BlockLast(stmt, st_wn);
            WN_Set_Linenum(st_wn, spos);
          }
        }
      }
    }

    // dtor temps created in cpnvert init_expr
    WhirlStmtBuilder stmt_bldr(_builder);
    stmt_bldr.pop_dtor_for_copy_ctor_stack(stmt);

    // ctor call
    if (!is_constant_array && isa<CXXConstructExpr>(init_expr) &&
        _builder->DeclBuilder().GetNonTrivialInitializer(init_expr)) {
      WhirlStmtBuilder stmt_bldr(_builder);
      WN *ctor_call = stmt_bldr.Emit_cxx_constructor_call(*it);
      if (ctor_call) {
        WN_INSERT_BlockLast(stmt, ctor_call);
        WN_Set_Linenum(ctor_call, spos);
      }
      has_ctor_call = true;
    }

    // dtor temps created in Emit_cxx_constructor_call
    stmt_bldr.pop_dtor_for_copy_ctor_stack(stmt);

    // handle other init expr
    if (!has_ctor_call && !isa<CXXConstructExpr>(init_expr)) {
      ST_IDX var_st = _builder->SB().GetST(decl);
      Is_True(var_st, ("bad var st"));
      Result target = Result::nwSym(var_st, ST_type(var_st));
      WhirlExprBuilder bldr(_builder);
      Result init = bldr.ConvertExpr(init_expr, target);
      if (init.isNode() || init.isSym() && init.Sym() != var_st) {
        TY_IDX var_ty = ST_type(var_st);
        WN *ldid_wn = init_expr->isGLValue() ? init.GetLValue()
                                             : init.GetRValue();
        if (isa<StringLiteral>(init_expr) && TY_kind(var_ty) == KIND_ARRAY) {
          WN *addr_wn = WN_Lda(Pointer_Mtype, 0, ST_ptr(var_st));
          Set_ST_addr_saved(ST_ptr(var_st));
          UINT len = cast<StringLiteral>(init_expr)->getByteLength();
          GenMstoreForString(addr_wn, ldid_wn, var_ty, len, 0, spos);
        } else {
          WN *init_wn;
          if (WN_operator(ldid_wn) == OPR_CALL && WN_rtype(ldid_wn) == MTYPE_V)
            init_wn = ldid_wn;
          else
            init_wn = WN_Stid(TY_mtype(var_ty), 0, ST_ptr(var_st),
                              var_ty, ldid_wn);
          WN_Set_Linenum(init_wn, spos);
          WN_INSERT_BlockLast(stmt, init_wn);
        }
      }
      // dtor temps created in cpnvert init_expr
      WhirlStmtBuilder stmt_bldr(_builder);
      stmt_bldr.pop_dtor_for_copy_ctor_stack(stmt);
    }

    // dtor call
    int num_element = 0;
    const CXXRecordDecl *record =
      _builder->DeclBuilder().ConvertConstantArrayType(decl->getType(),
                                                         num_element);
    if (record && !record->hasTrivialDestructor()) {
      ST_IDX dtor_st_idx = _builder->DeclBuilder().GetTcfST(decl);
      WN *atexit_call = Emit_dtor_call(dtor_st_idx);
      if (atexit_call) {
        WN_INSERT_BlockLast(stmt, atexit_call);
        WN_Set_Linenum(atexit_call, spos);
      }
      has_dtor_call = true;
    }

    cond_wn = WN_Relational(OPR_EQ, MTYPE_I4,
                            WN_Ldid(MTYPE_I4, 0, priority, ST_type(priority)),
                            WN_Intconst(Boolean_type, priority_size));
    WN *body_blk = WN_CreateIf(cond_wn, stmt, WN_CreateBlock());
    WN_INSERT_BlockLast(block, body_blk);
    WN_Set_Linenum(body_blk, spos);
    WhirlBlockUtil::popCurrentBlock();
  }

  DBG_VERIFY_WHIRL_IR(pu_tree);
#if 0
  Is_True(_builder->Scope().CurrentDecl() == decl,
          ("invalid scope"));
#endif
  Set_PU_Info_tree_ptr (pu_info, pu_tree);
  //Set_PU_Info_pu_dst(pu_info, _builder->Scope().CurrentDst());
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);

  _builder->AddPUInfo(pu_info);

  Write_PU_Info(pu_info); 

  return pu_info;
}

void
WhirlFuncBuilder::EmitVTableFieldInitialization(WN *blk, const CXXRecordDecl *decl) {
  ST_IDX this_st = _builder->Scope().Get_this();
  TY_IDX this_ty = TY_pointed(ST_type(this_st));
  Is_True(this_st, ("'this' symbol must have created"));
  ST_IDX vt_st = _builder->DeclBuilder().GetVTableST(decl);
  Is_True(vt_st != (ST_IDX)0, ("Invalid vtable st"));
  ASTContext *ast_c = _builder->Context();
  ItaniumVTableContext *vtable_c = cast<ItaniumVTableContext>(ast_c->getVTableContext());
  const VTableLayout &layout = vtable_c->getVTableLayout(decl);

  // prepare a vector to save the base info
  std::vector<const BaseSubobject*> base_info;
  base_info.resize(layout.getNumVTables());
  // ap's type is VTableLayout::AddressPointsMapTy::value_type, which is a pair of
  // <BaseSubobject, AddressPointLocation>
  for (auto &ap: layout.getAddressPoints()) {
    unsigned i = ap.second.VTableIndex;
    Is_True(i < layout.getNumVTables(), ("vtable index oob"));
    base_info[i] = &ap.first;
  }

  for (unsigned i = 0; i < layout.getNumVTables(); i++) {
    size_t begin = layout.getVTableOffset(i);
    size_t size = layout.getVTableSize(i);
    size_t idx = begin;
    for (unsigned j = begin; j < begin + size; j++) {
      idx++;
      if (layout.vtable_components()[j].getKind() == VTableComponent::CK_RTTI)
        break;
    }
    TYPE_ID machine_uint_ty = _builder->TB().GetUIntPtrMType();
    TY_IDX pointer_type = Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8));
    // get addr to first virtual function in vtable
    WN *lda = WN_Lda(Pointer_Mtype, 0, ST_ptr(vt_st));
    Set_ST_addr_saved(ST_ptr(vt_st));
    WN *size_wn = WN_Intconst(machine_uint_ty, idx * TY_size(pointer_type));
    WN *init_wn = WN_Add(WN_rtype(lda), lda, size_wn);
    // get this
    WN *ldid_wn = WN_Ldid(TY_mtype(ST_type(this_st)), 0, this_st, ST_type(this_st));
    // calculate field id
    const CXXBaseSpecifier* spec = Find_base(base_info[i]->getBase(), decl);
    UINT field_id = 1;  // skip the whole struct
    if (spec)
      field_id += _builder->TB().GetFieldIDFromDecl((const Decl*)spec);
    // calculate istore offset
    INT64 ofst = base_info[i]->getBaseOffset().getQuantity();
    // generate istore to set vptr
    WN* istore = WN_Istore(TY_mtype(WN_ty(ldid_wn)), base_info[i]->getBaseOffset().getQuantity(),
                           ST_type(this_st), ldid_wn, init_wn, field_id);
    WN_INSERT_BlockLast(blk, istore);
    WN_Set_Linenum(istore, GetSrcPos());
  }
}

PU_Info *
WhirlFuncBuilder::ConvertFunction(GlobalDecl gd, ST_IDX st_idx) {
  const FunctionDecl *decl = cast<FunctionDecl>(gd.getDecl());
  Is_True(_builder->Scope().CurrentDecl() == decl,
          ("invalid scope"));
//  Is_True(_builder->Lang_C(), ("TODO: C++ functions"));

  if (!decl->doesThisDeclarationHaveABody()) {
    return NULL;
  }
  // adjust export calss when we have body, the body may not generated by ast when
  // symbol created
  if(ST_sclass(ST_ptr(st_idx)) != SCLASS_TEXT) {
    Set_ST_sclass(ST_ptr(st_idx), SCLASS_TEXT);
  }

  UINT i = 0;
  bool append_this = false;
  if (_builder->Lang_CPP() && (isa<CXXConstructorDecl>(decl) ||
                               isa<CXXDestructorDecl>(decl) || 
                               (isa<CXXMethodDecl>(decl) && 
                                !(cast<CXXMethodDecl>(decl))->isStatic())))
    append_this = true;

  bool add_fake_parm = false;
  if (_builder->TB().NeedFakeParm(decl->getReturnType()))
    add_fake_parm = true;

  CreateLocalSymtab(st_idx);
  PU_Info *pu_info = CreatePUInfo(st_idx);
  Set_PU_Info_pu_dst(pu_info, _builder->Scope().CurrentDst());
  WN *pu_tree = NULL;

  if (_builder->Lang_CPP() && emit_exceptions)
    Setup_entry_for_eh();

  // get srcpos from decl
  SRCPOS srcpos = SetSrcPos(getLocation(decl));

  // create entry
  WN *body = WhirlBlockUtil::nwBlock();
  WN_Set_Linenum(body, srcpos);
  WN *pragma = WN_CreatePragma(WN_PRAGMA_PREAMBLE_END, (ST_IDX)0, 0, 0);
  WN_INSERT_BlockLast(body, pragma);
  WN_Set_Linenum(pragma, srcpos);

  UINT num_of_param = decl->getNumParams();
  if (add_fake_parm) num_of_param++;
  if (append_this) num_of_param++;
  pu_tree = WN_CreateEntry(num_of_param, st_idx, body, NULL, NULL);
  WN_Set_Linenum(pu_tree, srcpos);
  PushCurrentEntryWN(pu_tree);

  // add a fake arg0 for function needs to return the object in memory
  if (add_fake_parm) {
    TY_IDX return_ty = _builder->TB().ConvertType(decl->getReturnType());
    ST *st = New_ST();
    ST_Init(st, Save_Str2i(".arg", "", i), CLASS_VAR, SCLASS_FORMAL,
            EXPORT_LOCAL, Make_Pointer_Type(return_ty, FALSE));
    Set_ST_is_value_parm(st);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, st);
    i++;
  }

  // create st for *this in constructor & destructor
  const CXXRecordDecl *record_decl = NULL;
  ST *this_st = NULL;
  TY_IDX this_ty = TY_IDX_ZERO;
  TY_IDX this_ptr_ty = TY_IDX_ZERO;
  if (append_this) {
    if (isa<CXXConstructorDecl>(decl))
      record_decl = cast<CXXConstructorDecl>(decl)->getParent();
    else if (isa<CXXDestructorDecl>(decl))
      record_decl = cast<CXXDestructorDecl>(decl)->getParent();
    else if (isa<CXXMethodDecl>(decl))
      record_decl = cast<CXXMethodDecl>(decl)->getParent();
    this_ty = _builder->TB().ConvertType(record_decl->getTypeForDecl());
    Is_True(TY_kind(this_ty) == KIND_STRUCT, ("invalid ty"));
    if (cast<FunctionType>(decl->getType())->isConst())
      Set_TY_is_const(this_ty);
    if (cast<FunctionType>(decl->getType())->isVolatile())
      Set_TY_is_volatile(this_ty);
    this_ptr_ty = Make_Pointer_Type(this_ty);
    SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
    Is_True(symtab > GLOBAL_SYMTAB,
            ("invalid scope for function param"));
    this_st = New_ST(symtab);
    STR_IDX str_idx = Save_Str("this");
    ST_Init(this_st, str_idx, CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL, this_ptr_ty);
    Set_ST_is_value_parm(this_st);
    Set_ST_is_this_ptr(this_st);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, this_st);
    _builder->Scope().Set_this(ST_st_idx(this_st));
    i++;
  }

  // save current cap list in lambda helper. the cap list will be restored
  // in destructor
  WhirlLambdaContext ctx(_builder->LambdaHelper(),
                         record_decl && record_decl->isLambda());

  if(record_decl && record_decl->isLambda()) {
    // process capture list here
    CXXRecordDecl::field_iterator cur_fld = record_decl->field_begin();
    CXXRecordDecl::capture_const_iterator cur_cap = record_decl->captures_begin();
    for ( ; cur_fld != record_decl->field_end(); ++cur_fld, ++cur_cap) {
      if (cur_cap->capturesVariable())
        _builder->LambdaHelper().AddCapFld(cur_cap->getCapturedVar(), *cur_fld);
      else if(cur_cap->capturesThis())
        _builder->LambdaHelper().AddCapFld(record_decl, *cur_fld);
    }
    Is_True(cur_fld == record_decl->field_end() &&
            cur_cap == record_decl->captures_end(), ("Lambda Fld/Var mismatch"));
    _builder->LambdaHelper().SetLambdaPhase(IN_LAMBDA_BODY);
  }
  else {
    _builder->LambdaHelper().SetLambdaPhase(NOT_IN_LAMBDA);
  }

  // formal list
  WN_VECTOR args;
  for (i; i < num_of_param; ++i) {
    UINT real_parm = i;
    if (add_fake_parm)
      real_parm--;
    if (append_this)
      real_parm--;
    const ParmVarDecl *parm = decl->getParamDecl(real_parm);

    // If any of the arguments have a variably modified type,
    // make sure to emit the type size.
    QualType parm_type = parm->getOriginalType();
    if (parm_type->isVariablyModifiedType()) {
      _builder->EmitVariablyModifiedType(parm_type);
    }

    TY_IDX fty_idx = _builder->TB().ConvertType(parm->getType());
    ST_IDX fst_idx = _builder->SB().ConvertSymbol(parm);
    if (_builder->TB().NeedFakeParm(parm->getType())) {
      // set original param not used
      ST *orig_fst = ST_ptr(fst_idx);
      Set_ST_is_not_used(orig_fst);
      // create a new param with same name and pointer type
      fty_idx = Make_Pointer_Type(fty_idx);
      ST *tmp_st = New_ST(CURRENT_SYMTAB);
      ST_Init(tmp_st, ST_name_idx(orig_fst), CLASS_VAR, SCLASS_FORMAL,
              EXPORT_LOCAL, fty_idx);
      _builder->DeclBuilder().AddRealParmST(fst_idx, ST_st_idx(tmp_st));
      fst_idx = ST_st_idx(tmp_st);
    }
    WN_formal(pu_tree, i) = WN_CreateIdname(0, fst_idx);
    WN *ldid = WN_Ldid(TY_mtype(fty_idx), 0, ST_ptr(fst_idx), fty_idx);
    args.push_back(std::pair<WN *, TY_IDX>(ldid, fty_idx));
  }

  // call field's constructor from initializer list, which is the order of declaration
  if (append_this && isa<CXXConstructorDecl>(decl)) {
    const CXXConstructorDecl *ctor_decl = cast<CXXConstructorDecl>(decl);
    WN *ctor_ret, *init_wn, *ld_wn;
    Expr *init;
    bool need_zero_init = false;
    //bool decl_has_base = false;
    const CXXBaseSpecifier * base_spec = NULL;
    bool handle_vtable = false;
    for (const CXXCtorInitializer *I : ctor_decl->inits()) {
      // get field id
      UINT32 field_id = 0;
      if (I->isBaseInitializer()) {
        const CXXRecordDecl *base_decl = I->getBaseClass()->getAsCXXRecordDecl();
        Is_True(base_decl, ("not find base decl"));
        base_spec = Find_base(base_decl, record_decl);
        if (!base_spec)
          base_spec = Find_vbase(base_decl, record_decl);
        Is_True(base_spec, ("not find base spec"));
        field_id = _builder->TB().GetFieldIDFromDecl((const Decl*)base_spec);
        Is_True(field_id, ("wrong field id?"));
      }
      else if (I->isMemberInitializer()) {
        field_id = _builder->TB().GetFieldIDFromDecl(I->getMember());
        Is_True(field_id, ("wrong field id?"));
      }
      else if (I->isIndirectMemberInitializer()) {
        const IndirectFieldDecl *ifld = I->getIndirectMember();
        for (const NamedDecl *fld : ifld->chain()) {
          UINT32 x = _builder->TB().GetFieldIDFromDecl(cast<FieldDecl>(fld));
          field_id += x;
        }
        Is_True(field_id, ("wrong field id?"));
      }

      // create dest
      Result dest = Result::nwSym(ST_st_idx(this_st), this_ptr_ty);
      dest.SetRef();
      dest.SetFieldId(field_id);

      // handle initializer
      init = I->getInit();

      WhirlExprBuilder expr_bldr(_builder);
      // handle CXXInheritedCtorInitExpr
      if (init->getStmtClass() == Stmt::CXXInheritedCtorInitExprClass) {
        CXXInheritedCtorInitExpr *expr = cast<CXXInheritedCtorInitExpr>(init);
        // how to handle expr->inheritedFromVBase?
        // how to handle expr->constructsVBase?
        WN *call_wn = expr_bldr.EmitCXXConstructCall(expr->getConstructor(), dest,
                                                     CXXCtorType::Ctor_Base, args);
        WN_Set_Linenum(call_wn, srcpos);
        WN_INSERT_BlockLast(body, call_wn);
        continue;
      }

      // handle CXXConstructExpr
      const CXXConstructExpr *ctor_expr = NULL;
      if (init->getStmtClass() == Stmt::CXXConstructExprClass) {
        ctor_expr = cast<CXXConstructExpr>(init);
        if (ctor_expr->requiresZeroInitialization())
          need_zero_init = true;
        if (ctor_expr->getConstructor()->isDefaultConstructor() &&
            ctor_expr->getConstructor()->isTrivial() && !need_zero_init && !base_spec)
          continue;
        if (base_spec && base_spec->isVirtual() &&
            gd.getCtorType() == CXXCtorType::Ctor_Base)
          continue;
      }

      // initial vtable field
      if (!base_spec && !handle_vtable &&
          _builder->DeclBuilder().HasVTable(record_decl)) {
        EmitVTableFieldInitialization(body, record_decl);
        handle_vtable = true;
      }

#if 0
      // If record type of this declration has base, it shoud be the first field.
      UINT64 offset = 0;
      UINT32 field_id = 0;
      //TY_IDX ld_ty = this_ty;
      if (TY_size(this_ty) > 0 && /*!record_decl->field_empty() &&*/ (I->getMember() || base_spec)) {
        UINT32 cur_field_id = 0;
        if (I->getMember()) {
          field_id = _builder->TB().GetFieldIDFromDecl(I->getMember());
          Is_True(field_id, ("wrong field id?"));
        }
        else if (base_spec) {
          field_id = _builder->TB().GetFieldIDFromDecl((const Decl*)base_spec);
          Is_True(field_id, ("wrong field id?"));
        }
        Is_True(field_id != 0, ("failed to get field id"));
        FLD_HANDLE fld = get_fld_and_offset(this_ty, field_id, cur_field_id, offset);
        Is_True(!fld.Is_Null(), ("not find the field"));
      }
#endif

      // get type
      TY_IDX fty_idx = _builder->TB().ConvertType(init->getType());
      if (init->isGLValue())
        fty_idx = Make_Pointer_Type(fty_idx);
      TY_IDX ptr_ty = Make_Pointer_Type(fty_idx);
      TYPE_ID mtype = TY_mtype(fty_idx);
      // handle need_zero_init
      if (need_zero_init) {
        WN *ctor_ret = NULL;
        if (mtype == MTYPE_M) {
          UINT size = TY_size(fty_idx);
          if (size > 0) {
            ctor_ret = WN_CreateMstore(0, ptr_ty,
                                       WN_Intconst(MTYPE_I8, 0),
                                       dest.GetLValue(),
                                       WN_Intconst(MTYPE_I8, size));
          }
        }
        else {
          ctor_ret = WN_Istore(mtype, 0, ptr_ty,
                               dest.GetLValue(), Gen_null_const(fty_idx));
        }
        if (ctor_ret) {
          WN_Set_Linenum(ctor_ret, srcpos);
          WN_INSERT_BlockLast(body, ctor_ret);
        }
        continue;
      }

      // push a block for code emitted in converting sub expr
      WN *blk = WhirlBlockUtil::nwBlock();
      if (ctor_expr != NULL) {
        CXXCtorType ctor = base_spec ? CXXCtorType::Ctor_Base
                                     : CXXCtorType::Ctor_Complete;
        Result r = expr_bldr.ConvertCXXConstructExpr(ctor_expr, dest, ctor);
        Is_True(r.isNode() || r.isSym(), ("not node"));
        init_wn = r == dest ? NULL : r.GetRValue();
      }
      else {
        Result r = expr_bldr.ConvertExpr(init, dest);
        init_wn = r.isNone() || r == dest ? NULL : init->isGLValue() ? r.GetLValue() : r.GetRValue();;
      }
      // destruct temporaries
      WhirlStmtBuilder stmt_bldr(_builder);
      stmt_bldr.pop_dtor_for_copy_ctor_stack(blk);
      WhirlBlockUtil::popCurrentBlock();

      if (init_wn && OPERATOR_is_expression(WN_operator(init_wn))) {
        init_wn = WN_Istore(mtype, 0, ptr_ty,
                            dest.GetLValue(), init_wn);
        WN_Set_Linenum(init_wn, srcpos);
      }
      if (init_wn) {
        Is_True(OPERATOR_is_stmt(WN_operator(init_wn)), ("not stmt"));
        WN_INSERT_BlockLast(blk, init_wn);
      }

      if (!WN_block_empty(blk)) WN_INSERT_BlockLast(body, blk);
    }
    // initial vtable field
    if (!handle_vtable &&
        _builder->DeclBuilder().HasVTable(record_decl)) {
      EmitVTableFieldInitialization(body, record_decl);
    }
  }

#if 0
  // clean captured field map
  _builder->LambdaHelper().CleanCapFld();
  if(record_decl && record_decl->isLambda()) {
    // process capture list here
    CXXRecordDecl::field_iterator cur_fld = record_decl->field_begin();
    CXXRecordDecl::capture_const_iterator cur_cap = record_decl->captures_begin();
    for ( ; cur_fld != record_decl->field_end(); ++cur_fld, ++cur_cap) {
      if (cur_cap->capturesVariable())
        _builder->LambdaHelper().AddCapFld(cur_cap->getCapturedVar(), *cur_fld);
      else if(cur_cap->capturesThis())
        _builder->LambdaHelper().AddCapFld(record_decl, *cur_fld);
    }
    Is_True(cur_fld == record_decl->field_end() &&
            cur_cap == record_decl->captures_end(), ("Lambda Fld/Var mismatch"));
    _builder->LambdaHelper().SetLambdaPhase(IN_LAMBDA_BODY);
  }
  else {
    _builder->LambdaHelper().SetLambdaPhase(NOT_IN_LAMBDA);
  }
#endif

  // load vtable ptr if necessary
  if (append_this && isa<CXXDestructorDecl>(decl) &&
      _builder->DeclBuilder().HasVTable(record_decl)) {
    EmitVTableFieldInitialization(body, record_decl);
  }

  // expand body
  if (Stmt *stmt = decl->getBody()) {
    WhirlStmtBuilder stmt_bldr(_builder);
    WN *ret = stmt_bldr.ConvertStmt(stmt);
    if (ret) WN_INSERT_BlockLast(body, ret); //TODO
  }
  else {
    Is_True(false, ("Other kind of func body"));
  }

  // call field's destructor in the inverted order of declaration
  if (append_this && isa<CXXDestructorDecl>(decl)) {
    WN *dtor_ret = NULL;
    WN *dtor_blk = WhirlBlockUtil::nwBlock();
    const CXXRecordDecl *fld_decl = NULL;
    UINT32 field_id = 0;
    // call vbase's destructor if necessary
    if (gd.getDtorType() == CXXDtorType::Dtor_Complete) {
      for (const CXXBaseSpecifier &base: record_decl->vbases()) {
        field_id = _builder->TB().GetFieldIDFromDecl((const Decl*)&base);
        fld_decl = base.getType()->getAsCXXRecordDecl();
        if (fld_decl && fld_decl->hasNonTrivialDestructor() &&
            fld_decl->getDestructor()) {
          dtor_ret = EmitDestructorCall(fld_decl->getDestructor(),
                                        CXXDtorType::Dtor_Base, field_id);
          if (dtor_ret) WN_INSERT_BlockFirst(dtor_blk, dtor_ret);
        }
      }
    }
    // call base's destructor if necessary
    for (const CXXBaseSpecifier &base: record_decl->bases()) {
      if (base.isVirtual())
        continue;
      field_id = _builder->TB().GetFieldIDFromDecl((const Decl*)&base);
      fld_decl = base.getType()->getAsCXXRecordDecl();
      if (fld_decl && fld_decl->hasNonTrivialDestructor() &&
          fld_decl->getDestructor()) {
        dtor_ret = EmitDestructorCall(fld_decl->getDestructor(),
                                      CXXDtorType::Dtor_Base, field_id);
        if (dtor_ret) WN_INSERT_BlockFirst(dtor_blk, dtor_ret);
      }
    }
    // call field's destructor
    for (const FieldDecl *Field : record_decl->fields()) {
      if (Field->getType()->getTypeClass() != Type::Record)
        continue;
      fld_decl = Field->getType()->getAsCXXRecordDecl();
      field_id = _builder->TB().GetFieldIDFromDecl(Field);
      if (fld_decl && fld_decl->hasNonTrivialDestructor() &&
          fld_decl->getDestructor()) {
        dtor_ret = EmitDestructorCall(fld_decl->getDestructor(),
                                      CXXDtorType::Dtor_Complete, field_id);
        if (dtor_ret) WN_INSERT_BlockFirst(dtor_blk, dtor_ret);
      }
    }
    if (dtor_blk) WN_INSERT_BlockLast(body, dtor_blk);
    WhirlBlockUtil::popCurrentBlock();
  }
  // Insert a RETURN if it does not exist
  WN *wn = WN_last(body);
  if (wn == NULL || WN_operator(wn) != OPR_RETURN &&
      WN_operator(wn) != OPR_RETURN_VAL) {
    WN_INSERT_BlockLast(body,
                        WGEN_CreateReturn(SetSrcPos(getEndLocation(decl))));
  }

  if (_builder->Lang_CPP()) {
    WhirlStmtBuilder stmt_bldr(_builder);
    stmt_bldr.Do_handlers();
    if (emit_exceptions)
      stmt_bldr.Build_eh_type_info();
  }

  WhirlBlockUtil::popCurrentBlock();
  PopCurrentEntryWN();

  // reset must_inline if we have set the PU no_inline
  if (PU_no_inline(Get_Current_PU()))
    Clear_PU_must_inline(Get_Current_PU());

  Is_True(pu_tree != NULL,
          ("invalid pu tree"));
  DBG_VERIFY_WHIRL_IR(pu_tree);
  Is_True(_builder->Scope().CurrentDecl() == decl,
          ("invalid scope"));
  Set_PU_Info_tree_ptr (pu_info, pu_tree);
  //Set_PU_Info_pu_dst(pu_info, _builder->Scope().CurrentDst());
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);
  
  _builder->AddPUInfo(pu_info);
  
  Write_PU_Info(pu_info);
  
  return pu_info;
}

// adjust wn using thunk_info. adjust_this indicates
// if we want a this-pointer adjustment, or a return-result
// adjustment.
WN *
WhirlFuncBuilder::EmitAdjustThunk(WN *wn, ThunkInfo thunk_info, BOOL adjust_this) {
  Is_True(!thunk_info.isEmpty(), ("invalid ThunkInfo"));

  WN *adjust_wn = NULL;
  Is_True(WN_has_sym(wn), ("whirl node should have st"));
  ST *ptr_st = WN_st(wn);
  TY_IDX ptr_ty = ST_type(ptr_st);
  TYPE_ID ptr_mtype = TY_mtype(ptr_ty);

  bool is_virtual = FALSE;
  UINT64 ofst = 0;
  if (adjust_this) {
    is_virtual = thunk_info.This.NonVirtual ? FALSE : TRUE;
    ofst = is_virtual ?
             thunk_info.This.Virtual.Itanium.VCallOffsetOffset :
             thunk_info.This.NonVirtual;
  } else {
    is_virtual = thunk_info.Return.NonVirtual ? FALSE : TRUE;
    ofst = is_virtual ?
             thunk_info.Return.Virtual.Itanium.VBaseOffsetOffset :
             thunk_info.Return.NonVirtual;
  }

  if (!is_virtual) {
    adjust_wn = WN_Intconst(MTYPE_U8, ofst);
  } else {
    TYPE_ID uintptr_ty = _builder->TB().GetUIntPtrMType();
    WN *iload_vptr_wn = WN_Iload(uintptr_ty, 0,
                                 Make_Pointer_Type(MTYPE_To_TY(uintptr_ty)),
                                 WN_COPY_Tree(wn), 0);
    adjust_wn = WN_Iload(uintptr_ty, ofst,
                         MTYPE_To_TY(uintptr_ty),
                         iload_vptr_wn, 0);
  }
  wn = WN_Add(ptr_mtype, wn, adjust_wn);
  wn = WN_Stid(ptr_mtype, 0, ptr_st, ptr_ty, wn);
  WN_Set_Linenum(wn, GetSrcPos());
  return wn;
}

PU_Info *
WhirlFuncBuilder::EmitThunkFunction(const clang::ThunkInfo &ti, ST_IDX st_idx) {
  Is_True(!ti.isEmpty() && ti.Method, ("invalid ThunkInfo"));
  const GlobalDecl gd = GlobalDecl::getFromOpaquePtr((void*)ti.Method);
  const FunctionDecl *decl = cast<FunctionDecl>(gd.getDecl());
  Is_True(decl != NULL &&
          (isa<CXXMethodDecl>(decl) || isa<CXXDestructorDecl>(decl)),
          ("invalid decl"));
  Is_True(_builder->Scope().CurrentDecl() == decl,
          ("invalid scope"));

  if (!decl->doesThisDeclarationHaveABody()) {
    return NULL;
  }
  // adjust export calss when we have body,
  // the body may not generated by ast when
  // symbol created
  if(ST_sclass(ST_ptr(st_idx)) != SCLASS_TEXT) {
    Set_ST_sclass(ST_ptr(st_idx), SCLASS_TEXT);
  }

  // get the parent Decl CXXRecordDecl
  const CXXRecordDecl *record_decl = NULL;
  ST *this_st;
  TY_IDX this_ty;
  if (isa<CXXDestructorDecl>(decl)) {
    const CXXDestructorDecl *dtor = cast<CXXDestructorDecl>(decl);
    record_decl = dtor->getParent();
  } else if (isa<CXXMethodDecl>(decl)) {
    const CXXMethodDecl *md = cast<CXXMethodDecl>(decl);
    record_decl = md->getParent();
  }
  // get srcpos from record_decl
  SRCPOS srcpos = SetSrcPos(getLocation(record_decl));

  UINT i = 0;
  CreateLocalSymtab(st_idx);
  PU_Info *pu_info = CreatePUInfo(st_idx);
  DST_INFO_IDX func_dst = _builder->DstBuilder().CreateDstForFunc(st_idx, MTYPE_To_TY(MTYPE_V));
  _builder->Scope().SetCurrentDst(func_dst);
  Set_PU_Info_pu_dst(pu_info, func_dst);
  WN *pu_tree = NULL;

  if (_builder->Lang_CPP() && emit_exceptions)
    Setup_entry_for_eh();

  bool add_fake_parm = false;
  if (_builder->TB().NeedFakeParm(decl->getReturnType()))
    add_fake_parm = true;

  // create entry
  WN *body = WhirlBlockUtil::nwBlock();
  WN *pragma = WN_CreatePragma(WN_PRAGMA_PREAMBLE_END, (ST_IDX)0, 0, 0);
  WN_INSERT_BlockLast(body, pragma);
  WN_Set_Linenum(pragma, srcpos);
  WN_Set_Linenum(body, srcpos);

  UINT num_of_param = decl->getNumParams();
  if (add_fake_parm) num_of_param++;
  pu_tree = WN_CreateEntry(++num_of_param, st_idx, body, NULL, NULL);
  WN_Set_Linenum(pu_tree, srcpos);
  PushCurrentEntryWN(pu_tree);

  TY_IDX return_ty = _builder->TB().ConvertType(decl->getReturnType());

  // add a fake arg0 for function needs to return the object in memory
  ST *fake_param_st;
  if (_builder->TB().NeedFakeParm(decl->getReturnType())) {
    fake_param_st = New_ST();
    ST_Init(fake_param_st, Save_Str2i(".arg", "", i), CLASS_VAR, SCLASS_FORMAL,
            EXPORT_LOCAL, Make_Pointer_Type(return_ty, FALSE));
    Set_ST_is_value_parm(fake_param_st);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, fake_param_st);
    i++;
  }

  // create the fake first parm .arg0 (this ptr)
  this_ty = _builder->TB().ConvertType(record_decl->getTypeForDecl());
  Is_True(TY_kind(this_ty) == KIND_STRUCT, ("invalid ty"));
  SYMTAB_IDX symtab = _builder->Scope().CurrentSymtab();
  Is_True(symtab > GLOBAL_SYMTAB,
          ("invalid scope for function param"));
  this_st = New_ST(symtab);
  STR_IDX str_idx = Save_Str2i(".arg", "", i);
  ST_Init(this_st, str_idx, CLASS_VAR,
          SCLASS_FORMAL, EXPORT_LOCAL,
          Make_Pointer_Type(this_ty));
  Set_ST_is_value_parm(this_st);

  WN_formal(pu_tree, i) = WN_CreateIdname(0, this_st);
  i++;
  ST_IDX real_st = _builder->SB().GetST(decl);
  Is_True(real_st, ("bad thunk st"));
  WN *ldid_wn = WN_Ldid(TY_mtype(ST_type(this_st)), 0,
                        this_st, ST_type(this_st));

  // adjust *this, get offset from ThunkInfo
  WN *st_wn = EmitAdjustThunk(ldid_wn, ti, TRUE);
  WN_INSERT_BlockLast(body, st_wn);
  WN_Set_Linenum(st_wn, srcpos);

  // generate call wn
  WN *call_wn = WN_Create(OPR_CALL, TY_mtype(return_ty),
                          MTYPE_V, num_of_param);
  WN_Set_Linenum(call_wn, srcpos);
  WN_st_idx(call_wn) = real_st;
  WN_Set_Call_Default_Flags(call_wn);
  if (!_builder->DeclBuilder().Call_nothrow(decl))
    Mark_call_region(call_wn);

  UINT current_parm = 0;
  if (add_fake_parm) {
    WN *target_wn = WN_Lda(Pointer_Mtype, 0, fake_param_st);
    WN *arg_wn = WN_CreateParm(Pointer_Mtype, target_wn,
                               Make_Pointer_Type(ST_type(fake_param_st), FALSE),
                               WN_PARM_BY_VALUE);
    WN_kid(call_wn, current_parm++) = arg_wn;
  }

  // first parm
  TY_IDX parm_ty = WN_type(ldid_wn);
  WN *parm_first = WGEN_CreateParm(TY_mtype(parm_ty),
                                   WN_COPY_Tree(ldid_wn), parm_ty);
  WN_kid(call_wn, current_parm++) = parm_first;

  // formal list
  for (i; i < num_of_param; ++i) {
    UINT real_parm = i - current_parm;
    const ParmVarDecl *parm = decl->getParamDecl(real_parm);
    TY_IDX fst_ty = _builder->TB().ConvertType(parm->getType());
    ST *fst = New_ST(symtab);
    ST_Init(fst, Save_Str2i(".arg", "", i),
            CLASS_VAR, SCLASS_FORMAL, EXPORT_LOCAL,
            fst_ty);
    ST_IDX fst_idx = ST_st_idx(fst);
    WN_formal(pu_tree, i) = WN_CreateIdname(0, fst_idx);
    WN *ldid = WN_Ldid(TY_mtype(fst_ty), 0, fst_idx, fst_ty);
    WN *parm_wn = WGEN_CreateParm(Mtype_comparison(TY_mtype(fst_ty)),
                                  ldid, fst_ty);
    WN_kid(call_wn, i) = parm_wn;
  }

  // insert return WN at last
  WN *return_wn;
  if (decl->getReturnType()->isVoidType()) {
    WN_INSERT_BlockLast(body, call_wn);
    return_wn = WN_CreateReturn();
  } else {
    TYPE_ID ret_mtype = TY_mtype(return_ty);
    WN *blk = WN_CreateBlock();
    WN_INSERT_BlockLast(blk, call_wn);
    WN *ret = WN_Ldid(ret_mtype, -1, Return_Val_Preg, return_ty);
    WN *wn = WGEN_CreateComma(Mtype_comparison(ret_mtype), blk, ret);

    if (TY_kind(return_ty) == KIND_POINTER &&
        TY_pointed(return_ty) == this_ty) {
       // adjust the return result
      TY_IDX ret_type = MTYPE_To_TY(ret_mtype);
      ST *ret_st = Gen_Temp_Symbol(ret_type, "__thunk_base");
      wn = WN_Stid (ret_mtype, 0, ret_st, ret_type, wn);
      WN_INSERT_BlockLast(body, wn);
      WN_Set_Linenum(wn, srcpos);
      WN *thunk_wn = WN_Ldid(ret_mtype, 0, ret_st, ret_type);
      wn = EmitAdjustThunk(thunk_wn, ti, FALSE);
      // guard the result adjustment with a check for null-pointer.
      WN *test = WN_NE(ret_mtype,
                       WN_Ldid(ret_mtype, 0, ret_st, ret_type),
                       WN_Intconst(ret_mtype, 0));
      WN * then_block = WN_CreateBlock();
      WN * else_block = WN_CreateBlock();
      WN * if_stmt = WN_CreateIf(test, then_block, else_block);
      WN_INSERT_BlockLast(then_block, wn);
      WN_INSERT_BlockLast(body, if_stmt);
      WN_Set_Linenum(if_stmt, srcpos);
      wn = WN_Ldid(ret_mtype, 0, ret_st, ret_type);
    }
    return_wn = WN_CreateReturn_Val(OPR_RETURN_VAL, ret_mtype,
                                    MTYPE_V, wn);
  }
  WN_INSERT_BlockLast(body, return_wn);
  WN_Set_Linenum(return_wn, srcpos);
  WhirlBlockUtil::popCurrentBlock();
  PopCurrentEntryWN();

  Is_True(pu_tree != NULL,
          ("invalid pu tree"));
  DBG_VERIFY_WHIRL_IR(pu_tree);
  Is_True(_builder->Scope().CurrentDecl() == decl,
          ("invalid scope"));
  Set_PU_Info_tree_ptr (pu_info, pu_tree);
  Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
  Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);

  _builder->AddPUInfo(pu_info);

  Write_PU_Info(pu_info);

  return pu_info;
}


}
