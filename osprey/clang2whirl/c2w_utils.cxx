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

#include <stdarg.h>
#include "c2w_utils.h"
#include "c2w_map.h"
#include "wn.h"

namespace wgen {

std::vector<WN *> WhirlBlockUtil::blockVector;
extern TCON_VALUE_MAP _tcon_value_map;
std::stack<WN *> stmt_call_region_stack;

std::vector<WN*> curr_entry_wn;
void PushCurrentEntryWN(WN *wn) { curr_entry_wn.push_back(wn); }
void PopCurrentEntryWN() { curr_entry_wn.pop_back(); }
WN *CurrentEntryWN(void) {
  if (curr_entry_wn.size()==0)
    return NULL;
  else
    return curr_entry_wn.back();
}

WN *WhirlBlockUtil::nwBlock() {
  WN *blk = WN_CreateBlock();
  blockVector.push_back(blk);
  return blk;
}

WN *WhirlBlockUtil::getCurrentBlock() {
  return blockVector.back();
}

void WhirlBlockUtil::popCurrentBlock() {
  blockVector.pop_back();
}

void WhirlBlockUtil::pushBlock(WN *blk) {
  blockVector.push_back(blk);
}

ST *Create_tmp_sym(TY_IDX tyi, SRCPOS spos) {
  return Create_tmp_sym(tyi, TY_name_idx(tyi), spos);
}

ST *Create_tmp_sym(TY_IDX tyi, const char *name, SRCPOS spos) {
  return Create_tmp_sym(tyi, Save_Str(name), spos);
}

ST *Create_tmp_sym(TY_IDX tyi, STR_IDX strIdx, SRCPOS spos) {
  ST *tmpst = New_ST(CURRENT_SYMTAB);
  ST_Init(tmpst, strIdx,
          CLASS_VAR,
          CURRENT_SYMTAB != GLOBAL_SYMTAB ? SCLASS_AUTO : SCLASS_FSTATIC,
          EXPORT_LOCAL, tyi);
  if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
    Set_ST_is_temp_var(tmpst);
  // set srcpos
  Set_ST_Srcpos(*tmpst, spos);
  return tmpst;
}

TY_IDX
get_field_type(TY_IDX struct_type, UINT field_id) {
  UINT cur_field_id = 0;
  UINT64 field_offset = 0;
  FLD_HANDLE fld = get_fld_and_offset(struct_type, field_id, cur_field_id, field_offset);
  Is_True(!fld.Is_Null(), ("not find the field"));
  return FLD_type(fld);
}

// --------------------------------------------------------------------
// This function mimics FLD_get_to_field from common/com/symtab.cxx,
// but it also computes the offset of <field_id> within <ty_idx>
// We need this because FLD_ofst only gives the offset within the first
// enclosing struct.
// --------------------------------------------------------------------
FLD_HANDLE
get_fld_and_offset(TY_IDX ty_idx, UINT field_id, UINT& cur_field_id, UINT64& offset) {
  Is_True(field_id != 0, ("field_id should not be zero in get_fld_and_offset()"));
  if(TY_kind(ty_idx) == KIND_POINTER) {
    ty_idx = TY_pointed(ty_idx);
  }
  Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id) {
      offset += FLD_ofst(fld);
      return fld;
    }
    if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
        TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
      UINT64 nested_offset = offset + FLD_ofst(fld);
      fld = get_fld_and_offset(FLD_type(fld), field_id,
                             cur_field_id, nested_offset);
      if (cur_field_id == field_id) {
        offset = nested_offset;
        return fld;
      }
    }
  } while (!FLD_last_field(fld_iter++));
  return FLD_HANDLE();
}

ST*
Create_function(const char* name, TY_IDX ret_ty, ...) {
  // create ty
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);

  // set return type
  TYLIST tylist_idx;
  Set_TYLIST_type(New_TYLIST(tylist_idx), ret_ty);
  Set_TY_tylist(ty_idx, tylist_idx);

  // set parm type
  va_list ap;
  va_start(ap, ret_ty);
  TY_IDX parm_ty;
  do {
    parm_ty = va_arg(ap, TY_IDX);
    Set_TYLIST_type(New_TYLIST(tylist_idx), parm_ty);
  } while (parm_ty != TY_IDX_ZERO);
  va_end(ap);

  // create pu & st
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init (st, Save_Str(name), CLASS_FUNC,
           SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty_idx);
  return st;
}

ST*
Create_function(const char* name, TY_IDX ret_ty, INT parm_cnt, TY_IDX *parm_ty) {
  // create ty
  TY_IDX ty_idx;
  TY &func = New_TY(ty_idx);
  TY_Init(func, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
  Set_TY_align(ty_idx, 1);

  // set return type
  TYLIST tylist_idx;
  Set_TYLIST_type(New_TYLIST(tylist_idx), ret_ty);
  Set_TY_tylist(ty_idx, tylist_idx);

  // set parm type
  for (INT i = 0; i < parm_cnt; ++i) {
    Set_TYLIST_type(New_TYLIST(tylist_idx), parm_ty[i]);
  }
  Set_TYLIST_type(New_TYLIST(tylist_idx), 0);

  // create pu & st
  PU_IDX pu_idx;
  PU &pu = New_PU(pu_idx);
  TY_IDX pu_ty_idx = ty_idx;
  PU_Init(pu, pu_ty_idx, GLOBAL_SYMTAB + 1);
  ty_idx = (TY_IDX) pu_idx;
  ST *st = New_ST(GLOBAL_SYMTAB);
  ST_Init (st, Save_Str(name), CLASS_FUNC,
           SCLASS_EXTERN, EXPORT_PREEMPTIBLE, ty_idx);
  return st;
}

static ST_IDX
Emit_cxa_atexit(SRCPOS spos) {
  const char *cxa_atexit = "__cxa_atexit";
  static ST *st;
  if (st == NULL) {
    TY_IDX pty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_V));
    st = Create_function(cxa_atexit,
                         MTYPE_To_TY(MTYPE_I4),
                         pty, pty, pty, TY_IDX_ZERO);
  }
  Set_ST_Srcpos(*st, spos);
  return ST_st_idx(st);
}

bool
Check_call_region() {
  Is_True(!stmt_call_region_stack.empty(), ("stack empty"));
  if (!stmt_call_region_stack.empty() &&
      stmt_call_region_stack.top() != CALL_REGION_STACK_MARK)
    return true;
  return false;
}

bool
Check_wn_from_call_region(WN* wn) {
  if (stmt_call_region_stack.empty())
    return false;
  std::stack<WN *> copy_stack = stmt_call_region_stack;
  while (!copy_stack.empty()) {
    if (copy_stack.top() == wn)
      return true;
    copy_stack.pop();
  }
  return false;
}

void
Mark_call_region(WN *wn) {
  Is_True(wn == CALL_REGION_STACK_MARK ||
          WN_operator(wn) == OPR_CALL ||
           WN_operator(wn) == OPR_ICALL,
           ("should be call operator"));
  stmt_call_region_stack.push(wn);
}

void
Unmark_call_region() {
  Is_True(!stmt_call_region_stack.empty(), ("stack empty"));
  while (!stmt_call_region_stack.empty() &&
         stmt_call_region_stack.top() != CALL_REGION_STACK_MARK) {
    stmt_call_region_stack.pop();
    Is_True(!stmt_call_region_stack.empty(), ("stack empty"));
  }
  // pop CALL_REGION_STACK_MARK
  Is_True(stmt_call_region_stack.top() == CALL_REGION_STACK_MARK,
          ("stack should contains CALL_REGION_STACK_MARK"));
  stmt_call_region_stack.pop();
}

void
Clear_call_region() {
  while (!stmt_call_region_stack.empty()) {
    stmt_call_region_stack.pop();
  }
}

WN *
Emit_dtor_call(ST_IDX st_idx) {
  Is_True(st_idx != (ST_IDX)0, ("invalid symtab"));
  SRCPOS spos = ST_Srcpos(st_idx);

  // create dtor call
  ST_IDX atexit_st = Emit_cxa_atexit(spos);
  WN *atexit_call = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 3);
  WN_set_kid_count(atexit_call, 3);
  WN_st_idx(atexit_call) = atexit_st;
  WN_Set_Call_Default_Flags(atexit_call);
  WN_Set_Linenum(atexit_call, spos);

  // set first parm
  WN *parm_1 = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)), ST_ptr(st_idx), 0);
  Set_ST_addr_saved(ST_ptr(st_idx));
  Set_ST_addr_passed(ST_ptr(st_idx));
  WN_kid0(atexit_call) = WGEN_CreateParm(WN_rtype(parm_1), parm_1, WN_ty(parm_1));

  // set second parm
  WN *parm_2 = WN_Intconst(MTYPE_U8, 0);
  WN_kid1(atexit_call) = WGEN_CreateParm(WN_rtype(parm_2), parm_2, Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));

  // set third parm
  ST *shared_obj = New_ST(GLOBAL_SYMTAB);
  ST_Init(shared_obj, Save_Str("__dso_handle"), CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, Make_Pointer_Type(MTYPE_To_TY(MTYPE_V)));
  WN *parm_3 = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0, Make_Pointer_Type(ST_type(shared_obj)), shared_obj, 0);
  Set_ST_addr_saved(shared_obj);
  Set_ST_addr_passed(shared_obj);
  WN_kid2(atexit_call) = WGEN_CreateParm(WN_rtype(parm_3), parm_3, WN_ty(parm_3));

  return atexit_call;
}

static ST_IDX
Emit_cxa_guard_acquire(SRCPOS spos) {
  const char *str = "__cxa_guard_acquire";
  static ST *st;
  if (st == NULL) {
    st = Create_function(str,
                         MTYPE_To_TY(MTYPE_I4),
                         Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8)),
                         TY_IDX_ZERO);
  }
  Set_ST_Srcpos(*st, spos);
  return ST_st_idx(st);
}

static ST_IDX
Emit_cxa_guard_abort(SRCPOS spos) {
  const char *str = "__cxa_guard_abort";
  static ST *st;
  if (st == NULL) {
    st = Create_function(str,
                         MTYPE_To_TY(MTYPE_V),
                         Make_Pointer_Type(MTYPE_To_TY(MTYPE_I1)),
                         TY_IDX_ZERO);
  }
  Set_ST_Srcpos(*st, spos);
  return ST_st_idx(st);
}

static ST_IDX
Emit_cxa_guard_release(SRCPOS spos) {
  const char *str = "__cxa_guard_release";
  static ST *st;
  if (st == NULL) {
    st = Create_function(str,
                         MTYPE_To_TY(MTYPE_V),
                         Make_Pointer_Type(MTYPE_To_TY(MTYPE_I8)),
                         TY_IDX_ZERO);
  }
  Set_ST_Srcpos(*st, spos);
  return ST_st_idx(st);
}

// Itanium C++ ABI 3.3.2:
//   The following is pseudo-code showing how these functions can be used:
//     if (obj_guard.first_byte == 0) {
//       if ( __cxa_guard_acquire (&obj_guard) ) {
//         try {
//           ... initialize the object ...;
//         } catch (...) {
//            __cxa_guard_abort (&obj_guard);
//            throw;
//         }
//         ... queue object destructor with __cxa_atexit() ...;
//         __cxa_guard_release (&obj_guard);
//       }
//     }
WN *
Emit_guarded_init(STR_IDX guard_str_idx, WN *ctor, ST_IDX dtor_st, SRCPOS spos) {
  Is_True(guard_str_idx != (STR_IDX)0,
          ("Invalid mangled name for guarded initlized st"));

  // create guard init st
  ST *guard_var_st = New_ST(GLOBAL_SYMTAB);
  TY_IDX guard_ty = MTYPE_To_TY(MTYPE_I8);
  ST_Init(guard_var_st, guard_str_idx, CLASS_VAR,
          SCLASS_PSTATIC, EXPORT_LOCAL, guard_ty);
  Set_ST_is_initialized(guard_var_st);
  Set_ST_Srcpos(*guard_var_st, spos);
  Set_ST_init_value_zero(guard_var_st);

  // create st
  ST_IDX guard_acquire = Emit_cxa_guard_acquire(spos);
  ST_IDX guard_abort = Emit_cxa_guard_abort(spos);
  ST_IDX guard_release = Emit_cxa_guard_release(spos);

  // obj_guard.first_type
  WN *obj_guard = WN_Iload(MTYPE_I4, 0, MTYPE_To_TY(MTYPE_I1),
                           WN_Lda(Pointer_Mtype, 0, guard_var_st, 0), 0);
  Set_ST_addr_saved(guard_var_st);
  WN *cond1 = WN_EQ(MTYPE_I4, obj_guard, WN_Intconst(MTYPE_I4, 0));

  // __cxa_guard_acquire (&obj_guard)
  WN *call_blk = WhirlBlockUtil::nwBlock();
  TY_IDX ret_ty_idx = MTYPE_To_TY(MTYPE_I4);
  WN *call_acquire = WN_Create(OPR_CALL, TY_mtype(ret_ty_idx), MTYPE_V, 1);
  WN_st_idx(call_acquire) = guard_acquire;
  WN_Set_Linenum(call_acquire, spos);

  Set_ST_addr_saved(guard_var_st);
  Set_ST_addr_passed(guard_var_st);
  WN *parm = WGEN_CreateParm(TY_mtype(guard_ty),
                             WN_Lda(Pointer_Mtype, 0, guard_var_st, 0),
                             guard_ty);
  WN_kid(call_acquire, 0) = parm;
  WN_INSERT_BlockLast(call_blk, call_acquire);
  WN *ldid = WN_Ldid(TY_mtype(ret_ty_idx), -1, Return_Val_Preg, ret_ty_idx);
  WN *comma = WGEN_CreateComma(WN_rtype(ldid), call_blk, ldid);
  WN *cond2 = WN_NE(MTYPE_I4, comma, WN_Intconst(MTYPE_I4, 0));
  WhirlBlockUtil::popCurrentBlock();  // pop call_blk

  // __cxa_guard_release
  WN *call_release = WN_Create(OPR_CALL, MTYPE_V, MTYPE_V, 1);
  WN_st_idx(call_release) = guard_release;
  WN_Set_Linenum(call_release, spos);

  WN_kid(call_release, 0) = WN_COPY_Tree(parm);
  WN_Set_Call_Default_Flags(call_release);
  WN *block = WhirlBlockUtil::nwBlock();
  if (ctor)
    WN_INSERT_BlockLast(block, ctor);
  WN_INSERT_BlockLast(block, call_release);

  // call dtor if exist
  if (dtor_st) {
    WN *atexit_call = Emit_dtor_call(dtor_st);
    if (atexit_call) WN_INSERT_BlockLast(block, atexit_call);
  }
  WhirlBlockUtil::popCurrentBlock();  // pop block

  // create WN
  WN *qcquire_block = WN_CreateIf(cond2, block, WN_CreateBlock());
  WN_Set_Linenum(qcquire_block, spos);

  WN *guard_then_block = WhirlBlockUtil::nwBlock();
  WN_INSERT_BlockLast(guard_then_block, qcquire_block);

  WN *ret = WN_CreateIf(cond1, guard_then_block, WN_CreateBlock());
  WN_Set_Linenum(ret, spos);
  WhirlBlockUtil::popCurrentBlock();  // pop guard_then_block
  return ret;
}

// generate null const whirl node
WN *
Gen_null_const(TY_IDX ty_idx) {
  TCON tcon;
  TYPE_ID mtype = TY_mtype(ty_idx);
  if (MTYPE_is_integral(mtype)) {
    if (mtype == MTYPE_I1 || mtype == MTYPE_I2)
      mtype = MTYPE_I4;
    else if (mtype == MTYPE_U1 || mtype == MTYPE_U2)
      mtype = MTYPE_U4;
    return WN_Intconst(mtype, 0);
  } else if (MTYPE_is_complex(mtype)) {
    switch(TY_size(ty_idx)) {
      case 8:
        tcon = Host_To_Targ_Complex_4(MTYPE_C4, 0, 0);
        break;
      case 16:
        tcon = Host_To_Targ_Complex(MTYPE_C8, 0, 0);
        break;
      case 24:
      case 32:
        tcon = Host_To_Targ_Complex_10(MTYPE_C10, 0, 0);
        break;
      default:
        Is_True(false, ("unexpected size for complex constant"));
        break;
    }
  } else if (MTYPE_is_float(mtype)) {
    switch (TY_size(ty_idx)) {
    case 4:
      tcon = Host_To_Targ_Float_4(MTYPE_F4, 0);
      break;
    case 8:
      tcon = Host_To_Targ_Float(MTYPE_F8, 0);
      break;
    case 12:
    case 16:
      tcon = Host_To_Targ_Float_10(MTYPE_F10, 0);
      break;
    default:
      Is_True(false, ("unexpected size for intconstant"));
      break;
    }
  } else if (MTYPE_is_void(mtype)) {
    Is_True(false, ("unsupported void type in Gen_null_const"));
  } else
    Is_True(false, ("unsupported type in Gen_null_const"));

   ST *st = New_Const_Sym(Enter_tcon(tcon), ty_idx);
   return WN_CreateConst(OPR_CONST, mtype, MTYPE_V, st);
}

// convert condition wn
WN *
Handle_cond_wn(WN *wn) {
  Is_True(wn != NULL, ("null whirl node"));
  if (WN_operator(wn) == OPR_COMMA) {
    WN_kid1(wn) = Handle_cond_wn(WN_kid1(wn));
    return wn;
  }
  if (WN_operator(wn) == OPR_RCOMMA) {
    WN_kid0(wn) = Handle_cond_wn(WN_kid0(wn));
    return wn;
  }
  if (WN_rtype(wn) == MTYPE_B || OPERATOR_is_boolean(WN_operator(wn)))
    return wn;
  TYPE_ID mtype = WN_rtype(wn);
  WN *ret = WN_NE(mtype, wn, Gen_null_const(MTYPE_To_TY(mtype)));
  return ret;
}

WN *
Handle_expr_for_copy(WN *blk, WN *expr, TY_IDX ty, SRCPOS spos, const char *name) {
  Is_True(OPERATOR_is_expression(WN_operator(expr)),
          ("not expression"));
  if (WN_operator(expr) != OPR_LDID &&
      WN_operator(expr) != OPR_INTCONST &&
      WN_operator(expr) != OPR_LDA) {
    TYPE_ID dscty = TY_mtype(ty);

    TYPE_ID rty = WN_rtype(expr);
    ST    *st;
    UINT64 ofst = 0;
    if (rty == MTYPE_M) {
      st = Create_tmp_sym(ty, name ? name : ".safe.copy", spos);
    }
    else {
      ofst = Create_Preg(rty, name);
      st = MTYPE_To_PREG(rty);
    }
    WN *stid = WN_Stid(dscty, ofst, st, ty, expr);
    WN_INSERT_BlockLast(blk, stid);
    WN_Set_Linenum(stid, spos);
    expr = WN_Ldid(dscty, ofst, st, ty);
  }
  return expr;
}


WN *
WGEN_StidTemp(TY_IDX ty, WN *val, const char *name) {
  Is_True(val && OPERATOR_is_expression(WN_operator(val)),
          ("bad val"));
  ST *st = NULL;
  PREG_NUM ofst = 0;
  TYPE_ID mtype = TY_mtype(ty);
  if (mtype == MTYPE_M) {
    st = Gen_Temp_Symbol(ty, name ? name : ".anon.sym.");
    ofst = 0;
  }
  else {
    st = MTYPE_To_PREG(mtype);
    ofst = Create_Preg(mtype, name ? name : ".anon.preg");
  }
  return WN_Stid(mtype, ofst, st, ty, val);
}

WN *
WGEN_CreateComma(TYPE_ID rtype, WN* blk, WN* ldid) {
#ifdef Is_True_On
  WN* last = WN_last(blk);
  if (OPERATOR_is_call(WN_operator(last))) {
    if (rtype == MTYPE_M) {
      Is_True((WN_rtype(last) == MTYPE_M || WN_rtype(last) == MTYPE_V)
              && WN_rtype(ldid) == MTYPE_M,
              ("call or ldid not return M"));
    }
    else {
      Is_True(WN_rtype(last) != MTYPE_M && WN_rtype(ldid) != MTYPE_M,
              ("call or ldid returns M"));
    }
  }
#endif
  if (WN_operator(ldid) == OPR_COMMA) {
    WN_INSERT_BlockLast(blk, WN_kid0(ldid));
    ldid = WN_kid1(ldid);
  }
  return WN_CreateComma(OPR_COMMA, rtype, MTYPE_V, blk, ldid);
}

WN*
WGEN_CreateCselect(TYPE_ID rtype, WN *cond, WN *twn, WN *fwn) {
#ifdef Is_True_On
  Is_True(OPERATOR_is_expression(WN_operator(cond)), ("cond not expr"));
  Is_True(WN_rtype(cond) != MTYPE_M, ("cond returns M"));
  if (rtype == MTYPE_M) {
    Is_True(WN_rtype(twn) == MTYPE_M && WN_rtype(fwn) == MTYPE_M, ("kid not return M"));
  }
  else {
    Is_True(WN_rtype(twn) != MTYPE_M && WN_rtype(fwn) != MTYPE_M, ("kid returns M"));
  }
#endif
  return WN_CreateExp3(OPR_CSELECT, Mtype_comparison(rtype),
                       MTYPE_V, cond, twn, fwn);
}

WN*
WGEN_CreateParm(TYPE_ID rtype, WN *parm, TY_IDX ty) {
#ifdef Is_True_On
  Is_True(ty != TY_IDX_ZERO, ("ty is 0"));
  if (rtype == MTYPE_M) {
    Is_True(WN_rtype(parm) == MTYPE_M, ("parm rtype is not M"));
    Is_True(TY_mtype(ty) == MTYPE_M, ("ty is not M"));
  }
  else {
    Is_True(WN_rtype(parm) != MTYPE_M, ("parm rtype is M"));
    //Is_True(TY_mtype(ty) != MTYPE_M, ("ty is M"));
  }
#endif
  return WN_CreateParm(rtype, parm, ty, WN_PARM_BY_VALUE);
}

WN *
WGEN_CreateReturn(SRCPOS spos) {
  WN *ret = WN_CreateReturn();
  WN_Set_Linenum(ret, spos);
  return ret;
}

long double
convertToLongDouble(const llvm::APFloat val) {
  union {
    struct {
      uint64_t lo;
      uint64_t hi;
    } s;
    long double val;
  } buf;

  long double value = 0.;
  llvm::APInt api = val.bitcastToAPInt();
  if (&val.getSemantics() == &llvm::APFloat::x87DoubleExtended()) {
    buf.s.lo = api.getLoBits(64).getZExtValue();
    buf.s.hi = api.getHiBits(16).getZExtValue();
    value = buf.val;
  } else if (&val.getSemantics() == &llvm::APFloat::IEEEquad())
    Is_True(false, ("TODO: unsupported for IEEEquad"));
  else if (&val.getSemantics() == &llvm::APFloat::PPCDoubleDouble())
    Is_True(false, ("TODO: unsupported for PPCDoubleDouble"));
  else if (&val.getSemantics() == &llvm::APFloat::IEEEhalf())
    value = api.getZExtValue();
  else
    Is_True(false, ("unsupported floating point type"));
  return value;
}

// create tcon from value
TCON_IDX
createTconFromValue(TY_IDX ty_idx, long double value) {
  TCON tcon;
  switch (TY_mtype(ty_idx)) {
    case MTYPE_F4:
      tcon = Host_To_Targ_Float_4(MTYPE_F4, (float) value);
      break;
    case MTYPE_F8:
      tcon = Host_To_Targ_Float(MTYPE_F8, (double) value);
      break;
    case MTYPE_F10:
      tcon = Host_To_Targ_Float_10(MTYPE_F10, value);
      break;
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_I8:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_U8: {
      INT64 ival = value;
      tcon = Host_To_Targ(TY_mtype(ty_idx), ival);
      break;
    }
    default:
      Is_True(false, ("ConvertFloatingLiteral: unexpected type for real constant"));
      break;
  }
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  _tcon_value_map[tcon_idx] = value;
  return tcon_idx;
}

TCON_IDX
Convert_float_to_tcon(TY_IDX ty_idx, const llvm::APFloat init) {
  Is_True(ty_idx != (TY_IDX)0, ("invalid ty idx"));

  if (&init.getSemantics() == &llvm::APFloat::IEEEsingle() ||
      &init.getSemantics() == &llvm::APFloat::IEEEdouble()) {
    bool is_double = &init.getSemantics() == &llvm::APFloat::IEEEdouble();
    double value = is_double? init.convertToDouble() : init.convertToFloat();
    return createTconFromValue(ty_idx, value);
  }

  return createTconFromValue(ty_idx, convertToLongDouble(init));
}

TCON_IDX
Convert_complex_float_to_tcon(TY_IDX ty_idx, const llvm::APFloat real, const llvm::APFloat imag) {
  Is_True(ty_idx != (TY_IDX)0, ("invalid ty idx"));
  TCON tcon;
  switch (TY_size(ty_idx)) {
  case 8:
    Is_True(&real.getSemantics() == &llvm::APFloat::IEEEsingle() &&
            &imag.getSemantics() == &llvm::APFloat::IEEEsingle(),
            ("not IEEE float"));
    tcon = Host_To_Targ_Complex_4(MTYPE_C4, real.convertToFloat(), imag.convertToFloat());
    break;
  case 16:
    Is_True(&real.getSemantics() == &llvm::APFloat::IEEEdouble() &&
            &imag.getSemantics() == &llvm::APFloat::IEEEdouble(),
           ("not IEEE double"));
    tcon = Host_To_Targ_Complex(MTYPE_C8, real.convertToDouble(), real.convertToDouble());
    break;
  case 24:
  case 32:
    Is_True(&real.getSemantics() == &llvm::APFloat::x87DoubleExtended() &&
            &imag.getSemantics() == &llvm::APFloat::x87DoubleExtended(),
            ("not x87 long double"));
    tcon = Host_To_Targ_Complex_10(MTYPE_C10,
                                   convertToLongDouble(real),
                                   convertToLongDouble(imag));
    break;
  default:
    Is_True(false, ("unexpected size for complex constant"));
    break;
  }
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  return tcon_idx;
}

TCON_IDX
Convert_string_to_tcon(const clang::StringLiteral *expr, TY_IDX ty_idx) {
  Is_True(TY_kind(ty_idx) == KIND_ARRAY,
          ("Strings should always be arrays"));

  int length = expr->getByteLength() + TY_size(TY_etype(ty_idx));
  llvm::StringRef str;
  TCON tcon;
  if (expr->getCharByteWidth() == 1) {
    str = expr->getString();
    tcon = Host_To_Targ_String(MTYPE_STRING, str.str().c_str(),
                               TY_size(ty_idx) < length ? TY_size(ty_idx) : length);
  }
  else {
    str = expr->getBytes();
    // get right value for wide string literal
    char dest[length];
    memset(dest, 0, length);
    memcpy(dest, str.str().c_str(), expr->getByteLength());
    tcon = Host_To_Targ_String(MTYPE_STRING, dest, length);
  }
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  return tcon_idx;
}

TCON_IDX
Gen_complex_tcon(TY_IDX ty_idx, UINT64 real_val, long double imagin_val) {
  Is_True(ty_idx != (TY_IDX)0, ("invalid ty idx"));
  TCON tcon, elem_tcon;
  switch(TY_size(ty_idx)) {
    case 8:
      tcon = Host_To_Targ_Complex_4(MTYPE_C4, real_val, imagin_val);
      break;
    case 16:
      tcon = Host_To_Targ_Complex(MTYPE_C8, real_val, imagin_val);
      break;
    case 24:
    case 32:
      tcon = Host_To_Targ_Complex_10(MTYPE_C10, real_val, imagin_val);
      break;
    default:
      Is_True(false, ("unexpected size for complex constant"));
      break;
  }
  TCON_IDX tcon_idx = Enter_tcon(tcon);
  _tcon_value_map[tcon_idx] = real_val + imagin_val;
  return tcon_idx;
}

TCON_IDX
Convert_imaginary_to_tcon(const clang::ImaginaryLiteral *expr, Result elem, TY_IDX ty_idx) {
  long double val;

  if (elem.isTCon())
    val = _tcon_value_map[elem.Tcon()];
  else if (elem.isIntConst())
    val = elem.IntConst();
  else
    Is_True(false, ("invalid return type"));

  TCON_IDX tcon_idx;
  if (!MTYPE_is_complex(TY_mtype(ty_idx)))
    tcon_idx = createTconFromValue(ty_idx, 0);
  else
    tcon_idx = Gen_complex_tcon(ty_idx, 0, val);

  _tcon_value_map[tcon_idx] = val;
  return tcon_idx;
}

// store string (value WN) into address (addr WN)
// rather than directy copy assignment
void
GenMstoreForString(WN *addr, WN *value, TY_IDX ty, UINT len, UINT off, SRCPOS spos) {
  Is_True(addr != NULL && value != NULL, ("invalid wn"));
  Is_True(ty != (TY_IDX)0 && TY_kind(ty) == KIND_ARRAY, ("invalid ty idx"));
  UINT size = TY_size(ty);
  UINT length = len + TY_size(TY_etype(ty));
  Is_True(size > 0 && length > 0,
          ("neither ty size nor string length should be zero"));
  UINT load_size = (size > length) ? length : size;
  TY_IDX ptr_ty = Make_Pointer_Type(ty);
  WN *const_wn = WN_Intconst(MTYPE_I4, load_size);
  WN *load_wn = WN_CreateMload(0, ptr_ty, WN_COPY_Tree(value),
                               const_wn);
  WN *store_wn = WN_CreateMstore(off, ptr_ty, load_wn,
                                 WN_COPY_Tree(addr),
                                 WN_COPY_Tree(const_wn));
  WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), store_wn);
  WN_Set_Linenum(store_wn, spos);

  // fill the rest of array with zero value
  if (size > load_size) {
    store_wn = WN_CreateMstore(off + load_size, ptr_ty,
                               WN_Intconst(MTYPE_U4, 0),
                               WN_COPY_Tree(addr),
                               WN_Intconst(MTYPE_I4,size - load_size));
    WN_INSERT_BlockLast(WhirlBlockUtil::getCurrentBlock(), store_wn);
    WN_Set_Linenum(store_wn, spos);
  }
}

// buffer for simulating the initialized memory unit;
// it is managed independent of host's endianness
class INITBUF {
public:
  UINT64 ival;

  INITBUF(void) {}
  INITBUF(UINT64 i): ival(i) {}
  ~INITBUF(void) {}
  mUINT8 NthByte(INT i) { // i must be from 0 to 7
    INT rshft_amt = (Target_Byte_Sex == BIG_ENDIAN) ? 7-i : i;
    return (ival >> (rshft_amt * 8)) & 0xff;
  }
};

// at entry, assumes that in the current struct, initv for "bytes" bytes have
// been generated; at exit, "bytes" will be updated with the additional
// bytes that this invocation generates.
void
AddBitfieldInitvForTree(INT64 val, FLD_HANDLE fld,
                        INITV_IDX &last_initv, UINT64 &bytes) {
#if 0
  Is_True(init_val.isIntConst(),
          ("initialization value of bitfield expected to be integer"));
#endif
  INT bofst = FLD_bofst(fld);
  INT bsize = FLD_bsize(fld);
  if (bsize == 0)
    return;

//  INITBUF ib(init_val.IntConst());
  INITBUF ib(val);
  // truncate ival according to the bitfield size and leave it left-justified
  ib.ival = ib.ival << (64 - bsize);
  // shift the value back right to the precise position within INITBUF
  if (Target_Byte_Sex == BIG_ENDIAN)
    ib.ival = ib.ival >> bofst;
  else
    ib.ival = ib.ival >> (64 - bofst - bsize);

  // find number of bytes to output
  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
  // find number of bytes that have been output with previous bitfields
  INT bytes_out = bytes - FLD_ofst(fld);
  INT i;
  if (bytes_out > 0) {
    // verify that, other than the last output byte, the earlier bytes in
    // ib are all 0
    for (i = 0; i < bytes_out - 1; i++)
      Is_True(ib.NthByte(i) == 0,
              ("processing error in AddBitfieldInitvForTree"));
    if (ib.NthByte(bytes_out-1) != 0) {// merge and change last_initv
      if (INITV_kind(last_initv) == INITVKIND_VAL) {
        TCON &tc = INITV_tc_val(last_initv);
        mUINT8 last_ival = TCON_k0(tc);
        tc.vals.k0 = last_ival | ib.NthByte(bytes_out-1);
      }
      else { // need to create a new TCON
        if (INITV_kind(last_initv) == INITVKIND_ONE)
          INITV_Init_Integer(last_initv, MTYPE_I1,
                             1 | ib.NthByte(bytes_out-1));
        else {
          Is_True(INITV_kind(last_initv) == INITVKIND_ZERO,
                  ("processing error in static bit field initialization"));
          INITV_Init_Integer(last_initv, MTYPE_I1,
                             ib.NthByte(bytes_out-1));
        }
      }
    }
  }
  // output the remaining bytes
  for (i = bytes_out; i < num_of_bytes; i++) {
    INITV_IDX cur = New_INITV();
    INITV_Init_Integer(cur, MTYPE_I1, ib.NthByte(i));
    if (last_initv != 0) {
      Set_INITV_next(last_initv, cur);
    }
    last_initv = cur;
  }
  bytes += num_of_bytes - bytes_out;
}

}
