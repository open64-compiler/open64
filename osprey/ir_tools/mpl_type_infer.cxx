/*

   Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

// maple headers
#include "mir_parser.h"
#include "bin_mplt.h"
#include "opcode_info.h"
#include <cstdlib>
#include "mir_function.h"
#include "constant_fold.h"
#include "mir_type.h"
#include "intrinsics.h"
#include <iostream>
#include <fstream>
#include <unordered_set>

// open64 headers
#include "defs.h"
#include "errors.h"
#include "mpl_type_infer.h"

using namespace maple;

static bool IsDynamicPrimType(PrimType pty) {
  return (pty >= PTY_simplestr && pty <= PTY_dynnone);
}

static bool IsDynamicType(TyIdx tyIdx) {
  MIRType *mty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  Is_True(mty, ("invalud tyIdx"));
  return IsDynamicPrimType(mty->GetPrimType());
}

static PrimType StaticPrimType(PrimType pty) {
  switch (pty) {
  case PTY_dynnull:
  case PTY_dynnone:
    return PTY_ptr;
  case PTY_dynbool:
    return PTY_u1;
  case PTY_dyni32:
    return PTY_i32;
  case PTY_dynf64:
    return PTY_f64;
  case PTY_dynf32:
    return PTY_f32;
  default:
    return (PrimType)0;
  }
}

/*
 * TyHint
 */
TyHint TyHint::GetPointerType() const {
  if (tyIdx == 0)
    return TyHint(TC_pointer, (PrimType)0);
  MIRType *pty = GlobalTables::GetTypeTable().GetOrCreatePointerType(tyIdx, PTY_ptr);
  return TyHint(TC_pointer, pty->tyIdx);
}

TyHint TyHint::MergeHint(TyHint hint) const {
  TypeClass tc = (TypeClass)(tyClass | hint.tyClass);
  // remove dynamic hint
  if (!HasHint(TC_dynamic) || !hint.HasHint(TC_dynamic))
    tc = (TypeClass)(tc & ~TC_dynamic);
  TyIdx left = IsDynamic() ? TyIdx(0) : tyIdx;
  TyIdx right = hint.IsDynamic() ? TyIdx(0) : hint.tyIdx;
  TyIdx ty = left < right ? right : left;
  return TyHint(tc, ty);
}

bool TyHint::IsDynamic() const {
  if (HasHint(TC_dynamic) || tyIdx == 0)
    return true;
  return IsDynamicType(tyIdx);
}

TyIdx TyHint::GetTyIdx() const {
  return tyIdx;
}

PrimType TyHint::GetPrimType() const {
  Is_True(!IsDynamic() && tyIdx != 0, ("bad tyIdx"));
  MIRType *mty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
  return mty->GetPrimType();
}

/*
 * StTypeInfo
 */
TyHint StTypeInfo::AppendHint(TyHint hint) {
  tyHint = tyHint.MergeHint(hint);
  return tyHint;
}

TyHint StTypeInfo::AppendHint(StmtNode *stmt, TyHint hint) {
  tyHint = tyHint.MergeHint(hint);
  refs.push_back(StRefInfo(stmt, hint));
  return tyHint;
}

/*
 * FuncRetInfo
 */
TyHint FuncRetInfo::AppendHint(TyHint hint) {
  tyHint = tyHint.MergeHint(hint);
  return tyHint;
}

TyHint FuncRetInfo::AppendHint(MIRFunction *f, StmtNode *s, TyHint hint) {
  tyHint = tyHint.MergeHint(hint);
  rets.push_back(RetStmtInfo(f, s, hint));
  return tyHint;
}

/*
 * MPLTypeInfer
 */
TyIdx MPLTypeInfer::CreateAnyTy() {
  MIRType *ty = GlobalTables::GetTypeTable().GetUInt64();
  anyTy = ty->GetTypeIndex();
  return anyTy;
}

struct StructFieldDesc {
  const char* name;
  PrimType    pty;
};

inline static MIRType *CreateStructType(const char* name, const StructFieldDesc* fld, uint32 count, MIRModule *mod) {
  FieldVector fldArr;
  for (uint32 i = 0; i < count; ++i) {
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(fld[i].name);
    TyIdx tyIdx = TyIdx(fld[i].pty);
    fldArr.push_back(FieldPair(strIdx, TyidxFieldAttrPair(tyIdx, FieldAttrs())));
  }
  FieldVector prnArr;
  return GlobalTables::GetTypeTable().CreateStructType(name, fldArr, prnArr, mod);
}

TyIdx MPLTypeInfer::CreateObjTy() {
  const static StructFieldDesc desc[] = {
    { "_rc", PTY_u32 },
    { "_object", PTY_u32 }
  };
  MIRType *ty = CreateStructType("_object", desc, sizeof(desc)/sizeof(desc[0]), mod);
  objTy = GlobalTables::GetTypeTable().GetOrCreatePointerType(ty)->GetTypeIndex();
  return objTy;
}

TyIdx MPLTypeInfer::CreateRefTy() {
  MIRType *ty = GlobalTables::GetTypeTable().GetVoidPtr();
  anyTy = ty->GetTypeIndex();
  return anyTy;
}

TyIdx MPLTypeInfer::CreateStrTy() {
  const static StructFieldDesc desc[] = {
    { "_rc", PTY_u32 },
    { "_length", PTY_u32 }
  };
  MIRType *ty = CreateStructType("_string", desc, sizeof(desc)/sizeof(desc[0]), mod);
  strTy = GlobalTables::GetTypeTable().GetOrCreatePointerType(ty)->GetTypeIndex();
  return strTy;
}

TyIdx MPLTypeInfer::CreateArrTy() {
  const static StructFieldDesc desc[] = {
    { "_rc", PTY_u32 },
    { "_size", PTY_u32 },
    { "_maxs", PTY_u32 },
    { "_data", PTY_u32 }
  };
  MIRType *ty = CreateStructType("_array", desc, sizeof(desc)/sizeof(desc[0]), mod);
  arrTy = GlobalTables::GetTypeTable().GetOrCreatePointerType(ty)->GetTypeIndex();
  return arrTy;
}

TyHint MPLTypeInfer::CheckBuiltinObjectType(IntrinsiccallNode *call, TyHint *hint, uint32 opnd, bool result) {
  if (call->intrinsic != INTRN_JSOP_CALL ||
      call->Opnd(0)->op != OP_addroffunc)
    return TyHint();
  AddroffuncNode *addroffunc = static_cast<AddroffuncNode *>(call->Opnd(0));
  const std::string &fname = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(addroffunc->puIdx)->GetName();
  if (fname.compare(0, 6, ".Math.") == 0) {
    if (result)
      return TyHint(TC_none, PTY_f64);
    PrimType pty = call->Opnd(opnd)->primType;
    if (!IsDynamicPrimType(pty))
      return TyHint(TC_none, pty);
    else if (opnd == 0)
      return TyHint(TC_func_ptr, PTY_ptr);
    else if (opnd == 1)
      return TyHint(TC_none, GetObjTy());
    else
      return TyHint(TC_none, PTY_f64);
  }
  return TyHint();
}

TyHint MPLTypeInfer::GetIntrinsicType(MIRIntrinsicID intrn, TyHint *hint, uint32 opnd, bool result) {
  IntrinDesc *intrinDesc = &IntrinDesc::intrintable[intrn];
  switch (intrn) {
  case INTRN_JS_BOOLEAN:
    Is_True(result || opnd == 0, ("bad opnd"));
    return TyHint(TC_none, PTY_u1);

  case INTRN_JS_REGEXP:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, GetStrTy());

  case INTRN_JS_GET_BISTRING:
    if (result)
      return TyHint(TC_none, GetStrTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, PTY_i32);

  case INTRN_JS_GET_BIOBJECT:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, PTY_i32);

  case INTRN_JS_NEW:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, PTY_u64);

  case INTRN_JS_NEW_ARR_LENGTH:
    if (result)
      return TyHint(TC_none, GetArrTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, PTY_u32);

  case INTRN_JS_NEW_FUNCTION:
    if (result)
      return TyHint(TC_none, PTY_i64);
    Is_True(opnd <= 2, ("bad opnd"));
    break;

  case INTRN_JS_NEW_OBJECT_0:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(FALSE, ("bad opnd"));
    break;

  case INTRN_JS_NEW_OBJECT_1:
    if (result)
      return TyHint(TC_none, GetObjTy());
    return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JS_NUMBER:
    if (result)
      return TyHint(TC_none, PTY_i32);
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JS_REQUIRE:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, GetStrTy());

  case INTRN_JS_SETTIMEOUT:
    if (result)
      return TyHint(TC_dynamic, GetAnyTy());
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetRefTy());
    else
      return TyHint(TC_none, PTY_i32);

  case INTRN_JS_STRING:
    if (result)
      return TyHint(TC_none, GetStrTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_ADD:
    if (result)
      return hint[0].MergeHint(hint[1]).MergeHint(hint[2]);
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return hint[0];
    else
      return hint[0].MergeHint(hint[1]);

  case INTRN_JSOP_CALL:
    if (result)
      return hint[0].MergeHint(hint[1]).MergeHint(hint[2]);
    if (opnd == 0)
      return TyHint(TC_none, GetRefTy());
    else if (opnd == 1)
      return TyHint(TC_none, GetObjTy());
    break;

  case INTRN_JSOP_DELPROP:
    if (result)
      return TyHint(TC_none, PTY_u1);
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else
      return TyHint(TC_none, PTY_i32);

  case INTRN_JSOP_GETPROP:
  case INTRN_JSOP_CALLPROP:
  case INTRN_JSOP_GETPROP_BY_NAME:
    if (result)
      return TyHint(TC_none, GetRefTy());
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else
      return intrn == INTRN_JSOP_GETPROP ? TyHint(TC_none, PTY_i32)
                                         : TyHint(TC_none, GetStrTy());

  case INTRN_JSOP_IN:
    if (result)
      return TyHint(TC_none, PTY_u1);
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_dynamic, GetAnyTy());
    else
      return TyHint(TC_none, GetObjTy());

  case INTRN_JSOP_INITPROP_BY_NAME:
    Is_True(!result, ("no result for SETPROP"));
    Is_True(opnd <= 2, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else if (opnd == 1)
      return TyHint(TC_none, GetStrTy());
    else
      return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_INITPROP_GETTER:
    if (result)
      return TyHint(TC_none, GetRefTy());
    Is_True(opnd <= 2, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else if (opnd == 1)
      return TyHint(TC_none, GetStrTy());
    else
      return TyHint(TC_none, GetRefTy());

  case INTRN_JSOP_INSTANCEOF:
    if (result)
      return TyHint(TC_none, PTY_u1);
    Is_True(opnd <= 1, ("bad opnd"));
    return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_LENGTH:
    if (result)
      return TyHint(TC_none, PTY_i32);
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_MORE_ITERATOR:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, GetObjTy());

  case INTRN_JSOP_NEW:
    if (result)
      return TyHint(TC_dynamic, GetObjTy());
    if (opnd == 0)
      return TyHint(TC_dynamic, GetRefTy());
    else if (opnd == 1)
      return TyHint(TC_none, PTY_i32);
    else
      return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_NEW_ITERATOR:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else
      return TyHint(TC_none, PTY_i32);

  case INTRN_JSOP_NEXT_ITERATOR:
    if (result)
      return TyHint(TC_none, GetStrTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_none, GetStrTy());

  case INTRN_JSOP_SETPROP:
  case INTRN_JSOP_SETPROP_BY_NAME:
    Is_True(!result, ("no result for SETPROP"));
    Is_True(opnd <= 2, ("bad opnd"));
    if (opnd == 0)
      return TyHint(TC_none, GetObjTy());
    else if (opnd == 1)
      return intrn == INTRN_JSOP_SETPROP ? TyHint(TC_none, PTY_i32)
                                         : TyHint(TC_none, GetStrTy());
    else
      return TyHint(TC_dynamic, GetAnyTy());

  case INTRN_JSOP_STRICTEQ:
  case INTRN_JSOP_STRICTNE:
    if (result)
      return TyHint(TC_none, PTY_u1);
    Is_True(opnd <= 1, ("bad opnd"));
    if (opnd == 0)
      return hint[0];
    else
      return hint[0].MergeHint(hint[1]);

  case INTRN_JSOP_THIS:
    if (result)
      return TyHint(TC_none, GetObjTy());
    Is_True(FALSE, ("bad opnd"));
    break;

  case INTRN_JSOP_TYPEOF:
    if (result)
      return TyHint(TC_none, GetStrTy());
    Is_True(opnd == 0, ("bad opnd"));
    return TyHint(TC_dynamic, GetAnyTy());

  default:
#ifdef TRACE
    printf("TODO: intrn %s\n", intrinDesc->name);
#endif
    break;
  }
  return TyHint();
}

TyHint MPLTypeInfer::CheckTypesExpr(StmtNode *stmt, BaseNode *expr, TyHint hint) {
  TyHint rty;
  switch (expr->op) {
  case OP_constval: {
    ConstvalNode *cnode = static_cast<ConstvalNode *>(expr);
    PrimType pty = cnode->GetPrimType();
    if (!IsDynamicPrimType(pty))
      return TyHint(TC_none, pty);
    MIRConst *mirconst = cnode->constVal;
    switch (mirconst->kind) {
    case kConstInt:
      pty = PTY_i32;
      break;
    case kConstFloatConst:
      pty = PTY_f32;
      break;
    case kConstDoubleConst:
      pty = PTY_f64;
      break;
    default:
      FmtAssert(FALSE, ("unsupported const kind"));
      break;
    }
    cnode->SetPrimType(pty);
    rty = TyHint(TC_none, pty);
    break;
  }
  case OP_sizeoftype: {
    rty = TyHint(TC_none, expr->GetPrimType());
    break;
  }
  case OP_conststr: {
    rty = TyHint(TC_string, PTY_constStr);
    break;
  }
  case OP_addroflabel: {
    rty = TyHint(TC_label_ptr, PTY_ptr);
    break;
  }
  case OP_addroffunc: {
    rty = TyHint(TC_func_ptr, PTY_ptr);
    break;
  }
  case OP_addrof:
  case OP_dread: {
    AddrofNode *drnode = static_cast<AddrofNode *>(expr);
    rty = FindType(stmt, drnode->stIdx, hint);
    if (expr->op == OP_addrof) {
      if (expr->GetPrimType() == PTY_simplestr)
        rty = TyHint(TC_none, GetStrTy());
      else
        rty = rty.GetPointerType();
    }
    if (!rty.IsDynamic())
      drnode->SetPrimType(rty.GetPrimType());
    break;
  }
  case OP_regread: {
    RegreadNode *regread = static_cast<RegreadNode *>(expr);
    rty = FindType(stmt, regread->regIdx, hint);
    break;
  }
  case OP_extractbits:
  case OP_sext:
  case OP_zext: {
    UnaryNode *unode = static_cast<UnaryNode *>(expr);
    hint.TrimHint(TC_integer);
    rty = CheckTypesExpr(stmt, unode->uOpnd, hint);
    break;
  }
  case OP_ceil:
  case OP_floor:
  case OP_sqrt:
  case OP_trunc: {
    UnaryNode *unode = static_cast<UnaryNode *>(expr);
    hint.TrimHint(TC_float);
    CheckTypesExpr(stmt, unode->uOpnd, hint);
    rty = TyHint(TC_i64, PTY_i64);
    break;
  }
  case OP_recip:
  case OP_round: {
    UnaryNode *unode = static_cast<UnaryNode *>(expr);
    hint.TrimHint(TC_float);
    rty = CheckTypesExpr(stmt, unode->uOpnd, hint);
    break;
  }
  case OP_abs:
  case OP_bnot:
  case OP_neg: {
    UnaryNode *unode = static_cast<UnaryNode *>(expr);
    hint.TrimHint(TC_primitive);
    rty = CheckTypesExpr(stmt, unode->uOpnd, hint);
    break;
  }
  case OP_lnot: {
    UnaryNode *unode = static_cast<UnaryNode *>(expr);
    CheckTypesExpr(stmt, unode->uOpnd, hint);
    rty = TyHint(TC_none, PTY_u1);
    break;
  }
  case OP_cvt:
  case OP_retype: {
    TypeCvtNode *cnode = static_cast<TypeCvtNode *>(expr);
    rty = CheckTypesExpr(stmt, cnode->uOpnd, hint);
    if (IsDynamicPrimType(cnode->fromPrimType) &&
        !rty.IsDynamic()) {
      cnode->fromPrimType = rty.GetPrimType();
    }
    PrimType pty = cnode->GetPrimType();
    if (PrimitivType(pty).IsDynamic()) {
      rty = ResolveDynamicType(pty, rty, hint);
      if (!rty.IsDynamic())
        cnode->SetPrimType(rty.GetPrimType());
    }
    else {
      rty = TyHint(TC_none, pty);
    }
    break;
  }
  case OP_iaddrof: {
    // TODO
    break;
  }
  case OP_iread: {
    // TODO
    break;
  }
  case OP_ashr:
  case OP_band:
  case OP_bior:
  case OP_bxor:
  case OP_lshr:
  case OP_rem:
  case OP_shl: {
    BinaryNode *bnode = static_cast<BinaryNode *>(expr);
    hint.TrimHint(TC_integer);
    TyHint lty = CheckTypesExpr(stmt, bnode->bOpnd[0], hint);
    rty = CheckTypesExpr(stmt, bnode->bOpnd[1], hint);
    rty = lty.MergeHint(lty);
    break;
  }
  case OP_add:
  case OP_sub:
  case OP_mul:
  case OP_div:
  case OP_max:
  case OP_min: {
    BinaryNode *bnode = static_cast<BinaryNode *>(expr);
    hint.TrimHint(TC_primitive);
    TyHint lty = CheckTypesExpr(stmt, bnode->bOpnd[0], hint);
    rty = CheckTypesExpr(stmt, bnode->bOpnd[1], hint);
    rty = lty.MergeHint(lty);
    break;
  }
  case OP_cand:
  case OP_cior:
  case OP_land:
  case OP_lior: {
    BinaryNode *bnode = static_cast<BinaryNode *>(expr);
    hint.TrimHint(TC_integer);
    TyHint lty = CheckTypesExpr(stmt, bnode->bOpnd[0], hint);
    rty = CheckTypesExpr(stmt, bnode->bOpnd[1], hint);
    rty = TyHint(TC_none, PTY_u1);
    break;
  }
  case OP_eq:
  case OP_ne:
  case OP_ge:
  case OP_gt:
  case OP_le:
  case OP_lt: {
    CompareNode *bnode = static_cast<CompareNode *>(expr);
    hint = CheckTypesExpr(stmt, bnode->bOpnd[0], hint);
    hint = CheckTypesExpr(stmt, bnode->bOpnd[1], hint);
    if (IsDynamicPrimType(bnode->opndType) &&
        !hint.IsDynamic())
      bnode->opndType = hint.GetPrimType();
    rty = TyHint(TC_none, PTY_u1);
    break;
  }
  case OP_depositbits: {
    DepositbitsNode *bnode = static_cast<DepositbitsNode *>(expr);
    hint.TrimHint(TC_integer);
    rty = CheckTypesExpr(stmt, bnode->bOpnd[0], hint);
    CheckTypesExpr(stmt, bnode->bOpnd[1], hint);
    break;
  }
  case OP_array: {
    ArrayNode *arrNode = static_cast<ArrayNode *>(expr);
    // TODO
    break;
  }
  case OP_intrinsicop: {
    IntrinsicopNode *intrnop = static_cast<IntrinsicopNode *>(expr);
    MIRIntrinsicID intrinsic = intrnop->intrinsic;
    TyHint aty[32];
    Is_True(intrnop->NumOpnds() < 32, ("opnd num exceeds limit"));
    aty[0] = hint;
    for (uint32 i = 0; i < intrnop->NumOpnds(); ++i) {
      TyHint oty = GetIntrinsicType(intrinsic, aty, i, false);
      oty = CheckTypesExpr(stmt, intrnop->Opnd(i), oty);
      aty[i + 1] = oty;
    }
    rty = GetIntrinsicType(intrinsic, aty, intrnop->NumOpnds() + 1, true);
    if (!rty.IsDynamic()) {
      intrnop->SetPrimType(rty.GetPrimType());
      intrnop->tyIdx = rty.GetTyIdx();
    }
    break;
  }
  default:
    FmtAssert(FALSE, ("unsupported basenode"));
  }
  return rty;
}

void MPLTypeInfer::CheckTypesDeref(IassignNode *iass, TyHint hint) {
  if (!hint.IsDynamic() && iass->fieldID) {
    MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iass->tyIdx));
    MIRType *destType = ptrType->GetPointedType();
    MIRStructType *structType = dynamic_cast<MIRStructType *>(destType);
    FmtAssert(structType, ("CheckTypesDeref: non-zero field id not associated with structtype"));
    FixFieldType(structType, iass->fieldID, 0, hint.GetTyIdx());
  }
}

void MPLTypeInfer::CheckTypesStmt(StmtNode *stmt) {
  switch (stmt->op) {
  case OP_dassign: {
    DassignNode *dass = static_cast<DassignNode *>(stmt);
    TyHint rhsTy = CheckTypesExpr(stmt, dass->uOpnd, TyHint());
    AnnotateType(stmt, dass->stIdx, rhsTy);
    break;
  }
  case OP_regassign: {
    RegassignNode *regass = static_cast<RegassignNode *>(stmt);
    TyHint rhsTy = CheckTypesExpr(stmt, regass->uOpnd, TyHint());
    AnnotateType(stmt, regass->regIdx, rhsTy);
    break;
  }
  case OP_iassign: {
    IassignNode *iass = static_cast<IassignNode *>(stmt);
    CheckTypesExpr(stmt, iass->addrExpr, TyHint());
    TyHint rhsTy = CheckTypesExpr(stmt, iass->rhs, TyHint());
    CheckTypesDeref(iass, rhsTy);
    break;
  }
  case OP_brtrue:
  case OP_brfalse: {
    CondGotoNode *cgoto = static_cast<CondGotoNode *>(stmt);
    CheckTypesExpr(stmt, cgoto->uOpnd, TyHint());
    break;
  }
  case OP_eval:
  case OP_igoto: {
    UnaryStmtNode *ustmt = static_cast<UnaryStmtNode *>(stmt);
    CheckTypesExpr(stmt, ustmt->uOpnd, TyHint());
    break;
  }
  case OP_callassigned: {
    CallNode *cnode = static_cast<CallNode *>(stmt);
    Is_True(cnode->puIdx < funcRetInfo.size(), ("bad puIdx"));
    TyHint retTy = funcRetInfo[cnode->puIdx].Hint();
    if (retTy.IsDynamic() &&
        !IsDynamicPrimType(cnode->GetPrimType()))
      retTy = TyHint(TC_none, cnode->GetPrimType());
    if (!retTy.IsDynamic()) {
      cnode->SetPrimType(retTy.GetPrimType());
      cnode->tyIdx = retTy.GetTyIdx();
      if (!stmt->GetCallReturnVector()->empty()) {
        CallReturnPair crp = stmt->GetCallReturnVector()->front();
        if (crp.second.IsReg()) {
          FmtAssert(FALSE, ("TODO: return reg"));
        }
        else {
          AnnotateType(stmt, crp.first, retTy);
        }
      }
    }
    else {
      AddPendingCallStmt(cnode, cnode->puIdx, retTy);
    }
    // fall through
  }
  case OP_call:
  case OP_icall:
  case OP_icallassigned: {
    NaryStmtNode *cnode = static_cast<NaryStmtNode *>(stmt);
    for (uint32 i = 0; i < cnode->NumOpnds(); i++) {
      CheckTypesExpr(stmt, cnode->Opnd(i), TyHint());
    }
    break;
  }
  case OP_intrinsiccall:
  case OP_intrinsiccallassigned: {
    IntrinsiccallNode *intrncall = static_cast<IntrinsiccallNode *>(stmt);
    MIRIntrinsicID intrinsic = intrncall->intrinsic;
    TyHint aty[32];
    Is_True(intrncall->NumOpnds() < 32, ("opnd num exceeds limit"));
    aty[0] = stmt->op == OP_intrinsiccallassigned
               ? TyHint(TC_none, intrncall->GetPrimType())
               : TyHint(TC_none, PTY_void);
    for (uint32 i = 0; i < intrncall->NumOpnds(); i++) {
      TyHint oty = CheckBuiltinObjectType(intrncall, aty, i, false);
      if (oty.IsEmpty())
        oty = GetIntrinsicType(intrinsic, aty, i, false);
      oty = CheckTypesExpr(stmt, intrncall->Opnd(i), oty);
      aty[i + 1] = oty;
    }
    if (kOpcodeInfo.IsCallAssigned(stmt->op) &&
        !stmt->GetCallReturnVector()->empty()) {
      TyHint rty = CheckBuiltinObjectType(intrncall, aty, intrncall->NumOpnds() + 1, true);
      if (rty.IsEmpty())
        rty = GetIntrinsicType(intrinsic, aty, intrncall->NumOpnds() + 1, true);
      if (!rty.IsDynamic()) {
        intrncall->SetPrimType(rty.GetPrimType());
        intrncall->tyIdx = rty.GetTyIdx();
      }
      CallReturnPair crp = stmt->GetCallReturnVector()->front();
      if (crp.second.IsReg()) {
        FmtAssert(FALSE, ("TODO: return reg"));
      }
      else {
        AnnotateType(stmt, crp.first, rty);
      }
    }
    else {
      intrncall->SetPrimType(PTY_void);
    }
    break;
  }
  case OP_if: {
    IfStmtNode *ifStmt = static_cast<IfStmtNode *>(stmt);
    TyHint rty = CheckTypesExpr(stmt, ifStmt->uOpnd, TyHint(TC_none, PTY_u1));
    Is_True(ifStmt->thenPart, ("then part is null"));
    CheckTypesStmt(ifStmt->thenPart);
    if (ifStmt->elsePart) {
      CheckTypesStmt(ifStmt->elsePart);
    }
    break;
  }
  case OP_dowhile:
  case OP_while: {
    WhileStmtNode *whileStmt = static_cast<WhileStmtNode *>(stmt);
    CheckTypesExpr(whileStmt, whileStmt->uOpnd, TyHint(TC_none, PTY_u1));
    CheckTypesStmt(whileStmt->body);
    break;
  }
  case OP_switch: {
    SwitchNode *switchStmt = static_cast<SwitchNode *>(stmt);
    CheckTypesExpr(switchStmt, switchStmt->switchOpnd, TyHint(TC_i32, PTY_i32));
    break;
  }
  case OP_return: {
    NaryStmtNode *retStmt = static_cast<NaryStmtNode *>(stmt);
    if (retStmt->NumOpnds()) {
      Is_True(retStmt->NumOpnds() == 1, ("only 1 return value supported"));
      TyHint rty = CheckTypesExpr(retStmt, retStmt->Opnd(0), TyHint());
      StmtNode *prev = retStmt->GetPrev();
      if (!prev || prev->op != OP_return)
        retTy = retTy.MergeHint(rty);
    }
    break;
  }
  case OP_throw: {
    UnaryStmtNode *unode = static_cast<UnaryStmtNode*>(stmt);
    CheckTypesExpr(unode, unode->uOpnd, TyHint());
    break;
  }
  case OP_block: {
    BlockNode *blkNode = static_cast<BlockNode *>(stmt);
    CheckTypesBlock(blkNode);
    break;
  }
  case OP_comment:
  case OP_cleanuptry:
  case OP_endtry:
  case OP_finally:
  case OP_goto:
  case OP_gosub:
  case OP_jscatch:
  case OP_jstry:
  case OP_label:
  case OP_retsub:
    break;
  default:
    FmtAssert(FALSE, ("unsupported stmtnode"));
    break;
  }
}

void MPLTypeInfer::CheckTypesBlock(BlockNode *blkNode) {
  for (StmtNode *s = blkNode->GetFirst(); s; s = s->GetNext()) {
    CheckTypesStmt(s);
  }
}

TyHint MPLTypeInfer::ResolveDynamicType(PrimType pty, TyHint res, TyHint opnd) {
  if (pty == PTY_dynstr || pty == PTY_simplestr)
    return TyHint(TC_none, GetStrTy());
  if (pty == PTY_dynobj || pty == PTY_simpleobj)
    return TyHint(TC_none, GetObjTy());
  if (pty == PTY_dynundef || pty == PTY_dynany)
    return TyHint(TC_none, GetAnyTy());
  PrimType sty = IsDynamicPrimType(pty) ? StaticPrimType(pty) : pty;
  if (sty != (PrimType)0)
    return TyHint(TC_none, sty);
  return opnd.MergeHint(TyHint(TC_dynamic, pty));
}

TyHint MPLTypeInfer::FindType(StmtNode *stmt, StIdx stIdx, TyHint hint) {
  TypeInfoArray& array = stIdx.Islocal() ? localStTypeInfo
                                         : globalStTypeInfo;
  Is_True(stIdx.Idx() < array.size(), ("stIdx out of bound"));
  TyHint rhint = array[stIdx.Idx()].Hint();
  if (rhint.GetTyIdx() == 0) {
    MIRSymbol *st = func->GetLocalOrGlobalSymbol(stIdx);
    PrimType pty = st->GetType()->GetPrimType();
    if (IsDynamicPrimType(pty))
      pty = StaticPrimType(pty);
    if (pty != (PrimType)0) {
      rhint = array[stIdx.Idx()].AppendHint(TyHint(TC_none, pty));
    }
  }
  if (hint.GetTyIdx() != 0) {
    rhint = array[stIdx.Idx()].AppendHint(hint);
  }
  return rhint;
}

TyHint MPLTypeInfer::FindType(StmtNode *stmt, PregIdx pregIdx, TyHint hint) {
  Is_True(pregIdx < 0, ("TODO: pseudo reg"));
  switch (-pregIdx) {
  case kSregSp:
  case kSregFp:
  case kSregGp:
    return TyHint(TC_none, PTY_ptr);
  case kSregThrownval:
  case kSregMethodhdl:
    return TyHint(TC_none, GetObjTy());
  default:
    break;
  }
  return hint;
}

void MPLTypeInfer::AnnotateType(StmtNode *stmt, StIdx stIdx, TyHint hint) {
  TypeInfoArray& array = stIdx.Islocal() ? localStTypeInfo
                                         : globalStTypeInfo;
  Is_True(stIdx.Idx() < array.size(), ("stIdx out of bound"));
  TyHint temp = array[stIdx.Idx()].AppendHint(stmt, hint);
#ifdef TRACE
  printf("Annotate: [%d,%d] %s -> %s\n",
         stIdx.Scope(), stIdx.Idx(),
         hint.GetTyIdx() != 0 ? GetPrimTypeName(hint.GetPrimType()) : "N/A",
         temp.GetTyIdx() != 0 ? GetPrimTypeName(temp.GetPrimType()) : "N/A");
#endif
}

void MPLTypeInfer::AnnotateType(StmtNode *stmt, PregIdx pregIdx, TyHint hint) {
  FmtAssert(FALSE, ("preg type"));
}

void MPLTypeInfer::AddPendingCallStmt(StmtNode *stmt, PUIdx puIdx, TyHint hint) {
  Is_True(puIdx < funcRetInfo.size(), ("bad puIdx"));
  funcRetInfo[puIdx].AppendHint(func, stmt, hint);
}

uint32 MPLTypeInfer::FixFieldType(MIRStructType *structTy, uint32 fieldID, uint32 curId, TyIdx ty) {
  ++curId;
  if (structTy->fields.size() == 0) {
    return curId;
  }

  FieldVector::iterator it = structTy->fields.begin();
  FieldVector::iterator end = structTy->fields.end();
  while (it != end) {
    if (curId == fieldID) {
      it->second.first = ty;
      return curId + 1;
    }
    MIRType *curfieldtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(it->second.first);
    MIRStructType *substructty = curfieldtype->EmbeddedStructType();
    if (substructty != nullptr) {
      curId = FixFieldType(substructty, fieldID, curId, ty);
    }
    else {
      ++curId;
    }
    if (curId > fieldID)
      return curId;
    ++it;
  }
  return curId;
}

void MPLTypeInfer::FixPendingCallStmt() {
  for (MIRFunction *func : mod->functionList) {
    if (func->codeMemPool == nullptr || func->body == nullptr)
      continue;
    PUIdx puIdx = func->puIdx;
    Is_True(puIdx < funcRetInfo.size(), ("bad puIdx"));
    TyHint hint = funcRetInfo[puIdx].Hint();
    if (hint.IsDynamic()) {
#ifdef TRACE
      MIRFunction *f = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(puIdx);
      printf("Function: [%d] %s -> dynamic type\n", i, f->GetName().c_str());
#endif
      continue;
    }
    const FuncRetsArray& rets = funcRetInfo[puIdx].Rets();
    MIRFunction *prev = NULL;
    for (FuncRetsArray::const_iterator it = rets.begin();
         it != rets.end(); ++it) {
      if (prev != it->func) {
        this->func = it->func;
        mod->SetCurFunction(it->func);
        prev = it->func;
      }
      CheckTypesStmt(it->stmt);
    }
  }
}

void MPLTypeInfer::FixFunctionProtoType() {
  MIRSymbol *st = func->GetFuncSymbol();
  MIRFuncType *fty = (MIRFuncType *)st->GetType();
  // fix return type
  MIRType *rty = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fty->retTyIdx);
  if (IsDynamicPrimType(rty->GetPrimType()) &&
      !retTy.IsDynamic()) {
    fty->retTyIdx = retTy.GetTyIdx();
  }
  Is_True(func->puIdx < funcRetInfo.size(), ("bad puIdx"));
  funcRetInfo[func->puIdx].AppendHint(retTy);
  // clear parmTy, retTy
  retTy = TyHint();
  // fix formal type
  Is_True(func->formalDefVec.size() == fty->paramTypeList.size(),
          ("formal size mismatch"));
  for (int i = 0; i < func->formalDefVec.size(); ++i) {
    TyIdx parmTy = fty->paramTypeList[i];
    if (!IsDynamicType(parmTy))
      continue;
    FormalDef &fdef = func->formalDefVec[i];
    MIRSymbol *fst = fdef.formalSym;
    if (IsDynamicType(fst->tyIdx)) {
#ifdef TRACE
      printf("TODO: [%d,%d] %s still is dynamic type\n",
             fst->GetScopeIdx(), fst->GetStIndex(),
             fst->GetName().c_str());
#endif
      if (i != 0 || fst->GetName().compare("_this"))
        continue;
      fst->tyIdx = TyIdx(PTY_u64);  // TODO: object type
    }
#ifdef TRACE
printf("replace %s[%d] with %d\n", func->GetName().c_str(), i, fst->tyIdx.GetIdx());
#endif
    fty->paramTypeList[i] = fst->tyIdx;
    fdef.formalTyIdx = fst->tyIdx;
  }
}

void MPLTypeInfer::FixTypeStmt(StmtNode *stmt, MIRSymbol *st) {
#ifdef TRACE
  printf("Fix stmt: [%d,%d] %s -> %s\n",
         st->stIdx.Scope(), st->stIdx.Idx(),
         st->GetName().c_str(),
         GetPrimTypeName(st->GetType()->GetPrimType()));
#endif
  CheckTypesStmt(stmt);
}

void MPLTypeInfer::FixType(TypeInfoArray& info, int scope) {
  for (uint32 i = 1; i < info.size(); ++i) {
    StIdx stIdx(scope, i);
    TyHint hint = info[i].Hint();
    if (hint.IsDynamic()) {
#ifdef TRACE
      printf("TODO: [%d,%d] %s still is dynamic type\n", scope, i,
             func->GetLocalOrGlobalSymbol(stIdx)->GetName().c_str());
#endif
      continue;
    }
    MIRSymbol *st = func->GetLocalOrGlobalSymbol(stIdx);
    Is_True(st, ("bad stIdx"));
    st->SetTyIdx(hint.GetTyIdx());
    const StRefsArray refs = info[i].Refs();
    StRefsArray::const_iterator end = refs.end();
    for (StRefsArray::const_iterator it = refs.begin(); it != end; ++it) {
      FixTypeStmt(it->stmt, st);
    }
    // clear stmts foor global variable
    if (scope == kScopeGlobal)
      info[i].Clear();
  }
}

void MPLTypeInfer::CalibrateTypes() {
  globalStTypeInfo.resize(GlobalTables::GetGsymTable().GetSymbolTableSize());
  funcRetInfo.resize(GlobalTables::GetFunctionTable().funcTable.size());
  for (MIRFunction *func : mod->functionList) {
    if (func->codeMemPool == nullptr || func->body == nullptr)
      continue;
    this->func = func;
    mod->SetCurFunction(func);
    localStTypeInfo.resize(func->symTab->GetSymbolTableSize());
    // check body
    CheckTypesBlock(func->body);
    // fixup types
    FixType(localStTypeInfo, kScopeLocal);
    FixType(globalStTypeInfo, kScopeGlobal);
    //FixFunctionProtoType();
    localStTypeInfo.clear();
  }
  // update types for call return value
  FixPendingCallStmt();
  // update types for global symbols
  FixType(globalStTypeInfo, kScopeGlobal);
  globalStTypeInfo.clear();
}

