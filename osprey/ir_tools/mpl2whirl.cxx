/*
 * Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
 *
 * OpenArkCompiler is licensed under the Mulan PSL v1.
 * You can use this software according to the terms and conditions of the Mulan PSL v1.
 * You may obtain a copy of Mulan PSL v1 at:
 *
 *     http://license.coscl.org.cn/MulanPSL
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 * See the Mulan PSL v1 for more details.
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

// open64 headers start here
#include "defs.h"
#include "mempool.h"
#include "wn.h"			    /* for ir_reader.h */
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"
#include "ir_bwrite.h"		    /* for WN_open_output(), etc. */
#include "ir_bread.h"		    /* for WN_open_input(), etc. */
#include "erglob.h"
#include "errors.h"
#include "config.h"
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"              /* for Last_Dedicated_Preg_Offset */
#include "glob.h"
#include "controls.h"
#include "dwarf_DST.h"
#include "dwarf_DST_mem.h"
#include "dwarf_DST_producer.h"

#include "mpl_type_infer.h"        /* for type calibration */

BOOL Run_vsaopt = FALSE; // hack to workaround undefine since

static PU_Info *PU_Info_Root = NULL;
static PU_Info *PU_Info_Last = NULL;

using namespace maple;

bool ConstantFoldModule(maple::MIRModule *module) {
  maple::ConstantFold cf(module);
  MapleVector<maple::MIRFunction *> &funcList = module->functionList;
  for (MapleVector<maple::MIRFunction *>::iterator it = funcList.begin(); it != funcList.end(); it++) {
    maple::MIRFunction *curfun = *it;
    maple::BlockNode *block = curfun->body;
    module->SetCurFunction(curfun);
    if (!block) {
      continue;
    }
    cf.Simplify(block);
  }
  return true;
}

static constexpr uint64 RoundUpConst(uint64 offset, uint8 align) {
  return (-align) & (offset + align - 1);
}

static inline uint64 RoundUp(uint64 offset, uint8 align) {
  if (align == 0) {
    return offset;
  }
  return RoundUpConst(offset, align);
}

static constexpr uint64 RoundDownConst(uint64 offset, uint8 align) {
  return (-align) & offset;
}

static inline uint64 RoundDown(uint64 offset, uint8 align) {
  if (align == 0) {
    return offset;
  }
  return RoundDownConst(offset, align);
}

static UINT32 PrimType2whirl(PrimType ptyp) {
  switch (ptyp) {
  case PTY_void: return MTYPE_V;
  case PTY_i8: return MTYPE_I1;
  case PTY_i16: return MTYPE_I2;
  case PTY_i32: return MTYPE_I4;
  case PTY_i64: return MTYPE_I8;
  case PTY_u8: return MTYPE_U1;
  case PTY_u16: return MTYPE_U2;
  case PTY_u32: return MTYPE_U4;
  case PTY_u64: return MTYPE_U8;
  case PTY_u1: return MTYPE_U1;
  case PTY_ptr: return MTYPE_U8;
  case PTY_ref: return MTYPE_U8;
  case PTY_a32: return MTYPE_U4;
  case PTY_a64: return MTYPE_U8;
  case PTY_f32: return MTYPE_F4;
  case PTY_f64: return MTYPE_F8;
  case PTY_f128: return MTYPE_F16;
  case PTY_c64: return MTYPE_C4;
  case PTY_c128: return MTYPE_C8;
  case PTY_simplestr:
  case PTY_constStr: return MTYPE_STR;
  case PTY_agg: return MTYPE_M;
#ifdef TARG_X8664
  case PTY_v2i64: return MTYPE_V16I8;
  case PTY_v4i32: return MTYPE_V16I4;
  case PTY_v8i16: return MTYPE_V16I2;
  case PTY_v16i8: return MTYPE_V16I1;
  case PTY_v2f64: return MTYPE_V16F8;
  case PTY_v4f32: return MTYPE_V16F4;
#endif
#ifdef DYNAMICLANG
  case PTY_dynbool: return MTYPE_U1;
  case PTY_dyni32: return MTYPE_I4;
  case PTY_dynf32: return MTYPE_F4;
  case PTY_dynf64: return MTYPE_F8;
  case PTY_simpleobj: return MTYPE_U8;
  case PTY_dynstr: return MTYPE_U8;
  case PTY_dynany: return MTYPE_U8;
  case PTY_dynundef: return MTYPE_U8;
  case PTY_dynnull: return MTYPE_U8;
  case PTY_dynobj: return MTYPE_U8;
  case PTY_dynnone: return MTYPE_U8;
#endif
  case PTY_unknown: return MTYPE_UNKNOWN;
  default:
    FmtAssert(FALSE, ("PrimType2whirl: PrimType %s not supported", GetPrimTypeName(ptyp)));
    return MTYPE_UNKNOWN;
  }
}

// to facilitate the use of unordered_map
class StIdxHash
{
 public:
  std::size_t operator()(const StIdx &stIdx) const {
    return std::hash<uint32>{}(stIdx.Idx());
  }
};

class MPL2whirl : public MPLTypeInfer {
 private:
  std::unordered_map<TyIdx, TY_IDX, TyIdxHash> TyIdx2TY_IDXMap;
  std::unordered_map<StIdx, ST_IDX, StIdxHash> StIdx2ST_IDXMap;
  std::unordered_map<PregIdx, PREG_NUM> PregIdx2PREG_NUMMap;
  std::unordered_map<LabelIdx, LABEL_IDX> LabelIdx2LABEL_IDXMap;
 public:
  bool hasForwardTypeReference = FALSE;
 private:
  DST_INFO_IDX mod_dst = DST_INVALID_IDX;
  DST_INFO_IDX fn_dst = DST_INVALID_IDX;
  DST_INFO_IDX CreateCUDst();
  DST_INFO_IDX CreateFuncDst(ST *func_st);
 public:
  explicit MPL2whirl(MIRModule *module) : MPLTypeInfer(module) {
    mod_dst = CreateCUDst();
  }
  void TranslateTypes();
  void TranslateGlobalSymbols();
  void TranslateFunctionBodies();
 private:
  TY_IDX TyIdx2TY_IDX(TyIdx tyIdx) {
    std::unordered_map<TyIdx, TY_IDX, TyIdxHash>::iterator it = TyIdx2TY_IDXMap.find(tyIdx);
    if (it == TyIdx2TY_IDXMap.end()) {
      hasForwardTypeReference = TRUE;
      return 0;
    }
    return it->second;
  }
  ST_IDX StIdx2ST_IDX(StIdx stIdx) {
    std::unordered_map<StIdx, ST_IDX, StIdxHash>::iterator it = StIdx2ST_IDXMap.find(stIdx);
    if (it == StIdx2ST_IDXMap.end())
      return 0;
    return it->second;
  }
  PREG_NUM PregIdx2PREG_NUM(PregIdx pidx);
  LABEL_IDX LabelIdx2LABEL_IDX(LabelIdx lidx);
  void MIRSymbol2whirl(MIRSymbol *sym);
  WN *SimulateIntrinsicCall(IntrinsiccallNode *x, TYPE_ID rty);
  WN *ConvertJsopCall(IntrinsiccallNode *x, TYPE_ID ret_mtype);
  WN *ExprNode2whirl(BaseNode *x);
  MIRType *GetFuncPtrType(BaseNode *x);
  void StmtNode2whirl(StmtNode *stmt, WN *blk);
};

DST_INFO_IDX MPL2whirl::CreateCUDst() {
  return DST_mk_compile_unit((char *)mod->fileName.c_str(),
                             (char *)".",
                             (char *)"mpl2whirl",
                             DW_LANG_C89,  /* No Javascript in DWARF */
                             DW_ID_case_sensitive);
}

DST_INFO_IDX MPL2whirl::CreateFuncDst(ST *func_st) {
  USRCPOS spos;
  USRCPOS_clear(spos);
  DST_INFO_IDX type_dst = DST_INVALID_IDX;
  DST_INFO_IDX func_dst = DST_mk_subprogram(spos,
                                            (char *)ST_name(func_st),
                                            type_dst,
                                            DST_INVALID_IDX,
                                            ST_st_idx(func_st),
                                            DW_INL_not_inlined,
                                            DW_VIRTUALITY_none,
                                            0, false, true, false, false);
  return func_dst;
}

void MPL2whirl::TranslateTypes() {
  uint32 idx = 1;
  TY_IDX ty_idx = 0;
  for (; idx <= GlobalTables::GetTypeTable().lastDefaultTyIdx.GetIdx(); idx++) {
    TyIdx tyIdx(idx);
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
#ifndef DYNAMICLANG
    if (PrimitivType(type->primType).IsDynamic())
      continue;
    if (type->primType == PTY_simplestr || type->primType == PTY_simpleobj ||
        type->primType == PTY_gen)
      continue;
#else
    if (type->primType == PTY_simpleobj || type->primType == PTY_gen)
      continue;
#endif
#ifndef TARG_X8664
    if (type->primType >= PTY_v2i64 && type->primType <= PTY_v4f32)
      continue;
#endif
    TYPE_ID mtype = PrimType2whirl(type->primType);
    Set_TY_IDX_index(ty_idx, mtype);
    Set_TY_align(ty_idx, MAX(1, GetPrimTypeSize(type->primType)));
    if (mtype != MTYPE_UNKNOWN) {
      TyIdx2TY_IDXMap[tyIdx] = ty_idx;
    }
  }

  for (idx++; idx < GlobalTables::GetTypeTable().typeTable.size(); idx++) {
    TyIdx tyIdx(idx);
    MIRType *type = GlobalTables::GetTypeTable().GetTypeFromTyIdx(tyIdx);
    std::string typeName = type->GetName();
    switch (type->typeKind) {
      case kTypeByName: continue;
      case kTypePointer: {
        MIRPtrType *ptrType = static_cast<MIRPtrType *>(type);
        TY &ty = New_TY(ty_idx);
        if (typeName.empty())
          typeName = std::string("anon_ptr.");
        TY_Init(ty, Pointer_Size, KIND_POINTER, Pointer_Mtype, Save_Str(typeName.c_str()));
        Is_True(ptrType->pointedTyIdx.GetIdx() && TyIdx2TY_IDX(ptrType->pointedTyIdx),
                ("bad pointed ty"));
        Set_TY_pointed(ty, TyIdx2TY_IDX(ptrType->pointedTyIdx));
        Set_TY_align(ty_idx, Pointer_Size);
        if (ptrType->typeAttrs.GetAttr(ATTR_volatile))
          Set_TY_is_volatile(ty_idx);
        if (ptrType->typeAttrs.GetAttr(ATTR_const))
          Set_TY_is_const(ty_idx);
        if (ptrType->typeAttrs.GetAttr(ATTR_restrict))
          Set_TY_is_restrict(ty_idx);
        break;
      }
      case kTypeArray: {
        MIRArrayType *arrayType = static_cast<MIRArrayType *>(type);
        TY_IDX elem_ty_idx = TyIdx2TY_IDX(arrayType->eTyIdx);
        MIRType *elemType = arrayType->GetElemType();
        ARB_HANDLE arb, arb_first;
        for (UINT i = 0; i < arrayType->dim; i++) {
          arb = New_ARB();
          if (i == 0)
            arb_first = arb;
          ARB_Init(arb, 0, arrayType->sizeArray[i]-1, elemType->GetSize());
          Set_ARB_dimension(arb, arrayType->dim - i);
        }
        Set_ARB_last_dimen(arb);
        Set_ARB_first_dimen(arb_first);

        TY &ty = New_TY(ty_idx);
        TY_Init (ty, arrayType->GetSize(), KIND_ARRAY, MTYPE_M, Save_Str(typeName.c_str()));
        if (typeName.empty())
          Set_TY_anonymous(ty);
        Set_TY_etype (ty, elem_ty_idx);
        Set_TY_arb (ty, arb_first);
        Set_TY_align_exp(ty_idx, TY_align_exp(elem_ty_idx));
        break;
      }
      case kTypeStruct:
      case kTypeUnion: {
        MIRStructType *structType = static_cast<MIRStructType *>(type);
        size_t byteOfst = 0;
        size_t bitOfst = 0;

        FLD_IDX first_field_idx = Fld_Table.size();
        for (uint32 i = 0; i < structType->fields.size(); i++) {
          FieldPair *curField = &structType->fields[i];
          std::string fieldName = GlobalTables::GetStrTable().GetStringFromStrIdx(curField->first);
          MIRType *fieldType = structType->GetElemType(i);
          TY_IDX field_ty_idx = TyIdx2TY_IDX(curField->second.first);
          FieldAttrs fieldAttrs = curField->second.second;
          if (fieldAttrs.GetAttr(FLDATTR_volatile))
            Set_TY_is_volatile(field_ty_idx);
          if (fieldAttrs.GetAttr(FLDATTR_restrict))
            Set_TY_is_restrict(field_ty_idx);
          if (fieldAttrs.GetAttr(FLDATTR_const))
            Set_TY_is_const(field_ty_idx);
          if (fieldAttrs.attrAlign)
            Set_TY_align_exp(field_ty_idx, fieldAttrs.attrAlign);

          // align this field
          if (fieldType->typeKind != kTypeBitField) {
            if (byteOfst * 8 < bitOfst) {
              byteOfst = (bitOfst >> 3) + 1;
            }
            byteOfst = RoundUp(byteOfst, fieldType->GetAlign());
          } else {
            MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
            if (bitfType->fieldSize == 0) {  // special case, for aligning purpose
              bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
              byteOfst = bitOfst >> 3;
            } else {
              if (RoundDown(bitOfst + bitfType->fieldSize - 1, GetPrimTypeBitSize(bitfType->primType)) !=
                  RoundDown(bitOfst, GetPrimTypeBitSize(bitfType->primType))) {
                bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
                byteOfst = bitOfst >> 3;
              }
            }
          }
          Is_True(byteOfst + TY_size(field_ty_idx) <= structType->GetSize(), ("bad ofst"));

          FLD_HANDLE fld = New_FLD();
          FLD_Init(fld, Save_Str(fieldName.c_str()), field_ty_idx, type->typeKind == kTypeUnion ? 0 : byteOfst);
          if (fieldType->typeKind == kTypeBitField) {
            Set_FLD_is_bit_field(fld);
            Set_FLD_bofst(fld, bitOfst % 8); // bofset is remaining bits from byte offset
            Set_FLD_bsize(fld, static_cast<MIRBitfieldType *>(fieldType)->fieldSize);
          }
          if (fieldName.empty())
            Set_FLD_is_anonymous(fld);

          // update byteOfst and bitOfst
          if (fieldType->typeKind != kTypeBitField) {
            byteOfst += fieldType->GetSize();
            bitOfst = byteOfst * 8;
          } else {
            bitOfst += static_cast<MIRBitfieldType *>(fieldType)->fieldSize;
            byteOfst = bitOfst >> 3;
          }
        }
        FLD_IDX last_field_idx = Fld_Table.size() - 1;

        TY &ty = New_TY(ty_idx);
        TY_Init(ty, structType->GetSize(), KIND_STRUCT, MTYPE_M, Save_Str(typeName.c_str()));
        if (typeName.empty())
          Set_TY_anonymous(ty);
        if (structType->typeKind == kTypeUnion)
          Set_TY_is_union(ty_idx);
        if (last_field_idx >= first_field_idx) {
          Set_TY_fld(ty, FLD_HANDLE(first_field_idx));
          Set_FLD_last_field(FLD_HANDLE(last_field_idx));
        } else {
          Set_TY_fld(ty, FLD_HANDLE());
        }
        break;
      }
      case kTypeFunction: {
        MIRFuncType *funcType = static_cast<MIRFuncType *>(type);
        TY &ty = New_TY(ty_idx);
        TY_Init(ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, 0);
        Set_TY_align(ty_idx, 1);
        TY_IDX ret_ty_idx = TyIdx2TY_IDX(funcType->retTyIdx);
        TYLIST tylist_idx;
        Set_TYLIST_type(New_TYLIST(tylist_idx), ret_ty_idx);
        Set_TY_tylist (ty, tylist_idx);
        for (TyIdx parmTyIdx : funcType->paramTypeList) {
          TY_IDX arg_ty_idx = TyIdx2TY_IDX(parmTyIdx);
          Set_TYLIST_type(New_TYLIST(tylist_idx), arg_ty_idx);
        }
        Set_TYLIST_type(New_TYLIST(tylist_idx), 0);
        if (funcType->isVarArgs)
          Set_TY_is_varargs(ty);
        break;
      }
      default: FmtAssert(FALSE, ("TranslateTypes: NYI %s", typeName.c_str()));
    }
    TyIdx2TY_IDXMap[tyIdx] = ty_idx;
  }

  if (hasForwardTypeReference) {  // one more round to fix forward type refs
  }
}

PREG_NUM MPL2whirl::PregIdx2PREG_NUM(PregIdx pidx) {
  std::unordered_map<PregIdx, PREG_NUM>::iterator it = PregIdx2PREG_NUMMap.find(pidx);
  if (it != PregIdx2PREG_NUMMap.end())
    return it->second;
  MIRPreg *preg = func->pregTab->PregFromPregIdx(pidx);
  PREG_NUM pregnum = Create_Preg(PrimType2whirl(preg->primType), NULL);
  PregIdx2PREG_NUMMap[pidx] = pregnum;
  return pregnum;
}

LABEL_IDX MPL2whirl::LabelIdx2LABEL_IDX(LabelIdx lidx) {
  std::unordered_map<LabelIdx, LABEL_IDX>::iterator it = LabelIdx2LABEL_IDXMap.find(lidx);
  if (it != LabelIdx2LABEL_IDXMap.end())
    return it->second;
  LABEL_IDX lab_idx;
  LABEL &lab = New_LABEL(Current_scope, lab_idx);
  std::string labelName = func->labelTab->GetName(lidx);
  if (!labelName.empty())
    Set_LABEL_name_idx(lab, Save_Str(labelName.c_str()));
  LabelIdx2LABEL_IDXMap[lidx] = lab_idx;
  return lab_idx;
}

static ST_SCLASS StorageClass2whirl(MIRStorageClass sc) {
  switch (sc) {
  case kScInvalid: return SCLASS_UNKNOWN;
  case kScAuto: return SCLASS_AUTO;
  case kScFormal: return SCLASS_FORMAL;
  case kScPstatic: return SCLASS_PSTATIC;
  case kScFstatic: return SCLASS_FSTATIC;
  case kScGlobal: return SCLASS_COMMON; // SCLASS_DGLOBAL SCLASS_UGLOBAL:
  case kScExtern: return SCLASS_EXTERN;
  case kScText: return SCLASS_TEXT;
  case kScEHRegionSupp: return SCLASS_EH_REGION_SUPP;
  default: FmtAssert(FALSE, ("StorageClass2whirl: NYI %d", sc));
  }
  return SCLASS_UNKNOWN;
}

void MPL2whirl::MIRSymbol2whirl(MIRSymbol *sym) {
  std::string symName = sym->GetName();
  ST *st = NULL;
  ST_EXPORT eclass = sym->IsGlobal() ? EXPORT_PREEMPTIBLE : EXPORT_LOCAL;
  TY_IDX ty_idx = TyIdx2TY_IDX(sym->tyIdx);
  switch (sym->sKind) {
  case kStVar: {
    st = New_ST(sym->IsGlobal() ? GLOBAL_SYMTAB : GLOBAL_SYMTAB+1);
    if (sym->IsVolatile())
      Set_TY_is_volatile(ty_idx);
    if (sym->typeAttrs.GetAttr(ATTR_restrict))
      Set_TY_is_restrict(ty_idx);
    if (sym->typeAttrs.GetAttr(ATTR_const))
      Set_TY_is_const(ty_idx);
    ST_Init(st, Save_Str(symName.c_str()), CLASS_VAR, StorageClass2whirl(sym->storageClass), eclass, ty_idx);
    if (sym->IsWeak())
      Set_ST_is_weak_symbol(st);
    if (sym->typeAttrs.GetAttr(ATTR_const))
      Set_ST_is_const_var(st);
    break;
  }
  case kStFunc: {
    st = New_ST(GLOBAL_SYMTAB);
    MIRFunction *fn = sym->GetFunction();
    PU_IDX pu_idx;
    PU& pu = New_PU(pu_idx);
    PU_Init(pu, ty_idx, GLOBAL_SYMTAB+1);
    if (fn->GetAttr(FUNCATTR_static))
      eclass = EXPORT_LOCAL;
    ST_Init(st, Save_Str(symName.c_str()), CLASS_FUNC, StorageClass2whirl(sym->storageClass), eclass, ty_idx);
    Set_ST_pu(st, pu_idx);
    if (sym->IsWeak() || fn->GetAttr(FUNCATTR_weak))
      Set_ST_is_weak_symbol(st);
    break;
  }
  default: FmtAssert(FALSE, ("MIRSymbol2whirl: symbol kind %d NYI", sym->sKind));
    break;
  }
  StIdx2ST_IDXMap[sym->stIdx] = ST_st_idx(*st);
}

void MPL2whirl::TranslateGlobalSymbols() {
  for (StIdx stIdx : mod->symbolDefOrder) {
    MIRSymbol *sym = GlobalTables::GetGsymTable().GetSymbolFromStIdx(stIdx.Idx());
    if (sym->IsDeleted() || sym->storageClass == kScUnused ||
        (sym->isImported && !sym->appearsincode))
      continue;
    MIRSymbol2whirl(sym);
  }
}

static OPERATOR  Opcode2whirl(Opcode op) {
  switch (op) {
  case OP_cvt: return OPR_CVT;
  case OP_retype: return OPR_TAS;
  case OP_neg: return OPR_NEG;
  case OP_bnot: return OPR_BNOT;
  case OP_lnot: return OPR_LNOT;
  case OP_abs: return OPR_ABS;
  case OP_recip: return OPR_RECIP;
  case OP_sqrt: return OPR_SQRT;
  case OP_iaddrof: return OPR_ILDA;
  case OP_round: return OPR_RND;
  case OP_ceil: return OPR_CEIL;
  case OP_floor: return OPR_FLOOR;
  case OP_trunc: return OPR_TRUNC;
  case OP_extractbits: return OPR_EXTRACT_BITS;
  case OP_alloca: return OPR_ALLOCA;
  case OP_add: return OPR_ADD;
  case OP_sub: return OPR_SUB;
  case OP_mul: return OPR_MPY;
  case OP_div: return OPR_DIV;
  case OP_rem: return OPR_REM;
  case OP_max: return OPR_MAX;
  case OP_min: return OPR_MIN;
  case OP_band: return OPR_BAND;
  case OP_bior: return OPR_BIOR;
  case OP_bxor: return OPR_BXOR;
  case OP_land: return OPR_LAND;
  case OP_lior: return OPR_LIOR;
  case OP_cand: return OPR_CAND;
  case OP_cior: return OPR_CIOR;
  case OP_shl: return OPR_SHL;
  case OP_ashr: return OPR_ASHR;
  case OP_lshr: return OPR_LSHR;
  case OP_eq: return OPR_EQ;
  case OP_ne: return OPR_NE;
  case OP_lt: return OPR_LT;
  case OP_gt: return OPR_GT;
  case OP_le: return OPR_LE;
  case OP_ge: return OPR_GE;
  case OP_depositbits: return OPR_COMPOSE_BITS;
  case OP_select: return OPR_SELECT;
  case OP_while: return OPR_WHILE_DO;
  case OP_dowhile: return OPR_DO_WHILE;
  case OP_brtrue: return OPR_TRUEBR;
  case OP_brfalse: return OPR_FALSEBR;
  default: ;
  }
  return OPERATOR_UNKNOWN;
}

static INTRINSIC IntrinsicID2whirl(MIRIntrinsicID intrn) {
  switch (intrn) {
  case INTRN_C_strcmp: return INTRN_STRCMP;
  case INTRN_C_strncmp: return INTRN_STRNCMP;
  case INTRN_C_strcpy: return INTRN_STRCPY;
  case INTRN_C_strncpy: return INTRN_STRNCPY;
  case INTRN_C_strlen: return INTRN_STRLEN;
  case INTRN_C_memcmp: return INTRN_MEMCMP;
  case INTRN_C_memcpy: return INTRN_MEMCPY;
  case INTRN_C_memmove: return INTRN_MEMMOVE;
  case INTRN_C_memset: return INTRN_MEMSET;

  case INTRN_C_acosf: return INTRN_F4ACOS;
  case INTRN_C_asinf: return INTRN_F4ASIN;
  case INTRN_C_atanf: return INTRN_F4ATAN;
  case INTRN_C_cosf: return INTRN_F4COS;
  case INTRN_C_coshf: return INTRN_F4COSH;
  case INTRN_C_expf: return INTRN_F4EXP;
  case INTRN_C_logf: return INTRN_F4LOG;
  case INTRN_C_log10f: return INTRN_F4LOG10;
  case INTRN_C_sinf: return INTRN_F4SIN;
  case INTRN_C_sinhf: return INTRN_F4SINH;
  case INTRN_C_acos: return INTRN_F8ACOS;
  case INTRN_C_asin: return INTRN_F8ASIN;
  case INTRN_C_atan: return INTRN_F8ATAN;
  case INTRN_C_cos: return INTRN_F8COS;
  case INTRN_C_cosh: return INTRN_F8COSH;
  case INTRN_C_exp: return INTRN_F8EXP;
  case INTRN_C_log: return INTRN_F8LOG;
  case INTRN_C_log10: return INTRN_F8LOG10;
  case INTRN_C_sin: return INTRN_F8SIN;
  case INTRN_C_sinh: return INTRN_F8SINH;

  case INTRN_C_ffs: return INTRN_I4FFS;

  case INTRN_C_va_start: return INTRN_VA_START;
  case INTRN_C_constant_p: return INTRN_CONSTANT_P;

#ifdef DYNAMICLANG
  case INTRN_JS_INIT_CONTEXT: return INTRN_JSR_INIT_CONTEXT;
  case INTRN_JS_REQUIRE: return INTRN_JSR_REQUIRE;
  case INTRN_JS_GET_BIOBJECT: return INTRN_JSR_GET_BIOBJECT;
  case INTRN_JS_GET_BISTRING: return INTRN_ISR_GET_BISTRING;
  case INTRN_JSOP_THIS: return INTRN_JSO_THIS;
  case INTRN_JSOP_ADD: return INTRN_JSO_ADD;
  case INTRN_JSOP_CONCAT: return INTRN_JSO_CONCAT;
  case INTRN_JSOP_STRICTEQ: return INTRN_JSO_STRICTEQ;
  case INTRN_JSSTR_STRICTEQ: return INTRN_JSO_STREQ;
  case INTRN_JSOP_STRICTNE: return INTRN_JSO_STRICTNE;
  case INTRN_JSSTR_STRICTNE: return INTRN_JSO_STRNE;
  case INTRN_JSOP_INSTANCEOF: return INTRN_JSO_INSTANCEOF;
  case INTRN_JSOP_IN: return INTRN_JSO_IN;
  case INTRN_JSOP_OR: return INTRN_JSO_OR;
  case INTRN_JSOP_AND: return INTRN_JSO_AND;
  case INTRN_JSOP_TYPEOF: return INTRN_JSO_TYPEOF;
  case INTRN_JS_NEW: return INTRN_JSR_NEW;
  case INTRN_JS_STRING: return INTRN_JSR_STRING;
  case INTRN_JSSTR_LENGTH: return INTRN_JSO_STRLEN;
  case INTRN_JS_BOOLEAN: return INTRN_JSR_BOOLEAN;
  case INTRN_JS_NUMBER: return INTRN_JSR_NUMBER;
  case INTRN_JS_INT32: return INTRN_JSR_INT32;
  case INTRN_JS_REGEXP: return INTRN_JSR_REGEXP;
  case INTRN_JS_PRINT: return INTRN_JSR_PRINT;
  case INTRN_JS_ERROR: return INTRN_JSR_ERROR;
  case INTRN_JS_EVAL: return INTRN_JSR_EVAL;
  case INTRN_JS_ICALL: return INTRN_JSR_ICALL;
  case INTRN_JSOP_CALL: return INTRN_JSO_CALL;
  case INTRN_JSOP_CCALL: return INTRN_JSO_CCALL;
  case INTRN_JSOP_NEW: return INTRN_JSO_NEW;
  case INTRN_JS_SETTIMEOUT: return INTRN_JSR_SETTIMEOUT;
  case INTRN_JS_SETCYCLEHEADER: return INTRN_JSR_SETCYCLEHEADER;
  case INTRN_JS_NEW_OBJECT_0: return INTRN_JSR_NEW_OBJECT_0;
  case INTRN_JS_NEW_OBJECT_1: return INTRN_JSR_NEW_OBJECT_1;
  case INTRN_JSOP_SETPROP: return INTRN_JSO_SETPROP;
  case INTRN_JSOP_CALLPROP: return INTRN_JSO_CALLPROP;
  case INTRN_JSOP_GETPROP: return INTRN_JSO_GETPROP;
  case INTRN_JSOP_DELPROP: return INTRN_JSO_DELPROP;
  case INTRN_JSOP_SETPROP_BY_NAME: return INTRN_JSO_SETPROP_BY_NAME;
  case INTRN_JSOP_GETPROP_BY_NAME: return INTRN_JSO_GETPROP_BY_NAME;
  case INTRN_JSOP_SETPROP_BY_INDEX: return INTRN_JSO_SETPROP_BY_INDEX;
  case INTRN_JSOP_GETPROP_BY_INDEX: return INTRN_JSO_GETPROP_BY_INDEX;
  case INTRN_JSOP_INITPROP_BY_NAME: return INTRN_JSO_INITPROP_BY_NAME;
  case INTRN_JSOP_INITPROP_GETTER: return INTRN_JSO_INITPROP_GETTER;
  case INTRN_JSOP_INITPROP_SETTER: return INTRN_JSO_INITPROP_SETTER;
  case INTRN_JS_NEW_FUNCTION: return INTRN_JSR_NEW_FUNCTION;
  case INTRN_JS_NEW_ARR_ELEMS: return INTRN_JSR_NEW_ARR_ELEMS;
  case INTRN_JS_NEW_ARR_LENGTH: return INTRN_JSR_NEW_ARR_LENGTH;
  case INTRN_JSOP_LENGTH: return INTRN_JSO_LENGTH;
  case INTRN_JSOP_NEW_ITERATOR: return INTRN_JSO_NEW_ITERATOR;
  case INTRN_JSOP_NEXT_ITERATOR: return INTRN_JSO_NEXT_ITERATOR;
  case INTRN_JSOP_MORE_ITERATOR: return INTRN_JSO_MORE_ITERATOR;
  case INTRN_JS_ADDSYSEVENTLISTENER: return INTRN_JSR_ADDSYSEVENTLISTENER;
#endif

  default: {
    IntrinDesc *intrinDesc = &IntrinDesc::intrintable[intrn];
    FmtAssert(FALSE, ("IntrinsicID2whirl: %s NYI", intrinDesc->name));
    return INTRINSIC_NONE;
  }
  }
}

WN *MPL2whirl::SimulateIntrinsicCall(IntrinsiccallNode *x, TYPE_ID rty) {
  WN *opnd0 = NULL, *opnd1 = NULL, *res = NULL;
  switch (x->intrinsic) {
  case INTRN_JSOP_ADD:
    Is_True(x->NumOpnds() == 2, ("bad opnds"));
    opnd0 = ExprNode2whirl(x->Opnd(0));
    if (WN_rtype(opnd0) != rty)
      opnd0 = WN_Cvt(WN_rtype(opnd0), rty, opnd0);
    opnd1 = ExprNode2whirl(x->Opnd(1));
    if (WN_rtype(opnd1) != rty)
      opnd1 = WN_Cvt(WN_rtype(opnd1), rty, opnd1);
    if (x->tyIdx != GetStrTy())
      res = WN_Add(rty, opnd0, opnd1);
  default:
    break;
  }
  if (res == NULL)
    return NULL;
  if (kOpcodeInfo.IsCallAssigned(x->op) &&
      !x->GetCallReturnVector()->empty()) {
    CallReturnPair crp = x->GetCallReturnVector()->front();
    if (crp.second.IsReg()) {
      PREG_NUM preg = PregIdx2PREG_NUM(crp.second.pregIdx);
      res = WN_StidPreg(rty, preg, res);
    }
    else {
      ST_IDX st_idx = StIdx2ST_IDX(crp.first);
      ST *st = ST_ptr(st_idx);
      res = WN_Stid(rty, 0, st, ST_type(st), res);
    }
  }
  OPERATOR opr = WN_operator(res);
  return OPERATOR_is_expression(opr) ? WN_CreateEval(res)
                                     : res;
}

WN *MPL2whirl::ConvertJsopCall(IntrinsiccallNode *x, TYPE_ID ret_mtype) {
  MIRPtrType *funcPtrType = static_cast<MIRPtrType *>(GetFuncPtrType(x->Opnd(0)));
  WN *res = WN_Icall(ret_mtype, MTYPE_V, x->NumOpnds(), TyIdx2TY_IDX(funcPtrType->tyIdx));
  uint32 i;
  for (i = 1; i < x->NumOpnds(); i++) {
    WN *wn = ExprNode2whirl(x->Opnd(i));
    WN_kid(res, i-1) = WN_CreateParm(WN_rtype(wn), wn, MTYPE_To_TY(WN_rtype(wn)), WN_PARM_BY_VALUE);
  }
  WN_kid(res, i-1) = ExprNode2whirl(x->Opnd(0));
  return res;
}

WN *MPL2whirl::ExprNode2whirl(BaseNode *x) {
  WN *wn = NULL;
  switch (x->op) {
  // leaf nodes
  case OP_constval: {
    ConstvalNode *cNode = static_cast<ConstvalNode *>(x);
    TCON tcon;
    MIRConst *mirconst = cNode->constVal;
    switch (mirconst->kind) {
    case kConstInt:
      return WN_CreateIntconst(OPR_INTCONST, PrimType2whirl(x->primType), MTYPE_V, static_cast<MIRIntConst *>(mirconst)->value);
    case kConstFloatConst: {
      MIRFloatConst *floatConst = static_cast<MIRFloatConst *>(mirconst);
      tcon = Host_To_Targ_Float_4(MTYPE_F4, floatConst->GetValue());
      ST *st = New_Const_Sym(Enter_tcon(tcon), TyIdx2TY_IDX(TyIdx(PTY_f32)));
      return WN_CreateConst(OPR_CONST, MTYPE_F4, MTYPE_V, st);
    }
    case kConstDoubleConst: {
      MIRDoubleConst *doubleConst = static_cast<MIRDoubleConst *>(mirconst);
      tcon = Host_To_Targ_Float(MTYPE_F8, doubleConst->GetValue());
      ST *st = New_Const_Sym(Enter_tcon(tcon), TyIdx2TY_IDX(TyIdx(PTY_f64)));
      return WN_CreateConst(OPR_CONST, MTYPE_F8, MTYPE_V, st);
    }
    default: 
      FmtAssert(FALSE, ("ExprNode2whirl: MIRConst kind NYI"));
      return NULL;
    }
  }
  case OP_sizeoftype: {
    return WN_Intconst(MTYPE_I4, GetPrimTypeSize(x->primType));
  }
  case OP_conststr: {
    ConststrNode *cNode = static_cast<ConststrNode *>(x);
    std::string kStr = GlobalTables::GetUStrTable().GetStringFromStrIdx(cNode->strIdx);
    TCON tcon = Host_To_Targ_String(MTYPE_STRING, kStr.c_str(), kStr.size());
    ST *st = New_Const_Sym(Enter_tcon(tcon), TyIdx2TY_IDX(TyIdx(PTY_ptr)));
    return WN_Lda(Pointer_Mtype, ST_ofst(st), st);
  }
  case OP_addroflabel: {
    AddroflabelNode *addroflab = static_cast<AddroflabelNode *>(x);
    LABEL_IDX label_idx = LabelIdx2LABEL_IDX(addroflab->offset);
    return WN_LdaLabel(Pointer_Mtype, label_idx);
  }
  case OP_addroffunc: {
    AddroffuncNode *addroffunc = static_cast<AddroffuncNode *>(x);
    MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(addroffunc->puIdx);
    ST_IDX func_st_idx = StIdx2ST_IDX(func->stIdx);
    ST *func_st = &St_Table[func_st_idx];
    TY_IDX func_ty_idx = TyIdx2TY_IDX(func->funcType->tyIdx);
    Is_True(func_ty_idx, ("bad function type"));
    return WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0/*offset*/, Make_Pointer_Type(func_ty_idx), func_st, 0);
  }
  case OP_addrof:
  case OP_dread: {
    AddrofNode *drNode = static_cast<AddrofNode *>(x);
    ST_IDX st_idx = StIdx2ST_IDX(drNode->stIdx);
    ST *st = &St_Table[st_idx];
    TY_IDX ty_idx = st->u2.type;
    Is_True(ty_idx, ("bad type"));
    if (x->op == OP_addrof)
      return WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0/*offset*/, Make_Pointer_Type(ty_idx), st, drNode->fieldID);
    TY *ty = &Ty_Table[ty_idx];
    Is_True(TY_mtype(*ty) != 0, ("Bad mtype"));
    return WN_Ldid(TY_mtype(*ty), 0/*offset*/, st_idx, ty_idx, drNode->fieldID);
  }
  case OP_regread: {
    RegreadNode *regread = static_cast<RegreadNode *>(x);
    if (regread->regIdx > 0)
      return WN_LdidPreg(PrimType2whirl(regread->primType), PregIdx2PREG_NUM(regread->regIdx));
    if (regread->regIdx == kSregRetval0)
      return WN_LdidPreg(PrimType2whirl(regread->primType), -1);
    // TODO: hack here
    return WN_LdidPreg(PrimType2whirl(regread->primType), regread->regIdx);
    FmtAssert(FALSE, ("ExprNode2whirl: special preg %d NYI", regread->regIdx));
    return NULL;
  }
  // unary nodes
  case OP_neg:
  case OP_bnot:
  case OP_lnot:
  case OP_abs:
  case OP_recip:
  case OP_sqrt: {
    UnaryNode *uNode = static_cast<UnaryNode *>(x);
    wn = ExprNode2whirl(uNode->uOpnd);
    return WN_CreateExp1(Opcode2whirl(x->op), Mtype_comparison(PrimType2whirl(x->primType)),
                         MTYPE_V, wn);
  }
  case OP_round:
  case OP_ceil:
  case OP_floor:
  case OP_trunc:
  case OP_cvt: {
    TypeCvtNode *cvtNode = static_cast<TypeCvtNode *>(x);
    wn = ExprNode2whirl(cvtNode->uOpnd);
    TYPE_ID fromTy = PrimType2whirl(cvtNode->fromPrimType);
    TYPE_ID toTy = PrimType2whirl(x->primType);
    if (x->op == OP_cvt &&
        !Is_Valid_Opcode(OPCODE_make_op_MACRO(OPR_CVT, toTy, fromTy))) {
      return wn;
    }
    return WN_CreateExp1(Opcode2whirl(x->op), toTy, fromTy, wn);
  }
  case OP_sext:
  case OP_zext:
  case OP_extractbits: {
    ExtractbitsNode *exNode = static_cast<ExtractbitsNode *>(x);
    wn = ExprNode2whirl(exNode->uOpnd);
    if (x->op == OP_extractbits) {
      wn = WN_CreateExp1(OPR_EXTRACT_BITS, PrimType2whirl(x->primType), MTYPE_V, wn);
      WN_set_bit_offset_size(wn, exNode->bitsOffset, exNode->bitsSize);
      return wn;
    }
    else return WN_CreateCvtl(OPR_CVTL, PrimType2whirl(x->primType), MTYPE_V, exNode->bitsSize, wn);
  }
  case OP_retype: {
    RetypeNode *reNode = static_cast<RetypeNode *>(x);
    wn = ExprNode2whirl(reNode->uOpnd);
    return WN_Tas(PrimType2whirl(x->primType), TyIdx2TY_IDX(reNode->tyIdx), wn);
  }
  case OP_iaddrof: {
    IreadNode *iread = static_cast<IreadNode *>(x);
    wn = ExprNode2whirl(iread->uOpnd);
    wn = WN_CreateIlda(OPR_ILDA, Pointer_Mtype, MTYPE_V, 0/*offset*/, TyIdx2TY_IDX(iread->tyIdx));
    WN_set_field_id(wn, iread->fieldID);
    return wn;
  }
  case OP_iread: {
    IreadNode *iread = static_cast<IreadNode *>(x);
    wn = ExprNode2whirl(iread->uOpnd);
    MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
    MIRType *loadedType = ptrType->GetPointedType();
    MIRType *fieldType = loadedType;
    if (iread->fieldID != 0) {
      MIRStructType *structType = dynamic_cast<MIRStructType *>(loadedType);
      FmtAssert(structType, ("ExprNode2whirl: non-zero field id not associated with struct type")); 
      fieldType = structType->GetFieldType(iread->fieldID);
    }
    TYPE_ID desc = PrimType2whirl(fieldType->primType);
    return WN_CreateIload(OPR_ILOAD, Mtype_comparison(desc), desc,
        0/*offset*/, TyIdx2TY_IDX(loadedType->tyIdx), TyIdx2TY_IDX(iread->tyIdx), wn, iread->fieldID);
  }
  // binary nodes
  case OP_add:
  case OP_sub:
  case OP_mul:
  case OP_div:
  case OP_rem:
  case OP_max:
  case OP_min:
  case OP_band:
  case OP_bior:
  case OP_bxor:
  case OP_cand:
  case OP_cior:
  case OP_land:
  case OP_lior:
  case OP_shl:
  case OP_ashr:
  case OP_lshr: {
    BinaryNode *bNode = static_cast<BinaryNode *>(x);
    wn = ExprNode2whirl(bNode->bOpnd[0]);
    WN *wn1 = ExprNode2whirl(bNode->bOpnd[1]);
    return WN_CreateExp2(Opcode2whirl(x->op), PrimType2whirl(x->primType), MTYPE_V, wn, wn1);
  }
  case OP_eq:
  case OP_ne:
  case OP_ge:
  case OP_gt:
  case OP_le:
  case OP_lt: {
    CompareNode *bNode = static_cast<CompareNode *>(x);
    wn = ExprNode2whirl(bNode->bOpnd[0]);
    WN *wn1 = ExprNode2whirl(bNode->bOpnd[1]);
    return WN_CreateExp2(Opcode2whirl(x->op), MTYPE_U4,
                         Mtype_comparison(PrimType2whirl(bNode->opndType)), wn, wn1);
  }
  case OP_depositbits: {
    DepositbitsNode *bNode = static_cast<DepositbitsNode *>(x);
    wn = ExprNode2whirl(bNode->bOpnd[0]);
    WN *wn1 = ExprNode2whirl(bNode->bOpnd[1]);
    wn =  WN_CreateExp2(OPR_COMPOSE_BITS, PrimType2whirl(x->primType), MTYPE_V, wn, wn1);
    WN_set_bit_offset_size(wn, bNode->bitsOffset, bNode->bitsSize);
    return wn;
  }
  // n-ary
  case OP_array: {
    ArrayNode *arrNode = static_cast<ArrayNode *>(x);
    MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrNode->tyIdx));
    MIRArrayType *arrType = dynamic_cast<MIRArrayType *>(ptrType->GetPointedType());
    FmtAssert(arrType, ("ExprNode2whirl:: cannot get MIRArrayType"));
    size_t numDim = arrNode->NumOpnds() -1;
    WN *arraywn = WN_Create(OPR_ARRAY, Pointer_Mtype, MTYPE_V, numDim * 2 + 1);
    WN_kid(arraywn, 0) = ExprNode2whirl(arrNode->Opnd(0));
    size_t i;
    for (i = 0; i < numDim; i++) {
      WN_kid(arraywn, i+1) = WN_CreateIntconst(OPR_INTCONST, MTYPE_U4, MTYPE_V, arrType->sizeArray[i]);
    }
    for (i = 0; i < numDim; i++) {
      WN_kid(arraywn, numDim+i+1) = ExprNode2whirl(arrNode->Opnd(i+1));
    }
    MIRType *elemType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrType->eTyIdx);
    size_t elemSize = elemType->GetSize();
    for (; i < arrType->dim; i++) {
      elemSize *= arrType->sizeArray[i];
    }
    WN_element_size(arraywn) = elemSize;
    return arraywn;
  }
  case OP_intrinsicop: {
    IntrinsicopNode *intrnop = static_cast<IntrinsicopNode *>(x);
    TYPE_ID ret_ty = Mtype_comparison(PrimType2whirl(intrnop->primType));
    wn = WN_Create(OPR_INTRINSIC_OP, ret_ty, MTYPE_V, intrnop->NumOpnds());
    WN_intrinsic(wn) = IntrinsicID2whirl(intrnop->intrinsic);
    for (uint32 i = 0; i < intrnop->NumOpnds(); i++) {
      WN *wn0 = ExprNode2whirl(intrnop->Opnd(i));
      WN_kid(wn, i) = WN_CreateParm(WN_rtype(wn0), wn0, MTYPE_To_TY(WN_rtype(wn0)), WN_PARM_BY_VALUE);
    }
    return wn;
  }
  default: 
    FmtAssert(FALSE, ("ExprNode2whirl: OP_%s NYI", kOpcodeInfo.GetName(x->op)));
    return NULL;
  }
}

// x is an expression that computes to a function pointer; find and return the
// type of the function pointer
MIRType *MPL2whirl::GetFuncPtrType(BaseNode *x) {
  switch (x->op) {
  case OP_addroffunc: {
    AddroffuncNode *addroffunc = static_cast<AddroffuncNode *>(x);
    MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(addroffunc->puIdx);
    return GlobalTables::GetTypeTable().GetOrCreatePointerType(func->funcType->tyIdx, PTY_ptr);
  }
  case OP_dread: {
    AddrofNode *drNode = static_cast<AddrofNode *>(x);
    MIRSymbol *st = func->GetLocalOrGlobalSymbol(drNode->stIdx);
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(st->tyIdx);
  }
  case OP_iread: {
    IreadNode *iread = static_cast<IreadNode *>(x);
    MIRPtrType *ptrtype = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx));
    return ptrtype->GetPointedType();
  }
  case OP_intrinsicop: return GlobalTables::GetTypeTable().GetTypeFromTyIdx(GetObjTy());
  case OP_select: return GetFuncPtrType(x->Opnd(1));
  case OP_cvt: return GetFuncPtrType(static_cast<UnaryNode*>(x)->uOpnd);
  default: FmtAssert(FALSE, ("GetFuncPtrType: cannot find icall func ptr type"));
  }
}

static void SrcPosition2whirl(SrcPosition srcposition, WN *wn) {
  USRCPOS usrcpos;
  usrcpos.fillers[0] = srcposition.RawData();
  usrcpos.fillers[1] = srcposition.Linenum();
  WN_Set_Linenum(wn, usrcpos.srcpos);
}

void MPL2whirl::StmtNode2whirl(StmtNode *stmt, WN *blk) {
  WN *wn = NULL;
  WN *stmtwn = NULL;
  TYPE_ID ret_mtype = MTYPE_UNKNOWN;
  switch (stmt->op) {
  case OP_dassign: {
    DassignNode *dass = static_cast<DassignNode *>(stmt);
    WN *rhswn = ExprNode2whirl(dass->uOpnd);
    ST_IDX st_idx = StIdx2ST_IDX(dass->stIdx);
    ST *st = &St_Table[st_idx];
    TY_IDX ty_idx = st->u2.type;
    TY *ty = &Ty_Table[ty_idx];
    stmtwn =  WN_CreateStid(OPR_STID, MTYPE_V, TY_mtype(*ty), 0/*offset*/, st, ty_idx, rhswn, dass->fieldID);
    break;
  }
  case OP_regassign: {
    RegassignNode *regass = static_cast<RegassignNode *>(stmt);
    WN *rhswn = ExprNode2whirl(regass->uOpnd);
    if (regass->regIdx > 0) {
      TYPE_ID mtype = PrimType2whirl(regass->primType);
      ST *pregst = MTYPE_To_PREG(mtype);
      stmtwn = WN_Stid(mtype, PregIdx2PREG_NUM(regass->regIdx), pregst, MTYPE_To_TY(mtype), rhswn, 0);
    } else  {
      FmtAssert(FALSE, ("StmtNode2whirl: special preg %d NYI", regass->regIdx));
    }
    break;
  }
  case OP_iassign: {
    IassignNode *iass = static_cast<IassignNode *>(stmt);
    WN *rhswn = ExprNode2whirl(iass->rhs);
    WN *wn = ExprNode2whirl(iass->addrExpr);
    MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(iass->tyIdx));
    MIRType *destType = ptrType->GetPointedType();
    if (iass->fieldID != 0) {
      MIRStructType *structType = dynamic_cast<MIRStructType *>(destType);
      FmtAssert(structType, ("StmtNode2whirl: non-zero field id not associated with struct type")); 
      destType = structType->GetFieldType(iass->fieldID);
    }
    stmtwn = WN_Istore(PrimType2whirl(destType->primType), 0/*offset*/, 
        TyIdx2TY_IDX(iass->tyIdx), wn, rhswn, iass->fieldID);
    break;
  }
  case OP_goto: {
    GotoNode *gotoNode = static_cast<GotoNode *>(stmt);
    LABEL_IDX label_idx = LabelIdx2LABEL_IDX(gotoNode->offset);
    stmtwn = WN_CreateGoto(label_idx);
    break;
  }
  case OP_brtrue:
  case OP_brfalse: {
    CondGotoNode *cgoto = static_cast<CondGotoNode *>(stmt);
    wn = ExprNode2whirl(cgoto->uOpnd);
    LABEL_IDX label_idx = LabelIdx2LABEL_IDX(cgoto->offset);
    if (stmt->op == OP_brtrue)
      stmtwn = WN_CreateTruebr(label_idx, wn);
    else stmtwn = WN_CreateFalsebr(label_idx, wn);
    break;
  }
  case OP_label: {
    LabelNode *lnode = static_cast<LabelNode *>(stmt);
    LABEL_IDX lidx = LabelIdx2LABEL_IDX(lnode->labelIdx);
    stmtwn = WN_CreateLabel(lidx, 0, NULL);
    break;
  }
  case OP_eval: {
    UnaryStmtNode *uStmt = static_cast<UnaryStmtNode *>(stmt);
    wn = ExprNode2whirl(uStmt->uOpnd);
    stmtwn = WN_CreateEval(wn);
    break;
  }
  case OP_igoto: {
    UnaryStmtNode *uStmt = static_cast<UnaryStmtNode *>(stmt);
    wn = ExprNode2whirl(uStmt->uOpnd);
    stmtwn = WN_CreateAgoto(wn);
    break;
  }
  case OP_callassigned:
  case OP_call: {
    CallNode *cnode = static_cast<CallNode *>(stmt);
    MIRFunction *func = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(cnode->puIdx);
    ST_IDX func_st_idx = StIdx2ST_IDX(func->stIdx);
    ST *func_st = &St_Table[func_st_idx];
    MIRType *retType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(func->funcType->retTyIdx);
    ret_mtype = PrimType2whirl(retType->primType);
    stmtwn = WN_Create(OPR_CALL, ret_mtype, MTYPE_V, cnode->NumOpnds());
    WN_st_idx(stmtwn) = func_st_idx;
    for (uint32 i = 0; i < cnode->NumOpnds(); i++) {
      wn = ExprNode2whirl(cnode->Opnd(i));
      WN_kid(stmtwn, i) = WN_CreateParm(WN_rtype(wn), wn, MTYPE_To_TY(WN_rtype(wn)), WN_PARM_BY_VALUE);
    }
    break;
  }
  case OP_icall:
  case OP_icallassigned: {
    IcallNode *icallNode = static_cast<IcallNode *>(stmt);
    MIRPtrType *funcPtrType = static_cast<MIRPtrType *>(GetFuncPtrType(icallNode->Opnd(0)));
    MIRFuncType *funcType = static_cast<MIRFuncType *>(funcPtrType->GetPointedType());
    MIRType *retType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcType->retTyIdx);
    ret_mtype = PrimType2whirl(retType->primType);
    stmtwn = WN_Icall(PrimType2whirl(retType->primType), MTYPE_V, icallNode->NumOpnds(), TyIdx2TY_IDX(funcPtrType->tyIdx));
    for (uint32 i = 1; i < icallNode->NumOpnds(); i++) {
      wn = ExprNode2whirl(icallNode->Opnd(i));
      WN_kid(stmtwn, i-1) = WN_CreateParm(WN_rtype(wn), wn, MTYPE_To_TY(WN_rtype(wn)), WN_PARM_BY_VALUE);
    }
    WN_kid(stmtwn, icallNode->NumOpnds()-1) = ExprNode2whirl(icallNode->Opnd(0));
    break;
  }
  case OP_intrinsiccall:
  case OP_intrinsiccallassigned: {
    IntrinsiccallNode *intrncall = static_cast<IntrinsiccallNode *>(stmt);
    // not generate call so far
    if (intrncall->intrinsic == INTRN_JS_INIT_CONTEXT ||
        intrncall->intrinsic == INTRN_JS_NEW_FUNCTION)
      return;
    //MIRType *retType = intrncall->GetCallReturnType();
    //ret_mtype = PrimType2whirl(retType->primType);
    ret_mtype = Mtype_comparison(PrimType2whirl(intrncall->GetPrimType()));
    if (intrncall->intrinsic == INTRN_JSOP_CALL)
      stmtwn = ConvertJsopCall(intrncall, ret_mtype);
    else
      stmtwn = SimulateIntrinsicCall(intrncall, ret_mtype);
    if (stmtwn)
      break;
    stmtwn = WN_Create(OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, intrncall->NumOpnds());
    WN_intrinsic(stmtwn) = IntrinsicID2whirl(intrncall->intrinsic);
    for (uint32 i = 0; i < intrncall->NumOpnds(); i++) {
      wn = ExprNode2whirl(intrncall->Opnd(i));
      WN_kid(stmtwn, i) = WN_CreateParm(WN_rtype(wn), wn, MTYPE_To_TY(WN_rtype(wn)), WN_PARM_BY_VALUE);
    }
    break;
  }
  case OP_if: {
    IfStmtNode *ifStmt = static_cast<IfStmtNode *>(stmt);
    WN *test = ExprNode2whirl(ifStmt->uOpnd);
    WN *then_blk = WN_CreateBlock();
    StmtNode2whirl(ifStmt->thenPart, then_blk);
    WN *else_blk = NULL;
    if (ifStmt->elsePart != NULL) {
      else_blk = WN_CreateBlock();
      StmtNode2whirl(ifStmt->elsePart, else_blk);
    }
    stmtwn = WN_CreateIf(test, then_blk, else_blk);
    break;
  }
  case OP_dowhile:
  case OP_while: {
    WhileStmtNode *whileStmt = static_cast<WhileStmtNode *>(stmt);
    WN *test = ExprNode2whirl(whileStmt->uOpnd);
    WN *while_body = WN_CreateBlock();
    StmtNode2whirl(whileStmt->body, while_body);
    if (stmt->op == OP_while)
      stmtwn = WN_CreateWhileDo(test, while_body);
    else stmtwn = WN_CreateDoWhile(test, while_body);
    break;
  }
  case OP_switch: {
    SwitchNode *switchStmt = static_cast<SwitchNode *>(stmt);
    WN *test = ExprNode2whirl(switchStmt->switchOpnd);
    WN *case_block = WN_CreateBlock();
    for (CaseVector::iterator it = switchStmt->switchTable.begin();
         it != switchStmt->switchTable.end(); ++it) {
      LABEL_IDX label = LabelIdx2LABEL_IDX(it->second);
      WN *case_goto = WN_CreateCasegoto(it->first, label);
      SrcPosition2whirl(stmt->srcPosition, case_goto);
      WN_INSERT_BlockLast(case_block, case_goto);
    }
    LABEL_IDX def_lab = LabelIdx2LABEL_IDX(switchStmt->defaultLabel);
    LABEL_IDX exit_lab;
    New_LABEL(CURRENT_SYMTAB, exit_lab);
    WN *switch_wn = WN_CreateSwitch(switchStmt->switchTable.size(),
                                    test,
                                    case_block,
                                    WN_CreateGoto(def_lab),
                                    exit_lab);
    SrcPosition2whirl(stmt->srcPosition, switch_wn);
    WN *exit_lab_wn = WN_CreateLabel(ST_IDX_ZERO, exit_lab, 0, NULL);
    SrcPosition2whirl(stmt->srcPosition, switch_wn);
    stmtwn = WN_CreateBlock();
    WN_INSERT_BlockLast(stmtwn, switch_wn);
    WN_INSERT_BlockLast(stmtwn, exit_lab_wn);
    SrcPosition2whirl(stmt->srcPosition, stmtwn);
    WN_INSERT_BlockAfter(blk, WN_last(blk), stmtwn);
    return;
  }
  case OP_return: {
    NaryStmtNode *retStmt = static_cast<NaryStmtNode *>(stmt);
    WN *retwn = NULL;
    if (retStmt->NumOpnds() == 0)
      stmtwn = WN_CreateReturn();
    else {
      wn = ExprNode2whirl(retStmt->Opnd(0));
      stmtwn = WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(wn), MTYPE_V, wn);
    }
    break;
  }
  case OP_comment: {
    CommentNode *comment = static_cast<CommentNode *>(stmt);
    stmtwn = WN_CreateComment(comment->comment.c_str());
    break;
  }
  case OP_block: {
    BlockNode *blkNode = static_cast<BlockNode *>(stmt);
    for (StmtNode *s = blkNode->GetFirst(); s; s = s->GetNext()) {
      if (s->op == OP_intrinsiccallassigned &&
          ((IntrinsiccallNode *)s)->intrinsic == INTRN_JS_NEW_FUNCTION)
        s = s->GetNext()->GetNext();
      StmtNode2whirl(s, blk);
    }
    return;;
  }
  case OP_jscatch:
  case OP_jstry:
  case OP_cleanuptry:
  case OP_endtry:
  case OP_throw:
  case OP_finally:
  case OP_gosub:
  case OP_retsub:
    // TODO: handle jstry
    return;
  default: FmtAssert(FALSE, ("StmtNode2whirl: OP_%s NYI", kOpcodeInfo.GetName(stmt->op)));
    return;
  }
  SrcPosition2whirl(stmt->srcPosition, stmtwn);
  WN_INSERT_BlockAfter(blk, WN_last(blk), stmtwn);

  if (OPERATOR_is_call(WN_operator(stmtwn)) &&
      kOpcodeInfo.IsCallAssigned(stmt->op) &&
      !stmt->GetCallReturnVector()->empty()) {
    wn = WN_Ldid(ret_mtype, -1, Return_Val_Preg, MTYPE_To_TY(ret_mtype));
    CallReturnPair crp = stmt->GetCallReturnVector()->front();
    if (crp.second.IsReg()) {
      MIRPreg *preg = func->pregTab->PregFromPregIdx(crp.second.pregIdx);
      TYPE_ID mtype = PrimType2whirl(preg->primType);
      ST *pregst = MTYPE_To_PREG(mtype);
      stmtwn = WN_Stid(mtype, PregIdx2PREG_NUM(crp.second.pregIdx), pregst, MTYPE_To_TY(mtype), wn, 0);
    } else {
      ST_IDX st_idx = StIdx2ST_IDX(crp.first);
      ST *st = &St_Table[st_idx];
      TY_IDX ty_idx = st->u2.type;
      TY *ty = &Ty_Table[ty_idx];
      stmtwn = WN_CreateStid(OPR_STID, MTYPE_V, TY_mtype(*ty), 0/*offset*/, st, ty_idx, wn, crp.second.fieldID);
    }
    SrcPosition2whirl(stmt->srcPosition, stmtwn);
    WN_INSERT_BlockAfter(blk, WN_last(blk), stmtwn);
  }
}

void MPL2whirl::TranslateFunctionBodies() {
  for (MIRFunction *func : mod->functionList) {
    if (func->codeMemPool == nullptr || func->body == nullptr)
      continue;
    ST_IDX func_st_idx = StIdx2ST_IDX(func->stIdx);
    if (func_st_idx == 0)
      continue;
    this->func = func;
    this->mod->SetCurFunction(func);
    ST *func_st = &St_Table[func_st_idx];
    PU *pu = &Pu_Table[ST_pu(func_st)];
    switch (mod->srcLang) {
    case kSrcLangC: Set_PU_c_lang(*pu); break;
    case kSrcLangCPlusPlus: Set_PU_cxx_lang(*pu); break;
    case kSrcLangJs: Set_PU_c_lang(*pu); break;  // TODO
    case kSrcLangJava: Set_PU_java_lang(*pu); break;
    default: ;
    }
    if (func->GetAttr(FUNCATTR_inline)) {
      Set_PU_is_inline_function(*pu);
      Set_PU_is_marked_inline(*pu);
    }
    Current_pu = pu;
    Current_scope = pu->lexical_level;
    New_Scope(Current_scope, Malloc_Mem_Pool, TRUE);
    Scope_tab[Current_scope].st = func_st;
    PregIdx2PREG_NUMMap.clear();
    // local symbol table
    for (uint32 i = 1; i < func->symTab->GetSymbolTableSize(); i++) {
      MIRSymbol *s = func->symTab->GetSymbolFromStIdx(i);
      if (s->IsDeleted())
        continue;
      MIRSymbol2whirl(s);
    }

    WN *func_body = WN_CreateBlock();
    WN *entry_wn = WN_CreateEntry(func->formalDefVec.size(), func_st, func_body, NULL, NULL);
    for (uint32 j = 0; j < func->formalDefVec.size(); j++) {
      FormalDef *formalDef = &func->formalDefVec[j];
      WN_kid(entry_wn, j) = WN_CreateIdname(0, StIdx2ST_IDX(formalDef->formalSym->stIdx));
    }

    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, entry_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
    fn_dst = CreateFuncDst(func_st);
    Set_PU_Info_pu_dst (pu_info, fn_dst);
    Set_PU_Info_state (pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state (pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state (pu_info, WT_PROC_SYM, Subsect_InMem);
    Set_PU_Info_state (pu_info, WT_DEPGRAPH, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_PREFETCH, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_REGIONS, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_FEEDBACK, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_FREQ, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_AC_INTERNAL, Subsect_Missing);
    Set_PU_Info_state (pu_info, WT_ALIAS_CLASS, Subsect_Missing);
    if (strcmp(ST_name(func_st), "main") == 0) {
      Set_PU_is_mainpu(*pu);
      Set_PU_no_inline(*pu);
    }     
    if (PU_Info_Root != NULL)
      PU_Info_next(PU_Info_Last) = pu_info;
    else PU_Info_Root = pu_info;
    PU_Info_Last = pu_info;

    SrcPosition2whirl(func->srcPosition, func_body);
    for (StmtNode *s = func->body->GetFirst(); s; s = s->GetNext()) {
      if (s->op == OP_intrinsiccallassigned &&
          ((IntrinsiccallNode *)s)->intrinsic == INTRN_JS_NEW_FUNCTION)
        s = s->GetNext()->GetNext();
      StmtNode2whirl(s, func_body);
    }

    // deallocate the old map table
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    PregIdx2PREG_NUMMap.clear();
    LabelIdx2LABEL_IDXMap.clear();

    Write_PU_Info (pu_info);
    Delete_Scope(Current_scope);
  }
}

int main(int argc, char **argv) {
  if (argc != 5 ||
      (strcmp(argv[1], "-mpl") != 0 && strcmp(argv[1], "-bpl") != 0) ||
      (strcmp(argv[3], "-o") != 0)) {
    MIR_PRINTF("usage: ./mpl2whirl [ -mpl | -bpl ] <mpl_or_bpl_file> -o <whirl_file>\n");
    exit(1);
  }

  {
    bool ismpl = strcmp(argv[1], "-mpl") == 0;
    maple::MIRModule *themodule = new maple::MIRModule(argv[2]);
    std::string::size_type lastdot = themodule->fileName.find_last_of(".");
    // input the file
    if (ismpl) {
      maple::MIRParser theparser(*themodule);
      if (!theparser.ParseMIR()) {
        theparser.EmitError(themodule->fileName.c_str());
        return 1;
      }
    } else {
      BinaryMplImport binMplt(*themodule);
      binMplt.imported = false;
      std::string modid = themodule->fileName;
      if (!binMplt.Import(modid, true)) {
        ERR(kLncErr, "irbuild: cannot open .mplt or .bpl file: %s", modid.c_str());
        return 1;
      }
    }

    // translate to whirl
    MEM_Initialize();  // init memory
    Init_Error_Handler(100);
    Set_Error_File(NULL);
    const char *const phase_name = "Whirl Builder";
    Set_Error_Phase(phase_name);
    Set_Error_Line(ERROR_LINE_UNKNOWN);

    Irb_File_Name = argv[4];

    // initialize this whirl file
    Preconfigure();
//  Init_Controls_Tbl();
    Configure ();
    Pointer_Size = 8;  // this is because Configure_Target() was not called
    IR_reader_init();
    Initialize_Symbol_Tables (TRUE);
    Open_Output_Info(Irb_File_Name);
    DST_Init(NULL, 0);
    Set_Error_Phase(phase_name);

    // start translation
    MPL2whirl mpl2whirl(themodule);
    mpl2whirl.CalibrateTypes();
    mpl2whirl.TranslateTypes();
    mpl2whirl.TranslateGlobalSymbols();
    mpl2whirl.TranslateFunctionBodies();

    //Print_global_symtab(stdout);

    // finish this whirl file
    Verify_SYMTAB(GLOBAL_SYMTAB);
    Write_Global_Info(PU_Info_Root);
    Close_Output_Info();
    IR_reader_finish();

  }
  return 0;
}

/* Dummy definitions to satisify references from routines that got pulled
 * in by the header files but are never called
 */
void
Signal_Cleanup (INT sig) { }

const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    fprintf (stderr, "Internal: Host_Format_Parm () not implemented\n");
    return "";
}

INT8 Debug_Level = 0;
