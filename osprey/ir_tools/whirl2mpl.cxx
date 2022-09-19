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

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

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

#include <errno.h>		    /* for sys_errlist */
#include <stdio.h>		    /* for stderr */
#include <libgen.h>		    /* for basename() */
#include <sys/stat.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "mempool.h"
#include "wn.h"			    /* for ir_reader.h */
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"		    /* for IR_reader_init(), etc. */
#include "ir_bwrite.h"		    /* for WN_open_output(), etc. */
#include "ir_bread.h"		    /* for WN_open_input(), etc. */
#include "dwarf_DST_dump.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"              /* for Last_Dedicated_Preg_Offset */

BOOL Run_vsaopt = FALSE; // hack to workaround undefine since

extern std::vector<std::string> srcFileTable;

struct TY2mpl;
struct FixForwardTypeRef;
struct ST2mpl;
struct INITO2mpl;
struct ST_ATTR2mpl;

static bool use_binary = TRUE;

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

// return true if this expression opcode can result in a valid address
static bool OpCanFormAddress(Opcode op) {
  switch (op) {
  case OP_dread:
  case OP_regread:
  case OP_iread:
  case OP_ireadoff:
  case OP_ireadfpoff:
  case OP_ireadpcoff:
  case OP_addrof:
  case OP_addroffunc:
  case OP_addroflabel:
  case OP_addroffpc:
  case OP_iaddrof:
  case OP_conststr:
  case OP_conststr16:
  case OP_alloca:
  case OP_malloc:
  case OP_add:
  case OP_sub:
  case OP_select:
  case OP_array:
  case OP_intrinsicop:
    return true;
  default: ;
  }
  return false;
}

static PrimType MTYPE2mpl(TYPE_ID mtype, bool assertIfUnknown = TRUE) {
  switch (mtype) {
  case MTYPE_UNKNOWN: return PTY_void;
  case MTYPE_B: return PTY_u1;
  case MTYPE_I1: return PTY_i8;
  case MTYPE_I2: return PTY_i16;
  case MTYPE_I4: return PTY_i32;
  case MTYPE_I8: return PTY_i64;
  case MTYPE_U1: return PTY_u8;
  case MTYPE_U2: return PTY_u16;
  case MTYPE_U4: return PTY_u32;
  case MTYPE_U8: return PTY_u64;
  case MTYPE_F4: return PTY_f32;
  case MTYPE_F8: return PTY_f64;
  case MTYPE_F10: return PTY_f64;
  case MTYPE_F16: return PTY_f128;
  case MTYPE_STR: return PTY_constStr;
  case MTYPE_M: return PTY_agg;
  case MTYPE_C4: return PTY_c64;
  case MTYPE_C8: return PTY_c128;
  case MTYPE_V: return PTY_void;
  case MTYPE_A4: return PTY_ptr;
  case MTYPE_A8: return PTY_ptr;
#ifdef TARG_X8664
  case MTYPE_V16I8: return PTY_v2i64;
  case MTYPE_V16I4: return PTY_v4i32;
  case MTYPE_V16I2: return PTY_v8i16;
  case MTYPE_V16I1: return PTY_v16i8;
  case MTYPE_V16F8: return PTY_v2f64;
  case MTYPE_V16F4: return PTY_v4f32;
#endif
  default: {
    if (assertIfUnknown) {
      FmtAssert(FALSE, ("MTYPE2mpl: mtype %s not supported", MTYPE_name(mtype)));
    }
    return PTY_unknown;
  }
  }
}

static MIRType *MTYPE2MIRType(TYPE_ID mtype) {
  PrimType primType = MTYPE2mpl(mtype, TRUE);
  return GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
}

class WHIRL2mpl {
  friend TY2mpl;
  friend FixForwardTypeRef;
  friend ST2mpl;
  friend INITO2mpl;
  friend ST_ATTR2mpl;
 private:
   MIRBuilder *builder;
   MIRModule *mod;
   MIRFunction *fn = NULL;
   PregIdx lastRetPreg = 0;
   MIRSymbol *lastRetTemp = NULL;
   uint32 retTempCounter = 0;
   std::unordered_map<UINT32, TyIdx> TY_IDX2TyIdxMap;
   std::unordered_set<uint32> duplicatedLocalSymIndices;
   std::unordered_set<uint32> duplicatedGlobalSymIndices;
   std::unordered_set<TY_IDX> firstFieldDeletedStructs;
 public:
   bool hasForwardTypeReference = FALSE;
 public:
  explicit WHIRL2mpl(MIRBuilder *blder) : builder(blder), mod(blder->mirModule) {}
 private:
  TyIdx TY_IDX2TyIdx(TY_IDX ty_idx) {
    std::unordered_map<UINT32, TyIdx>::iterator it = TY_IDX2TyIdxMap.find(TY_IDX_index(ty_idx));
    if (it == TY_IDX2TyIdxMap.end()) {
      hasForwardTypeReference = TRUE;
      return TyIdx();
    }
    return it->second;
  }
  MIRType *TY_IDX2MIRType(TY_IDX ty_idx) {
    std::unordered_map<UINT32, TyIdx>::iterator it = TY_IDX2TyIdxMap.find(TY_IDX_index(ty_idx));
    if (it == TY_IDX2TyIdxMap.end()) {
      TY *ty = &Ty_Table[ty_idx];
      FmtAssert(FALSE, ("TY_IDX2MIRType: no corresponding MIRType for index %d", TY_IDX_index(ty_idx)));
    }
    return GlobalTables::GetTypeTable().GetTypeFromTyIdx(it->second);
  }
  MIRSymbol *ST_IDX2MIRSym(ST_IDX st_idx);
  PregIdx ST_IDX2Preg(ST_IDX st_idx, INT offset);
  IreadNode *GenIreadBasedOnDescOffset(BaseNode *ireadOpnd, MIRType *resType, TYPE_ID desc, INT offset);
  UINT CountDeletedFirstFields(TY_IDX struct_ty_idx, UINT field_id, UINT &cur_field_id);
  UINT32 GetAdjustedFieldID(TY_IDX struct_ty_idx, UINT32 field_id);
  BaseNode *EXPR2mpl(WN *wn, BlockNode *blk);
  LabelIdx LABEL_NUMBER2mpl(INT32 label_number);
  CallReturnPair CreateReturnPair4Call(TYPE_ID mtype, MIRType *retType);
  void STMT2mpl(WN *wn, BlockNode *blk);
  MIRConst *TCON_IDX2mpl(TCON_IDX tc, MIRType *mirtype = NULL);
  MIRConst *INITV_IDX2mpl4field(INITV_IDX idx, uint32 byteOfst, uint32 bitOfst, MIRType *fieldType);
  MIRAggConst *INITV_IDX2mpl4struct(INITV_IDX idx, MIRStructType *structType);
  MIRAggConst *INITV_IDX2mpl4union(INITV_IDX idx, MIRStructType *unionType);
  MIRConst *INITV_IDX2mpl(INITV_IDX idx, MIRType *mirtype);
 public:
  BlockNode *BLOCK2mpl(WN *wn, bool nullIfEmpty = false);
  void FUNC_ENTRY2mpl(WN *wn);
};

struct TY2mpl {
  WHIRL2mpl *whirl2mpl;
  TY2mpl(WHIRL2mpl *w) : whirl2mpl(w) {}

  void operator() (UINT idx, TY *ty) const;
};

void TY2mpl::operator() (UINT idx, TY *ty) const {
  char *tyname = TY_name_idx(*ty) == 0 ? NULL : TY_name(*ty);
  if (tyname && tyname[0] == '.') {
    char *newName = (char *)alloca(strlen(tyname) + 1);
    strcpy(newName, tyname);
    newName[0] = '_';
    tyname = newName;
  }
  MIRType *rettype = NULL;
  switch (TY_kind(*ty)) {
  case KIND_VOID:
  case KIND_SCALAR: {
    PrimType primType = MTYPE2mpl(ty->mtype, FALSE);
    if (primType != PTY_unknown) {
      whirl2mpl->TY_IDX2TyIdxMap[idx] = TyIdx(primType);
    }
    return;
  }
  case KIND_POINTER: {
    PrimType primType = MTYPE2mpl(ty->mtype);
    TY_IDX pointed_ty_idx = ty->Pointed();
    MIRPtrType type(whirl2mpl->TY_IDX2TyIdx(pointed_ty_idx), primType);
    if (TY_is_volatile(pointed_ty_idx)) {
      type.typeAttrs.SetAttr(ATTR_volatile);
    }
    rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&type);
    if (tyname != NULL && strcmp(tyname, "anon_ptr.") != 0) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(tyname);
      rettype->nameStrIdx = strIdx;
      whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
      whirl2mpl->mod->typeDefOrder.push_back(strIdx);
    }
    whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
    return;
  }
  case KIND_ARRAY: {
    TY_IDX e_ty_idx = ty->Etype();
    ARB_HANDLE arb(ty->Arb());
    INT dim = ARB_dimension(arb);
    MIRArrayType type;
    type.eTyIdx = whirl2mpl->TY_IDX2TyIdx(e_ty_idx);
    type.dim = dim;
    for (INT i = 0; i < dim; i++) {
      type.sizeArray[i] = (arb[i].Entry()->Ubnd_val() - arb[i].Entry()->Lbnd_val()) + 1;
    }
    rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&type);
    if (tyname != NULL) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(tyname);
      rettype->nameStrIdx = strIdx;
      whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
      whirl2mpl->mod->typeDefOrder.push_back(strIdx);
    }
    whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
    return;
  }
  case KIND_STRUCT: {
    MIRStructType sttype(TY_is_union(*ty) ? kTypeUnion : kTypeStruct);
    FieldVector fields;

    //if builtin_va_list type was predefined as x64 version, we change it to aarc64 version.
    if (tyname != NULL && std::string(tyname) == "__va_list_tag") {
      MIRStructType classValistType(kTypeStruct);
      GlobalTables::GetTypeTable().AddFieldToStructType(&classValistType, "stack", GlobalTables::GetTypeTable().GetUInt64());
      GlobalTables::GetTypeTable().AddFieldToStructType(&classValistType, "gr_top", GlobalTables::GetTypeTable().GetUInt64());
      GlobalTables::GetTypeTable().AddFieldToStructType(&classValistType, "vr_top", GlobalTables::GetTypeTable().GetUInt64());
      GlobalTables::GetTypeTable().AddFieldToStructType(&classValistType, "gr_offs", GlobalTables::GetTypeTable().GetInt32());
      GlobalTables::GetTypeTable().AddFieldToStructType(&classValistType, "vr_offs", GlobalTables::GetTypeTable().GetInt32());
      rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&classValistType);
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string(tyname));
      rettype->nameStrIdx = strIdx;
      whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
      whirl2mpl->mod->typeDefOrder.push_back(strIdx);
      whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
      return;
    }
    //special symbol for passing EH type table contents to back-end
    if (tyname != NULL && std::string(tyname) == "__TYPEINFO_ENTRY__") {
      MIRStructType ehTypeInfoEntryType(kTypeStruct);
      GlobalTables::GetTypeTable().AddFieldToStructType(&ehTypeInfoEntryType, "typeinfo_sym", GlobalTables::GetTypeTable().GetUInt64());
      GlobalTables::GetTypeTable().AddFieldToStructType(&ehTypeInfoEntryType, "filter", GlobalTables::GetTypeTable().GetUInt32());
      rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&ehTypeInfoEntryType);
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string(tyname));
      rettype->nameStrIdx = strIdx;
      whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
      whirl2mpl->mod->typeDefOrder.push_back(strIdx);
      whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
      return;
    }

    // if the field offset of the first and second fields are the same, and
    // the first field is not a bitfield, then delete the first field
    bool deleteFirstFld = FALSE;
    { FLD_HANDLE firstFld = TY_fld(*ty);
      FLD_HANDLE secondFld;
      if (whirl2mpl->mod->srcLang == kSrcLangCPlusPlus &&
          !firstFld.Is_Null() && FLD_is_bit_field(firstFld) == 0) {
        secondFld = FLD_next(firstFld);
        if (!secondFld.Is_Null()) {
          deleteFirstFld = FLD_ofst(firstFld) == 0 && FLD_ofst(secondFld) == 0;
        }
      }
      if (deleteFirstFld) {
        Set_TY_fld(*ty, secondFld);
        whirl2mpl->firstFieldDeletedStructs.insert(idx);
      }
    }

    FLD_HANDLE fld = TY_fld(*ty);
    UINT last_FLD_ofst = 0;
    while (!fld.Is_Null()) {  // process each field
      TY_IDX fty_idx = FLD_type(fld);
      TyIdx fTyIdx = whirl2mpl->TY_IDX2TyIdx(fty_idx);
      FieldAttrs fldAttrs;
      TyidxFieldAttrPair tfaPair;
      fldAttrs.attrAlign = TY_align_exp(fty_idx);
      if (TY_is_volatile(fty_idx)) {
	fldAttrs.SetAttr(FLDATTR_volatile);
      }
      if (TY_is_restrict(fty_idx)) {
	fldAttrs.SetAttr(FLDATTR_restrict);
      }
      if (TY_is_const(fty_idx)) {
	fldAttrs.SetAttr(FLDATTR_const);
      }
      if (FLD_is_bit_field(fld) != 0) {
	TY *fty = &Ty_Table[fty_idx];
	// determine the suitable bitfield base type based on bofst and bsize
        TYPE_ID fmtype;
        if (FLD_bsize(fld) == 0) { // special case, for aligning purpose
          UINT skippedBytes = FLD_ofst(fld) - last_FLD_ofst;
          switch (skippedBytes) {
          case 0: FmtAssert(FALSE, ("TY2mpl::operator(): illegal presence of 0-sized bitfield")); break;
          case 1: fmtype = MTYPE_U1; break;
          case 2: fmtype = MTYPE_U2; break;
          case 3:
          case 4: fmtype = MTYPE_U4; break;
          default: fmtype = MTYPE_U8; break;
          }
        } else { // normal case
          UINT bfalign = MTYPE_bit_size(fty->mtype);
          while (RoundDown(FLD_ofst(fld)*8 + FLD_bofst(fld) + FLD_bsize(fld) - 1, bfalign) !=
                 RoundDown(FLD_ofst(fld)*8 + FLD_bofst(fld), bfalign)) {
            bfalign *= 2;
          }
          switch (bfalign) {
            case 8: fmtype = MTYPE_U1; break;
            case 16: fmtype = MTYPE_U2; break;
            case 32: fmtype = MTYPE_U4; break;
            case 64: fmtype = MTYPE_U8; break;
          }
        }
        fmtype = Mtype_TransferSize(fmtype, fty->mtype);
        MIRBitfieldType bitfieldtype(FLD_bsize(fld), MTYPE2mpl(fmtype));
        tfaPair = std::make_pair(GlobalTables::GetTypeTable().GetOrCreateMIRType(&bitfieldtype), fldAttrs);
      } else {
        tfaPair = std::make_pair(fTyIdx, FieldAttrs());
      }
      std::string fname(FLD_name(fld));
      if (fname[0] == '.') {
        fname[0] = '_';
      }
      GStrIdx fStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(std::string(fname));
      fields.push_back(std::make_pair(fStrIdx, tfaPair));
      last_FLD_ofst = FLD_ofst(fld);
      fld = FLD_next (fld);
    }
    sttype.fields = fields;
    rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&sttype);
    GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(tyname == NULL ? "_TY_IDX" + std::to_string(idx) : std::string(tyname));
    rettype->nameStrIdx = strIdx;
    whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
    whirl2mpl->mod->typeDefOrder.push_back(strIdx);
    whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
    return;
  }
  case KIND_FUNCTION: {
    // return type
    TyIdx retTyIdx;
    if (TY_tylist(*ty) == 0) {
      retTyIdx = TyIdx(PTY_void);
    } else {
      TY_IDX rettype_idx = Tylist_Table[TY_tylist(*ty)];
      retTyIdx = whirl2mpl->TY_IDX2TyIdx(rettype_idx);
    }
    // types of the parameters
    std::vector<TyIdx> paramTypeList;
    std::vector<TypeAttrs> paramAttrsList;
    if (TY_tylist(*ty) != 0) {
      TYLIST_IDX indx = TY_tylist(*ty);
      ++indx;
      while (Tylist_Table[indx] != 0) {
        TY_IDX arg_ty_idx = Tylist_Table[indx];
        paramTypeList.push_back(whirl2mpl->TY_IDX2TyIdx(arg_ty_idx));
	paramAttrsList.push_back(TypeAttrs());
        ++indx;
      }
    }
    MIRFuncType funcType(retTyIdx, paramTypeList, paramAttrsList);
    if (TY_is_varargs(*ty))
      funcType.isVarArgs = true;
    rettype = GlobalTables::GetTypeTable().CreateMIRTypeNode(&funcType);
    if (tyname != NULL) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(tyname);
      rettype->nameStrIdx = strIdx;
      whirl2mpl->mod->typeNameTab->SetGStrIdxToTyIdx(strIdx, rettype->tyIdx);
      whirl2mpl->mod->typeDefOrder.push_back(strIdx);
    }
    whirl2mpl->TY_IDX2TyIdxMap[idx] = rettype->tyIdx;
    return;
  }
  default: FmtAssert(FALSE, ("TY2mpl::operator(): NYI %s", Kind_Name(TY_kind(*ty))));
  }
}

struct FixForwardTypeRef {
  WHIRL2mpl *whirl2mpl;
  FixForwardTypeRef(WHIRL2mpl *w) : whirl2mpl(w) {}

  void operator() (UINT idx, TY *ty) const;
};

void FixForwardTypeRef::operator() (UINT idx, TY *ty) const {
  switch (TY_kind(*ty)) {
  case KIND_POINTER: {
    MIRType *mirtype = whirl2mpl->TY_IDX2MIRType(make_TY_IDX(idx));
    if (static_cast<MIRPtrType *>(mirtype)->pointedTyIdx.GetIdx() == 0) {
      TY_IDX pointed_ty_idx = ty->Pointed();
      static_cast<MIRPtrType *>(mirtype)->pointedTyIdx = whirl2mpl->TY_IDX2TyIdx(pointed_ty_idx);
    }
    return;
  }
  case KIND_ARRAY: {
    MIRType *mirtype = whirl2mpl->TY_IDX2MIRType(make_TY_IDX(idx));
    if (static_cast<MIRArrayType *>(mirtype)->eTyIdx.GetIdx() == 0) {
      TY_IDX e_ty_idx = ty->Etype();
      static_cast<MIRArrayType *>(mirtype)->eTyIdx = whirl2mpl->TY_IDX2TyIdx(e_ty_idx);
    }
    return;
  }
  case KIND_STRUCT: {
    MIRStructType *mirtype = static_cast<MIRStructType *>(whirl2mpl->TY_IDX2MIRType(make_TY_IDX(idx)));
    FieldVector::iterator it = mirtype->fields.begin();
    FLD_HANDLE fld = TY_fld(*ty);
    INT fld_count = 0;

    for (; it != mirtype->fields.end(); it++, fld_count++) {
      if (it->second.first.GetIdx() == 0) {
        for(INT i = 0; i < fld_count; i++)
          fld = FLD_next(fld);

        fld_count = 0;
        TY_IDX fty_idx = FLD_type(fld);
        TyIdx fTyIdx = whirl2mpl->TY_IDX2TyIdx(fty_idx);
        it->second.first = fTyIdx;
      }
    }
    return;
  }
  case KIND_FUNCTION: {
    MIRFuncType *mirtype = static_cast<MIRFuncType *>(whirl2mpl->TY_IDX2MIRType(make_TY_IDX(idx)));
    // return type
    if (mirtype->retTyIdx.GetIdx() == 0) {
      TY_IDX ret_ty_idx = Tylist_Table[TY_tylist(*ty)];
      mirtype->retTyIdx = whirl2mpl->TY_IDX2TyIdx(ret_ty_idx);
    }
    // types of the parameters
    if (TY_tylist(*ty) != 0) {
      TYLIST_IDX indx = TY_tylist(*ty);
      ++indx;
      UINT paramIdx = 0;
      while (Tylist_Table[indx] != 0) {
	if (mirtype->paramTypeList[paramIdx].GetIdx() == 0) {
	  TY_IDX arg_ty_idx = Tylist_Table[indx];
	  mirtype->paramTypeList[paramIdx] = whirl2mpl->TY_IDX2TyIdx(arg_ty_idx);
	}
        ++indx;
	++paramIdx;
      }
    }
    return;
  }
  case KIND_VOID:
  case KIND_SCALAR:
  default: return;
  }
}

static MIRStorageClass ST_SCLASS2mpl(ST_SCLASS sc) {
  switch (sc) {
  case SCLASS_UNKNOWN: return kScInvalid;
  case SCLASS_AUTO: return kScAuto;
  case SCLASS_FORMAL: return kScFormal;
  case SCLASS_PSTATIC: return kScPstatic;
  case SCLASS_FSTATIC: return kScFstatic;
  case SCLASS_COMMON:
  case SCLASS_UGLOBAL:
  case SCLASS_DGLOBAL: return kScGlobal;
  case SCLASS_EXTERN: return kScExtern;
  case SCLASS_TEXT: return kScText;
  case SCLASS_EH_REGION_SUPP: return kScEHRegionSupp;
  default: FmtAssert(FALSE, ("ST_SCLASS2mpl: NYI %s", Sclass_Name(sc)));
  }
  return kScInvalid;
}

struct ST2mpl {
  WHIRL2mpl *whirl2mpl;
  ST2mpl(WHIRL2mpl *w) : whirl2mpl(w) {}

  void operator() (UINT idx, ST *st) const;
};

void ST2mpl::operator() (UINT idx, ST *st) const {
  if (st->storage_class == SCLASS_FORMAL) {
    return;  // formals have already been processed at the IDNAME nodes
  }
  if (st->sym_class == CLASS_CONST) {  // not handled here
    return;
  }
  std::string name_str(ST_name(*st));
  if (st->sym_class == CLASS_VAR) {
    if (name_str.find_first_of('.') != std::string::npos) {
      if (st->storage_class == SCLASS_PSTATIC && ST_IDX_level(st->st_idx) != GLOBAL_SYMTAB) {
        name_str.append(std::to_string(whirl2mpl->fn->puIdx));  // make pstatic name unique
      }
    }
    if (name_str[0] == '.') {
      name_str[0] = '_';
    }
    TY_IDX ty_idx = st->u2.type;
    MIRSymbol *sym = NULL;
    FmtAssert(st->storage_class != SCLASS_FORMAL, ("ST2mpl::operator(): should not be here for SCLASS_FORMAL symbols"));
    if ((st->storage_class == SCLASS_AUTO ||
         st->storage_class == SCLASS_EH_REGION ||
         st->storage_class == SCLASS_EH_REGION_SUPP ||
         st->storage_class == SCLASS_PSTATIC) && whirl2mpl->fn != NULL) {
      if (whirl2mpl->builder->GetLocalDecl(name_str) != NULL) {
        name_str.append(std::to_string(idx));  // make local name unique
        whirl2mpl->duplicatedLocalSymIndices.insert(idx);
      }
      sym = whirl2mpl->builder->CreateLocalDecl(name_str, whirl2mpl->TY_IDX2MIRType(ty_idx));
      if (st->storage_class == SCLASS_PSTATIC) {
	sym->SetStorageClass(kScPstatic);
      } else if (st->storage_class == SCLASS_EH_REGION_SUPP) {
	sym->SetStorageClass(kScEHRegionSupp);
      }
      if (sym->GetName() == "__Exc_Ptr__" || sym->GetName() == "__Exc_Filter__") {
        sym->SetAttr(ATTR_volatile);
      }
    } else {
      if (whirl2mpl->builder->GetGlobalDecl(name_str) != NULL) {
        name_str.append(std::to_string(idx));  // make global name unique
        whirl2mpl->duplicatedGlobalSymIndices.insert(idx);
      }
      sym = whirl2mpl->builder->CreateGlobalDecl(name_str, whirl2mpl->TY_IDX2MIRType(ty_idx), ST_SCLASS2mpl(st->storage_class));
      if (ST_is_weak_symbol(*st)) {
        sym->SetAttr(ATTR_weak);
      }
    }
    if (ST_is_const_var(*st) || TY_is_const(ty_idx)) {
      sym->SetAttr(ATTR_const);
    }
    sym->sKind = kStVar;
  } else if (st->sym_class == CLASS_FUNC) {
    TY_IDX ty_idx = PU_prototype(Pu_Table[st->u2.pu]);
    TY *ty = &Ty_Table[ty_idx];
    FmtAssert(TY_kind(*ty) == KIND_FUNCTION, ("ST2mpl::operator(): KIND_FUNCTION expected for function type"));
    MIRFuncType *funcType = static_cast<MIRFuncType *>(whirl2mpl->TY_IDX2MIRType(ty_idx));
    TyIdx funcTyIdx = funcType->tyIdx;
    MIRSymbol *funcst = GlobalTables::GetGsymTable().CreateSymbol(kScopeGlobal);
    GStrIdx strIdx = whirl2mpl->builder->GetOrCreateStringIndex(name_str);
    funcst->SetNameStridx(strIdx);
    GlobalTables::GetGsymTable().AddToStringSymbolMap(funcst);
    funcst->storageClass = kScText;
    funcst->sKind = kStFunc;

    MIRFunction *fn = whirl2mpl->mod->memPool->New<MIRFunction>(whirl2mpl->mod, funcst->GetStIdx());
    fn->funcType = funcType;
    fn->SetReturnStruct(GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcType->retTyIdx));
    fn->puIdx = GlobalTables::GetFunctionTable().funcTable.size();
    fn->puIdxOrigin = fn->puIdx;
    if (TY_is_varargs(*ty)) {
      fn->SetVarargs();
    }
    GlobalTables::GetFunctionTable().funcTable.push_back(fn);
    funcst->SetTyIdx(funcTyIdx);
    funcst->SetFunction(fn);
    if (ST_is_weak_symbol(*st)) {
      funcst->SetAttr(ATTR_weak);
      fn->SetAttr(FUNCATTR_weak);
    }
    if (ST_is_export_local(st)) {
      fn->SetAttr(FUNCATTR_static);
    }
    for (uint32 i = 0; i < funcType->paramTypeList.size(); i++) {
      FormalDef formalDef(GStrIdx(0), nullptr, funcType->paramTypeList[i], funcType->paramAttrsList[i]);
      fn->formalDefVec.push_back(formalDef);
    }
  }
}

MIRSymbol *WHIRL2mpl::ST_IDX2MIRSym(ST_IDX st_idx) {
  ST *st = &St_Table[st_idx];
  if (st->sym_class == CLASS_PREG) {
    return NULL;
  } else if (st->sym_class == CLASS_CONST) {
    FmtAssert(FALSE, ("ST_IDX2MIRSym: sym_class CLASS_CONST has no MIRSymbol"));
    return NULL;
  } else {
    std::string symName(ST_name(st));
    if (symName.find_first_of('.') != std::string::npos) {
      if (st->storage_class == SCLASS_PSTATIC && ST_IDX_level(st_idx) != GLOBAL_SYMTAB) {
        symName.append(std::to_string(fn->puIdx));  // make pstatic name unique
      }
    }
    if (symName[0] == '.') {
      symName[0] = '_';
    }
    MIRSymbol *sym = NULL;
    if (ST_IDX_level(st_idx) == GLOBAL_SYMTAB) {
      if (duplicatedGlobalSymIndices.find(ST_IDX_index(st_idx)) != duplicatedGlobalSymIndices.end()) {
        symName.append(std::to_string(ST_IDX_index(st_idx)));
      }
      sym = builder->GetGlobalDecl(symName);
    } else { // SCLASS_AUTO SCLASS_FORMAL SCLASS_PSTATIC
      if (duplicatedLocalSymIndices.find(ST_IDX_index(st_idx)) != duplicatedLocalSymIndices.end()) {
        symName.append(std::to_string(ST_IDX_index(st_idx)));
      }
      sym = builder->GetLocalDecl(symName);
    }
    FmtAssert(sym != NULL, ("ST2MIRSym: cannot find symbol"));
    return sym;
  }
}

PregIdx WHIRL2mpl::ST_IDX2Preg(ST_IDX st_idx, INT offset) {
  ST *st = &St_Table[st_idx];
  if (st->sym_class != CLASS_PREG) {
    return 0;
  }
  TY_IDX preg_ty_idx = st->u2.type;
  TY *preg_ty = &Ty_Table[preg_ty_idx];
  return fn->pregTab->EnterPregNo(offset, MTYPE2mpl(preg_ty->mtype));
}

// if x has agg type information, returns it
static MIRType *FindAggType(BaseNode *x, MIRFunction *fn) {
  switch (x->op) {
  case OP_dread: {
    AddrofNode *addrof = static_cast<AddrofNode *>(x);
    MIRSymbol *sym = fn->GetLocalOrGlobalSymbol(addrof->stIdx, false);
    MIRType *symType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
    if (addrof->fieldID != 0) {
      MIRStructType *structType = static_cast<MIRStructType *>(symType);
      symType = structType->GetFieldType(addrof->fieldID);
    }
    if (symType->typeKind == kTypeStruct ||
        symType->typeKind == kTypeUnion ||
        symType->typeKind == kTypeArray) {
      return symType;
    } else {
      return NULL;
    }
  }
  case OP_iread: {
    IreadNode *iread = static_cast<IreadNode *>(x);
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx);
    MIRType *pointedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(static_cast<MIRPtrType *>(mirtype)->pointedTyIdx);
    if (iread->fieldID != 0) {
      MIRStructType *structType  = static_cast<MIRStructType *>(pointedType);
      pointedType = structType->GetFieldType(iread->fieldID);
    }
    if (pointedType->typeKind == kTypeStruct ||
        pointedType->typeKind == kTypeUnion ||
        pointedType->typeKind == kTypeArray) {
      return pointedType;
    } else {
      return NULL;
    }
  }
  default: return NULL;
  }
}

// if x has ptr type information, returns it
static MIRPtrType *FindPtrType(BaseNode *x, MIRFunction *fn) {
  switch (x->op) {
  case OP_addrof:
  case OP_dread: {
    AddrofNode *addrof = static_cast<AddrofNode *>(x);
    MIRSymbol *sym = fn->GetLocalOrGlobalSymbol(addrof->stIdx, false);
    MIRType *symType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
    if (x->op == OP_dread && addrof->fieldID != 0) {
      MIRStructType *structType = static_cast<MIRStructType *>(symType);
      symType = structType->GetFieldType(addrof->fieldID);
    }
    if (x->op == OP_addrof) {
      return static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(symType, PTY_ptr));
    } else if (symType->typeKind == kTypePointer) {
      return static_cast<MIRPtrType *>(symType);
    } else {
      return NULL;
    }
  }
  case OP_iread: {
    IreadNode *iread = static_cast<IreadNode *>(x);
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iread->tyIdx);
    MIRType *pointedType = static_cast<MIRPtrType *>(mirtype)->GetPointedType();
    if (iread->fieldID != 0) {
      MIRStructType *structType  = static_cast<MIRStructType *>(pointedType);
      pointedType = structType->GetFieldType(iread->fieldID);
    }
    if (pointedType->typeKind == kTypePointer) {
      return static_cast<MIRPtrType *>(pointedType);
    } else {
      return NULL;
    }
  }
  case OP_iaddrof: {
    IreadNode *iaddrof = static_cast<IreadNode *>(x);
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(iaddrof->tyIdx);
    MIRType *pointedType = static_cast<MIRPtrType *>(mirtype)->GetPointedType();
    MIRStructType *structType  = static_cast<MIRStructType *>(pointedType);
    pointedType = structType->GetFieldType(iaddrof->fieldID);
    return static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(pointedType, PTY_ptr));
  }
  default: return NULL;
  }
}

// x is an address expression returning the base of the array
static MIRPtrType *FindArrayType(BaseNode *x, MIRFunction *fn, UINT elemSize) {
  switch (x->op) {
  case OP_addrof:
  case OP_dread:
  case OP_iaddrof:
  case OP_iread: {
    MIRPtrType *ptrType = FindPtrType(x, fn);
    return ptrType;
  }
  case OP_conststr: {
    MIRArrayType arrayType;
    TYPE_ID elem_type_id = 0;
    switch (elemSize) {
    case 1: elem_type_id = MTYPE_I1; break;
    case 2: elem_type_id = MTYPE_I2; break;
    case 4: elem_type_id = MTYPE_I4; break;
    case 8: elem_type_id = MTYPE_I8; break;
    }
    arrayType.eTyIdx = TyIdx(MTYPE2mpl(elem_type_id));
    arrayType.dim = 1;
    ConststrNode *conststrNode = static_cast<ConststrNode *>(x);
    const std::string uString = GlobalTables::GetUStrTable().GetStringFromStrIdx(conststrNode->strIdx);
    arrayType.sizeArray[0] = (uString.size() + elemSize - 1)/ elemSize;
    MIRType *mirType = GlobalTables::GetTypeTable().CreateMIRTypeNode(&arrayType);
    return static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(mirType->tyIdx, PTY_ptr));
  }
  case OP_array: {
    ArrayNode *arry = static_cast<ArrayNode *>(x);
    MIRPtrType *ptrType = static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(arry->tyIdx));
    MIRArrayType *arryType = static_cast<MIRArrayType *>(GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrType->pointedTyIdx));
    MIRType *elemType = arryType->GetElemType();
    while (elemType->typeKind == kTypeArray) {
      elemType = static_cast<MIRArrayType *>(elemType)->GetElemType();
    }
    return static_cast<MIRPtrType *>(GlobalTables::GetTypeTable().GetOrCreatePointerType(elemType->tyIdx, PTY_ptr));
  }
  case OP_add: {
    return FindArrayType(x->Opnd(0), fn, elemSize);
  }
  default:
    return NULL;
  }
}

static bool ArrayTypeMatched(MIRArrayType *candType, UINT elemSize, std::vector<uint32> &sizeArray) {
  INT i = 0;
  INT j = -1;
  for (; i < sizeArray.size(); i++) {
    j++;
    if (j == candType->dim) {
      candType = dynamic_cast<MIRArrayType *>(candType->GetElemType());
      if (candType == NULL) {
        return FALSE;
      }
      j = 0;
    }
    if (candType->sizeArray[j] != sizeArray[i]) {
      return FALSE;
    }
  }
  UINT candElemSize = candType->GetElemType()->GetSize();
  for (; i < candType->dim; i++) {
    candElemSize *= candType->sizeArray[i];
  }
  return candElemSize == elemSize;
}


static MIRArrayType *MatchArrayType(MIRType *candType, UINT elemSize, std::vector<uint32> &sizeArray) {
  if (candType->typeKind == kTypeArray) {
    MIRArrayType *candArrayType = static_cast<MIRArrayType *>(candType);
    if (ArrayTypeMatched(candArrayType, elemSize, sizeArray)) {
      return candArrayType;
    } else {  // special-case array of va_list_tag
      MIRType *elemType = candArrayType->GetElemType();
      if (elemType->typeKind == kTypeArray &&
          static_cast<MIRArrayType *>(elemType)->GetElemType()->GetName() == "__va_list_tag") {
        return candArrayType;
      }
    }
  }
  if (candType->typeKind == kTypeStruct || candType->typeKind == kTypeUnion) {
    MIRStructType *structType = static_cast<MIRStructType *>(candType);
    for (size_t i = 0; i < structType->fields.size(); i++) {
      MIRType *fieldType = structType->GetElemType(i);
      MIRArrayType *matchedType = MatchArrayType(fieldType, elemSize, sizeArray);
      if (matchedType) {
        return matchedType;
      }
    }
  }
  return NULL;
}

static FieldID FindFieldID4Offset(MIRStructType *structType, int64 offset, FieldID startFieldID) {
  uint32 byteOfst = 0;
  uint32 bitOfst = 0;
  for (int32 fieldIdx = 0; fieldIdx < structType->fields.size(); fieldIdx++) {
    TyIdx fTyIdx = structType->fields[fieldIdx].second.first;
    MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
    if (fieldType->GetSize() == 0) {
      if (fieldType->typeKind == kTypeBitField) {  // do aligning
        MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
        bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
        byteOfst = bitOfst >> 3;
      }
      continue;
    }
    // align for current field
    if (fieldType->typeKind != kTypeBitField) {
      if (byteOfst * 8 < bitOfst) {
        byteOfst = (bitOfst >> 3) + 1;
      }
      byteOfst = RoundUp(byteOfst, fieldType->GetAlign());
      bitOfst = byteOfst * 8;
      // check offset match
      if (byteOfst == offset) {
        if (fieldType->NumberOfFieldIDs() > 0) {
          MIRStructType *structFieldType;
          if (fieldType->typeKind == kTypeArray) {
#if 0
            structFieldType = static_cast<MIRStructType *>(static_cast<MIRArrayType *>(fieldType)->EmbeddedStructType());
#else
            return startFieldID + fieldIdx;
#endif
          } else {
            structFieldType = static_cast<MIRStructType *>(fieldType);
          }
          return FindFieldID4Offset(structFieldType, 0, startFieldID + fieldIdx + 1);
	} else {
	  return startFieldID + fieldIdx;
	}
      }
    } else {
      MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
      if (RoundDown(bitOfst + bitfType->fieldSize - 1, GetPrimTypeBitSize(bitfType->primType)) !=
          RoundDown(bitOfst, GetPrimTypeBitSize(bitfType->primType))) {
        bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
        byteOfst = bitOfst >> 3;
      }
    }
    if (structType->typeKind == kTypeUnion) {
      continue;
    }
    // increment byteOfst and bitOfst for current field
    if (fieldType->typeKind != kTypeBitField) {
      byteOfst += fieldType->GetSize();
      bitOfst = byteOfst * 8;
      if (fieldType->NumberOfFieldIDs() > 0) {
	if (byteOfst <= offset) {
	  startFieldID += fieldType->NumberOfFieldIDs();
	} else {
          if (fieldType->typeKind == kTypeArray) {
            return 0;
          }
	  return FindFieldID4Offset(static_cast<MIRStructType *>(fieldType), offset - byteOfst, startFieldID + fieldIdx + 1);
	}
      }
    } else {
      bitOfst += static_cast<MIRBitfieldType *>(fieldType)->fieldSize;
      byteOfst = bitOfst >> 3;
    }
  }
  return 0;
}

static MIRIntrinsicID INTRINSIC2mpl(INTRINSIC intrn) {
  switch (intrn) {
  case INTRN_STRCMP: return INTRN_C_strcmp;
  case INTRN_STRNCMP: return INTRN_C_strncmp;
  case INTRN_STRCPY: return INTRN_C_strcpy;
  case INTRN_STRNCPY: return INTRN_C_strncpy;
  case INTRN_STRLEN: return INTRN_C_strlen;
  case INTRN_MEMCMP: return INTRN_C_memcmp;
  case INTRN_MEMCPY: return INTRN_C_memcpy;
  case INTRN_MEMMOVE: return INTRN_C_memmove;
  case INTRN_MEMSET: return INTRN_C_memset;

  case INTRN_F4ACOS: return INTRN_C_acosf;
  case INTRN_F4ASIN: return INTRN_C_asinf;
  case INTRN_F4ATAN: return INTRN_C_atanf;
  case INTRN_F4COS: return INTRN_C_cosf;
  case INTRN_F4COSH: return INTRN_C_coshf;
  case INTRN_F4EXP: return INTRN_C_expf;
  case INTRN_F4LOG: return INTRN_C_logf;
  case INTRN_F4LOG10: return INTRN_C_log10f;
  case INTRN_F4SIN: return INTRN_C_sinf;
  case INTRN_F4SINH: return INTRN_C_sinhf;
  case INTRN_F8ACOS: return INTRN_C_acos;
  case INTRN_F8ASIN: return INTRN_C_asin;
  case INTRN_F8ATAN: return INTRN_C_atan;
  case INTRN_F8COS: return INTRN_C_cos;
  case INTRN_F8COSH: return INTRN_C_cosh;
  case INTRN_F8EXP: return INTRN_C_exp;
  case INTRN_F8LOG: return INTRN_C_log;
  case INTRN_F8LOG10: return INTRN_C_log10;
  case INTRN_F8SIN: return INTRN_C_sin;
  case INTRN_F8SINH: return INTRN_C_sinh;

  case INTRN_I4FFS: return INTRN_C_ffs;

  case INTRN_VA_START: return INTRN_C_va_start;
  case INTRN_CONSTANT_P: return INTRN_C_constant_p;

  default: FmtAssert(FALSE, ("INTRINSIC2mpl: %s NYI", INTRINSIC_name(intrn)));
  }
}

static Opcode OPERATOR2mpl(OPERATOR opr) {
  switch (opr) {
  case OPR_CVT: return OP_cvt;
  case OPR_TAS: return OP_retype;
  case OPR_NEG: return OP_neg;
  case OPR_BNOT: return OP_bnot;
  case OPR_LNOT: return OP_lnot;
  case OPR_ABS: return OP_abs;
  case OPR_RECIP: return OP_recip;
  case OPR_SQRT: return OP_sqrt;
  case OPR_ILDA: return OP_iaddrof;
  case OPR_RND: return OP_round;
  case OPR_CEIL: return OP_ceil;
  case OPR_FLOOR: return OP_floor;
  case OPR_TRUNC: return OP_trunc;
  case OPR_EXTRACT_BITS: return OP_extractbits;
  case OPR_ALLOCA: return OP_alloca;
  case OPR_ADD: return OP_add;
  case OPR_SUB: return OP_sub;
  case OPR_MPY: return OP_mul;
  case OPR_DIV: return OP_div;
  case OPR_REM: return OP_rem;
  case OPR_MAX: return OP_max;
  case OPR_MIN: return OP_min;
  case OPR_BAND: return OP_band;
  case OPR_BIOR: return OP_bior;
  case OPR_BXOR: return OP_bxor;
  case OPR_LAND: return OP_land;
  case OPR_LIOR: return OP_lior;
  case OPR_CAND: return OP_cand;
  case OPR_CIOR: return OP_cior;
  case OPR_SHL: return OP_shl;
  case OPR_ASHR: return OP_ashr;
  case OPR_LSHR: return OP_lshr;
  case OPR_EQ: return OP_eq;
  case OPR_NE: return OP_ne;
  case OPR_LT: return OP_lt;
  case OPR_GT: return OP_gt;
  case OPR_LE: return OP_le;
  case OPR_GE: return OP_ge;
  case OPR_COMPOSE_BITS: return OP_depositbits;
  case OPR_SELECT: return OP_select;
  case OPR_WHILE_DO: return OP_while;
  case OPR_DO_WHILE: return OP_dowhile;
  case OPR_TRUEBR: return OP_brtrue;
  case OPR_FALSEBR: return OP_brfalse;
  default: ;
  }
  return kOpUndef;
}
// Given the TY of a struct, get to the field corresponding to field_id.
// cur_field_id gives the field_id of the struct itself.  If the field is
// found, cur_field_id is set equal to field_id. During the process, count
// the number of sub-structs passed whose first fields were deleted.
UINT WHIRL2mpl::CountDeletedFirstFields(TY_IDX struct_ty_idx, UINT field_id, UINT &cur_field_id)
{
    UINT count = 0;
    if (firstFieldDeletedStructs.find(TY_IDX_index(struct_ty_idx)) != firstFieldDeletedStructs.end()) {
      count++;
    }
    FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
    do {
	FLD_HANDLE fld(fld_iter);
	cur_field_id++;
	if (cur_field_id == field_id)
	    return count;
	if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
            TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
	    count += CountDeletedFirstFields(FLD_type(fld), field_id, cur_field_id);
	    if (cur_field_id == field_id)
		return count;
	}
    } while (!FLD_last_field(fld_iter++));
    return count;
}

UINT32 WHIRL2mpl::GetAdjustedFieldID(TY_IDX struct_ty_idx, UINT32 field_id) {
  if (field_id >= 2 && mod->srcLang == kSrcLangCPlusPlus) {
    UINT cur_field_id = 0;
    UINT count = CountDeletedFirstFields(struct_ty_idx, field_id, cur_field_id);
    return field_id - count;
  }
  else return field_id;
}

IreadNode *WHIRL2mpl::GenIreadBasedOnDescOffset(BaseNode *ireadOpnd, MIRType *resType, TYPE_ID desc, INT offset) {
  if (offset != 0) {
    BaseNode *constvalNode = builder->CreateIntConst(offset, PTY_ptr);
    MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
    ireadOpnd = builder->CreateExprBinary(OP_add, mirType, ireadOpnd, constvalNode);
  }
  MIRType *ptrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(TyIdx(MTYPE2mpl(desc)), PTY_ptr);
  return builder->CreateExprIread(resType, ptrType, 0, ireadOpnd);
}

static uint32 GetDimSize(WN *wn) {
  OPERATOR opr = WN_operator(wn);
  if (opr != OPR_INTCONST) {
    return 0;
  }
  uint32 answer = WN_const_val(wn);
  return (answer == 0) ? 1 : answer;
}

static bool HasCOMMA(WN *wn) {
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_COMMA || opr == OPR_RCOMMA) {
    return true;
  }
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    if (HasCOMMA(WN_kid(wn, i))) {
      return true;
    }
  }
  return false;
}

static bool HasSideEffect(WN *wn, bool isCPP) {
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_COMMA || opr == OPR_RCOMMA) {
    return true;
  }
  if (isCPP && opr == OPR_ILOAD) {
    return true;
  }
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    if (HasSideEffect(WN_kid(wn, i), isCPP)) {
      return true;
    }
  }
  return false;
}

BaseNode *WHIRL2mpl::EXPR2mpl(WN *wn, BlockNode *blk) {
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
  // leaf ops starts
  case OPR_INTCONST: {
    ConstvalNode *constNode = builder->CreateIntConst(WN_const_val(wn), MTYPE2mpl(WN_rtype(wn)));
    return constNode;
  }
  case OPR_CONST: {
    ST *st = &St_Table[WN_st_idx(wn)];
    FmtAssert(ST_sym_class(st) == CLASS_CONST, ("EXPR2mpl: OPR_CONST with non-const ST"));
    MIRConst *mirconst = TCON_IDX2mpl(st->u1.tcon);
    if (mirconst->kind == kConstStrConst) {
      ConststrNode *conststrNode = fn->codeMemPool->New<ConststrNode>(PTY_ptr, static_cast<MIRStrConst *>(mirconst)->value);
      return conststrNode;
    } else {
      ConstvalNode *constNode = fn->codeMemPool->New<ConstvalNode>(mirconst->type->primType, mirconst);
      return constNode;
    }
  }
  case OPR_LDA: {
    ST *st = &St_Table[WN_st_idx(wn)];
    if (st->sym_class == CLASS_CONST) {
      MIRConst *mirconst = TCON_IDX2mpl(st->u1.tcon);
      FmtAssert(mirconst->kind == kConstStrConst, ("EXPR2mpl: CLASS_CONST symbol in LDA refers to other than string constant"));
      ConststrNode *conststrNode = fn->codeMemPool->New<ConststrNode>(PTY_ptr, static_cast<MIRStrConst *>(mirconst)->value);
      return conststrNode;
    } else {
      MIRSymbol *sym = ST_IDX2MIRSym(WN_st_idx(wn));
      FmtAssert(sym != NULL, ("EXPR2mpl: cannot find LDA symbol"));
      if (sym->sKind == kStVar) {
        AddrofNode *addrofNode = builder->CreateAddrof(sym, MTYPE2mpl(WN_rtype(wn)));
        if (WN_offset(wn) == 0) {
          UINT adjusted_field_id = GetAdjustedFieldID(st->u2.type, WN_field_id(wn));
          addrofNode->fieldID = static_cast<FieldID>(adjusted_field_id);
          return addrofNode;
        } else {  // ignroe the fieldid
          BaseNode *constvalNode = builder->CreateIntConst(WN_offset(wn), PTY_i64);
          return builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), addrofNode, constvalNode);
        }
      } else if (sym->sKind == kStFunc) {
        AddroffuncNode *aof = fn->codeMemPool->New<AddroffuncNode>();
        aof->primType = PTY_ptr;
        aof->puIdx = sym->GetFunction()->puIdx;
        return aof;
      } else {
        FmtAssert(FALSE, ("EXPR2mpl: LDA of unsupported symbol kind"));
      }
    }
  }
  case OPR_LDA_LABEL: {
    AddroflabelNode *aol = fn->codeMemPool->New<AddroflabelNode>(LABEL_NUMBER2mpl(WN_label_number(wn)));
    aol->primType = PTY_ptr;
    return aol;
  }
  case OPR_LDID: {
    MIRSymbol *sym = ST_IDX2MIRSym(WN_st_idx(wn));
    if (sym != NULL) {  // symbols
      PrimType primType = MTYPE2mpl(WN_rtype(wn));
      if (GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->GetTyIdx())->typeKind == kTypePointer) {
	primType = PTY_ptr;  // to avoid PTR_u64
      }
      MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
      MIRType *loadedType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
      ST *st = &St_Table[WN_st_idx(wn)];
      UINT adjusted_field_id = GetAdjustedFieldID(st->u2.type, WN_field_id(wn));
      FieldID fldID = static_cast<FieldID>(adjusted_field_id);
      if (WN_offset(wn) == 0 ||
          (fldID != 0 && (mod->srcLang == kSrcLangCPlusPlus || loadedType->GetSize() > WN_offset(wn)))) {
        AddrofNode *dreadNode = builder->CreateExprDread(mirtype, fldID, sym);
        return dreadNode;
      } else {  // split dread to addrof/iread in order to use WN_offset(wn)
        fldID = 0;
        AddrofNode *addrofNode = builder->CreateExprAddrof(fldID, sym, NULL);
        return GenIreadBasedOnDescOffset(addrofNode, mirtype, WN_desc(wn), WN_offset(wn));
      }
    } else if (WN_offset(wn) == -1) {  // fetching last call's return value
      if (lastRetPreg != 0) {
        PregIdx pregIdx = lastRetPreg;
        PrimType primType = MTYPE2mpl(WN_desc(wn));
        lastRetPreg = 0;
        return builder->CreateExprRegread(primType, pregIdx);
      } else if (lastRetTemp != NULL) {
        AddrofNode *dreadNode = builder->CreateExprDread(lastRetTemp->GetType(), 0, lastRetTemp);
        lastRetTemp = NULL;
        return dreadNode;
      } else {
        FmtAssert(FALSE, ("EXPR2mpl: cannot locate return value for LDID with -1 offset"));
      }
    } else {  // pregs
      PregIdx pregIdx = ST_IDX2Preg(WN_st_idx(wn), WN_offset(wn));
      PrimType primType = MTYPE2mpl(WN_desc(wn));
      return builder->CreateExprRegread(primType, pregIdx);
    }
  }
  case OPR_LDBITS: {
    MIRSymbol *sym = ST_IDX2MIRSym(WN_st_idx(wn));
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    AddrofNode *addrofNode = builder->CreateExprAddrof(0, sym, NULL);
    IreadNode *iread = GenIreadBasedOnDescOffset(addrofNode, mirtype, WN_desc(wn), 0/*offset*/);
    ExtractbitsNode *x = fn->codeMemPool->New<ExtractbitsNode>(OP_extractbits);
    x->primType = primType;
    x->uOpnd = iread;
    x->bitsOffset = WN_bit_offset(wn);
    x->bitsSize = WN_bit_size(wn);
    return x;
  }
  // unary ops starts
  case OPR_ILOAD: {
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    PrimType descPrimType = WN_desc(wn) == MTYPE_BS ? primType : MTYPE2mpl(WN_desc(wn));
    MIRType *ireadtype = TY_IDX2MIRType(WN_load_addr_ty(wn));
    MIRType *pointedType = static_cast<MIRPtrType *>(ireadtype)->GetPointedType();
    UINT adjusted_field_id = GetAdjustedFieldID(TY_pointed(WN_load_addr_ty(wn)), WN_field_id(wn));
    FieldID fldID = static_cast<FieldID>(adjusted_field_id);
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    if ((fldID == 0 || mod->srcLang != kSrcLangCPlusPlus && pointedType && pointedType->GetSize() <= WN_offset(wn)) &&
        WN_offset(wn) != 0) {
      fldID = 0;
      BaseNode *constvalNode = builder->CreateIntConst(WN_offset(wn), PTY_ptr);
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
      opnd0 = builder->CreateExprBinary(OP_add, mirType, opnd0, constvalNode);
    }
    MIRType *resMirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    if (GetPrimTypeSize(primType) == 8 && GetPrimTypeSize(descPrimType) == 4) {
      MIRType *descMirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(descPrimType));
      opnd0 =  builder->CreateExprIread(descMirtype, ireadtype, fldID, opnd0);
      return builder->CreateExprTypeCvt(OP_cvt, resMirtype, descMirtype, opnd0);
    }
    return builder->CreateExprIread(resMirtype, ireadtype, fldID, opnd0);
  }
  case OPR_NEG:
  case OPR_BNOT:
  case OPR_LNOT:
  case OPR_ABS:
  case OPR_RECIP:
  case OPR_SQRT:
  case OPR_ALLOCA: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    if (opr == OPR_ALLOCA) {
      primType = PTY_ptr;
    }
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    UnaryNode *x = builder->CreateExprUnary(OPERATOR2mpl(opr), mirtype, opnd0);
    return x;
  }
  case OPR_ILDA: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    MIRPtrType *iaddroftype = dynamic_cast<MIRPtrType *>(TY_IDX2MIRType(WN_ty(wn)));
    UINT adjusted_field_id = GetAdjustedFieldID(TY_pointed(WN_ty(wn)), WN_field_id(wn));
    IreadNode *x = builder->CreateExprIaddrof(mirtype, iaddroftype, static_cast<FieldID>(adjusted_field_id), opnd0);
    return x;
  }
  case OPR_CVT:
  case OPR_RND:
  case OPR_CEIL:
  case OPR_FLOOR:
  case OPR_TRUNC: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    PrimType opndPrimType = MTYPE2mpl(WN_desc(wn));
    TypeCvtNode *x = fn->codeMemPool->New<TypeCvtNode>(OPERATOR2mpl(opr));
    x->primType = primType;
    x->fromPrimType = opndPrimType;
    x->uOpnd = opnd0;
    return x;
  }
  case OPR_CVTL: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    if (GetPrimTypeSize(GetRegPrimType(opnd0->primType)) != GetPrimTypeSize(primType)) {
      MIRType *fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(GetRegPrimType(opnd0->primType)));
      MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
      opnd0 = builder->CreateExprTypeCvt(OP_cvt, mirtype, fromType, opnd0);
    }
    ExtractbitsNode *x = fn->codeMemPool->New<ExtractbitsNode>(IsSignedInteger(primType) ? OP_sext : OP_zext);
    x->primType = primType;
    x->uOpnd = opnd0;
    x->bitsOffset = 0;
    x->bitsSize = WN_offset(wn);
    return x;
  }
  case OPR_TAS: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *retypetype = TY_IDX2MIRType(WN_ty(wn));
    RetypeNode *x = fn->codeMemPool->New<RetypeNode>();
    x->primType = primType;
    x->uOpnd = opnd0;
    x->tyIdx = retypetype->tyIdx;
    return x;
  }
  case OPR_EXTRACT_BITS: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    ExtractbitsNode *x = fn->codeMemPool->New<ExtractbitsNode>(OP_extractbits);
    x->primType = primType;
    x->uOpnd = opnd0;
    x->bitsOffset = WN_bit_offset(wn);
    x->bitsSize = WN_bit_size(wn);
    return x;
  }
  // binary ops starts
  case OPR_SUB:
  case OPR_MPY:
  case OPR_DIV:
  case OPR_REM:
  case OPR_MAX:
  case OPR_MIN:
  case OPR_BAND:
  case OPR_BIOR:
  case OPR_BXOR:
  case OPR_LAND:
  case OPR_LIOR:
  case OPR_SHL:
  case OPR_ASHR:
  case OPR_LSHR: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    if (GetPrimTypeSize(GetRegPrimType(opnd0->primType)) != GetPrimTypeSize(primType)) {
      MIRType *fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(GetRegPrimType(opnd0->primType)));
      opnd0 = builder->CreateExprTypeCvt(OP_cvt, mirtype, fromType, opnd0);
    }
    if (GetPrimTypeSize(GetRegPrimType(opnd1->primType)) != GetPrimTypeSize(primType)) {
      MIRType *fromType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(GetRegPrimType(opnd1->primType)));
      opnd1 = builder->CreateExprTypeCvt(OP_cvt, mirtype, fromType, opnd1);
    }
    BinaryNode *x = builder->CreateExprBinary(OPERATOR2mpl(opr), mirtype, opnd0, opnd1);
    return x;
  }
  case OPR_ADD: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
    if ((opnd0->primType != PTY_ptr && opnd1->primType == PTY_ptr) ||
         opnd1->op == OP_addrof ||
         !OpCanFormAddress(opnd0->op)) { // swap the two operands
      BaseNode *tmp = opnd0;
      opnd0 = opnd1;
      opnd1 = tmp;
    }
    if (opnd0->primType == PTY_ptr && opnd1->op == OP_constval) {
      MIRPtrType *ptrType = FindPtrType(opnd0, fn);
      if (ptrType != NULL) {
	MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(ptrType->pointedTyIdx);
	MIRStructType *pointedStructType = dynamic_cast<MIRStructType *>(mirType);
	if (pointedStructType != NULL) { // try to generate iaddrof
	  MIRConst *mirConst = dynamic_cast<ConstvalNode *>(opnd1)->constVal;
	  int64 offset = dynamic_cast<MIRIntConst *>(mirConst)->value;
	  FieldID fieldID = FindFieldID4Offset(pointedStructType, offset, 1);
	  if (fieldID != 0) {
	    return builder->CreateExprIaddrof(ptrType, ptrType, fieldID, opnd0);
	  }
	}
      }
    }
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    BinaryNode *x = builder->CreateExprBinary(OPERATOR2mpl(opr), mirtype, opnd0, opnd1);
    return x;
  }
  case OPR_COMPOSE_BITS: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    DepositbitsNode *x = fn->codeMemPool->New<DepositbitsNode>();
    x->primType = primType;
    x->bOpnd[0] = opnd0;
    x->bOpnd[1] = opnd1;
    x->bitsOffset = WN_bit_offset(wn);
    x->bitsSize = WN_bit_size(wn);
    return x;
  }
  case OPR_EQ:
  case OPR_NE:
  case OPR_LT:
  case OPR_GT:
  case OPR_LE:
  case OPR_GE: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    MIRType *opndtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(MTYPE2mpl(WN_desc(wn))));
    CompareNode *x = builder->CreateExprCompare(OPERATOR2mpl(opr), mirtype, opndtype, opnd0, opnd1);
    return x;
  }
  case OPR_RROTATE: {
    // x rrotate n => (x >> n) | (x << (32-n))
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *rrotateAmt = EXPR2mpl(WN_kid1(wn), blk);
    PrimType valuePrimType = MTYPE2mpl(WN_desc(wn));
    MIRType *valueMirType = GlobalTables::GetTypeTable().typeTable[valuePrimType];
    BaseNode *shrNode = builder->CreateExprBinary(OP_lshr, valueMirType, opnd0, rrotateAmt);

    UINT valueSize = MTYPE_byte_size(WN_desc(wn));
    FmtAssert(valueSize == 4 || valueSize == 8, ("EXPR2mpl: cannot handle RROTATE with operand size %d", valueSize));
    PrimType shiftPrimType = rrotateAmt->primType;
    // form 32 - n
    BaseNode *sizeAmt = builder->CreateIntConst(valueSize*8, shiftPrimType);
    MIRType *shiftMirType = GlobalTables::GetTypeTable().typeTable[shiftPrimType];
    BaseNode *shlAmt = builder->CreateExprBinary(OP_sub, shiftMirType, sizeAmt, EXPR2mpl(WN_kid1(wn), blk));
    opnd0 = EXPR2mpl(WN_kid0(wn), blk);  // generate second time
    BaseNode *shlNode = builder->CreateExprBinary(OP_shl, valueMirType, opnd0, shlAmt);
    return builder->CreateExprBinary(OP_bior, valueMirType, shrNode, shlNode);
  }
  case OPR_COMMA: {
    WN *blk_wn = WN_kid0(wn);
    WN *wn2 = WN_first(blk_wn);
    while (wn2) {
      STMT2mpl(wn2, blk);
      wn2 = WN_next(wn2);
    }
    return EXPR2mpl(WN_kid1(wn), blk);
  }
  case OPR_CAND:
  case OPR_CIOR: {
    BaseNode *opnd0 = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    if (!HasCOMMA(WN_kid1(wn))) {
      BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
      BinaryNode *x = builder->CreateExprBinary(OPERATOR2mpl(opr), mirtype, opnd0, opnd1);
      return x;
    }
    // generate short-circuit code
    PregIdx pregIdx = fn->pregTab->CreatePreg(primType);
//  opnd0 = builder->CreateExprCompare(OP_ne, mirtype, mirtype, opnd0, builder->CreateIntConst(0, primType));
    RegassignNode *regass = builder->CreateStmtRegassign(primType, pregIdx, opnd0);
    blk->AddStatement(regass);
    LabelIdx labIdx = fn->labelTab->CreateLabel();
    fn->labelTab->AddToStringLabelMap(labIdx);
    BaseNode *cond = builder->CreateExprRegread(primType, pregIdx);
    CondGotoNode *cgoto = fn->codeMemPool->New<CondGotoNode>(opr == OPR_CIOR ? OP_brtrue : OP_brfalse);
    cgoto->uOpnd = cond;
    cgoto->offset = labIdx;
    blk->AddStatement(cgoto);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
//  opnd1 = builder->CreateExprCompare(OP_ne, mirtype, mirtype, opnd1, builder->CreateIntConst(0, primType));
    regass = builder->CreateStmtRegassign(primType, pregIdx, opnd1);
    blk->AddStatement(regass);
    LabelNode *lbl = fn->codeMemPool->New<LabelNode>();
    lbl->labelIdx = labIdx;
    blk->AddStatement(lbl);
    return builder->CreateExprRegread(primType, pregIdx);
  }
  // ternary ops starts
  case OPR_SELECT: {
    BaseNode *cond = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
    BaseNode *opnd2 = EXPR2mpl(WN_kid2(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));
    MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
    TernaryNode *x = builder->CreateExprTernary(OPERATOR2mpl(opr), mirtype, cond, opnd1, opnd2);
    return x;
  }
  case OPR_CSELECT: {  // generate if-then-else
    BaseNode *cond = EXPR2mpl(WN_kid0(wn), blk);
    PrimType primType = MTYPE2mpl(WN_rtype(wn));

    if (!HasSideEffect(WN_kid1(wn), mod->srcLang == kSrcLangCPlusPlus) && !HasSideEffect(WN_kid2(wn), mod->srcLang == kSrcLangCPlusPlus)) {
      BaseNode *opnd1 = EXPR2mpl(WN_kid1(wn), blk);
      BaseNode *opnd2 = EXPR2mpl(WN_kid2(wn), blk);
      MIRType *mirtype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(primType));
      TernaryNode *x = builder->CreateExprTernary(OP_select, mirtype, cond, opnd1, opnd2);
      return x;
    }

    BlockNode *thenblk = fn->codeMemPool->New<BlockNode>();
    BaseNode *thenexpr = EXPR2mpl(WN_kid1(wn), thenblk);
    FmtAssert(WN_rtype(wn) != MTYPE_M, ("EXPR2mpl: cannot handle MCSELECT"));
    PregIdx pregIdx = fn->pregTab->CreatePreg(primType);
    RegassignNode *regass = builder->CreateStmtRegassign(primType, pregIdx, thenexpr);
    thenblk->AddStatement(regass);

    BlockNode *elseblk = fn->codeMemPool->New<BlockNode>();
    BaseNode *elseexpr = EXPR2mpl(WN_kid2(wn), elseblk);
    regass = builder->CreateStmtRegassign(primType, pregIdx, elseexpr);
    elseblk->AddStatement(regass);

    IfStmtNode *s = fn->codeMemPool->New<IfStmtNode>();
    s->uOpnd = cond;
    s->thenPart = thenblk;
    s->elsePart = elseblk;
    blk->AddStatement(s);
    return builder->CreateExprRegread(primType, pregIdx);
  }
  // n-ary ops starts
  case OPR_ARRAY: {
    ArrayNode *x = fn->codeMemPool->New<ArrayNode>(&fn->codeMemPoolAllocator, FALSE);
    x->primType = PTY_ptr;
    UINT numDim = WN_kid_count(wn) >> 1;
    BaseNode *opnd = EXPR2mpl(WN_kid0(wn), blk); // the base address
    x->nOpnd.push_back(opnd);
    std::vector<uint32> sizeArray;
    for (UINT i = 1; i <= numDim; i++) {
      opnd = EXPR2mpl(WN_kid(wn, numDim+i), blk);
      sizeArray.push_back(GetDimSize(WN_kid(wn, i)));
      x->nOpnd.push_back(opnd);
    }
    x->numOpnds = x->nOpnd.size();
    MIRPtrType *arrayType = FindArrayType(x->Opnd(0), fn, WN_element_size(wn));
    MIRArrayType *matchedArrayType = NULL;
    if (arrayType != NULL) {
      matchedArrayType = MatchArrayType(arrayType->GetPointedType(), WN_element_size(wn), sizeArray);
    }
    if (matchedArrayType == NULL) {
      FmtAssert(WN_element_size(wn) <= 8, ("Expr2mpl: cannot match or create appropriate array type"));
      TYPE_ID elem_type_id = 0;
      switch (WN_element_size(wn)) {
      case 1: elem_type_id = MTYPE_I1; break;
      case 2: elem_type_id = MTYPE_I2; break;
      case 4: elem_type_id = MTYPE_I4; break;
      case 8: elem_type_id = MTYPE_I8; break;
      }
      MIRArrayType arrayType(TyIdx(MTYPE2mpl(elem_type_id)), sizeArray);
      matchedArrayType = static_cast<MIRArrayType *>(GlobalTables::GetTypeTable().GetOrCreateMIRTypeNode(&arrayType));
    }
    x->tyIdx = GlobalTables::GetTypeTable().GetOrCreatePointerType(matchedArrayType, PTY_ptr)->tyIdx;
    return x;
  }
  case OPR_INTRINSIC_OP: {
    if (WN_intrinsic(wn) == INTRN_EXPECT) {
      // ignore, just take the first argument
      return EXPR2mpl(WN_kid0(WN_kid0(wn)), blk);
    } else {

      MIRIntrinsicID intrinsicID = INTRINSIC2mpl(WN_intrinsic(wn));
      PrimType primType = MTYPE2mpl(WN_rtype(wn));
      IntrinsicopNode *x = fn->codeMemPool->New<IntrinsicopNode>(&fn->codeMemPoolAllocator, OP_intrinsicop, primType);
      x->intrinsic = intrinsicID;

      for (UINT i = 0; i < WN_kid_count(wn); i++) {
        WN *parmwn = WN_kid(wn, i);
        FmtAssert(WN_operator(parmwn) == OPR_PARM, ("EXPR2mpl: expecting PARM but get %s", OPERATOR_name(WN_operator(parmwn))));
        BaseNode *opnd = EXPR2mpl(WN_kid0(parmwn), blk);
        x->nOpnd.push_back(opnd);
      }
      x->numOpnds = x->nOpnd.size();
      return x;
    }
  }
  default: FmtAssert(FALSE, ("EXPR2mpl: operator %s NYI", OPERATOR_name(opr)));
  }
}

LabelIdx WHIRL2mpl::LABEL_NUMBER2mpl(INT32 label_number) {
  std::string labName("L");
  labName.append(std::to_string(label_number));
  GStrIdx labStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(labName);
  LabelIdx labidx = fn->labelTab->strIdxToLabIdxMap[labStrIdx];
  if (labidx == 0) {
    labidx = fn->labelTab->CreateLabel();
    fn->labelTab->labelTable.at(labidx) = labStrIdx;
    fn->labelTab->AddToStringLabelMap(labidx);
  }
  return labidx;
}

// wn is a expression that computes to a function pointer; find the TY_IDX of
// the KIND_FUNCTION TY node
static TY_IDX GetFuncType(WN *wn) {
  switch (WN_operator(wn)) {
  case OPR_LDA: {
    ST *st = &St_Table[WN_st_idx(wn)];
    FmtAssert(st->sym_class == CLASS_FUNC, ("GetFuncType: found non-function symbol"));
    return PU_prototype(Pu_Table[st->u2.pu]);
  }
  case OPR_LDID: {
    ST *st = &St_Table[WN_st_idx(wn)];
    TY_IDX st_ty_idx = st->u2.type;
    if (WN_field_id(wn) == 0) {
      TY *st_ty = &Ty_Table[st_ty_idx];
      return st_ty->Pointed();
    } else {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(st_ty_idx, WN_field_id(wn), cur_field_id);
      TY *fld_ty = &Ty_Table[FLD_type(fld)];
      return fld_ty->Pointed();
    }
  }
  case OPR_ILOAD: {
    TY_IDX iload_ty_idx = WN_load_addr_ty(wn);
    TY *iload_ty = &Ty_Table[iload_ty_idx];
    if (WN_field_id(wn) == 0) {
      return iload_ty->Pointed();
    } else {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(iload_ty->Pointed(), WN_field_id(wn), cur_field_id);
      TY *fld_ty = &Ty_Table[FLD_type(fld)];
      return fld_ty->Pointed();
    }
  }
  case OPR_SELECT:
  case OPR_CSELECT: return GetFuncType(WN_kid1(wn));
  default: FmtAssert(FALSE, ("GetFuncType: cannot find ICALL functype type"));
  }
  return 0;
}

CallReturnPair WHIRL2mpl::CreateReturnPair4Call(TYPE_ID mtype, MIRType *retType) {
  if (mtype != MTYPE_M && mtype != MTYPE_V) {
    lastRetPreg = fn->pregTab->CreatePreg(MTYPE2mpl(mtype));
    lastRetTemp = NULL;
    RegFieldPair regFldPair(0, lastRetPreg);
    return CallReturnPair(StIdx(), regFldPair);
  }
  std::string tempName = "_ret.temp" + std::to_string(++retTempCounter);
  lastRetTemp = builder->CreateLocalDecl(tempName, retType);
  lastRetPreg = 0;
  return std::make_pair(lastRetTemp->stIdx, RegFieldPair());
}

static void SRCPOS2mpl(WN *wn, StmtNode *stmt) {
  USRCPOS srcpos;
  USRCPOS_srcpos(srcpos) = WN_Get_Linenum(wn);
  stmt->srcPosition.SetRawData(srcpos.fillers[0]);
  stmt->srcPosition.SetLinenum(srcpos.fillers[1]);
}

void WHIRL2mpl::STMT2mpl(WN *wn, BlockNode *blk) {
  OPERATOR opr = WN_operator(wn);
  StmtNode *stmt = NULL;
  switch (opr) {
  case OPR_LABEL: {
    LabelIdx labidx = LABEL_NUMBER2mpl(WN_label_number(wn));
    LabelNode *s = fn->codeMemPool->New<LabelNode>();
    s->labelIdx = labidx;
    if (WN_Label_Is_Handler_Begin(wn)) {
      SRCPOS2mpl(wn, s);
      blk->AddStatement(s);
      CppCatchNode *cppCatch = fn->codeMemPool->New<CppCatchNode>();
      blk->AddStatement(cppCatch);
      return;
    }
    stmt = s;
    break;
  }
  case OPR_GOTO: {
    LabelIdx labidx = LABEL_NUMBER2mpl(WN_label_number(wn));
    GotoNode *s = fn->codeMemPool->New<GotoNode>(OP_goto);
    s->offset = labidx;
    stmt = s;
    break;
  }
  case OPR_FALSEBR:
  case OPR_TRUEBR: {
    LabelIdx labidx = LABEL_NUMBER2mpl(WN_label_number(wn));
    BaseNode *cond = EXPR2mpl(WN_if_test(wn), blk);
    CondGotoNode *s = fn->codeMemPool->New<CondGotoNode>(OPERATOR2mpl(opr));
    s->offset = labidx;
    s->uOpnd = cond;
    stmt = s;
    break;
  }
  case OPR_STID: {
    BaseNode *rhs = EXPR2mpl(WN_kid0(wn), blk);
    MIRSymbol *sym = ST_IDX2MIRSym(WN_st_idx(wn));
    if (sym != NULL) {
      MIRType *storeType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
      ST *st = &St_Table[WN_st_idx(wn)];
      UINT adjusted_field_id = GetAdjustedFieldID(st->u2.type, WN_field_id(wn));
      FieldID fldID = static_cast<FieldID>(adjusted_field_id);
      MIRType *rhsAggType = FindAggType(rhs, fn);
      if (WN_offset(wn) == 0 ||
          (fldID != 0 && (mod->srcLang == kSrcLangCPlusPlus || storeType->GetSize() > WN_offset(wn)))) {
        DassignNode *s = builder->CreateStmtDassign(sym, fldID, rhs);
        stmt = s;
      } else {  // split dassign to addrof/istore in order to use WN_offset(wn)
        fldID = 0;
        BaseNode *iassLhs = builder->CreateExprAddrof(0, sym, NULL);
        BaseNode *constvalNode = builder->CreateIntConst(WN_offset(wn), PTY_i64);
        iassLhs = builder->CreateExprBinary(OP_add, GlobalTables::GetTypeTable().GetPtr(), iassLhs, constvalNode);
        if (rhsAggType == NULL) {
          rhsAggType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(MTYPE2mpl(WN_desc(wn))));
        }
        MIRType *iassPtrType = GlobalTables::GetTypeTable().GetOrCreatePointerType(rhsAggType->tyIdx, PTY_ptr);
        IassignNode *s = builder->CreateStmtIassign(iassPtrType, fldID, iassLhs, rhs);
        stmt = s;
      }
    } else {
      PregIdx pregIdx = ST_IDX2Preg(WN_st_idx(wn), WN_offset(wn));
      PrimType primType = fn->pregTab->PregFromPregIdx(pregIdx)->primType;
      RegassignNode *s = builder->CreateStmtRegassign(primType, pregIdx, rhs);
      stmt = s;
    }
    break;
  }
  case OPR_ISTORE: {
    BaseNode *rhs = EXPR2mpl(WN_kid0(wn), blk);
    BaseNode *lhs = EXPR2mpl(WN_kid1(wn), blk);
    MIRType *istoretype = TY_IDX2MIRType(WN_ty(wn));
    MIRPtrType *ptrType = FindPtrType(lhs, fn);
    MIRType *pointedType = NULL;
    if (ptrType) {
      pointedType = ptrType->GetPointedType();
    }
    UINT adjusted_field_id = GetAdjustedFieldID(TY_pointed(WN_ty(wn)), WN_field_id(wn));
    FieldID fldID = static_cast<FieldID>(adjusted_field_id);
    if (((fldID == 0) ||
         (mod->srcLang != kSrcLangCPlusPlus && pointedType && pointedType->GetSize() <= WN_offset(wn)) ||
         (pointedType == GlobalTables::GetTypeTable().GetVoid())) &&
        WN_offset(wn) != 0) {
      if (pointedType != GlobalTables::GetTypeTable().GetVoid()) {
        fldID = 0;
      }
      BaseNode *constvalNode = builder->CreateIntConst(WN_offset(wn), PTY_ptr);
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
      lhs = builder->CreateExprBinary(OP_add, mirType, lhs, constvalNode);
    }
    IassignNode *s = builder->CreateStmtIassign(istoretype, fldID, lhs, rhs);
    stmt = s;
    break;
  }
  case OPR_MSTORE: {
    BaseNode *lhs = EXPR2mpl(WN_kid1(wn), blk);
    if (WN_offset(wn) != 0) {
      BaseNode *constvalNode = builder->CreateIntConst(WN_offset(wn), PTY_ptr);
      MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
      lhs = builder->CreateExprBinary(OP_add, mirType, lhs, constvalNode);
    }
    BaseNode *len = EXPR2mpl(WN_kid2(wn), blk);
    BaseNode *rhs;
    IntrinsiccallNode *s;
    if (WN_operator(WN_kid0(wn)) == OPR_MLOAD) {
      s = fn->codeMemPool->New<IntrinsiccallNode>(&fn->codeMemPoolAllocator, OP_intrinsiccall, INTRN_C_memcpy);
      WN *mload_wn = WN_kid0(wn);
      rhs = EXPR2mpl(WN_kid0(mload_wn), blk);
      if (WN_offset(mload_wn) != 0) {
        BaseNode *constvalNode = builder->CreateIntConst(WN_offset(mload_wn), PTY_ptr);
        MIRType *mirType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
        rhs = builder->CreateExprBinary(OP_add, mirType, rhs, constvalNode);
      }
    } else {
      s = fn->codeMemPool->New<IntrinsiccallNode>(&fn->codeMemPoolAllocator, OP_intrinsiccall, INTRN_C_memset);
      rhs = EXPR2mpl(WN_kid0(wn), blk);
    }
    s->nOpnd.push_back(lhs);
    s->nOpnd.push_back(rhs);
    s->nOpnd.push_back(len);
    s->numOpnds = 3;
    stmt = s;
    break;
  }
  case OPR_IF: {
    BaseNode *cond = EXPR2mpl(WN_if_test(wn), blk);
    BlockNode *thenpart = BLOCK2mpl(WN_then(wn));
    BlockNode *elsepart = NULL;
    if (WN_else(wn)) {
      elsepart = BLOCK2mpl(WN_else(wn), true/*nullIfEmpty*/);
    }
    IfStmtNode *s = fn->codeMemPool->New<IfStmtNode>();
    s->uOpnd = cond;
    s->thenPart = thenpart;
    s->elsePart = elsepart;
    stmt = s;
    break;
  }
  case OPR_DO_WHILE:
  case OPR_WHILE_DO: {
    BaseNode *cond = EXPR2mpl(WN_while_test(wn), blk);
    BlockNode *whilebody = BLOCK2mpl(WN_while_body(wn));
    WhileStmtNode *s = fn->codeMemPool->New<WhileStmtNode>(OPERATOR2mpl(opr));
    s->uOpnd = cond;
    s->body = whilebody;
    stmt = s;
    break;
  }
  case OPR_SWITCH: {
    BaseNode *opnd = EXPR2mpl(WN_switch_test(wn), blk);
    SwitchNode *s = fn->codeMemPool->New<SwitchNode>(mod);
    s->switchOpnd = opnd;
    LabelIdx dftlabidx = 0;
    if (WN_kid_count(wn) > 2) {
      WN *dftgoto = WN_switch_default(wn);
      dftlabidx = LABEL_NUMBER2mpl(WN_label_number(dftgoto));
    }
    s->defaultLabel = dftlabidx;
    WN *switchblk = WN_kid1(wn);
    WN *wn2 = WN_first(switchblk);
    while (wn2) {
      CasePair casepair = std::make_pair(WN_const_val(wn2), LABEL_NUMBER2mpl(WN_label_number(wn2)));
      s->switchTable.push_back(casepair);
      wn2 = WN_next(wn2);
    }
    stmt = s;
    break;
  }
  case OPR_CALL: {
    MIRSymbol *sym = ST_IDX2MIRSym(WN_st_idx(wn));
    FmtAssert(sym != NULL, ("STMT2mpl: cannot find function in call"));
    if (sym->GetName() == "setjmp") {
      fn->SetHasSetjmp();
    }
    Opcode callop = WN_rtype(wn) == MTYPE_V ? OP_call : OP_callassigned;
    MIRFunction *callee = sym->GetFunction();
    if (callee->IsReturnStruct()) {
      callop = OP_callassigned;
    }
    CallNode *s = fn->codeMemPool->New<CallNode>(&fn->codeMemPoolAllocator, callop);
    s->puIdx = callee->puIdx;
    for (UINT i = 0; i < WN_kid_count(wn); i++) {
      WN *parmwn = WN_kid(wn, i);
      FmtAssert(WN_operator(parmwn) == OPR_PARM, ("STMT2mpl: expecting PARM but get %s", OPERATOR_name(WN_operator(parmwn))));
      BaseNode *opnd = EXPR2mpl(WN_kid0(parmwn), blk);
      s->nOpnd.push_back(opnd);
    }
    s->numOpnds = s->nOpnd.size();
    if (callop == OP_callassigned) {
      CallReturnPair crp = CreateReturnPair4Call(WN_rtype(wn), callee->GetReturnType());
      s->returnValues.push_back(crp);
    }
    stmt = s;
    break;
  }
  case OPR_ICALL: {
    BaseNode *funcptrOpnd = EXPR2mpl(WN_kid(wn, WN_kid_count(wn)-1), blk);
    TY_IDX func_ty_idx = GetFuncType(WN_kid(wn, WN_kid_count(wn)-1));
    MIRFuncType *funcType = static_cast<MIRFuncType *>(TY_IDX2MIRType(func_ty_idx));
    Opcode icallop = WN_rtype(wn) == MTYPE_V ? OP_icall : OP_icallassigned;
    MIRType *retType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(funcType->retTyIdx);
    if (retType->typeKind == kTypeStruct || retType->typeKind == kTypeUnion) {
      icallop = OP_icallassigned;
    }
    IcallNode *s = fn->codeMemPool->New<IcallNode>(&fn->codeMemPoolAllocator, icallop);
    s->nOpnd.push_back(funcptrOpnd);
    for (UINT i = 0; i < WN_kid_count(wn)-1; i++) {
      WN *parmwn = WN_kid(wn, i);
      FmtAssert(WN_operator(parmwn) == OPR_PARM, ("STMT2mpl: expecting PARM but get %s", OPERATOR_name(WN_operator(parmwn))));
      BaseNode *opnd = EXPR2mpl(WN_kid0(parmwn), blk);
      s->nOpnd.push_back(opnd);
    }
    s->numOpnds = s->nOpnd.size();
    if (icallop == OP_icallassigned) {
      CallReturnPair crp = CreateReturnPair4Call(WN_rtype(wn), retType);
      s->returnValues.push_back(crp);
    }
    stmt = s;
    break;
  }
  case OPR_INTRINSIC_CALL: {
    MIRIntrinsicID intrinsicID = INTRINSIC2mpl(WN_intrinsic(wn));
    IntrinsiccallNode *s = fn->codeMemPool->New<IntrinsiccallNode>(
        &fn->codeMemPoolAllocator,
        WN_rtype(wn) == MTYPE_V ? OP_intrinsiccall : OP_intrinsiccallassigned,
        intrinsicID);
    for (UINT i = 0; i < WN_kid_count(wn); i++) {
      WN *parmwn = WN_kid(wn, i);
      FmtAssert(WN_operator(parmwn) == OPR_PARM, ("STMT2mpl: expecting PARM but get %s", OPERATOR_name(WN_operator(parmwn))));
      BaseNode *opnd = EXPR2mpl(WN_kid0(parmwn), blk);
      s->nOpnd.push_back(opnd);
    }
    s->numOpnds = s->nOpnd.size();
    if (s->op == OP_intrinsiccallassigned) {
      lastRetPreg = fn->pregTab->CreatePreg(MTYPE2mpl(WN_rtype(wn)));
      RegFieldPair regFldPair(0, lastRetPreg);
      s->returnValues.push_back(CallReturnPair(StIdx(), regFldPair));
    }
    stmt = s;
    break;
  }
  case OPR_EVAL: {
    BaseNode *evalopnd = EXPR2mpl(WN_kid0(wn), blk);
    UnaryStmtNode *s = builder->CreateStmtUnary(OP_eval, evalopnd);
    stmt = s;
    break;
  }
  case OPR_RETURN_VAL: {
    BaseNode *retval = EXPR2mpl(WN_kid0(wn), blk);
    NaryStmtNode *s = builder->CreateStmtReturn(retval);
    stmt = s;
    break;
  }
  case OPR_RETURN: {
    BaseNode *retval = NULL;
    if (fn->funcType->retTyIdx.GetIdx() != PTY_void) {
      MIRType *retType = fn->GetReturnType();
      if (retType->primType == PTY_f32) {
        retval = builder->CreateFloatConst(0.0);
      } else if (retType->primType == PTY_f64) {
        retval = builder->CreateDoubleConst(0.0d);
      } else if (GetPrimTypeSize(GetRegPrimType(retType->primType)) == 8) {
        retval = builder->CreateIntConst(0, PTY_u64);
      } else {
        retval = builder->CreateIntConst(0, PTY_u32);
      }
    }
    NaryStmtNode *s = builder->CreateStmtReturn(retval);
    stmt = s;
    break;
  }
  case OPR_COMMENT: {
    std::string str(Index_To_Str(WN_offset(wn)));
    CommentNode *s = fn->codeMemPool->New<CommentNode>(mod);
    s->comment = str;
    stmt = s;
    break;
  }
  case OPR_AGOTO: {
    BaseNode *evalopnd = EXPR2mpl(WN_kid0(wn), blk);
    UnaryStmtNode *s = builder->CreateStmtUnary(OP_igoto, evalopnd);
    stmt = s;
    break;
  }
  case OPR_REGION: {
    if (WN_region_kind(wn) == REGION_KIND_TRY) {
      BlockNode *s = BLOCK2mpl(WN_region_body(wn));
      WN *pragmaBlockWn = WN_region_pragmas(wn);
      WN *wn2 = WN_first(pragmaBlockWn);
      std::vector<LabelIdx> catchLabels;
      while (wn2 != NULL) {
        FmtAssert(WN_operator(wn2) == OPR_GOTO, ("STMT2mpl: non-GOTO found in Try Region pragma block"));
        LabelIdx labidx = LABEL_NUMBER2mpl(WN_label_number(wn2));
        catchLabels.push_back(labidx);
        wn2 = WN_next(wn2);
      }
      if (catchLabels.size() == 0) {
        INITO_IDX inidx = WN_ereg_supp(wn);
        if (inidx != 0) {
          ST_IDX st_idx = INITO_st_idx(Inito_Table[inidx]);
          INITV_IDX initv_idx = INITO_val(Inito_Table[inidx]);
          INITV &initv = Initv_Table[initv_idx];
          if (INITV_kind(initv) == INITVKIND_BLOCK) {
            INITV_IDX idx = INITV_blk(initv);
            while (idx) {
              INITV &initv0 = Initv_Table[idx];
              if (INITV_kind(initv0) == INITVKIND_LABEL) {
                LabelIdx lidx = LABEL_NUMBER2mpl(INITV_lab(initv0));
                catchLabels.push_back(lidx);
              }
              idx = INITV_next(initv0);
            }
          }
        }
      }
      TryNode *tryNode = fn->codeMemPool->New<TryNode>(mod, OP_cpptry);
      for (LabelIdx lidx : catchLabels) {
        tryNode->offsets.push_back(lidx);
      }
      s->InsertFirst(tryNode);
      StmtNode *endtryNode = fn->codeMemPool->New<StmtNode>(OP_endtry);
      s->InsertLast(endtryNode);
      blk->AppendStatementsFromBlock(s);
    }
    return;
  }
#if 0
  case OPR_ASM_STMT: {
    FmtAssert(FALSE, ("STMT2mpl: opcode %s NYI", OPERATOR_name(opr)));
    return;
  }
#endif
  default: {
    printf("%s NYI\n", OPERATOR_name(opr));
    return;
  }
  }
  SRCPOS2mpl(wn, stmt);
  blk->AddStatement(stmt);
}

BlockNode *WHIRL2mpl::BLOCK2mpl(WN *wn, bool nullIfEmpty) {
  FmtAssert(WN_opcode(wn) == OPC_BLOCK, ("BLOCK2mpl: node is not BLOCK but is %s", OPCODE_name(WN_opcode(wn))));
  WN *wn2 = WN_first(wn);
  if (nullIfEmpty && wn2 == NULL) {
    return NULL;
  }
  BlockNode *block = fn->codeMemPool->New<BlockNode>();
  SRCPOS2mpl(wn, block);
  while (wn2 != NULL) {
    STMT2mpl(wn2, block);
    wn2 = WN_next(wn2);
  }
  return block;
}

MIRConst *WHIRL2mpl::TCON_IDX2mpl(TCON_IDX tc, MIRType *mirtype) {
  TCON &c = Tcon_Table[tc];
  if (mirtype == NULL) {
    mirtype = MTYPE2MIRType(TCON_ty(c));
  }
  switch (TCON_ty(c)) {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
    return mod->memPool->New<MIRIntConst>((int64)c.vals.ival.v0, mirtype);
  case MTYPE_B:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
    return mod->memPool->New<MIRIntConst>((int64)c.vals.uval.u0, mirtype);
  case MTYPE_I8:
  case MTYPE_U8:
    return mod->memPool->New<MIRIntConst>(c.vals.llval.ll0, mirtype);
  case MTYPE_F4:
    return mod->memPool->New<MIRFloatConst>(c.vals.fval, mirtype);
  case MTYPE_F8:
    return mod->memPool->New<MIRDoubleConst>(c.vals.dval, mirtype);
  case MTYPE_STRING: {
    INT slen = c.vals.sval.len;
    char *bytes = Index_to_char_array(c.vals.sval.cp);
    if (mirtype->typeKind != kTypeArray) {
      std::string uString(bytes, slen);
      UStrIdx uStrIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(uString);
      MIRType *strType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr));
      MIRStrConst *strConst = mod->memPool->New<MIRStrConst>(uStrIdx, strType);
      return strConst;
    } else {
      MIRArrayType *arrayType = static_cast<MIRArrayType *>(mirtype);
      MIRType *etype = GlobalTables::GetTypeTable().GetTypeFromTyIdx(arrayType->eTyIdx);
      INT esize = etype->GetSize();
      MIRAggConst *aggConst = mod->memPool->New<MIRAggConst>(mod, mirtype);
      INT i = 0;
      while (i < slen) {
	UINT64 value = (unsigned char)bytes[i];
        i++;
        for (INT j = 1; j < esize && i < slen; j++, i++) {
          value != (bytes[i] << j*8);
        }
        MIRIntConst *intConst = mod->memPool->New<MIRIntConst>(value, etype);
        aggConst->constVec.push_back(intConst);
      }
      return aggConst;
    }
  }
  default:
    FmtAssert(FALSE, ("TCON_IDX2mpl: mtype %s NYI", MTYPE_name(TCON_ty(c))));
  }
  return NULL;
}

MIRConst *WHIRL2mpl::INITV_IDX2mpl4field(INITV_IDX idx, uint32 byteOfst, uint32 bitOfst, MIRType *fieldType) {
  if (fieldType->typeKind != kTypeBitField) {
    uint32 initvByteOfst = 0;
    while (initvByteOfst < byteOfst && idx != 0) {
      INITV &initv0 = Initv_Table[idx];
      initvByteOfst += Get_INITV_Size(idx);
      idx = INITV_next(initv0);
    }
    if (idx == 0) {
      return NULL;
    }
    MIRConst *mirConst = INITV_IDX2mpl(idx, fieldType);
    return mirConst;
  } else {
    MIRBitfieldType *bitfieldType = static_cast<MIRBitfieldType *>(fieldType);
    uint32 initvBitOfst = 0;
    while (idx != 0 && (initvBitOfst + Get_INITV_Size(idx)*8) <= bitOfst) {
      INITV &initv0 = Initv_Table[idx];
      initvBitOfst += Get_INITV_Size(idx)*8;
      idx = INITV_next(initv0);
    }
    if (idx == 0 || INITV_kind(idx) == INITVKIND_PAD) {
      return NULL;
    }
    MIRIntConst *mirIntConst;
    uint32 bitOffsetTarget = bitOfst + bitfieldType->fieldSize;
    INT numNotYetCoveredBits = 0;
    switch (INITV_kind(idx)) {
    case INITVKIND_ZERO:
    case INITVKIND_ONE:
    case INITVKIND_VAL: {
      mirIntConst = dynamic_cast<MIRIntConst *>(INITV_IDX2mpl(idx, fieldType));
      uint64 bits = mirIntConst->value;
      numNotYetCoveredBits = bitOffsetTarget - (initvBitOfst + Get_INITV_Size(idx)*8);
#if HOST_IS_LITTLE_ENDIAN
      UINT numBitsToDiscard = bitOfst - initvBitOfst;
      bits = bits >> numBitsToDiscard;  // discard initial bits
      if (numNotYetCoveredBits <= 0) { // entire bitfield covered
	bits = bits << (64 - bitfieldType->fieldSize);
	bits = bits >> (64 - bitfieldType->fieldSize);
      } else { // only cover low-order part of bitfield
	bits = bits << (64 - (Get_INITV_Size(idx)*8 - numBitsToDiscard));
	bits = bits >> (64 - (Get_INITV_Size(idx)*8 - numBitsToDiscard));
      }
#else
#endif
      mirIntConst->value = bits;
      break;
    }
    default: {
      FmtAssert(FALSE, ("INITV_IDX2mpl4field: non-matching initv for bitfield"));
      return NULL;
    }
    }
    INT numBitsCovered = bitfieldType->fieldSize - numNotYetCoveredBits;
    while (numNotYetCoveredBits > 0) {
      initvBitOfst += Get_INITV_Size(idx)*8;
      INITV &initv0 = Initv_Table[idx];
      idx = INITV_next(initv0);
      MIRIntConst *mirIntConst1 = dynamic_cast<MIRIntConst *>(INITV_IDX2mpl(idx, fieldType));
      uint64 bits = mirIntConst1->value;
      numNotYetCoveredBits = bitOffsetTarget - (initvBitOfst + Get_INITV_Size(idx)*8);
#if HOST_IS_LITTLE_ENDIAN
      if (numNotYetCoveredBits <= 0) { // entire bitfield covered
        bits = bits << (64 - (bitfieldType->fieldSize - numBitsCovered));
        bits = bits >> (64 - bitfieldType->fieldSize);
      } else { // only cover middle part of bitfield
	bits = bits << (64 - Get_INITV_Size(idx)*8);
	bits = bits >> (64 - numBitsCovered - Get_INITV_Size(idx)*8);
        numBitsCovered += Get_INITV_Size(idx)*8;
      }
#else
#endif
      mirIntConst->value |= bits;
    }
    return mirIntConst;
  }
}

MIRAggConst *WHIRL2mpl::INITV_IDX2mpl4struct(INITV_IDX idx, MIRStructType *structType) {
  MIRAggConst *aggConst = mod->memPool->New<MIRAggConst>(mod, structType);
  uint32 byteOfst = 0;
  uint32 bitOfst = 0;
  for (int32 fieldIdx = 0; fieldIdx < structType->fields.size(); fieldIdx++) {
    TyIdx fTyIdx = structType->fields[fieldIdx].second.first;
    MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
    if (fieldType->GetSize() == 0) {
      if (fieldType->typeKind == kTypeBitField) {  // do aligning
        MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
        bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
        byteOfst = bitOfst >> 3;
      }
      continue;
    }
    if (fieldType->typeKind != kTypeBitField) {
      if (byteOfst * 8 < bitOfst) {
        byteOfst = (bitOfst >> 3) + 1;
      }
      byteOfst = RoundUp(byteOfst, fieldType->GetAlign());
      bitOfst = byteOfst * 8;
    } else {
      MIRBitfieldType *bitfType = static_cast<MIRBitfieldType *>(fieldType);
      if (RoundDown(bitOfst + bitfType->fieldSize - 1, GetPrimTypeBitSize(bitfType->primType)) !=
          RoundDown(bitOfst, GetPrimTypeBitSize(bitfType->primType))) {
        bitOfst = RoundUp(bitOfst, GetPrimTypeBitSize(bitfType->primType));
        byteOfst = bitOfst >> 3;
      }
    }
    MIRConst *fmirConst = INITV_IDX2mpl4field(idx, byteOfst, bitOfst, fieldType);
    if (fmirConst == NULL && fieldType->typeKind != kTypeStruct &&
        fieldType->typeKind != kTypeUnion && fieldType->typeKind != kTypeArray) {
      MIRIntConst *intConst = mod->memPool->New<MIRIntConst>(0, fieldType);
      fmirConst = intConst;
    }
    if (fmirConst != NULL) {
      fmirConst->fieldID = fieldIdx+1;
      aggConst->constVec.push_back(fmirConst);
    }
    if (fieldType->typeKind != kTypeBitField) {
      byteOfst += fieldType->GetSize();
      bitOfst = byteOfst * 8;
    } else {
      bitOfst += static_cast<MIRBitfieldType *>(fieldType)->fieldSize;
      byteOfst = bitOfst >> 3;
    }
  }
  return aggConst;
}

static bool ScalarTypesMatched(TYPE_ID mtype, PrimType primType) {
  if (primType == PTY_ptr && mtype == MTYPE_U8) {
    return TRUE;
  }
  if (MTYPE_byte_size(mtype) != GetPrimTypeSize(primType)) {
    return FALSE;
  }
  if (mtype == MTYPE_F4 && primType == PTY_f32) {
    return TRUE;
  }
  if (mtype == MTYPE_F8 && primType == PTY_f64) {
    return TRUE;
  }
  if (IsPrimitiveFloat(primType)) {
    return FALSE;
  }
  return TRUE;
}

MIRAggConst *WHIRL2mpl::INITV_IDX2mpl4union(INITV_IDX idx, MIRStructType *unionType) {
  INITV &initv0 = Initv_Table[idx];
  MIRType *matchedMirType = NULL;
  UINT fieldIdx;
  switch (INITV_kind(initv0)) {
  case INITVKIND_VAL: {
    TCON &c = Tcon_Table[INITV_tc(initv0)];
    if (TCON_ty(c) != MTYPE_STRING) {
      for (fieldIdx = 0; fieldIdx < unionType->fields.size(); fieldIdx++) {
        TyIdx fTyIdx = unionType->fields[fieldIdx].second.first;
        MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
        if (fieldType->typeKind == kTypeScalar &&
            ScalarTypesMatched(TCON_ty(c), fieldType->primType)) {
          matchedMirType = fieldType;
          break;
        }
      }
    } else { // MTYPE_STRING
      for (fieldIdx = 0; fieldIdx < unionType->fields.size(); fieldIdx++) {
        TyIdx fTyIdx = unionType->fields[fieldIdx].second.first;
        MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
        if (fieldType->typeKind == kTypePointer) {
          matchedMirType = fieldType;
          break;
        }
        if (fieldType->typeKind == kTypeArray &&
            IsPrimitiveInteger(static_cast<MIRArrayType *>(fieldType)->GetElemType()->primType)) {
          matchedMirType = fieldType;
          break;
        }
      }
    }
    break;
  }
  case INITVKIND_ZERO:
  case INITVKIND_ONE: {
    for (fieldIdx = 0; fieldIdx < unionType->fields.size(); fieldIdx++) {
      TyIdx fTyIdx = unionType->fields[fieldIdx].second.first;
      MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
      if (fieldType->typeKind == kTypeScalar) {
        matchedMirType = fieldType;
        break;
      }
    }
    break;
  }
  case INITVKIND_SYMOFF: {
    for (fieldIdx = 0; fieldIdx < unionType->fields.size(); fieldIdx++) {
      TyIdx fTyIdx = unionType->fields[fieldIdx].second.first;
      MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
      if (fieldType->typeKind == kTypePointer) {
        matchedMirType = fieldType;
        break;
      }
    }
    break;
  }
  case INITVKIND_BLOCK: {
    for (fieldIdx = 0; fieldIdx < unionType->fields.size(); fieldIdx++) {
      TyIdx fTyIdx = unionType->fields[fieldIdx].second.first;
      MIRType *fieldType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(fTyIdx);
      if (fieldType->typeKind == kTypeArray ||
          fieldType->typeKind == kTypeStruct ||
          fieldType->typeKind == kTypeUnion) {
        matchedMirType = fieldType;
        break;
      }
    }
    break;
  }
  default: FmtAssert(FALSE, ("INITV_IDX2mpl4union: unexpected INITV_kind"));
  }
  FmtAssert(matchedMirType, ("INITV_IDX2mpl4union: cannot find matching scalar type"));
  MIRConst *fmirConst = INITV_IDX2mpl(idx, matchedMirType);
  fmirConst->fieldID = fieldIdx+1;
  MIRAggConst *aggConst = mod->memPool->New<MIRAggConst>(mod, unionType);
  aggConst->constVec.push_back(fmirConst);
  return aggConst;
}

MIRConst *WHIRL2mpl::INITV_IDX2mpl(INITV_IDX idx, MIRType *mirtype) {
  INITV &initv = Initv_Table[idx];
  if (INITV_repeat1(initv) > 1) {
    FmtAssert(FALSE, ("INITV_IDX2mpl: repeat field in INITV NYI"));
  }
  switch (INITV_kind(initv)) {
  case INITVKIND_PAD:
    return NULL;
  case INITVKIND_ZERO:
    return mod->memPool->New<MIRIntConst>(0, mirtype ? mirtype : MTYPE2MIRType(INITV_mtype(initv)));
  case INITVKIND_ONE:
    return mod->memPool->New<MIRIntConst>(1, mirtype ? mirtype : MTYPE2MIRType(INITV_mtype(initv)));
  case INITVKIND_VAL:
    return TCON_IDX2mpl(INITV_tc(initv), mirtype);
  case INITVKIND_LABEL: {
    MIRLblConst *labconst = mod->memPool->New<MIRLblConst>(LABEL_NUMBER2mpl(INITV_lab(initv)), fn->puIdx,
            GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr)));
    return labconst;
  }
  case INITVKIND_SYMOFF: {
    FmtAssert(ST_CLASS(INITV_st(initv)) != CLASS_CONST, ("INITV_IDX2mpl: SYMOFF symbol cannot be CLASS_CONST"));
    ST *st = &St_Table[INITV_st(initv)];
    if (st->sym_class == CLASS_CONST) {
      MIRConst *mirconst = TCON_IDX2mpl(st->u1.tcon, mirtype);
      FmtAssert(mirconst->kind == kConstStrConst, ("INITV_IDX2mpl: CLASS_CONST symbol in INITV refers to other than string constant"));
      if (INITV_ofst(initv) != 0) {
	std::string origString = GlobalTables::GetUStrTable().GetStringFromStrIdx(static_cast<MIRStrConst *>(mirconst)->value);
	std::string newString = origString.substr(INITV_ofst(initv));
	UStrIdx uStrIdx = GlobalTables::GetUStrTable().GetOrCreateStrIdxFromName(newString);
	return mod->memPool->New<MIRStrConst>(uStrIdx, mirconst->type);
      }
      return mirconst;
    } else {
      MIRSymbol *sym = ST_IDX2MIRSym(INITV_st(initv));
      if (sym->sKind == kStFunc) {
        FmtAssert(INITV_ofst(initv) == 0, ("INITV_IDX2mpl: SYMOFF with non-zero offset for function symbol"));
        MIRAddroffuncConst *aofconst = mod->memPool->New<MIRAddroffuncConst>(
            sym->GetFunction()->puIdx,
            GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr)));
        return aofconst;
      } else {
        MIRAddrofConst *stconst = mod->memPool->New<MIRAddrofConst>(sym->stIdx, 0, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr)), INITV_ofst(initv));
        return stconst;
      }
    }
  }
  case INITVKIND_BLOCK: {
    INITV_IDX idx = INITV_blk(initv);
    if (mirtype->typeKind == kTypeArray) {
      MIRAggConst *aggconst = mod->memPool->New<MIRAggConst>(mod, mirtype);
      MIRArrayType *arrayType = static_cast<MIRArrayType *>(mirtype);
      MIRType *etype = arrayType->GetElemType();
      while (idx) {
	INITV &initv0 = Initv_Table[idx];
        if (INITV_kind(initv0) != INITVKIND_PAD) {
          aggconst->constVec.push_back(INITV_IDX2mpl(idx, etype));
        } else if (etype->typeKind != kTypeStruct &&
                   etype->typeKind != kTypeUnion &&
                   etype->typeKind != kTypeArray) {
          UINT numPadElems = initv0.Pad() / etype->GetSize();
          for (UINT i = 0; i < numPadElems; i++) {
            MIRIntConst *intConst = mod->memPool->New<MIRIntConst>(0, etype);
            aggconst->constVec.push_back(intConst);
          }
        }
	idx = INITV_next(initv0);
      }
      return aggconst;
    } else {
      FmtAssert(mirtype->typeKind == kTypeStruct || mirtype->typeKind == kTypeUnion,
                ("INITV_IDX2mpl: illegal type kind for INITVKIND_BLOCK"));
      MIRStructType *structType = static_cast<MIRStructType *>(mirtype);
      if (structType->typeKind != kTypeUnion) {
        return INITV_IDX2mpl4struct(idx, structType);
      } else {
        return INITV_IDX2mpl4union(idx, structType);
      }
    }
  }
  default:
    FmtAssert(FALSE, ("INITV_IDX2mpl: NYI"));
  }
  return NULL;
}

struct INITO2mpl {
  WHIRL2mpl *whirl2mpl;
  INITO2mpl(WHIRL2mpl *w) : whirl2mpl(w) {}

  void operator() (UINT idx, INITO *inito) const;
};

void INITO2mpl::operator() (UINT idx, INITO *inito) const {
  MIRSymbol *sym = whirl2mpl->ST_IDX2MIRSym(inito->st_idx);
  if (sym == NULL) {
    return;
  }
  MIRType *symType = GlobalTables::GetTypeTable().GetTypeFromTyIdx(sym->tyIdx);
  MIRConst *konst = NULL;
  if (sym->storageClass != kScEHRegionSupp) {
    konst = whirl2mpl->INITV_IDX2mpl(inito->val, symType);
  } else {  // special processing for kScEHRegionSupp symbols
    INITV &initv = Initv_Table[inito->val];
    if (symType->typeKind == kTypeArray) {
      MIRArrayType *arrayType = static_cast<MIRArrayType *>(symType);
      if (INITV_kind(initv) == INITVKIND_BLOCK) {
        if (sym->GetName() != "__TYPEINFO_TABLE__" ) {
          return;
#if 0
          konst = whirl2mpl->INITV_IDX2mpl(inito->val, symType);
#endif
        } else {
          INITV_IDX idx = INITV_blk(initv);
          // the typeinfo symbol
          INITV &initv0 = Initv_Table[idx];
          if (INITV_kind(initv0) == INITVKIND_ZERO) {
            return;
          }
          FmtAssert(INITV_kind(initv0) == INITVKIND_VAL, ("INITO2mpl: unexpected init value for __TYPEINFO_TABLE__"));
          TCON_IDX tc = INITV_tc(initv0);
          TCON &c = Tcon_Table[tc];
          MIRAggConst *aggconst = whirl2mpl->mod->memPool->New<MIRAggConst>(whirl2mpl->mod, arrayType->GetElemType());
          ST_IDX st_idx = c.vals.uval.u0;
          if (st_idx != 0) {
            MIRSymbol *sym = whirl2mpl->ST_IDX2MIRSym(st_idx);
            MIRAddrofConst *stconst = whirl2mpl->mod->memPool->New<MIRAddrofConst>(sym->stIdx, 0, GlobalTables::GetTypeTable().GetPtr(), 0/*fieldID*/);
            aggconst->constVec.push_back(stconst);
          } else {
            MIRIntConst *zeroconst = whirl2mpl->mod->memPool->New<MIRIntConst>(0, GlobalTables::GetTypeTable().GetUInt32(), 0/*fieldID*/);
            aggconst->constVec.push_back(zeroconst);
          }
          idx = INITV_next(initv0);
          // the filter
          initv0 = Initv_Table[idx];
          FmtAssert(INITV_kind(initv0) == INITVKIND_VAL, ("INITO2mpl: unexpected init value for __TYPEINFO_TABLE__"));
          tc = INITV_tc(initv0);
          c = Tcon_Table[tc];
          MIRIntConst *filterconst = whirl2mpl->mod->memPool->New<MIRIntConst>(c.vals.uval.u0, GlobalTables::GetTypeTable().GetUInt32(), 1/*fieldID*/);
          aggconst->constVec.push_back(filterconst);
          FmtAssert(INITV_next(initv0) == 0, ("INITO2mpl: unexpected init value for __TYPEINFO_TABLE__"));

          MIRAggConst *arrayconst = whirl2mpl->mod->memPool->New<MIRAggConst>(whirl2mpl->mod, arrayType);
          arrayconst->constVec.push_back(aggconst);
          konst = arrayconst;
        }
      } else {
        return;
#if 0
        INITV_IDX idx = inito->val;
        MIRAggConst *aggconst = whirl2mpl->mod->memPool->New<MIRAggConst>(whirl2mpl->mod, arrayType);
        while (idx) {
          INITV &initv0 = Initv_Table[idx];
          FmtAssert(INITV_kind(initv0) == INITVKIND_VAL, ("INITO2mpl: unexpected initv kind for EHRegionSupp array"));
          TCON &c = Tcon_Table[INITV_tc(initv0)];
          if (c.vals.ival.v0 == 0) {
            aggconst->constVec.push_back(whirl2mpl->TCON_IDX2mpl(INITV_tc(initv0), NULL));
          } else {
            MIRLblConst *labconst = whirl2mpl->mod->memPool->New<MIRLblConst>(whirl2mpl->LABEL_NUMBER2mpl(c.vals.uval.u0),
                whirl2mpl->fn->puIdx, GlobalTables::GetTypeTable().GetTypeFromTyIdx(TyIdx(PTY_ptr)));
            aggconst->constVec.push_back(labconst);
          }
          idx = INITV_next(initv0);
        }
        konst = aggconst;
#endif
      }
    } else {
      if (INITV_kind(initv) != INITVKIND_BLOCK) {
        konst = whirl2mpl->INITV_IDX2mpl(inito->val, symType);
      } else {
        return;
#if 0
        INITV_IDX idx = INITV_blk(initv);
        MIRAggConst *aggconst = whirl2mpl->mod->memPool->New<MIRAggConst>(whirl2mpl->mod, symType);
        while (idx) {
          INITV &initv0 = Initv_Table[idx];
          aggconst->constVec.push_back(whirl2mpl->INITV_IDX2mpl(idx, NULL));
          idx = INITV_next(initv0);
        }
        konst = aggconst;
#endif
      }
    }
  }
  sym->value.konst = konst;
}

struct ST_ATTR2mpl {
  WHIRL2mpl *whirl2mpl;
  ST_ATTR2mpl(WHIRL2mpl *w) : whirl2mpl(w) {}

  void operator() (UINT idx, ST_ATTR *st_attr) const;
};

void ST_ATTR2mpl::operator() (UINT idx, ST_ATTR *st_attr) const {
  if (st_attr->kind == ST_ATTR_SECTION_NAME) {
    std::string section_name(&Str_Table[st_attr->Get_section_name()]);
    if (section_name == ".ctors") {
      MIRSymbol *sym = whirl2mpl->ST_IDX2MIRSym(st_attr->st_idx);
      MIRConst *mirConst = sym->GetConst();
      if (mirConst->kind == kConstAddrofFunc) {
        MIRAddroffuncConst *addroffuncConst = static_cast<MIRAddroffuncConst *>(mirConst);
        MIRFunction *fn = GlobalTables::GetFunctionTable().GetFunctionFromPuidx(addroffuncConst->GetValue());
        fn->SetAttr(FUNCATTR_constructor);
      }
    }
  }
}

void WHIRL2mpl::FUNC_ENTRY2mpl(WN *wn) {
  FmtAssert(WN_opcode(wn) == OPC_FUNC_ENTRY, ("FUNC_ENTRY2mpl: node is not FUNC_ENTRY but is %s", OPCODE_name(WN_opcode(wn))));
  retTempCounter = 0;
  ST_IDX func_st_idx = WN_st_idx(wn);
  ST *func_st = &St_Table[func_st_idx];
  if (Pu_Table[func_st->u2.pu].src_lang & PU_CXX_LANG) {
    mod->srcLang = kSrcLangCPlusPlus;
  } else if (Pu_Table[func_st->u2.pu].src_lang & PU_C_LANG) {
    mod->srcLang = kSrcLangC;
  }
  std::string funcName(&Str_Table[func_st->u1.name_idx]);
  printf("%s\n", funcName.c_str());
  GStrIdx funcStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(funcName);
  MIRSymbol *funcSt = GlobalTables::GetGsymTable().GetSymbolFromStrIdx(funcStrIdx);
  FmtAssert(funcSt != NULL, ("FUNC_ENTRY2mpl: funcSt is NULL"));
  fn = funcSt->value.mirFunc;
  mod->SetCurFunction(fn);
  fn->symTab = fn->dataMemPool->New<MIRSymbolTable>(&fn->dataMPAllocator);
  INT argCount = 0;
  WN *idname_wn = WN_kid(wn, argCount);
  while (WN_opcode(idname_wn) == OPC_IDNAME) {
    ST_IDX arg_st_idx = WN_st_idx(idname_wn);
    ST *arg_st = &St_Table[arg_st_idx];
    std::string argName(&Str_Table[arg_st->u1.name_idx]);
    if (argName[0] == '.') {
      argName[0] = '_';
    }
    GStrIdx argStrIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(argName);
    if (fn->formalDefVec.size() == argCount) {
      TY_IDX arg_ty_idx = arg_st->u2.type;
      FormalDef fd(NULL, TY_IDX2TyIdx(arg_ty_idx), TypeAttrs());
      fn->formalDefVec.push_back(fd);
    }
    fn->formalDefVec[argCount].formalStrIdx = argStrIdx;
    MIRSymbol *formalSym = fn->symTab->CreateSymbol(kScopeLocal);
    formalSym->SetStorageClass(kScFormal);
    formalSym->sKind = kStVar;
    formalSym->nameStrIdx = argStrIdx;
    fn->symTab->AddToStringSymbolMap(formalSym);
    fn->formalDefVec[argCount].formalSym = formalSym;
    if (mod->srcLang == kSrcLangCPlusPlus) {
      TY_IDX arg_ty_idx = arg_st->u2.type;
      fn->formalDefVec[argCount].formalTyIdx = TY_IDX2TyIdx(arg_ty_idx);
    }
    formalSym->tyIdx = fn->formalDefVec[argCount].formalTyIdx;
    argCount++;
    idname_wn = WN_kid(wn,argCount);
  }
  WN *block_wn = WN_kid(wn, WN_kid_count(wn)-1);
  if (block_wn == NULL) {
    return;
  }
  fn->pregTab = fn->dataMemPool->New<MIRPregTable>(&fn->dataMPAllocator);
  SYMTAB_IDX level = PU_lexical_level(func_st);
  // make sure maple's new preg numbers do not clash with those from whirl
  fn->pregTab->maxPregNo = Scope_tab[level].preg_tab->Size() + Last_Dedicated_Preg_Offset;
  fn->typeNameTab = fn->dataMemPool->New<MIRTypeNameTable>(&fn->dataMPAllocator);
  fn->labelTab = fn->dataMemPool->New<MIRLabelTable>(&fn->dataMPAllocator);

  USRCPOS srcpos;
  USRCPOS_srcpos(srcpos) = WN_Get_Linenum(block_wn);
  fn->srcPosition.SetRawData(srcpos.fillers[0]);
  fn->srcPosition.SetLinenum(srcpos.fillers[1]);

  // translate local symbol table
  duplicatedLocalSymIndices.clear();
  For_all_entries(*Scope_tab[level].st_tab, ST2mpl(this), 1);

  // translate the initializations
  For_all_entries(*Scope_tab[level].inito_tab, INITO2mpl(this), 1);

  fn->body = BLOCK2mpl(block_wn);

  mod->functionList.push_back(fn);
}

/* Binary to ASCII conversion */
static void
ir_b2a_process_PUs (PU_Info *pu_tree, WHIRL2mpl *whirl2mpl)
{
  PU_Info *pu;
  WN *wn;

  for (pu = pu_tree; pu != NULL; pu = PU_Info_next (pu)) {
    if (pu != pu_tree) {  // first pu already read to determine the language
      Read_Local_Info (MEM_pu_nz_pool_ptr, pu);
    }

    wn = PU_Info_tree_ptr(pu);

    whirl2mpl->FUNC_ENTRY2mpl(wn);

    // IR_put_func (wn, NULL);

#if 0 //code below should be uncommented
    if (PU_Info_child(pu)) {
      ir_b2a_process_PUs(PU_Info_child(pu), whirl2mpl);
    }

    SYMTAB_IDX level = PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
    Print_local_symtab (stdout, Scope_tab[level]);
#endif
  }
}

static void
ir_b2a (char *global_file, char *input_file) {
    PU_Info *pu_tree;

    if (global_file == NULL) {
	(void)Open_Input_Info (input_file);
    } else {
        (void)Open_Global_Input (global_file);
        (void)Open_Local_Input (input_file);
    }

    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
    pu_tree = Read_Global_Info (NULL);

    IR_reader_init();
    IR_Dwarf_Gen_File_Table(FALSE);  // this will set srcFileTable

    MIRModule theModule(input_file);
    // set the language based on the first PU
    if (pu_tree != NULL) {
      PU_Info *pu = pu_tree;
      Read_Local_Info (MEM_pu_nz_pool_ptr, pu);
      WN *wn = PU_Info_tree_ptr(pu);
      ST_IDX func_st_idx = WN_st_idx(wn);
      ST *func_st = &St_Table[func_st_idx];
      if (Pu_Table[func_st->u2.pu].src_lang & PU_CXX_LANG) {
        theModule.srcLang = kSrcLangCPlusPlus;
      } else if (Pu_Table[func_st->u2.pu].src_lang & PU_C_LANG) {
        theModule.srcLang = kSrcLangC;
      }
    }

    for (INT idx = 0; idx < srcFileTable.size(); idx++) {
      GStrIdx strIdx = GlobalTables::GetStrTable().GetOrCreateStrIdxFromName(srcFileTable[idx]);
      theModule.srcFileInfo.push_back(MIRInfoPair(strIdx, idx+1));
    }

    MIRBuilder mirBuilder(&theModule);
    WHIRL2mpl whirl2mpl(&mirBuilder);

    // translate all the types
    For_all (Ty_Table, TY2mpl(&whirl2mpl));
    if (whirl2mpl.hasForwardTypeReference) {
      For_all (Ty_Table, FixForwardTypeRef(&whirl2mpl));
    }

    // translate global symbols
    For_all (St_Table, GLOBAL_SYMTAB, ST2mpl(&whirl2mpl));

    // translate the function bodies
    ir_b2a_process_PUs(pu_tree, &whirl2mpl);

    // translate the initializations
    For_all (Inito_Table, GLOBAL_SYMTAB, INITO2mpl(&whirl2mpl));

    // process the global ST_ATTR_TAB
    For_all (St_Attr_Table, GLOBAL_SYMTAB, ST_ATTR2mpl(&whirl2mpl));

    /* print the symbol tables */
//  Print_global_symtab (stdout);

    Free_Input_Info ();

    // output maple file
    theModule.flavor = kFeProduced;
    theModule.numFuncs = theModule.functionList.size();
    if (!use_binary) {
      theModule.OutputAsciiMpl("", ".mpl");
    } else {
      BinaryMplt binMplt(theModule);
      std::string modname = theModule.fileName;
      std::string::size_type lastdot = modname.find_last_of(".");
      std::string filestem = modname.substr(0, lastdot);
      binMplt.GetBinExport().not2mplt = TRUE;
      binMplt.Export(filestem + ".bpl");
    }
} // ir_b2a

extern BOOL
file_exists (char *path)
{
  INT st;
  struct stat sbuf;
  st = stat(path, &sbuf);
  if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
    return FALSE;
  else
    return TRUE;
}

static void
usage (char *progname)
{
  fprintf (stderr, "Usage: %s [-a] <Whirl IR>\n", progname);
  exit (1);
}

int main (INT argc, char *argv[])
{
    register char *progname;
    register INT a2b, b2a, sel, all;
    char *infile;

    MEM_Initialize();
    Set_Error_Tables (Phases, host_errlist);
    Init_Error_Handler (10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    WHIRL_Mldid_Mstid_On = TRUE;
    Pointer_Size = 8;  // this is because Configure_Target() was not called

    progname = basename (argv[0]);
    // weird linux bug with basename where it doesn't strip the leading /
    if (*progname == '/') ++progname;

    if (strcmp (progname, "whirl2mpl") != 0) {
	fprintf(stderr, "unrecognized command %s\n", progname);
        exit(0);
    }
    Read_Global_Data = NULL;
    if (argc < 2)
      usage(progname);
    INT binarg = 1;
    if (*argv[binarg] == '-') {
      if (strncmp(argv[binarg], "-a", 2) == 0) {
        use_binary = FALSE;
      } else {
        usage(progname);
      }
      binarg++;
    }
    if (binarg == argc)
        usage(progname);
    if (!file_exists(argv[binarg]))
        usage(progname);
    infile = argv[binarg];
    ir_b2a (Read_Global_Data, infile);

    exit (0);
} /* main */


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
