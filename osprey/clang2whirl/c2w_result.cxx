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

//
// Created by lishijie on 4/15/19.
//

#include <llvm/Support/raw_ostream.h>
#include "open64inc.h"
#include "c2w_result.h"
#include "c2w_utils.h"

namespace wgen {

WN*
Result::Generate_Ilda(WN *addr, INT64 ofst, TY_IDX ty) {
  Is_True(WN_operator(addr) == OPR_LDID ||
          WN_operator(addr) == OPR_ILOAD,
          ("wrong candidate for ilda"));
  Is_True(TY_kind(ty) == KIND_POINTER &&
          TY_kind(TY_pointed(ty)) == KIND_STRUCT &&
          FieldId() < FLD_get_count(TY_pointed(ty)),
          ("wrong type"));
  WN *ilda = WN_CreateExp1(OPR_ILDA, Pointer_Mtype, MTYPE_V, addr);
  WN_load_offset(ilda) = ofst;
  if (WN_has_sym(addr))
    WN_st_idx(ilda) = WN_st_idx(addr);
  WN_set_ty(ilda, Ty());
  WN_set_field_id(ilda, FieldId());
  return ilda;
}

//
// TY_IDX Result::FieldTy() const
// Get field ty if ty is struct type and field id is not 0
TY_IDX
Result::FieldTy() const {
  TY_IDX ty = Ty();
  if (FieldId() == 0) {
    return ty;
  }
  if (IsRef()) {
    Is_True(TY_kind(ty) == KIND_POINTER, ("not pointer type"));
    ty = TY_pointed(ty);
  }
  Is_True(TY_kind(ty) == KIND_STRUCT, ("not struct type"));
  UINT64 ofst = 0;
  IDTYPE fld = 0;
  FLD_HANDLE fh = get_fld_and_offset(ty, FieldId(), fld, ofst);
  Is_True(!fh.Is_Null(), ("not find the field"));
  return FLD_type(fh);
}

//
// WN* Result::GetLValue
// Get WHIRL node for Result as LValue
WN*
Result::GetLValue() {
  if (isNode()) {
    WN *wn = Node();
    // copy node is necessary
    if (IsWnUsed())
      wn = WN_COPY_Tree(wn);
    else
      SetWnUsed();

    if (FieldId()) {
      TY_IDX ty = Ty();
      Is_True(TY_kind(ty) == KIND_POINTER, ("not ptr type"));
      TY_IDX sty = TY_pointed(ty);
      Is_True(TY_kind(sty) == KIND_STRUCT, ("not struct type"));
      UINT64 ofst = 0;
      IDTYPE fld = 0;
      FLD_HANDLE fh = get_fld_and_offset(ty, FieldId(), fld, ofst);
      Is_True(!fh.Is_Null(), ("not find the field"));
      if (IsDot() && WN_operator(wn) == OPR_ILOAD) {
        return WN_Iload(WN_rtype(wn),
                        WN_offset(wn) + ofst, sty, WN_kid0(wn),
                        WN_field_id(wn) + FieldId());
      } else if (WN_operator(wn) == OPR_LDID || WN_operator(wn) == OPR_ILOAD)
        return Generate_Ilda(wn, ofst, ty);
      return WN_Add(Pointer_Mtype, wn, WN_Intconst(Pointer_Mtype, ofst));
    }
    return wn;
  }
  else if (isSym()) {
    Is_True(!IsArrow(), ("TODO"));
    ST* st = ST_ptr(Sym());
    UINT64 ofst = 0;
    TY_IDX ty = ST_type(st);
    if (FieldId()) {
      IDTYPE fld = 0;
      FLD_HANDLE fh = get_fld_and_offset(ty, FieldId(), fld, ofst);
      Is_True(!fh.Is_Null(), ("not find the field"));
    }
    if (IsRef() || IsDeref()) {
      Is_True(!IsAddrOf(), ("Addr of Ref or Deref"));
      Is_True(TY_kind(Ty()) == KIND_POINTER, ("not pointer"));
      if (IsConstSym() ||
          TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_FUNCTION) {
        Set_ST_addr_saved(st);
        return WN_Lda(Pointer_Mtype, ofst, st, FieldId());
      }
      Is_True(TY_kind(ty) == KIND_POINTER, ("not pointer"));
      if (FieldId()) {
        WN* ldid = WN_Ldid(Pointer_Mtype, 0, st, ty, 0);
        return Generate_Ilda(ldid, ofst, ty);
      }
      return WN_Ldid(Pointer_Mtype, ofst, st, ty, FieldId());
    }
    Set_ST_addr_saved(st);
    return WN_Lda(Pointer_Mtype, ofst, st, FieldId());
  }
  Is_True(FALSE, ("TODO"));
  return NULL;
}

//
// WN* Result::GetRValue
// Get WHIRL node for Result as RValue
// Array/arrow/deref not handled here. They are handled in ConvertToRValue
WN*
Result::GetRValue() {
  if (isNode()) {
    WN* wn = Node();
    // copy node is necessary
    if (IsWnUsed())
      wn = WN_COPY_Tree(wn);
    else
      SetWnUsed();
    return wn;
  }
  else if (isSym()) {
    ST* st = ST_ptr(Sym());
    UINT64 ofst = 0;
    TY_IDX ty = ST_type(st);
    TY_IDX sty = ty;
    if (IsRef() && TY_kind(ty) != KIND_ARRAY) {
      Is_True(TY_kind(ty) == KIND_POINTER, ("not pointer type"));
      sty = TY_pointed(ty);
    }
    TYPE_ID mty = TY_mtype(sty);
    TYPE_ID desc = mty;
    TYPE_ID rtype = Mtype_comparison(desc);
    if (FieldId()) {
      if (TY_kind(sty) == KIND_ARRAY)
        sty = TY_etype(sty);
      IDTYPE fld = 0;
      FLD_HANDLE fh = get_fld_and_offset(sty, FieldId(), fld, ofst);
      Is_True(!fh.Is_Null(), ("not find the field"));
      if (TY_kind(FLD_type(fh)) == KIND_ARRAY) {
        Set_ST_addr_saved(st);
        return WN_Lda(Pointer_Mtype, ofst, st, FieldId());
      }
      mty = desc = TY_mtype(FLD_type(fh));
      rtype = Mtype_comparison(desc);
      if (FLD_is_bit_field(fh)) {
        desc = MTYPE_BS;
        rtype = Mtype_comparison(TY_mtype(FLD_type(fh)));
        ofst = 0;
      }
    }
    if (IsRef() && TY_kind(ty) == KIND_ARRAY) {
      ARB_HANDLE arb = TY_arb(ty);
      if (ARB_ubnd_var(arb) && !ARB_const_ubnd(arb))
        ty = Make_Pointer_Type(ty);
    }
    if (IsAddrOf() || IsConstSym() ||
        TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_FUNCTION) {
      Set_ST_addr_saved(st);
      return WN_Lda(Pointer_Mtype, ofst, st, FieldId());
    }
    else if (IsRef()) {
      Is_True(TY_kind(ty) == KIND_POINTER, ("not pointer type"));
      WN *addr = WN_Ldid(Pointer_Mtype, 0, st, ty, 0);
      if (TY_kind(sty) == KIND_ARRAY || TY_kind(sty) == KIND_FUNCTION)
        return addr;
      return WN_Iload(mty, ofst, TY_pointed(ty), addr, FieldId());
    }
    if (TY_kind(ty) == KIND_POINTER && FieldId() > 0) {
      Is_True(TY_kind(TY_pointed(ty)) == KIND_STRUCT,
              ("not struct type"));
      WN *ldid = WN_Ldid(Pointer_Mtype, 0, st, ty, 0);
      return Generate_Ilda(ldid, ofst, ty);
    }
    return WN_CreateLdid(OPR_LDID, rtype, desc,
                         ofst, st, ty, FieldId());
  }
  else if (isIntConst()) {
    TYPE_ID mty = Mtype_comparison(TY_mtype(Ty()));
    return WN_Intconst(mty, IntConst());
  }
  else if (isTCon()) {
    ST *st = New_Const_Sym(Tcon(), Ty());
    return WN_CreateConst(OPR_CONST, TY_mtype(Ty()), MTYPE_V, st);
  }
  Is_True(FALSE, ("TODO"));
}

//
// WN* Result::GetRValue
// Convert Result into a RValue
// Handle array/arrow/deref in this function
Result
Result::ConvertToRValue(TY_IDX rty) {
  WN* ret = NULL;
  if (isNode()) {
    if (!IsRValue() ||
        IsArray() || IsArrow() || IsDeref() || IsRef()) {
      TY_IDX pty = Ty();
      TY_IDX lty = TY_kind(pty) == KIND_POINTER ? TY_pointed(pty) : pty;
      UINT64 ofst = 0;
      TYPE_ID desc = TY_mtype(rty);
      TYPE_ID rtype = Mtype_comparison(desc);
      if (FieldId()) {
        IDTYPE fld = 0;
        Is_True(TY_kind(lty) == KIND_STRUCT, ("not struct type"));
        FLD_HANDLE fh = get_fld_and_offset(lty, FieldId(), fld, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        if (FLD_is_bit_field(fh)) {
          desc = MTYPE_BS;
          rtype = Mtype_comparison(TY_mtype(FLD_type(fh)));
          ofst = 0;
        }
      }
      WN *wn = Node();
      // copy node is necessary
      if (IsWnUsed())
        wn = WN_COPY_Tree(wn);
      else
        SetWnUsed();
      // generate iload
      ret = WN_CreateIload(OPR_ILOAD, rtype, desc,
                           ofst, lty, Make_Pointer_Type(lty, FALSE),
                           wn, FieldId());
      Result r = Result::nwNode(ret, rty);
      r.SetRValue();
      return r;
    }
    Is_True(IsRValue(), ("wrong node?"));
    return *this;
  }
  else if (isSym()) {
    ST_IDX st = Sym();
    TY_IDX ty = ST_type(st);
    if (IsAddrOf())
      ret = NULL; //WN_Lda();
    else if (IsDeref()) {
      Is_True(FieldId() == 0, ("deref with field id?"));
      Is_True(TY_kind(Ty()) == KIND_POINTER, ("not ptr type"));
      if (IsConstSym() ||
          TY_kind(ty) == KIND_ARRAY || TY_kind(ty) == KIND_FUNCTION) {
        ret = WN_Ldid(TY_mtype(rty), 0, st, Ty(), 0);
      }
      else {
        Is_True(TY_kind(ty) == KIND_POINTER, ("not ptr type"));
        WN *addr = WN_Ldid(Pointer_Mtype, 0, st, Ty(), 0);
        ret = WN_Iload(TY_mtype(rty), 0, Ty(), addr, 0);
      }
    }
    else if (IsRef()) {
      Is_True(TY_kind(Ty()) == KIND_POINTER, ("not ptr type"));
      TY_IDX lty = TY_pointed(Ty());
      UINT64 ofst = 0;
      TYPE_ID mtype = TY_mtype(rty);
      TYPE_ID rtype = Mtype_comparison(mtype);
      if (FieldId()) {
        IDTYPE fld = 0;
        Is_True(TY_kind(lty) == KIND_STRUCT, ("not struct type"));
        FLD_HANDLE fh = get_fld_and_offset(lty, FieldId(), fld, ofst);
        Is_True(!fh.Is_Null(), ("not find the field"));
        if (FLD_is_bit_field(fh)) {
          mtype = MTYPE_BS;
          ofst = 0;
        }
      }
      WN *addr = WN_Ldid(Pointer_Mtype, 0, st, Ty(), 0);
      ret = WN_CreateIload(OPR_ILOAD, rtype, mtype,
                           ofst, lty, Ty(),
                           addr, FieldId());
    }
    else if (IsArrow()) {
      ret = NULL; //WN_Lda();
    }
    else {
      ret = GetRValue();
    }
    Is_True(ret != NULL, ("wrong sym?"));
    Result r = Result::nwNode(ret, rty);
    r.SetRValue();
    return r;
  }
  else if (isIntConst()) {
    Is_True(FALSE, ("TODO"));
    return Result::nwNone();
  }
}


void Result::dump() {
  llvm::outs() << "kind : " << Kind() << "\n";
}

}
