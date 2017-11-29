/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//====================================================================
//
// Module: wssa_points_to.cxx
//
// Revision history:
//  Dec-10 - Original Version
//
// Description:
//  Points-to analysis for WHIRL SSA
//
// Exported classes:
//  WSSA::WHIRL_SSA_MANAGER
//
// SEE ALSO:
//  be/com/wssa_mgr.h
//  be/com/wssa_core.h (PHI_NODE, CHI_NODE, MU_NODE)
//  be/com/wssa_sym.h  (WST_Symbol_Entry, WST_Version_Entry)
//
// ====================================================================

#include <stdio.h>
#include <stdarg.h>
#include "wssa_utils.h"
#include "wssa_mgr.h"
#include "wssa_wn.h"
#include "wn.h"
#include "wn_util.h"
#include "wn_lower.h"
#include "symtab.h"
#include "opt_points_to.h"

namespace WSSA {

//====================================================================
//  Find_addr_base
//    find the address base of the wn
//  Analyze_addr_expr
//    analyze the address expression of ILOAD/ISTORE (flow-free)
//  Analyze_addr_for_memop
//    analyze the address for IlOAD/ISTORE
//  Create_wst_for_direct_memop:
//    find or create wst for LDID/LDBITS/STID/STBITS
//  Create_wst_for_indirect_memop:
//    find or create wst for ILOAD/ISTORE/etc
//  Create_wst_for_wn:
//    find or create wst for whirl node
//====================================================================
WN*
WHIRL_SSA_MANAGER::Find_addr_base(WN* tree) {
  if (tree == NULL)
    return NULL;
  switch (WN_operator(tree)) {
    case OPR_INTRINSIC_OP:
#ifdef TARG_SL
      FmtAssert(FALSE, ("TODO: remove targ macro"));
      if(INTRN_copy_addr(WN_intrinsic(tree)))
        return Find_addr_base(WN_kid0(WN_kid0(tree)));
#endif
      return NULL;
    case OPR_PARM:
      if ((WN_Parm_By_Reference(tree) || WN_Parm_Dereference(tree))) {
        Is_True(WN_kid_count(tree) == 1, ("no kid for PARM"));
        return Find_addr_base(WN_kid0(tree));
      }
      return NULL;
    case OPR_LDA:
      return tree;
    case OPR_LDID: {
      ST* st = WN_st(tree);
      if (Is_FORTRAN() && ST_sclass(st) == SCLASS_FORMAL &&
          !ST_is_value_parm(st))
        return tree;
      if (ST_pt_to_unique_mem(st)) {
        TY_IDX ty = WN_ty(tree);
        Is_True(TY_kind(Ty_Table[ty]) == KIND_POINTER, ("ty is not pointer, fix FE"));
        return tree;
      }
      TY_IDX ty = WN_ty(tree);
      if (TY_kind(Ty_Table[ty]) == KIND_POINTER)
        return tree;
      Is_True(!TY_is_restrict(ty), ("__restrict must be pointer"));
      return NULL;
    }
    case OPR_ARRAY:
      return Find_addr_base(WN_kid0(tree));
    case OPR_ADD: {
      WN* ret_wn;
      for (INT i = 0; i < WN_kid_count(tree); ++i) {
        ret_wn = Find_addr_base(WN_kid(tree, i));
        if (ret_wn != NULL)
          return ret_wn;
      }
      return NULL;
    }
    case OPR_SUB:
      return Find_addr_base(WN_kid(tree, 0));
    default:
      return NULL;
  }
}

void
WHIRL_SSA_MANAGER::Analyze_range(WN* expr, POINTS_TO* pt) {
  Is_True(WN_operator(expr) == OPR_ARRAY, ("expr is not ARRAY"));
  Is_True(pt->Bit_Size() == 0, ("ARRAY do not address bit field"));
  if (pt->Ofst_kind() != OFST_IS_FIXED || !pt->Is_pointer())
    return;
  INT64 elem_size = WN_element_size(expr);
  if (elem_size < 0) {
    // non-contiguous array
    pt->Set_byte_ofst(0);
    pt->Set_byte_size(0);
    pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    return;
  }
  INT64 upper = WN_element_size(expr);
  INT64 lower = 0;
  INT32 dim = WN_num_dim(expr);
  for (INT32 i = dim; i > 0; --i) {
    WN* dim_wn = WN_kid(expr, i);
    WN* idx_wn = WN_kid(expr, i + dim);
    if (WN_operator(dim_wn) != OPR_INTCONST ||
        WN_const_val(dim_wn) == 1 ||
        WN_const_val(dim_wn) == 0) {
      // upper is unknown
      pt->Set_byte_ofst(0);
      pt->Set_byte_size(0);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
      return;
    }
    if (WN_operator(idx_wn) == OPR_INTCONST) {
      lower = elem_size * WN_const_val(idx_wn) + lower;
      upper = elem_size * WN_const_val(idx_wn) + upper;
    }
    else {
      // assume accessing the whole array
      lower = 0;
      upper = elem_size * WN_const_val(dim_wn) + upper;
    }
    elem_size *= WN_const_val(dim_wn);
  }

  upper -= WN_element_size(expr);
  Is_True(upper >= lower, ("bounds error"));
  pt->Set_byte_ofst(pt->Byte_Ofst() + lower);
  pt->Set_byte_size(upper - lower);
  pt->Set_ofst_kind(OFST_IS_FIXED);
  return;
}

void
WHIRL_SSA_MANAGER::Analyze_addr_arith(WN* expr, POINTS_TO* pt) {
  OPERATOR opr = WN_operator(expr);
  switch(opr) {
    case OPR_INTRINSIC_OP:
#ifdef TARG_SL
      FmtAssert(FALSE, ("TODO: unify targ macro"));
#endif
      break;
    case OPR_INTCONST:
      pt->Set_expr_kind(EXPR_IS_INT);
      pt->Set_const_val(WN_const_val(expr));
      pt->Invalidate_ptr_info();
      break;
    case OPR_LDBITS:
    case OPR_ILDBITS:
    case OPR_LDID:
    case OPR_ILOAD: {
      TY_IDX ty = WN_object_ty(expr);
      if (TY_kind(ty) == KIND_POINTER) {
        Analyze_addr_expr(expr, pt);
      }
      else {
        pt->Set_expr_kind(EXPR_IS_INT);
        pt->Invalidate_ptr_info();
      }
      break;
    }
    case OPR_LDA:
    case OPR_ARRAY:
      Analyze_addr_expr(expr, pt);
      break;
    case OPR_PAREN:
      Analyze_addr_arith(WN_kid0(expr), pt);
      break;
    case OPR_NEG: {
      POINTS_TO kid_pt;
      kid_pt.Init();
      Analyze_addr_arith(WN_kid0(expr), &kid_pt);
      if (kid_pt.Expr_kind() == EXPR_IS_INT) {
        pt->Copy_fully(kid_pt);
      }
      else {
        pt->Set_expr_kind(EXPR_IS_UNKNOWN);
        pt->Set_base_kind(BASE_IS_UNKNOWN);
        pt->Set_ofst_kind(OFST_IS_UNKNOWN);
      }
      pt->Invalidate_ptr_info();
      break;
    }
    case OPR_ADD: {
      POINTS_TO kid0_pt;
      POINTS_TO kid1_pt;
      kid0_pt.Init();
      kid0_pt.Set_expr_kind(EXPR_IS_UNKNOWN);
      kid1_pt.Init();
      kid1_pt.Set_expr_kind(EXPR_IS_UNKNOWN);
      Analyze_addr_arith(WN_kid0(expr), &kid0_pt);
      Analyze_addr_arith(WN_kid1(expr), &kid1_pt);

      pt->Set_expr_kind(EXPR_IS_UNKNOWN);
      if (kid0_pt.Expr_kind() == EXPR_IS_BEING_PROCESSED) {
        pt->Copy_fully(kid0_pt);
        pt->Invalidate_ptr_info();
      }
      else if (kid0_pt.Expr_kind() == EXPR_IS_ADDR) {
        if (kid1_pt.Expr_kind() == EXPR_IS_INT) {
          pt->Copy_fully(kid0_pt);
          if ((kid0_pt.Ofst_kind() == OFST_IS_FIXED || 
               kid0_pt.Iofst_kind() == OFST_IS_FIXED) && 
              kid1_pt.Int_is_constant()) {
            pt->Shift_ofst(kid1_pt.Int_const_val());
          }
          else if (!pt->Is_field()) {
            pt->Set_ofst_kind(OFST_IS_UNKNOWN);
            pt->Set_iofst_kind(OFST_IS_UNKNOWN);
          }
        }
        else if (kid1_pt.Expr_kind() == EXPR_IS_ADDR) {
          pt->Copy_fully(kid0_pt);
          pt->Meet(&kid1_pt, (ST*)expr);
        }
      }
      else if (kid0_pt.Expr_kind() == EXPR_IS_INT) {
        if (kid1_pt.Expr_kind() == EXPR_IS_ADDR) {
          pt->Copy_fully(kid1_pt);
          if ((kid1_pt.Ofst_kind() == OFST_IS_FIXED ||
               kid1_pt.Iofst_kind() == OFST_IS_FIXED) &&
               kid0_pt.Int_is_constant()) {
            pt->Shift_ofst(kid0_pt.Int_const_val());
          }
          else if (!pt->Is_field()) {
            pt->Set_ofst_kind(OFST_IS_UNKNOWN);
            pt->Set_iofst_kind(OFST_IS_UNKNOWN);
          }
        }
        else if (kid1_pt.Expr_kind() == EXPR_IS_INT) {
          pt->Set_expr_kind(EXPR_IS_INT);
          pt->Invalidate_ptr_info();
        }
      }

      if (pt->Expr_kind() == EXPR_IS_UNKNOWN) {
        pt->Set_base_kind(BASE_IS_UNKNOWN);
        pt->Set_ofst_kind(OFST_IS_UNKNOWN);
        pt->Invalidate_ptr_info();
      }
      break;
    }
    case OPR_SUB: {
      POINTS_TO kid0_pt;
      POINTS_TO kid1_pt;
      kid0_pt.Init();
      kid0_pt.Set_expr_kind(EXPR_IS_UNKNOWN);
      kid1_pt.Init();
      kid1_pt.Set_expr_kind(EXPR_IS_UNKNOWN);
      Analyze_addr_arith(WN_kid0(expr), &kid0_pt);
      Analyze_addr_arith(WN_kid1(expr), &kid1_pt);

      pt->Set_expr_kind(EXPR_IS_UNKNOWN);
      if (kid0_pt.Expr_kind() == EXPR_IS_BEING_PROCESSED) {
        pt->Copy_fully(kid0_pt);
        pt->Invalidate_ptr_info();
      }
      else if (kid0_pt.Expr_kind() == EXPR_IS_ADDR) {
        if (kid1_pt.Expr_kind() == EXPR_IS_INT) {
          pt->Copy_fully(kid0_pt);
          if ((kid0_pt.Ofst_kind() == OFST_IS_FIXED ||
               kid0_pt.Iofst_kind() == OFST_IS_FIXED) &&
              kid1_pt.Int_is_constant()) {
            pt->Shift_ofst(-kid1_pt.Int_const_val());
          }
          else {
            pt->Set_ofst_kind(OFST_IS_UNKNOWN);
            pt->Set_iofst_kind(OFST_IS_UNKNOWN);
          }
        }
        else if (kid1_pt.Expr_kind() == EXPR_IS_ADDR) {
          pt->Copy_fully(kid0_pt);
          pt->Meet(&kid1_pt, (ST*)expr);
        }
      }
      else if (kid0_pt.Expr_kind() == EXPR_IS_INT) {
        if (kid1_pt.Expr_kind() == EXPR_IS_INT) {
          pt->Set_expr_kind(EXPR_IS_INT);
        }
        pt->Invalidate_ptr_info();
      }

      if (pt->Expr_kind() == EXPR_IS_UNKNOWN) {
        pt->Set_base_kind(BASE_IS_UNKNOWN);
        pt->Set_ofst_kind(OFST_IS_UNKNOWN);
        pt->Invalidate_ptr_info();
      }
      break;
    }
    // other integer operations
    case OPR_ABS:
    case OPR_SQRT:
    case OPR_RSQRT:
    case OPR_RECIP:
    case OPR_REALPART:
    case OPR_IMAGPART:
    case OPR_RND:
    case OPR_TRUNC:
    case OPR_CEIL:
    case OPR_FLOOR:
    case OPR_BNOT:
    case OPR_LNOT:
    case OPR_COMPLEX:
    case OPR_MPY:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_EQ:
    case OPR_NE:
    case OPR_GE:
    case OPR_GT:
    case OPR_LE:
    case OPR_LT:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_LAND:
    case OPR_LIOR:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_MADD:
    case OPR_MSUB:
    case OPR_NMADD:
    case OPR_NMSUB:
    case OPR_CVT:
    case OPR_CVTL:
      pt->Set_expr_kind(EXPR_IS_INT);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
      pt->Invalidate_ptr_info ();
      break;
    // other unknown operations
    default:
      pt->Set_expr_kind(EXPR_IS_UNKNOWN);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
      pt->Invalidate_ptr_info();
      break;
  }
}

void
WHIRL_SSA_MANAGER::Analyze_lda_base(WN* expr, POINTS_TO* pt) {
  Is_True(WN_operator(expr) == OPR_LDA, ("expr is not LDA"));
  ST* st = WN_st(expr);
  pt->Analyze_ST(WN_st(expr), WN_offset(expr), 0, 0, 0, WN_ty(expr), TRUE);
  pt->Set_base(st);
  pt->Set_byte_ofst(WN_lda_offset(expr));
  pt->Set_byte_size(0);
  pt->Set_is_pointer();
  pt->Invalidate_ptr_info();
}

void
WHIRL_SSA_MANAGER::Analyze_ldid_base(WN* expr, POINTS_TO* pt) {
  Is_True(WN_operator(expr) == OPR_LDID, ("expr is not LDID"));
  pt->Set_expr_kind(EXPR_IS_UNKNOWN);
  pt->Set_base_kind(BASE_IS_UNKNOWN);
  pt->Set_ofst_kind(OFST_IS_UNKNOWN);
  TY_IDX ty = WN_ty(expr);
  if (ty == (TY_IDX)0 || TY_kind(ty) != KIND_POINTER)
    return;
  ST* st = WN_st(expr);
  if (ST_class(st) != CLASS_VAR)
    return;
  INT64 offset = WN_offset(expr);
  if (offset == 0) {
    if (TY_is_restrict(ST_type(st))) {
      pt->Set_restricted();
      pt->Set_based_sym(st);
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    } else if (WOPT_Enable_Unique_Pt_Vsym && ST_pt_to_unique_mem(st)) {
      pt->Set_unique_pt();
      pt->Set_based_sym(st);
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    } else if (Alias_Pointer_Cray && st != NULL && !ST_is_temp_var(st)) {
      pt->Set_unique_pt();
      pt->Set_based_sym(st);
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    }
    if (Alias_Pointer_Parms && Is_FORTRAN() &&
        ST_sclass(st) == SCLASS_FORMAL && !ST_is_value_parm(st)) {
      pt->Set_F_param();
      pt->Set_based_sym(st);
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_global();
      pt->Set_named();   // For the Ragnarok option
    }

    pt->Set_ofst_kind(OFST_IS_FIXED);
    pt->Set_byte_ofst(0);
    pt->Set_byte_size(0);

    if (pt->Based_sym () == NULL && WOPT_Enable_Pt_Keep_Track_Ptr) {
      pt->Set_pointer (st);
      pt->Set_pointer_ver ((VER_ID)0);
      pt->Set_iofst_kind (OFST_IS_FIXED);
    }
  }
  else {
    if (ST_pt_to_unique_mem(st)) {
      pt->Set_unique_pt();
      pt->Set_based_sym(st);
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    }
    pt->Set_ofst_kind(OFST_IS_FIXED);
    pt->Set_byte_ofst(0);
    pt->Set_byte_size(0);

    pt->Set_pointer ((ST*)NULL);
    pt->Set_pointer_ver ((VER_ID)0);
    pt->Set_iofst_kind (OFST_IS_INVALID);
  }
}

void
WHIRL_SSA_MANAGER::Analyze_addr_expr(WN* expr, POINTS_TO* pt) {
  OPERATOR opr = WN_operator(expr);
  switch (opr) {
    case OPR_ARRAY:
      Analyze_addr_expr(WN_kid0(expr), pt);
      Analyze_range(expr, pt);
      pt->Invalidate_ptr_info();
      break;
    case OPR_LDA:
      Analyze_lda_base(expr, pt);
      break;
    case OPR_LDBITS:
    case OPR_LDID:
      Analyze_ldid_base(expr, pt);
      break;
    case OPR_ADD:
    case OPR_SUB:
    case OPR_NEG:
      Analyze_addr_arith(expr, pt);
      break;
    case OPR_ILOAD:
    case OPR_ILDBITS:
      pt->Set_expr_kind(EXPR_IS_ADDR);
      pt->Set_base_kind(BASE_IS_DYNAMIC);
      pt->Set_ofst_kind(OFST_IS_FIXED);
      pt->Set_byte_ofst(0);
      pt->Set_byte_size(0);
      pt->Set_bit_ofst_size(0, 0);
      pt->Set_base(NULL);
      break;
#ifdef TARG_SL
    case OPR_INTRINSIC_OP:
      FmtAssert(FALSE, ("TODO: unify targ macro"));
#endif
    default:
      pt->Set_expr_kind(EXPR_IS_UNKNOWN);
      pt->Set_base_kind(BASE_IS_UNKNOWN);
      pt->Set_ofst_kind(OFST_IS_UNKNOWN);
      break;
  }
}

void
WHIRL_SSA_MANAGER::Analyze_addr_for_memop(WN* memop, POINTS_TO* pt) {
  Is_True(memop != NULL && pt != NULL, ("memop or pt is NULL"));
  pt->Set_expr_kind(EXPR_IS_ADDR);
  pt->Set_base_kind(BASE_IS_UNKNOWN);
  pt->Set_ofst_kind(OFST_IS_UNKNOWN);
  pt->Reset_attr();
  OPERATOR opr = WN_operator(memop);
  switch (opr) {
    case OPR_ILDBITS:
    case OPR_ILOAD:
    case OPR_MLOAD:
    case OPR_ILOADX:
      Analyze_addr_expr(WN_kid0(memop), pt);
      break;
    case OPR_ISTBITS:
    case OPR_ISTORE:
    case OPR_MSTORE:
    case OPR_ISTOREX:
      Analyze_addr_expr(WN_kid1(memop), pt);
      break;
    default:
      FmtAssert(FALSE, ("unexpected opr %s", OPCODE_name(WN_opcode(memop))));
  }
  pt->Shift_ofst(WN_offset(memop));
  pt->Set_ty(WN_object_ty(memop));
  TY_IDX hl_ty = 0;
  UINT32 fld_id = 0;
  WN_hl_object_ty (memop, hl_ty, fld_id);
  pt->Set_hl_ty (hl_ty);
  pt->Set_field_id (fld_id);
  //Collect_f90_pointer_info(pt, memop);
  Update_From_Restricted_Map(memop, pt);
}

UINT64
WHIRL_SSA_MANAGER::Desc_byte_size(WN* wn) {
  OPERATOR opr = WN_operator(wn);
  Is_True(opr == OPR_LDID   || opr == OPR_STID ||
          opr == OPR_ILOAD  || opr == OPR_ILDBITS ||
          opr == OPR_ISTORE || opr == OPR_ISTBITS,
          ("opr is not LDID or STID"));
  Is_True(WN_desc(wn) == MTYPE_M, ("desc not MTYPE_M"));
  TY_IDX ty_idx = WN_ty(wn);
  UINT field_id = WN_field_id(wn);
  if (field_id != 0) {
    Is_True(TY_kind(ty_idx) == KIND_STRUCT,
            ("ty is not KIND_STRUCT"));
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field(ty_idx, field_id, cur_field_id);
    Is_True(! fld.Is_Null(),
            ("can not find the field"));
    ty_idx = FLD_type(fld);
  }
  return TY_size(ty_idx);
}

WST_IDX
WHIRL_SSA_MANAGER::Create_wst_for_direct_memop(WN* wn) {
  OPERATOR opr = WN_operator(wn);
  Is_True(opr == OPR_LDID || opr == OPR_STID ||
          opr == OPR_LDBITS || opr == OPR_STBITS,
          ("invalid opr"));
  ST* st = WN_st(wn);
  if (ST_class(st) == CLASS_PREG) {
    PREG_NUM preg_num = WN_offset(wn);
    WST_IDX idx = Find_wst(st, preg_num);
    if (idx == WST_INVALID) {
      idx = New_wst(st, preg_num);
    }
    return idx;
  }
  else {
    INT field_id = 0;
    INT64 byte_offset = WN_offset(wn);
    INT64 byte_size = 0;
    INT8 bit_offset = 0;
    INT8 bit_size = 0;
    if (opr == OPR_LDBITS || opr == OPR_STBITS) {
      bit_offset = WN_bit_offset(wn);
      bit_size = WN_bit_size(wn);
    }
    else {
      field_id = WN_field_id(wn);
    }
    if (WN_desc(wn) == MTYPE_BS) {
      UINT bit_field_id = 0;
      UINT64 bit_field_offset = 0;
      FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(
                           WN_ty(wn), WN_field_id(wn),
                           bit_field_id, bit_field_offset);
      byte_offset += bit_field_offset;
      bit_size = FLD_bsize(fld);
      bit_offset = FLD_bofst(fld);
      byte_size = TY_size(FLD_type(fld));
    }
    else if (WN_desc(wn) == MTYPE_M) {
      byte_size = Desc_byte_size(wn);
    }
    else {
      byte_size = MTYPE_size_min(WN_desc(wn)) >> 3;
      if (ST_sclass(st) == SCLASS_REG)
        byte_size = TY_size(ST_type(st));
    }
    WST_IDX idx = Find_wst(st, byte_offset, byte_size,
                           bit_offset, bit_size);
    if (idx == WST_INVALID) {
      WST_Field_Info field_info(ST_st_idx(st), field_id, byte_size,
                                byte_offset, bit_size, bit_offset, 0);
      idx = New_wst(st, field_info);
    }
    return idx;
  }
}

WST_IDX
WHIRL_SSA_MANAGER::Create_wst_for_indirect_memop(WN* wn) {
  OPERATOR opr = WN_operator(wn);
  Is_True(opr == OPR_ILOAD  || opr == OPR_ILDBITS ||
          opr == OPR_ISTORE || opr == OPR_ISTBITS,
          ("invalid opr"));
  POINTS_TO init_pt;
  init_pt.Init();
  if (Update_From_Restricted_Map(wn, &init_pt)) {
    FmtAssert(FALSE, ("TOFO: restricted map"));
  }

  WN* addr_wn = ((OPERATOR_is_scalar_istore (opr) || opr == OPR_MSTORE) ?
                WN_kid1(wn) : WN_kid0(wn));
  INT64 byte_offset = (opr == OPR_PARM || opr == OPR_ASM_INPUT) ?
                    0 : WN_offset(wn);
  INT64 byte_size = 0;
  UINT8 bit_offset = 0;
  UINT8 bit_size = 0;
  BOOL direct_use = (addr_wn != NULL && WN_operator(addr_wn) == OPR_LDID);
  addr_wn = Find_addr_base(addr_wn);
  Is_True(addr_wn != NULL &&
          (WN_operator(addr_wn) == OPR_LDA ||
           WN_operator(addr_wn) == OPR_LDID),
          ("invalid base address expr"));
  POINTS_TO pt;
  pt.Init();
  pt.Set_base_kind(BASE_IS_UNKNOWN);
  pt.Set_ofst_kind(OFST_IS_INVALID);
  Analyze_addr_for_memop(wn, &pt);

  if (addr_wn != NULL) {
    if (opr == OPR_ILDBITS || opr == OPR_ISTBITS) {
      bit_offset = WN_bit_offset(wn);
      bit_size = WN_bit_size(wn);
    }
    if (WN_desc(wn) == MTYPE_BS) {
      UINT bit_field_id = 0;
      UINT64 bit_field_offset = 0;
      FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(
                           WN_ty(wn), WN_field_id(wn),
                           bit_field_id, bit_field_offset);
      byte_offset += bit_field_offset;
      bit_size = FLD_bsize(fld);
      bit_offset = FLD_bofst(fld);
      byte_size = TY_size(FLD_type(fld));
    }
    else if (WN_desc(wn) == MTYPE_M) {
      byte_size = Desc_byte_size(wn);
    }
    else {
      byte_size = MTYPE_size_min(WN_desc(wn)) >> 3;
    }
    
    //WST_IDX idx = Find_wst(opr, 
    if (WN_operator(addr_wn) == OPR_LDA) {
      printf(" WSSA LDA, ");
    }
    else {
      printf(" WSSA LDID, ");
    }
    printf("%s, ofst=%d, size=%d, bofst=%d, bsize=%d\n  ",
           ST_name(WN_st(addr_wn)), (INT32)byte_offset, (INT32)byte_size, (INT32)bit_offset, (INT32)bit_size);
    pt.Print(stdout);
  }

  return WST_INVALID;
}

WST_IDX
WHIRL_SSA_MANAGER::Create_wst_for_wn(WN* wn) {
  OPERATOR opr = WN_operator(wn);
  Is_True(wn != NULL && 
          (OPERATOR_is_load(opr) || OPERATOR_is_store(opr)),
          ("wn is not load or store"));
  if (opr == OPR_LDID || opr == OPR_LDBITS ||
      opr == OPR_STID || opr == OPR_STBITS) {
    return Create_wst_for_direct_memop(wn);
  }
  else {
    return Create_wst_for_indirect_memop(wn);
  }
}

} /* namespace WSSA */

