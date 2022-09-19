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

#include "defs.h"
#include "config_vsa.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vra.h"
#include "opt_vsa.h"
#include "report.h"
#include "erbe.h"
#include "builtin_rule_defs.h"
#include "pro_core.h"
#include "opt_addr_util.h"
#include "opt_vsa_util.h"
#include "opt_vsa_rbc.h"

#include <vector>
#include <ext/hash_set>
using __gnu_cxx::hash_set;

// =============================================================================
//
// VSA_OOB_CHECKER
// An out-of-bound checker POC
// 
// =============================================================================
class VSA_OOB_CHECKER {
  enum {
    FLAG_STORE = -1,   // the coderep is IVAR for istore
    FLAG_OPND0 = 0,    // the coderep is opnd 0
  };
private:
  MEM_POOL   _loc_pool;
  COMP_UNIT *_comp_unit;
  VSA       *_vsa;
  BOOL       _trace;

public:
  VSA_OOB_CHECKER(COMP_UNIT *cu)
    : _comp_unit(cu), _vsa(cu->Vsa()) {
    _trace = Get_Trace(TP_WOPT2, VSA_OOB_DUMP_FLAG);
    OPT_POOL_Initialize(&_loc_pool, "oob check pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_loc_pool, VSA_DUMP_FLAG);
  }

  ~VSA_OOB_CHECKER()
  {
    OPT_POOL_Pop(&_loc_pool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_loc_pool, VSA_DUMP_FLAG);
  }

private:
  MEM_POOL *Loc_pool()        { return &_loc_pool; }
  DNA_NODE *Dna()             { return _comp_unit->Dna(); }
  VSA      *Vsa(void) const   { return _vsa; }
  BOOL      Tracing(void)const{ return _trace; }
  TY_IDX    Find_array_type_from_offset(TY_IDX ty, INT64 ofst,
                                        INT64& fld_ofst, MTYPE ref_mty);
  void      Check_addrinfo_oob(const VSA_ADDRESS_INFO* info, INT64 lb,
                               INT64 ub, BB_NODE* bb,
                               CALL_STACK& cs, SRCPOS_HANDLE* sp_h);
  void      Check_addrinfo_oob(const VSA_ADDRESS_INFO* info, CODEREP* lb,
                               CODEREP* ub, BB_NODE* bb,
                               CALL_STACK& cs, SRCPOS_HANDLE* sp_h);

  void      Check_coderep_oob(CODEREP* cr, BB_NODE* bb, SRCPOS_HANDLE* sp_h,
                              std::vector<bool>& visited, INT flag);
  void      Check_coderep(CODEREP* cr, STMTREP* stmt, INT flag);

  const char *
  Get_certainty_desc(VRA_RESULT res)
  {
    return res == VA_YES ? ISSUE_CERTAINTY_desc(IC_DEFINITELY) :
                    res == VA_POSSIBLE ? ISSUE_CERTAINTY_desc(IC_MAYBE) : "";
  }

public:
  void      Check_heapobj_oob(CODEREP* cr, const VSA_ADDRESS_INFO* info,
                              BB_NODE* bb,
                              CALL_STACK& cs, SRCPOS_HANDLE* sp_h);

  CODEREP  *Get_actualarg(DNA_NODE *caller, RNA_NODE *rna, IDTYPE argid);
  void      Check_callers_arg_vsym(CODEREP *x, VSYM_OBJ_REP *vor,
                                   CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                                   VSA_ADDRESS_INFO *info, std::vector<bool>& visited);
  void      Check_vsym_oob(CODEREP *x, BB_NODE* bb, STMTREP* stmt,
                           CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                           VSA_ADDRESS_INFO *info, std::vector<bool>& visited,
                           VSYM_TRACKER* tracker);
  void      Check_vsym_oob(VSYM_OBJ_REP* vor, BB_NODE* bb, STMTREP* stmt,
                           CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                           VSA_ADDRESS_INFO *info, std::vector<bool>& visited,
                           VSYM_TRACKER* tracker);
  void      Check_symbol_oob(const VSA_ADDRESS_INFO* info,
                             BB_NODE* bb, CALL_STACK& cs, SRCPOS_HANDLE* sp_h);
  void      Check_array_oob(const VSA_ADDRESS_INFO* info,
                            CODEREP* cr, BB_NODE* bb, SRCPOS_HANDLE* srcpos_h);
  void      Check_index_oob(const VSA_ADDRESS_INFO* info,
                            SRCPOS_HANDLE* srcpos_h);

  void      Check_stmtrep(STMTREP* sr, BB_NODE* bb);
};

TY_IDX
VSA_OOB_CHECKER::Find_array_type_from_offset(TY_IDX ty, INT64 ofst,
                                             INT64& fld_ofst, MTYPE ref_mty)
{
  Is_True(TY_kind(ty) == KIND_STRUCT, ("ty is not struct"));
  INT64 cur_ofst = 0;
  TY_IDX ret_ty = (TY_IDX)0;
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_ofst = FLD_ofst(fld);
    TY_IDX cur_fld_idx = FLD_type(fld);
    if (TY_kind(cur_fld_idx) == KIND_ARRAY &&
        cur_ofst <= ofst &&
        (cur_ofst + TY_size(cur_fld_idx)) > ofst) {
      fld_ofst += cur_ofst;
      // check the mtype to find the most correct field
      if (ref_mty != MTYPE_UNKNOWN &&
          ref_mty == TY_mtype(TY_etype(cur_fld_idx)))
        return cur_fld_idx;
      ret_ty = cur_fld_idx;
    }
    if (TY_kind(cur_fld_idx) == KIND_STRUCT &&
        TY_fld(cur_fld_idx) != FLD_HANDLE() &&
        (cur_ofst + TY_size(cur_fld_idx)) > ofst) {
      INT64 new_ofst = ofst - cur_ofst;
      if (new_ofst < 0)
        return (TY_IDX)0;
      TY_IDX arr_ty = Find_array_type_from_offset(cur_fld_idx, new_ofst, fld_ofst, ref_mty);
      if (arr_ty != (TY_IDX)0) {
        fld_ofst += cur_ofst;
        return arr_ty;
      }
    }
  } while (!FLD_last_field(fld_iter++));

  return ret_ty;
}

void
VSA_OOB_CHECKER::Check_addrinfo_oob(const VSA_ADDRESS_INFO* info, INT64 lb, INT64 ub,
                                    BB_NODE* bb, CALL_STACK& cs, SRCPOS_HANDLE* srcpos_h)
{
  INT variable = info->Pos_offset().size() + info->Neg_offset().size() +
                 info->Pos_index().size() + info->Neg_index().size();
  if (variable > 1) {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle multiple index/offset\n"));
    return;
  }

  Is_True(info->Base() != NULL,
          ("address info base is NULL"));
  if (info->Base() == NULL)
    return;

  if (variable == 0) {
    if (info->Fix_ofst() < lb || info->Fix_ofst() >= ub) {
      //srcpos_h->Add_message("offset %d out of range [%d, %d)", info->Fix_ofst(), lb, ub);
      srcpos_h->Set_msgid("AOB.1");
      VSA* base_vsa = info->Dna()->Comp_unit()->Vsa();
      base_vsa->Classify_aob_error(info->Base(), IC_DEFINITELY, srcpos_h);
    }
    return;
  }

  TY_IDX ty;
  INT    fld_id = 0;
  INT64 offset = info->Fix_ofst();
  BOOL is_array = FALSE;
  {
    // switch to base's context
    CONTEXT_SWITCH ctx(info->Dna());
    switch (info->Base()->Kind()) {
    case CK_LDA:
      ty = info->Base()->Lda_ty();
      fld_id = info->Base()->Afield_id();
      break;
    case CK_VAR:
      ty = info->Base()->object_ty();
      fld_id = info->Base()->Field_id();
      break;
    case CK_IVAR:
      ty = info->Base()->object_ty();
      fld_id = info->Base()->I_field_id();
      break;
    default:
      Is_True(FALSE, ("TODO: handle base kind %d", info->Base()->Kind()));
      return;
    }

    if (TY_kind(ty) == KIND_POINTER)
      ty = TY_pointed(ty);

    if (TY_kind(ty) == KIND_STRUCT) {
      TY_IDX arr_ty = (TY_IDX)0;
      INT64 fld_ofst = 0;
      if (fld_id != 0) {
        UINT tmp_fld_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(ty, fld_id, tmp_fld_id);
        Is_True_Ret(!fld.Is_Null(), ("wrong fld id %d", fld_id));
        if (TY_kind(FLD_type(fld)) == KIND_ARRAY) {
          arr_ty = FLD_type(fld);
          fld_ofst = FLD_ofst(fld);
        }
      }
      else {
        // no field id, try to find it according to offset and mtype
        arr_ty = Find_array_type_from_offset(ty, offset, fld_ofst, info->Mtype());
      }
      if (arr_ty != (TY_IDX)0) {
        Is_True_Ret(TY_kind(arr_ty) == KIND_ARRAY, ("not an array type"));
        is_array = TRUE;
        ub = 1;
        for (INT i = 0; i < TY_AR_ndims(arr_ty); ++i)
          ub *= (TY_AR_ubnd_val(arr_ty, i) + 1);
        offset -= fld_ofst;   // adjust offset according to field offset
        INT esize = TY_size(TY_etype(arr_ty));
        if (esize > 0)
          offset /= esize;
        lb = -offset;         // lb = 0 - offset
        ub -= offset;         // ub = ub - offset
      }
    } // NOTE: why not handle KIND_ARRAY at this level?
  }

  CODEREP* index = NULL;
  BOOL     is_neg = info->Neg_index().size() == 1;
  INT      scale = 1;
  INT      remainder = 0;
  INT64    orig_ub;
  INT64    orig_lb;
  INT64    orig_ofst;
  if (info->Pos_index().size() == 1 || is_neg) {
    std::pair<CODEREP*, CODEREP*> cr_pair = is_neg ? info->Neg_index().front()
                                                   : info->Pos_index().front();
    index = cr_pair.first;
    if (index->Kind() != CK_VAR)
      return;
    CODEREP *scale_cr = cr_pair.second;
    if (scale_cr != NULL) {
      Is_True(scale_cr->Kind() == CK_CONST, ("scale not constant"));
      scale = scale_cr->Const_val();
    }
  }
  else if (info->Pos_offset().size() == 1) {
    index = info->Pos_offset().front();
  }
  else if (info->Neg_offset().size() == 1) {
    index = info->Neg_offset().front();
    is_neg = TRUE;
  }

  if (is_array == FALSE) {
    if (scale > 1) {
      orig_lb = lb; lb /= scale;
      orig_ub = ub; remainder = ub % scale; ub /= scale; 
      orig_ofst = offset; offset /= scale;
    }
    if (offset != 0) {
      lb -= offset;
      ub -= offset;
    }
  }

  // exchange lb/ub if index is negative
  if (is_neg) {
    // lb <= -index < ub    -->
    // -ub < index <= -lb   -->
    // -ub+1 <= index < lb + 1
    INT64 tmp = lb;
    lb = -ub + 1;
    ub = -tmp + 1;
  }

  if (index != NULL) {
    // doesn't consider lower bound OOB if index is unsigned
    PATH_SELECTED path;
    srcpos_h->Compose_path_selected(Vsa()->Ipsa(), &path);
    DNA_NODE *index_dna = info->Index_dna();
    VSA      *index_vsa = index_dna->Comp_unit()->Vsa();
    VRA      *index_vra = index_dna->Comp_unit()->Vra();
    BB_NODE  *index_bb  = info->Index_bb();

    if (index->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE* phi = index->Defphi();
      Is_True(phi != NULL, ("phi is null"));
      IDTYPE phi_bb = phi->Bb()->Id();
      CONTEXT_SWITCH ctx(index_dna);
      for (INT i = 0; i < phi->Size(); ++i) {
        if (index_vsa->Is_path_possible(index_bb, phi, i)) {
          BB_NODE* pred = phi->Bb()->Nth_pred(i);
          path.Add_phi(index_dna->Dna_idx(), phi_bb, pred->Id());
        }
      }
    }
    else {
      STMTREP* def = index->Get_defstmt();
      CONTEXT_SWITCH ctx(index_dna);
      if (def != NULL && index_vsa->Is_path_possible(def, cs, srcpos_h) == FALSE)
        return;
    }

    VRA_RESULT res0, res1;
    {
      // switch to index_dna's context because global symtab are referenced below
      CONTEXT_SWITCH ctx(index_dna);
      // Find def of cr by checking u-d of lb and ub
      CODEREP *minb = NULL;
      CODEREP *maxb = NULL;
      BOOL has_bound = index_vra->Get_bounds(index, index_bb, minb, maxb);
      CODEREP *index_in_dna = index;
      VRA *index_in_vra = index_vra;
      IDTYPE bb_in_dna = index_bb->Id();
      DNA_NODE *def_dna = index_dna;
      if (minb != NULL && (minb->Kind() != CK_CONST || minb->Const_val() != -INT64_MAX)) {
        index_vsa->Find_def_cr_in_dna(minb, def_dna, path, index_in_dna, def_dna, bb_in_dna);
        if (def_dna != index_dna)
          index_in_vra = def_dna->Comp_unit()->Vra();
      }
      res0 = (MTYPE_is_unsigned(index->Dtyp()) && lb <= 0)? VA_NO :
                          index_in_vra->Expr_cmp_val<OPR_LT>(index_in_dna, bb_in_dna, lb, path);

      if (index_in_vra != index_vra)
        index_in_vra = index_vra;  // restore index_vra
      if (def_dna != index_dna)
        def_dna = index_dna;       // restore index_dna
      if (bb_in_dna != index_bb->Id())
        bb_in_dna = index_bb->Id();  // restore bb_in_dna
      if (maxb != NULL && (maxb->Kind() != CK_CONST || maxb->Const_val() != INT64_MAX)) {
        index_vsa->Find_def_cr_in_dna(maxb, def_dna, path, index_in_dna, def_dna, bb_in_dna);
        if (def_dna != index_dna)
          index_in_vra = def_dna->Comp_unit()->Vra();
      }
      res1 = index_in_vra->Expr_cmp_val<OPR_GE>(index_in_dna, bb_in_dna, ub, path);
    }

    // check value range with user annotation
    // find value range annotated on coderep
    VALUE_RANGE_VEC *vr_vec = index_vsa->Cr_2_vr(index->Coderep_id());
    if (vr_vec == NULL) {
      // go U-D to see if index is from parameter or function return value
      CODEREP *cr = index;
      STMTREP *defstmt = cr->Get_defstmt();
      while (defstmt != NULL && defstmt->Opr() == OPR_STID) {
        if (defstmt->Rhs() == NULL)
          break;
        cr = defstmt->Rhs();
        if (cr->Kind() == CK_VAR && defstmt != cr->Get_defstmt())
          defstmt = cr->Get_defstmt();
        else
          break;
      }
      if (cr != NULL && cr->Kind() == CK_VAR) {
        IDTYPE parm_idx = index_dna->Is_param(cr);
        if (parm_idx != INVALID_VAR_IDX) {
          // it's from parameter
          vr_vec = index_vsa->Cr_2_vr(cr->Coderep_id());
        }
        else {
          if (defstmt != NULL && defstmt->Opr() == OPR_CALL) {
            // it's return value of certain function
            RNA_NODE *rna = index_dna->Get_callsite_rna(defstmt);
            if (rna != NULL) {
              for (CALLEE_VECTOR::const_iterator iter = rna->Callee_list().begin();
                   iter != rna->Callee_list().end(); iter++) {
                DNA_NODE *callee = index_vsa->Ipsa()->Get_dna(iter->Callee());
                if (callee == NULL)
                  continue;
                if (!callee->Is_set(DNA_HAS_ASSUMPTION))
                  continue;
                vr_vec = callee->Comp_unit()->Vsa()->Cr_2_vr(0);
                if (vr_vec != NULL)
                  break;
              } // for Callee_list
            } // rna != NULL
          } // defstmt->Opr() == OPR_CALL
        }
      } // cr->Kind() == CK_VAR
    } // vr_vec == NULL
#if 0
    // already integrated in vra builder
    if (vr_vec != NULL) {
      // value range annotation found, re-calculate OOB conditions
      for (INT idx = 0; idx < vr_vec->size(); idx++) {
        VALUE_RANGE *vr = (*vr_vec)[idx];
        if (vr != NULL) {
          res0 = (MTYPE_is_unsigned(index->Dtyp()) && lb <= 0) ? VA_NO :
            vr->Lower() < lb ? VA_YES : VA_NO;
          res1 = vr->Upper() > ub ? VA_YES : VA_NO;
          if (res0 == VA_YES || res1 == VA_YES)
            break;
        }
      }
    }
#endif
    if (remainder != 0) {
      // short term fix for rounding error bug, for Sep. 2019 release,
      // use orig_lb, orig_ub, original offset and scale to perform oob test
      INT64 minb = -INT64_MAX;
      INT64 maxb = INT64_MAX;
      FOLD_CONTEXT fctx(index_vsa->Htable());
      BOOL has_bound = index_vra->Get_bounds(index, index_bb, minb, maxb);
      CODEREP *lhs, *rhs;
      if (res0 == VA_POSSIBLE && minb != -INT64_MAX) {
        lhs = index_vra->New_cr(MTYPE_I8, minb*scale);
        rhs = index_vra->New_cr(MTYPE_I8, orig_lb);
        res0 = index_vra->Compare_cr_xfa<OPR_LT>(lhs, index_bb->Id(), rhs, Dna(), path);
      }
      if (res1 == VA_POSSIBLE && maxb != INT64_MAX) { // lhs preserve the roundoff effect
        lhs = index_vra->New_cr(MTYPE_I8, maxb*scale+orig_ofst); 
        rhs = index_vra->New_cr(MTYPE_I8, orig_ub);
        res1 = index_vra->Compare_cr_xfa<OPR_GE>(lhs, index_bb->Id(), rhs, Dna(), path);
      }
    }
    else if (res0 == VA_POSSIBLE || res1 == VA_POSSIBLE) {
      // refine possible by check u-d of lb and ub
      CODEREP  *minb = NULL;
      CODEREP  *maxb = NULL;
      FOLD_CONTEXT fctx(index_vsa->Htable());
      BOOL has_bound = index_vra->Get_bounds(index, index_bb, minb, maxb);
      if (res0 == VA_POSSIBLE && minb != NULL &&
          (minb->Kind() != CK_CONST || minb->Const_val() != -INT64_MAX)) {
        CODEREP *rhs = index_vra->New_cr(MTYPE_I8, lb);
        res0 = index_vra->Compare_cr_xfa<OPR_LT>(minb, index_bb->Id(), rhs, Dna(), path);
      }
      if (res1 == VA_POSSIBLE && maxb != NULL &&
          (maxb->Kind() != CK_CONST || maxb->Const_val() != INT64_MAX)) {
        CODEREP *rhs = index_vra->New_cr(MTYPE_I8, ub);
        res1 = index_vra->Compare_cr_xfa<OPR_GE>(maxb, index_bb->Id(), rhs, Dna(), path);
      }
    }

    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d < %lld: %s\n",
                             index->Coderep_id(),
                             lb, VRA_RESULT_NAME(res0)));
    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d >= %lld: %s\n",
                             index->Coderep_id(),
                             ub, VRA_RESULT_NAME(res1)));
    if (res0 == VA_YES || res0 == VA_POSSIBLE ||
        res1 == VA_YES || res1 == VA_POSSIBLE) {
#if 0
      const char* var = NULL;
      {
        CONTEXT_SWITCH ctx(index_dna);
        var = srcpos_h->Find_cr_stname(index, NULL, index_dna);
      }
      if (var == NULL) var = "<index>";
      const char* lb_ic   = Get_certainty_desc(res0);
      const char* lb_desc = (res0 == VA_YES || res0 == VA_POSSIBLE) ? " less than lower bound." : "";
      const char* ub_ic   = Get_certainty_desc(res1);
      const char* ub_desc = (res1 == VA_YES || res1 == VA_POSSIBLE) ? " greater than upper bound." : "";
      srcpos_h->Add_message("%s out of range [%d, %d): %s%s%s%s", var, lb, ub,
                            lb_ic, lb_desc, ub_ic, ub_desc);
#endif
      ISSUE_CERTAINTY ic  = (res0 == VA_YES || res1 == VA_YES) ? IC_DEFINITELY : IC_MAYBE;
      srcpos_h->Set_msgid("AOB.1");
      index_vsa->Classify_aob_error(index, ic, srcpos_h);
    }
    else if (res0 == VA_UNKNOWN && res1 == VA_UNKNOWN) {
      // unknown index range, check if index is tainted
      TAG_BASE *tag_base = Vsa()->Rbc()->Find_tag_base((char *) "tainted");
      if (tag_base) {
        // switch to index_dna's context because global symtab are referenced below
        CONTEXT_SWITCH dna_ctx(index_dna);
        if (index_vsa->Rbc()->Check_tag(index_dna, info->Index_stmt(), index,
                                        CHECK_BY_TAG, tag_base, srcpos_h)) {
          ISSUE_CERTAINTY ic  = IC_MAYBE;
          srcpos_h->Set_msgid("AOB.1");
          index_vsa->Classify_aob_error(index, ic, srcpos_h);
        }
      }
    }
  } // index != NULL
  else {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle negtive index\n"));
  }
}

void
VSA_OOB_CHECKER::Check_addrinfo_oob(const VSA_ADDRESS_INFO* info, CODEREP* lb, CODEREP* ub,
                                    BB_NODE* bb, CALL_STACK& cs, SRCPOS_HANDLE* srcpos_h)
{
  if (info->Pos_offset().size() > 0 ||
      info->Neg_offset().size() > 0) {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle variable offset\n"));
    return;
  }
  if (info->Pos_index().size() + info->Neg_index().size() != 1) {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle multiple index\n"));
    return;
  }

  CODEREP* index = NULL;
  CODEREP* scale = NULL;
  if (info->Pos_index().size() == 1) {
    index = info->Pos_index().front().first;
    scale = info->Pos_index().front().second;
    if (index->Kind() != CK_VAR)
      return;
  }

  INT scale_const = 1;
  if (scale != NULL) {
    Is_True(scale->Kind() == CK_CONST,
            ("scale not constant"));
    //lb /= info->Scale()->Const_val();
    scale_const = scale->Const_val();
    lb = _comp_unit->Vra()->New_div_cr(lb, scale_const);
    if (lb == NULL) {
      Is_Trace(_trace, (TFile, "VSA-AOB-TODO: not able to simplify scale\n"));
      return;
    }
    //ub /= info->Scale()->Const_val();
    ub = _comp_unit->Vra()->New_div_cr(ub, scale_const);
    if (ub == NULL) {
      Is_Trace(_trace, (TFile, "VSA-AOB-TODO: not able to simplify scale\n"));
      return;
    }
  }
  if (info->Fix_ofst() != 0) {
    INT64 ofst = info->Fix_ofst() / scale_const;
    INT   rem  = info->Fix_ofst() % scale_const;
    if (rem < 0)
      ofst -= 1;
    if (ofst != 0) {
      CODEREP* ofst_cr = _comp_unit->Vra()->New_cr(MTYPE_I8, ofst);
      //lb -= info->Fix_ofst();
      lb = _comp_unit->Vra()->New_cr(OPCODE_make_op(OPR_SUB, MTYPE_I8, MTYPE_V),
                        lb,
                        ofst_cr,
                        TRUE);
      //ub -= info->Fix_ofst();
      ub = _comp_unit->Vra()->New_cr(OPCODE_make_op(OPR_SUB, MTYPE_I8, MTYPE_V),
                        ub,
                        ofst_cr,
                        TRUE);
    }
  }
  if (index != NULL) {
    PATH_SELECTED path;
    srcpos_h->Compose_path_selected(Vsa()->Ipsa(), &path);
    DNA_NODE *index_dna = info->Index_dna();
    VSA      *index_vsa = index_dna->Comp_unit()->Vsa();
    VRA      *index_vra = index_dna->Comp_unit()->Vra();
    BB_NODE  *index_bb  = info->Index_bb();

    if (index->Is_flag_set(CF_DEF_BY_PHI)) {
      PHI_NODE* phi = index->Defphi();
      Is_True(phi != NULL, ("phi is null"));
      IDTYPE phi_bb = phi->Bb()->Id();
      CONTEXT_SWITCH ctx(index_dna);
      for (INT i = 0; i < phi->Size(); ++i) {
        if (index_vsa->Is_path_possible(index_bb, phi, i)) {
          BB_NODE* pred = phi->Bb()->Nth_pred(i);
          path.Add_phi(index_dna->Dna_idx(), phi_bb, pred->Id());
        }
      }
    }
    else {
      STMTREP* def = index->Get_defstmt();
      CONTEXT_SWITCH ctx(index_dna);
      if (def != NULL && index_vsa->Is_path_possible(def, cs, srcpos_h) == FALSE)
        return;
    }

    // problematic here because index and lb/ub may in different functions
    VRA_RESULT res0, res1;
    {
      FOLD_CONTEXT fctx(index_vsa->Htable());
      res0 = index_vra->Compare_cr<OPR_LT>(index, index_bb, lb, path);
      res1 = index_vra->Compare_cr<OPR_GE>(index, index_bb, ub, path);
    }

    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d < cr%d: %s\n",
                             index->Coderep_id(),
                             lb->Coderep_id(), VRA_RESULT_NAME(res0)));
    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d >= cr%d: %s\n",
                             index->Coderep_id(),
                             ub->Coderep_id(), VRA_RESULT_NAME(res1)));
    if (res0 == VA_POSSIBLE || res1 == VA_POSSIBLE) {
      // refine possible by check u-d of lb and ub
      CODEREP  *minb = NULL;
      CODEREP  *maxb = NULL;
      BOOL has_bound = index_vra->Get_bounds(index, index_bb, minb, maxb);
      if (res0 == VA_POSSIBLE && minb != NULL &&
          (minb->Kind() != CK_CONST || minb->Const_val() != -INT64_MAX))
        res0 = index_vra->Compare_cr_xfa<OPR_LT>(minb, index_bb->Id(), lb, Dna(), path);
      if (res1 == VA_POSSIBLE && maxb != NULL &&
          (maxb->Kind() != CK_CONST || maxb->Const_val() != INT64_MAX))
        res1 = index_vra->Compare_cr_xfa<OPR_GE>(maxb, index_bb->Id(), ub, Dna(), path);
    }
    if (res0 == VA_YES || res0 == VA_POSSIBLE ||
        res1 == VA_YES || res1 == VA_POSSIBLE) {
#if 0
      const char* var = NULL;
      {
        CONTEXT_SWITCH ctx(index_dna);
        var = srcpos_h->Find_cr_stname(index, NULL, index_dna);
      }
      if (var == NULL) var = "<index>";
      const char* lb_val = srcpos_h->Find_cr_stname(lb, NULL, Dna());
      if (lb_val == NULL) lb_val = "<lb>";
      const char* ub_val = srcpos_h->Find_cr_stname(ub, NULL, Dna());
      if (ub_val == NULL) ub_val = "<ub>";
      const char* lb_ic   = Get_certainty_desc(res0);
      const char* lb_desc = (res0 == VA_YES || res0 == VA_POSSIBLE) ? " less than lower bound." : "";
      const char* ub_ic   = Get_certainty_desc(res1);
      const char* ub_desc = (res1 == VA_YES || res1 == VA_POSSIBLE) ? " greater than upper bound." : "";
      srcpos_h->Add_message("%s out of range [%s, %s): %s%s%s%s", var, lb_val, ub_val,
                            lb_ic, lb_desc, ub_ic, ub_desc);
#endif
      ISSUE_CERTAINTY ic  = (res0 == VA_YES || res1 == VA_YES) ? IC_DEFINITELY : IC_MAYBE;
      srcpos_h->Set_msgid("AOB.1");
      index_vsa->Classify_aob_error(index, ic, srcpos_h);
    }
    else if (res0 == VA_UNKNOWN && res1 == VA_UNKNOWN) {
      // unknown index range, check if index is tainted
      TAG_BASE *tag_base = Vsa()->Rbc()->Find_tag_base((char *) "tainted");
      if (tag_base) {
        // switch to index_dna's context because global symtab are referenced below
        CONTEXT_SWITCH dna_ctx(index_dna);
        if (index_vsa->Rbc()->Check_tag(index_dna, info->Index_stmt(), index,
                                        CHECK_BY_TAG, tag_base, srcpos_h)) {
          ISSUE_CERTAINTY ic  = IC_MAYBE;
          srcpos_h->Set_msgid("AOB.1");
          index_vsa->Classify_aob_error(index, ic, srcpos_h);
        }
      }
    }
  }
  else {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle negtive index\n"));
  }
}

void
VSA_OOB_CHECKER::Check_heapobj_oob(CODEREP* cr, const VSA_ADDRESS_INFO* info, BB_NODE* bb,
                                   CALL_STACK& cs, SRCPOS_HANDLE* srcpos_h)
{
  HEAP_OBJ_REP* ho = Vsa()->Cr_2_heap_obj(cr);
  if (ho == NULL) {
    Is_Trace(_trace, (TFile, "VSA-AOB-WARN: no heap object found for cr%d\n",
                             info->Base()->Coderep_id()));
    return;
  }
  Is_True_Ret(ho->Attr() != ROR_DEF_BY_VARPHI && ho->Attr() != ROR_DEF_BY_VORPHI,
              ("ho def by varphi/vorphi, which doesn\'t have size"));
  CODEREP* size = ho->Heap_obj()->Byte_size();
  if (size == NULL) {
    Is_Trace(_trace, 
             (TFile, "VSA-AOB-TODO: HEAP_OBJ size is not set\n"));
    return;
  }
  INT64 lb = 0;
  // Java array data start from some offset
  // skip fld_id > 0, get array length will load fid 4
  if(PU_java_lang(Get_Current_PU()) && info->Fix_ofst() > 0 && info->Fld_id() == 0) {
    FLD_HANDLE fld = Get_java_array_data_fld(cr);
    if(!fld.Is_Null()) {
      lb = FLD_ofst(fld);
    }
  }
  if (size->Kind() == CK_CONST) {
    Check_addrinfo_oob(info, lb, size->Const_val(), bb, cs, srcpos_h);
  }
  // only check when size is defined in the same DNA as index
  // TODO: handle size parameter later
  else if (_comp_unit->Dna() == info->Index_dna()) {
    CODEREP* lb = _comp_unit->Vra()->New_cr(MTYPE_I8, 0);
    Check_addrinfo_oob(info, lb, size, bb, cs, srcpos_h);
  }
}

void
VSA_OOB_CHECKER::Check_callers_arg_vsym(CODEREP *x, VSYM_OBJ_REP *vor,
                                        CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                                        VSA_ADDRESS_INFO *info,
                                        std::vector<bool>& visited)
{
#if 0
  IDTYPE which_arg = Dna()->Is_param(x);
  IPSA  *ipsa = Vsa()->Ipsa();

  // check all callers in call-by list
  if (Dna()->Clby_list()->size() > VAR_INIT_ID) {
    RNA_NODE* call_site;
    SRCPOS_TREENODE *cur_treenode;
    if (cs.empty()) {
      call_site = NULL;
      // fork the srcpos_h before traverse the caller chain
      sp_h->Add_children(Dna()->Clby_list()->size() - 1);
      cur_treenode = sp_h->Cur_node();
    }
    else {
      call_site = cs.top();
      cs.pop();
    }

    for (INT i = VAR_INIT_ID; i < Dna()->Clby_list()->size(); ++i) {
      RNA_NODE *rna = (*Dna()->Clby_list())[i];

      // skip callsites which isn't previous call site
      if (call_site != NULL && call_site != rna)
        continue;

       // skip the rna if it has already visited
      if (ipsa->Traved(rna))
        continue;
      ipsa->Set_trav(rna);

      DNA_NODE *caller = ipsa->Get_dna(rna->Caller_idx());
      {
        Is_True(caller != NULL, ("bad caller"));
        // switch to caller context and push a new frame
        CONTEXT_SWITCH context(caller);
        CODEREP* cr = Get_actualarg(caller, rna, which_arg);
        if (cr == NULL)
          continue;

        STMTREP* stmt = rna->Callstmt();

        CHECKER_STATUS sts = hlp.Status();
        if (_CHECKER::SUSPECT & CS_HEAP_OBJ) {
          HEAP_OBJ_REP* hor = Vsa()->Cr_2_heap_obj(cr);
          if (sts == CS_VAR_UD && hor != NULL) {
            hor = Vsa()->Find_stmt_cur_hor(stmt, hor->Heap_obj());
          }
          else if (!Tracker()->Empty()) {
            VSYM_TRACKER::VSYM_VECTOR vsym_stack = Tracker()->Save_stack();
            VSYM_FLD_REP *vfr = Tracker()->Fld_rep();
            if (Tracker()->Vor()) {
              hor = Tracker()->Vor()->Vsym_obj()->Base_hor();
            }
            else {
              // check if IVAR base is LDA?
            }
            while (hor != NULL && !Tracker()->Empty()) {
              FIELD_OBJ_REP* fobj = hor->Find_fld(vfr);
              if (fobj == NULL) {
                hor = NULL;
                break;
              }
              Is_True(vfr->Match(fobj->Fld_rep()), ("fld id mismatch"));
              hor = fobj->Hor();
              vfr = Tracker()->Fld_rep();
              Tracker()->Pop();
            }
            Tracker()->Restore_stack(vsym_stack);
          }
          if (hor != NULL) {
            Set_hor(hor);
          }
          else {
            Set_hor(NULL);
          }
        }

        // set context
        _ctx.Set_bb(stmt->Bb());
        _ctx.Set_stmtrep(stmt);
        _ctx.Set_coderep(cr);
        if (sts == CS_VAR_UD)
          sts = Check_coderep();
        else if (sts == CS_VSYM_UD)
          sts = Check_vsym_ud();
        //else if (sts == CS_HOR_UD)
        //  sts = Check_heap_obj_ud();
        else if (sts != CS_DONE) {
          Is_True(FALSE, ("bad status"));
        }
        if (sts != CS_DONE)
          Continue_trav(sts);
        // restart stack after visiting the caller
        _ctx.Tracker()->Restore_stack(vsym_stack);
        // pop-up the frame
        TRAV_FRAME* tmp = _ctx.Pop_frame(TD_UP);
        Is_True(tmp == par_frame, ("bad call stack"));
        // break if go back to previous call site
        if (frame != NULL)
          break;
      }
    }
    if(me->Clby_list()->size() == VAR_INIT_ID)
    {
      _checker.Add_def_cr(me, _ctx.Coderep());
    }
    return;
  }
#endif
}

// =============================================================================
//
// VSA_OOB_CHECKER::Check_vsym_oob, Track down the value of a vsym for OOB check
//      Start out for AOB checking.  Clone the full list of paramenters.  Will
//      need to clean them up.
//
// =============================================================================
void
VSA_OOB_CHECKER::Check_vsym_oob(CODEREP *x, BB_NODE* bb, STMTREP* stmt,
                                CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                                VSA_ADDRESS_INFO *info, std::vector<bool>& visited,
                                VSYM_TRACKER* tracker)
{
  CODEREP     *cr = x;
  VSYM_OBJ_REP *vor = Vsa()->Cr_2_vor(x);
  if (vor == NULL) {
    Is_Trace(Tracing(), (TFile, "Check_vsym_oob: no vsym object found for cr%d\n",
                         x->Coderep_id()));
    return;
  }
  Is_Trace_cmd(Tracing(), vor->Print(TFile));
  Is_Trace(Tracing(), (TFile, " :\n"));

  VSYM_TRACKER n_tracker;
  if (tracker == NULL)
    tracker = &n_tracker;

  if ( tracker->Init(Vsa(), &cr, stmt,  Vsa()->Loc_pool()) != CS_VSYM_UD )
    return;
  Check_vsym_oob(vor, bb, stmt, cs, sp_h, info, visited, tracker);
}

void
VSA_OOB_CHECKER::Check_vsym_oob(VSYM_OBJ_REP* vor, BB_NODE* bb, STMTREP* stmt,
                                CALL_STACK& cs, SRCPOS_HANDLE* sp_h,
                                VSA_ADDRESS_INFO *info, std::vector<bool>& visited,
                                VSYM_TRACKER* tracker)
{
  Is_True(vor != NULL, ("null vor"));
  switch (vor->Attr()) {
  case ROR_DEF_BY_CHI:
  {
    STMTREP *sr = vor->Stmt_def();
    // do not assert, as vor may not have define, report UIV? 
    Is_Trace(Tracing(), (TFile, " Check_vsym_oob: vo%dv%d def by chi:\n",
                         vor->Vsym_obj()->Id(), vor->Version()));
    if (sr != NULL) {
      Is_Trace_cmd(Tracing(), sr->Print(TFile));
    }
    else {
      Is_Trace(Tracing(), (TFile, "  sr is NULL.\n"));
      return;
    }
    if (vor->Is_entry_chi() || (sr && sr->Opr() == OPR_OPT_CHI)) {
      // parameter, global from caller
      CODEREP* base = Vsa()->Find_vor_chi_cr(sr, vor);
      Is_True(base != NULL, ("not find cr for vor on OPT_CHI"));

      if (base != NULL) {
        sp_h->Append_data(sr, base, Dna(), PATHINFO_CHI);
        IDTYPE parm = Dna()->Is_param(base);
        if (parm != INVALID_VAR_IDX) {
          if (IPSA_insession())
            Dna()->Check_callers_argument_for_aob(Vsa()->Ipsa(), base, cs, sp_h, info, tracker);
        }
        else
          ;//Check_caller_global_vsym(base, vor);
      }
    }
#if 0
    else if (OPERATOR_is_call(sr->Opr())) {
      // check the side effect from the call
      RNA_NODE *rna = Vsa()->Sr_2_rna(sr);
      if (rna != NULL) {
        if(Ipsa()->Is_jni_call(rna)) {
          return Check_jni(sr, rna);
        } else {
          CODEREP* cr = Vsa()->Find_vor_chi_cr(sr, vor);
          Is_True(cr != NULL, ("not find cr for vor on OPT_CHI"));
          sp_h->Append_data(sr, cr, Dna(), PATHINFO_CALL_CHI);
          AUX_STAB_ENTRY* aux = NULL;
          if (cr->Kind() == CK_VAR) {
            aux = Vsa()->Opt_stab()->Aux_stab_entry(cr->Aux_id());
            // check if return value
            if (aux->Is_return_preg())
              return Check_callee_return_vsym(rna, cr->Offset(), vor);
            if (!aux->St())
              return;
          }
          // check if parameter
          pair<IDTYPE, BOOL> arg = rna->Get_arg(cr, TRUE);
          if (arg.first != INVALID_VAR_IDX) {
            if (arg.second)
              return Check_callee_output_vsym(rna, arg.first, vor);
            else
              return Check_callee_input_vsym(rna, arg.first, vor);
          }
          if (aux && aux->Is_global())
            return Check_callee_global_vsym(rna, aux->St(), vor);
        }
      }
    }
    else {
      Is_True(FALSE, ("TODO: Handle %s for vsym chi def", OPERATOR_name(sr->Opr()) + 4));
    }
#endif
  }
  break;
  case ROR_DEF_BY_PHI:
    break;
  case ROR_DEF_BY_ISTORE:
    break;
  case ROR_DEF_BY_NONE:
    break;
  default:
    break;
  }
}


void
VSA_OOB_CHECKER::Check_symbol_oob(const VSA_ADDRESS_INFO* info, BB_NODE* bb, CALL_STACK& cs, SRCPOS_HANDLE* srcpos_h)
{
  CODEREP* base = info->Base();
  Is_True(base != NULL && base->Kind() == CK_LDA,
          ("base is not LDA"));
  // TODO, check if st is VLA or VLS
  ST* st = base->Lda_base_st();
  if (ST_class(st) != CLASS_VAR)
    return;
  if (ST_is_temp_var(st))
    return;
  INT64 size = ST_size(st);
  if (size == 0) {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: Check_symbol_oob size is 0\n"));
    return;
  }
  Check_addrinfo_oob(info, 0, size, bb, cs, srcpos_h);
}

void
VSA_OOB_CHECKER::Check_array_oob(const VSA_ADDRESS_INFO* info, CODEREP* cr, BB_NODE* bb, SRCPOS_HANDLE* srcpos_h)
{
  Is_True(cr->Kind() == CK_OP && cr->Opr() == OPR_ARRAY,
          ("not array cr"));
  INT dim = cr->Kid_count() >> 1;
  INT i;
  for (i = 1; i <= dim; ++ i) {
    CODEREP *size = cr->Opnd(i);
    CODEREP *index = cr->Opnd(i + dim);
    // TODO: verify array bounds by TY/ARB
    // TODO: array in other languages than C/C++
    Is_True(size->Kind() == CK_CONST, ("TODO: array upper bound is not const"));
    PATH_SELECTED path;
    srcpos_h->Compose_path_selected(Vsa()->Ipsa(), &path);
    // doesn't consider lower bound OOB if index is unsigned
    VRA_RESULT res0 = MTYPE_is_unsigned(index->Dtyp()) ? VA_NO :
                          _comp_unit->Vra()->Expr_cmp_val<OPR_LT>(index, info->Bb()->Id(), (INT64)0, path);
    VRA_RESULT res1 = _comp_unit->Vra()->Expr_cmp_val<OPR_GE>(index, info->Bb()->Id(), size->Const_val(), path);
    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d < 0: %s\n",
                             index->Coderep_id(),
                             VRA_RESULT_NAME(res0)));
    Is_Trace(_trace, (TFile, "VSA-AOB: index cr%d >= %lld: %s\n",
                             index->Coderep_id(),
                             size->Const_val(), VRA_RESULT_NAME(res1)));
    if (res0 == VA_YES || res0 == VA_POSSIBLE ||
        res1 == VA_YES || res1 == VA_POSSIBLE) {
#if 0
      const char* var = NULL;
      {
        CONTEXT_SWITCH ctx(_comp_unit->Dna());
        var = srcpos_h->Find_cr_stname(index, NULL, _comp_unit->Dna());
      }
      if (var == NULL) var = "<index>";
      const char* lb_ic   = Get_certainty_desc(res0);
      const char* lb_desc = (res0 == VA_YES || res0 == VA_POSSIBLE) ? " less than lower bound." : "";
      const char* ub_ic   = Get_certainty_desc(res1);
      const char* ub_desc = (res1 == VA_YES || res1 == VA_POSSIBLE) ? " greater than upper bound." : "";
      srcpos_h->Add_message("%s %s out of range [%d, %d): %s%s%s%s", var, 0, size->Const_val(),
                            lb_ic, lb_desc, ub_ic, ub_desc);
#endif
      ISSUE_CERTAINTY ic  = (res0 == VA_YES || res1 == VA_YES) ? IC_DEFINITELY : IC_MAYBE;
      srcpos_h->Set_msgid("AOB.1");
      _comp_unit->Vsa()->Classify_aob_error(index, ic, srcpos_h);
    }
  }
}

void
VSA_OOB_CHECKER::Check_index_oob(const VSA_ADDRESS_INFO* info, SRCPOS_HANDLE* srcpos_h)
{
  // check if index used in iload/istore has lower/upper bound
  if (info->Pos_index().size() + info->Neg_index().size() != 1) {
    Is_Trace(_trace, (TFile, "VSA-AOB-TODO: handle multiple index\n"));
    return;
  }

  DNA_NODE *index_dna = info->Index_dna();
  Is_True_Ret(index_dna != NULL, ("not find index dna"));
  VSA *index_vsa = info->Index_dna()->Comp_unit()->Vsa();
  Is_True_Ret(index_vsa != NULL, ("not find index vsa"));
  VRA *index_vra = info->Index_dna()->Comp_unit()->Vra();
  Is_True_Ret(index_vra != NULL, ("not find index vra"));
  BB_NODE *index_bb = info->Index_bb();
  Is_True_Ret(index_bb != NULL, ("not find index bb"));

  CODEREP *ub = NULL;
  for (INT i = 0; i < info->Pos_index().size(); ++i) {
    CODEREP *index = info->Pos_index()[i].first;
    Is_True(index != NULL, ("sym is null"));
    // skip other kinds than CK_VAR
    if (index->Kind() != CK_VAR)
      continue;
    AUX_STAB_ENTRY *sym = index_vsa->Opt_stab()->Aux_stab_entry(index->Aux_id());
    Is_True(sym != NULL, ("sym is null"));
    ST *st = sym->St();
    // skip non-local variable
    if (st == NULL ||
        (ST_sclass(st) != SCLASS_REG &&
         ST_sclass(st) != SCLASS_AUTO))
      continue;
    BOOL has_ub = index_vra->Get_ub(index, index_bb, ub);
    if (has_ub == FALSE) {
      // if index doesn't have a upper bound, report a M AOB
      srcpos_h->Set_msgid("AOB.1");
      index_vsa->Classify_aob_error(index, IC_MAYBE, srcpos_h);
      return;
    }
  }
}

void
VSA_OOB_CHECKER::Check_coderep(CODEREP* cr, STMTREP* stmt, INT flag)
{
  INT i;
  CODEREP *base;

  switch (cr->Kind()) {
  case CK_IVAR:
    // check base at first
    Check_coderep(flag == FLAG_STORE ? cr->Istr_base() : cr->Ilod_base(), stmt, 0);
    if (cr->Opr() == OPR_PARM) {
      Is_True(flag != FLAG_STORE, ("parm should not be store"));

      // check if argument is marked with REF_ILOAD or REF_ISTORE
      RNA_NODE *rna = Vsa()->Sr_2_rna(stmt);
      if (rna != NULL && rna->Callee_cnt() > 0) {
        if (flag < stmt->Rhs()->Kid_count() &&
            !rna->Is_set_arg_flag(flag + VAR_INIT_ID, REF_ILOAD | REF_ISTORE))
          break; // NO ILOAD/ISTORE, don't check AOB
      }

      // Insert an additional layer for buffer underwrite check
      // which might be an address computation expression to be
      // passed to another function.
      VSA_ADDRESS_INFO info;
      if (_comp_unit->Analyze_address_info(stmt, cr, &info, FALSE, FALSE) ) {
        if (info.Base() == NULL) {
          break; // may want to report
        }
        // do not report AOB on LDA_LABEL
        // do not report AOB on last param of va_arg
        if (info.Base()->Kind() == CK_LDA &&
            (info.Base()->Is_flag_set(CF_LDA_LABEL) ||
             Dna()->Is_va_arg_last_param(info.Base()->Lda_base_st())))
          break;
        SRCPOS_HANDLE srcpos_h(info.Base(), stmt, Vsa()->Comp_unit()->Dna(),
                               Loc_pool(), Vsa());
        std::vector<bool> visited;
        visited.resize(_comp_unit->Cfg()->Total_bb_count());
        CALL_STACK cs;
        Vsa()->Ipsa()->Begin_trav_counter();
        Vsa()->Classify_aob_error(info.Base(), stmt->Bb(), stmt, cs,
                                 &srcpos_h, &info, visited);
        Vsa()->Ipsa()->End_trav_counter();
        Is_True(cs.empty(), ("call stack is not empty"));
      }
    }
    else {
      VSA_ADDRESS_INFO info;
      BOOL store = (flag == FLAG_STORE);
      if (_comp_unit->Analyze_address_info(stmt, cr, &info, store, FALSE) == FALSE)
        break;
      if (info.Base() == NULL) // NPD issue
        break;

      SRCPOS_HANDLE srcpos_h(info.Base(), stmt, Vsa()->Comp_unit()->Dna(), Loc_pool(), Vsa(), cr);
      std::vector<bool> visited;
      visited.resize(_comp_unit->Cfg()->Total_bb_count());
      CALL_STACK cs;
      Vsa()->Ipsa()->Begin_trav_counter();
      Vsa()->Classify_aob_error(info.Base(), stmt->Bb(), stmt, cs, &srcpos_h, &info, visited);
      Vsa()->Ipsa()->End_trav_counter();
      Is_True(cs.empty(), ("call stack is not empty"));
    }
    break;
  case CK_OP:
    for (i = 0; i < cr->Kid_count(); ++i) {
      Check_coderep(cr->Opnd(i), stmt, i);
    }
    break;
  case CK_VAR:
    {
      ST *st = Vsa()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->St();
      INT32 ofst = cr->Offset();
      INT32 size = MTYPE_byte_size(cr->Dsctyp());
      if (st != NULL && ST_class(st) == CLASS_VAR) {
        if (Dna()->Is_va_arg_last_param(st))
          break;  // do not report AOB on last param of va_arg
        INT64 st_size = 0;
        if (ST_sclass(st) == SCLASS_EXTERN) {
          WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
          UINT32 file_idx = File_Index;
          ST_IDX st_idx = ST_st_idx(st);
          if (mgr != NULL && mgr->Resolve(file_idx, st_idx, file_idx, st_idx))
            st_size = ST_size(file_idx, st_idx);
          else
            break;
        }
        else {
          st_size = ST_size(st);
        }
        if (ofst < 0 || ofst + size > st_size) {
          SRCPOS_HANDLE srcpos_h(cr, stmt, Vsa()->Comp_unit()->Dna(), Loc_pool());
          //srcpos_h.Add_message("offset %d out of range [%d, %d)", ofst, 0, ST_size(st));
          srcpos_h.Set_msgid("AOB.1");
          Vsa()->Classify_aob_error(cr, IC_DEFINITELY, &srcpos_h);
        }
      }
    }
    break;
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  default:
    break;
  }
}


void
VSA_OOB_CHECKER::Check_stmtrep(STMTREP* sr, BB_NODE* bb) {
  if (OPERATOR_is_store(sr->Opr()) && sr->Lhs() != NULL)
    Check_coderep(sr->Lhs(), sr, FLAG_STORE);
  if (sr->Rhs() != NULL)
    Check_coderep(sr->Rhs(), sr, 0);
}

// =============================================================================
// VSA::Classify_aob_error(CODEREP *cr, BB_NODE* cur_bb, STMTREP* stmt,
//                         CALL_STACK& cs, SRCPOS_HANDLE* sp_h, VSA_ADDRESS_INFO *info,
//                         std::vector<bool>& visited, VSYM_TRACKER *tracker)
// =============================================================================
void
VSA::Classify_aob_error(CODEREP *cr, BB_NODE* cur_bb, STMTREP* stmt,
                        CALL_STACK& cs, SRCPOS_HANDLE* sp_h, VSA_ADDRESS_INFO *info,
                        std::vector<bool>& visited, VSYM_TRACKER *tracker)
{
  Is_True(cr != NULL, ("cr is NULL"));
  Is_True(sp_h != NULL, ("srcpos handle is NULL"));
  Is_True(info != NULL && info->Base() != NULL,
          ("address or base is NULL"));

  if (cr->Kind() == CK_CONST || cr->Kind() == CK_RCONST) {
    // NPD, or RBC need to handle this
    return;
  }

  if (cr->Kind() == CK_OP) {
    if (cr->Opr() == OPR_ARRAY) {
      VSA_OOB_CHECKER checker(Comp_unit());
      checker.Check_array_oob(info, cr, info->Bb(), sp_h);
      return;
    }
    if (cr->Opr() == OPR_ALLOCA) {
      VSA_OOB_CHECKER checker(Comp_unit());
      checker.Check_heapobj_oob(cr, info, cur_bb, cs, sp_h);
      return;
    }
    Is_True(stmt != NULL, ("stmt is NULL"));
    VSA_ADDRESS_INFO n_info;
    if (Comp_unit()->Analyze_pointer_info(stmt, cr, &n_info, FALSE) == FALSE)
      return;
    if (n_info.Base() == NULL)
      return;
    // do not report AOB on LDA_LABEL
    // do not report AOB on last param of va_arg
    if (n_info.Base()->Kind() == CK_LDA &&
        (n_info.Base()->Is_flag_set(CF_LDA_LABEL) ||
         Dna()->Is_va_arg_last_param(n_info.Base()->Lda_base_st())))
      return;
    info->Merge(&n_info);
    Is_True(info->Base() != NULL, ("not find base address"));
    sp_h->Set_orig_stname(Dna(), info->Base());
    Classify_aob_error(info->Base(), cur_bb, stmt, cs, sp_h, info, visited, tracker);
    return;
  }

  if (cr->Kind() == CK_LDA) {
    Is_True(stmt != NULL, ("stmt is NULL"));
    if (tracker == NULL) {
      if (!Is_path_possible(stmt, cs, sp_h))
        return;

      info->Merge(cr, Comp_unit()->Dna());
      // get size and check if it's in range
      VSA_OOB_CHECKER checker(Comp_unit());
      checker.Check_symbol_oob(info, cur_bb, cs, sp_h);
    }
    else if (tracker->Size() == 1) {
      MU_NODE* mu = Find_stmt_var_mu(stmt, cr->Lda_base_st(), tracker->Fld_rep());
      if (mu != NULL) {
        Classify_aob_error(mu->OPND(), stmt->Bb(), stmt, cs, sp_h, info, visited);
      }
    }
    // TODO: multi-level IVAR
    return;
  }

  if (cr->Kind() == CK_IVAR) {
    Is_True(stmt != NULL, ("stmt is NULL"));
    // This routine traverse the U-D of variable.
    // Vsym U-D should be the traversed as well.
    if (VSA_Model_Lda() && Is_path_possible(stmt, cs, sp_h)) {
      VSA_OOB_CHECKER checker(Comp_unit());
      checker.Check_vsym_oob(cr, cur_bb, stmt, cs, sp_h, info, visited, tracker);
    }
    return;
  }

  if (cr->Kind() == CK_VAR &&
      cr->Is_flag_set(CF_DEF_BY_PHI)) {
    PHI_NODE *phi = cr->Defphi();
    Is_True(phi != NULL, ("invalid def phi"));
    BB_NODE *phi_bb = phi->Bb();

    if (visited[phi_bb->Id()])
      return;
    visited[phi_bb->Id()] = true;

    BB_LOOP *loop_info = phi_bb->Loop();
    if (loop_info) {
      CODEREP *lb = NULL, *ub = NULL;
      HEAP_OBJ_REP *hor = Comp_unit()->Vsa()->Cr_2_heap_obj(cr);
      if (hor != NULL &&
          hor->Attr() != ROR_DEF_BY_VARPHI &&  // varphi/vorphi hor doesn't have size cr
          hor->Attr() != ROR_DEF_BY_VORPHI &&
          Comp_unit()->Vra()->Get_bounds(cr, phi_bb, lb, ub)) {
        Is_True(lb != NULL && ub != NULL,
                ("failed to get lb/ub"));
        // swap ub/lb since ub doesn't have offset
        if (ub->Kind() == CK_LDA || ub->Kind() == CK_VAR ||
            ub->Kind() == CK_IVAR) {
          CODEREP *tmp = lb;
          lb = ub;
          ub = tmp;
        }
        // get offset
        CODEREP *ofst_cr = NULL;
        if ((lb->Kind() == CK_LDA || lb->Kind() == CK_VAR ||
             lb->Kind() == CK_IVAR) &&
            Comp_unit()->Vra()->Canonicalize_coderep(ub, lb, ofst_cr) == TRUE) {
          Is_True(ofst_cr != NULL, ("failed to get ofst_cr"));
          if (ofst_cr->Kind() == CK_OP && ofst_cr->Kid_count() == 2) {
            Is_True(ofst_cr->Opnd(0) == lb ||
                    ofst_cr->Opnd(1) == lb, ("lb is not kid 0"));
            ofst_cr = ofst_cr->Opnd(0) == lb ? ofst_cr->Opnd(1)
                                             : ofst_cr->Opnd(0);
          }

          // get size
          CODEREP *size_cr;
          if (hor->Attr() == ROR_DEF_BY_LDA) {
            CODEREP *ho_cr = hor->Heap_obj()->Ho_cr();
            Is_True(ho_cr != NULL && ho_cr->Kind() == CK_LDA,
                    ("ho cr is not LDA"));
            size_cr = Comp_unit()->Vra()->New_cr(MTYPE_I8, ST_size(ho_cr->Lda_base_st()));
          }
          else {
            size_cr = hor->Heap_obj()->Byte_size();
            // remove CVT/CVTL
            if (size_cr && size_cr->Kind() == CK_OP &&
                (size_cr->Opr() == OPR_CVT || size_cr->Opr() == OPR_CVTL))
              size_cr = size_cr->Opnd(0);
          }
          // no size info, do nothing so far
          if (size_cr == NULL)
            return;

          // check offset
          PATH_SELECTED path;
          VRA_RESULT res0 = Comp_unit()->Vra()->Compare_cr<OPR_GE>(ub->Opnd(0), phi_bb,
                                                                   size_cr, path);
          VRA_RESULT res2 = Comp_unit()->Vra()->Compare_cr<OPR_LT>(ub->Opnd(0), phi_bb,
                                                                   size_cr, path);
          if (res0 == VA_YES) {
            // report D AOB
            Classify_aob_error(cr, IC_DEFINITELY, sp_h);
          }
          else if (res0 == VA_POSSIBLE && res2 == VA_POSSIBLE) {
            // report M AOB
            Classify_aob_error(cr, IC_MAYBE, sp_h);
          }
        }
      }
    }

    sp_h->Append_data(phi_bb, Comp_unit()->Dna(), PATHINFO_PHI);
    sp_h->Add_children(phi->Size());
    SRCPOS_TREENODE *cur_node = sp_h->Cur_node();
    sp_h->Path()->Push_mark(phi);

    BB_NODE* pred;
    CODEREP *opnd;
    BB_LIST_ITER bb_iter;
    INT32 pred_idx = 0;
    FOR_ALL_ELEM(pred, bb_iter, Init(phi_bb->Pred())) {
      //not use sp_h->Path() because the BB isn't in correct context in sp_h
      //TODO: improve later
      //if (sp_h->Path()->Is_path_possible(phi, pred_idx) == FALSE)
      if (Is_path_possible(cur_bb, phi, pred_idx) == FALSE)
        continue;
      opnd = phi->OPND(pred_idx);
      sp_h->Set_cur_node(cur_node, pred_idx);
      sp_h->Append_data(pred, Comp_unit()->Dna(), PATHINFO_BRANCH);
      sp_h->Path()->Add_bb(pred);
      VSA_ADDRESS_INFO n_info(info);
      if (tracker) {
        VSYM_TRACKER n_tracker(*tracker);
        Classify_aob_error(opnd, cur_bb, NULL, cs, sp_h, &n_info, visited, &n_tracker);
      }
      else
        Classify_aob_error(opnd, cur_bb, NULL, cs, sp_h, &n_info, visited, NULL);
      ++ pred_idx;
      sp_h->Path()->Pop_mark(phi, FALSE);
    }
    sp_h->Path()->Pop_mark(phi, TRUE);

    return;
  }

  ST *st = NULL;
  if (cr->Kind() == CK_VAR) {
    AUX_ID aux = cr->Aux_id();
    AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux);
    if (sym->St() != NULL)
      st = sym->St();
  }

  if (cr->Kind() == CK_VAR &&
      cr->Is_flag_set(CF_DEF_BY_CHI)) {
    if (cr->Def_at_entry() && st != NULL) {
      BB_NODE* defbb = cr->Defstmt() ? cr->Defstmt()->Bb() : Cfg()->Entry_bb();
      sp_h->Append_data(st, defbb, Comp_unit()->Dna(), PATHINFO_ST_DECLARE);

      switch (ST_sclass(st)) {
      case SCLASS_AUTO:
        // UIV or NPD already reported
        break;
      case SCLASS_FORMAL:
      case SCLASS_FORMAL_REF:
      case SCLASS_REG:
        if (IPSA_insession() &&
            Comp_unit()->Dna()->Clby_list()->size() > VAR_INIT_ID) {
          Comp_unit()->Dna()->Check_callers_argument_for_aob(Ipsa(), cr, cs, sp_h, info, tracker);
        }
#if 0
        else if (info->Index_dna() == Dna()) {
          // don't do this cross function
          VSA_OOB_CHECKER checker(Comp_unit());
          checker.Check_index_oob(info, sp_h);
        }
#endif
        break;
      case SCLASS_PSTATIC:
        // TODO: pstatic?
        break;
      case SCLASS_COMMON:
      case SCLASS_FSTATIC:
      case SCLASS_UGLOBAL:
      case SCLASS_DGLOBAL:
      case SCLASS_EXTERN:
        if (IPSA_insession()) {
          ST_IDX st_idx = ST_st_idx(st);
          DNA_NODE* dna = Comp_unit()->Dna();
          UINT32 file_idx = dna->File_idx();
          if(file_idx !=0 &&
             ST_sclass(st) == SCLASS_EXTERN) {
            WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
            Is_True(mgr != NULL, ("not in xfa mode"));
            mgr->Resolve(file_idx, st_idx, file_idx, st_idx);
          }
          if(dna->Clby_list()->size() > VAR_INIT_ID) {
            dna->Check_callers_global_for_aob(Ipsa(), file_idx, st_idx, cr->Offset(), cs, sp_h, info, tracker);
          }  else if(PU_java_lang(Get_Current_PU())) {
            // no caller, reach to root node
            // check <clinit> method for java static global initialziation 
            DNA_NODE *clinit = Ipsa()->Get_global_clinit_dna(file_idx, st_idx);
            if(clinit != NULL) {
              clinit->Check_clinit_side_effects_for_aob(Ipsa(), file_idx, st_idx, cs, sp_h, info);
            }
          }
        }
        break;
      }
      return;
    }
  }

  STMTREP *def = cr->Get_defstmt();
  if (def != NULL) {
    if (OPERATOR_is_call(def->Opr())) {
      if (def->Callee_returns_new_heap_memory() &&
          cr == Comp_unit()->Find_return_value(def)) {
        sp_h->Append_data(def, Comp_unit()->Dna(), PATHINFO_CALL_CHI);
        if (tracker == NULL) {
          if (!Is_path_possible(def, cs, sp_h))
            return;

          VSA_OOB_CHECKER checker(Comp_unit());
          checker.Check_heapobj_oob(cr, info, cur_bb, cs, sp_h);
        }
        // TODO: multi-level IVAR
      }
      else {
        RNA_NODE *rna = Sr_2_rna(def);
        if (rna != NULL && cr->Kind() == CK_VAR) {
          // check callee, either return value or output parameter
          SRCPOS_TREENODE *cur_node = sp_h->Add_children(rna->Callee_list().size());
          sp_h->Path()->Push_mark(Comp_unit());
          INT i = 0;
          for (CALLEE_VECTOR::const_iterator it = rna->Callee_list().begin();
               it != rna->Callee_list().end(); it++) {
            DNA_NODE* callee = Ipsa()->Get_dna(it->Callee());
            if (callee == NULL || callee->Non_functional())
              continue;
            AUX_STAB_ENTRY* aux = _opt_stab->Aux_stab_entry(cr->Aux_id());
            sp_h->Set_cur_node(cur_node, i);
            sp_h->Append_data(def, Comp_unit()->Dna(), PATHINFO_DNA_CALLRETURN);
            VSA_ADDRESS_INFO n_info(info);
            cs.push(rna);
            INT size = cs.size();
            if (tracker) {
              VSYM_TRACKER n_tracker(*tracker);
              callee->Check_callee_side_effects_for_aob(Ipsa(), cr, cs, sp_h, &n_info, &n_tracker);
            }
            else
              callee->Check_callee_side_effects_for_aob(Ipsa(), cr, cs, sp_h, &n_info, NULL);
            Is_True(cs.size() == size && cs.top() == rna,
                    ("call stack corrupted"));
            cs.pop();
            ++i;
          }
          sp_h->Path()->Pop_mark(Comp_unit(), TRUE);
        }
        // TODO: IVAR
      }
    }
    else if (!cr->Is_flag_set(CF_DEF_BY_CHI)) {
      Is_True(def->Rhs() != NULL, ("def rhs stmt is NULL"));
      if (def->Rhs() != NULL) {
        sp_h->Append_stpath(def, def->Rhs(), Comp_unit()->Dna(), FALSE);
        Classify_aob_error(def->Rhs(), cur_bb, def, cs, sp_h, info, visited, tracker);
      }
    }
  }
}

// =============================================================================
// VSA::Report_aob_error(CODEREP *x, BB_NODE *bb,
//                       INT32 linenum, VAL_RANGE_RESULT rr)
// =============================================================================
void
VSA::Classify_aob_error(CODEREP *x, ISSUE_CERTAINTY ic, SRCPOS_HANDLE* sp_h)
{
  AUX_ID aux = x->Kind() == CK_VAR ? x->Aux_id() :
                   x->Kind() == CK_LDA ? x->Lda_aux_id() : 0;
  if (VSA_Issue_Certainty_Maybe) ic = IC_MAYBE;
  Report_vsa_error(x, aux, AOB, ic, sp_h);
  if (VSA_Xsca) {
    Report_xsca_error(x,
                      sp_h->Orig_stname() ? sp_h->Orig_stname() : aux ? Sym_name(aux) : "",
                      "MISRA_18_1", sp_h);
  }
}


// =============================================================================
// VSA::Perform_oob_analysis(COMP_UNIT* cu)
// =============================================================================
void
VSA::Perform_aob_analysis(COMP_UNIT* cu) {
  BB_NODE *bb;
  CFG_ITER cfg_iter;
  VSA_OOB_CHECKER checker(cu);
  FOR_ALL_ELEM (bb, cfg_iter, Init(_cfg)) {
    STMTREP_ITER stmt_iter(bb->Stmtlist());
    STMTREP *stmt;
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      checker.Check_stmtrep(stmt, bb);
    }
  }
}

