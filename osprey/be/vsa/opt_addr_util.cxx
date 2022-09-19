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
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_main.h"
#include "opt_htable.h"
#include "opt_addr_util.h"
#include "opt_cr_util.h"
#include "opt_vra.h"
#include <vector>

// =============================================================================
//
// PTR_KIND_NAME
//
// =============================================================================
const char*
PTR_KIND_NAME(PTR_KIND kind) {
  static const char* name[] = {
    "unknown", "formal", "heap", "symbol",
    "const", "ivar", "phi"
  };
  Is_True(kind >= PTR_UNKNOWN && kind < PTR_KIND_LAST,
          ("invalid kind"));
  return name[kind];
}

// =============================================================================
//
// Print helper functions
//
// =============================================================================
template<typename _T> static void
Print_vec_item(COMP_UNIT* cu, _T item, FILE *fp);

template<> void
Print_vec_item<CODEREP*>(COMP_UNIT* cu, CODEREP* item, FILE *fp)
{
  if (cu == NULL)
    fprintf(fp, "cr%d", item->Coderep_id());
  else
    cu->Vra()->Print_coderep(item, fp);
}

template<> void
Print_vec_item<std::pair<CODEREP*, CODEREP*> >(COMP_UNIT* cu,
                                               std::pair<CODEREP*, CODEREP*> item,
                                               FILE *fp)
{
  if (cu == NULL)
    fprintf(fp, "cr%d", item.first->Coderep_id());
  else
    cu->Vra()->Print_coderep(item.first, fp);
  fprintf(fp, " * %lld", item.second->Const_val());
}

template<typename _T> static void
Print_cr_vec(COMP_UNIT* cu, const std::vector<_T>& vec, FILE *fp) {
  typename std::vector<_T>::const_iterator it;
  for (it = vec.begin(); it != vec.end(); ++it) {
    if (it != vec.begin())
      fprintf(fp, " + ");
    Print_vec_item(cu, *it, fp);
  }
}

template<typename _T> static void
Print_cr_vec(COMP_UNIT* cu,
            const std::vector<_T>& pos, const std::vector<_T>& neg,
            FILE *fp) {
  if (pos.size() > 0) {
    fprintf(fp, "(");
    Print_cr_vec(cu, pos, fp);
    fprintf(fp, ")");
  }
  if (neg.size() > 0) {
    if (pos.size() > 0)
      fprintf(fp, " - (");
    else
      fprintf(fp, "-(");
    Print_cr_vec(cu, neg, fp);
    fprintf(fp, ")");
  }
}

// =============================================================================
//
// Vector_insert_item
//
// =============================================================================
template<typename _T>
static void
Vector_insert_item(std::vector<_T>& to_add,
                   std::vector<_T>& to_del, _T cr) {
  typename std::vector<_T>::iterator it;
  for (it = to_del.begin(); it != to_del.end(); ++it) {
    if (cr == *it) {
      to_del.erase(it);
      return;
    }
  }
  to_add.push_back(cr);
}

// =============================================================================
//
// VSA_ADDRESS_INFO::Merge
//
// =============================================================================
void
VSA_ADDRESS_INFO::Merge(const VSA_ADDRESS_INFO* info) {
  CR_PAIR_VEC::const_iterator pit;
  INT index_cnt = 0;
  // merge index
  pit = info->Pos_index().begin();
  for (; pit != info->Pos_index().end(); ++pit) {
    Vector_insert_item(_pos_index, _neg_index, *pit);
    ++ index_cnt;
  }
  pit = info->Neg_index().begin();
  for (; pit != info->Neg_index().end(); ++pit) {
    Vector_insert_item(_neg_index, _pos_index, *pit);
    ++ index_cnt;
  }

  // merge variable offset
  CR_VEC::const_iterator cit;
  cit = info->Pos_offset().begin();
  for (; cit != info->Pos_offset().end(); ++cit) {
    Vector_insert_item(_pos_offset, _neg_offset, *cit);
    ++ index_cnt;
  }
  cit = info->Neg_offset().begin();
  for (; cit != info->Neg_offset().end(); ++cit) {
    Vector_insert_item(_neg_offset, _pos_offset, *cit);
    ++ index_cnt;
  }

  // merge fixed offset
  if (info->Fix_ofst() != 0)
    _fix_ofst += info->Fix_ofst();

  // replace old base with new one
  if (info->Base() != NULL) {
    _dna = info->Dna();
    _base = info->Base();
  }

  if (index_cnt > 0) {
    _index_stmt  = info->Index_stmt();
    _index_dna = info->Index_dna();
  }
}

// =============================================================================
//
// VSA_ADDRESS_INFO::Merge
//
// =============================================================================
void
VSA_ADDRESS_INFO::Merge(CODEREP* cr, DNA_NODE* dna)
{
  switch (cr->Kind()) {
  case CK_LDA:
    _dna = dna;
    _base = cr;
    _fix_ofst += cr->Offset();
    break;
  default:
    Is_True(FALSE,
            ("Only LDA is supported so far"));
  }
}

// =============================================================================
//
// VSA_ADDRESS_INFO::Subtract
//   *this -= subtrahend;
// =============================================================================
void
VSA_ADDRESS_INFO::Subtract(const VSA_ADDRESS_INFO& subend) {
  // subtract index
  CR_PAIR_VEC::const_iterator pit;
  pit = subend.Pos_index().begin();
  for (; pit != subend.Pos_index().end(); ++pit) {
    Vector_insert_item(_neg_index, _pos_index, *pit);
  }
  pit = subend.Neg_index().begin();
  for (; pit != subend.Neg_index().end(); ++pit) {
    Vector_insert_item(_pos_index, _neg_index, *pit);
  }
  // subtract variable offset
  CR_VEC::const_iterator cit;
  cit = subend.Pos_offset().begin();
  for (; cit != subend.Pos_offset().end(); ++cit) {
    Vector_insert_item(_neg_offset, _pos_offset, *cit);
  }
  cit = subend.Neg_offset().begin();
  for (; cit != subend.Neg_offset().end(); ++cit) {
    Vector_insert_item(_pos_offset, _neg_offset, *cit);
  }
  // subtract fixed ofst
  _fix_ofst -= subend._fix_ofst;
}

// =============================================================================
//
// VSA_ADDRESS_INFO::Print
//
// =============================================================================
void
VSA_ADDRESS_INFO::Print(COMP_UNIT* cu, FILE *fp) const {
  BOOL plus = FALSE;
  fprintf(fp, "+kind = %s, address = ", PTR_KIND_NAME(_kind));
  if (_base != NULL) {
    if (cu == NULL)
      fprintf(fp, "cr%d", _base->Coderep_id());
    else
      cu->Vra()->Print_coderep(_base, fp);
    plus = TRUE;
  }
  if (_pos_index.size() > 0 || _neg_index.size() > 0) {
    if (plus)
      fprintf(fp, " + ");
    else
      plus = TRUE;
    Print_cr_vec(cu, _pos_index, _neg_index, fp);
  }
  if (_pos_offset.size() > 0 || _neg_offset.size() > 0) {
    if (plus)
      fprintf(fp, " + ");
    else
      plus = TRUE;
    Print_cr_vec(cu, _pos_offset, _neg_offset, fp);
  }
  if (_fix_ofst != 0) {
    if (plus)
      fprintf(fp, " %c ", _fix_ofst > 0 ? '+' : '-');
    fprintf(fp, "%lld", _fix_ofst > 0 ? _fix_ofst : - _fix_ofst);
  }
  fprintf(fp, "\n");
}

// =============================================================================
// VSA_ACCESS_INFO
// =============================================================================
BOOL
VSA_ACCESS_INFO::Subtract(const VSA_ACCESS_INFO& rhs)
{
  if (_lb_val == INT64_MAX || _ub_val == INT64_MAX)
    return TRUE;

  INT64 rhs_lb = rhs.Lower_bound_value();
  if (rhs_lb == INT64_MAX)
    return TRUE;
  INT64 rhs_ub = rhs.Upper_bound_value();
  if (rhs_ub == INT64_MAX)
    return TRUE;

  BOOL ret = FALSE;
  if (_lb_val < rhs_lb) {
    _ub_val = rhs_lb;
    ret = TRUE;
  }
  else if (_ub_val > rhs_ub) {
    _lb_val = rhs_ub;
    ret = TRUE;
  }
  return ret;
}

void
VSA_ACCESS_INFO::Print(FILE *fp) const
{
}

// =============================================================================
//
// VSA Pointer information builder
//  to analysis coderep to get its base, index, scale
//  and offset
//
// =============================================================================
class VSA_ADDRINFO_BUILDER {
private:
  typedef std::vector<CODEREP *> CR_VEC;
  typedef std::vector<std::pair<CODEREP*, CODEREP*> > CR_PAIR_VEC;

private:
  COMP_UNIT *_comp_unit;
  STMTREP   *_use_stmt;   // where the cr is used
  CODEREP   *_base;       // temporary base
  CR_PAIR_VEC _pos_index; // temporary positive index
  CR_PAIR_VEC _neg_index; // temporary negtive index
  CR_VEC     _pos_offset; // temporary positive index
  CR_VEC     _neg_offset; // temporary negtive index
  INT64      _fix_offset; // temporary fixed offset
  PTR_KIND   _kind;       // temporary pointer kind
  BOOL       _check_ud;   // should base's UD been traversed

private:
  // clean all temporary data
  void Initialize();

  // check if cr used in control dependency
  BOOL Is_cr_in_control_dependency(CODEREP* cr, BB_NODE* def);

  // adjust val according to type and add to _fix_offset
  void Add_fix_offset(TYPE_ID type, INT64 val);
  // adjust val according to type and subtract from _fix_offset
  void Sub_fix_offset(TYPE_ID type, INT64 val);

  // check the base from an OP cr
  CODEREP* Analyze_op_base(CODEREP* cr);
  // check and add cr to offset
  void Analyze_offset(CODEREP* cr, BOOL sub);
  // check and add cr to index
  void Analyze_index(CODEREP* cr, CODEREP* scale, BOOL sub);
  // check if cr is scale * index
  void Analyze_scale_index(CODEREP* cr, BOOL sub);
  // check if cr is valid address expression
  void Analyze_op_address(CODEREP* cr);
  // check if cr is valid pointer variable
  void Analyze_var_address(CODEREP* cr);
  // analyze coderep. addr indicates if cr can be address expression or not
  void Analyze_coderep(CODEREP* cr, BOOL addr);

  CODEREP* Vector_to_cr(const CR_VEC& to_add, const CR_VEC& to_sub) const;
  // copy temporary address info to final info
  void Generate_ptr_info(CODEREP* cr, VSA_ADDRESS_INFO* info);

  // copy temporary address info to final access info
  BOOL Generate_access_info(CODEREP* cr, VSA_ACCESS_INFO* info);

public:
  VSA_ADDRINFO_BUILDER(COMP_UNIT* cu, STMTREP* use, BOOL check_ud)
    : _comp_unit(cu), _use_stmt(use), _check_ud(check_ud)
  {
  }

  // analyze IVAR cr and keep result in info
  BOOL Analyze_address(CODEREP* cr, VSA_ADDRESS_INFO* info, BOOL store) {
    Is_True(cr->Kind() == CK_IVAR &&
            ((store == FALSE && cr->Ilod_base() != NULL) ||
             (store == TRUE  && cr->Istr_base() != NULL)),
            ("only handle IVAR"));
    // clean all temporary data
    Initialize();

    // analyze iload/istr base of the ivar to get address information
    CODEREP* base = store ? cr->Istr_base() : cr->Ilod_base();
    Analyze_coderep(base, TRUE);
    // adjust fixed offset
    if (cr->Opr() != OPR_PARM) {
      Add_fix_offset(MTYPE_I8, cr->Offset());
      if(cr->Kind() == CK_IVAR && cr->I_field_id() != 0) {
        info->Set_fld_id(cr->I_field_id());
      }
    }
    if (_kind == PTR_UNKNOWN)
      return FALSE;

    // fill result into ptr info
    Generate_ptr_info(cr, info);

    // set ILOAD/ISTORE mtype
    info->Set_mtype(cr->Dsctyp());
    return TRUE;
  }

  // analyze address CR and keep result in info
  // input CR can be ilod/istr base of IVAR or any address expressions or
  // pointer variables
  BOOL Analyze_pointer(CODEREP* cr, VSA_ADDRESS_INFO* info) {
    // clean all temporary data
    Initialize();

    // analyze cr to get the address information
    Analyze_coderep(cr, TRUE);
    if (_kind == PTR_UNKNOWN)
      return FALSE;

    // fill result into ptr info
    Generate_ptr_info(cr, info);
    return TRUE;
  }

  // analyze CR and return the base CR
  CODEREP* Analyze_base(CODEREP* cr) {
    // analyze cr to get the base information
    switch (cr->Kind()) {
    case CK_LDA:
      return cr;
    case CK_VAR:
    case CK_IVAR:
    {
      TY_IDX tyi = cr->object_ty();
      if (TY_kind(tyi) == KIND_POINTER) {
        return cr;
      }
      else if (TY_kind(tyi) == KIND_SCALAR && cr->Kind() == CK_VAR) {
        AUX_STAB_ENTRY *sym = _comp_unit->Opt_stab()->Aux_stab_entry(cr->Aux_id());
        if (sym->Is_preg() && cr->Dtyp() == Pointer_Mtype)
          return cr;
      } else if (TY_kind(tyi) == KIND_SCALAR && cr->Kind() == CK_IVAR) {
        return Analyze_base(cr->Ilod_base());
      }
      return NULL;
    }
    case CK_OP:
      if (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB ||
          cr->Opr() == OPR_BAND || cr->Opr() == OPR_BIOR) {
        return Analyze_op_base(cr);
      }
      if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_CVTL) {
        return Analyze_base(cr->Opnd(0));
      }
      return NULL;
    default:
      return NULL;
    }
  }

  BOOL Analyze_access(CODEREP *cr, VSA_ACCESS_INFO *info, BOOL store) {
    Is_True(cr->Kind() == CK_IVAR &&
            cr->Opr() != OPR_PARM &&
            ((store == FALSE && cr->Ilod_base() != NULL) ||
             (store == TRUE  && cr->Istr_base() != NULL)),
            ("only handle IVAR"));

    // clean all temporary data
    Initialize();

    // analyze iload/istr base of the ivar to get address information
    CODEREP* base = store ? cr->Istr_base() : cr->Ilod_base();
    Analyze_coderep(base, TRUE);
    // adjust fixed offset
    if (cr->Opr() != OPR_PARM) {
      Add_fix_offset(MTYPE_I8, cr->Offset());
    }
    if (_kind == PTR_UNKNOWN)
      return FALSE;
    if (_pos_index.size() != 1 ||
        _neg_index.size() != 0)
      return FALSE;
    if (_pos_offset.size() + _neg_offset.size() != 0)
      return FALSE;

    // find out lb/ub/stride/etc and fill into VSA_ACCESS_INFO
    info->Set_store(store);
    return Generate_access_info(cr, info);
  }

  void Print(FILE* fp = NULL) const;
};

void
VSA_ADDRINFO_BUILDER::Initialize() {
  _base = NULL;
  _pos_index.clear();
  _neg_index.clear();
  _pos_offset.clear();
  _neg_offset.clear();
  _fix_offset = 0;
  _kind = PTR_UNKNOWN;
}

BOOL
VSA_ADDRINFO_BUILDER::Is_cr_in_control_dependency(CODEREP* cr, BB_NODE* def)
{
  Is_True(_use_stmt != NULL,
          ("use stmt is not set"));
  BB_NODE* use_bb = _use_stmt->Bb();
  BB_NODE* cd;
  BB_NODE_SET_ITER cd_iter;
  FOR_ALL_ELEM(cd, cd_iter, Init(use_bb->Rcfg_dom_frontier())) {
    Is_True(cd->Succ() != NULL,
            ("cd bb does not have successors"));
    if (!cd->Succ()->Multiple_bbs())
      continue;
    if (def->Dominates(cd)) {
      STMTREP* sr = cd->Last_stmtrep();
      OPERATOR opr = sr->Opr();
      if (opr != OPR_TRUEBR && opr != OPR_FALSEBR &&
          opr != OPR_COMPGOTO)
        continue;
      if (sr->Rhs()->Contains(cr))
        return TRUE;
    }
  }
  return FALSE;
}

void
VSA_ADDRINFO_BUILDER::Add_fix_offset(TYPE_ID type, INT64 val) {
  switch (type) {
  case MTYPE_I4:
    _fix_offset += (INT32)val;  return;
  case MTYPE_U4:
    _fix_offset += (UINT32)val; return;
  case MTYPE_I8:
  case MTYPE_U8:
    _fix_offset += val; return;
  default:
    Is_True(FALSE, ("TODO: handle dtype %d", type));
  }
}

void
VSA_ADDRINFO_BUILDER::Sub_fix_offset(TYPE_ID type, INT64 val) {
  switch (type) {
  case MTYPE_I4:
    _fix_offset -= (INT32)val;  return;
  case MTYPE_U4:
    _fix_offset -= (UINT32)val; return;
  case MTYPE_I8:
  case MTYPE_U8:
    _fix_offset -= val; return;
  default:
    Is_True(FALSE, ("TODO: handle dtype %d", type));
  }
}

CODEREP*
VSA_ADDRINFO_BUILDER::Analyze_op_base(CODEREP* cr) {
  Is_True(cr->Kind() == CK_OP &&
          (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB ||
           cr->Opr() == OPR_BAND || cr->Opr() == OPR_BIOR),
          ("cr not ADD or SUB"));
  CODEREP* opr0 = Analyze_base(cr->Opnd(0));
  if (cr->Opr() == OPR_SUB)
    return opr0;

  CODEREP* opr1 = Analyze_base(cr->Opnd(1));
  if (opr0 &&
      (opr0->Kind() == CK_LDA || TY_kind(opr0->object_ty()) == KIND_POINTER))
    return opr0;
  else if (opr1 &&
      (opr1->Kind() == CK_LDA || TY_kind(opr1->object_ty()) == KIND_POINTER))
    return opr1;
  else if (opr0)
    return opr0;
  else if (opr1)
    return opr1;
  return NULL;
}

void
VSA_ADDRINFO_BUILDER::Analyze_offset(CODEREP* cr, BOOL sub) {
  if (cr->Kind() == CK_CONST) {
    if (sub)
      Sub_fix_offset(cr->Dtyp(), cr->Const_val());
    else
      Add_fix_offset(cr->Dtyp(), cr->Const_val());
    return;
  }

  // try to flatten the tree
  if (cr->Kind() == CK_OP) {
    if (cr->Opr() == OPR_CVT) {
      Analyze_offset(cr->Opnd(0), sub);  // can CVT be skipped???
      return;
    }
    else if (cr->Opr() == OPR_NEG) {
      Analyze_offset(cr->Opnd(0), !sub);
      return;
    }
    else if (cr->Opr() == OPR_ADD) {
      Analyze_offset(cr->Opnd(0), sub);
      Analyze_offset(cr->Opnd(1), sub);
      return;
    }
    else if (cr->Opr() == OPR_SUB) {
      Analyze_offset(cr->Opnd(0), sub);
      Analyze_offset(cr->Opnd(1), !sub);
      return;
    }
  }

  if (sub)
    Vector_insert_item(_neg_offset, _pos_offset, cr);
  else
    Vector_insert_item(_pos_offset, _neg_offset, cr);
}

void
VSA_ADDRINFO_BUILDER::Analyze_index(CODEREP* cr, CODEREP* scale, BOOL sub) {
  // try to flatten the tree
  if (cr->Kind() == CK_OP) {
    if (cr->Opr() == OPR_CVT) {
      Analyze_index(cr->Opnd(0), scale, sub);  // can CVT be skipped???
      return;
    }
    else if (cr->Opr() == OPR_NEG) {
      Analyze_index(cr->Opnd(0), scale, !sub);
      return;
    }
    else if (cr->Opr() == OPR_ADD) {
      Analyze_index(cr->Opnd(0), scale, sub);
      Analyze_index(cr->Opnd(1), scale, sub);
      return;
    }
    else if (cr->Opr() == OPR_SUB) {
      Analyze_index(cr->Opnd(0), scale, sub);
      Analyze_index(cr->Opnd(1), scale, !sub);
      return;
    }
  }

  std::pair<CODEREP*, CODEREP*> val = std::make_pair(cr, scale);
  if (sub)
    Vector_insert_item(_neg_index, _pos_index, val);
  else
    Vector_insert_item(_pos_index, _neg_index, val);
}

void
VSA_ADDRINFO_BUILDER::Analyze_scale_index(CODEREP* cr, BOOL sub) {
  Is_True(cr->Kind() == CK_OP,
          ("not op cr"));
  if (cr->Opr() != OPR_MPY) {
    Analyze_offset(cr, sub);
    return;
  }

  CODEREP* op0 = cr->Opnd(0);
  CODEREP* op1 = cr->Opnd(1);
  CODEREP* scale = (op0->Kind() == CK_CONST) ? op0 : op1;
  CODEREP* index = (op0->Kind() == CK_CONST) ? op1 : op0;
  if (scale != NULL && scale->Kind() == CK_CONST)
    Analyze_index(index, scale, sub);
  else
    Analyze_offset(cr, sub);
}

void
VSA_ADDRINFO_BUILDER::Analyze_op_address(CODEREP* cr) {
  Is_True(cr->Kind() == CK_OP &&
          (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB),
          ("cr not ADD or SUB"));
  CODEREP* op0 = cr->Opnd(0);
  CODEREP* op1 = cr->Opnd(1);
  if (cr->Opr() == OPR_ADD && op1->Kind() == CK_LDA) {
    // CODEMAP may exchange operands of OPR_ADD
    // if op1 is LDA, make it the base
    Analyze_coderep(op1, TRUE);
    Analyze_coderep(op0, FALSE);
    return;
  }
  if (cr->Opr() == OPR_SUB &&
      (op0->Kind() == CK_LDA ||
       op0->Kind() == CK_VAR && TY_kind(op0->object_ty()) == KIND_POINTER) &&
      (op1->Kind() == CK_LDA ||
       op1->Kind() == CK_VAR && TY_kind(op1->object_ty()) == KIND_POINTER)) {
    // (ptr - ptr) generates an offset
    Analyze_offset(op0, FALSE);
    Analyze_offset(op1, TRUE);
    return;
  }
  if (op0->Kind() == CK_OP && op0->Opr() == OPR_MPY)
    Analyze_scale_index(op0, FALSE);
  else
    Analyze_coderep(op0, TRUE);

  if (op1->Kind() == CK_OP && op1->Opr() == OPR_MPY)
    Analyze_scale_index(op1, cr->Opr() == OPR_SUB);
  else if (cr->Opr() == OPR_ADD)
    Analyze_coderep(op1, _kind == PTR_UNKNOWN);
  else
    Analyze_offset(op1, cr->Opr() == OPR_SUB);
}

void
VSA_ADDRINFO_BUILDER::Analyze_var_address(CODEREP* cr) {
  // follow U-D of the var to figure out where the cr is initialy assigned
  // stop when reach to assignment from entry, LDA, malloc call or PHI
  Is_True(_kind == PTR_UNKNOWN && _base == NULL,
          ("ptr info already set"));
  Is_True(cr->Kind() == CK_VAR,
          ("only VAR allows here"));
  if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
    _kind = PTR_PHI;
    _base = cr;
  }
  else if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP* sr = cr->Defstmt();
    Is_True(sr != NULL,
            ("no defstmt for cr DEF_BY_CHI"));
    if (sr->Opr() == OPR_CALL && sr->Callee_returns_new_heap_memory()) {
      _kind = PTR_HEAP;
      _base = cr;
    }
    else if (OPERATOR_is_call(sr->Opr())) {
      _kind = PTR_RETURN;
      _base = cr;
    }
    else if (sr->Opr() == OPR_OPT_CHI) {
      // TODO: check if cr is uninitialized local symbol
      _kind = PTR_FORMAL;
      _base = cr;
    }
    else {
      _kind = PTR_VAR_CR;
      _base = cr;
    }
  }
  else if (cr->Is_var_volatile()) {
    _kind = PTR_FORMAL;
    _base = cr;
  }
  else if (_check_ud) {
    STMTREP* sr = cr->Defstmt();
    Is_True(sr != NULL && sr->Rhs() != NULL,
            ("No sr or no rhs in sr"));
    if (!Is_cr_in_control_dependency(cr, sr->Bb()))
      Analyze_coderep(sr->Rhs(), TRUE);
  }
  else {
    _kind = PTR_VAR_CR;
    _base = cr;
  }
}

void
VSA_ADDRINFO_BUILDER::Analyze_coderep(CODEREP* cr, BOOL addr) {
  AUX_STAB_ENTRY* aux;
  switch (cr->Kind()) {
  case CK_CONST:
    if (addr && _kind == PTR_UNKNOWN) {
      //Is_True(_kind == PTR_UNKNOWN && _fix_offset == 0,
      //        ("ptr info already set"));
      _kind = PTR_CONST;
    }
    Add_fix_offset(cr->Dtyp(), cr->Const_val());
    break;

  case CK_RCONST:
    // Is_True(FALSE, ("TODO: handle RCONST"));
    break;

  case CK_LDA:
    Is_True(addr == TRUE,
            ("unexpected LDA in offset expression"));
    Is_True(_kind == PTR_UNKNOWN && _base == NULL,
            ("ptr info already set"));
    _kind = PTR_ST;
    _base = cr;
    Add_fix_offset(MTYPE_I8, cr->Offset());
    break;

  case CK_VAR:
    aux = _comp_unit->Opt_stab()->Aux_stab_entry(cr->Aux_id());
    if (addr == TRUE &&
        (TY_kind(cr->object_ty()) == KIND_POINTER ||
         (aux->Is_preg() && cr->Dtyp() == Pointer_Mtype))) {
      Is_True(_kind == PTR_UNKNOWN && _base == NULL,
              ("ptr kind already set"));
      Analyze_var_address(cr);
    }
    else {
      Analyze_offset(cr, FALSE);
    }
    //if (! aux->Is_preg()) {
    //  Add_fix_offset(MTYPE_I8, cr->Offset());
    //}
    break;

  case CK_IVAR:
    if (addr == TRUE &&
        TY_kind(cr->object_ty()) == KIND_POINTER) {
      Is_True(_kind == PTR_UNKNOWN && _base == NULL,
              ("ptr kind already set"));
      _kind = PTR_IVAR;
      _base = cr;
    }
    else {
      Analyze_offset(cr, FALSE);
    }
    //Add_fix_offset(MTYPE_I8, cr->Offset());
    break;

  case CK_OP:
    if (addr == TRUE &&
        (cr->Opr() == OPR_ADD || cr->Opr() == OPR_SUB)) {
      Analyze_op_address(cr);
    }
    else if (cr->Opr() == OPR_CVT) {
      Analyze_coderep(cr->Opnd(0), _base == NULL);  // skip CVT ???
    }
    else {
      Analyze_scale_index(cr, FALSE);
    }
    break;

  default:
    Is_True(FALSE, ("unknown kind %d", cr->Kind()));
  }
}

CODEREP*
VSA_ADDRINFO_BUILDER::Vector_to_cr(const CR_VEC& to_add, const CR_VEC& to_sub) const {
  CODEREP* res = NULL;
  CR_VEC::const_iterator it;
  for (it = to_add.begin(); it != to_add.end(); ++it) {
    if (res == NULL)
      res = *it;
    else
      res = _comp_unit->Vra()->New_cr(OPCODE_make_op(OPR_ADD,
                                                     res->Dtyp(),
                                                     MTYPE_V),
                                      res, *it, TRUE);
  }
  for (it = to_sub.begin(); it != to_sub.end(); ++it) {
    if (res == NULL)
      res = _comp_unit->Vra()->New_unary_cr(OPCODE_make_op(OPR_NEG,
                                                     (*it)->Dtyp(),
                                                     MTYPE_V),
                                      *it, TRUE);
    else
      res = _comp_unit->Vra()->New_cr(OPCODE_make_op(OPR_SUB,
                                                     res->Dtyp(),
                                                     MTYPE_V),
                                      res, *it, TRUE);
  }
  return res;
}

void
VSA_ADDRINFO_BUILDER::Generate_ptr_info(CODEREP* cr, VSA_ADDRESS_INFO* info) {
  info->Set_stmt(_use_stmt);
  info->Set_dna(_comp_unit->Dna());
  info->Set_index_stmt(_use_stmt);
  info->Set_index_dna(_comp_unit->Dna());
  info->Set_base(_base);
  info->Set_index(_pos_index, _neg_index);
  info->Set_var_ofst(_pos_offset, _neg_offset);
  info->Set_fix_ofst(_fix_offset);
  info->Set_kind(_kind);
}

BOOL
VSA_ADDRINFO_BUILDER::Generate_access_info(CODEREP* cr, VSA_ACCESS_INFO* info) {
  Is_True(_pos_index.size() == 1, ("pos index count is not 1"));

  BB_NODE *idom = _use_stmt->Bb()->Idom();
  Is_True(idom != NULL, ("bb idom is NULL"));
  BB_LOOP *loop = idom->Loop();
  if (loop == NULL || loop->Iv() == NULL) {
    return FALSE;
  }

  CODEREP *iv = loop->Iv();
  CODEREP *index = _pos_index.front().first;
  CODEREP *scale = _pos_index.front().second;
  if (index != iv && !index->Contains(iv)) {
    return FALSE;
  }

  if (!iv->Is_flag_set(CF_DEF_BY_PHI)) {
    return FALSE;
  }

  PHI_NODE *phi = iv->Defphi();
  Is_True(phi != NULL, ("invalid phi"));
  if (phi->Size() != 2) {
    return FALSE;
  }

  CODEREP *step = NULL;
  CODEREP *init = NULL;
  for (INT i = 0; i < phi->Size(); ++i) {
    CODEREP *opnd = phi->OPND(i);
    if (opnd->Is_flag_set(CF_DEF_BY_PHI) ||
        opnd->Is_flag_set(CF_DEF_BY_CHI)) {
      return FALSE;
    }
    STMTREP *def = opnd->Defstmt();
    if (def->Bb() == loop->Step()) {
      // step
      step = def->Rhs();
    }
    else {
      init = def->Rhs();
    }
  }

  if (step == NULL || init == NULL) {
    return FALSE;
  }

  if (step->Opr() != OPR_ADD && step->Opr() != OPR_SUB) {
    return FALSE;
  }

  CODEREP *lb = NULL;
  CODEREP *ub = NULL;
  CODEREP *stride = NULL;
  if (step->Opnd(0)->Kind() == CK_VAR &&
      step->Opnd(0)->Aux_id() == iv->Aux_id()) {
    stride = step->Opnd(1);
  }
  else if (step->Opnd(1)->Kind() == CK_VAR &&
           step->Opnd(1)->Aux_id() == iv->Aux_id()) {
    stride = step->Opnd(0);
  }
  else {
    return FALSE;
  }

  STMTREP* sr = loop->Trip_count_stmt();
  CODEREP* tc = sr ? sr->Rhs() : loop->Trip_count_expr();
  if (tc == NULL) {
    return FALSE;
  }

  if (step->Opr() == OPR_ADD) {
    lb = init;
    OPCODE opc = OPCODE_make_op(OPR_ADD, step->Dtyp(), MTYPE_V);
    ub = CR_UTIL(_comp_unit).New_binary_cr(opc, init, tc, TRUE);
  }
  else {
    OPCODE opc = OPCODE_make_op(OPR_SUB, step->Dtyp(), MTYPE_V);
    lb = CR_UTIL(_comp_unit).New_binary_cr(opc, init, tc, TRUE);
    ub = init;
  }

  info->Set_index(index);
  info->Set_scale(scale);
  info->Set_lower_bound(lb);
  info->Set_upper_bound(ub);
  info->Set_stride(stride);
  info->Set_fix_ofst(_fix_offset);
  info->Set_elem_width(MTYPE_byte_size(cr->Dsctyp()));
  info->Set_exit_early(loop->Exit_early());
  return TRUE;
}

void
VSA_ADDRINFO_BUILDER::Print(FILE* fp) const {
  fprintf(fp, "+kind = %s\n", PTR_KIND_NAME(_kind));
  if (_base != NULL) {
    fprintf(fp, " -base = ");
    _comp_unit->Vra()->Print_coderep(_base, fp);
    fprintf(fp, "\n");
  }
  if (_pos_index.size() > 0 || _neg_index.size() > 0) {
    fprintf(fp, " -index = ");
    Print_cr_vec(_comp_unit, _pos_index, _neg_index, fp);
    fprintf(fp, "\n");
  }
  if (_pos_offset.size() > 0 || _neg_offset.size() > 0) {
    fprintf(fp, " -var_ofst = ");
    Print_cr_vec(_comp_unit, _pos_offset, _neg_offset, fp);
    fprintf(fp, "\n");
  }
  if (_fix_offset != 0)
    fprintf(fp, " -fix_ofst = %lld\n", _fix_offset);
}

// =============================================================================
//
// COMP_UNIT::Analyze_base_info
//
// =============================================================================
CODEREP*
COMP_UNIT::Analyze_base_info(STMTREP* use, CODEREP* cr, BOOL check_ud) {
  VSA_ADDRINFO_BUILDER bldr(this, use, check_ud);
  return bldr.Analyze_base(cr);
}

// =============================================================================
//
// COMP_UNIT::Analyze_pointer_info
//
// =============================================================================
BOOL
COMP_UNIT::Analyze_pointer_info(STMTREP* use, CODEREP* cr, VSA_ADDRESS_INFO* info, BOOL check_ud) {
  VSA_ADDRINFO_BUILDER bldr(this, use, check_ud);
  return bldr.Analyze_pointer(cr, info);
}

// =============================================================================
//
// COMP_UNIT::Analyze_address_info
//
// =============================================================================
BOOL
COMP_UNIT::Analyze_address_info(STMTREP* use, CODEREP* cr, VSA_ADDRESS_INFO* info, BOOL store, BOOL check_ud) {
  Is_True(cr->Kind() == CK_IVAR &&
          ((store == FALSE && cr->Ilod_base() != NULL) ||
           (store == TRUE && cr->Istr_base() != NULL)),
          ("Not an IVAR cr"));
  VSA_ADDRINFO_BUILDER bldr(this, use, check_ud);
  return bldr.Analyze_address(cr, info, store);
}

// =============================================================================
//
// COMP_UNIT::Analyze_access_info
//
// =============================================================================
BOOL
COMP_UNIT::Analyze_access_info(STMTREP* use, CODEREP* cr, VSA_ACCESS_INFO* info) {
  Is_True(cr->Kind() == CK_IVAR && cr->Opr() != OPR_PARM,
          ("Not an IVAR cr"));
  VSA_ADDRINFO_BUILDER bldr(this, use, FALSE);
  return bldr.Analyze_access(cr, info, use->Lhs() == cr);
}

