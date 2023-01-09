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

//==============================================================================
// opt_vsa_format.cxx
//
// check issues in scanf/printf format string
//
//==============================================================================

#include "defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vra.h"
#include "opt_vsa.h"
#include "opt_vsa_util.h"
#include "report.h"
#include <vector>
#include "opt_vsa_rbc.h"
#include "opt_vsa_report.h"
#include "builtin_rule_defs.h"
#include "config_vsa.h"
#include "pro_core.h"

class FORMAT_PARSER_CONTEXT {
private:
  MEM_POOL    _loc_pool;
  DNA_NODE   *_caller;
  RNA_NODE   *_callsite;
  INT         _buf_arg;   // -1 means write to file
  INT         _len_arg;   // -1 means no buffer length
  INT         _fmt_arg;   // index to format argument
  INT         _v_arg;     // index to first va_arg
  std::vector<CODEREP*> _var_len;    // variable length

public:
  FORMAT_PARSER_CONTEXT(DNA_NODE *caller, RNA_NODE *callsite,
                        INT buf_arg, INT len_arg, INT fmt_arg, INT v_arg)
    : _caller(caller), _callsite(callsite),
      _buf_arg(buf_arg), _len_arg(len_arg), _fmt_arg(fmt_arg), _v_arg(v_arg)
  {
    OPT_POOL_Initialize(&_loc_pool, "format context pool", FALSE, VSA_DUMP_FLAG);
    OPT_POOL_Push(&_loc_pool, VSA_DUMP_FLAG);
  }
  ~FORMAT_PARSER_CONTEXT()
  {
    OPT_POOL_Pop(&_loc_pool, VSA_DUMP_FLAG);
    OPT_POOL_Delete(&_loc_pool, VSA_DUMP_FLAG);
  }

private:
  TYPE_ID     Get_lda_mtype(CODEREP *cr, BOOL array) const;
  const char* Get_const_string(STMTREP* sr, CODEREP* cr) const;
  INT         Get_const_format_length(INT64 value, INT short_cnt, INT long_cnt, char fmt, INT sharp) const;

public:
  MEM_POOL*   Loc_pool() { return &_loc_pool; }
  DNA_NODE*   Dna() const{ return _caller; }
  void        Initialize(const char* fmt);
  void        Finalize(INT arg, INT len);
  CODEREP*    Get_arg(INT arg) const;
  std::pair<CODEREP*, RNA_NODE*>
              Get_object_length(CODEREP* cr) const;
  INT         Get_string_length(CODEREP* cr);
  INT         Get_arg_length(INT arg, INT& short_cnt, INT& long_cnt, char fmt, INT sharp, INT& length) const;
  const char* Get_format_string() const;
  INT         Add(INT a, CODEREP* b);
  void        Annotate_malloc(INT arg) const;

public:
  void        Check_type(INT n_arg, CODEREP* cr, INT s_cnt, INT l_cnt, char fmt);
  void        Check_npd(CODEREP* cr);
  void        Check_uaf(CODEREP* cr);
  void        Check_object_size(CODEREP* cr, INT size);
  void        Check_unlimit_string(INT n_arg, CODEREP* cr);
  void        Report_error(const char* var, const char* msg);
  void        Report_error(CODEREP* cr, UINT32 anat);
};


void
FORMAT_PARSER_CONTEXT::Initialize(const char* fmt)
{
  // Do nothing
}

void
FORMAT_PARSER_CONTEXT::Finalize(INT arg, INT len)
{
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  CODEREP* buf_cr = NULL;
  CODEREP* buf_len = NULL;
  CODEREP* len_cr = NULL;
  RNA_NODE *buf_rna = NULL;
  RNA_NODE *len_rna = NULL;

  if (VSA_Fmt &&
      (arg + _v_arg - 1 > _callsite->Arg_cnt())) {
    STMTREP* callstmt = _callsite->Callstmt();
    CODEREP* fmt_cr = _callsite->Get_arg(_fmt_arg);
    SRCPOS_HANDLE srcpos_h(fmt_cr, callstmt, _caller, Loc_pool());
    const char* call_name = callstmt->Opr() == OPR_CALL ? ST_name(callstmt->St()) : "<printf>";
    srcpos_h.Add_message("The format item %d is missing in calling %s", arg, call_name);
    UINT32 cost = Get_builtin_rule_fix_cost(FMT);
    vsa->Report_vsa_error(NULL, (char*)NULL, "FMT", cost, IC_DEFINITELY, &srcpos_h);
    //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_MISSING_FORMAT_PARAM, FALSE, &srcpos_h);
  }

  if (!VSA_Aob)  // do nothing if AOB is off
    return;

  if (_buf_arg != -1) {
    buf_cr = _callsite->Get_arg(_buf_arg);
    Is_True(buf_cr != NULL,
            ("can not find buf parm at %d", _buf_arg));
    std::pair<CODEREP*, RNA_NODE*> buf_ret = Get_object_length(buf_cr);
    buf_len = buf_ret.first;
    buf_rna = buf_ret.second;
  }

  if (_len_arg != -1) {
    len_cr = _callsite->Get_arg(_len_arg);
    Is_True(len_cr != NULL,
            ("can not find len parm at %d", _len_arg));
  }

  if (buf_cr == NULL && len_cr == NULL)    // printf/scanf
    return;

  STMTREP* callstmt = _callsite->Callstmt();
  SRCPOS spos = callstmt->Linenum();
  if (buf_cr != NULL && len_cr == NULL) { // sprintf
    INT   var_buf_cnt = 0;
    INT64 fix_buf_len = 0;
    if (buf_len != NULL) {
      if (buf_len->Kind() == CK_CONST)
        fix_buf_len = buf_len->Const_val();
      else if (buf_len->Kind() == CK_VAR)
        var_buf_cnt = 1;
      else if (buf_len->Kind() == CK_OP && buf_len->Opr() == OPR_ADD) {
        var_buf_cnt = 1;
        CODEREP* opnd = buf_len->Opnd(1);
        while (opnd->Kind() == CK_OP && opnd->Opr() == OPR_ADD) {
          ++ var_buf_cnt;
          opnd = opnd->Opnd(1);
        }
        if (opnd->Kind() == CK_CONST)
          fix_buf_len = opnd->Const_val();
      }
    }
    if (fix_buf_len <= len) {
      //fprintf(stderr, "%d:%d sprintf out-of-bound\n",
      //                SRCPOS_filenum(spos), SRCPOS_linenum(spos));
      SRCPOS_HANDLE srcpos_h(buf_cr, callstmt, _caller, Loc_pool());
      const char* call_name = callstmt->Opr() == OPR_CALL ? ST_name(callstmt->St()) : "<sprintf>";
      const char* buf_desc = NULL;
      if (buf_cr) {
        buf_desc = srcpos_h.Find_cr_stname(buf_cr, callstmt, Dna());
        if (!buf_desc &&
            callstmt->Prev() &&
            OPERATOR_is_store(callstmt->Prev()->Opr()) &&
            callstmt->Prev()->Rhs() == buf_cr) {
          // if no desc for this cr, try search it's prev stmt which probably contains the same value
          buf_desc = srcpos_h.Find_cr_stname(callstmt->Prev()->Lhs(), callstmt->Prev(), Dna(), FALSE);
        }
      }
      srcpos_h.Add_message("%s writes %d bytes to buffer which is %d bytes",
                           call_name, len + 1/* for last '\0' */, fix_buf_len);
      //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_SPRINTF_OVERFLOW, FALSE, &srcpos_h);
      ISSUE_CERTAINTY ic = (!VSA_Issue_Certainty_Maybe && buf_len) ? IC_DEFINITELY : IC_MAYBE;
      vsa->Report_vsa_error(buf_cr, buf_desc, AOB, ic, &srcpos_h);
    }
    else if (_var_len.size() > var_buf_cnt || len == -1) {
      SRCPOS_HANDLE srcpos_h(buf_cr, callstmt, _caller, Loc_pool());
      const char* call_name = callstmt->Opr() == OPR_CALL ? ST_name(callstmt->St()) : "<sprintf>";
      srcpos_h.Add_message("%s has variant length items and may cause out of bound",
                           call_name);
      //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_SPRINTF_OVERFLOW, TRUE, &srcpos_h);
      vsa->Report_vsa_error(buf_cr, (char*)NULL, AOB, IC_MAYBE, &srcpos_h);
    }
    return;
  }

  std::pair<CODEREP*, RNA_NODE*> len_cr_ret;
  len_cr_ret = vsa->Eval_size_info(len_cr, _callsite->Callstmt(), MAX_VALUE);
  len_cr = len_cr_ret.first;
  len_rna = len_cr_ret.second;
  Is_True(buf_cr != NULL && len_cr != NULL,
          ("invalid parameters for snprintf"));
  if (buf_len != NULL &&
      buf_len->Kind() == CK_CONST &&
      len_cr->Kind() == CK_CONST &&
      (buf_rna == NULL || len_rna == NULL || buf_rna == len_rna)) {
    // valid if both 0 to get the size to be written
    if (buf_len->Const_val() == 0 && len_cr->Const_val() == 0)
      return;

    if (buf_len->Const_val() < len_cr->Const_val()) {
      //fprintf(stderr, "%d:%d snprintf out-of-bound\n",
      //                SRCPOS_filenum(spos), SRCPOS_linenum(spos));
      SRCPOS_HANDLE srcpos_h(buf_cr, callstmt, _caller, Loc_pool());
      const char* call_name = callstmt->Opr() == OPR_CALL ? ST_name(callstmt->St()) : "<snprintf>";
      const char* buf_desc = buf_cr ? srcpos_h.Find_cr_stname(buf_cr, callstmt, Dna()) : "";
      srcpos_h.Add_message("%s writes %ld bytes to %s, which is %ld bytes",
                           call_name, len_cr->Const_val(), buf_desc, buf_len->Const_val());
      //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_SNPRINTF_OVERFLOW, FALSE, &srcpos_h);
      ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
      vsa->Report_vsa_error(buf_cr, (char*)NULL, AOB, ic, &srcpos_h);
    }
  }
  else if (buf_len != NULL) {
    PATH_SELECTED path;
    STMTREP* stmt = _callsite->Callstmt();
    VRA* vra = _caller->Comp_unit()->Vra();
    VRA_RESULT res = vra->Compare_cr<OPR_LT>(buf_len, stmt->Bb(), len_cr, path);
    if (res == VA_YES || res == VA_POSSIBLE) {
      //fprintf(stderr, "%d:%d snprintf out-of-bound\n",
      //                SRCPOS_filenum(spos), SRCPOS_linenum(spos));
      SRCPOS_HANDLE srcpos_h(buf_cr, callstmt, _caller, Loc_pool());
      const char* call_name = stmt->Opr() == OPR_CALL ? ST_name(stmt->St()) : "<snprintf>";
      const char* buf_desc = buf_cr ? srcpos_h.Find_cr_stname(buf_cr, callstmt, Dna()) : "";
      const char* blen_desc = srcpos_h.Find_cr_stname(buf_len, callstmt, Dna());
      if (blen_desc == NULL) blen_desc = "<buffer_size>";
      const char* slen_desc = srcpos_h.Find_cr_stname(len_cr, callstmt, Dna());
      if (slen_desc == NULL) slen_desc = "<print_size>";
      srcpos_h.Add_message("%s writes %s bytes to %s, which is %s bytes",
                           call_name, slen_desc, buf_desc, blen_desc);
      //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_SNPRINTF_OVERFLOW, res == VA_POSSIBLE, &srcpos_h);
      ISSUE_CERTAINTY ic = (!VSA_Issue_Certainty_Maybe && res == VA_YES) ? IC_DEFINITELY : IC_MAYBE;
      vsa->Report_vsa_error(buf_cr, (char*)NULL, AOB, ic, &srcpos_h);
    }
    // TODO: more complex compares
  }

#if 0
  // This is neither FMT nor AOB. will refine later
  if (VSA_Fmt &&
      buf_len != NULL &&
      buf_len->Kind() == CK_CONST &&
      buf_len->Const_val() < len) {
    //fprintf(stderr, "%d:%d snprintf truncated\n",
    //                SRCPOS_filenum(spos), SRCPOS_linenum(spos));
    SRCPOS_HANDLE srcpos_h(buf_cr, callstmt, _caller, Loc_pool());
    const char* call_name = callstmt->Opr() == OPR_CALL ? ST_name(callstmt->St()) : "<snprintf>";
    srcpos_h.Add_message("%s tries to write %ld bytes but only %ld bytes are allowed, causing the content truncated",
                         call_name, len, buf_len->Const_val());
    //vsa->Rbc()->Report_rbc_error(vsa, callstmt, __MVSA_SNPRINTF_TRUNCATED, FALSE, &srcpos_h);
    UINT32 cost = Get_builtin_rule_fix_cost(FMT);
    vsa->Report_vsa_error(buf_cr, (char*)NULL, "FMT-3", cost, IC_DEFINITELY, &srcpos_h);
  }
#endif
  // TODO: more complex rules
}

CODEREP*
FORMAT_PARSER_CONTEXT::Get_arg(INT arg) const
{
  if (arg + _v_arg > _callsite->Arg_cnt()) {
    return NULL;
  }
  Is_True(arg + _v_arg <= _callsite->Arg_cnt(),
          ("arg %d(+%d) exceeds total arguments (%d)",
           arg, _v_arg, _callsite->Arg_cnt()));
  CODEREP* cr = _callsite->Get_arg(arg + _v_arg);
  Is_True(cr != NULL,
          ("can not find parm at %d+%d", arg, _v_arg));
  return cr;
}

std::pair<CODEREP*, RNA_NODE*>
FORMAT_PARSER_CONTEXT::Get_object_length(CODEREP* cr) const
{
  if (cr == NULL)
    return std::pair<CODEREP*, RNA_NODE*>(NULL, NULL);
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  return vsa->Get_object_length(cr, _callsite->Callstmt(), MIN_VALUE);
}

INT
FORMAT_PARSER_CONTEXT::Get_string_length(CODEREP* cr)
{
  if (cr == NULL)
    return 0;
  if (cr->Kind() == CK_LDA) {
    ST* fmt_st = cr->Lda_base_st();
    if (ST_class(fmt_st) == CLASS_CONST &&
        TCON_ty(ST_tcon_val(fmt_st)) == MTYPE_STR) {
      const char * str = Index_to_char_array(TCON_str_idx(ST_tcon_val(fmt_st)));
      return strlen(str);
    }
    return ST_size(fmt_st);
  }
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  CODEREP* cr_len = vsa->Get_string_length(cr, _callsite->Callstmt(), MAX_VALUE).first;
  if (cr_len != NULL && cr_len->Kind() == CK_CONST)
    return cr_len->Const_val();
  _var_len.push_back(cr_len);
  return 0;
}

INT
FORMAT_PARSER_CONTEXT::Get_const_format_length(INT64 value, INT short_cnt, INT long_cnt, char fmt, INT sharp) const
{
  char fmt_str[8];
  INT index = 0;
  BOOL is_signed = (fmt == 'd' || fmt == 'i');
  INT64 smin = - INT64_MAX;
  INT64 smax = INT64_MAX;
  UINT64 umax = UINT64_MAX;

  fmt_str[index++] = '%';
  if (sharp)
    fmt_str[index++] = '#';

  switch (short_cnt) {
  case 2:
    fmt_str[index++] = 'h';
    fmt_str[index++] = 'h';
    if (is_signed) {
      smin = SCHAR_MIN;
      smax = SCHAR_MAX;
    }
    else {
      umax = UCHAR_MAX;
    }
    break;
  case 1:
    fmt_str[index++] = 'h';
    if (is_signed) {
      smin = SHRT_MIN;
      smax = SHRT_MAX;
    }
    else {
      umax = USHRT_MAX;
    }
    break;
  default:
    switch (long_cnt) {
    case 2:
      fmt_str[index++] = 'l';
      // fall through
    case 1:
      // xvsa is 64-bit. if target is 32bit, "%llu" in target equals to "%lu" in xvsa
      // "%lu" in target equals to "%u" in xvsa.
      if (Is_Target_32bit())
        break;
      fmt_str[index++] = 'l';
      break;
    default:
      if (is_signed) {
        smin = INT_MIN;
        smax = INT_MAX;
      }
      else {
        umax = UINT_MAX;
      }
      break;
    }
  }
  // adjust value according to type
  if (is_signed) {
    if (value > smax)
      value = smax;
    else if (value < smin)
        value = smin;
  }
  else {
    if ((UINT64)value > umax)
      value = umax;
  }
  // try format string
  fmt_str[index++] = fmt;
  fmt_str[index] = '\0';
  return snprintf(NULL, 0, fmt_str, value);
}

INT
FORMAT_PARSER_CONTEXT::Get_arg_length(INT arg, INT& short_cnt, INT& long_cnt, char fmt, INT sharp, INT& length) const
{
  CODEREP* cr = Get_arg(arg);
  if (cr == NULL)
    return FALSE;
  Is_True(cr != NULL,
          ("can not find buf parm at %d", arg));
  INT ret = FALSE;
  switch (cr->Kind()) {
  case CK_CONST:
    length = Get_const_format_length(cr->Const_val(), short_cnt, long_cnt, fmt, sharp);
    return ret;
  case CK_VAR:
  case CK_IVAR:
    if (cr->Dsctyp() == MTYPE_U1) {
      short_cnt = 2;
      ret = TRUE;
    }
    else if (cr->Dsctyp() == MTYPE_U2) {
      short_cnt = 1;
      ret = TRUE;
    }
    break;
  case CK_OP:
    if (MTYPE_is_signed(cr->Dtyp()))
      break;
    if (cr->Opr() == OPR_CVTL) {
      if (cr->Offset() <= 8) {
        short_cnt = 2;
        ret = TRUE;
      }
      else if (cr->Offset() <= 16) {
        short_cnt = 1;
        ret = TRUE;
      }
    }
    else if (cr->Opr() == OPR_LSHR) {
      CODEREP* kid1 = cr->Opnd(1);
      if (kid1->Kind() == CK_CONST) {
        if (kid1->Const_val() >= 24) {
          short_cnt = 2;
          ret = TRUE;
        }
        else if (kid1->Const_val() >= 16) {
          short_cnt = 1;
          ret = TRUE;
        }
      }
    }
    break;
  default:
    break;
  }
  // check if cr has constant bounds
  VRA* vra = _caller->Comp_unit()->Vra();
  INT64 min = INT64_MIN, max = INT64_MAX;
  if (vra &&
      vra->Get_bounds(cr, _callsite->Callstmt()->Bb(), min, max) != VA_NB) {
    INT min_len = Get_const_format_length(min, short_cnt, long_cnt, fmt, sharp);
    INT max_len = Get_const_format_length(max, short_cnt, long_cnt, fmt, sharp);
    length = (min_len > max_len) ? min_len : max_len;
  }
  return ret;
}

TYPE_ID
FORMAT_PARSER_CONTEXT::Get_lda_mtype(CODEREP *cr, BOOL array) const
{
  Is_True(cr && cr->Kind() == CK_LDA, ("not lda"));
  TY_IDX ty_idx = cr->Lda_ty();
  Is_True(TY_kind(ty_idx) == KIND_POINTER, ("Lda ty not pointer"));
  ty_idx = TY_pointed(ty_idx);

  UINT field_id = cr->Afield_id();
  if (field_id != 0) {
    Is_True(TY_kind(ty_idx) == KIND_STRUCT, ("Lda base ty not struct"));
    UINT cur_field_id = 0;
    FLD_HANDLE fld = FLD_get_to_field (ty_idx, field_id, cur_field_id);
    Is_True (!fld.Is_Null(), ("Invalid field id %d for type 0x%x",
                              field_id, ty_idx));
    ty_idx = FLD_type(fld);
  }

  if (TY_kind(ty_idx) == KIND_ARRAY) {
    // LDA can also be applied to multi-dimension array, like
    // char a[M][N]; printf("%s", a[0]);
    do {
      ty_idx = TY_etype(ty_idx);
    } while(TY_kind(ty_idx) == KIND_ARRAY);
    return TY_mtype(ty_idx);
  }
  else
    return array ? MTYPE_UNKNOWN : TY_mtype(ty_idx);
}

const char*
FORMAT_PARSER_CONTEXT::Get_const_string(STMTREP* stmt, CODEREP* cr) const
{
  Is_True(cr != NULL, ("invalid null cr"));
  ST* st = NULL;
  STMTREP* sr = stmt;
  switch (cr->Kind()) {
  case CK_LDA:
    st = cr->Lda_base_st();
    if (ST_class(st) == CLASS_CONST) {
      // const string, find string from tcon table
      Is_True(TCON_ty(ST_tcon_val(st)) == MTYPE_STR,
      ("format string LDA is not const string?"));
      const char* str = Index_to_char_array(TCON_str_idx(ST_tcon_val(st)));
      Is_True(str != NULL, ("invalid char array index"));
      return str + cr->Offset();
    }
    else if (ST_class(st) == CLASS_VAR) {
      // variable, find string from MSTORE
      Is_True(TY_kind(ST_type(st)) == KIND_ARRAY ||
              TY_kind(ST_type(st)) == KIND_STRUCT,
              ("format string LDA is not an array?"));
      MU_LIST_ITER mu_iter;
      MU_NODE *mnode;
      FOR_ALL_NODE( mnode, mu_iter, Init(stmt->Mu_list()) ) {
        if (mnode->Aux_id() == cr->Lda_aux_id())
          break;
      }
      if (mnode == NULL ||
          !mnode->Is_Valid() ||
          mnode->OPND()->Is_flag_set(CF_IS_ZERO_VERSION))
        return NULL;
      sr = mnode->OPND()->Defstmt();
      if (sr == NULL || sr->Opr() != OPR_MSTORE)
        return NULL;
      if (sr->Lhs()->Istr_base()->Kind() != CK_LDA ||
          sr->Lhs()->Istr_base()->Lda_aux_id() != cr->Lda_aux_id())
        return NULL;
      const char* str = Get_const_string(sr, sr->Rhs());
      return str;
    }
    else {
      Is_True(FALSE, ("TODO: LDA on non-const or non-var"));
      return NULL;
    }
    break;

  case CK_VAR:
    if (cr->Is_flag_set(CF_DEF_BY_PHI) || cr->Is_flag_set(CF_DEF_BY_CHI))
      return NULL;  // TODO
    sr = cr->Defstmt();
    if (sr == NULL || sr->Rhs() == NULL)
      return NULL;  // TODO
    return Get_const_string(sr, sr->Rhs());
    
  case CK_IVAR:
    if (cr->Opr() == OPR_MLOAD)
      return Get_const_string(sr, cr->Ilod_base());
    break;
  case CK_OP:
    break;

  default:
    Is_True(FALSE, ("TODO: Get_const_string cr_kind=%d", cr->Kind()));
    break;
  }
  return NULL;
}

const char*
FORMAT_PARSER_CONTEXT::Get_format_string() const
{
  // need to switch to caller's context ???
  STMTREP* stmt = _callsite->Callstmt();
  CODEREP* fmt_cr = _callsite->Get_arg(_fmt_arg);
  if (fmt_cr == NULL) {
    DevWarn("Can not find format string at parm %d\n", _fmt_arg);
    return NULL;
  }
  return Get_const_string(stmt, fmt_cr);
}

INT
FORMAT_PARSER_CONTEXT::Add(INT a, CODEREP* b)
{
  if (b->Kind() == CK_CONST)
    return a + b->Const_val();
  _var_len.push_back(b);
  return a;
}

void
FORMAT_PARSER_CONTEXT::Annotate_malloc(INT arg) const
{
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  CODEREP* cr = Get_arg(arg);
  //Is_True(cr != NULL, ("Arg %d is NULL", arg));
  // TODO, annotate hor on cr
}

void
FORMAT_PARSER_CONTEXT::Check_type(INT n_arg, CODEREP* cr, INT s_cnt, INT l_cnt, char fmt)
{
  if (cr == NULL || !VSA_Fmt)
    return;

  TYPE_ID mtype = MTYPE_UNKNOWN;
  BOOL report_fmt = TRUE;
  switch (fmt) {
  case 'd':                 // integer type
    if (MTYPE_is_integral(cr->Dtyp()))
      report_fmt = FALSE;
    break;

  case 'f':                  // floating point type
    if (MTYPE_is_float(cr->Dtyp()))
      report_fmt = FALSE;
    break;

  case 's':                  // string type
  case 'n':                  // integer pointer type
    if (cr->Kind() == CK_OP) {
      // TODO: refine CK_OP to figure out the real mtype
      if (MTYPE_is_integral(cr->Dtyp()) &&
          MTYPE_byte_size(cr->Dtyp()) == MTYPE_byte_size(Pointer_Mtype))
        report_fmt = FALSE;
      break;
    }
    else if (cr->Kind() == CK_LDA) {
      mtype = Get_lda_mtype(cr, fmt == 's');
    }
    else {
      TY_IDX ty = cr->object_ty();
      if (cr->Kind() == CK_VAR &&
          _caller->Comp_unit()->Opt_stab()->Aux_stab_entry(cr->Aux_id())->Is_preg()) {
        hash_set<IDTYPE> visited;
        ty = _caller->Comp_unit()->Vsa()->Find_preg_type(cr, visited);
      }
      if (ty == TY_IDX_ZERO) {
        report_fmt = FALSE;  // should report an 'M' FMT?
        break;
      }
      if (TY_kind(ty) == KIND_POINTER &&
          TY_kind(TY_pointed(ty)) == KIND_STRUCT) {
        // LDA of field 1 will lost when lower ILDA-LDID from p->f1:
        //  LDLD p
        // ILDA ofst=0 field=1   ==>  LDID p
        // check if first real field is char array
        TY_IDX fld_ty = Get_first_real_field(TY_pointed(ty));
        if (fld_ty != TY_IDX_ZERO && TY_kind(fld_ty) == KIND_ARRAY) {
          mtype = TY_mtype(TY_etype(fld_ty));
        }
      }
      else if (TY_kind(ty) == KIND_POINTER) {
        ty = TY_pointed(ty);
        if (TY_kind(ty) == KIND_ARRAY)
          mtype = TY_mtype(TY_etype(ty));
        else
          mtype = TY_mtype(ty);
      }
    }
    if (fmt == 's' &&
        (mtype == MTYPE_I1 || mtype == MTYPE_U1 || mtype == MTYPE_V)) {
      report_fmt = FALSE;
    }
    if (fmt == 'n' &&
        (mtype == MTYPE_I4 || mtype == MTYPE_U4)) {
      report_fmt = FALSE;
    }
    break;
  case 'p':                    // value with pointer type
    // pointer type
    if (MTYPE_is_integral(cr->Dtyp()) &&
        MTYPE_byte_size(cr->Dtyp()) == MTYPE_byte_size(Pointer_Mtype))
      report_fmt = FALSE;
    break;

  default:
    report_fmt = FALSE;
    break;
  }

  if (report_fmt) {
    VSA* vsa = _caller->Comp_unit()->Vsa();
    Is_True(vsa != NULL, ("vsa is NULL"));
    STMTREP* stmt = _callsite->Callstmt();
    SRCPOS_HANDLE sp_h(cr, stmt, _caller, Loc_pool());
    const char *var_name = sp_h.Find_cr_stname(cr, stmt, _caller, FALSE);
    char var_name_buf[16];
    if (var_name == NULL) {
      snprintf(var_name_buf, 16, "#%d", n_arg + _v_arg);
      var_name = var_name_buf;
    }
    UINT32 cost = Get_builtin_rule_fix_cost(FMT);
    vsa->Report_vsa_error(cr, var_name, "FMT", cost, IC_DEFINITELY, &sp_h);
  }
}

void
FORMAT_PARSER_CONTEXT::Check_npd(CODEREP* cr)
{
  if (cr == NULL || !VSA_Npd())
    return;
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  STMTREP* stmt = _callsite->Callstmt();
  if (VSA_New_Npd_Checker()) {
    vsa->Scan_npd_new(cr, stmt);
  } else {
    SRCPOS_HANDLE srcpos_h(cr, stmt, _caller, Loc_pool());
    hash_set<IDTYPE> visited_bb;
    CALL_STACK cs;
    vsa->Classify_vul_error(cr, stmt->Bb(), stmt, cs, &srcpos_h, FOR_ILOD_BASE, visited_bb);
    Is_True(cs.empty(), ("call stack is not empty"));
  }
}

void
FORMAT_PARSER_CONTEXT::Check_uaf(CODEREP *cr)
{
  if (cr == NULL || !VSA_Uaf())
    return;

  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  if (vsa) {
    IDTYPE arg_idx = _callsite->Get_arg_with_cr(cr);
    if (arg_idx != INVALID_VAR_IDX) {
      _callsite->Set_arg_flag(arg_idx, REF_ILOAD);
    }
    vsa->Check_uaf(cr, _callsite->Callstmt());
  }
}

void
FORMAT_PARSER_CONTEXT::Check_object_size(CODEREP* cr, INT size)
{
  if (cr == NULL)
    return;
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  VRA* vra = _caller->Comp_unit()->Vra();
  if (vra == NULL)
    return;
  std::pair<CODEREP *, RNA_NODE*> len_ret = Get_object_length(cr);
  CODEREP* len = len_ret.first;
  if (len == NULL)
    return;
  CODEREP* sz_cr = vsa->Add_const(MTYPE_I8, size);
  STMTREP* stmt = _callsite->Callstmt();
  PATH_SELECTED path;
  VRA_RESULT res = vra->Compare_cr<OPR_LT>(len, stmt->Bb(), sz_cr, path);
  SRCPOS spos = stmt->Linenum();
  if (res == VA_YES || res == VA_POSSIBLE) {
    //fprintf(stderr, "VSA warning: buffer overflow found [%c] at line %d:%d\n",
    //                res == VA_YES ? 'D' : 'M',
    //                SRCPOS_filenum(spos), SRCPOS_linenum(spos));
    SRCPOS_HANDLE srcpos_h(cr, stmt, _caller, Loc_pool());
    const char* dest_desc = srcpos_h.Find_cr_stname(cr, stmt, Dna());
    if (dest_desc == NULL) dest_desc = "<buffer>";
    const char* call_name = stmt->Opr() == OPR_CALL ? ST_name(stmt->St()) : "<printf>";
    if (len->Kind() == CK_CONST)
      srcpos_h.Add_message("%s writes %d bytes to %s, which is %ld bytes",
                           call_name, size, dest_desc, len->Const_val());
    else
      srcpos_h.Add_message("%s writes %d bytes to %s, which is more than buffer size",
                           call_name, size, dest_desc);
    //vsa->Rbc()->Report_rbc_error(vsa, stmt, __MVSA_SCANF_OVERFLOW, FALSE, &srcpos_h);
    ISSUE_CERTAINTY ic = (!VSA_Issue_Certainty_Maybe && res == VA_YES) ? IC_DEFINITELY : IC_MAYBE;
    vsa->Report_vsa_error(cr, dest_desc, AOB, ic, &srcpos_h);
  }
}

void
FORMAT_PARSER_CONTEXT::Check_unlimit_string(INT n_arg, CODEREP* cr)
{
  if (_buf_arg != -1 && _len_arg == -1) {
    //  fprintf(stderr, "probably buffer overflow for unlimited length string\n");
    VSA* vsa = _caller->Comp_unit()->Vsa();
    Is_True(vsa != NULL, ("vsa is NULL"));
    STMTREP* stmt = _callsite->Callstmt();
    SRCPOS_HANDLE srcpos_h(cr, stmt, _caller, Loc_pool());
    const char* call_name = stmt->Opr() == OPR_CALL ? ST_name(stmt->St()) : "<printf>";
    const char* cr_desc = cr ? srcpos_h.Find_cr_stname(cr, stmt, Dna()) : "";
    if (n_arg == -1)
      srcpos_h.Add_message("`%%m' in %s may cause buffer overflow", call_name);
    else
      srcpos_h.Add_message("the %d parameter in %s may cause buffer overflow", n_arg, call_name);
    //vsa->Rbc()->Report_rbc_error(vsa, stmt, __MVSA_SPRINTF_OVERFLOW, TRUE, &srcpos_h);
    ISSUE_CERTAINTY ic = VSA_Issue_Certainty_Maybe ? IC_MAYBE : IC_DEFINITELY;
    vsa->Report_vsa_error(cr, cr_desc, AOB, ic, &srcpos_h);
  }
}

void
FORMAT_PARSER_CONTEXT::Report_error(const char* var, const char* msg)
{
  if (!VSA_Fmt)
    return;
  VSA* vsa = _caller->Comp_unit()->Vsa();
  Is_True(vsa != NULL, ("vsa is NULL"));
  //fprintf(stderr, "Error %s\n", msg);
  STMTREP* stmt = _callsite->Callstmt();
  SRCPOS_HANDLE srcpos_h(NULL, stmt, _caller, Loc_pool());
  srcpos_h.Add_message(msg);
  //report FMT instead of non-standard WRONG_FORMAT_STRING
  //vsa->Rbc()->Report_rbc_error(vsa, stmt, __MVSA_WRONG_FORMAT_STRING, FALSE, &srcpos_h);
  UINT32 cost = Get_builtin_rule_fix_cost(FMT);
  vsa->Report_vsa_error(_callsite->Get_arg(_fmt_arg),
                        var, "FMT", cost, IC_DEFINITELY, &srcpos_h);
}

void
FORMAT_PARSER_CONTEXT::Report_error(CODEREP* cr, UINT32 anat)
{
  VSA* vsa = _caller->Comp_unit()->Vsa();
  STMTREP* stmt = _callsite->Callstmt();
  SRCPOS_HANDLE srcpos_h(cr, stmt, _caller, Loc_pool());
  const char* cr_desc = cr ? srcpos_h.Find_cr_stname(cr, stmt, Dna()) : "";
  vsa->Report_vsa_error(cr, cr_desc, anat, IC_DEFINITELY, &srcpos_h);
}

// macros to adapt Vsa_parse_printf_format/Vsa_parse_scanf_format
#define INT_PARM_TYPE        CODEREP*
#define INT_PARM_INITIALIZER NULL
#define INTP_PARM_TYPE       CODEREP*
#define INTP_PARM_INITIALIZER NULL
#define FLT_PARM_TYPE        CODEREP*
#define FLT_PARM_INITIALIZER NULL
#define PTR_PARM_TYPE        CODEREP*
#define PTR_PARM_INITIALIZER NULL
#define STR_PARM_TYPE        CODEREP*
#define STR_PARM_INITIALIZER NULL

#define INT_PARM_CMP(arg, cmp, val) \
  (arg->Kind() == CK_CONST && arg->Const_val() cmp val)

#define INT_PARM_SET(arg, val) do { \
    Is_True(arg->Kind() == CK_CONST, ("arg is not const")); \
    arg->Set_const_val(val); \
  } while (0)

#define INT_PARM_ADD(ctx, a, b) \
  ctx->Add(a, b)

#define PARSER_ARGS

#define PARSER_INIT(ctx, fmt) \
  ctx->Initialize(fmt)

#define PARSER_FINI(ctx, arg, len) \
  ctx->Finalize(arg, len)

#define PARSER_GET_ARG(ctx, arg, type) \
  ctx->Get_arg(arg)

#define PARSER_CHECK_TYPE(ctx, n_arg, arg, s_cnt, l_cnt, fmt) \
  ctx->Check_type(n_arg, arg, s_cnt, l_cnt, fmt)

#define PARSER_CHECK_NPD(ctx, arg) \
  ctx->Check_npd(arg)

#define PARSER_CHECK_UAF(ctx, arg) \
  ctx->Check_uaf(arg)

#define PARSER_CHECK_OBJ_SIZE(ctx, arg, sz) \
  ctx->Check_object_size(arg, sz)

#define PARSER_CHECK_UNLIMIT_STRING(ctx, n_arg, arg) \
  ctx->Check_unlimit_string(n_arg, arg)

#define PARSER_STR_LENGTH(ctx, obj) \
  ctx->Get_string_length(obj)

#define PARSER_REPORT_ERROR(ctx, var, msg) \
  ctx->Report_error(var, msg)

#define PARSER_REPORT_VSA_ERROR(ctx, var, kind) \
  ctx->Report_error(var, kind)

#define PARSER_ANNOTATE_MALLOC(ctx, arg) \
  ctx->Annotate_malloc(arg)

#define PARSER_RETURN(ctx, arg, len) \
  0

#define PARSER_GET_ARG_LENGTH(ctx, arg, short_cnt, long_cnt, fmt, sharp, length) \
  ctx->Get_arg_length(arg, short_cnt, long_cnt, fmt, sharp, length)

#include "opt_vsa_printf.h"

#include "opt_vsa_scanf.h"

void
VSA::Check_printf(DNA_NODE* caller, RNA_NODE* callsite,
                  INT buf_arg, INT len_arg, INT fmt_arg, INT v_arg)
{
  FORMAT_PARSER_CONTEXT ctx(caller, callsite,
                            buf_arg, len_arg, fmt_arg, v_arg);
  const char* fmt_str = ctx.Get_format_string();
  if (fmt_str == NULL) {
    // TODO: probably defined by PHI, we can still do something
    ctx.Finalize(-1, -1);
    return;
  }
  Vsa_parse_printf_format(&ctx, fmt_str);
}

void
VSA::Check_scanf(DNA_NODE* caller, RNA_NODE* callsite,
                 INT fmt_arg, INT v_arg)
{
  FORMAT_PARSER_CONTEXT ctx(caller, callsite,
                            -1, -1, fmt_arg, v_arg);
  const char* fmt_str = ctx.Get_format_string();
  if (fmt_str == NULL) {
    // TODO: probably defined by PHI, we can still do something
    ctx.Finalize(-1, -1);
    return;
  }
  Vsa_parse_scanf_format(&ctx, fmt_str);
}

