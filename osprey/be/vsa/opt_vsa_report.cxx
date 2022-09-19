//-*-c++-*-

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

// =============================================================================
// =============================================================================
//
// Module: opt_vsa_report.xxx
//
// =============================================================================
#include "opt_defs.h"
#include "opt_bb.h"
#include "opt_main.h"
#include "opt_vsa_util.h"
#include "opt_vsa_report.h"
#include "vsa_json_writter.h"
#include "config_vsa.h"
#include "opt_vsa_path.h"
#include "erbe.h"

// ============================================================================
// VSA_REPORT_INITIALIZER
// initialize Report_vsa_error_p
// ============================================================================
class VSA_REPORT_INITIALIZER {
public:
  VSA_REPORT_INITIALIZER() {
    Report_vsa_error_p = Vsa_error_print;
  }
};
static VSA_REPORT_INITIALIZER vsa_initializer;

// ============================================================================
// VIW_IMPL
// declaration for different version issue writter
// ============================================================================
template<INT N, class WRITTER>
class VIW_IMPL : public VSA_ISSUE_WRITTER {
  friend class VSA_ISSUE_WRITTER;
private:
  VIW_IMPL(FILE *fp) {}

public:
  // print a single path to json file
  void Print_json_path(const SRCPOSINFO* path, FILE* fp) { }

  // print a single issue to json file
  void Print_json_issue(const VSA_ISSUE* issue, FILE* fp) { }

  // flush the file
  void Flush() { }
};

// ============================================================================
// template<> VIW_IMPL<WRITTER_VER_0>
// implementation for version 0 writter, for static HTML report
// ============================================================================
template<class WRITTER>
class VIW_IMPL<WRITTER_VER_0, WRITTER> : public VSA_ISSUE_WRITTER {
  friend class VSA_ISSUE_WRITTER;
private:
  WRITTER &_writter;

  VIW_IMPL(FILE *fp) : _writter(WRITTER::Get_writter(fp)) {}

public:
  // print a single path
  void Print_json_path(const SRCPOSINFO* path, FILE* fp)
  {
    SRCPOS spos = path->Spos();
    _writter.template Start<TAG_MAP>(NULL, 3);   // no key, 3 attrs
    _writter.Append("fileno", SRCPOS_filenum(spos));
    _writter.Append("lineno", Srcpos_To_Line(spos));
    _writter.Append("message", path->Msg_id());
    _writter.template End<TAG_MAP>(NULL, 3);     // no key, 3 attrs
  }

  // print a single issue
  void Print_json_issue(const VSA_ISSUE* issue, FILE* fp)
  {
    const char* category = "Vul";
    const char* severity = "@@severity@@";
    SRCPOS spos = issue->Start_spos();
    if (issue->Sp_h() == NULL)
      spos = SRCPOS_NODE::Transpos(spos, File_Index);
    const char *key = issue->Key();
    const char *aname = issue->Rule_code();
    const char *subcode = issue->Error_code() ? issue->Error_code() : "";
    const char *vname = Vname();
    const char *pname = Pname();
    _writter.template Start<TAG_MAP>(NULL, 11);  // no key, 11 attrs, include path
    _writter.Append("fileno", SRCPOS_filenum(spos));
    _writter.Append("lineno", SRCPOS_linenum(spos));
    _writter.Append("colno",  SRCPOS_column(spos));
    _writter.Append("key", key);
    _writter.Append("category", category);
    _writter.Append("severity", severity);
    _writter.Append("vulname", aname);
    _writter.Append("errcode", subcode);
    _writter.Append("varname", vname);
    _writter.Append("funcname", pname);
    _writter.template Start<TAG_ARRAY>("path", 0);
    Print_json_paths(issue, fp);
    _writter.template End<TAG_ARRAY>("path", 0);
    _writter.template End<TAG_MAP>(NULL, 11);    // no key, 11 attrs, include path
  }

  void Flush()
  {
    _writter.Flush();
  }
};

// ============================================================================
// template<> VIW_IMPL<WRITTER_VER_1>
// implementation for version 1 writter
// ============================================================================
template<class WRITTER>
class VIW_IMPL<WRITTER_VER_1, WRITTER> : public VSA_ISSUE_WRITTER {
  friend class VSA_ISSUE_WRITTER;
private:
  WRITTER &_writter;

  VIW_IMPL(FILE *fp) : _writter(WRITTER::Get_writter(fp)) {}

public:
  // print a single path
  void Print_json_path(const SRCPOSINFO* path, FILE* fp)
  {
    SRCPOS spos = path->Spos();
    char *msgbuf = NULL;
    const char *msg;
    msgbuf = (char*)malloc(32);
    snprintf(msgbuf, 32, "%d", path->Msg_id());
    msg = msgbuf;
    char *vname = NULL;
    if (path->Vname())
      vname = Vsa_demangle(path->Vname());
    char *fname = NULL;
    if (path->Fname())
      fname = Vsa_demangle(path->Fname(), TRUE);
    Is_True(fname != NULL, ("fname not set"));
    _writter.template Start<TAG_MAP>(NULL, 6);  // no key, 6 sttrs
    _writter.Append("fid", SRCPOS_filenum(spos));
    _writter.Append("sln", Srcpos_To_Line(spos));
    _writter.Append("scn", SRCPOS_column(spos));
    _writter.Append("m", msg);
    _writter.Append("vn", vname);
    _writter.Append("fn", fname);
    _writter.template End<TAG_MAP>(NULL, 6);    // no key, 6 attrs
    if (vname)
      free(vname);
    if (fname)
      free(fname);
    if (msgbuf)
      free(msgbuf);
  }

  // print a single issue
  void Print_json_issue(const VSA_ISSUE* issue, FILE* fp)
  {
    const char *key = issue->Key();
    const char* certainty = ISSUE_CERTAINTY_name(issue->Certainty());
    const char* ruleset = issue->Rule_set();
    const char* rulecode = issue->Rule_code();
    const char* errorcode = issue->Error_code();
    const char* msg = issue->Sp_h() && issue->Sp_h()->Message()
                        ? issue->Sp_h()->Message()
                        : NULL;
    char msgbuf[32];
    if (msg == NULL) {
      snprintf(msgbuf, 32, "${%s.1}", rulecode);
      msg = msgbuf;
    }
    SRCPOS spos = issue->Start_spos();
    if (issue->Sp_h() == NULL)
      spos = SRCPOS_NODE::Transpos(spos, File_Index);
    UINT   complexity = issue->Complexity();
    Is_True(complexity > 0, ("complexity is 0"));

    _writter.template Start<TAG_MAP>(NULL, 13);  // no key, 13 attrs include path
    _writter.Append("fid", SRCPOS_filenum(spos));
    _writter.Append("sln", SRCPOS_linenum(spos));
    _writter.Append("scn",  SRCPOS_column(spos));
    _writter.Append("k", key);
    _writter.Append("rs", ruleset);
    _writter.Append("rc", rulecode);
    _writter.Append("ec", errorcode);
    _writter.Append("c", certainty);
    _writter.Append("ic", complexity);
    _writter.Append("vn", Vname());
    _writter.Append("fn", Pname());
    _writter.Append("m", msg);
    _writter.template Start<TAG_ARRAY>("paths", 0);
    Print_json_paths(issue, fp);
    _writter.template End<TAG_ARRAY>("paths", 0);
    _writter.template End<TAG_MAP>(NULL, 13);   // no key, 12 attrs
  }

  // flush the writter
  void Flush()
  {
    _writter.Flush();
  }
};


// ============================================================================
// VSA_ISSUE_WRITTER

// ============================================================================
// VSA_ISSUE_WRITTER
// ============================================================================
VSA_ISSUE_WRITTER* VSA_ISSUE_WRITTER::_writter;

template<INT N, class WRITTER> VSA_ISSUE_WRITTER*
VSA_ISSUE_WRITTER::Create_writter(FILE *fp)
{
  if (N == VSA_Json_Version)
    return new VIW_IMPL<N, WRITTER>(fp);
  else
    return Create_writter<N-1, WRITTER>(fp);
}

template<> VSA_ISSUE_WRITTER*
VSA_ISSUE_WRITTER::Create_writter<WRITTER_MIN, CBOR_WRITTER>(FILE *fp)
{
  return new VIW_IMPL<WRITTER_MIN, CBOR_WRITTER>(fp);
}

template<> VSA_ISSUE_WRITTER*
VSA_ISSUE_WRITTER::Create_writter<WRITTER_MIN, PLAIN_TEXT_WRITTER>(FILE *fp)
{
  return new VIW_IMPL<WRITTER_MIN, PLAIN_TEXT_WRITTER>(fp);
}

VSA_ISSUE_WRITTER::VSA_ISSUE_WRITTER()
{
  _dname_v = NULL;
  _dname_p = NULL;
  _issue_count = 0;
  _json = (Need_vsafile() && RFile != NULL && VSA_Output_Json);
}

VSA_ISSUE_WRITTER*
VSA_ISSUE_WRITTER::Get_writter(FILE *fp)
{
  if (_writter == NULL) {
    _writter = VSA_Output_Cbor
                 ? Create_writter<WRITTER_MAX, CBOR_WRITTER>(fp)
                 : Create_writter<WRITTER_MAX, PLAIN_TEXT_WRITTER>(fp);
  }
  return _writter;
}

// ============================================================================
// VSA_ISSUE_WRITTER
// ============================================================================
void
VSA_ISSUE_WRITTER::Fix_vname()
{
  if (_dname_v == NULL)
    return;
  INT pos = strlen(_dname_v) - 1;
  INT mark = 0;
  while (pos >= 0) {
    char ch = _dname_v[pos];
    if (ch == '.')
      mark = pos;
    else if (ch < '0' || ch > '9')
      break;
    --pos;
  }
  if (mark != 0)
    _dname_v[mark] = '\0';
}

void
VSA_ISSUE_WRITTER::Print_issue(const VSA_ISSUE* issue, FILE* fp)
{
  if (issue->Start_spos() == 0)
    return;
  _dname_v = issue->Vname() == NULL ? NULL : Vsa_demangle(issue->Vname());
  if (_dname_v) {
    // do not report issue in system library whose variable name is
    // "blahblah::__blah"
    char *colon = strrchr(_dname_v, ':');
    if (colon && colon > _dname_v &&
        colon[-1] == ':' &&
        colon[1] == '_' &&
        colon[2] == '_') {
      free(_dname_v);
      _dname_v = NULL;
      return;
    }
  }
  if (VSA_Filter_Include) {
    const char *dirname = NULL;
    const char *fname = NULL;
    Get_Srcpos_Filename(issue->Start_spos(), &fname, &dirname);
    if (dirname && Vsa_check_path_ignore(dirname))
      return;
  }
  Fix_vname();
  _dname_p = Vsa_demangle(issue->Pname(), TRUE);
  Print_text_issue(issue, stderr);

  if (Json()) {
    Print_json_issue(issue, fp);
  }
  if (_dname_v) {
    free(_dname_v); _dname_v = NULL;
  }
  if (_dname_p) {
    free(_dname_p); _dname_p = NULL;
  }
  ++ _issue_count;
}

#define Alterr VsaTxt_File
// =============================================================================
//
// Hack to prune away unexpected issue_key
//
// =============================================================================
static char*
Find_char(char *str, char c)
{
  do {
    ++str;
  } while (*str != 'c' && *str != '\0');
  return str;
}

static char*
Prune_issue_key_hack(char *key)
{
  if (key != NULL) {
    char *cursor = Find_char(key, '@');
    cursor = Find_char(key, ':');
    cursor = Find_char(key, '@');
    *cursor = '\0';
  }
  return key;
}

extern "C" IDTYPE IPSA_put_isukey(const char* str);  // defined in opt_dna.cxx

void
VSA_ISSUE_WRITTER::Print_text_issue(const VSA_ISSUE* issue, FILE* fp)
{
  BOOL is_builtin = issue->Is_builtin();
  BOOL is_cert = issue->Is_cert();
  BOOL is_user = issue->Is_user();
  BOOL is_sml = issue->Is_sml();
  BOOL is_builtin_rbc = is_builtin && issue->Is_rbc();
  ARptMsg arptm(is_builtin ? issue->Rule_code() : "RBC",
                TRUE, FALSE, FALSE); // for type and category in report;
  INT len = strlen(issue->Rule_set()) + strlen(issue->Rule_code()) + 32;
  if (_dname_v)
    len += strlen(_dname_v);
  if (issue->Error_code())
    len += strlen(issue->Error_code());

  char *rbc_msg = (char*)malloc(len);
  rbc_msg[0] = '\0';
  Is_True(issue->Rule_set() != NULL, ("_rule_set not set"));
  if (is_builtin_rbc) {
    strcat(rbc_msg, issue->Rule_set());
    strcat(rbc_msg, "],[");
  }
  if (!is_builtin && !is_builtin_rbc) {
    strcat(rbc_msg, issue->Get_rule_set());
    strcat(rbc_msg, "],[");
    const char* msg = issue->Rule_code();
    if (msg != NULL && *msg != '\0' && strcmp(msg, "RBC") != 0) {
      strcat(rbc_msg, msg);
      strcat(rbc_msg, "],[");
    }
  }
  if (issue->Error_code() != NULL && *issue->Error_code() != '\0') {
    strcat(rbc_msg, issue->Error_code());
    strcat(rbc_msg, "],[");
  }
  if (Vname())
    strcat(rbc_msg, Vname());

  INT ecode = issue->Is_sml()
                ? EC_VSA_Sml_violation
                : issue->Certainty() == IC_MAYBE ? EC_VSA_Rbc_may_violation
                                                 : EC_VSA_Rbc_violation;
  SRCPOS_HANDLE* sp_h = issue->Sp_h();
  char *path = sp_h != NULL ? sp_h->Compose_path_string() : NULL;

  // print issue key ahead of the formatted error message
  if (Alterr != NULL) { // prepend [ issue->Key() ],
    if (VSA_Magic_Id == NULL)
      fprintf(Alterr, "[A10],[%s],", ((char *)issue->Key()));
    else
      fprintf(Alterr, "[%.5s],[%s],", VSA_Magic_Id,
              //issue->Key_id(), 
              ((char *)issue->Key()));
  }

  if (path) {
    ErrMsgSrcpos(ecode, issue->Start_spos(),
                 arptm.getCat(), rbc_msg, Pname(), path);
    free(path);
  }
  else {
    char path_buf[512];
    STRING_BUFFER strbuf(path_buf, 512);
    const SRCPOS *sptr = issue->Spos_array();
    UINT32 count = issue->Spos_count();
    Is_True(sptr && count > 0, ("no spos"));
    strbuf.Append('[');
    for (INT i = count - 1; i >= 0; --i) {
      if (i != count - 1)
        strbuf.Append(',');
      SRCPOS spos = SRCPOS_NODE::Transpos(sptr[i], File_Index);
      strbuf.Format("%d:%d:%d:%d",
                    SRCPOS_filenum(spos), SRCPOS_linenum(spos), SRCPOS_column(spos),
               i > 0 ? PATHINFO_INL_BEGIN : PATHINFO_VUL_SPOT);
    }
    strbuf.Append(']');
    ErrMsgSrcpos(ecode, issue->Start_spos(),
                 arptm.getCat(), rbc_msg, Pname(), strbuf.To_string());
  }
  free(rbc_msg);
}

void
VSA_ISSUE_WRITTER::Print_json_paths(const VSA_ISSUE* issue, FILE* fp)
{
  SRCPOS            master = issue->Start_spos();
  INT               count = 0;
  INT               headcnt = VSA_Path_Head_Max;
  INT               tailcnt = VSA_Path_Tail_Max;
  SRCPOS            spos = 0;
  SRCPOSINFO_VECTOR vector;
  SRCPOS_HANDLE    *sp_h = issue->Sp_h();

  if (sp_h != NULL) {
    sp_h->Compose_path_vector(&vector);
    BOOL truncated = vector.size() > headcnt + tailcnt ? TRUE : FALSE;
    for (SRCPOSINFO_VECTOR::iterator it = vector.begin();
         it != vector.end(); ++it) {
      spos = it->Spos();
      //if (count > 0)
      //  fprintf(fp, ",\n");
      ++ count;
      Print_json_path(&(*it), fp);
      if (truncated && count >= headcnt) {
        //fprintf(fp, "\n");
        std::advance(it, vector.size() - headcnt - tailcnt);
        truncated = FALSE;
      }
    }
    if (count == 0 || spos != master) {
      //if (count > 0)
      //  fprintf(fp, ",\n");
      SRCPOSINFO path(master, PATHINFO_VUL_SPOT, sp_h->Orig_stname(), sp_h->Orig_puname());
      Print_json_path(&path, fp);
    }
  }
  else {
    const SRCPOS *sptr = issue->Spos_array();
    UINT32 count = issue->Spos_count();
    Is_True(sptr && count > 0, ("no spos"));
    for (INT i = count - 1; i >= 0; --i) {
      SRCPOS spos = SRCPOS_NODE::Transpos(sptr[i], File_Index);
      SRCPOSINFO path(spos,
                      i > 0 ? PATHINFO_INL_BEGIN : PATHINFO_VUL_SPOT,
                      issue->Vname(), issue->Pname());
      Print_json_path(&path, fp);
    }
  }
}

UINT
VSA_ISSUE::Complexity(void) const
 {
   return ((Sp_h() ? Sp_h()->Complexity() : 1) * Fix_cost());
 }
