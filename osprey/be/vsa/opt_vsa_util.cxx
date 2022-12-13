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
// Module: opt_vsa_util.cxx
//
// =============================================================================
//
// Description:
//
// Utilities for Vulnerability Static Analysis
//
// =============================================================================
// =============================================================================

#include <strings.h>
#include "defs.h"
#include "vsa_defs.h"
#include "opt_defs.h"
#include "config_wopt.h"
#include "glob.h"
#include "printsrc.h"
#include "erglob.h"
#include "erbe.h"
#include "config_vsa.h"
#include "opt_htable.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_vsa_util.h"
#include "opt_vsa.h"
#include "whirl_file_mgr.h"
#include "opt_vsa_report.h"
#include "java_defs.h"

// ====================================================================
// VSA statistics
// ====================================================================
#ifdef Is_True_On

struct VSA_STATS _vsa_stats;
static struct VSA_STATS _vsa_stats_sav;

void
VSA_STATS::Print(FILE *fp) const
{
  fprintf(fp, "VSA object statistics:\n");
  fprintf(fp, "-------------------------------\n");

#define PRINT(x, y) fprintf(fp, " # of %-18s %d\n", y, _vsa_stats._num_##x);
  FOR_ALL_STAT_ITEM(PRINT)
#undef PRINT
  fprintf(fp, "\n");
}

void
VSA_STATS::Print_delta(const char* fname, FILE *fp) const
{
  fprintf(fp, "VSA object statistics for %s:\n", fname);
  fprintf(fp, "-------------------------------\n");

#define PRINT(x, y) fprintf(fp, " # of %-18s %d\n", y, \
                            _vsa_stats._num_##x - _vsa_stats_sav._num_##x);
  FOR_ALL_STAT_ITEM(PRINT)
#undef PRINT
  fprintf(fp, "\n");

  // save current data
  memcpy(&_vsa_stats_sav, &_vsa_stats, sizeof(_vsa_stats));
}
#endif // Is_True_On

// ====================================================================
//
// return final string
//
// ====================================================================
const char*
STRING_BUFFER::To_string() const
{
  if (_current == _top)
    *(_current - 1) = '\0';
  else
    *(_current) = '\0';
  return _current != _base ? _base : NULL;
}

// ====================================================================
//
// adjust current by add given ofst
//
// ====================================================================
void
STRING_BUFFER::Adjust_current(INT ofst)
{
  char * addr = _current + ofst;
  Is_True(addr >= _base && addr <= _top, ("buffer overflow"));
  if (addr >= _base && addr <= _top)
    _current = addr;
}


// ====================================================================
//
// adjust current to the end of string or last element of buffer
//
// ====================================================================
void
STRING_BUFFER::Adjust_current()
{
  while (_current < _top && *_current != '\0')
    ++ _current;
}

// ====================================================================
//
// append char to buffer
//
// ====================================================================
BOOL
STRING_BUFFER::Append(char ch)
{
  if (_current < _top) {
    *_current++ = ch;
    return TRUE;
  }
  else {
    return FALSE;
  }
}

// ====================================================================
//
// append string to buffer. escape indicates if str contains
// invalid characters
//
// ====================================================================
BOOL
STRING_BUFFER::Append(const char* str, BOOL escape)
{
  if (str == NULL || *str == '\0')
    return TRUE;
  return Append_n(str, strlen(str), escape);
}

BOOL
STRING_BUFFER::Append_n(const char *str, INT len, BOOL escape)
{
  if (str == NULL || *str == '\0')
    return TRUE;
  INT avail = _top - _current;
  if (avail <= 0)
    return FALSE;
  INT copy_len = len;
  INT str_len = strlen(str);
  if(str_len < len) {
    copy_len = str_len;
  }
  if (escape) {
    // escape is true, str may contains invalid characters
    // call Json_copy to ignore those characters
    copy_len = Json_copy(_current, str, avail);
  }
  else {
    // do memcpy directly
    copy_len = copy_len < avail ? copy_len : avail;
    memcpy(_current, str, copy_len);
  }
  BOOL ret = copy_len < avail ? TRUE : FALSE;
  _current += copy_len;
  Is_True(_current <= _top, ("buffer overflow"));
  return ret;
}

BOOL
STRING_BUFFER::Append_stname(const char *str)
{
  if (str == NULL || *str == '\0')
    return TRUE;
  // str may contain '.' for PSTATIC, remove char after '.'
  while ((_current < _top) &&
         (*str != '\0') &&
         (*str != '.')) {
    *_current++ = *str++;
  }
  return _current < _top ? TRUE : FALSE;
}

// ====================================================================
//
// format parameters into buffer
//
// ====================================================================
BOOL
STRING_BUFFER::Format(const char* fmt, ...)
{
  INT avail = _top - _current;
  if (avail <= 0)
    return FALSE;
  va_list ap;
  va_start(ap, fmt);
  INT len = vsnprintf(_current, avail, fmt, ap);
  va_end(ap);
  INT copy_len = len < avail ? len : avail;
  BOOL ret = len < avail ? TRUE : FALSE;
  _current += copy_len;
  Is_True(_current <= _top, ("buffer overflow"));
  return ret;
}

// ====================================================================
// Message print routines for ALL VSA analysis messages
// Any change in output format should be done here
// ====================================================================

#include "../../include/gnu/demangle.h"
extern "C" char *cplus_demangle (const char *, int);

// ATTENTION: this method will return malloced heap address, please free after use
char *
Vsa_demangle(const char *sym_name, BOOL append_paren)
{
  char *p = NULL;
  int options;
  if (!sym_name)
    return strdup("");
  int len = strlen(sym_name);
  if (len <= 2) {
    if (append_paren) {
      p = (char*)malloc(8);
      memcpy(p, sym_name, len);
      memcpy(p+len, "()", 3);  // include last '\0'
      return p;
    }
    return strdup(sym_name);
  }
  if (sym_name[0] == '_' && sym_name[1] == 'Z') {
    const char *orig_name = sym_name;
    char *fld_dot = strstr((char*)sym_name, ".");
    char *dup_name = NULL;
    char *fld_name = NULL;
    // convert mangled class name with fld
    // ex: _ZN10staticVsym4tstrE.str -> staticVsym.tstr.str
    if(fld_dot) {
      dup_name = strdup(sym_name);
      int dot_pos = fld_dot - sym_name;
      fld_name = dup_name + dot_pos + 1;
      dup_name[dot_pos] = '\0';
      sym_name = dup_name;
    }
    options = DMGL_PARAMS|DMGL_ANSI|DMGL_TYPES;
    if (PU_java_lang(Get_Current_PU()))
      options |= DMGL_JAVA;
    p = cplus_demangle(sym_name, options);
    if(fld_dot) {
      if(p) {
        char *comb_name = (char *)malloc(strlen(p) + strlen(fld_name) + 2);
        strcpy(comb_name, p);
        strcat(comb_name, ".");
        strcat(comb_name, fld_name);
        free(p);
        p = comb_name;
      }
      if(dup_name) {
        free(dup_name);
      }
      sym_name = orig_name;
    }
    if (p) {
      if (PU_java_lang(Get_Current_PU())) {
        char * pch;
        pch = strstr (p, ")");
        if (pch && strlen(pch) > 1) {
          strncpy (pch + 1, "\000\000", 1);
        }
      }
      return p;
    }
  }
  if (append_paren) {
    // C function or demangle failed. append "()" to end
    p = (char*)malloc(len + 4);
    memcpy(p, sym_name, len);
    memcpy(p+len, "()", 3);  // include last '\0'
    return p;
  }
  return strdup(sym_name);
}

char *
Vsa_sym_name(AUX_STAB_ENTRY* sym)
{
  if (sym->Is_preg()) {
    PREG_NUM preg = sym->St_ofst();
    // try preg home at first
    WN* preg_home = Preg_Home(preg);
    if (preg_home != NULL && WN_has_sym(preg_home) && WN_st(preg_home) != NULL)
      return ST_name(WN_st(preg_home));
    // try st the preg is assigned to
    ST_IDX st = Preg_Assign_To(preg);
    if (st != ST_IDX_ZERO) {
      return ST_name(st);
    }
  }
  return sym->St() ? ST_name(sym->St()) : (char*)"$nosymbol";
}

const char*
Vsa_spos_key(char *buf, INT len, SRCPOS cur_pos, SRCPOS entry_spos, INLCXT* inlcxt)
{
  const char* ptr = buf;
  SRCPOS spos = cur_pos;
  while (inlcxt != NULL) {
    ST_IDX st = inlcxt->Inlcxt_call_st();
    SRCPOS inl_entry = ST_Srcpos(st);
    if (inl_entry != 0) {
      INT ret = snprintf(buf, len, "@%x%.*s",
                                   SRCPOS_linenum(spos) - SRCPOS_linenum(inl_entry),
                                   ISKEY_PATH_FUNC_LEN, ST_name(st));
      buf += ret;
      len -= ret;
      spos = inlcxt->Inlcxt_line_num();
    }
    inlcxt = inlcxt->Parent();
  }
  if (len > 3)
    snprintf(buf, len, "@%x",
                       SRCPOS_linenum(spos) - SRCPOS_linenum(entry_spos));
  return ptr;
}

const char*
Vsa_issue_key(char* buf, INT len, INT fileno, const char* fname, const char* vname, const char* aname, const char* path, const char* cname, const char* rname)
{
  // file name
  char *ptr = buf;
  Get_Global_File_Full_Path(fileno, ptr, len);
  INT real_len = strlen(ptr);
  if (real_len > ISKEY_MAX_FILE_LEN) {
    memmove(ptr, ptr + real_len - ISKEY_MAX_FILE_LEN, ISKEY_MAX_FILE_LEN);
    real_len = ISKEY_MAX_FILE_LEN;
  }
  // @ function name
  ptr[real_len] = '@';
  real_len += 1;
  INT func_len = strlen(fname);
  if (func_len > ISKEY_MAX_FUNC_LEN) {
    func_len = ISKEY_MAX_FUNC_LEN;
  }
  memcpy(&ptr[real_len], fname, func_len);
  real_len += func_len;
  // @ variable name
  ptr[real_len] = '@';
  real_len += 1;
  INT var_len = strlen(vname);
  if (var_len > ISKEY_MAX_VAR_LEN) {
    var_len = ISKEY_MAX_VAR_LEN;
  }
  memcpy(&ptr[real_len], vname, var_len);
  real_len += var_len;
  // @ ana type
  ptr[real_len] = '@';
  real_len += 1;
  INT ana_len = strlen(aname);
  if (ana_len > ISKEY_MAX_ANA_LEN) {
    ana_len = ISKEY_MAX_ANA_LEN;
  }
  memcpy(&ptr[real_len], aname, ana_len);
  real_len += ana_len;
  // @ category type
  ptr[real_len] = '@';
  real_len += 1;
  INT cat_len = strlen(cname);
  if (cat_len > ISKEY_MAX_CAT_LEN) {
    cat_len = ISKEY_MAX_CAT_LEN;
  }
  memcpy(&ptr[real_len], cname, cat_len);
  real_len += cat_len;
  // @ rule name
  ptr[real_len] = '@';
  real_len += 1;
  INT rule_len = strlen(rname);
  if (rule_len > ISKEY_MAX_RULE_LEN) {
    rule_len = ISKEY_MAX_RULE_LEN;
  }
  memcpy(&ptr[real_len], rname, rule_len);
  real_len += rule_len;
  // @ line offset
  INT path_len = strlen(path);
  if (path_len > ISKEY_MAX_PATH_LEN)
    path_len = ISKEY_MAX_PATH_LEN;
  memcpy(&ptr[real_len], path, path_len);
  real_len += path_len;
  ptr[real_len] = '\0';
  return ptr;
}

static INT  def_issue_count;
static INT* vsa_issue_count;
void
Vsa_json_print(const char* category, const char* certainty, SRCPOS spos,
               const char *aname, const char *vname, const char *pname,
               const char *subcode, const char *key,
               SRCPOS_HANDLE *srcpos_h, const char *cname)
{
  Is_True(RFile != NULL && VSA_Output_Json, ("Wrong output file or format"));
  const char* ruleset  = "";
  char        vulname[64];
  if (strcmp(cname, "CERT") == 0) {
    cname = "STANDARD";
    ruleset = "CERT";
    snprintf(vulname, 63, "CERT-%s-%s", subcode, certainty);
    aname = subcode;
  }
  else {
    if (*cname == '\0')
      cname = "BUILTIN";
    ruleset = "VULNERABILITY";
    snprintf(vulname, 63, "%s-%s", aname, certainty);
    if (strcmp(aname, "RBC") == 0) {
      aname = subcode;
    }
    else {
    }
  }

  if (vsa_issue_count == NULL) {
    WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
    if (mgr != NULL && VSA_Single_Report == FALSE)
      vsa_issue_count = (INT*)calloc(mgr->Get_file_count() + 1, sizeof(INT));
    else
      vsa_issue_count = &def_issue_count;
  }

  UINT file_idx = VSA_Single_Report ? 0 : File_Index;

  if (vsa_issue_count[file_idx] > 0)
    fprintf(RFile, ",\n");
  ++ vsa_issue_count[file_idx];

  fprintf(RFile, "  { \"fileId\":\"%d\", \"lineNo\":%d, \"columnNo\":%d, \"key\":\"%s\", "
                     "\"category\":\"%s\", \"ruleSet\":\"%s\", \"errorCode\":\"%s\", \"certainty\":\"%s\", "
                     "\"vulnerableName\":\"%s\", \"severity\":\"%s\", "
                     "\"variableName\":\"%s\", \"functionName\":\"%s\", \"message\":\"@@issueMessage@@\", \n",
                 SRCPOS_filenum(spos), SRCPOS_linenum(spos), SRCPOS_column(spos), key,
                 cname, ruleset, aname, certainty,
                 vulname, "@@severity@@",
                 vname, pname);
  fprintf(RFile, "   \"tracePaths\": [\n");
  if (srcpos_h != NULL) {
    srcpos_h->Write_path_json(RFile, spos);
  }
  else {
    fprintf(RFile, "    { \"fileId\":\"%d\", \"lineNo\":%d, \"columnNo\":%d, \"message\":\"%d\", "
                   "\"variableName\":\"\", \"functionName\":\"\" }",
                SRCPOS_filenum(spos), Srcpos_To_Line(spos), SRCPOS_column(spos),
                PATHINFO_VUL_SPOT);
  }
  fprintf(RFile, "\n   ]\n");
  fprintf(RFile, "  }");
}

void
Vsa_error_print(const VSA_ISSUE* issue)
{
  if (!VSA_Maybe_Report && issue->Certainty() == IC_MAYBE)
    return;

  VSA_ISSUE_WRITTER* writter = VSA_ISSUE_WRITTER::Get_writter(RFile);
  writter->Print_issue(issue, RFile);
}

#if 0
// =============================================================================
// Print function for VSA_ISSUE
// =============================================================================
void
VSA_ISSUE::Print_text(FILE* fp) const
{
  // temporary implementationa
  BOOL is_builtin = strcmp(_rule_set, "BUILTIN") == 0;
  BOOL is_cert = strcmp(_rule_set, "CERT") == 0;
  BOOL is_user = strcmp(_rule_set, "USER") == 0;
  BOOL is_builtin_rbc = is_builtin && strcmp(_rule_code, "RBC") == 0;
  ARptMsg arptm(is_builtin ? _rule_code : "RBC",
                TRUE, FALSE, FALSE); // for type and category in report;
  char *dname_v = _variable_name ? Vsa_demangle(_variable_name) :
                                   (char*)"\"\"";
  char *dname_p = Vsa_demangle(_function_name);
  INT len = strlen(dname_v) + strlen(_rule_set) + 16;
  if (_error_code != NULL)
    len += strlen(_error_code);
  char *rbc_msg = (char*)malloc(len);
  rbc_msg[0] = '\0';
  Is_True(_rule_set != NULL, ("_rule_set not set"));
  if (is_builtin_rbc) {
    strcat(rbc_msg, _rule_set);
    strcat(rbc_msg, "],[");
  }
  if (!is_builtin && !is_builtin_rbc) {
    const char* msg = (is_cert || is_user) ? _rule_set : _rule_code;
    if (msg != NULL && *msg != '\0' && strcmp(msg, "RBC") != 0) {
      strcat(rbc_msg, msg);
      strcat(rbc_msg, "],[");
    }
  }
  if (_error_code != NULL && *_error_code != '\0') {
    strcat(rbc_msg, _error_code);
    strcat(rbc_msg, "],[");
  }
  strcat(rbc_msg, dname_v);

  char *path = _srcpos_h != NULL ? _srcpos_h->Compose_path_string()
                                 : NULL;
  INT ecode = _certainty == IC_MAYBE ? EC_VSA_Rbc_may_violation : EC_VSA_Rbc_violation;
  if (path) {
    ErrMsgSrcpos(ecode, _start_spos,
                 arptm.getCat(), rbc_msg, dname_p, path);
    free(path);
  }
  else {
    ErrMsgSrcpos(ecode, _start_spos,
                 arptm.getCat(), rbc_msg, dname_p, "");
  }

  if (dname_v)
    free(dname_v);
  if (dname_p)
    free(dname_p);
}

void
VSA_ISSUE::Print_json(FILE* fp) const
{
  // temporary implementation
  char *dname_v = _variable_name ? Vsa_demangle(_variable_name) :
                                   (char*)"\"\"";
  char *dname_p = Vsa_demangle(_function_name);

  Vsa_json_print(_rule_set,
                 ISSUE_CERTAINTY_name((ISSUE_CERTAINTY)_certainty),
                 _start_spos,
                 _rule_code,
                 dname_v,
                 dname_p,
                 _error_code,
                 _issue_key,
                 _srcpos_h,
                 _rule_set);
  if (dname_v)
    free(dname_v);
  if (dname_p)
    free(dname_p);
}
#endif

//typedef mempool_allocator<INTENT_DESC*> INTENT_D_ALLOCATOR;
//typedef vector<INTENT_DESC*, INTENT_D_ALLOCATOR> INTENT_D_ALLOCATOR;


ITENT_DESC ITENT_INTERNAL_DESC[] = {
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "kmalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "xmalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "mm_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "dlmalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "jemalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "__kmalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "calloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|BLOCK, "mmap" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|BLOCK, "munmap" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "_Znwm" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "_Znam" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "_F90_ALLOCATE" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, ALLOC|DEALLOC|HEAP|ALLOCINIT, "xrealloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, ALLOC|DEALLOC|HEAP|ALLOCINIT, "krealloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, ALLOC|DEALLOC|HEAP|ALLOCINIT, "__krealloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, ALLOC|DEALLOC|HEAP|ALLOCINIT, "realloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "free" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "kfree" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "kzfree" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "xfree" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "_ZdlPv" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "_ZdaPv" },
  { MEM_ACC,    PTR_VAR, NO_ALIAS,   BLOCK, "__morecore" },
  // alios specfic malloc/free
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "zalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "aos_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "aos_zalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "os_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "os_zalloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "aos_free" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "os_free" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "tcp_seg_free" },
  // alios specific malloc/free for pca10056
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "krhino_mm_alloc" },
  //{ MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "k_mm_alloc" },   // k_mm internal use only
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "krhino_mm_free" },
  //{ MEM_ACC,    PROC,    ALIAS_VSYM,   DEALLOC|HEAP, "k_mm_free" },    // k_mm internal use only
  // alios specific malloc/free for mk3060
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "mem_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "memp_malloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "mem_free" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "memp_free" },
  // alios specific malloc/free for csky
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "mm_memalign" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "mm_calloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "mm_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "mm_zalloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "kmm_memalign" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "kmm_calloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "kmm_malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "kmm_zalloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "mm_free" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "kmm_free" },
  // special malloc/free for cJSON library
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "cJSON_malloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "cJSON_free" },
  // string manipulation function
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "strdup" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "strndup" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP|ALLOCINIT, "__strdup" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_ZN4java4lang6ObjectC1Ev" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_ZN4java4lang7Runtime10getRuntimeEJPS1_v" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_ZN4java4lang6Object8getClassEJPNS0_5ClassEv" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_ZN4java4util7HashMap3getEJPNS_4lang6ObjectES4_" },
  { PROC_EFF,   PROC,    NO_ALIAS,   NORETURN, "exit"    },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "stdout" },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "stdin" },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "stderr" },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "_ZN4java4lang6System3errE" },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "_ZN4java4lang6System2inE" },
  { PROC_EFF,   PTR_VAR, NO_ALIAS,   CAT_NONE, "_ZN4java4lang6System3outE" },
  { PROC_EFF,   PROC,    NO_ALIAS,   CAT_NONE, "printk" },
  // { PROC_EFF,   PROC,    NO_ALIAS,   CAT_NONE, "printf" },
  { PROC_EFF,   PROC,    MOD_PARM_0, CAT_NONE, "sprintf" },
  { PROC_EFF,   PROC,    MOD_PARM_0, CAT_NONE, "snprintf" },
  { PROC_EFF,   PROC,    NO_ALIAS,   CAT_NONE, "fprintf" },
  { PARA_EFF,   PROC,    ALIAS_WOPT, TAINTED|PARMVAR, "fscanf" },
  { PARA_EFF,   PROC,    ALIAS_WOPT, TAINTED|PARMVAR, "__isoc99_fscanf" },
  { PARA_EFF,   PROC,    ALIAS_WOPT, TAINTED|PARMVAR, "scanf" },
  { PARA_EFF,   PROC,    ALIAS_WOPT, TAINTED|PARMVAR, "__isoc99_scanf" },
  { PARA_EFF,   PROC,    MOD_PARM_0, TAINTED|PARMVSYM, "fread" },
  { PARA_EFF,   PROC,    MOD_PARM_1, TAINTED|PARMVSYM, "read" },
  { PARA_EFF,   PROC,    MOD_PARM_1, TAINTED|PARMVSYM, "recv" },
  { PARA_EFF,   PROC,    MOD_PARM_1|MOD_PARM_4, TAINTED|PARMVSYM, "recvfrom" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "atoi" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "atol" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "atoll" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "strtol" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "strtoll" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "atof" },
  //{ PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "strtod" },
  { PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "rand" },
  { PARA_EFF,   PROC,    NO_ALIAS,   TAINTED|PARMRET, "URAND31" },
  //{ IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, ".preg_" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, ".anon_" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, "_temp_" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, ".result_decl_" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, ".init" },    // struct initialization
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, "_ZTV" },       // C++ vtable
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, "/usr/include" },  // standard header path
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   PART_IGN, "/usr/local/include" },  // standard header path
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   REGEX_IGN, "_ZNSt8_Rb_treeI.+St4pairIK.+ESt10_Select1stIS\\w_ESt4lessI.+ESaIS\\w_EE22_M_emplace_hint_uniqueIJRKSt21piecewise_construct_tSt5tupleIJO.+_IJEEEEESt17_Rb_tree_iteratorIS\\w_ESt23_Rb_tree_const_iteratorIS\\w_EDpOT_$"}, // part of mangled func name _M_emplace_hint_unique which is called in std::map operator[] in c++/5/bits/stl_map.h
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   REGEX_IGN, "^_ZNSt3mapI.+St4lessI.+ESaISt4pairIK.+EEEixE.+"}, // part of mangled func name of std::map operator[] in c++/5/bits/stl_map.h
  { IGNORE_RES, PTR_VAR, NO_ALIAS,   IGN, "stdin" },
  { IGNORE_RES, PTR_VAR, NO_ALIAS,   IGN, "stdout" },
  { IGNORE_RES, PTR_VAR, NO_ALIAS,   IGN, "stderr" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   IGN, "errno" },
  { IGNORE_RES, VAL_VAR, NO_ALIAS,   IGN, "__Exc_Ptr__" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "setuid" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "getuid" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "atexit" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "system" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "ptrace" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "dup" },
  { CHK_RET,    VAL_VAR, NO_ALIAS,   SYS_RSRC, "dup2" },

  // rongcard specific builtin function
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "TEE_Malloc" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|DEALLOC|HEAP, "TEE_Realloc" },
  { MEM_ACC,    PROC,    ALIAS_VSYM, DEALLOC|HEAP, "TEE_Free" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "sm_strdup" },
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "sm_strndup" },

  // cipher
  { MEM_ACC,    PROC,    NO_ALIAS,   ALLOC|HEAP, "sh_malloc" },

  // intrinsic
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_AllocObjectNoFinalizer" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_NewPrimArray" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_NewObjectArray" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_InitClass" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_NewMultiArray" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_ThrowAbstractMethodError" },
  { MEM_ACC,    PROC,    NO_ALIAS,   CAT_NONE, "_Jv_CheckCast" },

  // stl intrinsic
  { PARA_EFF,    PROC,   MOD_PARM_0|REF_PARM_0|REF_PARM_1, PARMVSYM, "_ZNSt3__14list9push_backERKT_"},
  { PARA_EFF,    PROC,   MOD_PARM_0|REF_PARM_0|REF_PARM_1|REF_PARM_3, PARMVSYM, "_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_9size_typeENS5_15const_referenceE"},
};


static BOOL
Is_func_alloc(ITENT_DESC* it, const char* name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != MEM_ACC)
    return FALSE;
  
  if (strcmp(it->Get_name(), name) == 0) {
    if (it->Get_hdl() == PROC) {
      if ((it->Get_cat() & ALLOC) != 0)
	return TRUE;
    }
  }
  return FALSE;
}


static BOOL
Is_func_dealloc(ITENT_DESC* it, const char* name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != MEM_ACC)
    return FALSE;
  
  if (strcmp(it->Get_name(), name) == 0) {
    if (it->Get_hdl() == PROC) {
      INT mc = it->Get_MEM_cat();
      if ((mc & DEALLOC) != 0)
	return TRUE;
    }
  }
  return FALSE;
}

static BOOL
Is_func_alloc_initialized(ITENT_DESC* it, const char* name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != MEM_ACC)
    return FALSE;
  
  if (strcmp(it->Get_name(), name) == 0) {
    if (it->Get_hdl() == PROC) {
      if ((it->Get_cat() & ALLOC) != 0) {
        if ((it->Get_cat() & ALLOCINIT) != 0) {
          return TRUE;
        }
      }
    }
  }
  return FALSE;
}


static TAINTMODEL
Is_func_arg_tainted(ITENT_DESC* it, const char* name, IDTYPE* which_arg)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != PARA_EFF)
    return TM_NONE;
  
  if (strcmp(it->Get_name(), name) == 0) {
    if (it->Get_hdl() == PROC) {
      if ((it->Get_cat() & TAINTED) != 0) {
        switch (it->Get_cat() & TAINT_PARM_MASK) {
        case PARMRET:
          (*which_arg = TM_ARG_RET);
          return TM_ARG_RET;
        case PARMREG:
          (*which_arg = TM_ARG_REG);
          return TM_ARG_REG;
        case PARMVAR:
          (*which_arg = TM_ARG_VAR);
          return TM_ARG_VAR;
        case PARMVSYM:
          (*which_arg = TM_ARG_VSYM);
          return TM_ARG_VSYM;
        default:
          break;
        }
      }
    }
  }
  return TM_NONE;
}


// mark func or intrinsic has no side effect or not
// if func has no side effect, will purge chi list after rename codemap
static BOOL
Is_func_nosideff(ITENT_DESC* it, const char* name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != MEM_ACC)
    return FALSE;
  
  if (strcmp(it->Get_name(), name) == 0) {
    if (it->Get_hdl() == PROC) {
      return it->Get_alias() == NO_ALIAS;
    }
  }
  return FALSE;
}


static BOOL
Is_sym_ignore(ITENT_DESC* it, const char *name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != IGNORE_RES)
    return FALSE;

  INT len = strlen(it->Get_name());
  if (strncmp(name, it->Get_name(), len) == 0) {
    const char *rem_str = name + len;
    if (*rem_str == '\0' ||                             // exact match
        *rem_str == '.'  ||                             // match [ignore].XX
        (*rem_str == '-' && *(rem_str + 1) == '>')) {   // match [ignore]->XX
      if ((it->Get_hdl() == PTR_VAR) || (it->Get_hdl() == VAL_VAR) ) {
        INT mc = it->Get_IGNORE_cat();
        if ((mc & IGN) != 0) {
          return TRUE;
        }
      }
    }
  }
  return FALSE;
}


static BOOL
Is_part_sym_ignore(ITENT_DESC* it, const char *name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != IGNORE_RES)
    return FALSE;
  // we assume partial name is always prefix
  INT len = strlen(it->Get_name());
  if (strncmp(it->Get_name(), name, len) == 0) {
    if ((it->Get_hdl() == PTR_VAR) || (it->Get_hdl() == VAL_VAR) ) {
      INT mc = it->Get_IGNORE_cat();
      if ((mc & PART_IGN) != 0)
	return TRUE;
    }
  }

  return FALSE;
}

// Return TRUE if 'name' match with 'it', else return FALSE
BOOL
Is_sym_regex_ignore(ITENT_DESC* it, const char *name)
{
  BOOL ret = FALSE;
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != IGNORE_RES ||
      (it->Get_IGNORE_cat() & REGEX_IGN) == 0)
    return FALSE;
  regex_t reg;
  regmatch_t dummy[1];
  int reti = 0;
  reti = regcomp(&reg, it->Get_name(), REG_EXTENDED | REG_NOSUB | REG_NEWLINE);
  if (!reti) {
    reti = regexec(&reg, name, 1, dummy, 0);
    if (!reti) {
      if ((it->Get_hdl() == VAL_VAR))
        ret = TRUE;
    }
    regfree(&reg);
  }

  return ret;
}

BOOL
Vsa_check_regex_sym_ignore(const char *name)
{
  ITENT_DESC intent;
  if(!name)
    return FALSE;
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i];
    if (Is_sym_regex_ignore(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}

// TODO: this function can be more efficient
// all ignored symbol pattern can be defined by one regular expression
BOOL
Vsa_check_sym_ignore(const char *name)
{
  ITENT_DESC intent;
  if(!name)
    return FALSE;
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_sym_ignore(&intent, name) || (Is_part_sym_ignore(&intent, name))) {
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
Vsa_check_path_ignore(const char *path)
{
  ITENT_DESC intent;
  if(!path)
    return FALSE;
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i];
    if (Is_part_sym_ignore(&intent, path)) {
      return TRUE;
    }
  }
  return FALSE;
}


BOOL
Vsa_check_free_heap(const char *name)
{
  ITENT_DESC intent;
  
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_func_dealloc(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}


BOOL
Vsa_check_alloc_heap(const char *name)
{
  ITENT_DESC intent;
  
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_func_alloc(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
Vsa_check_alloc_initialized(const char *name)
{
  ITENT_DESC intent;
  
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_func_alloc_initialized(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}

BOOL
Vsa_check_no_sideffect(const char *name)
{
  ITENT_DESC intent;
  
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_func_nosideff(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}

ALIAS_INTENT
Vsa_check_alias_intent(const char *name)
{
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    if (strcmp(ITENT_INTERNAL_DESC[i].Get_name(), name) == 0) {
      return ITENT_INTERNAL_DESC[i].Get_alias();
    }
  }
  return ALIAS_WOPT;
}

TAINTMODEL
Vsa_check_func_arg_tainted(const char *name, IDTYPE* which_arg)
{
  ITENT_DESC intent;
  
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    TAINTMODEL tm;
    if ((tm = Is_func_arg_tainted(&intent, name, which_arg)) != TM_NONE) {
      return tm;
    }
  }
  return TM_NONE;
}

FLD_HANDLE
Get_java_array_data_fld(CODEREP *cr)
{
  FLD_HANDLE handle;
  if(cr->Kind() == CK_VAR) {
    TY_IDX ty = cr->Lod_ty();
    if(TY_kind(ty) == KIND_POINTER) {
      TY_IDX arr_ty = TY_pointed(ty);
      if(TY_is_array_class(arr_ty)) {
        // get data info at fld 5 (fixed id in JFE)
        UINT field_id = 0;
        const UINT data_fld_id = 5;
        return FLD_get_to_field(arr_ty, data_fld_id, field_id);
      }
    }
  }
  return FLD_HANDLE();
}

CODEREP*
Vsa_malloc_size_opnd(STMTREP* call, CODEREP *ret_cr, COMP_UNIT *cu)
{
  // TODO: for INTRN ARRAY, return the size opnd
  if(ret_cr && call->Opr() == OPR_INTRINSIC_CALL) {
    INTRINSIC intrinsic = call->Rhs()->Intrinsic();
    // Calculate allocate size by elem_cnt * elem_size + data_ofst + padding
    if ((PU_src_lang(Get_Current_PU()) & PU_JAVA_LANG)) {
      CODEREP *elem_cnt = NULL;
      if (intrinsic == INTRN_NEW_PRIM_ARR)
        elem_cnt = call->Rhs()->Opnd(1)->Ilod_base();
      else if (intrinsic == INTRN_NEW_OBJ_ARR)
        elem_cnt = call->Rhs()->Opnd(0)->Ilod_base();
      else
        return NULL;
      // TODO: handle variable elem cnt
      if(elem_cnt != NULL && 
         elem_cnt->Kind() == CK_CONST && 
         ret_cr->Kind() == CK_VAR) {
        FLD_HANDLE fld = Get_java_array_data_fld(ret_cr);
        Is_True(!fld.Is_Null(), ("unable to get fld info from java alloc ret cr"));
        TY_IDX data_ty = FLD_type(fld);
        UINT64 ofst = FLD_ofst(fld);
        Is_True(TY_kind(data_ty) == KIND_ARRAY, ("data is not array type"));
        // get element info
        TY_IDX elem_ty = TY_etype(data_ty);
        UINT64 elem_size = TY_size(elem_ty);
        // compute the padding
        UINT64 padding = 0;
        if(elem_size >= Pointer_Size && ofst % Pointer_Size != 0) {
          padding = Pointer_Size - (ofst % Pointer_Size);
        }
        UINT64 alloc_size = ofst + (elem_cnt->Const_val() * elem_size) + padding;
        CODEREP *tmp_size = Alloc_stack_cr(0);
        tmp_size->Init_const(MTYPE_I8, alloc_size);
        CODEREP *size_cr = cu->Htable()->Rehash(tmp_size);
        return size_cr;
      }
      return NULL;
      // TODO:
      // _Jv_NewMultiArray
    } else {
      return NULL;
    }
  } else if (call->Opr() == OPR_CALL && strcmp(ST_name(call->St()), "memp_malloc") == 0) {
    return NULL;
  } else if (call->Opr() == OPR_CALL &&
             (strcmp(ST_name(call->St()), "realloc") == 0 ||
              strcmp(ST_name(call->St()), "strndup") == 0 ||
              strcmp(ST_name(call->St()), "TEE_strndup") == 0)) {
    return call->Rhs()->Kid_count() > 1 ?
           call->Rhs()->Opnd(1)->Ilod_base() : NULL;
  } else if (call->Rhs()->Kid_count() > 0) {
    CODEREP *opnd = call->Rhs()->Opnd(0)->Ilod_base();
    if (opnd->Kind() == CK_LDA) {
      CODEREP *tmp_size = Alloc_stack_cr(0);
      tmp_size->Init_const(MTYPE_I8, ST_size(opnd->Lda_base_st()));
      opnd = cu->Htable()->Rehash(tmp_size);
    }
    return opnd;
  } else {
    return NULL;
  }
}

IDTYPE
Vsa_free_ptr_index(const char* name)
{
  if (strcmp(name, "memp_free") == 0)
    return 1;  // memp_free frees opnd 1, where opnd 0 is the pool
  else
    return 0;  // assume always free opnd 0
}

CODEREP*
Vsa_free_ptr_opnd(STMTREP* call)
{
  IDTYPE index = Vsa_free_ptr_index(ST_name(call->St()));
  return call->Rhs()->Opnd(index)->Ilod_base()->Find_actual_arg();
}

BOOL
Is_alloc_not_initialized(STMTREP* call)
{
  if (call->Opr() != OPR_CALL)
    return FALSE;
  const char* name = ST_name(call->St());
  return !Vsa_check_alloc_initialized(name);
#if 0
  if (strstr(name, "malloc") != NULL)
    return TRUE;
  return FALSE;
#endif
}

CODEREP *
Is_new_thread_created(STMTREP *call)
{
  if (call->Opr() != OPR_CALL)
    return NULL;
  const char* name = ST_name(call->St());
  if (strcmp(name, "pthread_create") == 0) {
    // POSIX thread
    // pthread_create(pthread_t, const pthread_attr_t *,
    //                void *(*start_routine)(void *), void *arg)
    Is_True_Ret(call->Rhs()->Kind() == CK_OP &&
                call->Rhs()->Opr() == OPR_CALL &&
                call->Rhs()->Kid_count() == 4,
                ("pthread_create param mismatch"), NULL);
    Is_True_Ret(call->Rhs()->Opnd(2) != NULL &&
                call->Rhs()->Opnd(2)->Kind() == CK_IVAR &&
                call->Rhs()->Opnd(2)->Opr() == OPR_PARM,
                ("bad pthread_create opnd 3"), NULL);
    return call->Rhs()->Opnd(2)->Ilod_base();
  }
  else if (strcmp(name, "thrd_create") == 0) {
    // C11 thread
    // thrd_create(thrd_t *, thrd_start_t, void *)
    Is_True_Ret(call->Rhs()->Kind() == CK_OP &&
                call->Rhs()->Opr() == OPR_CALL &&
                call->Rhs()->Kid_count() == 3,
                ("thrd_create param mismatch"), NULL);
    Is_True_Ret(call->Rhs()->Opnd(1) != NULL &&
                call->Rhs()->Opnd(1)->Kind() == CK_IVAR &&
                call->Rhs()->Opnd(1)->Opr() == OPR_PARM,
                ("bad thrd_create opnd 2"), NULL);
    return call->Rhs()->Opnd(1)->Ilod_base();
  }
  else if (strncmp(name, "_ZNSt6threadC1IRFv", 18) == 0) {
    // std::thread t1(threadfunc);
    // TODO: c++ std::thread intrinsic
    // TODO: std::thread t1(std::bind(&A::threadfunc,&a));
    // TODO: std::thread t1(functor::operator()());
    // TODO: std::thread t1([...]{...});
    Is_True_Ret(call->Rhs()->Kind() == CK_OP &&
                call->Rhs()->Opr() == OPR_CALL &&
                call->Rhs()->Kid_count() > 2,
                ("thrd_create param mismatch"), NULL);
    Is_True_Ret(call->Rhs()->Opnd(1) != NULL &&
                call->Rhs()->Opnd(1)->Kind() == CK_IVAR &&
                call->Rhs()->Opnd(1)->Opr() == OPR_PARM,
                ("bad thrd_create opnd 2"), NULL);
    return (call->Rhs()->Kid_count() > 2) ?
             call->Rhs()->Opnd(1)->Ilod_base() : NULL;
  }
  else if (strcmp(name, "kthread_create") == 0) {
    // kernel thread
    // kthread_create(int (*threadfn(void *data), void *data,
    //                     const char namefmt[], ...)
    Is_True_Ret(call->Rhs()->Kind() == CK_OP &&
                call->Rhs()->Opr() == OPR_CALL &&
                call->Rhs()->Kid_count() >= 3,
                ("kthread_create param mismatch"), NULL);
    Is_True_Ret(call->Rhs()->Opnd(0) != NULL &&
                call->Rhs()->Opnd(0)->Kind() == CK_IVAR &&
                call->Rhs()->Opnd(0)->Opr() == OPR_PARM,
                ("bad kthread_create opnd 1"), NULL);
    return call->Rhs()->Opnd(0)->Ilod_base();
  }
  return NULL;
}

CODEREP *
Is_register_signal_handler(STMTREP *call)
{
  if (call->Opr() != OPR_CALL)
    return NULL;
  const char *name = ST_name(call->St());
  if (strcmp(name, "signal") == 0) {
    // sighandler_t signal(int signum, sighandler_t handler)
    Is_True_Ret(call->Rhs()->Kind() == CK_OP &&
                call->Rhs()->Opr() == OPR_CALL &&
                call->Rhs()->Kid_count() == 2, ("signal param mismatch"), NULL);

    Is_True_Ret(call->Rhs()->Opnd(1) != NULL &&
                call->Rhs()->Opnd(1)->Kind() == CK_IVAR &&
                call->Rhs()->Opnd(1)->Opr() == OPR_PARM,
                ("bad signal opnd 2"), NULL);
    return call->Rhs()->Opnd(1)->Ilod_base();
  }
  return NULL;
}

static UINT64
str_to_uint64(const char *str, char **end) {
  if (str == NULL)
    return 0;
  if (str[0] == '0' && (str[1] == 'x' || str[1] == 'X'))
    return strtoll(str, end, 16);
  else if (str[0] == '0')
    return strtoll(str, end, 8);
  else
    return strtoll(str, end, 10);
}

BOOL
Is_const_addr_safe(UINT64 addr) {
  static BOOL initialized;
  typedef std::vector< std::pair<UINT64, UINT64> > REG_MAP;
  static REG_MAP reg_map;

  // initialize the register map
  if (!initialized) {
    OPTION_LIST *ol;
    for (ol = VSA_Reg_Map; ol != NULL; ol = OLIST_next(ol)) {
      // '-' for a range of [start, end], ',' to seperate different ranges
      // example: start1-end1,start2-end2,start3,start4
      const char *start = OLIST_val(ol);
      char *end = NULL;
      while (start && *start) {
        UINT64 saddr = str_to_uint64(start, &end);
        if (saddr <= 0 || end == NULL)
          break;  // ignore this option
        UINT64 eaddr = saddr;
        if (*end == '-')
          eaddr = str_to_uint64(end + 1, &end);
        if (eaddr == 0 || eaddr < saddr)
          break;  // ignore this option
        // add to map
        reg_map.push_back(std::make_pair(saddr, eaddr));
        if (*end == ',')
          start = end + 1;
        else
          break;
      }
    }
    initialized = TRUE;
  }

  // check if addr is safe
  // TODO: binary search?
  REG_MAP::iterator end = reg_map.end();
  for (REG_MAP::iterator it = reg_map.begin(); it != end; ++it) {
    if (addr >= it->first && addr <= it->second)
      return TRUE;
  }
  return FALSE;
}


INTRINSIC
Get_call_intrinsic(STMTREP* call)
{
  OPERATOR opr = call->Opr();
  Is_True(OPERATOR_is_call(opr), ("not call"));
  if (opr == OPR_INTRINSIC_CALL)
    return call->Rhs()->Intrinsic();
  if (opr != OPR_CALL)
    return INTRINSIC_NONE;

  const char* name = ST_name(call->St());
  INT kid_count = call->Rhs()->Kid_count();
  if (kid_count == 3 &&
      strcmp(name, "memcmp") == 0)
    return INTRN_MEMCMP;
  if (kid_count == 3 &&
      strcmp(name, "memcpy") == 0)
    return INTRN_MEMCPY;
  if (kid_count == 3 &&
      strcmp(name, "memset") == 0)
    return INTRN_MEMSET;
  if (kid_count == 2 &&
      strcmp(name, "strcmp") == 0)
    return INTRN_STRCMP;
  if (kid_count == 2 &&
      strcmp(name, "strcpy") == 0)
    return INTRN_STRCPY;
  if (kid_count == 1 &&
      strcmp(name, "strlen") == 0)
    return INTRN_STRLEN;
  if (kid_count == 3 &&
      strcmp(name, "strncpy") == 0)
    return INTRN_STRNCPY;
  return INTRINSIC_NONE;
}

// =============================================================================
// Count_cr_kids: coount all cr kids
// =============================================================================
INT
Count_cr_kids(CODEREP *cr)
{
  Is_True(cr != NULL, ("null cr"));
  INT kid = 1;
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  case CK_VAR:
    break;
  case CK_IVAR:
    if (cr->Opr() == OPR_PARM) {
      // ignore PARM itself
      kid = Count_cr_kids(cr->Ilod_base());
    }
    else {
      kid += Count_cr_kids(cr->Ilod_base());
      if (cr->Opr() == OPR_MLOAD)
        kid += Count_cr_kids(cr->Mload_size());
    }
    break;
  case CK_OP:
    if (cr->Opr() == OPR_CVT || cr->Opr() == OPR_TAS ||
        cr->Opr() == OPR_CVTL) {
      kid = Count_cr_kids(cr->Opnd(0));
    }
    else {
      for (INT i = 0; i < cr->Kid_count(); ++i) {
        kid += Count_cr_kids(cr->Opnd(i));
      }
    }
    break;
  default:
    Is_True(FALSE, ("unknown cr kind"));
    break;
  }
  return kid;
}

// =============================================================================
// Is_var_no_sideffect: Vsa_check_var_no_sideffect helper function
// =============================================================================
static BOOL
Is_var_no_sideffect(ITENT_DESC* it, const char *name)
{
  FmtAssert((it!=NULL), ("Intent_desc ptr is nil"));
  if (it->Get_mtyp() != PROC_EFF)
    return FALSE;
  // we assume partial name is always prefix
  if ((it->Get_hdl() == PTR_VAR) || (it->Get_hdl() == VAL_VAR) ) {
    INT len = strlen(it->Get_name());
    if (strncmp(it->Get_name(), name, len) == 0) {
      return it->Get_alias() == NO_ALIAS;
    }
  }
  return FALSE;
}

// =============================================================================
// Vsa_check_var_no_sideffect: check if any function has no sideffect to variable 
// TODO: remove the chi to var from all function call
// =============================================================================
BOOL
Vsa_check_var_no_sideffect(const char *name)
{
  ITENT_DESC intent;
  for (int i=0; i < sizeof(ITENT_INTERNAL_DESC)/sizeof(ITENT_DESC); i++) {
    intent = ITENT_INTERNAL_DESC[i]; 
    if (Is_var_no_sideffect(&intent, name)) {
      return TRUE;
    }
  }
  return FALSE;
}

// =============================================================================
// Is_initv_zero
// =============================================================================
static BOOL
Is_initv_zero_impl(INITV_TAB* initv_tab, TCON_TAB* tcon_tab, INITV_IDX initv_idx,
                   INT& cur_ofst, INT offset)
{
  INT size, repeat;
  while (initv_idx != 0 && cur_ofst <= offset) {
    const INITV& initv = (*initv_tab)[initv_idx];

    switch (INITV_kind(initv)) {
    case INITVKIND_BLOCK:
      {
        INITV_IDX blk_inv = INITV_blk(initv);
        BOOL ret = Is_initv_zero_impl(initv_tab, tcon_tab, blk_inv,
                                      cur_ofst, offset);
        if (cur_ofst > offset)
          return ret;
      }
      break;
    case INITVKIND_SYMOFF:
    case INITVKIND_SYMDIFF:
    case INITVKIND_LABEL:
      repeat = INITV_repeat1(initv);
      cur_ofst += Pointer_Size * repeat;
      if (cur_ofst > offset)
        return FALSE;  // symbol or label, can't be zero
      break;
    case INITVKIND_VAL:
      {
        const TCON& tcon = (*tcon_tab)[INITV_tc(initv)];
        repeat = INITV_repeat2(initv);
        if (TCON_ty(tcon) == MTYPE_STR) {
          size = TCON_str_len(tcon);
          cur_ofst += size * repeat;
          Is_True(cur_ofst < offset, ("TODO: tcon str"));
        }
        else {
          size = MTYPE_byte_size(TCON_ty(tcon));
          cur_ofst += size * repeat;
          if (cur_ofst > offset)
            return Targ_Is_Zero(tcon) || Targ_Is_Neg_Zero(tcon);
        }
      }
      break;
    case INITVKIND_ZERO:
      repeat = INITV_repeat2(initv);
      cur_ofst += MTYPE_byte_size(INITV_mtype(initv)) * repeat;
      if (cur_ofst > offset)
        return TRUE;
      break;
    case INITVKIND_ONE:
      repeat = INITV_repeat2(initv);
      cur_ofst += MTYPE_byte_size(INITV_mtype(initv)) * repeat;
      if (cur_ofst > offset)
        return FALSE;
      break;
    case INITVKIND_PAD:
      repeat = INITV_repeat1(initv);
      cur_ofst += INITV_pad(initv) * repeat;
      if (cur_ofst > offset)
        return TRUE;
      break;
    default:
      FmtAssert(FALSE, ("unexpected initv type"));
      break;
    }
    initv_idx = INITV_next(initv);
  }
  return FALSE;
}

BOOL
Is_initv_zero(UINT32 file_idx, INITV_IDX initv_idx, INT offset)
{
  INITV_TAB* initv_tab;
  TCON_TAB*  tcon_tab;
  if (file_idx == 0) {
    initv_tab = Initv_Table_ptr;
    tcon_tab = Tcon_Table_ptr;
  }
  else {
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(mgr != NULL, ("not in xfa mode"));
    initv_tab = mgr->Get_file(file_idx).Initv_tab();
    tcon_tab = mgr->Get_file(file_idx).Tcon_tab();
  }
  INT cur_offset = 0;
  return Is_initv_zero_impl(initv_tab, tcon_tab, initv_idx,
                            cur_offset, offset);
}


// =============================================================================
// Get_initv_from_offset
// =============================================================================
static BOOL
Get_initv_from_offset(INITV_TAB* initv_tab, TCON_TAB* tcon_tab, INITV_IDX initv_idx,
                   INT& cur_ofst, INT offset, INITV_IDX *n_initv_idx)
{
  INT size, repeat;
  while (initv_idx != 0 && cur_ofst <= offset) {
    const INITV& initv = (*initv_tab)[initv_idx];

    switch (INITV_kind(initv)) {
    case INITVKIND_BLOCK:
      {
        INITV_IDX blk_inv = INITV_blk(initv);
        BOOL ret = Get_initv_from_offset(initv_tab, tcon_tab, blk_inv,
                                         cur_ofst, offset, n_initv_idx);
        if (cur_ofst > offset)
          return ret;
      }
      break;
    case INITVKIND_SYMOFF:
    case INITVKIND_SYMDIFF:
    case INITVKIND_LABEL:
      repeat = INITV_repeat1(initv);
      cur_ofst += Pointer_Size * repeat;
      if (cur_ofst > offset) {
        *n_initv_idx = initv_idx;
        return TRUE;
      }
      break;
    case INITVKIND_VAL:
      {
        const TCON& tcon = (*tcon_tab)[INITV_tc(initv)];
        repeat = INITV_repeat2(initv);
        if (TCON_ty(tcon) == MTYPE_STR) {
          size = TCON_str_len(tcon);
          cur_ofst += size * repeat;
          Is_True(cur_ofst < offset, ("TODO: tcon str"));
        }
        else {
          size = MTYPE_byte_size(TCON_ty(tcon));
          cur_ofst += size * repeat;
          if (cur_ofst > offset) {
            *n_initv_idx = initv_idx;
            return TRUE;
          }
        }
      }
      break;
    case INITVKIND_ZERO:
      repeat = INITV_repeat2(initv);
      cur_ofst += MTYPE_byte_size(INITV_mtype(initv)) * repeat;
      if (cur_ofst > offset) {
        *n_initv_idx = initv_idx;
        return TRUE;
      }
      break;
    case INITVKIND_ONE:
      repeat = INITV_repeat2(initv);
      cur_ofst += MTYPE_byte_size(INITV_mtype(initv)) * repeat;
      if (cur_ofst > offset) {
        *n_initv_idx = initv_idx;
        return TRUE;
      }
      break;
    case INITVKIND_PAD:
      repeat = INITV_repeat1(initv);
      cur_ofst += INITV_pad(initv) * repeat;
      if (cur_ofst > offset) {
        *n_initv_idx = initv_idx;
        return TRUE;
      }
      break;
    default:
      FmtAssert(FALSE, ("unexpected initv type"));
      break;
    }
    initv_idx = INITV_next(initv);
  }
  return FALSE;
}

BOOL
Get_initv_from_offset(UINT32 file_idx, INITV_IDX initv_idx, INT offset, INITV_IDX *n_initv_idx)
{
  INITV_TAB* initv_tab;
  TCON_TAB*  tcon_tab;
  if (file_idx == 0) {
    initv_tab = Initv_Table_ptr;
    tcon_tab = Tcon_Table_ptr;
  }
  else {
    WHIRL_FILE_MANAGER *mgr = WHIRL_FILE_MANAGER::Get();
    Is_True(mgr != NULL, ("not in xfa mode"));
    initv_tab = mgr->Get_file(file_idx).Initv_tab();
    tcon_tab = mgr->Get_file(file_idx).Tcon_tab();
  }
  INT cur_offset = 0;
  return Get_initv_from_offset(initv_tab, tcon_tab, initv_idx,
                            cur_offset, offset, n_initv_idx);
}

// =============================================================================
// Find_base_pointer_load, to find the LDID node that carries the base
//      address of a pointer dereference.
// =============================================================================
CODEREP*
Find_base_pointer_load(CODEREP *x)
{
  CODEREP *retv = NULL;

  switch (x->Kind()) {
  case CK_LDA:
    return x;

  case CK_CONST:
  case CK_RCONST:
    break;

  case CK_VAR:
    if (Ty_Table[x->object_ty()].kind == KIND_POINTER)
      return x;
    break;

  case CK_IVAR:
    Is_True(x->Opr() != OPR_PARM, ("VSA::Find_base_pointer_load should never be here"));
    if (x->Opr() == OPR_PARM)  // Should never come here
      return NULL;
    else if (x->Ilod_base()->Kind() == CK_VAR)
      return x->Ilod_base();   // return Ilod_base unconditionally if it's var
                               // pointer type may missing due to type conv and copy propagation
    else {
      retv = Find_base_pointer_load(x->Ilod_base());
      // how do we deal with (* *p)?
      // we will still return p.
      return retv;
    }
    break;

  case CK_OP:
    if (x->Opr() == OPR_INTRINSIC_OP)  // some intrinsic op returns pointer?
      return NULL;
    for (INT32 i = 0; i < x->Kid_count(); i++) {
      // return immediately if we find the param reference
      CODEREP *opnd = x->Opnd(i);
      retv = Find_base_pointer_load(opnd);
      if (retv != NULL)
        return retv;
    }
    break;

  default:;
  }
  return NULL;
}

// =============================================================================
// Find_ilod_base, to find the base cr for ilod, which can be a
//  lda, var or ivar
//  return cr must have pointer type
// =============================================================================
CODEREP*
Find_ilod_base(CODEREP* x) {
  switch (x->Kind()) {
  case CK_LDA:
    Is_True(TY_kind(x->Lda_ty()) == KIND_POINTER, ("not pointer"));
    return x;
  case CK_VAR:
  case CK_IVAR:
    if (MTYPE_byte_size(x->Dtyp()) != MTYPE_byte_size(Pointer_Mtype))
      return NULL;
    if (TY_kind(x->object_ty()) == KIND_POINTER)
      return x;
    // special handling for va_list ap
    if (x->Dsctyp() == Pointer_Mtype && x->Dtyp() == Pointer_Mtype &&
        TY_kind(x->lod_addr_ty()) == KIND_POINTER &&
        strstr(TY_name(TY_pointed(x->lod_addr_ty())), "va_list") != NULL)
      return x;
    break;
  case CK_OP:
    if (MTYPE_byte_size(x->Dtyp()) != MTYPE_byte_size(Pointer_Mtype))
      return NULL;
    if (x->Opr() == OPR_ADD || x->Opr() == OPR_BAND ||
        x->Opr() == OPR_BIOR) {
      CODEREP *opnd0 = x->Opnd(0);
      if (opnd0->Kind() == CK_LDA)
        return opnd0;
      BOOL is_opnd0_ptr = TY_kind(opnd0->object_ty() == KIND_POINTER);
      opnd0 = Find_ilod_base(opnd0);
      if (is_opnd0_ptr && opnd0)
        return opnd0;

      CODEREP *opnd1 = x->Opnd(1);
      if (opnd1->Kind() == CK_LDA)
        return opnd1;
      BOOL is_opnd1_ptr = (opnd1->object_ty() == KIND_POINTER);
      opnd1 = Find_ilod_base(opnd1);
      if (is_opnd1_ptr && opnd1)
        return opnd1;

      return opnd0 ? opnd0 : opnd1;
    }
    else if (x->Opr() != OPR_INTRINSIC_OP && x->Kid_count() > 0)
      return Find_ilod_base(x->Opnd(0));
    break;
  default:
    break;
  }
  return NULL;
}

// =============================================================================
// Find_ptr_defstmt, to find define statement for a pointer coderep, which
//  can be call or istore
//  if found, return the statement. otherwise NULL returned
// =============================================================================
STMTREP*
Find_ptr_defstmt(CODEREP* cr)
{
  STMTREP* def;
  INT i;
  Is_True(cr != NULL, ("invalid cr"));
  switch (cr->Kind()) {
  case CK_VAR:
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION) ||
        cr->Is_flag_set(CF_DEF_BY_PHI))
      break;
    def = cr->Defstmt();
    if (cr->Is_flag_set(CF_DEF_BY_CHI))
      return def;
    if (def && def->Rhs())
      return Find_ptr_defstmt(def->Rhs());
    break;
  case CK_OP:
    for (i = 0; i < cr->Kid_count(); ++i) {
      CODEREP* opnd = cr->Opnd(i);
      if (opnd->Kind() == CK_VAR &&
          TY_kind(opnd->object_ty()) != KIND_POINTER)
        continue;
      def = Find_ptr_defstmt(cr->Opnd(i));
      if (def != NULL)
        return def;
    }
    break;
  case CK_IVAR:
    if (TY_kind(cr->object_ty()) == KIND_POINTER)
      return cr->Ivar_defstmt();
    break;
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
  default:
    break;
  }
  return NULL;
}

// =============================================================================
// Get_vtab_st_from_class (Java ONLY)
//  give a java class st, return its vtable st
// =============================================================================
ST*
Get_vtab_st_from_class(ST* cls_st)
{
  Is_True(PU_java_lang(Get_Current_PU()), ("only for java"));
  Is_True(cls_st && ST_is_class_symbol(cls_st), ("only for class st"));
  TY_IDX cls_ty = ST_vtable_ty_idx(cls_st);
  Is_True(cls_ty != TY_IDX_ZERO && TY_kind(cls_ty) == KIND_STRUCT,
          ("cls ty is not struct"));
  ST_IDX sti = TY_vtable(cls_ty);
  Is_True(sti != ST_IDX_ZERO && ST_is_vtable(ST_ptr(sti)),
          ("cls ty does not have vtable"));
  return ST_ptr(sti);;
}

// =============================================================================
// Get_vtab_entry
// give a vtable st, return the entry according to the offset
// =============================================================================
ST*
Get_vtab_entry(ST * vst, int offset, BOOL is_call_offset)
{
  Is_True(ST_is_vtable(vst), ("st is not vtable"));
  INITO_IDX inito_idx = Find_INITO_For_Symbol(vst);
  // inito_idx is zero if vtable inito in another file
  if (inito_idx == 0) {
    return NULL;
  }
  const INITO& inito = Inito_Table[inito_idx];
  Is_True(INITV_kind(inito.val) == INITVKIND_BLOCK, ("Wrong vtable initv"));
  INITV_IDX initv_idx = INITV_blk(inito.val);
#if 0
  if (is_call_offset) {
    INITV_IDX result_idx = 0;
    INITV_IDX idx = initv_idx;
    for(int i=0; i <= offset ; idx = INITV_next(idx)) {
      if (!idx)
        return ST_IDX_ZERO;
      INITVKIND  kind = INITV_kind(idx);
      if (kind == INITVKIND_SYMOFF && ST_class(INITV_st(idx)) == CLASS_FUNC) {
        initv_idx = idx;
        i += Pointer_Size;
      }
    }
  }
  else {
    for (int i = 0; i < offset; i += Pointer_Size) {
      initv_idx = INITV_next(initv_idx);
      if (!initv_idx)
        return ST_IDX_ZERO;
    }
  }
#endif
  INT cur_ofst = 0;
  for ( ; initv_idx; initv_idx = INITV_next(initv_idx)) {
    if (cur_ofst == offset)
      break;
    Is_True(cur_ofst < offset, ("failed to find initv at offset"));
    cur_ofst += Get_INITV_Size(initv_idx);
  }
  // Java abstract method do not generate function symbol, put 0 as the initv value
  if(!PU_java_lang(Get_Current_PU())) {
    Is_True(INITV_kind(initv_idx) == INITVKIND_SYMOFF, ("Wrong INITV_kind for entry"));
  }
  if(INITV_kind(initv_idx) == INITVKIND_SYMOFF) {
    ST_IDX sti = INITV_st(initv_idx);
    return ST_ptr(sti);
  } else {
    return NULL;
  }
}

// =============================================================================
// Get_base_offset_from_vtable
// give a vtable st, return the base offset according to vtable
// =============================================================================
int
Get_base_offset_from_vtable(ST * vst) {
  Is_True(ST_is_vtable(vst), ("st is not vtable"));
  INITO_IDX inito_idx = Find_INITO_For_Symbol(vst);
  // inito_idx is zero if vtable inito in another file
  if (inito_idx == 0) {
    return 0;
  }
  const INITO& inito = Inito_Table[inito_idx];
  Is_True(INITV_kind(inito.val) == INITVKIND_BLOCK, ("Wrong vtable initv"));
  INITV_IDX initv_idx = INITV_blk(inito.val);
  INT cur_ofst = 0;
  for ( ; initv_idx; initv_idx = INITV_next(initv_idx)) {
    cur_ofst += Get_INITV_Size(initv_idx);
    if (INITV_kind(initv_idx) == INITVKIND_SYMOFF &&
        strncmp("_ZTI", ST_name(INITV_st(initv_idx)), 4) == 0)
      break;
  }
  return cur_ofst;
}

// ============================================================================
//
// Checking if the following pattern matches cr
// Which is and only is generated when -Wf,-devirtIndicator given to FE and
// The PU is in Java lang
//
//
//>   ...     <------- Original Value
//>  U8PARM
//>   LDA U8  <------- UTF_constant_function_name
//>  U8PARM
//> U8INTRINSIC_OP 1667
//
// ============================================================================
BOOL
Is_valid_lookup_virt_op(CODEREP *cr) {
  // These conditions must be met:
  // 1. Must a an Operator coderep
  // 2. Must be an intrinsic OP
  // 3. Must be LOOKUP_VIRT_FUNC
  // 4. Has == 2 operands
  // 5. Has == 2 non-NULL operands
  // 6. Has 2nd operand that is a LDA
  if (cr->Kind() == CK_OP &&
      cr->Opr() == OPR_INTRINSIC_OP &&
      cr->Intrinsic() == INTRN_LOOKUP_VIRT_FUNC) {
    // Try to get the two components and verify them are not NULLPTR.
    Is_True (cr->Kid_count() == 2, ("LOOKUP_VIRT_FUNC should have two operands."));
    CODEREP *orig_target_param = cr->Opnd(0);
    CODEREP *func_name_param = cr->Opnd(1);
    Is_True (orig_target_param != NULL && func_name_param != NULL,
             ("LOOKUP_VIRT_FUNC should have the first two params as non-NULL values"));
    
    Is_True (func_name_param->Kind() == CK_IVAR &&
             func_name_param->Ilod_base() != NULL &&
             func_name_param->Ilod_base()->Kind() == CK_LDA,
             ("LOOKUP_VIRT_FUNC should have a LDA as second operand.", (func_name_param->Print((FILE*) stderr), 1) ));
        
    return TRUE;
  }
  return FALSE;
}

// ============================================================================
//
// Get old value from INTRN_LOOKUP_VIRT_FUNC
// Which is used for appending a function name string after the original value.
// This function is only used for compatibility issues and thus supoorting
// older features to work, real retrieval of according function ptr should be
// elsewhere.
//
// The returned cr could be in two forms,
// ILOAD/LDID for Virtual Call
// LOOKUP_IF for Interface Call
//
// @return NULL if cr is NULL, or cr itself if cr
// does not look like a devirt-indicator
//
// ============================================================================
CODEREP *
Get_lookup_virt_original_target_info(CODEREP *cr) {
  if (cr == NULL) {
    return NULL;
  }

  // Double checking if the coderep is actually a lookup_virt_func intrinsic op
  Is_True_Ret(Is_valid_lookup_virt_op(cr), ("The given cr is not a valid lookup_virt_func intrinsic op"), NULL);
  
  // Return the ILOAD/LDID of the function ptr (for virtual functions)
  //  or
  // Return the LOOKUP_IF (for interface functions)
  // Get the value from the first param.
  return cr->Opnd(0)->Ilod_base();
}


// ============================================================================
//
// Get the callee-function name (with type at compile time) from INTRN_LOOKUP_VIRT_FUNC
// Which is used for appending a function name string after the original value.
//
// The returned string should be a const char * string.
// FIXME: Implement this correctly.
//
// @return NULL if cr is NULL, or cr itself if cr
// does not look like a devirt-indicator
//
// ============================================================================
const char *
Get_lookup_virt_mangled_function_name(CODEREP *cr) {
  if (cr == NULL) {
    return NULL;
  }

  // Double checking if the coderep is actually a lookup_virt_func intrinsic op
  Is_True_Ret(Is_valid_lookup_virt_op(cr), ("The given cr is not a valid lookup_virt_func intrinsic op"), NULL);

  // Return the constant's stringc of the function name
  // Get the value from the second param.
  Is_True_Ret(cr->Opnd(1)->Kind() == CK_IVAR &&
              cr->Opnd(1)->Ilod_base()->Kind() == CK_LDA, ("Second param should be PARM of LDA"),
              NULL);

  // Get the constant string symbol from the last operand. (for func name of callee)
  ST *func_name_st = cr->Opnd(1)->Ilod_base()->Lda_base_st();
  Is_True_Ret(func_name_st != NULL,
              ("Cannot locate function name inito info for st = %0#llx",
               (cr->Print((FILE*) stderr), (UINT64) func_name_st)),
              NULL);
  // Get the result from the INITO/INITV matching the function name symbol
  // The 0 here indicates that this there is only one initv bound to the inito entry.
  // In case of BLOCKs, the offset would be larger than 0.
  const char *function_name_str = Get_string_from_str_const_sym(ST_st_idx(func_name_st));
  return function_name_str;
}

// ============================================================================
//
// Check if the statement is an interface call,
// We're not using this because some functions is
// still lacking the marker of INTERFACE_CALL
//
// ============================================================================
BOOL
Is_stmt_interface_call(STMTREP *stmt) {
  return (stmt->Opr() == OPR_ICALL &&
          stmt->Call_flags() & WN_CALL_IS_INTERFACE);
}

// ============================================================================
//
// Determine if an coderep matches pattern with LOOKUP_IF instrinsic
// which is used for runtime behavior of (Java) PUs.
// Please note that for virtual call, such cheecking is impossible
// due to the possible copy propagation
//  ....
//     ... (e.g. ILOAD)
//   ... (e.g. ILOAD)
// LOOKUP_IF
//
// ============================================================================
BOOL
Is_lookup_virt_op_interface_call(CODEREP *cr) {
  if (cr->Kind() == CK_OP &&
      cr->Opr() == OPR_INTRINSIC_CALL &&
      cr->Intrinsic() == INTRN_LOOKUP_IF) {
    return TRUE;
  }
  return FALSE;
}

// ============================================================================
//
// Determine if an ICALL is an inteface call.
// which is used for checking before performing finding of LOOKUP_IF
// @info, valid only for both C++/C and Java PUs
// @param stmt Statement to be checked.
// @return TRUE if the stmt is a virtual call, FALSE otherwise
//
// ============================================================================
BOOL
Is_stmt_virtual_call(STMTREP *stmt) {
  if (stmt->Opr() == OPR_ICALL &&
      stmt->Call_flags() & WN_CALL_IS_VIRTUAL) {
    return TRUE;
  }
  return FALSE;
}


// ============================================================================
// Get base pointer
// ============================================================================
BOOL
Get_icall_info(CODEREP *cr, TY_IDX &ty_idx)
{
  Is_True(cr->Kind() == CK_OP && OPERATOR_is_call(cr->Opr()), ("Cr is not call, cr%d.", cr->Coderep_id()));
  BOOL ret = FALSE;
  CODEREP *base = cr->Opnd(0)->Ilod_base();
  Is_True(base != NULL, ("Get_icall_info, cannot locate base cr."));
  TY_IDX base_ty_idx = base->object_ty();
  if (TY_kind(base_ty_idx) == KIND_POINTER) {
    base_ty_idx = TY_pointed(base_ty_idx);
  }
  if (TY_kind(base_ty_idx) == KIND_STRUCT) {
    ty_idx = base_ty_idx;
    ret = TRUE;
  }
  return ret;
}

// ============================================================================
//
// Get the virtual call's offset from ILOAD
// (if Java PU, with devirtIndicator, then extract operand from lookup_virt_func op)
//
// ============================================================================
BOOL
Get_virtual_call_ofst (CODEREP *cr, INT32 &ofst) {
  CODEREP *ild = cr;
  if (Is_valid_lookup_virt_op(cr)) {
    // for Java case, we will now use the first param for now.
    ild = Get_lookup_virt_original_target_info(cr);
  } // else would be the C++ case.
  
  // ILOAD should be loading a function's pointer from V-Table
  Is_True(ild->Kind() == CK_IVAR, ("not ivar for loading function ptr from vtable", (cr->Print((FILE*) stdout), 1)));
  ofst = ild->Offset();
  return TRUE;
}

// =============================================================================
// Get_intrfc_call_info (Java ONLY)
// @ret : sucess or fail
// @param call_cr: interface call CODEREP
// @outputparm offset: interface offset
// @outputparm ty_idx: class symbol type
// interface call address layout
//   iloadVTableFieldWN
//   classSymLda
//   offset
// INTRN_LOOKUP_IF
// =============================================================================
BOOL
Get_intrfc_call_info(CODEREP *call_cr, INT32& offset, TY_IDX& ty_idx)
{
  BOOL ret = FALSE;
  CODEREP *icall_addr = call_cr->Opnd(call_cr->Kid_count() -1);
  // read_addr is for runtime use,
  // which is the real location of the function
  if (Is_valid_lookup_virt_op(icall_addr)) {
    CODEREP *lda_func_name = icall_addr->Opnd(1)->Ilod_base();
    Is_True(lda_func_name->Kind() == CK_LDA,
      ("Cannot process LOOKUP_VIRT_FUNC due to unknown 2nd operand, LDA required, "
       "kind = %d given.", lda_func_name->Kind()));

    Is_True(icall_addr->Opnd(0)->Kind() == CK_IVAR &&
            icall_addr->Opnd(0)->Opr() == OPR_PARM,
            ("Interface call with LOOKUP should have PARM+LDID as 1st param",
             (call_cr->Print((FILE*) stdout), 1)));
    icall_addr = icall_addr->Opnd(0)->Ilod_base();

  } else {
    // In circumstances that uses the original way of ICALL structure
  }

  Is_True(icall_addr->Kind() == CK_VAR, ("Get_intrfc_call_addr: cr is not var",
                                         (call_cr->Print((FILE*) stdout), 1)));
  if(icall_addr->Kind() == CK_VAR) {
    STMTREP *addr_defstmt = icall_addr->Defstmt();
    while(addr_defstmt && 
          addr_defstmt->Opr() != OPR_INTRINSIC_CALL &&
          addr_defstmt->Rhs()->Intrinsic() != INTRN_LOOKUP_IF) {
      addr_defstmt = addr_defstmt->Rhs()->Defstmt();
    }
    Is_True(addr_defstmt, ("unable to find lookup if"));
    if(addr_defstmt) {
      CODEREP *if_call_addr = addr_defstmt->Rhs();
      Is_True(if_call_addr && if_call_addr->Kid_count() == 3, ("invalid iterface call addr"));
      if(if_call_addr && if_call_addr->Kid_count() == 3) {
        CODEREP *call_offset = if_call_addr->Opnd(2)->Ilod_base();
        Is_True(call_offset, ("class or offset invalid"));
        if(call_offset && call_offset->Kind() == CK_CONST) {
          offset = (INT32)call_offset->Const_val();
          ret = TRUE;
        }
        CODEREP *class_cr = if_call_addr->Opnd(1)->Ilod_base();
        Is_True(class_cr && class_cr->Kind() == CK_LDA, ("class or offset invalid"));
        if(ret && class_cr && class_cr->Kind() == CK_LDA) {
          // reset to false until we get type
          ret = FALSE;
          ST *base_st = class_cr->Lda_base_st();
          Is_True(ST_is_class_symbol(base_st), ("base st is not class symbol"));
          if(ST_is_class_symbol(base_st)) {
            ty_idx = ST_vtable_ty_idx(base_st);
            ret = TRUE;
          }
        }
      }
    }
  }
  return ret;
}

#if 0
CODEREP *
// Add change here to derive the icall base
Get_intrfc_call_addr(CODEREP *call_cr)
{
  CODEREP *icall_addr = call_cr->Opnd(call_cr->Kid_count() -1);
  Is_True(icall_addr->Kind() == CK_VAR, ("Get_intrfc_call_addr: cr is not var");
  if(icall_addr->Kind() != CK_VAR) {
    return NULL;
  }
  STMTREP *addr_defstmt = icall_addr->Defstmt();
  while(addr_defstmt && 
        addr_defstmt->Opr() != OPR_INTRINSIC_CALL &&
        addr_defstmt->Rhs()->Intrinsic() != INTRN_LOOKUP_IF) {
    addr_defstmt = addr_defstmt->Rhs()->Defstmt();
  }
  Is_True(addr_defstmt, ("unable to find lookup if"));
  if(!addr_defstmt) {
    return NULL;
  } else {
    return addr_defstmt->Rhs();
  }
}

// =============================================================================
// Get_intrfc_call_ofst (Java ONLY)
// interface call address layout
//   iloadVTableFieldWN
//   classSymLda
//   offset
// INTRN_LOOKUP_IF
// =============================================================================
INT32
// TODO: Add Intrfc change here.
Get_intrfc_call_ofst(CODEREP *if_call_addr)
{
  Is_True(if_call_addr && if_call_addr->Kid_count() == 3, ("invalid iterface call addr"));
  if(if_call_addr && if_call_addr->Kid_count() == 3) {
    CODEREP *call_offset = if_call_addr->Opnd(2)->Ilod_base();
    Is_True(call_offset, ("class or offset invalid"));
    if(call_offset && call_offset->Kind() == CK_CONST) {
      return call_offset->Const_val();
    } else {
      return -1;
    }
  }
  return -1;
}

// =============================================================================
// Get_intrfc_call_ty (Java ONLY)
// interface call address layout
//   iloadVTableFieldWN
//   classSymLda
//   offset
// INTRN_LOOKUP_IF
// =============================================================================
TY_IDX
// TODO: Add Transformation here.
Get_intrfc_call_ty(CODEREP *if_call_addr)
{
  Is_True(if_call_addr && if_call_addr->Kid_count() == 3, ("invalid iterface call addr"));
  if(if_call_addr && if_call_addr->Kid_count() == 3) {
    CODEREP *class_cr = if_call_addr->Opnd(1)->Ilod_base();
    Is_True(class_cr && class_cr->Kind() == CK_LDA, ("class or offset invalid"));
    if(class_cr && class_cr->Kind() == CK_LDA) {
      ST *base_st = class_cr->Lda_base_st();
      Is_True(ST_is_class_symbol(base_st), ("base st is not class symbol"));
      if(ST_is_class_symbol(base_st)) {
        TY_IDX class_ty = ST_vtable_ty_idx(base_st);
        return class_ty;
      }
    }
  }
  return TY_IDX_ZERO;
}
#endif

BOOL
Is_EH_rt_call(const char* name)
{
  const char* eh_rt[] = {
    // unwind routines
    "_Unwind_Resume",
    // c++ eh runtime
    "__cxa_allocate_exception",
    "__cxa_free_exception",
    "__cxa_get_exception_ptr",
    "__cxa_begin_catch",
    "__cxa_end_catch"
  };

  for (int i = 0; i < sizeof(eh_rt)/sizeof(eh_rt[0]); ++i) {
    if (strcmp(name, eh_rt[i]) == 0)
      return TRUE;
  }
  return FALSE;
}

BOOL
Is_struct_ptr(CODEREP *x)
{
  TY_IDX ty;
  if (x->Kind() == CK_LDA)
    ty = x->Lda_ty();
  else if (x->Kind() == CK_VAR)
    ty = x->object_ty();
  else
    return FALSE;  // handle OP/IVAR later
  if (TY_kind(ty) != KIND_POINTER)
    return FALSE;
  if (TY_kind(TY_pointed(ty)) != KIND_STRUCT)
    return FALSE;
  return TRUE;
}

SRCPOS
Get_bb_first_linenum(BB_NODE* bb)
{
  if (bb->First_stmtrep()) {
    //Is_True(bb->First_stmtrep()->Linenum() != 0,
    //        ("hit stmtrep linenum 0"));
    return bb->First_stmtrep()->Linenum();
  }
  if (bb->Linenum())
    return bb->Linenum();
  BB_NODE* tmp1 = bb;
  BB_NODE* tmp2 = bb->Next();
  while (tmp2 && CFG::Fall_through(tmp1, tmp2)) {
    if (tmp2->First_stmtrep()) {
      Is_True(tmp2->First_stmtrep()->Linenum() != 0,
              ("hit stmtrep linenum 0"));
      return tmp2->First_stmtrep()->Linenum();
    }
    if (tmp2->Linenum())
      return tmp2->Linenum();
    tmp1 = tmp2;
    tmp2 = tmp1->Next();
  }
  tmp1 = bb;
  tmp2 = bb->Prev();
  while (tmp2 && CFG::Fall_through(tmp2, tmp1)) {
    if (tmp2->Last_stmtrep()) {
      Is_True(tmp2->Last_stmtrep()->Linenum() != 0,
              ("hit stmtrep linenum 0"));
      return tmp2->Last_stmtrep()->Linenum();
    }
    if (tmp2->Linenum())
      return tmp2->Linenum();
    tmp1 = tmp2;
    tmp2 = tmp2->Prev();
  }
  return 0;
}

SRCPOS
Get_bb_last_linenum(BB_NODE* bb)
{
  if (bb->Last_stmtrep()) {
    //Is_True(bb->Last_stmtrep()->Linenum() != 0,
    //        ("hit stmtrep linenum 0"));
    return bb->Last_stmtrep()->Linenum();
  }
  if (bb->Linenum())
    return bb->Linenum();
  BB_NODE* tmp1 = bb;
  BB_NODE* tmp2 = bb->Prev();
  while (tmp2 && CFG::Fall_through(tmp2, tmp1)) {
    if (tmp2->Last_stmtrep()) {
      //Is_True(tmp2->Last_stmtrep()->Linenum() != 0,
      //        ("hit stmtrep linenum 0"));
      return tmp2->Last_stmtrep()->Linenum();
    }
    if (tmp2->Linenum())
      return tmp2->Linenum();
    tmp1 = tmp2;
    tmp2 = tmp2->Prev();
  }
  tmp1 = bb;
  tmp2 = bb->Next();
  while (tmp2 && CFG::Fall_through(tmp1, tmp2)) {
    if (tmp2->First_stmtrep()) {
      Is_True(tmp2->First_stmtrep()->Linenum() != 0,
              ("hit stmtrep linenum 0"));
      return tmp2->First_stmtrep()->Linenum();
    }
    if (tmp2->Linenum())
      return tmp2->Linenum();
    tmp1 = tmp2;
    tmp2 = tmp1->Next();
  }
  return 0;
}


/******************************************************************************
 *
 *  This is used for extracting the string value from constant string variables
 *  such as _Utfxx symbols generated by Java FE, (null terminated)
 *  the routine searches the inito-initv table for the
 *  matching constant data of the symbol
 * @param st the symbol_idx of the constant string
 * @param offset
 * @return NULL if failed to find the symbol (assertion triggered),
 *         or the given length UTF-8 string for a constant value.
 *
 ******************************************************************************/
char *
Get_string_from_str_const_sym(ST_IDX name_st_idx)
{
  INITV_ENTRIES result;
  // Get the initv list for the symbol
  Get_initv_entry_by_st(name_st_idx, &result);
  STR_IDX str_idx = result.Get_initv_str_idx(STRING_IDX, TRUE);
  return (str_idx != STR_IDX_ZERO) ? Index_to_char_array(str_idx) : NULL;
}

char *
Get_class_const_value(ST *st, UINT32 offset)
{
  if(!ST_is_class_const_data(st)) {
    return NULL;
  }
  /* TODO: This may not be a multi-byte null terminated string like utf-8 */
  INITV_ENTRIES result;
  // Get the initv list by iterating the INITV table starting from
  // INITO->val, the inito is found by Find_INITO...
  Get_initv_entry_by_st(ST_st_idx(st), &result);
  // Double check if offset is larger than INITV list's size.
  // The item to be fetched should be a symbol (leads to another INITO)
  Is_True(offset < result.Size(),
          ("offset:%d out of INITO size:%d\n", offset*Pointer_Size, result.Size()));
  // Guarding for release mode.
  if (offset >= result.Size() || result.Get_kind(offset) != INITVKIND_SYMOFF)
    return NULL;
  // Get the result directly from table
  ST_IDX name_st_idx = result.Get_initv_st(offset);
  // Get the result again from inito/initv tables.
  return Get_string_from_str_const_sym(name_st_idx);
}

// ============================================================================
//
// Compile_pattern_arr compile regular expression array, return compiled
//    regular expression array
//
// ============================================================================
regex_t **
Compile_pattern_arr(const char *pattern_arr[], INT32 len, BOOL allow_wrong_pattern, MEM_POOL *pool)
{
  regex_t **reg_exp_arr = (regex_t **) MEM_POOL_Alloc(pool, sizeof(regex_t *) * len);
  for (INT i = 0; i < len; i++) {
    regex_t *reg_exp = (regex_t *) MEM_POOL_Alloc(pool, sizeof(regex_t));
    const char *pattern = pattern_arr[i];
    int reti = regcomp(reg_exp, pattern, REG_EXTENDED | REG_NEWLINE);
    if (reti) {
      Is_True(FALSE,
        ("Regular expression pattern is wrong, please check, pattern: %s.", pattern));
      if (allow_wrong_pattern) {
        reg_exp_arr[i] = NULL;
      } else {
        return NULL;
      }
    } else {
      reg_exp_arr[i] = reg_exp;
    }
  }
  return reg_exp_arr;
}

// ============================================================================
//
// Get_utf_string_from_inito get st_idx inito, and find char array in string
//    table, copy string into mem pool
//
// ============================================================================
char *
Get_utf_string_from_inito(UINT32_MAP *st_inito_cache, ST_IDX st_idx, MEM_POOL *pool)
{
  UINT32_MAP::iterator iter = st_inito_cache->find(st_idx);
  if(st_idx == ST_IDX_ZERO || iter == st_inito_cache->end()) {
    return NULL;
  }
  INITV_ENTRIES utf_entries;
  Get_initv_entry(iter->second, &utf_entries);
  Is_True_Ret(utf_entries.Size() == STRING_END,
              ("Get_utf_string_from_inito: invalid string initv size"), NULL);
  STR_IDX str_idx = utf_entries.Get_initv_str_idx(STRING_IDX, TRUE);
  char *str = Index_to_char_array(str_idx);
  INT32 len = strlen(str);
  char *copied_str = (char *) MEM_POOL_Alloc(pool, len + 1);
  strncpy(copied_str, str, len);
  copied_str[len] = '\0';
  return copied_str;
}

// ============================================================================
//
// Get_opr_from_char return OPERATOR from char:
// OPR_NE for "ne"/"!=", OPR_EQ for "eq"/"==", OPR_GT for "gt"/">",
// OPR_GE for "ge"/">=", OPR_LT for "lt"/"<", OPR_LE for "le"/"<="
//
// ============================================================================
OPERATOR
Get_opr_from_char(const char *opr)
{
  OPERATOR ret = OPERATOR_UNKNOWN;
  if (opr != NULL && strlen(opr) > 1) {
    if (opr[0] == 'n') {
      if (opr[1] == 'e')
        ret = OPR_NE;
    }
    else if (opr[0] == 'e') {
      if (opr[1] == 'q')
        ret = OPR_EQ;
    }
    else if (opr[0] == 'g') {
      if (opr[1] == 't')
        ret = OPR_GT;
      else if (opr[1] == 'e')
        ret = OPR_GE;
    }
    else if (opr[0] == 'l') {
      if (opr[1] == 't')
        ret = OPR_LT;
      else if (opr[1] == 'e')
        ret = OPR_LE;
    }
  }
  return ret;
}

// ============================================================================
//
// Is_lib_func returns if the function name is from lib/STL/BOOST API
//
// ============================================================================
BOOL
Is_lib_func(char *fname)
{
  BOOL ret = FALSE;
  if (fname) {
    if (strcmp(fname, "__cxa_throw") == 0 ||
        strcmp(fname, "__cxa_begin_catch") == 0 ||
        strcmp(fname, "__cxa_end_catch") == 0 ||
        strcmp(fname, "__cxa_rethrow") == 0 ||
        strcmp(fname, "_Unwind_Resume") == 0 ||
        strcmp(fname, "Fail_FmtAssertion") == 0 ||
        strcmp(fname, "exit") == 0 ||
        strcmp(fname, "_Exit") == 0 ||
        strcmp(fname, "abort") == 0 ||
        strncmp(fname, "assert", 6) ==0 ||
        strcmp(fname, "_ZnwmPv") == 0 ||
        strncmp(fname, "_ZN5boost", 9) == 0 ||
        strncmp(fname, "_ZN9__gnu_cxx", 13) == 0 ||
        strncmp(fname, "_ZNK9__gnu_cxx", 14) == 0 ||
        strncmp(fname, "_ZNSa", 5) == 0 ||
        strncmp(fname, "_ZNSt", 5) == 0 ||
        strncmp(fname, "_ZNKSt", 6) == 0 ||
        strncmp(fname, "_ZSt", 4) == 0)
      ret = TRUE;
  }
  return ret;
}

// ============================================================================
// Get_first_real_field
// Get first non-struct field type from struct
// ============================================================================
TY_IDX
Get_first_real_field(TY_IDX ty) {
  Is_True(TY_kind(ty) == KIND_STRUCT, ("not struct ty"));
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
  TY_IDX fld_ty = TY_IDX_ZERO;
  do {
    FLD_HANDLE fld(fld_iter);
    if (fld.Is_Null())
      break;
    fld_ty = FLD_type(fld);
    if (TY_kind(fld_ty) != KIND_STRUCT) {
      return fld_ty;
    }
    else if (TY_fld(fld_ty) != FLD_HANDLE()) {
      fld_ty = Get_first_real_field(fld_ty);
      if (fld_ty != TY_IDX_ZERO) {
        return fld_ty;
      }
    }
  } while (!FLD_last_field(fld_iter++));
  return fld_ty;
}

// ============================================================================
// Is_mmap_retval
// Check if cr is mmap() return value
// ============================================================================
BOOL
Is_mmap_retval(CODEREP *cr) {
  if (cr->Kind() != CK_VAR || cr->Is_flag_set(CF_DEF_BY_PHI))
    return FALSE;
  if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
    STMTREP *sr = cr->Defstmt();
    Is_True(sr != NULL, ("invalid sr"));
    if (sr->Opr() == OPR_CALL &&
        strcmp(ST_name(sr->St()), "mmap") == 0)
      return TRUE;
    else
      return FALSE;
  }
  else {
    STMTREP *sr = cr->Defstmt();
    Is_True(sr && sr->Lhs() == cr && sr->Rhs() != NULL, ("invalid sr"));
    if (sr->Rhs())
      return Is_mmap_retval(sr->Rhs());
    else
      return FALSE;
  }
}

// ============================================================================
// Get_param_ty
// Get function parameter type, 0 is return type, parameter starts from 1
// ============================================================================
TY_IDX
Get_param_ty(TY_IDX ty, INT index) {
  if (ty == TY_IDX_ZERO || TY_kind(ty) != KIND_FUNCTION)
    return TY_IDX_ZERO;
  return TYLIST_ty(TY_tylist(ty) + index);
}
