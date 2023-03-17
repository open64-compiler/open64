//-*-c++-*-

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

// =============================================================================
// =============================================================================
//
// Module: opt_vsa_report_base.xxx
//
// =============================================================================
#include "defs.h"
#include "pathscale_defs.h"
#include "config.h"
#include "config_vsa.h"
#include "printsrc.h"
#include "report.h"
/* VSA report file related */
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include "wn.h"
#include "opt_vsa_report.h"
#include "whirl_file_mgr.h"
#include "vsa_json_writter.h"
#include "vtxt_hdr.h"

const char* git_revision_string = "";
const char* open64_version_string = OPEN64_MAJOR_VERSION;
const char* builtin_rv_string = "1";
static char Fuzz_char(char);

// unique instance for plain test writter
template<> PLAIN_TEXT_WRITTER* PLAIN_TEXT_WRITTER::_instance = NULL;
// unique instance for cbor writter
template<> CBOR_WRITTER*       CBOR_WRITTER::_instance = NULL;

// ============================================================================
// VRW_IMPL
// declaration for different version writter
// ============================================================================
template<INT N, class WRITTER>
class VRW_IMPL : public VSA_REPORT_WRITTER {
  friend class VSA_REPORT_WRITTER;
private:
  VRW_IMPL(FILE* fp) {}

public:
  // print json file header
  void Print_json_header(FILE* fp) { }

  // print source file list
  void Print_json_source(FILE* fp) { }

  // print json footer
  void Print_json_footer(FILE* fp) { }
};

// ============================================================================
// Specialize Start, End, Append for files section for Alterr
// ============================================================================

#define Alterr VsaTxt_File

static void Start_filelist(const char ch, UINT32 indent, bool& comma) {
  if (Alterr != NULL) {
    if (comma)
      fprintf(Alterr, ",\n");
    if (indent)
      fprintf(Alterr, "%*s%c\n", indent, " ", ch);
    else
      fprintf(Alterr, "%c\n", ch);
    comma = FALSE;  // no comma needed for next item
  }
}

// end fileid map
static void End_filelist(const char ch, UINT32 indent, bool& comma) {
  if (Alterr != NULL) {
    if (indent)
      fprintf(Alterr, "\n%*s%c",indent," ", ch);
    else
      fprintf(Alterr, "\n%c", ch);
    comma = TRUE;   // need a comma for next item
  }
}

// append <key, value> to current map
static void Append_filelistentry(const char* key, const char* value, bool& comma) {
  if (Alterr != NULL) {
    if (comma)
      fprintf(Alterr, ",\n");
    else
      comma = TRUE;  // need a comma for next item
    if (value != NULL) {
      fprintf(Alterr, "%*s\"%s\": \"%s\"", 2, " ", key, value);
    }
    else {
      fprintf(Alterr, "%*s\"%s\":null", 2, " ", key);
    }
  }
}

static void Append_filelistentry(const char* key, long value, bool& comma) {
  if (Alterr != NULL) {
    Is_True(key != NULL, ("null key for map"));
    if (comma)
      fprintf(Alterr, ",\n");
    else
      comma = TRUE;  // need a comma for next item
    fprintf(Alterr, "%*s\"%s\": %ld", 2, " ", key, value);
  }
}

static void Flush_filelist(void) {
  if (Alterr != NULL) {
    fflush(Alterr);
  }
}  

static void Win32_Fix_Path(const char *path) {
  char *p = (char *)path;
  while (*p) {
    if (*p == '\\')
      *p = '/';
    p++;
  }
}

// ============================================================================
// Write_vsarpt_source
//   helper functions to print source file list
// ============================================================================
//
// #define VTXT_VERSION_ID ("0.6")  switch to version 0,7 on April 23, 2021
//                                  0.7.2, switch on nhv by default 09/03/21
// #define VTXT_VERSION_ID ("0.7.2") use VTXT_HDR
// ============================================================================

template<class WRITTER> void
Write_vsarpt_source(WRITTER& writter, const char *finfo, const char *fid,
                    const char* path, const char* issue)
{
  INT i;
  char fname[1024];
  INT file_count = Get_Global_File_Count();
  bool comma = false;
  // start an array for finfo
  writter.template Start<TAG_ARRAY>(finfo, 0);
  if (Alterr) {
    VTXT_HDR vtxt_hdr(0, 7, 2);
    char     version[100+100+100+1];// 99 major, minor, mminor versions
    char     hdr_body[16 * 4 + 1];// each mUINT64 need 16 bytes 

    fprintf(Alterr, "{\"V\", %.5s, %s,%s}\n",
            (VSA_Magic_Id)? VSA_Magic_Id : "A",
            vtxt_hdr.Version(version),
            vtxt_hdr.Filehdr(hdr_body)); // label the vtxt file kind
  }
  Start_filelist('[', 0, comma);
  // append files
  for (i = 1; i < file_count; ++i) {
    const char* name = Get_Global_File_Full_Path(i, fname, sizeof(fname));

    // fix windows path
    Win32_Fix_Path(name);

    INT name_len = strlen(name);
    if (name_len >= 3 && name[name_len - 1] == 'i' &&
        (name[name_len - 2] == '.' ||
         (name[name_len - 2] == 'i' && name[name_len - 3] == '.')))
      continue;
    writter.template Start<TAG_MAP>(NULL, 2);  // no key, 2 attrs
    writter.Append(fid, i);
    writter.Append(path, name);
    writter.template End<TAG_MAP>(NULL, 2);    // no key, 2 attrs
    Start_filelist('{', 2, comma);
    Append_filelistentry(fid, i, comma);
    Append_filelistentry(path, name, comma);
    End_filelist('}', 2, comma);
  }
  // end array for finfo
  writter.template End<TAG_ARRAY>(finfo, 0);
  End_filelist(']', 0, comma);
  if (Alterr !=NULL)
    fprintf(Alterr, "\n");
  // start array for issue
  writter.template Start<TAG_ARRAY>(issue, 0);
  // flush file because issue uses another writter
  writter.Flush();
  Flush_filelist();
}

// ============================================================================
// Write_ruleset_info
//   helper functions to print ruleset info
// ============================================================================
template<class WRITTER> void
Write_ruleset_info(WRITTER& writter, const char *rsn,
                   const char *rs, const char *rv)
{
  WHIRL_FILE_MANAGER* mgr = WHIRL_FILE_MANAGER::Get();
  UINT32 cnt = mgr ? mgr->Get_ruleset_count() + 1 : 1;
  writter.template Start<TAG_ARRAY>(rsn, cnt);
  // add builtin ruleset
  writter.template Start<TAG_MAP>(NULL, 2);   // no key, 2 attrs
  writter.Append(rs, "BUILTIN");
  writter.Append(rv, builtin_rv_string);
  writter.template End<TAG_MAP>(NULL, 2);     // no key, 2 attrs
  // add ruleset from .a
  for (INT i = 0; i < cnt - 1; ++i) {
    const RULESET_INFO& info = mgr->Get_ruleset(i);
    writter.template Start<TAG_MAP>(NULL, 2); // no key, 2 attrs
    writter.Append(rs, info.Name());
    writter.Append(rv, info.Version());
    writter.template End<TAG_MAP>(NULL, 2);   // no key, 2 attrs
  }
  writter.template End<TAG_ARRAY>(rsn, cnt);
}

// ============================================================================
// Write_vsarpt_cmdline
//   helper functions to print command line
// ============================================================================
INT    saved_argc;
char** saved_argv;

template<class WRITTER> void
Write_vsarpt_cmdline(WRITTER& writter, const char *cmd)
{
  if (saved_argc == 0)
    writter.Append(cmd, (const char*)NULL);
  std::string buf;
  char tmp[32];
  for (INT i = 0; i < saved_argc; ++i) {
    char *ptr = tmp;
    if (i > 0)
      *ptr++ = Fuzz_char(' ');
    Json_copy_fuzz(ptr, saved_argv[i], sizeof(tmp) - 1);
    buf.append(tmp);
  }
  writter.Append(cmd, buf.c_str());
}

// ============================================================================
// Write_vsarpt_environment
//   helper functions to print environment variables
// ============================================================================
template<class WRITTER> void
Write_vsarpt_environment(WRITTER& writter, const char *env)
{
  std::string env_str;
  char tmp[128];
  const char* val;

#define APPEND_ENV(x, y)                     \
  val = getenv(x);                           \
  if (val != NULL) {                         \
    Json_copy_fuzz(tmp, x, sizeof(tmp));     \
    env_str.append(tmp);                     \
    env_str.append(1, Fuzz_char('='));       \
    Json_copy_fuzz(tmp, val, sizeof(tmp));   \
    env_str.append(tmp);                     \
    if (y == TRUE)                           \
      env_str.append(1, Fuzz_char(','));     \
  }

  APPEND_ENV("EUID", TRUE);
  APPEND_ENV("HOME", TRUE);
  APPEND_ENV("HOSTNAME", TRUE);
  APPEND_ENV("HOSTTYPE", TRUE);
  APPEND_ENV("LANG", TRUE);
  APPEND_ENV("LD_LIBRARY_PATH", TRUE);
  APPEND_ENV("LD_PRELOAD", TRUE);
  APPEND_ENV("MACHTYPE", TRUE);
  APPEND_ENV("OSTYPE", TRUE);
  APPEND_ENV("PATH", TRUE);
  APPEND_ENV("PWD", TRUE);
  APPEND_ENV("SHELL", TRUE);
  APPEND_ENV("XC5_CONFIG_FILE", TRUE);
  APPEND_ENV("UID", TRUE);
  APPEND_ENV("USER", FALSE);
#undef APPEND_ENV

  writter.Append(env, env_str.c_str());
}

// ============================================================================
// Write_vsarpt_setting
//   helper functions to print vsa settings
// ============================================================================
template<class WRITTER> void
Write_vsarpt_setting(WRITTER& writter, const char *set)
{
  std::string set_str;
  char tmp[32];

#define APPEND_SET(x, y)               \
  Json_copy_fuzz(tmp, x, sizeof(tmp)); \
  set_str.append(tmp);                 \
  set_str.append(1, y ? Fuzz_char('T') : Fuzz_char('F')); \
  set_str.append(1, Fuzz_char(',')); \

  APPEND_SET("UIV:", VSA_Uiv);
  APPEND_SET("AOB:", VSA_Aob);
  APPEND_SET("NPD:", VSA_Npd);
  APPEND_SET("RBC:", VSA_Rbc);
  APPEND_SET("UBF:", VSA_Ubf);
  APPEND_SET("UAF:", VSA_Uaf);
  APPEND_SET("MSF:", VSA_Msf);
  APPEND_SET("DBF:", VSA_Dbf);
  APPEND_SET("DDV:", VSA_Ddv);
  APPEND_SET("RAL:", VSA_Ral);
#undef APPEND_SET

  writter.Append(set, set_str.c_str());
}

// ============================================================================
// template<> VRW_IMPL<WRITTER_VER_0>
// implementation for version 0 writter
// ============================================================================
template<class WRITTER>
class VRW_IMPL<WRITTER_VER_0, WRITTER> : public VSA_REPORT_WRITTER {
  friend class VSA_REPORT_WRITTER;
private:
  WRITTER &_writter;
  VRW_IMPL(FILE* fp) : _writter(WRITTER::Get_writter(fp)) { }

public:
  // write json header
  void Print_json_header(FILE* fp)
  {
    // start a indefinate length map
    _writter.template Start<TAG_MAP>(NULL, 0);
  }

  // write json source file list
  void Print_json_source(FILE* fp)
  {
    Write_vsarpt_source(_writter, "scanFile", "id", "name", "issue");
  }

  // write json file footer
  void Print_json_footer(FILE* fp)
  {
    // end issue array
    _writter.template End<TAG_ARRAY>("issue", 0);
    // start record map
    _writter.template Start<TAG_MAP>("mvsaRecord", 6);  // # of attrs below
    // append engine
    _writter.Append("engine", "mastiff");
    // append engineVersion
    _writter.Append("engineVersion", open64_version_string);
    // append scanMode
    _writter.Append("scanMode", Run_ipsaopt ? "xa" :
                                 Run_vsaopt ? "sa" : "none");
    // append logPath
    _writter.Append("logPath", Vsa_Report_File);
    // append scanStarted
    _writter.Append("scanStarted", Start().tv_sec * 1000000
                                    + Start().tv_usec);
    // append scanEnd
    _writter.Append("scanEnded", End().tv_sec * 1000000
                                  + End().tv_usec);
    // end record map
    _writter.template End<TAG_MAP>("mvsaRecord", 6);  // # of attrs above
    // end whole .v file
    _writter.template End<TAG_MAP>(NULL, 0);
    // flush file
    _writter.Flush();
  }
};

// ============================================================================
// template<> VRW_IMPL<WRITTER_VER_1>
// implementation for version 1 writter
// ============================================================================
template<class WRITTER>
class VRW_IMPL<WRITTER_VER_1, WRITTER> : public VSA_REPORT_WRITTER {
  friend class VSA_REPORT_WRITTER;
private:
  WRITTER &_writter;
  VRW_IMPL(FILE* fp) : _writter(WRITTER::Get_writter(fp)) { }

public:
  // write json header
  void Print_json_header(FILE* fp)
  {
    // start a indefinate length map
    _writter.template Start<TAG_MAP>(NULL, 0);
  }

  // write json source file list
  void Print_json_source(FILE* fp)
  {
    Write_vsarpt_source(_writter, "files", "fid", "path", "issues");
  }

  // write json footer
  void Print_json_footer(FILE* fp)
  {
    // end issue array
    _writter.template End<TAG_ARRAY>("issues", 0);
    // write ruleset info
    Write_ruleset_info(_writter, "rulesets", "rs", "rv");
    // append version
    _writter.Append("v", WRITTER_VER_1);
    // append task id
    _writter.Append("id", "@@scanTaskId@@");
    // append status
    _writter.Append("s", "@@status@@");
    // append message
    _writter.Append("m", "@@message@@");
    // append engine
    _writter.Append("eng", "Xcalibyte");
    // append engine version
    _writter.Append("ev", open64_version_string);
    // append engine revision
    _writter.Append("er", git_revision_string);
    // append cmd line
    Write_vsarpt_cmdline(_writter, "x1");
    // append environment
    Write_vsarpt_environment(_writter, "x2");
    // append scan start
    _writter.Append("ss", Start().tv_sec * 1000000
                            + Start().tv_usec);
    // append scan end
    _writter.Append("se", End().tv_sec * 1000000
                            + End().tv_usec);
    struct rusage u;
    if (getrusage(RUSAGE_SELF, &u) == 0) {
      // append user time
      _writter.Append("usr", u.ru_utime.tv_sec * 1000000
                               + u.ru_utime.tv_usec);
      // append sys time
      _writter.Append("sys", u.ru_stime.tv_sec * 1000000
                               + u.ru_stime.tv_usec);
      // append rss size
      _writter.Append("rss", u.ru_maxrss);
    }
    // end whole .v file
    _writter.template End<TAG_MAP>(NULL, 0);
    // flush buffer
    _writter.Flush();
  }
};

// ============================================================================
// VSA_REPORT_WRITTER
// ============================================================================
VSA_REPORT_WRITTER* VSA_REPORT_WRITTER::_writter;

template<INT N, class W> VSA_REPORT_WRITTER*
VSA_REPORT_WRITTER::Create_writter(FILE* fp)
{
  if (N == VSA_Json_Version)
    return new VRW_IMPL<N, W>(fp);
  else
    return Create_writter<N-1, W>(fp);
}

template<> VSA_REPORT_WRITTER*
VSA_REPORT_WRITTER::Create_writter<WRITTER_MIN, CBOR_WRITTER>(FILE* fp)
{
  return new VRW_IMPL<WRITTER_MIN, CBOR_WRITTER>(fp);
}

template<> VSA_REPORT_WRITTER*
VSA_REPORT_WRITTER::Create_writter<WRITTER_MIN, PLAIN_TEXT_WRITTER>(FILE* fp)
{
  return new VRW_IMPL<WRITTER_MIN, PLAIN_TEXT_WRITTER>(fp);
}

VSA_REPORT_WRITTER::VSA_REPORT_WRITTER()
{
  _json = (Need_vsafile() && RFile != NULL && VSA_Output_Json);
}

VSA_REPORT_WRITTER*
VSA_REPORT_WRITTER::Get_writter(FILE* fp)
{
  if (_writter == NULL) {
    _writter = VSA_Output_Cbor
                 ? Create_writter<WRITTER_MAX, CBOR_WRITTER>(fp)
                 : Create_writter<WRITTER_MAX, PLAIN_TEXT_WRITTER>(fp);
  }
  return _writter;
}

// ============================================================================
// implementation to write header/source/header
// called by driver
// ============================================================================
void
Write_vsarpt_header(FILE *fp) {
  // write header
  VSA_REPORT_WRITTER* writter = VSA_REPORT_WRITTER::Get_writter(fp);
  writter->Print_header(fp);
}

void
Write_vsarpt_source(FILE* fp) {
  // write source list
  VSA_REPORT_WRITTER* writter = VSA_REPORT_WRITTER::Get_writter(fp);
  writter->Print_source(fp);
}

void
Write_vsarpt_footer(FILE *fp) {
  // write footer
  VSA_REPORT_WRITTER* writter = VSA_REPORT_WRITTER::Get_writter(fp);
  writter->Print_footer(fp);
}

// ============================================================================
// helper functions for common functions
// ============================================================================
static char
Fuzz_char(char ch) {
  if (ch >= 'a' && ch <= 'z')
    ch = 'z' - ch + 'a';
  else if (ch >= 'A' && ch <= 'Z')
    ch = 'Z' - ch + 'A';
  else if (ch >= '0' && ch <= '9')
    ch = '9' - ch + '0';
  else if (ch == '-')
    ch = '@';
  else if (ch == ':')
    ch = '*';
  else if (ch == '.')
    ch = '~';
  else if (ch == '=')
    ch = '.';
  else if (ch == ' ')
    ch = '#';
  return ch;
}

void
Json_write(const char* msg, FILE* fp) {
  char ch;
  const char *p = msg;
  while ((ch = *p++) != '\0') {
    if (ch == '\r' || ch == '\n')
      continue;
    if (ch == '\"' || ch == '\\') {
      fputc('\\', fp);
      fputc(ch, fp);
    }
    else
      fputc(ch, fp);
  }
}

void
Json_write_fuzz(const char* msg, FILE* fp) {
  char ch;
  const char *p = msg;
  while ((ch = *p++) != '\0') {
    if (ch == '\r' || ch == '\n')
      continue;
    if (ch == '\"' || ch == '\\') {
      fputc('\\', fp);
      fputc(ch, fp);
    }
    else {
      ch = Fuzz_char(ch);
      fputc(ch, fp);
    }
  }
}

size_t
Json_copy(char* dst, const char* src, size_t len)
{
  char ch;
  char* top = dst + len - 1;  // reserve last char for '\0'
  char* dst_orig = dst;
  while ((ch = *src++) != '\0' && dst < top) {
    if (ch == '\r' || ch == '\n')
      continue;
    if (ch == '\"' || ch == '\\') {
      if (dst == top - 1)
        break;  // not enough space, give up
      *dst++ = '\\';
    }
    *dst++ = ch;
  }
  *dst = '\0';
  return dst - dst_orig;  // not include last '\0'
}

size_t
Json_copy_fuzz(char* dst, const char* src, size_t len)
{
  char ch;
  char* top = dst + len - 1;  // reserve last char for '\0'
  char* dst_orig = dst;
  while ((ch = *src++) != '\0' && dst < top) {
    if (ch == '\r' || ch == '\n')
      continue;
    if (ch == '\"' || ch == '\\') {
      if (dst == top - 1)
        break;  // not enough space, give up
      *dst++ = '\\';
      *dst++ = ch;
    }
    else {
      ch = Fuzz_char(ch);
      *dst++ = ch;
    }
  }
  *dst = '\0';
  return dst - dst_orig;  // not include last '\0'
}

BOOL
Is_ddc_candidate(WN* wn, const char* sym)
{
  if (Srcpos_To_Line(WN_Get_Linenum(wn)) == 0)
    return FALSE;
  OPERATOR opr = WN_operator(wn);
  if (opr == OPR_DEALLOCA ||
      opr == OPR_GOTO ||
      opr == OPR_LABEL ||
      opr == OPR_PRAGMA ||
      opr == OPR_RETURN ||
      opr == OPR_TRUEBR)
    return FALSE;
  if (opr == OPR_CALL) {
    if (sym == NULL)
      sym = ST_name(WN_st(wn));
    if (strcmp(sym, "__cxa_end_catch") == 0 ||
        strcmp(sym, "_Unwind_Resume") == 0)
      return FALSE;
    if (PU_cxx_lang(Get_Current_PU())) {
      if (strncmp(sym, "_ZThn", 5) == 0 ||
          strncmp(sym, "_ZTv", 4) == 0)
        return FALSE;
      INT len = strlen(sym);
      if (strncmp(sym, "_Z", 2) == 0 &&
          strcmp(sym + len - 4, "D1Ev") == 0)
        return FALSE;
    }
  }
  return TRUE;
}

void (*Report_vsa_error_p)(const VSA_ISSUE* issue);

static const char*
Vsa_format_key(char *buf, INT size, const char *vname, const char *rule, SRCPOS spos)
{
  char *kptr = buf;
  INT nlen = snprintf(kptr, size, "%s%s%s",
                      (vname && *vname) ? vname : "",
                      (vname && *vname) ? "@" : "", rule);
  if (spos != 0 && nlen < size) {
    kptr += nlen;
    size -= nlen;
    const char *file_name = NULL;
    const char *dir_name = NULL;
    Get_Local_Srcpos_Filename(spos, &file_name, &dir_name);
    nlen = snprintf(kptr, size, "@%s:%d",
                    file_name, SRCPOS_linenum(spos));
  }
  return buf;
}

void
Report_vsa_error(const char *fname, const char *vname, const char *rule,
                 BOOL maybe, SRCPOS *spos, INT count)
{
  Is_True_Ret(spos && count > 0, ("invalid spos"));
  char keybuf[512];
  Vsa_format_key(keybuf, sizeof(keybuf), vname, rule,
                 count > 0 ? spos[0] : 0);
  VSA_ISSUE issue("BUILTIN", rule, keybuf, 0, vname, fname,
                  maybe ? IC_MAYBE : IC_DEFINITELY,
                  spos, count);
  issue.Set_fix_cost(1);
  if (Report_vsa_error_p)
    Report_vsa_error_p(&issue);
}

void
Report_xsca_error(const char *fname, const char *vname, const char *rule,
                  BOOL maybe, SRCPOS *spos, INT count)
{
  Is_True_Ret(spos && count > 0, ("invalid spos"));
  char keybuf[512];
  Vsa_format_key(keybuf, sizeof(keybuf), vname, rule,
                 count > 0 ? spos[0] : 0);
  VSA_ISSUE issue("SML", rule, keybuf, 0, vname, fname,
                  maybe ? IC_MAYBE : IC_DEFINITELY,
                  spos, count);
  issue.Set_fix_cost(1);
  if (Report_vsa_error_p)
    Report_vsa_error_p(&issue);
}

