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
// Module: opt_vsa_report.h
//
// =============================================================================
//
// Description:
//
// report module for issues found by checkers
//
// =============================================================================
// =============================================================================

#ifndef opt_vsa_report_INCLUDED
#define opt_vsa_report_INCLUDED

#include "defs.h"
#include "opt_vsa_meta.h"
#include "srcpos.h"
#include "opt_util.h"
#include <sys/time.h>

class SRCPOS_HANDLE;
class SRCPOSINFO;

// ============================================================================
// VSA_ISSUE
//  issue reported by VSA
// ============================================================================
class VSA_ISSUE {
  friend class VSA_REPORT_WRITTER;
  template<INT N, class W> friend class VIW_IMPL;
public:
   enum FORMAT
   {
     TEXT,
     JSON
   };

private:
  const char    *_rule_set;
  const char    *_rule_code;
  const char    *_error_code;
  SRCPOS         _start_spos;
  SRCPOS         _end_spos;
  const char    *_issue_key;
  IDTYPE         _isukey_id;
  const char    *_message;
  const char    *_variable_name;
  const char    *_function_name;
  const char    *_type_name;
  SRCPOS_HANDLE *_srcpos_h;
  SRCPOS        *_spos_array;
  UINT32         _spos_count;
  mUINT8         _certainty;
  mUINT8         _severity;
  mUINT8         _priority;
  mUINT8         _fix_cost;

  VSA_ISSUE(const VSA_ISSUE&);            // no copy constructor
  VSA_ISSUE& operator=(const VSA_ISSUE&); // no assign operator

public:
  const char     *Rule_set() const   { return _rule_set;   }
  const char     *Rule_code() const  { return _rule_code;  }
  const char     *Error_code() const { return _error_code; }
  SRCPOS          Start_spos() const { return _start_spos; }
  SRCPOS          End_spos() const   { return _end_spos;   }
  const char     *Key() const        { return _issue_key;  }
  IDTYPE          Key_id() const     { return _isukey_id;  }
  const char     *Message() const    { return _message;    }
  const char     *Vname() const      { return _variable_name; }
  const char     *Pname() const      { return _function_name; }
  const char     *Tname() const      { return _type_name;  }
  SRCPOS_HANDLE  *Sp_h() const       { return _srcpos_h;   }
  const SRCPOS   *Spos_array() const { return _spos_array ? _spos_array : &_start_spos; }
  UINT32          Spos_count() const { return _spos_array ? _spos_count : 1; }
  ISSUE_CERTAINTY Certainty() const  { return (ISSUE_CERTAINTY)_certainty;  }

  // helper functions
  BOOL Is_builtin() const { return strcmp(_rule_set, "BUILTIN") == 0; }
  BOOL Is_cert() const    { return strcmp(_rule_set, "CERT") == 0;    }
  BOOL Is_sml() const     { return strcmp(_rule_set, "SML") == 0;     }
  BOOL Is_standard() const{ return Is_cert() || strcmp(_rule_set, "CWE") == 0; }
  BOOL Is_user() const    { return strcmp(_rule_set, "USER") == 0;    }
  BOOL Is_rbc() const     { return strcmp(_rule_code, "RBC") == 0;    }

public:
  VSA_ISSUE(const char* rule_set, const char* rule_code, const char* issue_key,
            IDTYPE keyid, const char* vname, const char* pname,
            SRCPOS spos,ISSUE_CERTAINTY certainty, SRCPOS_HANDLE *sp_h)
   : _rule_set(rule_set), _rule_code(rule_code), _error_code(NULL),
     _start_spos(spos), _end_spos(0),
     _message(NULL), _issue_key(issue_key), _isukey_id(keyid),
     _variable_name(vname), _function_name(pname),  _type_name(pname),
     _srcpos_h(sp_h), _spos_array(NULL), _spos_count(0),
     _certainty(certainty), _severity(0), _priority(0), _fix_cost(0) { }

  VSA_ISSUE(const char* rule_set, const char* rule_code, const char* issue_key,
            IDTYPE keyid, const char* vname, const char* pname,
            ISSUE_CERTAINTY certainty, SRCPOS *spos, INT count)
   : _rule_set(rule_set), _rule_code(rule_code), _error_code(NULL),
     _start_spos(spos[0]), _end_spos(0),
     _message(NULL), _issue_key(issue_key), _isukey_id(keyid),
     _variable_name(vname), _function_name(pname),  _type_name(pname),
     _srcpos_h(NULL), _spos_array(spos), _spos_count(count),
     _certainty(certainty), _severity(0), _priority(0), _fix_cost(0) { }

  VSA_ISSUE(const char* rule_set, const char* rule_code,
            const char* err_code, const char* issue_key,
            IDTYPE keyid, const char* vname, const char* pname,
            SRCPOS spos, ISSUE_CERTAINTY certainty, SRCPOS_HANDLE *sp_h)
   : _rule_set(rule_set), _rule_code(rule_code), _error_code(err_code),
     _start_spos(spos), _end_spos(0),
     _message(NULL), _issue_key(issue_key), _isukey_id(keyid),
     _variable_name(vname), _function_name(pname), _type_name(NULL),
     _srcpos_h(sp_h), _spos_array(NULL), _spos_count(0),
     _certainty(certainty), _severity(0), _priority(0), _fix_cost(0) { }

  // fix cost
  UINT Fix_cost() const        { return _fix_cost; }
  void Set_fix_cost(UINT cost) { _fix_cost = cost; }
  UINT Complexity() const;
  const char *Get_rule_set() const {
    if (strcmp(_rule_set, "SML") == 0) {
      if (strncmp(_rule_code, "MSR", 3) == 0)
        return "MSR";
      if (strncmp(_rule_code, "GJB", 3) == 0)
        return "GJB";
      if (strncmp(_rule_code, "AUTOSAR", 7) == 0)
        return "ATS";
    }
    return _rule_set;
  }

};

// ============================================================================
// WRITTER_MIN/WRITTER_MIN
//  minimal and maximal version supported by writter
// ============================================================================
enum WRITTER_VERSION {
  WRITTER_MIN = 0,
  WRITTER_VER_0 = WRITTER_MIN,
  WRITTER_VER_1,
  WRITTER_VER_2,
  WRITTER_MAX = WRITTER_VER_2,
};

// ============================================================================
// WRITTER_FORMAT
//  plain text or cbor
// ============================================================================
enum WRITTER_FORMAT {
  WRITTER_TEXT = 0,    // in plain text
  WRITTER_CBOR = 1     // in cbor
};

// ============================================================================
// report writter
// ============================================================================
class VSA_REPORT_WRITTER {
private:
  static VSA_REPORT_WRITTER* _writter;
  template<INT V, class W>
  static VSA_REPORT_WRITTER* Create_writter(FILE* fp);

private:
  struct timeval _scan_start; // time when scan starts
  struct timeval _scan_end;   // time when scan ends
  BOOL           _json;       // is VSA:json on?

  VSA_REPORT_WRITTER(const VSA_REPORT_WRITTER&); // disable copy constructor
  VSA_REPORT_WRITTER& operator=(const VSA_REPORT_WRITTER&);

protected:
  VSA_REPORT_WRITTER();

  BOOL           Json() const  { return _json;       }
  struct timeval Start() const { return _scan_start; }
  struct timeval End() const   { return _scan_end;   }

  virtual void Print_json_header(FILE* fp) { }
  virtual void Print_json_source(FILE* fp) { }
  virtual void Print_json_footer(FILE* fp) { }

public:
  static VSA_REPORT_WRITTER* Get_writter(FILE *fp);

  void Print_header(FILE* fp)
  {
    gettimeofday(&_scan_start, NULL);
    if (Json())
      Print_json_header(fp);
  }

  void Print_source(FILE* fp)
  {
    if (Json()) {
      Print_json_source(fp);
      // Print_text_source(VsaTxt_File);
    }
  }

  void Print_footer(FILE* fp)
  {
    gettimeofday(&_scan_end, NULL);
    if (Json())
      Print_json_footer(fp);
    fclose(fp);
  }

};

// ============================================================================
// issue writter
// ============================================================================
class VSA_ISSUE_WRITTER {
private:
  static VSA_ISSUE_WRITTER* _writter;
  template<INT V, class W>
  static VSA_ISSUE_WRITTER* Create_writter(FILE* fp);

private:
  char          *_dname_v;    // demangled variable name for issue
  char          *_dname_p;    // demangled function name for issue
  INT            _issue_count;// issue count
  BOOL           _json;       // is VSA:json on?

  // disable copy constructor and assign operator
  VSA_ISSUE_WRITTER(const VSA_ISSUE_WRITTER&);
  VSA_ISSUE_WRITTER& operator=(const VSA_ISSUE_WRITTER&);

protected:
  VSA_ISSUE_WRITTER();

  const char* Vname() const { return _dname_v; }
  const char* Pname() const { return _dname_p; }
  INT  Issue_count() const  { return _issue_count; }
  BOOL Json() const         { return _json;    }
  void Fix_vname();

  virtual void Print_json_issue(const VSA_ISSUE* issue, FILE* fp) { }
  virtual void Print_json_path(const SRCPOSINFO* path, FILE* fp) { }

  void Print_json_paths(const VSA_ISSUE* issue, FILE* fp);
  void Print_text_issue(const VSA_ISSUE* issue, FILE* fp);

public:
  static VSA_ISSUE_WRITTER* Get_writter(FILE* fp);

  void Print_issue(const VSA_ISSUE* issue, FILE* fp);

  virtual void Flush() { }
};

// ============================================================================
// SIMPLE_STACK
// a simple fixed length stack
// ============================================================================
template<typename _DATA, INT _MAX>
class SIMPLE_STACK {
private:
  _DATA  _data[_MAX];
  _DATA *_ptr;

public:
  SIMPLE_STACK() : _ptr(_data) { }

  void Push(_DATA data) {
    Is_True_Ret(_ptr < &_data[_MAX], ("stack oob"));
    *_ptr = data;
    ++ _ptr;
  }

  void Pop() {
    Is_True_Ret(_ptr > _data, ("stack empty"));
    -- _ptr;
  }

  UINT Size() const {
    return _ptr - _data;
  }

  _DATA* Data() {
    return _data;
  }
};

// ============================================================================
// REVERSE_STACK
// operates like a simple stack, grow downaward but data was fatched from the top
// ============================================================================
template<typename _DATA, INT _MAX>
class REVERSE_STACK {
private:
  _DATA  _data[_MAX];
  _DATA *_ptr;

public:
  REVERSE_STACK() : _ptr(&_data[_MAX]) { }

  void Push(_DATA data) {
    Is_True_Ret(_ptr > _data, ("stack oob"));
    -- _ptr;
    *_ptr = data;
  }

  void Pop() {
    Is_True_Ret(_ptr < &_data[_MAX], ("stack empty"));
    ++ _ptr;
  }

  UINT Size() const {
    return &_data[_MAX] - _ptr;
  }

  _DATA* Data() {
    return _ptr;
  }
};

extern void Json_write(const char* msg, FILE* fp);
extern size_t Json_copy(char* dst, const char* src, size_t len);
extern size_t Json_copy_fuzz(char* dst, const char* src, size_t len);

extern BOOL Is_ddc_candidate(WN* wn, const char* sym = NULL);
extern void (*Report_vsa_error_p)(const VSA_ISSUE* issue);
extern void Report_vsa_error(const char *fname, const char *vname, const char *rule,
                             BOOL maybe, SRCPOS *spos, INT count);
inline void Report_vsa_error(const char *fname, const char *vname, const char *rule,
                             BOOL maybe, SRCPOS spos)
{
  Report_vsa_error(fname, vname, rule, maybe, &spos, 1);
}

extern void Report_xsca_error(const char *fname, const char *vname, const char *rule,
                              BOOL maybe, SRCPOS *spos, INT count);
inline void Report_xsca_error(const char *fname, const char *vname, const char *rule,
                              BOOL maybe, SRCPOS spos)
{
  Report_xsca_error(fname, vname, rule, maybe, &spos, 1);
}

#endif /* opt_vsa_report_INCLUDED */
