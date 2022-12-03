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

// ====================================================================
// ====================================================================
//
// Module: opt_vsa.h
//
// ====================================================================
//

#ifndef opt_vsa_util_INCLUDED
#define opt_vsa_util_INCLUDED

#ifndef srcpos_INCLUDED
#include "srcpos.h"
#endif /* srcpos_INCLUDED */

#include "vsa_intent.h"  // user declared intents
#include "report.h"      // in common/util for vsa_report
#include "config_vsa.h"  // VSA options
#include "symtab_idx.h"
#include "wintrinsic.h"
#include "wn.h"
#include <regex.h>
#include <stdarg.h>
#include <ext/hash_set>
#include "irbdata.h"
using __gnu_cxx::hash_set;

class AUX_STAB_ENTRY;
class BB_NODE;
class CODEREP;
class COMP_UNIT;
class FLD_HANDLE;
class INLCXT;
class ST;
class STMTREP;
class WN;

#define DEFAULT_HASH_TABLE_SIZE    13

typedef struct {
  bool operator() (const char* k1, const char* k2) const
  {
    if(!k1 && !k2) {
      return TRUE;
    } else {
      if(!k1 || !k2) {
        return FALSE;
      } else {
        if(strlen(k1) != strlen(k2)) {
          return FALSE;
        } else {
          return (strcmp(k1, k2) == 0);
        }
      }
    }
  }
} equal_str;

typedef mempool_allocator<IDTYPE> IDTYPE_ALLOCATOR;
typedef __gnu_cxx::hash<IDTYPE>   IDTYPE_HASHER;
typedef std::equal_to<IDTYPE>     IDTYPE_EQUAL;
typedef hash_set<IDTYPE, IDTYPE_HASHER,
                 IDTYPE_EQUAL, IDTYPE_ALLOCATOR> IDTYPE_SET;

typedef mempool_allocator<uint64_t>       UINT64_ALLOCATOR;
typedef __gnu_cxx::hash<uint64_t>         UINT64_HASHER;
typedef __gnu_cxx::equal_to<uint64_t>     UINT64_EQUAL;
typedef hash_set<uint64_t, UINT64_HASHER, UINT64_EQUAL, UINT64_ALLOCATOR> UINT64_SET;

typedef const char * C_STR;

typedef mempool_allocator<C_STR> C_STR_ALLOCATOR;
typedef vector<C_STR, C_STR_ALLOCATOR> C_STR_VEC;
typedef C_STR_VEC::iterator C_STR_VEC_ITER;

typedef mempool_allocator<STRING> STRING_ALLOCATOR;
typedef vector<STRING, STRING_ALLOCATOR> STRING_VEC;

typedef mempool_allocator< pair<UINT32, UINT32> > UINT32_ALLOCATOR;
typedef hash_map<UINT32, UINT32, __gnu_cxx::hash<UINT32>, __gnu_cxx::equal_to<UINT32>, UINT32_ALLOCATOR> UINT32_MAP;
typedef UINT32_MAP::iterator UINT32_MAP_ITER;

// =============================================================================
//
// VALUE_RANGE
//    1. when _lower < _upper, it represents [_lower, _upper)
//    2. when _lower == _upper, it represents (-INF, _lower) && (_lower, +INF)
//    3. when _lower > _upper, it means nothing for now
//
// =============================================================================
class VALUE_RANGE {
private:
  INT64  _lower;
  INT64  _upper;

  VALUE_RANGE(void);                            // REQUIRED UNDEFINED UNWANTED methods
  VALUE_RANGE(const VALUE_RANGE&);              // REQUIRED UNDEFINED UNWANTED methods
  VALUE_RANGE& operator = (const VALUE_RANGE&); // REQUIRED UNDEFINED UNWANTED methods

public:
  VALUE_RANGE(INT64 lower, INT64 upper)
  {
    FmtAssert(lower <= upper, ("Lower bound is greater than upper bound!"));
    _lower = lower;
    _upper = upper;
  }

  INT64 Lower(void) const { return _lower; }
  INT64 Upper(void) const { return _upper; }

  BOOL   In_range(INT64 val) {
    if (_lower == _upper) {
      return val != _lower;
    }
    else if (_lower < _upper) {
      return _lower <= val && val < _upper;
    }
    else {
      return FALSE;
    }
  }

  BOOL   Not_equal(INT64 val) {
    if (_lower == _upper) {
      return val == _lower;
    }
    else if (_lower < _upper) {
      return val < _lower || _upper <= val;
    }
    else {
      return FALSE;
    }
  }
};

typedef mempool_allocator<VALUE_RANGE*>     VR_ALLOCATOR;
typedef vector<VALUE_RANGE*, VR_ALLOCATOR>  VALUE_RANGE_VEC;
typedef pair<IDTYPE, VALUE_RANGE_VEC*>      VR_VEC_PAIR;
typedef mempool_allocator<VR_VEC_PAIR>      VR_PAIR_ALLOCATOR;
typedef hash_map<IDTYPE, VALUE_RANGE_VEC*, IDTYPE_HASHER, IDTYPE_EQUAL, VR_PAIR_ALLOCATOR> VR_MAP;

// =============================================================================
//
// STRING_BUFFER
//
// maintain buffer to compose a string
//
// =============================================================================
class STRING_BUFFER {
private:
  CXX_MEM_POOL  _loc_pool;   // pool to allocate buffer memory
  char         *_current;    // current pointer to buffer
  char         *_base;       // base address to buffer
  char         *_top;        // top address to buffer

public:
  // constructor
  STRING_BUFFER(char* buf, INT size)
    : _loc_pool("STRING_BUFFER_pool", FALSE),
      _current(buf), _base(buf), _top(buf + size) { }

  // buffer memory will be released along with STRING_BUFFER deconstruct
  STRING_BUFFER(INT size)
    : _loc_pool("STRING_BUFFER_pool", FALSE)
  {
    _current = (char *) MEM_POOL_Alloc(Loc_pool(), size);
    _base = _current;
    _top = _current + size;
  }

  MEM_POOL *Loc_pool() { return _loc_pool(); }

public:
  operator const char*()   const   { return To_string();        }
  char        *Current()   const   { return _current;           }
  INT          Avail()     const   { return _top - _current;    }
  const char  *To_string() const;
  void         Adjust_current(INT ofst);
  void         Adjust_current();
  BOOL         Append(char ch);
  BOOL         Append(const char* str, BOOL escape = FALSE);
  BOOL         Append_n(const char *str, INT len, BOOL escape = FALSE);
  BOOL         Append_stname(const char *str);
  BOOL         Format(const char* fmt, ...);
};

extern BOOL Is_magic_symbol(const char *var);
extern void Register_ident_asgn(WN *wn);

//class SRCPOS_HANDLE;
//extern void Vsa_error_print(const char *vname, const char *pname, const char *aname, SRCPOS spos, INT ecode, const char *errmsg, const char *key, SRCPOS_HANDLE *srcpos_h = NULL, const char* cname = "");
//extern void Rbc_error_print(const char *vname, const char *pname, const char *rname, SRCPOS spos, BOOL maybe, const char *key, SRCPOS_HANDLE *srcpos_h = NULL, const char* cname = "");

class VSA_ISSUE;
extern void Vsa_error_print(const VSA_ISSUE* issue);

extern char *Vsa_sym_name(AUX_STAB_ENTRY* sym);
extern const char *Vsa_rule_name(UINT32 rule_id);

#define ISKEY_MAX_FILE_LEN 256
#define ISKEY_MAX_FUNC_LEN 128
#define ISKEY_MAX_VAR_LEN  256
#define ISKEY_MAX_ANA_LEN    4
#define ISKEY_MAX_PATH_LEN 360
#define ISKEY_PATH_FUNC_LEN 16
#define ISKEY_MAX_CAT_LEN   8
#define ISKEY_MAX_RULE_LEN  20
#define ISKEY_MAX_KEY_LEN  (256+128+256+4+360+20+10+20)
extern const char *Vsa_spos_key(char* buf, INT len, SRCPOS cur_spos, SRCPOS entry_spos, INLCXT* inlcxt);
extern const char *Vsa_issue_key(char* buf, INT len, INT fileno, const char* fname, const char* vname, const char* aname, const char* path, const char* cname = "", const char* rname = "");

extern BOOL Vsa_check_free_heap(const char *name);
extern BOOL Vsa_check_alloc_heap(const char *name);
extern BOOL Vsa_check_no_sideffect(const char *name);
extern ALIAS_INTENT Vsa_check_alias_intent(const char *name);
extern BOOL Vsa_part_sym_ignore(const char *name);
extern BOOL Vsa_check_regex_sym_ignore(const char *name);
extern BOOL Vsa_check_sym_ignore(const char *name);
extern BOOL Vsa_check_path_ignore(const char *path);
extern BOOL Vsa_check_var_no_sideffect(const char *name);
extern TAINTMODEL Vsa_check_func_arg_tainted(const char *name, IDTYPE* which_arg);
extern BOOL Is_ident_asgn(const WN *wn);
extern BOOL Is_EH_rt_call(const char *name);
extern BOOL Is_struct_ptr(CODEREP *x);
extern BOOL Is_initv_zero(UINT32 file, INITV_IDX initv, INT ofst);
extern BOOL Get_initv_from_offset(UINT32 file_idx, INITV_IDX initv_idx, INT offset, INITV_IDX *n_initv_idx);
extern BOOL Is_alloc_not_initialized(STMTREP* call);
extern BOOL Is_const_addr_safe(UINT64 addr);
extern BOOL Is_sym_regex_ignore(ITENT_DESC* it, const char *name);
extern INTRINSIC Get_call_intrinsic(STMTREP* call);
extern INT  Count_cr_kids(CODEREP *cr);
extern CODEREP* Is_new_thread_created(STMTREP *call);
extern CODEREP* Is_register_signal_handler(STMTREP *call);

extern FLD_HANDLE Get_java_array_data_fld(CODEREP* cr);
extern CODEREP* Vsa_malloc_size_opnd(STMTREP* call, CODEREP* ret_cr, COMP_UNIT *cu);
extern IDTYPE   Vsa_free_ptr_index(const char *name);
extern CODEREP* Vsa_free_ptr_opnd(STMTREP* call);
extern CODEREP* Find_base_pointer_load(CODEREP *x);
extern CODEREP* Find_ilod_base(CODEREP *x);
extern STMTREP* Find_ptr_defstmt(CODEREP *x);
extern ST* Get_vtab_st_from_class(ST* cls_st);
extern ST* Get_vtab_entry(ST * vst, int offset, BOOL kind);
extern int Get_base_offset_from_vtable(ST * vst);
extern BOOL     Is_valid_lookup_virt_op(CODEREP *cr);
extern BOOL     Is_stmt_virtual_call(STMTREP *stmt);
/* Check for interface call by its LOOKUP_IF instrinsic op */
extern BOOL     Is_lookup_virt_op_interface_call(CODEREP *cr);
extern CODEREP* Get_lookup_virt_original_target_info(CODEREP *cr);
extern const char *Get_lookup_virt_mangled_function_name(CODEREP *cr);
extern BOOL     Get_icall_info(CODEREP *cr, TY_IDX &ty_idx);
extern BOOL     Get_virtual_call_ofst(CODEREP *cr, INT32 &ofst);
extern BOOL     Get_intrfc_call_info(CODEREP *, INT32&, TY_IDX&);
#if 0
extern CODEREP* Get_intrfc_call_addr(CODEREP*);
extern TY_IDX   Get_intrfc_call_ty(CODEREP *);
extern INT32    Get_intrfc_call_ofst(CODEREP *);
#endif
extern SRCPOS   Get_bb_first_linenum(BB_NODE* bb);
extern SRCPOS   Get_bb_last_linenum(BB_NODE* bb);

char *Vsa_demangle(const char *sym_name, BOOL append_paren = FALSE);
extern char    *Get_class_const_value(ST *st, UINT32 offset);
extern char    *Get_string_from_str_const_sym(ST_IDX st);
extern regex_t** Compile_pattern_arr(const char *pattern_arr[], INT32 len, BOOL allow_wrong_pattern, MEM_POOL *pool);
extern char    *Get_utf_string_from_inito(UINT32_MAP *st_inito_cache, ST_IDX st_idx, MEM_POOL *pool);
extern BOOL     Is_lib_func(char *fname);

extern TY_IDX   Get_first_real_field(TY_IDX ty);

extern BOOL     Is_mmap_retval(CODEREP *cr);

#define         BUILTIN_XVSA_RANGE     "__builtin_xvsa_range"
#define         BUILTIN_XVSA_NE        "__builtin_xvsa_ne"
#define         BUILTIN_XVSA_COMPARE   "__builtin_xvsa_compare"
OPERATOR        Get_opr_from_char(const char *opr);

inline BOOL
Is_target_system_library() { return VSA_Target_Env == VSA_ENV_SYS_LIB; }

#endif  /* opt_vsa_util_INCLUDED */
