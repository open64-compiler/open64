/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#include <cstdlib>
#include <iostream>
#include <fstream>

#include <errno.h>		    /* for sys_errlist */
#include <stdio.h>		    /* for stderr */
#include <unistd.h>                 /* for unlink */
#include <libgen.h>		    /* for basename() */
#include <sys/stat.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "mempool.h"
#include "wn.h"			    /* for ir_reader.h */
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"		    /* for IR_reader_init(), etc. */
#include "ir_bwrite.h"		    /* for WN_open_output(), etc. */
#include "ir_bread.h"		    /* for WN_open_input(), etc. */
#include "dwarf_DST_dump.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "config.h"
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"              /* for Last_Dedicated_Preg_Offset */

#include <vector>
#include <ext/hash_set>
#include <ext/hash_map>

BOOL Run_vsaopt = FALSE; // hack to workaround undefine since

// ==================================================================
// Common definitions
// ==================================================================

// Output language
enum OUTPUT_LANG {
  L_JAVASCRIPT,       // generate ffi wrapper for javascript
  L_TYPESCRIPT,       // generate ffi wrapper for typescript
  L_PYTHON,           // generate ffi wrapper for python
  L_LAST,             // last unknown language
};

// Type hint
enum TYPE_HINT {
  NORMAL,             // normal type (same representation as WHIRL)
  ARRAY,              // array type
  CSTRING,            // C string type
  POINTER,            // pointer type
  REFERENCE,          // reference type
};

// Language description
struct LangDesc {
  OUTPUT_LANG _lang;  // Language
  const char *_name;  // name, short name
  const char *_desc;  // desc, full name
};

// all current supported foreign languages
static const LangDesc lang_desc[] = {
  { L_JAVASCRIPT, "js", "JavsScript" },   // JavaScript
  { L_TYPESCRIPT, "ts", "TypeScript" },   // TypeScript
  { L_PYTHON,     "py", "Python"     },   // Python
};

// API info
struct ApiInfo {
  const char         *_name;              // API name
  UINT32              _ret_ty;            // API return type
  std::vector<UINT32> _parm_ty;           // API parameters
};

// name of wffi library
const char *libwffi_name = "libwffi.so";

// GetLangId
// Get OUTPUT_LANG from name
static OUTPUT_LANG
GetLangId(const char *name) {
  for (int i = 0; i < sizeof(lang_desc)/sizeof(lang_desc[0]); ++i) {
    if (strcasecmp(name, lang_desc[i]._name) == 0 ||
        strcasecmp(name, lang_desc[i]._desc) == 0)
      return lang_desc[i]._lang;
  }
  return L_LAST;
}

// GetLangName
static const char *
GetLangName(OUTPUT_LANG lang) {
  if (lang >= L_LAST)
    return  "*bad lang*";
  else
    return lang_desc[lang]._desc;
}

// GetTypeWithHint
static UINT32
GetTypeWithHint(UINT32 ty, TYPE_HINT hint) {
  return (TY_IDX_index(ty) << 8) | hint;
}

// GetTypeHint
static TYPE_HINT
GetTypeHint(UINT32 ty) {
  return (TYPE_HINT)(ty & 0xFF);
}

// GetTypeHintFromWN
static TYPE_HINT
GetTypeHintFromWN(WN *wn) {
  Is_True(WN_operator(wn) == OPR_STID ||             // return value
          WN_operator(wn) == OPR_LDID ||             // ptr/ref
          WN_operator(wn) == OPR_LDA, ("bad wn"));   // array
  const char* name = ST_name(WN_st(wn));
  // LDA of array type
  if (WN_operator(wn) == OPR_LDA)
    return ARRAY;
  else if (strncmp(name, "cstr", 4) == 0)
    return CSTRING;
  else if (strncmp(name, "ptr", 3) == 0)
    return POINTER;
  else if (strncmp(name, "ref", 3) == 0)
    return REFERENCE;
  else {
    Is_True(FALSE, ("bad st name"));
    return NORMAL;
  }
}

// ==================================================================
// Globaal variables
// ==================================================================
OUTPUT_LANG output_lang;   // output language
const char *input_file;    // input whirl file
const char *output_file;   // output ffi interface file

// include implementation for js generator
#include "whirl_ffi_js.h"
// include implementation for ts generator
#include "whirl_ffi_ts.h"
// include implementation for py generator
#include "whirl_ffi_py.h"
// other languages may continue here

// ==================================================================
// CodeGen
// base class for all CodeGen to generate code for ffi wrapper in
// target language
// ==================================================================
class CodeGen {
private:
  typedef __gnu_cxx::hash_map<UINT32, UINT32> TY_MAP;
  typedef __gnu_cxx::hash_set<ST_IDX>         ST_MAP;
  TY_MAP               _ty_map;      // check if ty already added
  ST_MAP               _st_set;      // check if st already added
  std::vector<UINT32>  _ty_array;    // types to be emitted
  std::vector<ApiInfo> _st_array;    // symbols to be emitted
  const char          *_ofname;      // output file name
  FILE                *_output;      // output file
  OUTPUT_LANG          _output_lang; // output language

private:
  // write output file header
  template<typename _Impl>
  void Write_header(const _Impl& impl) {
    impl.Write_header(_output);
  }

  // write output file footer
  template<typename _Impl>
  void Write_footer(const _Impl& impl) {
    impl.Write_footer(_output);
  }

  // start type section
  template<typename _Impl>
  void Write_type_begin(const _Impl& impl) {
    impl.Write_type_begin(_output);
  }

  // write single type
  template<typename _Impl>
  void Write_type(const _Impl& impl, UINT32 ty) {
    impl.Write_type(_output, ty, TRUE);
  }

  // finish type section
  template<typename _Impl>
  void Write_type_end(const _Impl& impl) {
    impl.Write_type_end(_output);
  }

  // start symbol section
  template<typename _Impl>
  void Write_symbol_begin(const _Impl& impl) {
    impl.Write_symbol_begin(_output);
  }

  // write single symbol
  template<typename _Impl>
  void Write_symbol(const _Impl& impl, const ApiInfo &api, BOOL lastone) {
    impl.Write_symbol(_output, api, lastone);
  }

  // finish symbol section
  template<typename _Impl>
  void Write_symbol_end(const _Impl& impl) {
    impl.Write_symbol_end(_output);
  }

private:
  // write copyright at beginning of the file
  template<typename _Impl>
  void Write_copyright(const _Impl &impl) {
    const char* lc = impl.Line_comment();
    struct tm tm;
    time_t now = time(NULL);
    localtime_r(&now, &tm);
    Is_True(_output != NULL, ("invalid output file"));
    fprintf(_output, "%s %s\n", lc, _ofname);
    if (tm.tm_year <= 2020)
      fprintf(_output, "%s Copyright (C) 2020 Xcalibyte Limited, Inc.  "
                       "All Rights Reserved\n\n", lc);
    else
      fprintf(_output, "%s Copyright (C) 2020-%04d Xcalibyte Limited, Inc.  "
                       "All Rights Reserved\n\n", lc, tm.tm_year);
    fprintf(_output, "%s WHIRL Foreign Function Interface (WFFI) for %s\n",
                     lc, impl.Lang_name());
    fprintf(_output, "%s Generated on %04d-%02d-%02d %02d:%02d:%02d\n",
                     lc, tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
                     tm.tm_hour, tm.tm_min, tm.tm_sec);
    fprintf(_output, "%s DO NOT CHANGE THIS FILE BY HAND!\n\n", lc);
  }

  // generate the wffi file
  template<typename _Impl>
  void Generate() {
    _Impl impl;
    // write copyright
    Write_copyright(impl);
    // write header
    Write_header(impl);
    // write types
    {
      Write_type_begin(impl);
      std::vector<UINT32>::const_iterator it = _ty_array.begin();
      std::vector<UINT32>::const_iterator end = _ty_array.end();
      for ( ; it != end ; ++it ) {
        Write_type(impl, *it);
      }
      Write_type_end(impl);
    }
    // write symbols
    {
      Write_symbol_begin(impl);
      std::vector<ApiInfo>::const_iterator it = _st_array.begin();
      std::vector<ApiInfo>::const_iterator end = _st_array.end();
      while (it != end) {
        const ApiInfo &info = *it;
        ++it;
        Write_symbol(impl, info, it == end);
      }
      Write_symbol_end(impl);
    }
    // write footer and finish file
    Write_footer(impl);
  }

  // generate the wffi file
  void Generate_output() {
    switch (_output_lang) {
    case L_JAVASCRIPT:
      Generate<JavaScriptCodeGen>();
      break;
    case L_TYPESCRIPT:
    case L_PYTHON:
    default:
      Is_True(FALSE, ("TODO support lang %s", GetLangName(_output_lang)));
      break;
    }
  }

  // check if a symbol has been added
  BOOL Is_st_added(ST_IDX st) {
    if (_st_set.find(st) != _st_set.end())
      return TRUE;
    _st_set.insert(st);
    return FALSE;
  }

  // check or add a ty, return the combination of TY_IDX_index and hint
  UINT32 Add_ty(TY_IDX ty, TYPE_HINT hint) {
    UINT32 ty_idx = TY_IDX_index(ty);
    TY_MAP::iterator it = _ty_map.find(ty_idx);
    if (it != _ty_map.end())
      return it->second;
    UINT32 ret = (ty_idx << 8) | hint;
    _ty_array.push_back(ret);
    _ty_map.insert(std::make_pair(ty_idx, ret));
    return ret;
  }

  // get internal type representation for a whirl TY with hint 
  UINT32 Get_type(TY_IDX ty, TYPE_HINT hint) {
    if (TY_kind(ty) == KIND_STRUCT) {
      Is_True(FALSE, ("TODO: handle struct field before add type"));
    }
    return Add_ty(ty, hint);
  }

public:
  // constructor
  CodeGen(OUTPUT_LANG lang, const char* ofname)
     : _ofname(ofname), _output(NULL), _output_lang(lang) { }

  // destructor
  ~CodeGen() {
    if (_output && _output != stdout) {
      // if _output is not null, means Finalize() isn't called
      // close and remove this file
      fclose(_output);
      unlink(_ofname);
    }
  }

  // initialize codegen, open the output file
  BOOL Initialize() {
    if (_ofname == NULL) {
      _ofname = "<stdout>";
      _output = stdout;
    }
    else {
      _output = fopen(_ofname, "w");
      if (_output == NULL) {
        fprintf(stderr, "Error: failed to open %s: %s\n",
                _ofname, strerror(errno));
        return FALSE;
      }
    }
    return TRUE;
  }

  // finalize the codegen, write types/symbols to output file
  void Finalize() {
    Is_True(_output != NULL, ("bad output file"));
    // generate wrapper
    Generate_output();
    // flush or close output file
    if (_output == stdout)
      fflush(_output);
    else
      fclose(_output);
    _output = NULL;
  }

  // handle WHIRL CALL node to get the symbols and their types
  void Handle_call(WN *call);
};


// CodeGen::Handle_call
// Handle call to get ty & st
void
CodeGen::Handle_call(WN *call)
{
  Is_True(call && WN_operator(call) == OPR_CALL, ("not call"));
  ST* st = WN_st(call);
  Is_True(st && ST_class(st) == CLASS_FUNC, ("not func symbol"));
  // check if st has been added
  if (Is_st_added(ST_st_idx(st)))
    return;

  // construct a new ApiInfo and add to array
  _st_array.push_back(ApiInfo());
  ApiInfo& api_info = _st_array.back();

  // set api name
  api_info._name = ST_name(st);

  // check function type
  TY_IDX ty = ST_type(st);
  Is_True(TY_kind(ty) == KIND_FUNCTION, ("not func type"));
  TYLIST_IDX tylist = TY_tylist(ty);
  Is_True(tylist > 0 && tylist < TYLIST_Table_Size(), ("bad tylist"));
  // check return type
  TY_IDX rty = Tylist_Table[tylist];
  Is_True(rty > 0 && TY_IDX_index(rty) < TY_Table_Size(),
          ("bad rty"));
  // TODO: check return value st for array/cstring/pointer/reference/etc
  api_info._ret_ty = Get_type(rty, NORMAL);
  ++ tylist;

  // check parameter type
  INT i = 0;
  TY_IDX pty = Tylist_Table[tylist];
  while (pty != 0) {
    TYPE_HINT hint = NORMAL;
    if (TY_kind(pty) != KIND_SCALAR) {
      WN *parm = WN_kid(call, i);
      Is_True(parm && WN_operator(parm) == OPR_PARM, ("bad parm"));
      // get hint from whirl node
      hint = GetTypeHintFromWN(WN_kid0(parm));
    }
    // add type to parameter vector
    api_info._parm_ty.push_back(Get_type(pty, hint));
    ++ tylist;
    ++i;
    pty = Tylist_Table[tylist];
  };
}

// ==================================================================
// main driver starts below
// ==================================================================

// process_block
// traverse block and check each stmt
static void
process_block(CodeGen *cg, WN *block) {
  Is_True(WN_operator(block) == OPR_BLOCK, ("not block"));
  for (WN* wn = WN_first(block); wn != NULL; wn = WN_next(wn)) {
    switch (WN_operator(wn)) {
    case OPR_CALL:
      // check call and add call st if needed
      cg->Handle_call(wn);
      break;
    case OPR_BLOCK:
      // check block
      process_block(cg, wn);
      break;
    default:
      break;
    }
  }
}

// process_file
// open file and traverse IR to get all wffi APIs and write to output file
static void
process_file (const char *global_file, const char *input_file,
        OUTPUT_LANG lang, const char *output_file) {
  // create CodeGen object
  CodeGen cg(lang, output_file);
  if (cg.Initialize() == FALSE)
    return;

  // read input IR file
  PU_Info *pu_tree;
  if (global_file == NULL) {
    (void)Open_Input_Info ((char *)input_file);
  }
  else {
    (void)Open_Global_Input ((char *)global_file);
    (void)Open_Local_Input ((char *)input_file);
  }

  Initialize_Symbol_Tables (FALSE);
  New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
  pu_tree = Read_Global_Info (NULL);

  IR_reader_init();
  IR_Dwarf_Gen_File_Table(FALSE);  // this will set srcFileTable

  // set the language based on the first PU
  Is_True(pu_tree != NULL &&
          PU_Info_next(pu_tree) == NULL &&
          PU_Info_child(pu_tree) == NULL, ("TODO: more than 1 pu"));
  if (pu_tree != NULL) {
    PU_Info *pu = pu_tree;
    Read_Local_Info(MEM_pu_nz_pool_ptr, pu);
    WN *wn = PU_Info_tree_ptr(pu);
    Is_True(WN_operator(wn) == OPR_FUNC_ENTRY, ("not func entry"));
    // process func body
    process_block(&cg, WN_func_body(wn));
    Free_Local_Info(pu);
  }

  // Finalize codegen
  cg.Finalize();

  Free_Input_Info ();
} // process_file

// file_exists
// check if the file exists
static BOOL
file_exists(const char *path)
{
  INT st;
  struct stat sbuf;
  st = stat(path, &sbuf);
  if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
    return FALSE;
  else
    return TRUE;
}

// usage
// output usage and exit
static void
usage(const char *progname)
{
  fprintf (stderr, "Usage: %s -x <output_lang> <whirl_file> [ -o output_file>\n", progname);
  exit (1);
}

// main
int main (INT argc, char *argv[])
{
  const char *progname;
  INT a2b, b2a, sel, all;

  MEM_Initialize();
  Set_Error_Tables (Phases, host_errlist);
  Init_Error_Handler (10);
  Set_Error_File(NULL);
  Set_Error_Line(ERROR_LINE_UNKNOWN);
  WHIRL_Mldid_Mstid_On = TRUE;
  Pointer_Size = 8;  // needed?

  progname = basename (argv[0]);
  if (*progname == '/') ++progname;

  Read_Global_Data = NULL;
  if (argc < 4)
    usage(progname);

  int i = 1;
  output_lang = L_LAST;
  while (i < argc) {
    // -x lang
    if (strcmp(argv[i], "-x") == 0) {
      ++ i;
      if (i < argc)
        output_lang = GetLangId(argv[i]);
    }
    // -o output file, or stdout if omitted
    else if (strcmp(argv[i], "-o") == 0) {
      ++ i;
      if (i < argc)
         output_file = argv[i];
    }
    else if (input_file == NULL) {
      input_file = argv[i];
    }
    else {
      // only 1 input file is allowed
      usage(progname);
    }
    ++ i;
  }

  if (output_lang == L_LAST ||
      input_file == NULL ||
      !file_exists(input_file))
    usage(progname);

  process_file (Read_Global_Data, input_file, output_lang, output_file);

  exit (0);
} /* main */

// Signal_Cleanup
// remove output file if signal hit
void
Signal_Cleanup (INT sig) {
  if (file_exists(output_file))
    unlink(output_file);
}

// to fix link error
const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
  fprintf (stderr, "Internal: Host_Format_Parm () not implemented\n");
  return "";
}

INT8 Debug_Level = 0;
INT trace_verbose = FALSE;
