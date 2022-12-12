/*
   Copyright (C) 2021-2022 Xcalibyte (Shenzhen) Limited.

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

#include <map>
#include <cstdarg>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <unordered_set>
#include <unordered_map>

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Transforms/Scalar/InstSimplifyPass.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/BDCE.h"
#include "llvm/Transforms/Utils/Mem2Reg.h"
 #include "llvm/Transforms/IPO/GlobalDCE.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"


#include <errno.h>            /* for sys_errlist */
#include <stdio.h>            /* for stderr */
#include <libgen.h>           /* for basename() */
#include <sys/stat.h>
#if ! defined(BUILD_OS_DARWIN)
#include <elf.h>
#endif /* ! defined(BUILD_OS_DARWIN) */
#include "defs.h"
#include "util.h"
#include "mempool.h"
#include "wn.h"               /* for ir_reader.h */
#include "stab.h"
#include "const.h"
#include "pu_info.h"
#include "irbdata.h"
#include "ir_reader.h"        /* for IR_reader_init(), etc. */
#include "ir_bwrite.h"        /* for WN_open_output(), etc. */
#include "ir_bread.h"         /* for WN_open_input(), etc. */
#include "dwarf_DST_dump.h"
#include "vho_lower.h"
#include "be_symtab.h"        /* for Be_preg_tab */
#include "wn_lower.h"
#include "erglob.h"
#include "errors.h"
#include "err_host.tab"
#include "cxx_memory.h"       /* CXX_NEW */
#include "config_targ_opt.h"  /* ABI_Name */
#include "config.h"
#include "config_vho.h"       /* VHO_Struct_Opt */
#include "tracing.h"
#include "intrn_info.h"
#include "targ_sim.h"         /* for Last_Dedicated_Preg_Offset */
#include "ttype.h"
#include "data_layout.h"


BOOL Run_vsaopt = FALSE;      // hack to workaround undefine since

// SC - 2022 Hack to enable Is_True and FmtAssert working without
//      having to bring in the whole bunch of things (such as config
//      errors.{c,h}
//      Backtrace is turned off since GDB and IDE can do that anyway

#ifndef ES_ERRABORT           // another hack for FmtAssert
#define ES_ERRABORT 11
#endif
/* ====================================================================
 *
 * Fail_FmtAssertion
 *
 * A FmtAssertion has failed -- report the fact.
 *
 * Report a failure with the given printf format and parameters to the
 * error file(s), and then abort.
 *
 * ====================================================================
 */

// SC - 2022
// From errors.c and errors.h
//
#ifdef Is_True_On
# define Is_True FmtAssert
#else
# define Is_True(a, b)  ((void)1)
#endif


static INT Compiler_Line = 0;
static const char *Compiler_File = NULL;

#ifdef Abort_Compiler_Location
#undef Abort_Compiler_Location
#endif
#define Abort_Compiler_Location __Abort_Compiler_Location

void __Abort_Compiler_Location (
  const char * file_name,
  INT    line_number )
{
  Compiler_File = file_name;
  Compiler_Line = line_number;
}

#ifdef FmtAssert
#undef FmtAssert
#define FmtAssert(Cond,ParmList)                             \
  (Cond ? (void)1                                            \
          : (Abort_Compiler_Location ( __FILE__, __LINE__ ), \
      w2ll_Fail_FmtAssertion ParmList) )
#endif

/* Severity descriptors: */
typedef struct {
    INT  level;    /* Severity level */
    char symbol[5];  /* Symbol used to prefix messages */
    const char *name;  /* Name used in message headers */
} SEVERITY_DESCRIPTOR;

static SEVERITY_DESCRIPTOR Severities[] = {
    {ES_IGNORE,		"??? ",	"Ignore"},
    {ES_ADVISORY,	"--- ", "Advisory"},
    {ES_WARNING,	"!!! ", "Warning"},
    {ES_SAADV1,  	"!!! ", "[Vul],[D]"},
    {ES_SAADV2,  	"!!! ", "[Vul],[M]"},
    {ES_SAADV3,  	"!!! ", "[Vul],[R]"},
    {ES_SAADV4,  	"!!! ", "[Pfm],[D]"},
    {ES_CONFORMANCE,	"!!! ", "[SML],[D]"},
    {ES_CONFORMANCE_M,	"!!! ", "[SML],[M]"},
    {ES_ERRBENIGN,	"### ", "Error"},
    {ES_ERRPHASE,	"### ", "Error"},
    {ES_ERRABORT,	"### ", "Error"},    
};

/* Access functions: */
#define SEV_level(n)  (Severities[n].level)
#define SEV_symbol(n) (Severities[n].symbol)
#define SEV_name(n)   (Severities[n].name)
//  end - SC - 2022

// =============================================================================
// Macros and Toggle for Tracing
// =============================================================================
/* Is_Trace */
#ifndef Is_Trace
#ifdef Is_True_On
#define Is_Trace(Cond, Parmlist) { if (Cond) fprintf Parmlist ; }
#define Is_Trace_cmd(Cond, Cmd) { if (Cond) Cmd ; }
#else
#define Is_Trace(Cond, Parmlist) ((void) 1)
#define Is_Trace_cmd(Cond, Cmd)  ((void) 1)
#endif
#endif

#ifdef Is_True_On
static BOOL  Tracing_enabled = TRUE;
#else
static BOOL  Tracing_enabled = FALSE;
#endif

/* The trace file descriptor, to be used in fprintf tracing: */
static FILE *TFile_internal = NULL;
char *       TFile_Name = NULL;
BOOL         Non_stdout_TFile = FALSE;

#ifdef TFile
#undef TFile
#define TFile TFile_internal
#endif

char *       Current_file = NULL;
static void
Set_current_file( char *name )
{
  Current_file = name;
}
char *       Current_function = NULL;
static void
Set_current_function( char *name )
{
  Current_function = name;
}
// End of Macros and Toggle for Tracing

#define DevWarn w2ll_DevWarn
static bool dev_warn_enabled = true;

static void
w2ll_DevWarn( const char *fmt, ...)
{
  va_list args;

  if ( dev_warn_enabled ) {
    /* Write to standard error first: */
    /* with newer gcc need to do start/end around each vprintf */
    va_start ( args, fmt );
    fprintf ( stderr, "!!! DevWarn: %s :", Current_function);
    vfprintf ( stderr, fmt, args );
    fprintf ( stderr, "\n" );
    fflush ( stderr );
    va_end(args);

  /* write to trace file: */
  if ( TFile != NULL ) {
    va_start ( args, fmt );
    fprintf ( TFile, "!!! DevWarn: %s :", Current_function);
    vfprintf ( TFile, fmt, args );
    fprintf ( TFile, "\n" );
    fflush ( TFile );
    va_end(args);
  }

  }
}

static void  /* SC 2022 - so that we do not mix up with rest of BE's name */
w2ll_Fail_FmtAssertion ( const char *fmt, ... )
{
  va_list vp;
  
  INT dlevel = ES_ERRABORT;  /* Severity level */
  INT mlevel = dlevel;
  char hmsg[512], emsg[512];
  INT loc;
  hmsg[0] = '\0';
#if 0  // SC - 2022
  /* Count error: */
  ++Error_Counts[dlevel];
  
  /* Prepare header line: */
  loc = sprintf ( &hmsg[0], "%s%s%s", SEV_symbol(mlevel),
      "Compiler ", SEV_name(mlevel) );
  if ( Source_File_Name != NULL && *Source_File_Name != 0 ) {
    loc += sprintf ( &hmsg[loc], " in file %s", Source_File_Name );
  }
  if ( Current_Phase != NULL ) {
    loc += sprintf ( &hmsg[loc], " during %s phase", Current_Phase );
  }
  sprintf ( &hmsg[loc], ":\n" );
#else
  int do_traceback = false;  
#endif // SC - 2022

  /* Prepare main error message: */
  va_start ( vp, fmt );
  loc = sprintf ( &emsg[0], "%s", SEV_symbol(mlevel) );
  loc += vsprintf ( &emsg[loc], fmt, vp );
  sprintf ( &emsg[loc], "\n" );
  va_end ( vp );

  // SC - 2022
  // do not do traceback
  /* do tracestack */
#if 0  // SC - 2022
  do_traceback = true;
  /* Report the error: */
  Emit_Message ( hmsg, emsg, (ERROR_DESC *)0  );
#else
  Is_Trace_cmd(Tracing_enabled, fprintf(TFile, "\n### Assertion failure %s:%d:\n%s%s### For %s in %s\n", Compiler_File, Compiler_Line, hmsg, emsg, Current_file, Current_function););
  do_traceback = false;
  fprintf(stderr,  "\n### Assertion failure %s:%d:\n%s%s### For %s in %s\n", Compiler_File, Compiler_Line, hmsg, emsg, Current_file, Current_function);
#endif  // SC - 2022


  /* Abort: */
  // SC - 2022
#if 0  // SC - 2022  
  Signal_Cleanup( 0 );
  exit(RC_INTERNAL_ERROR);
#else  
  exit(1);
#endif // SC - 2022  
} // w2ll_Fail_FmtAssertion



// SML 2022 - to show current function name in message


// =============================================================================
//
// Tracing Facility created for building this program in ir_tools directory
//
// =============================================================================
#define TRC_FILE_EXTENSION ".t"
#define DBar "================================================================================\n"
#define SBar "--------------------------------------------------------------------------------\n"

static void
Set_trace_file ( char *filename )  /* Name of new trace file */
{
  if (filename == NULL)
    return;

  if ( Non_stdout_TFile && TFile_internal != NULL ) {
    fclose (TFile_internal);
  }

  if ( filename != NULL ) {
    TFile_internal = fopen ( filename, "w" );  /* Truncate */
    if ( TFile_internal != NULL ) {
      setvbuf(TFile_internal,NULL,_IONBF,0);
      TFile_Name = filename;
      Non_stdout_TFile = TRUE;
      return;
    }
    Is_True(FALSE, ("Set_Trace_File call failed"));
  }
  TFile_internal = stdout;
  TFile_Name = NULL;
  Non_stdout_TFile = FALSE;
}

static FILE *Get_trace_file(void) 
{
  if (TFile_internal == NULL)
    TFile_internal = stdout;

  return TFile_internal;
}

void
Trace_To_Stderr(void)
{
  if ( Non_stdout_TFile && TFile_internal != NULL ) {
    fclose (TFile_internal);
  }

  TFile_internal = stderr;
  TFile_Name = NULL;
  Non_stdout_TFile = FALSE;
  Set_Error_Trace (NULL);
}

static char *
New_extension ( const char *name, const char *ext )
{
  char *newname;
  INT16 len, i;

  /* Allocate a new name string: */
  len = strlen(name);
  newname = (char *) malloc ( len + strlen(ext) + 1 );
  strcpy ( newname, name );
  for ( i=len-1; i>=0; i-- ) {
    if ( newname[i] == '/' ) break;  /* Don't touch directory prefixes */
    if ( newname[i] == '.' ) {
      newname[i] = 0;
      break;
    }
  }
  strcat ( newname, ext );
  return newname;
}

char*
Create_trc_filename(char* infile)
{
  if (Tracing_enabled ) {
    return New_extension(infile, TRC_FILE_EXTENSION);
  }
  return NULL;
}

extern void dump_ty(TY_IDX ty_idx);

// =============================================================================
//
// End of Tracing Facility
//
// =============================================================================


extern std::vector<std::string> srcFileTable;

struct ST2llvm;
struct TY2llvm;
struct ST_ATTR2llvm;
struct TY2PTR;
struct COLLECT_LVTY;
struct COLLECT_VALUE;
struct COLLECT_RNA;
struct GEN_JMPTBL;
struct CMP_EQNE;

static bool use_binary = TRUE;

static constexpr uint64_t RoundUpConst(uint64_t offset, uint8_t align) {
  return (-align) & (offset + align - 1);
}

static inline uint64_t RoundUp(uint64_t offset, uint8_t align) {
  if (align == 0) {
    return offset;
  }
  return RoundUpConst(offset, align);
}

static constexpr uint64_t RoundDownConst(uint64_t offset, uint8_t align) {
  return (-align) & offset;
}

static inline uint64_t RoundDown(uint64_t offset, uint8_t align) {
  if (align == 0) {
    return offset;
  }
  return RoundDownConst(offset, align);
}

typedef llvm::Type            LVTY;
typedef std::vector<LVTY*>    LVTYVEC;
typedef std::vector<TY_IDX>   TYIDXVEC;
typedef llvm::FunctionType    LVFUNCTY;
typedef llvm::Function        LVFUNC;
typedef llvm::Module          LVMODULE;
typedef llvm::DIBuilder       LVDIBUILDER;
typedef llvm::DICompileUnit   LVDICU;
typedef llvm::DILocation      LVDILOC;
typedef llvm::DIScope         LVDISCOPE;
typedef llvm::DIFile          LVDIFILE;
typedef llvm::LLVMContext     LVCONTEXT;
typedef llvm::IRBuilder<>     LVBUILDER;
typedef llvm::BasicBlock      LVBB;
typedef llvm::Value           LVVAL;
typedef llvm::Instruction     LVINST;
typedef std::vector<LVVAL*>   LVVALVEC;
typedef llvm::AllocaInst      LVALC;
typedef llvm::GlobalVariable  LVGLBVAR;
typedef llvm::CallInst        LVCALL;
typedef llvm::Attribute       LVATTR;
typedef llvm::Constant        LVCONST;
typedef std::vector<LVCONST*> LVCONSTVEC;
typedef llvm::BlockAddress    LVBLKADDR;
typedef llvm::GlobalAlias     LVGLBALIAS;
typedef llvm::PointerType     LVPTRTY;
typedef llvm::StructType      LVSTRUCTTY;

// =============================================================================
//
//
// =============================================================================
static const char *OPERATOR2name(OPERATOR opr) { return OPERATOR_name(opr); }

// =============================================================================
// =============================================================================
//
// W2LBB: wrap the llvm::IRBuilder
//        WN* for W2LBB to WN*
//        label name, extracted from WN* or fabricated for fallthrough 
//
// =============================================================================
// =============================================================================
class W2LBB {
private:
  LVBB       *_lvbb;
  const char *_labname;
  WN         *_wn;      // the WHIRL Label node, contains LabelIdx
  LVBLKADDR  *_blkaddr; // the llvm::BlockAddress for the label
  W2LBB(void);                      // REQUIRED UNDEFINED UNWANTED methods
  W2LBB(const W2LBB &);             // REQUIRED UNDEFINED UNWANTED methods
  W2LBB& operator = (const W2LBB&); // REQUIRED UNDEFINED UNWANTED methods
public:
  W2LBB(LVBB *lvbb, const char *labname)
    : _lvbb(lvbb), _labname(labname), _wn(nullptr), _blkaddr(nullptr) { }
  ~W2LBB(void) { }

  LVBB *Lvbb(void)          { return _lvbb;    }
  WN   *Wn(void)            { return _wn;      }
  void  Wn(WN *wn)          { _wn = wn;        }
  const char *Labname(void) { return _labname; }
  LVBLKADDR *Blkaddr(void)  { return _blkaddr; }
  void       Blkaddr(LVBLKADDR *blkaddr) {
    FmtAssert(blkaddr != nullptr, ("W2LBB::Blkaddr: blkaddr is nullptr"));
    _blkaddr = blkaddr;
  }
  void  Print(FILE *fp);
};  // class W2LBB

void
W2LBB::Print(FILE *fp)
{
  fprintf(fp, "Lable Name = %s: LLVM BB = %p\n", _labname, _lvbb);
  if (_wn) fdump_wn(fp, _wn);
}


// =============================================================================
// AGGMAP maintain map from original param name of aggregate type to its new
// name with its mtype.
// =============================================================================
class AGGMAP {
private:
  char   *_name;   // new param name, %s%s%d, orig_name, SEPARATOR, fld_offset
  INT32   _regno;  // the input parameter preg number
  TY_IDX  _type;   // the type of the new parameter

  AGGMAP(void);                      // REQUIRED UNDEFINED UNWANTED methods
  //AGGMAP(const AGGMAP &);          // Needed by std::vector::push_back
  AGGMAP& operator = (const AGGMAP&);// REQUIRED UNDEFINED UNWANTED methods

  void    Print(FILE *fp) {
    fprintf(fp, "Aggmap:{name = %s, regno = %d, type = %d}",
            _name, _regno, _type);
  }

public:
  AGGMAP(char *nm, INT32 rn, TYPE_ID ty): _name(nm), _regno(rn), _type(ty) {
    Is_Trace_cmd(TRUE, Print(TFile));
  }
  ~AGGMAP(void) { }

  char   *Name(void)      { return _name; }
  INT32   Regno(void)     { return _regno;}
  TY_IDX  Type(void)      { return _type; }
  void    Type(TY_IDX t)  { _type = t;    }
};

class W2LFILE {
private:
  DST_FILE_NAME *_dst_file_name;
  LVDIFILE      *_lvdi_file;
  W2LFILE(void);                      // REQUIRED UNDEFINED UNWANTED methods

public:
  W2LFILE(DST_FILE_NAME *dst_file_name) : _dst_file_name(dst_file_name) {
    _lvdi_file = nullptr;
  }

  LVDIFILE *Lvdi_file(void) {
    return _lvdi_file;
  }

  void      Lvdi_file(LVDIFILE *f) {
    _lvdi_file = f;
  }

  DST_FILE_NAME* Dst_File_Name(void) { 
    return _dst_file_name;
  }
};

typedef std::pair<LVTY*, LVALC*> TYALC;
typedef std::pair<char*, INT32>  PNMREG;
typedef std::pair<WN*, WN*>      WNPAIR;
typedef std::pair<INT, INT>      PREGPAIR;

typedef std::vector<WNPAIR>                 ARGVEC;
typedef std::vector<WN*>                    WNVEC;
typedef std::vector<W2LBB*>                 W2LBBLIST;
typedef W2LBBLIST::iterator                 W2LBBITER;
typedef std::string                         _STRING;
typedef std::unordered_map<_STRING, W2LBB*> LAB2W2LBB;
typedef std::unordered_map<_STRING, TYALC>  NAM2ALC;
typedef std::unordered_map<_STRING, LVVAL*> NAM2VAL;
typedef std::map<UINT32,  LVTY* >           WTY2LVT;
typedef std::vector<PREGPAIR>               PREGMAP;
typedef std::vector<AGGMAP>                 AGMVEC;
typedef std::vector<INT>                    INTVEC;
typedef std::unordered_map<ST*, LVVAL*>     ST2LVVAL;
typedef std::vector<W2LFILE>                W2LFILEVEC;
typedef std::vector<std::string>            STRVEC;

static BOOL ENABLE_TRACE = TRUE;
template<typename T>
void LVPRINT(const T &n, std::string msg = "", bool err = false) {
  if (!ENABLE_TRACE) return;
  auto &os = err ? llvm::errs() : llvm::outs();
  os << msg << ": ";
  n->print(os);
  os << "\n";
}

// =============================================================================
//
// RNA keeps track of a call in the current PU to support effective debuging.
// _ori_call: original call node, collected before lowering.
// _lo_call: call not after the lowering.
// _actual: a vector of stid nodes that store value of actual arguments; these
// actual arguments are used to create the actual argumen tlist for llvm's call.  
//
// Method of using RNA information:
// step 1: RNAITER rit = whirl2llvm->Find_rna(call_node)
// step 2: WNPAIR rit->Actual(n), n is the nth PARM of the call_node
// if the PARM is M type, the first and second will be filled with the actual arg
// Note: the second may still be nullptr, based on the size of the data type
//
// =============================================================================
class RNA {
private:
  WN    *_ori_call; // original call node in the input WHIRL file
  WN    *_lo_call;  // call node may change if there exists MPARM
  WN    *_ori_retv; // original return value copy statement
  WN    *_lo_retv;  // lowered return value copy statement
  WNVEC  _actvec;   // vector of stid to preg generated by lower
  ARGVEC _argvec;   // actual pregs related to the parm list in _lo_call

  //RNA(void);                 // REQUIRED UNDEFINED UNWANTED methods
  //RNA(const RNA &);          // Needed by std::vector::push_back
  RNA& operator = (const RNA&);// REQUIRED UNDEFINED UNWANTED methods

public:
  RNA():_ori_call(NULL), _lo_call(NULL), _ori_retv(NULL), _lo_retv(NULL) { }
  RNA(WN *ori, WN *ori_retv): _lo_call(NULL), _lo_retv(NULL) {
    _ori_call = WN_COPY_Tree(ori); // make a copy for _ori_call only
    _ori_retv = WN_COPY_Tree(ori_retv);
  }

  ~RNA(void)                  { }

  WN    *Orig(void)           { return _ori_call;   }
  WN    *Orig_retv(void)      { return _ori_retv;   }
  void   Lowered(WN *lwn)     { _lo_call = lwn;     }
  WN    *Lo_retv(void)        { return _lo_retv;    }
  void   Lo_retv(WN *lr)      { _lo_retv = lr;      }
  WN    *Lowered(void)        { return _lo_call;    }
  WNVEC& Actvec(void)         { return _actvec;     }
  void   Add_actual(WN *wn)   { _actvec.push_back(wn); }
  void   Add_arg(WNPAIR wp)   { _argvec.push_back(wp); }
  BOOL   operator == (WN *wn) { return WN_Get_Linenum(wn) == WN_Get_Linenum(Orig()); }
  ARGVEC& Argvec(void)        { return _argvec;     }
  WNPAIR  Actual(INT idx)     { return _argvec[idx];}

  UINT32 Argdiff(void) const  {
    INT parmcnt = WN_kid_count(_lo_call);
    INT actlcnt = _argvec.size();
    Is_True(actlcnt >= parmcnt,
            ("RNA::Argdiff, number of actual arguments is less than formal parameter"));
    return actlcnt - parmcnt;
  }

  void   Print(FILE *fp)      {
    fprintf(fp, ("Original call\n"));
    fdump_tree(fp, Orig());
    if (Orig_retv()) fdump_tree(fp, Orig_retv());
    if (Lowered() != NULL) {
      fprintf(fp, ("After Lowering\n"));
      fdump_tree(fp, Lowered());
      if (Lo_retv()) fdump_tree(fp, Lo_retv());
    }
    fprintf(fp, ("Actual arg list\n"));
    INT i;
    WNVEC::iterator cur;
    for (i = 0, cur = Actvec().begin(); cur != Actvec().end(); i++, cur++)
      fdump_tree(fp, *cur);
    fprintf(fp, "Actual stid count: %d\n", i);
    ARGVEC::iterator arg;
    for (i = 0, arg = Argvec().begin(); arg != Argvec().end(); i++, arg++) {
      fprintf(fp, "arg%d:", i);
      fprintf(fp, "preg%d", WN_offset(arg->first));
      if (arg->second == NULL)
        fprintf(fp, "\n");
      else
        fprintf(fp, ", preg%d\n", WN_offset(arg->second));
    }
  }

  WN    *Get_real_actual(INT idx) {
    Is_True(idx < Actvec().size(), ("Get_real_actual idx out of range"));
    return _actvec[idx];
  }
};

typedef std::vector<RNA> RNAVEC; 
typedef RNAVEC::iterator RNAITER;

// =============================================================================
// =============================================================================
//
// W2LBUILDER: wrap the llvm::IRBuilder
//             container for the list of llvm::BasicBlock
//             Must maintain the BBLIST to generate llvm IR
//
// =============================================================================
// =============================================================================
class W2LBUILDER {
private:
  LVBUILDER *_builder;      // wrap llvm::IRBuilder
  W2LBBLIST  _bblist;       // llvm::BasicBlock container
  LAB2W2LBB  _lab2bb;       // map from labname to LVBB*
  NAM2ALC    _locvars;      // map local variable name to its ptr
  PREGMAP    _preg4parm;    // preg reversed back to parm
  RNAVEC     _rnavec;       // call list in current PU
  INTVEC     _parm_order;   // record the order of parameters
  LVALC     *_last_alloc;   // last alloca instruction
  LVVAL     *_ret_val;

  W2LBUILDER(const W2LBUILDER &);             // REQUIRED UNDEFINED UNWANTED methods
  W2LBUILDER& operator = (const W2LBUILDER&); // REQUIRED UNDEFINED UNWANTED methods

public:
  W2LBUILDER(LVBUILDER *builder):_builder(builder) {
    _ret_val = nullptr;
    _last_alloc = nullptr;
  }
  ~W2LBUILDER() { }

  LVBUILDER  *Builder(void)                 { return _builder; }
  void        Builder(LVBUILDER *lb)        { _builder = lb;   }

  void        Expr2rnalst(WN *wn);          // capture calls in expr for rna
  void        Stmt2rnalst(WN *wn);          // traverse stmts to calls
  void        Print_rnalst(FILE *fp);       // print all elements
  RNAITER     Find_rna(WN *actual);         // the related rna for actual arg
  BOOL        Rnaiter_end(RNAITER rnaiter)  { return rnaiter == _rnavec.end(); }

  void        Append_rnalst(RNA rna)        { _rnavec.push_back(rna); }
  void        Build_w2bblist(WN *entry) ;   // by traversing PU by entry
  void        Put_labnam(const char *ln, W2LBB *p){
    FmtAssert(ln, ("ACCESS VIOLATION"));
    FmtAssert(_lab2bb.find(ln) == _lab2bb.end(), ("Put_labnam: %s already exists", ln));    
    _lab2bb[ln] = p;
  }
  void        Add_preg4parm(INT r1, INT r2) { 
    FmtAssert(! Is_preg4parm(r1), ("Add_preg4parm: Preg %d already exist.", r1));
    _preg4parm.push_back(std::make_pair(r1, r2));
    _parm_order.push_back(r2);
  }

  BOOL        Is_preg4parm(INT preg) {
    PREGMAP::iterator it;
    for (it = _preg4parm.begin(); it != _preg4parm.end(); ++it) {
      if (it->first == preg)
        return TRUE;
    }
    return FALSE;
  }

  INT         Get_preg4parm_regno(INT preg) {
    PREGMAP::iterator it;
    INT i = 0;
    for (it = _preg4parm.begin(); it != _preg4parm.end(); ++it) {
      if (it->first == preg) {
        return it->second;
      }
    }
    return 0;
  }

  INT         Get_preg4parm_idx(INT preg)   {
    PREGMAP::iterator it;
    INT i = 0;
    for (it = _preg4parm.begin(); it != _preg4parm.end(); ++it) {
      if (it->first == preg) {
        INT preg_no = it->second;

        // find the index of the parameter
        for (std::size_t j = 0; j < _parm_order.size(); j++) {
          if (_parm_order[j] == preg_no) {
            i = j;
            break;
          }
        }
        break;
      }
    }
    FmtAssert(it != _preg4parm.end(), ("Get_preg4parm_idx: Preg %d is not int _preg4parm", preg));
    return i;
  }

  bool        Has_locvar(const char *name) const {
    return _locvars.find(name) != _locvars.end();
  }
  void        Put_locvar(const char *nm, LVALC *p, LVTY *ty){
    FmtAssert(nm, ("ACCESS VIOLATION"));
    FmtAssert(_locvars.find(nm) == _locvars.end(), ("Put_locvar: %s already exists", nm));
    _locvars[nm] = std::make_pair(ty, p);
  }
  W2LBB      *Get_labnam(const char *ln)    {
    FmtAssert(ln, ("ACCESS VIOLATION"));
    if (_lab2bb.find(ln) != _lab2bb.end())
      return _lab2bb[ln];
    else
      return nullptr;
  }
  TYALC       Get_locvar(const char *nm)    {
    FmtAssert(nm, ("ACCESS VIOLATION"));
    if (_locvars.find(nm) != _locvars.end())
      return _locvars[nm]; 
    else 
      return std::make_pair(nullptr, nullptr);
  }
  void        Mutate_locvar(const char *nm, LVALC *p, LVTY *ty) {
    FmtAssert(nm, ("ACCESS VIOLATION"));
    FmtAssert(_locvars.find(nm) != _locvars.end(), ("Mutate_locvar: %s does not exist", nm));
    _locvars[nm] = std::make_pair(ty, p);
  }
  LVVAL      *RetVal()                     { return _ret_val;          }
  void        RetVal(LVVAL *ret_val)       { _ret_val = ret_val;       }
  LVALC      *LastAlloc()                  { return _last_alloc;       }
  void        LastAlloc(LVALC *last_alloc) { _last_alloc = last_alloc; }
}; // W2LBUILDER

#define FIELD_SEPARATOR '%'
enum BUFSZ { SZ = 1024 };
enum ACTION { ACT_LD, ACT_STR, ACT_LDA };
enum WHIRLEVEL {VHL, HL, ML, LL};
enum TESTINGM {
  TESTDEFAULT = 0x0,
  TESTREDEF   = 0x1,
  TESTACTUAL  = 0x2,
  TESTFORMAL  = 0x4,
  TESTJMPTBL  = 0x8,
  TESTLOWERVARARG = 0x10,
  TESTINTRC   = 0x20
};
enum STKMARK { ONSTACK = INT_MAX };
enum EXT_FLG {
  NONE = 0,
  SEXT = 1,
  ZEXT = 2,
};
typedef std::vector<EXT_FLG> SIGNVEC;
typedef enum _callee_kind {
  CK_NORMAL  = 0,
  CK_BUILTIN = 1,
  CK_VARARG  = 2,  // vaargs, TODO: need refine
} CALLEE_KIND;


// =============================================================================
// =============================================================================
//
// WHIRL2llvm managing the context for whirl2llvm
//
// =============================================================================
// =============================================================================
class WHIRL2llvm {
  friend ST2llvm;
  friend TY2llvm;
  friend ST_ATTR2llvm;
  friend TY2PTR;
  friend COLLECT_LVTY;
  friend COLLECT_VALUE;
  friend COLLECT_RNA;
  friend GEN_JMPTBL;
  friend CMP_EQNE;
private:
  LVMODULE    *_module;
  LVCONTEXT    _context;
  LVDIBUILDER *_di_builder;
  LVDICU      *_di_cu;
  WHIRLEVEL    _whirl_level;
  W2LBUILDER  *_builder;
  W2LBUILDER  *_rna_builder;
                           // function type building blocks, move to a class?
  WN          *_cur_func;
  AGMVEC       _parm_name;  // vector for formal parm names
  LVFUNC      *_svd_lvfunc; // saved llvm function definition
  LVFUNC      *_cur_lvfunc; // current llvm function definition
  LVBB        *_lventry;    // function entry
  INT          _last_label;
  NAM2VAL      _glbvars;    // map global variable name to its ptr
  WTY2LVT      _wty2lvty;   // map WHIRL TY_IDX to llvm::Type
  BOOL         _has_fwd_tyref;
  BOOL         _ignore_fwd_tyref;  // Hack to ignore not found assert in For_all(Ty_table..) loop
  INT          _cur_pu_num; // the numbering of PU for tracing
  INT          _skip_b;     // skip_before
  INT          _skip_a;     // skip_after
  INT          _testing_m;  // enable feature that is developing
  ST2LVVAL     _constants;  // map constant's ST to its value
  ST2LVVAL     _variables;  // map variables's ST to its ptr
  W2LFILEVEC   _w2l_files;  // vector of debug info files
  STRVEC       _inc_dirs;   // vector of INCLUDE_DIRECTORIES
  LVGLBVAR    *_glb_ctors;  // llvm.global_ctors builtin array
  LVCONSTVEC   _glb_init_registry;  // the vector of global ctors' registry

  // constant strings
  const std::string PARM_PREG = "_w2ll_parm_preg_";          // prefix for formal parameter
  const std::string GLOBAL_CTORS_NAME = "llvm.global_ctors"; // llvm global ctors array
  const std::string CTOR_SECTION = ".ctor";                  // .ctor section name
  const std::string GLB_I_PREFIX = "_GLOBAL__I_";            // prefix of global init function
  const std::string GLB_SUB_I_PREFIX = "_GLOBAL__sub_I_";    // prefix of global sub init function

private:
  WHIRL2llvm(void);                          // REQUIRED UNDEFINED UNWANTED methods
  WHIRL2llvm(const WHIRL2llvm &);            // REQUIRED UNDEFINED UNWANTED methods
  WHIRL2llvm& operator = (const WHIRL2llvm&);// REQUIRED UNDEFINED UNWANTED methods

  void        Whirl_level(WHIRLEVEL l) { _whirl_level = l;   }
  void        Set_whirl_level(const char *in);
  AGMVEC     &Parm_name(void)       { return _parm_name;     }
  char       *Parm_name(INT idx)    { Is_True(idx < _parm_name.size(),
                                              ("Parm_name access out of bound"));
                                      return _parm_name[idx].Name(); }
  BOOL        Mload_ty(TY_IDX ty)   { return MTYPE_is_m(TY_mtype(Ty_Table[ty])); }

public:
  LVCONTEXT  &Context(void)         { return _context;  }
  void        Builder(W2LBUILDER *b){ _builder = b;     } // alive 1 FUNC_ENTRY
  void        RnaBld(W2LBUILDER *b) { _rna_builder = b; } // alive 1 FUNC_ENTRY
  RNAITER     Find_rna(WN *a)       { return _rna_builder->Find_rna(a); }
  BOOL        Rnaiter_end(RNAITER r){ return _rna_builder->Rnaiter_end(r); }
  void        Cur_func(WN* wn)      {
    FmtAssert((wn != NULL) && (WN_opcode(wn) == OPC_FUNC_ENTRY),
              ("FUNC_ENTRY2llvm: node is not FUNC_ENTRY but is %s", OPCODE_name(WN_opcode(wn))));
    _cur_func = wn;
  }

  WN         *Cur_func(void)        { return _cur_func;     }
  void        Svd_lvfunc(LVFUNC *f) { _svd_lvfunc = f;      }
  LVFUNC     *Svd_lvfunc(void)      { return _svd_lvfunc;   }
  void        Cur_lvfunc(LVFUNC *f) { _cur_lvfunc = f;      }
  LVFUNC     *Cur_lvfunc(void)      { return _cur_lvfunc;   }
  void        Lventry(LVBB *bb)     { _lventry = bb;        }
  LVBB       *Lventry(void)         { return _lventry;      }
  INT         Get_lastlab(void)     { return _last_label++; }
  void        Last_label(INT n)     { _last_label = n;      }
  BOOL        Has_fwd_tyref(void)   { return _has_fwd_tyref;}
  void        Has_fwd_tyref(BOOL V) { _has_fwd_tyref = V;   }
  void        Has_fwd_tyref(BOOL V, UINT idx) { _has_fwd_tyref = V;   
    Is_Trace(Tracing_enabled, (TFile, "set ForwardTypeRef [%d] %d\n", V, idx));
  }
  BOOL        Ign_fwd_tyref(void)   { return _ignore_fwd_tyref; }
  void        Ign_fwd_tyref(BOOL b) { _ignore_fwd_tyref = b; }
  INT         Cur_pu_num(void)      { return _cur_pu_num;   }
  void        Inc_cur_pu_num(void)  { ++_cur_pu_num;        }
  BOOL        Skip_cur_pu(void)     { return !((_cur_pu_num >= _skip_b) && (_cur_pu_num <= _skip_a)); }
  INT         Testing_mode(void)    { return _testing_m;    }
  WTY2LVT    &Get_wty2lvty()        { return _wty2lvty;     }

  void        Add_preg4parm(INT r1, INT r2){ _builder->Add_preg4parm(r1, r2); }
  BOOL        Is_preg4parm(INT reg) { return _builder->Is_preg4parm(reg); }
  BOOL        Is_preg4parm(WN *data);
  BOOL        Is_rhs_inparm_ld(WN *stmt);
  BOOL        Is_int_parm_reg(WN *ldstr);
  BOOL        Is_float_parm_reg(WN *ldstr);
  INT32       Collect_call_actuals(WN *str, RNAITER& actual);
  BOOL        Is_lowered_actual(WN *stid);
  LVVAL      *Save_to_inparm(WN *wn, const char *varname, LVVAL *rhs, INT idx);

  LVTY       *Insert_ty_entry(UINT idx, TY *ty);
  LVTY       *Insert_ty_entry(UINT idx, TY_IDX ty_idx) {
    TY *ty = &(Ty_Table[ty_idx]);
    return Insert_ty_entry(idx, ty);
  }
  WHIRLEVEL   Whirl_level(void)     { return _whirl_level;   }

  void        ST2const(ST *st, LVVAL *val) {
    FmtAssert(_constants.find(st) == _constants.end(),
              ("ST2const: ST %s already exists", ST_name(st)));
    _constants.insert(std::make_pair(st, val));
  }

  LVVAL      *ST2const(ST *st) {
    ST2LVVAL::iterator it = _constants.find(st);
    if (it == _constants.end())
      return nullptr;
    return it->second;
  }

  void      ST2lvval(ST *st, LVVAL *val) {
    FmtAssert(_variables.find(st) == _variables.end(),
              ("ST2lvval: ST %s already exists", ST_name(st)));
    _variables.insert(std::make_pair(st, val));
  }

  LVVAL    *ST2lvval(ST *st) {
    ST2LVVAL::iterator it = _variables.find(st);
    if (it == _variables.end())
      return nullptr;
    return it->second;
  }

  LVFUNC *ST2func(ST *st) {
    return Module()->getFunction(ST_name(st));
  }

  LVVAL   *ST2const_or_val(ST *st) {
    switch(ST_class(st)) {
    case CLASS_CONST:
      return ST2const(st);
    case CLASS_VAR:
      return ST2lvval(st);
    case CLASS_FUNC:
      return ST2func(st);
    default:
      return nullptr;
    }
  }

private:
  char       *Syn_name_w_modifier(char *name, INT32 modifier) {
    char nmbuf[SZ];
    sprintf(nmbuf, "%s%c%d", name, FIELD_SEPARATOR, modifier);
    char *name_w_modifier = strdup(nmbuf);
    return name_w_modifier;
  }

  BOOL        Is_name_w_modifier(char *str1, char *str2, INT32 cmp_len) {
    if (str1[cmp_len] != FIELD_SEPARATOR &&
        str2[cmp_len] != FIELD_SEPARATOR)
      return FALSE;
    return strncmp(str1, str2, cmp_len) == 0;
  }

  char       *Adjust_parm_name(char *name, INT32 modifier) {
    char *altname = Syn_name_w_modifier(name, modifier);
    for (AGMVEC::iterator it = Parm_name().begin();
         it != Parm_name().end(); ++it) {
      if (strcmp((*it).Name(), altname) == 0) {
        free(altname);
        return (*it).Name();
      }
    }
    free(altname);
    return name;
  }

  char       *Find_parm_name(const char *name, INT32 reg) {
    if (Parm_name().empty())
      return (char*)name;
    for (AGMVEC::iterator it = Parm_name().begin();
         it != Parm_name().end(); ++it) {
      if (strncmp((*it).Name(), name, strlen(name)) == 0 &&
          (*it).Regno() == reg) {
        return (*it).Name();
      }
    }
    Is_True(FALSE, ("Find_parm_name cannot find %s with PREG%d", name, reg));
  }

  TYPE_ID     Find_parm_type(const char *name, BOOL *on_stack) {
    if (Parm_name().empty())
      return MTYPE_UNKNOWN;
    for (AGMVEC::iterator it = Parm_name().begin();
         it != Parm_name().end(); ++it) {
      if (strcmp((*it).Name(), name) == 0) {
        *on_stack = ((*it).Regno() == ONSTACK)? TRUE : FALSE;
        return (*it).Type();
      }
    }
    FmtAssert(FALSE, ("Find_parm_type cannot find %s", name));
    return MTYPE_UNKNOWN;
  }

public:
  LVPTRTY    *GetLVPtrTy() {
    return LVPTRTY::get(Context(), 0);
  }

  INT         Get_preg4parm_idx(INT reg) {
    INT parm_reg = _builder->Get_preg4parm_regno(reg);
    if (parm_reg != 0 && !Parm_name().empty()) {
      INT i = 0;
      for (AGMVEC::iterator it = Parm_name().begin();
           it != Parm_name().end(); ++it, ++i) {
        if (it->Regno() == parm_reg)
          return i;
      }
    }

    return _builder->Get_preg4parm_idx(reg);
  }

  bool        DoesMapHasThisType(TY_IDX idx) {
    auto res = _wty2lvty.find(TY_IDX_index(idx));
    if (res != _wty2lvty.end()) {
      if (res->second != nullptr) return true;
    }
    return false;
  }

  bool        IsPreg(WN *wn) {
    if (!WN_has_sym(wn)) return FALSE;
    ST *st = WN_st(wn);
    if (st->sym_class == CLASS_PREG) return true;
    else return false;
  }

  std::string GetRegName(WN *wn) {
    ST *st = WN_st(wn);
    std::string reg(ST_name(st));
    reg[0] = '_';
    reg.append("_" + std::to_string(WN_offset(wn)));
    return reg;
  }

  bool        IsCallResReg(WN *wn) {
    if (!IsPreg(wn)) return false;
    INT regnum = WN_offset(wn);
    return (regnum == First_Int_Preg_Return_Offset ||
            regnum == Last_Int_Preg_Return_Offset  ||
            regnum == First_Float_Preg_Return_Offset);
  }

  void        Flush_cur_func(void)  {
    // called when cur func is done;  clean out assocated data
    _cur_func = NULL;
    _parm_name.clear();
    _svd_lvfunc = NULL;
    _cur_lvfunc = NULL;
    _lventry = NULL;
    Builder(NULL);
    Last_label(0);   // 0 is invalid label number
  }

  TY_IDX      Get_ptrtype(TY_IDX type);

  LVTY       *Wty2llvmty(TYPE_ID mtype, TY_IDX idx, UINT size = 0) {
#if 0
    Is_Trace(Tracing_enabled,
             (TFile, "In Wty2llvmty idx / TY_idx 0x%x / %d (%s)\n", idx, TY_IDX_index(idx), TY_name(idx)));
#endif
    INT tysize = (size == 0)? MTYPE_bit_size(mtype) : size;
    if (MTYPE_is_integral(mtype)) {
      LVTY *res = nullptr;
      switch (TY_kind(idx)) {
      case KIND_POINTER: {

        Is_Trace(Tracing_enabled,
                  (TFile, "Wty2llvmty: handling Pointer, idx [%d]: %s size %lld Mtype %s\n",
                    TY_IDX_index(idx), TY_name(idx), TY_size(idx), Mtype_Name(TY_mtype(idx))));

        auto pointee_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        FmtAssert(pointee_ty != nullptr,
                  ("Wty2llvmty: pointee_type for %s does not exist", TY_name(idx)));

        res = GetLVPtrTy();
        break;
      }
      case KIND_STRUCT: {
        auto struct_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        FmtAssert(struct_ty != nullptr, ("Wty2llvmty: struct_ty %s is not exist", TY_name(idx)));
        res = struct_ty;
        break;
      }
      default: {
        res = llvm::IntegerType::get(_context, tysize);
        break;
      }
      }

      return res;
    } else if (MTYPE_is_float(mtype)) {
      if (TY_kind(idx) == KIND_POINTER) {
        Is_Trace(Tracing_enabled,
                  (TFile, "Wty2llvmty: handling Pointer, idx [%d]: %s size %lld Mtype %s\n",
                    TY_IDX_index(idx), TY_name(idx), TY_size(idx), Mtype_Name(TY_mtype(idx))));

        auto pointee_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        FmtAssert(pointee_ty != nullptr,
                  ("Wty2llvmty: pointee_type for %s does not exist", TY_name(idx)));

        return GetLVPtrTy();
      }

      switch (tysize) {
      case 16: return llvm::Type::getHalfTy(_context);
      case 32: return llvm::Type::getFloatTy(_context);
      case 64: return llvm::Type::getDoubleTy(_context);
      case 80: return llvm::Type::getX86_FP80Ty(_context);
      case 128: return llvm::Type::getFP128Ty(_context);
      default: break;
      }
    } else if (MTYPE_is_void(mtype)) {
      switch (TY_kind(idx)) {
      case KIND_ARRAY: {
        auto array_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        // if we are NOT in the FWD_ref fixing loop, we should just continue
        if (Ign_fwd_tyref() == FALSE) {
          FmtAssert(array_ty != nullptr,
                    ("Wty2llvmty: array_ty %s (%d) does not exist", TY_name(idx), TY_IDX_index(idx)));
        }
        return array_ty;
      }
      default: {
        return llvm::Type::getVoidTy(_context);
      }
      }
    } else if (MTYPE_is_m(mtype)) {
      FmtAssert(idx != 0, ("Wty2llvmty: TY_IDX should not be 0"));
      if (TY_kind(idx) == KIND_POINTER) {
        idx = TY_pointed(idx);
      }

      switch (TY_kind(idx)) {
      case KIND_STRUCT: {
        auto struct_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        FmtAssert(struct_ty != nullptr, ("Wty2llvmty: struct_ty %s does not exist", TY_name(idx)));
        return struct_ty;
      }
      case KIND_ARRAY: {
        // FIXME: we should remove this hardcode
        // clang converts the va_list type to i8**
        if (TY_name(idx) == std::string("va_list")) {
          return LVTY::getInt8PtrTy(_context);
        }

        auto array_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
        // if we are NOT in the FWD_ref fixing loop, we should just continue
        if (Ign_fwd_tyref() == FALSE) {
          FmtAssert(array_ty != nullptr,
                    ("Wty2llvmty: array_ty %s (%d) does not exist", TY_name(idx), TY_IDX_index(idx)));
        }
        return array_ty;
      }
      default: {
        FmtAssert(FALSE, ("Wty2llvmty: type %s[%d] kind %d isn't able to be handled ", TY_name(idx), idx, TY_kind(idx)));
      }
      }
    } else if (MTYPE_is_pointer(mtype)) {
      FmtAssert(idx != 0, ("Wty2llvmty: TY_IDX should not be 0"));
      auto pointee_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
      FmtAssert(pointee_ty != nullptr, ("Wty2llvmty: pointee_type %s is not exist", TY_name(idx)));
      if (pointee_ty->isFunctionTy() && (TY_kind(idx) != KIND_POINTER)) {
        return pointee_ty;
      }
      return GetLVPtrTy();
    } else if (MTYPE_is_str(mtype)) {
      FmtAssert(TY_kind(idx) == KIND_ARRAY, ("Wty2llvmty: constant string should be array"));
      auto array_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
      return array_ty;
    }
    else {
      // assert, not handled yet
      if (Ign_fwd_tyref() == FALSE) {
        switch (TY_kind(idx)) {
        case KIND_FUNCTION: {
          // function kind does not have type in WHIRL
          auto pointee_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
          if (pointee_ty != nullptr)
            return pointee_ty;
        }
        case KIND_ARRAY: {
          auto array_ty = Get_wty2lvty(TY_IDX_index(idx), idx);
          // if we are NOT in the FWD_ref fixing loop, we should just continue
          if (Ign_fwd_tyref() == FALSE) {
            FmtAssert(array_ty != nullptr,
                    ("Wty2llvmty: array_ty %s (%d) does not exist", TY_name(idx), TY_IDX_index(idx)));
          }
          return array_ty;
        }
        }
        FmtAssert(FALSE, ("Wty2llvmty: can't handle %s type now", Mtype_Name(mtype)));
      }
      return NULL;
    }
    FmtAssert(FALSE, ("Wty2llvmty: create %s type failed", Mtype_Name(mtype)));
    return NULL;
  }

  WN         *Ref_func_retreg(WN *wn);
  void        Set_func_attr(TY_IDX putyidx, LVFUNC *func, SIGNVEC *sign_list);
  void        Set_intrc_as_call(void);
  void        Reset_intrc_as_call(void);
  BOOL        Intrc_as_call(void);
  LVVAL      *Handle_intrn_call(WN *wn);

  BOOL        Contain_lowered_vararg(TY_IDX putyidx);
  void        Collect_vararg_input_parm(PLOC ploc, LVTYVEC& argstype, SIGNVEC *info_list);
  LVTY       *Create_ret_type(TY_IDX putyidx, LVTYVEC& argstype, SIGNVEC *info_list, PLOC *p);
  WNPAIR      Load_mload_actual(WN*, PLOC&);
  LVTY       *Collect_formal_parm_type(LVTYVEC& argstype, WN *func_wn, SIGNVEC *info_list);
  EXT_FLG     Get_ext_flag(LVTY *lvty, TYPE_ID tyid);
  TYPE_ID     Adjust_return_ldid_mtype(TYPE_ID mtype, PREG_NUM& pregno);
  void        Create_mload_formal(LVTYVEC& argstype, TY_IDX idx, PLOC& ploc, char *name,
                                  SIGNVEC *info_list = nullptr);
  LVTY       *Create_formal_parm_type(TY_IDX putyidx, LVTYVEC& argstype, TYIDXVEC& tyidxvec,
                                  SIGNVEC *info_list = nullptr);
  LVALC      *Collect_retval_pregname(WN *wn);
  TY_IDX      GetFuncType(WN *wn);
  LVFUNCTY   *Get_function_ty(WN *wn, SIGNVEC *sign_info = NULL);

  // The struct consists of cxx empty struct members and other padding area, 
  // whose contents are not defined.
  BOOL Is_struct_content_padding(TY_IDX ty, INT32 offset, INT32 len) {
    FLD_IDX fld_idx = Ty_Table[ty].Fld();
    if (fld_idx == 0) return TRUE;
    do {
      FLD_HANDLE fld(fld_idx);
      if (TY_kind(FLD_type(fld)) != KIND_STRUCT) {
        INT32 myoffset, myend;
        myoffset = FLD_ofst(fld);
        myend= myoffset + TY_size(FLD_type(fld));
        if (myoffset <= offset && myend > offset ||
            myoffset < offset + len && myend >= offset + len ||
            offset <= myoffset && offset + len > myoffset)
          return FALSE;
      } else if (!Is_struct_content_padding(FLD_type(fld), offset - FLD_ofst(fld), len ))
        return FALSE;
      if (FLD_last_field(fld))
        break;
      fld_idx++;
    } while (1);
    return TRUE;
  } 

  void        Set_parm_name(LVFUNC *lvfunc) {
    if (lvfunc == NULL)
      return;

    BOOL use_parm_name = !Parm_name().empty();
    LVFUNC::arg_iterator curargs = lvfunc->arg_begin();
    LVFUNC::arg_iterator svdargs = (Svd_lvfunc())? Svd_lvfunc()->arg_begin() : NULL;
    INT argcount = 0;            // for IDNAME list
    INT parm_name_idx = 0;       // for Parm_name()
    WN *idname_wn = WN_kid(Cur_func(), argcount);
    while (WN_opcode(idname_wn) == OPC_IDNAME) {
      char  *idname = ST_name(WN_st(idname_wn));
      char  *pname = (use_parm_name)?Parm_name(parm_name_idx):idname;
      Is_True(curargs != lvfunc->arg_end(), ("Set_parm_name access lvfunc's arg_end"));
      LVFUNC::arg_iterator x = curargs++;
      x->setName(pname);
      Is_Trace(Tracing_enabled,
               (TFile, "Set_parm_name(%d): %s with lvtypeID:%d\n",
                parm_name_idx, pname, x->getType()->getTypeID()));
      parm_name_idx++;

      TY_IDX formal_ty = WN_type(idname_wn);
      if (use_parm_name && Mload_ty(formal_ty)) { 
        // may need to set additional parm name pattern matches
        // how it is saved in create_mload_formal()
        while (parm_name_idx < Parm_name().size() &&
               Is_name_w_modifier(idname, Parm_name(parm_name_idx), strlen(idname))) {
          Is_True(curargs != lvfunc->arg_end(), ("Set_parm_name access lvfunc's arg_end"));
          curargs->setName(Parm_name(parm_name_idx));
          Is_Trace(Tracing_enabled,
                   (TFile, "Set_parm_name(%d): %s with type:%d\n",
                    parm_name_idx, Parm_name(parm_name_idx), curargs->getType()->getTypeID()));
          curargs++;
          parm_name_idx++;
        }
      }

      argcount++;
      idname_wn = WN_kid(Cur_func(), argcount);
    } // loop on idname list

    if (Contain_lowered_vararg(ST_pu_type(WN_st(Cur_func())))) {
      for (; parm_name_idx < Parm_name().size(); parm_name_idx++, curargs++) {
        Is_True(curargs != lvfunc->arg_end(),
                ("Set_parm_name access lvfunc's arg_end"));
        curargs->setName(Parm_name(parm_name_idx));
      }
    }
  }

  BOOL        Contain_mload_formal(WN *puwn) {
    Is_True(WN_operator(puwn) == OPR_FUNC_ENTRY,
            ("Contain_mload_formal takes WN of OPR_FUNC_ENTRY only"));

    INT argcount = 0;
    WN *idname_wn = WN_kid(puwn, argcount);
    while (WN_opcode(idname_wn) == OPC_IDNAME) {

      if (Mload_ty(WN_type(idname_wn))) { 
        return TRUE;
      }

      argcount++;
      idname_wn = WN_kid(puwn, argcount);
    }

    return FALSE;
  }

  BOOL        Need_redefine_functy(WN *wn) {
    if (Testing_mode() & TESTREDEF) {
      if (OPERATOR_is_call(WN_operator(wn))) {
        // cannot redefine by func_entry logic
        return FALSE;
      }
      // Must deal with all WHIRL level to enable mix and match during debug
      if (Contain_mload_formal(Cur_func()))
        return TRUE;
      // Must deal with vararg
      if (Contain_lowered_vararg(ST_pu_type((WN_st(wn)))))
        return TRUE;
      // return FALSE; must test against Whirl_level() == ML Shin 09032022
    }

    if (Whirl_level() == ML)
      return TRUE;
    else {
      Is_Trace(Tracing_enabled, (TFile, "Need not redefine functy for VHL/HL WHIRL\n"));
      return FALSE;
    }
  }

  LVFUNC     *GetFunction(WN *wn) {
    ST *func_st = WN_st(wn);
    std::string funcname(ST_name(func_st));
    auto func = Module()->getFunction(funcname);
    if (func == NULL || Need_redefine_functy(wn)) {
      // the function does not exist in the module space yet, or
      // the ML form has transformed the M type Parameters
      Is_Trace(Tracing_enabled,
               (TFile, "Need to redefine func %s in GetFunction\n", ST_name(func_st)));
      Svd_lvfunc(func);  // save defined by type table
      SIGNVEC sign_info;
      LVFUNCTY *lvfuncty = Get_function_ty(wn, &sign_info);
      if (func != nullptr) {
        // remove the previous function from the module
        func->removeFromParent();
      }

      LVFUNC *new_func = Create_func(lvfuncty, wn, &sign_info);
      if (func != nullptr) func->replaceAllUsesWith(new_func);
      func = new_func;
    }
    FmtAssert(func != nullptr, ("GetFunction: can't find function %s by wn", ST_name(func_st)));
    return func;
  }

  llvm::GlobalValue::LinkageTypes GetFuncLinkageType(ST *st) {
    auto link = llvm::GlobalValue::ExternalLinkage;
    if (ST_export(st) == EXPORT_LOCAL) {
      link = llvm::GlobalValue::LinkageTypes::InternalLinkage;
    }

    // check if it is extern inline function
    if (PU_is_extern_inline(Pu_Table[ST_pu(st)])) {
      link = llvm::GlobalValue::LinkageTypes::LinkOnceODRLinkage;
    }
    return link;
  }

  LVFUNC     *Create_func(LVFUNCTY *lvfuncty, WN *wn, SIGNVEC *sign_info) {
    if (lvfuncty == NULL)
      return NULL;

    WN    *func_wn = (wn == NULL)? Cur_func() : wn;
    ST    *func_st = WN_st(func_wn);
    TY_IDX func_tyidx;
    BOOL   is_icall = wn != NULL && WN_operator(wn) == OPR_ICALL;
    if (is_icall) {
      func_wn = WN_kid(wn, WN_kid_count(wn)-1);
      func_tyidx = GetFuncType(func_wn);
    } else {
      func_wn = (wn == NULL)? Cur_func() : wn;
      func_tyidx = ST_type(WN_st(func_wn));
    }
    
    std::string funcname(ST_name(func_st));
 
    auto link = GetFuncLinkageType(func_st);

    llvm::Function *func = llvm::Function::Create(lvfuncty, link, funcname, _module);

    if (func != NULL) {
      if (PU_src_lang(ST_pu(func_st)) & PU_CXX_LANG ||
          PU_src_lang(ST_pu(func_st)) & PU_C_LANG) {
        func->setCallingConv(llvm::CallingConv::C);
      }
      else {
        // TODO: to add other language
      }
    }
    Set_func_attr(func_tyidx, func, sign_info);
    
    return func;
  }

  // used in function entry, set parameter name
  LVFUNC     *Gen_func(WN *wn) {
    auto func = GetFunction(wn);
    FmtAssert (WN_operator(wn) == OPR_FUNC_ENTRY, ("Gen_func: expect for FUNC_ENTRY"));
    Cur_lvfunc(func);  // stash away for later use
    Set_parm_name(func);
    return func;
  }

  LVVAL *Get_arg_by_name(const char *name) {
    for (auto it = _cur_lvfunc->arg_begin(); it != _cur_lvfunc->arg_end(); it++) {
      if (it->getName() == std::string(name)) return it;
    }
    FmtAssert(FALSE, ("Get_arg_by_name: get argument %s failed", name));
    return nullptr;
  }

  void        SetCallInstAttrs(LVCALL *call, LVFUNC *callee) {
    auto callee_attrs = callee->getAttributes();
    call->setAttributes(callee_attrs);
  }

  void        Put_locvar(const char *nm, LVALC *p, LVTY *ty) { _builder->Put_locvar(nm, p, ty); }
  TYALC       Get_locvar(const char *nm)           { return _builder->Get_locvar(nm); }
  void        Mutate_locvar(const char *nm, LVALC *p, LVTY *ty) { _builder->Mutate_locvar(nm, p, ty); }
  void        Put_glbvar(const char *nm, LVVAL *p){
    FmtAssert(nm, ("ACCESS VIOLATION"));
    if (_glbvars.find(nm) != _glbvars.end()) {
      FmtAssert(_glbvars.find(nm) == _glbvars.end(), ("Put_glbvar: %s already exists", nm));
    }
    FmtAssert(_glbvars.find(nm) == _glbvars.end(), ("Put_glbvar: %s already exists", nm));
    _glbvars[nm] = p;
  }
  LVVAL      *Get_glbvar(const char *nm) {
    FmtAssert(nm, ("ACCESS VIOLATION"));
    if (_glbvars.find(nm) != _glbvars.end())
      return _glbvars[nm]; 
    else
      return nullptr;
  }

  void        Put_wty2lvty(UINT32 tyidx, LVTY *lvty) {
    WTY2LVT::iterator tmp_idx = _wty2lvty.find(tyidx);
    WTY2LVT::iterator tmp_end = _wty2lvty.end();

    //    FmtAssert(tmp_idx == tmp_end, ("Put_wty2lvty: %d already exists", tyidx)); - SC
    // My take is Put_ semantic simply put tyidx into the map.
    // The right interface should be that it returns the entry index in the map,
    // if fail, return end-of-iterator
    Is_Trace(Tracing_enabled, (TFile, "Insert TY %s [%d] to lvty %p\n", TY_name(tyidx), tyidx, lvty));
    _wty2lvty.insert(std::pair<unsigned int, llvm::Type*>(tyidx, lvty));
  }
  
  LVTY       *Get_wty2lvty(UINT32 idx, TY_IDX tyidx) {
    //    printf("Get idx %d\n", idx);
    if (_wty2lvty.find(idx) != _wty2lvty.end())
      return _wty2lvty[idx];
    else {
      return Insert_ty_entry(idx, tyidx);
    }
  }
  
  void        Set_wty2lvty(UINT32 tyidx, LVTY *lvty) {
    Is_Trace(Tracing_enabled, (TFile, "Set TY %s [%d] to lvty %p\n", TY_name(tyidx), tyidx, lvty));
 
    FmtAssert(_wty2lvty.find(tyidx) != _wty2lvty.end(), ("Set_wty2lvty: %d doesn't exist", tyidx));
    _wty2lvty[tyidx] = lvty;

  }
  
  void        Put_labnam(const char *ln, W2LBB *p)       { _builder->Put_labnam(ln, p);     }
  W2LBB      *Get_labnam(const char *ln)                 { return _builder->Get_labnam(ln); }
  TYALC       Get_preg(WN *wn, BOOL is_load = FALSE, TYPE_ID mtype = 0, TY_IDX ty_idx = 0);

  LVGLBVAR   *Create_glbvar(TYPE_ID tyid, TY_IDX idx, const char *name, llvm::GlobalValue::LinkageTypes linkTy) {
    LVGLBVAR *gvar = Create_glbvar(Wty2llvmty(tyid, idx), name, linkTy);
    return gvar;
  }

  LVGLBVAR   *Create_glbvar(LVTY *lvty, const char *name, llvm::GlobalValue::LinkageTypes linkTy) {
    std::string Name = std::string(name);
    _module->getOrInsertGlobal(Name, lvty);
    LVGLBVAR *gvar = _module->getNamedGlobal(Name);
    gvar->setLinkage(linkTy);
    Put_glbvar(name, gvar);
    return gvar;
  }
  
  TYALC       Create_locvar(TYPE_ID tyid, TY_IDX idx, LVVAL *arr_size, const char *name, UINT64 index = 0) {
    Is_Trace(Tracing_enabled, (TFile, "Create_locvar for variable %s, index in ST_tab is %llu\n", name, index));
    LVTY *ty = Wty2llvmty(tyid, idx);

    return Create_locvar(ty, arr_size, name, index);
  }

  TYALC       Create_locvar(LVTY *lvty, LVVAL *arr_size, const char *name, UINT64 index = 0) {
    Is_Trace(Tracing_enabled, (TFile, "Create_locvar for variable %s, index in ST_tab is %llu\n", name, index));

    std::string varname(name);
    if (index != 0) varname += "_" + std::to_string(index);

    auto cur_insert_point = Lvbuilder()->GetInsertBlock();
    if (auto last_alloc = _builder->LastAlloc()) {
      Lvbuilder()->SetInsertPoint(last_alloc);
    } 
    else {   // aggregate locvar creation in the Entry BB
      LVINST *last_inst = Lventry()->empty() ? nullptr : &(Lventry()->back());
      if (last_inst && 
          (llvm::isa<llvm::BranchInst>(last_inst) ||
           llvm::isa<llvm::ReturnInst>(last_inst) ||
           llvm::isa<llvm::SwitchInst>(last_inst) ||
           llvm::isa<llvm::IndirectBrInst>(last_inst))) {
        Lvbuilder()->SetInsertPoint(last_inst);
      } else {
        Lvbuilder()->SetInsertPoint(Lventry());
      }
    }

    auto retv = Lvbuilder()->CreateAlloca(lvty, arr_size, varname.c_str());
    Put_locvar(varname.c_str(), retv, lvty);
    Lvbuilder()->SetInsertPoint(cur_insert_point);
    if (_builder->LastAlloc() == nullptr) _builder->LastAlloc(retv);
    return std::make_pair(lvty, retv);
  }

  TYALC       Create_locvar(TYPE_ID tyid, const char *name) {
    TY_IDX ty_idx = Be_Type_Tbl(tyid);
    return Create_locvar(tyid, ty_idx, NULL, name);
  }

  bool        Has_locvar(const char *name) const {
    return _builder->Has_locvar(name);
  }

  bool        Has_locvar(ST *st) const {
    std::string name = ST_name(st);
    name += "_" + std::to_string(ST_index(st));
    return _builder->Has_locvar(name.c_str());
  }

  LVCONST    *TCON2llvm(TCON_IDX tc, TY_IDX ty_idx);
  LVCONST    *INITV2llvm(const INITV &initv, TY_IDX ty_idx);
  LVCONST    *INITV2llvm4struct(INITV_IDX idx, TY_IDX ty_idx, llvm::StructType *struct_ty);
  LVCONST    *INITV2llvm4field(INITV_IDX idx, const FLD_HANDLE &fld, LVTY *field_ty);
  LVCONST    *INITV2llvm4pad(INITV_IDX idx, TY_IDX ty_idx);
  LVCONST    *INITV2llvm4pad(const INITV &initv, TY_IDX ty_idx);
  LVCONST    *INITV2llvm4zero(const INITV &initv, TY_IDX ty_idx);

  BOOL        Has_volatile_expr(WN *wn);
  LVVAL      *EXPR2llvm(WN *wn, WN *parent = NULL);
  WN         *STMT2llvm(WN *wn, W2LBB *lvbb);
  W2LBB      *BLOCK2llvm(WN *wn, W2LBB *lvbb, bool nullIfEmpty = false);
  LVVAL      *WN2llvmSymAct(WN *wn, ACTION act = ACT_LD, LVVAL *rhs = NULL);
  LVVAL      *RetVal() { return _builder->RetVal(); }
  void        RetVal(LVVAL *ret_val) { _builder->RetVal(ret_val); }
  char       *Fabricate_labnam(const char *prefix, INT32 labnum) {
    char buf[SZ];
    sprintf(buf, "%s%d", prefix, labnum);
    return strdup(buf); // caller to free the return value
  }

  char       *LABEL_NUMBER2name(INT32 labnum) {
    if (labnum == 0)  // pass in 0 for fallthough BB
      return Fabricate_labnam("FT", Get_lastlab());
    else
      return Fabricate_labnam("L", labnum);
  }

  void        Put_w2lbb(const char *labname, W2LBB *w2lbb) { 
    FmtAssert(labname != NULL, ("WHIRL2llvm::Put_w2lbb, labname shall be non-null"));
    char *tmpname = strdup(labname);
    W2LBB *retv = Get_labnam(labname);
    FmtAssert(retv == NULL, ("WHIRL2llvm::Put_w2lbb, labname entered before"));
    Put_labnam(labname, w2lbb);
    free(tmpname);
  }

  W2LBB      *Create_w2lbb(const char *labname) {
    FmtAssert(labname != NULL, ("WHIRL2llvm::Create_w2lbb must called with valid label name"));
    // Create a BasicBlock
    LVBB *lvbb = LVBB::Create(Context(), labname, Cur_lvfunc());
    if (lvbb == NULL)
      return NULL;
    
    // create enclosure for lvbb
    W2LBB *retv = new W2LBB(lvbb, labname);
    
    // create lvblkaddr for lvbb
    retv->Blkaddr(LVBLKADDR::get(Cur_lvfunc(), lvbb)); 
    
    // establish labname to w2lbb mapping
    Put_w2lbb(labname, retv);
    return retv;
  }

  W2LBB      *Get_w2lbb(const char *labname) { 
    W2LBB *retv = Get_labnam(labname);
    if (retv) { // already exist
      return retv;
    }
    return Create_w2lbb(labname);
  }

  BOOL         IsBuiltin(const LVFUNC *func) {
    return func->getName().startswith("__builtin_");
  }

  CALLEE_KIND  Callee_kind(WN *wn, const LVFUNC *func) {
    if (IsBuiltin(func)) return CK_BUILTIN;
    if (TY_is_varargs(ST_pu_type((WN_st(wn)))))
        return CK_VARARG;
    return CK_NORMAL;
  }

  TY_IDX      Get_load_addr_ty(WN *wn) {
    TY_IDX load_addr_ty = 0;
    switch (WN_operator(wn)) {
      case OPR_LDA:
      case OPR_LDID:
      case OPR_STID: {
        load_addr_ty = ST_type(WN_st(wn));
        break;
      }
      case OPR_ISTORE: {
        if (WN_operator(WN_kid1(wn)) == OPR_LDA) // ugly hack
          load_addr_ty = WN_ty(WN_kid1(wn)); 
        else
          load_addr_ty = WN_ty(wn);
        load_addr_ty = (Ty_Table[load_addr_ty]).Pointed();  // from KIND_POINTER
        break;
      }
      case OPR_ILOAD: {
        load_addr_ty = WN_load_addr_ty(wn);
        break;
      }
      default: ;
    }
    return load_addr_ty;
  }

  // recognize memroy access of records array
  // Eg.: struct A {int num}; struct A arr[1];
  //      arr[0].num = 1;
  // whirl ignores the first dereference -- arr[0]
  bool IsArrayOfStruct(WN *wn) {
    Is_Trace(Tracing_enabled, (TFile, "IsArrayOfStruct: "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

    TY_IDX load_addr_ty = Get_load_addr_ty(wn);
    const TY &ty = Ty_Table[load_addr_ty];

    if (TY_kind(ty) != KIND_ARRAY) return false;

    // get element type
    TY_IDX e_ty_idx = ty.Etype(); 
    
    return (TY_kind(e_ty_idx) == KIND_STRUCT);
  }

  template <TY_KIND KIND>
  bool IsSpecificTypeAsField(WN *wn) {
    Is_Trace(Tracing_enabled, (TFile, "IsSpecificTypeAsField: "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

    switch (WN_operator(wn)) {
      case OPR_LDA:
      case OPR_LDID:
      case OPR_STID:
      case OPR_ILOAD:
      case OPR_ISTORE:
        break;
      default:
        return false;
    }

    TY_IDX load_addr_ty = Get_load_addr_ty(wn);
    const TY &ty = Ty_Table[load_addr_ty];
    if (TY_kind(ty) != KIND_STRUCT) return false;

    UINT64 offset = WN_offset(wn);
    FLD_HANDLE fld = TY_fld(ty);
    while (fld.Is_Null() == false) {
      bool same_field = IsSameField(fld, offset);
      UINT64 ofst = FLD_ofst(fld);
      if (same_field) {
        return (TY_kind(FLD_type(fld)) == KIND);
      }

      if (offset < ofst) break;
      fld = FLD_next(fld);
    }
    return false;
  }

  // recognize memroy access of field array
  // Eg.: struct A { int nums[3]; }; struct A aa;
  //      a.nums[0] = 1;
  // whirl ignores the first dereference -- nums[0]
  bool IsArrayAsField(WN *wn) {
    Is_Trace(Tracing_enabled, (TFile, "IsArrayAsField: "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

    return IsSpecificTypeAsField<KIND_ARRAY>(wn);
  }

  // recognize the nested struct
  // Eg.: struct A { struct B { int num; } b; };
  //     A a; a.b.num = 1;
  bool IsStructAsField(WN *wn) {
    Is_Trace(Tracing_enabled, (TFile, "IsStructAsField: "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

    return IsSpecificTypeAsField<KIND_STRUCT>(wn);
  }

  enum class NESTED_KIND {
    NESTED_NONE,
    NESTED_ARRAY_OF_STRUCT,
    NESTED_ARRAY_AS_FIELD,
    NESTED_STRUCT_AS_FIELD,
  };

  NESTED_KIND IsNestedAggregate(WN *wn) {
    Is_Trace(Tracing_enabled, (TFile, "IsNestedAggregate: "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

    if (IsArrayAsField(wn)) {
      return NESTED_KIND::NESTED_ARRAY_AS_FIELD;
    } else if (IsArrayOfStruct(wn)) {
      return NESTED_KIND::NESTED_ARRAY_OF_STRUCT;
    } else if (IsStructAsField(wn)) {
      return NESTED_KIND::NESTED_STRUCT_AS_FIELD;
    } else {
      return NESTED_KIND::NESTED_NONE;
    }
  }

  bool IsSameField(const FLD_HANDLE &fld, UINT64 offset) {
    UINT64 ofst = FLD_ofst(fld);
    bool same_field = (offset == ofst) ||
      // field n    <-- within this field
      // field n+1
      // ...
      (!FLD_next(fld).Is_Null() &&
          (FLD_ofst(FLD_next(fld)) > offset) && (ofst < offset)) ||
      // ...
      // field n-1
      // field n    <-- within this field
      (FLD_next(fld).Is_Null() &&
          ((TY_size(FLD_type(fld)) + ofst) > offset) && (ofst < offset));
    return same_field;
  }

  LVVAL      *CreateTrunc(LVVAL *lhs, LVTY *rhs_ty) {
    auto lhs_ty = lhs->getType();
    if (lhs_ty == rhs_ty) return lhs;

    if (lhs_ty->isIntegerTy()) {
      FmtAssert(rhs_ty->isIntegerTy(), ("CreateTrunc: rhs should be integer type"));
      FmtAssert(lhs_ty->getIntegerBitWidth() > rhs_ty->getIntegerBitWidth(), ("CreateTrunc: lhs width should be longer than lhs"));
      return Lvbuilder()->CreateTrunc(lhs, rhs_ty);
    } else if (lhs_ty->isFloatingPointTy()) {
      FmtAssert(rhs_ty->isFloatingPointTy(), ("CreateTrunc: rhs should be floating type"));
      FmtAssert(lhs_ty->getTypeID() > rhs_ty->getTypeID(), ("CreateTrunc: lhs width should be longer than lhs"));
      return Lvbuilder()->CreateFPTrunc(lhs, rhs_ty);
    } else {
      FmtAssert(FALSE, ("CreateTrunc: can't handle current type"));
    }
    return nullptr;
  }

  LVVAL      *CreateExt(LVVAL *lhs, LVTY *rhs_ty, bool is_signed = false) {
    auto lhs_ty = lhs->getType();
    if (lhs_ty == rhs_ty) return lhs;

    if (lhs_ty->isIntegerTy()) {
      // if lhs is a one-bit value, then only the zero-extend should be performed
      if (lhs_ty->getIntegerBitWidth() == 1) is_signed = false;

      FmtAssert(rhs_ty->isIntegerTy(), ("CreateExt: rhs should be integer type"));
      FmtAssert(lhs_ty->getIntegerBitWidth() < rhs_ty->getIntegerBitWidth(), ("CreateExt: lhs width should be shorter than lhs"));
      if (is_signed) 
        return Lvbuilder()->CreateSExt(lhs, rhs_ty);
      else
        return Lvbuilder()->CreateZExt(lhs, rhs_ty);
    } else if (lhs_ty->isFloatingPointTy()) {
      FmtAssert(rhs_ty->isIntegerTy(), ("CreateExt: rhs should be floating type"));
      FmtAssert(lhs_ty->getTypeID() < rhs_ty->getTypeID(), ("CreateExt: lhs width should be shorter than lhs"));
      return Lvbuilder()->CreateFPExt(lhs, rhs_ty);
    } else {
      FmtAssert(FALSE, ("CreateExt: can't handle current type"));
    }
    return nullptr;
  }

  bool        IsPointerAndIntegerTy(LVTY *t1, LVTY *t2) {
    return (t1->isPointerTy() && t2->isIntegerTy()) ||
           (t1->isIntegerTy() && t2->isPointerTy());
  }

  void        HandlePointerAndIntegerType(WN *wn, LVVAL **val, LVTY *dest_ty, bool to_ptr) {
    if (to_ptr) {
      // LVPRINT(dest_ty, "dest_ty");
      // LVPRINT((*val)->getType(), "val_ty");
      FmtAssert((dest_ty->isPointerTy() || dest_ty->isArrayTy()) &&
                (*val)->getType()->isIntegerTy(),
              ("dest_ty should be pointer type and val type should be int type"));
      Is_Trace(Tracing_enabled, (TFile, "HandlePointerAndIntegerType to_ptr for:\n"));
      if (wn != nullptr) {
        Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
      }
      *val = Lvbuilder()->CreateIntToPtr(*val, dest_ty);
    }
    else {
      FmtAssert((dest_ty->isIntegerTy() || dest_ty->isArrayTy()) &&
                (*val)->getType()->isPointerTy(),
              ("dest_ty should be int type and val type should be pointer type"));
      Is_Trace(Tracing_enabled, (TFile, "HandlePointerAndIntegerType to_int for:\n"));
      if (wn != nullptr) {
        Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
      }
      *val = Lvbuilder()->CreatePtrToInt(*val, dest_ty);
    }
  }

  LVVAL     *CastToTargetType(WN *wn, LVVAL *val, LVTY *target_ty, bool is_signed) {
    LVTY *val_ty = val->getType();

    if (val_ty == target_ty) return val;
    if (target_ty->isVoidTy()) return val;  // void is compatible with any type -shin

    enum val_ty_type { Int_ty, Ptr_ty, Flt_ty, Other_ty };
    val_ty_type ty_class;
    if (val_ty->isIntegerTy()) 
      ty_class = Int_ty;
    else if (val_ty->isFloatingPointTy())
       ty_class = Flt_ty;
    else if (val_ty->isPointerTy())
      ty_class = Ptr_ty;
    else
      ty_class = Other_ty;

    Is_Trace(Tracing_enabled, (TFile, "CastToTargetType (class %d) for:\n", ty_class));
    if (wn != nullptr) {
      Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
    }

    switch (ty_class) {
    case Int_ty: {
      if (target_ty->isIntegerTy()) {
        if (val_ty->getIntegerBitWidth() > target_ty->getIntegerBitWidth()) {
    // truncation
          Is_Trace(Tracing_enabled, (TFile, "  Trunc val\n"));
          return CreateTrunc(val, target_ty);
        } else {
    // extension
          Is_Trace(Tracing_enabled, (TFile, "  Extend val\n"));
          return CreateExt(val, target_ty, is_signed);
        }
      }
      else if (target_ty->isPointerTy() || target_ty->isArrayTy()) {
          Is_Trace(Tracing_enabled, (TFile, "  convert val to pointer:\n"));
          HandlePointerAndIntegerType(wn, &val, target_ty, true);
          return val;
      }
      else {
        LVPRINT(val, "STORE data");
        LVPRINT(target_ty, "STORE target_ty type");
        FmtAssert(FALSE, ("CastToTargetType: can't handle other src-type now"));
      }
      break;
    }
    case Flt_ty: {
      FmtAssert(target_ty->isFloatingPointTy(), ("CastToTargetType: val should be float type"));
      if (val_ty->getTypeID() > target_ty->getTypeID()) {
        Is_Trace(Tracing_enabled, (TFile, "  Reduce val fp precision\n"));
        return CreateTrunc(val, target_ty);
      } else {
        return CreateExt(val, target_ty);
      }
      break;
    }
    case Ptr_ty: {
      if (target_ty->isIntegerTy()) {
        Is_Trace(Tracing_enabled, (TFile, "  convert to integer\n"));
        HandlePointerAndIntegerType(wn, &val, target_ty, false);
      } else if (target_ty->isPointerTy()) {
        Is_Trace(Tracing_enabled, (TFile, "  cast pointer arg to int for:\n"));
        return Lvbuilder()->CreateBitCast(val, target_ty);
      }
      else {
        LVPRINT(val, "CastToTargetType, Val");
        LVPRINT(target_ty, "CastToTargetType, target_ty");
        FmtAssert(target_ty == val_ty,
                  ("CastToTargetType: expects target_ty %d but get val_ty %d",
                   target_ty->getTypeID(), val_ty->getTypeID()));
      }
      return val;
      break;      
    }
    case Other_ty: {
      LVPRINT(val, "val");
      LVPRINT(val_ty, "val_ty");
      LVPRINT(target_ty, "target_ty");
      FmtAssert(FALSE, ("CastToTargetType: can't handle %d type now", val_ty->getTypeID()));      
      return nullptr;
      break; 
    }
    default:
      FmtAssert(FALSE, ("CastToTargetType: Unexpected ty in enum"));
      break;
    }
    return nullptr;
  }

  void        Handle_arg_diff_ty(WN *wn, LVVALVEC &arglist, const LVFUNC *lvfunc) {
    const LVFUNCTY *lvfuncty = lvfunc->getFunctionType();
    Is_Trace(Tracing_enabled, (TFile, "Handle_arg_diff_ty: adjust actual args type in keeping with formal type:\n"));
    FmtAssert(arglist.size() == lvfuncty->getNumParams(),
      ("Handle_arg_diff_ty: Function %s, parameter number is wrong, expected %d got %d.",
        lvfunc->getName().str().c_str(), lvfuncty->getNumParams(), arglist.size()));

    for (int i = 0; i < arglist.size(); i++) {
      auto formal_ty = lvfuncty->getParamType(i);
      Is_True(arglist[i], ("Handle_arg_diff_ty: actual arg (count %d) is null", i));
      bool is_signed = lvfunc->getArg(i)->hasSExtAttr();
      arglist[i] = CastToTargetType(wn, arglist[i], formal_ty, is_signed);
    }
  }

  void        Handle_arg_diff_ty(WN *wn, LVVALVEC &arglist, const LVFUNCTY *lvfuncty) {
    Is_Trace(Tracing_enabled, (TFile, "Handle_arg_diff_ty: adjust actual args type in keeping with formal type:\n"));
    FmtAssert(arglist.size() == lvfuncty->getNumParams(),
      ("Handle_arg_diff_ty: Parameter number is wrong, expected %d got %d.",
        lvfuncty->getNumParams(), arglist.size()));

    for (int i = 0; i < arglist.size(); i++) {
      auto formal_ty = lvfuncty->getParamType(i);
      Is_True(arglist[i], ("Handle_arg_diff_ty: actual arg (count %d) is null", i));
      
      // FIXME: handle the case of function pointer
      // bool is_signed = lvfunc->getArg(i)->hasSExtAttr();

      arglist[i] = CastToTargetType(wn, arglist[i], formal_ty, /* FIXME: collect signext flag. */FALSE);
    }
  }

  LVVAL      *HandleStoreDifferentType(WN *wn, LVVAL *val, LVTY *dest_ty, bool is_signed = false) {
    Is_Trace(Tracing_enabled, (TFile, "HandleStoreDifferentType for:\n"));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
    return CastToTargetType(wn, val, dest_ty, is_signed);
  }

  // eg. I4I2LDID
  LVVAL      *HandleLoadImplicitCast(WN *wn, LVVAL *val, LVTY *desty, bool is_signed) {
    LVVAL *res = val;
    if (val->getType()->isIntegerTy()) {
      res = CastToTargetType(wn, val, desty, is_signed);
    }
    return res;
  }

  inline bool IsWNCmp(WN *wn) { return OPERATOR_is_compare(WN_operator(wn));  }

  void        HandleBinaryDifferentType(WN *wn, LVVAL **lhs, LVVAL **rhs) {
    Is_Trace(Tracing_enabled, (TFile, "HandleBinaryDifferentType for:\n"));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));

    LVTY *lhs_ty = (*lhs)->getType();
    LVTY *rhs_ty = (*rhs)->getType();
    if (lhs_ty == rhs_ty) return;
    else {
      // 1. both of lhs and rhs are int type
      if (lhs_ty->isIntegerTy() && rhs_ty->isIntegerTy()) {
        auto wn_ty1 = WN_desc(WN_kid0(wn));
        auto wn_ty2 = WN_desc(WN_kid1(wn));
        auto lhs_bitwidth = lhs_ty->getIntegerBitWidth();
        auto rhs_bitwidth = rhs_ty->getIntegerBitWidth();
        if (lhs_bitwidth > rhs_bitwidth) {
          // zext/sext rhs
          Is_Trace(Tracing_enabled, (TFile, "HandleBinaryDifferentType extend rhs for:\n"));
          Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
          if (MTYPE_is_signed(wn_ty2) && !llvm::isa<llvm::CmpInst>(*rhs)) {
            // add sext instruction
            *rhs = CreateExt(*rhs, lhs_ty, true);
          } else {
            *rhs = CreateExt(*rhs, lhs_ty, false);
          }
        } else {
          FmtAssert(lhs_bitwidth < rhs_bitwidth,
                    ("HandleBinaryDifferentType: lhs size should less than rhs size"));
          // zext/sext lhs
          Is_Trace(Tracing_enabled, (TFile, "HandleBinaryDifferentType extend lhs for:\n"));
          Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
          if (MTYPE_is_signed(wn_ty1) && !llvm::isa<llvm::CmpInst>(*lhs)) {
            // add sext instruction
            *lhs = CreateExt(*lhs, rhs_ty, true);
          } else {
            *lhs = CreateExt(*lhs, rhs_ty, false);
          }
        }
      } else if (IsPointerAndIntegerTy(lhs_ty, rhs_ty)) {
        // 2. one of lhs or rhs is pointer, and the other one is int
        // if wn is compare
        if (IsWNCmp(wn)) {
          if (lhs_ty->isIntegerTy())
            HandlePointerAndIntegerType(wn, lhs, rhs_ty, true);
          else
            HandlePointerAndIntegerType(wn, rhs, lhs_ty, true);
        } else {
          if (lhs_ty->isIntegerTy()) {
            // convert pointer to int64
            // TODO: add ABI option, default 64bits
            HandlePointerAndIntegerType(wn, rhs, LVTY::getInt64Ty(Context()), false);
            *lhs = CreateExt(*lhs, (*rhs)->getType());
          } else {
            // TODO: add ABI option, default 64bits
            HandlePointerAndIntegerType(wn, lhs, LVTY::getInt64Ty(Context()), false);
            *rhs = CreateExt(*rhs, (*lhs)->getType());
          }
        }
      } else {
        LVPRINT(*lhs,"Binary lhs type");
        LVPRINT(*rhs,"Binary rhs type");
        FmtAssert(FALSE, ("HandleBinaryDifferentType: can't handle the two values now"));
      }
    }
  }

  BOOL        Gen_displacement(WN *wn, LVVAL **base);
  LVVAL      *Gen_displacement_by_offset(WN *wn, LVVAL **base, INT offset);
  void        Create_bin_operands(WN *wn, LVVAL** lhs, LVVAL** rhs, BOOL tymatch = FALSE) {
    const char *oprname = OPERATOR2name(WN_operator(wn));
    *lhs = EXPR2llvm(WN_kid0(wn));
    FmtAssert(*lhs, ("Create lhs of %s failed", oprname));
    *rhs = EXPR2llvm(WN_kid1(wn));
    FmtAssert(*rhs, ("Create rhs of %s failed", oprname));
    if (tymatch) HandleBinaryDifferentType(wn, lhs, rhs);
  }

  UINT64      GenConstVarId() const {
    static UINT64 var_id;
    return var_id++;
  }

  LVVAL      *CreateCompare(WN *wn, LVVAL *lhs, LVVAL *rhs);
  LVVAL      *CreateBinary(WN *wn);
  LVVAL      *CreateUnary(WN *wn);
  LVVAL      *CreateBr(W2LBB *targbb) { return Lvbuilder()->CreateBr(targbb->Lvbb()); }

  void        Setup_be_scope(void) {
    BE_symtab_initialize_be_scopes();
    BE_symtab_alloc_scope_level(GLOBAL_SYMTAB);
    SYMTAB_IDX scope_level;
    for (scope_level = 0;
         scope_level <= GLOBAL_SYMTAB;
         ++scope_level) {
      // No need to deal with levels that don't have st_tab's. Currently
      // this should be only zero.
      if (Scope_tab[scope_level].st_tab != NULL) {
        Scope_tab[scope_level].st_tab->
          Register(*Be_scope_tab[scope_level].be_st_tab);
      }
      else {
        Is_True(scope_level == 0,
                ("Nonexistent st_tab for level %d", scope_level));
      }
    }
  }

  void        Gen_jmp_table(WN *branch) {
    // if ((Testing_mode() & TESTJMPTBL) != TESTJMPTBL)
      // return;
    WN *wn;
    INT i;
    ST *st;
    INITO_IDX ino;
    INITV_IDX inv, prev_inv, array_inv;

    // build jump table in init-data list
    // get symboltable address
    st = WN_st(branch);

    // create INITO for jmp_table of st
    ino = New_INITO(st);
    prev_inv = INITV_IDX_ZERO;

    // create INITV for jmp_table
    array_inv = New_INITV();

    wn = WN_first(WN_kid1(branch));	// first goto
    for (i = 0; i < WN_num_entries(branch); i++, wn = WN_next(wn)) {
      FmtAssert ((wn && WN_opcode(wn) == OPC_GOTO),
                 ("Gen_jmp_table: XGOTO block doesn't have goto's? (%d)", WN_opcode(wn)));
      LABEL_IDX lab = WN_label_number(wn);
      inv = New_INITV();
      INITV_Init_Label (inv, lab);
      prev_inv = Append_INITV (inv, ino, prev_inv);
    }
  }

  WN         *Lower_puwn(PU_Info *pu_info, WN *wn);

  LVSTRUCTTY *Get_glb_ctors_elem_ty() {
    // create element type -- { i32, ptr, ptr }
    LVTY *ptr = GetLVPtrTy();
    std::vector<LVTY*> elems { LVTY::getInt32Ty(Context()), ptr, ptr };
    return LVSTRUCTTY::get(Context(), elems);
  }

  LVGLBVAR   *Get_lv_glb_ctors(UINT64 size) {
    if (_glb_ctors != nullptr) return _glb_ctors;

    // create this array
    LVTY *elements = Get_glb_ctors_elem_ty();

    // array type
    LVTY *array_ty = llvm::ArrayType::get(elements, size);
    _module->getOrInsertGlobal(GLOBAL_CTORS_NAME, array_ty);
    _glb_ctors = _module->getNamedGlobal(GLOBAL_CTORS_NAME);
    _glb_ctors->setLinkage(llvm::GlobalValue::LinkageTypes::AppendingLinkage);
    _glb_ctors->setSection(CTOR_SECTION);
    return _glb_ctors;
  }

  LVCONST *Get_glb_init_priority(const std::string &func_name) {
    llvm::StringRef name(func_name);
    LVTY *ty = LVTY::getInt32Ty(Context());
    if (name.startswith(GLB_I_PREFIX)) {
      llvm::StringRef num_str = name.drop_while([](char ch) { return !std::isdigit(ch) || ch == '0'; });
      return llvm::ConstantInt::get(ty, std::stoul(num_str.str()));
    } else {
      FmtAssert(name.startswith(GLB_SUB_I_PREFIX),
        ("The global init function(%s) should start with _GLOBAL__sub_I_", func_name.c_str()));
      return llvm::ConstantInt::get(ty, 65535);
    }
  }

  void Add_glb_init_info(LVCONST *info) {
    _glb_init_registry.push_back(info);
  }

  void Register_llvm_glb_ctors() {
    UINT64 size = _glb_init_registry.size();
    Get_lv_glb_ctors(size);
    auto array_ty = llvm::ArrayType::get(Get_glb_ctors_elem_ty(), size);
    auto init = llvm::ConstantArray::get(array_ty, _glb_init_registry);
    _glb_ctors->setInitializer(init);
  }

public:
  WHIRL2llvm(char *input_file, INT skip_before, INT skip_after, INT testing_mode) :
    _skip_b(skip_before), _skip_a(skip_after), _testing_m(testing_mode) {
    _module = new llvm::Module(input_file, _context);
    _di_builder = new LVDIBUILDER(*_module);
    _has_fwd_tyref = FALSE;
    _cur_pu_num = -1;
    _cur_func = NULL;
    _svd_lvfunc = NULL;
    _cur_lvfunc = NULL;
    _lventry = NULL;
    _last_label = 0;
    _glb_ctors = nullptr;
    Set_whirl_level(input_file);
  }

  ~WHIRL2llvm() {
    // _di_builder->finalize();
    // delete _di_builder;
    delete _module;
    _di_builder = NULL;
    _di_cu = NULL;
    _module = NULL;
    _cur_func = NULL;
  }

  W2LBUILDER   *Builder(void)         { return _builder; }
  W2LBUILDER   *RnaBld(void)          { return _rna_builder; } // alive 1 FUNC_ENTRY
  LVBUILDER    *Lvbuilder(void)       { return _builder->Builder(); }
  LVMODULE     *Module(void)          { return _module; }
  LVDIBUILDER  *DIBuilder()           { return _di_builder; }
  LVDICU       *DICU()                { return _di_cu; }
  void          DICU(LVDICU *cu)      { _di_cu = cu; }
  STRVEC       &Inc_dirs()            { return _inc_dirs; }
  W2LFILEVEC   &W2L_files()            { return _w2l_files; }
  void          FUNC_ENTRY2llvm(PU_Info *, WN *);
  void          Init_debug_info();
  void          Create_inc_dirs();
  void          Create_w2lfiles();
  LVDILOC      *SRC_POS2diloc(const USRCPOS &);
  LVDILOC      *SRC_POS2diloc(INT64);
};  // class WHIRL2llvm

// =============================================================================
//
//
// =============================================================================
LVTY *BuiltinTy2llvm(TYPE_ID mtype, LVCONTEXT &ctx) {
  switch(mtype) {
    case MTYPE_B: 
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_I8:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
    case MTYPE_U8: {
      auto size = MTYPE_bit_size(mtype);
      return LVTY::getIntNTy(ctx, size);
    }
    case MTYPE_F4: return LVTY::getFloatTy(ctx);
    case MTYPE_F8: return LVTY::getDoubleTy(ctx);
    case MTYPE_F10: return LVTY::getDoubleTy(ctx);
    case MTYPE_F16: return LVTY::getFP128Ty(ctx);
    case MTYPE_V: return LVTY::getVoidTy(ctx);
    case MTYPE_UNKNOWN: return LVTY::getVoidTy(ctx);
    default: {
      Is_Trace(Tracing_enabled, (TFile, "BuiltinTy2llvm: mtype %s not supported\n", MTYPE_name(mtype)));
    }
    return nullptr;
  }
}

LVTY *WHIRL2llvm::Insert_ty_entry(UINT idx, TY *ty) {
  char *tyname = (TY_name_idx(*ty) == 0) ? NULL : TY_name(*ty);

  if (tyname && tyname[0] == '.') {
    char *newName = (char *)alloca(strlen(tyname) + 1);
    strcpy(newName, tyname);
    newName[0] = '_';
    tyname = newName;
  }

  LVTY *lvty = nullptr;
  switch (TY_kind(*ty)) {

  case KIND_VOID: {
    lvty = llvm::Type::getVoidTy(Context());
    Put_wty2lvty(idx, lvty);
    return lvty;
  }
  case KIND_SCALAR: {
    lvty = BuiltinTy2llvm(ty->mtype, Context());
    if (lvty) Put_wty2lvty(idx, lvty);
    return lvty;
  }
  case KIND_FUNCTION: {
    LVTY *func_ty = NULL;
    LVTY *lv_ret_ty = nullptr;

    if (TY_tylist(*ty) == 0) {
      lv_ret_ty = LVTY::getVoidTy(Context());
    } else {
      TY_IDX rettype_idx = Tylist_Table[TY_tylist(*ty)];
      if (DoesMapHasThisType(rettype_idx)) {
        lv_ret_ty = Wty2llvmty(TY_mtype(rettype_idx), rettype_idx);
      } else {
        Put_wty2lvty(idx, nullptr);
        //        Has_fwd_tyref(TRUE, idx); - SC
        return nullptr;
      }
    }

    LVTYVEC param_type_list;
    TYIDXVEC tyidxvec;
    if (TY_tylist(*ty) != 0) {
      TYLIST_IDX indx = TY_tylist(*ty);
      ++indx;   // inconsistent with the code in ST2llvm::operator()
      while (Tylist_Table[indx] != 0) {
        TY_IDX arg_ty_idx = Tylist_Table[indx];
        if (DoesMapHasThisType(arg_ty_idx)) {
          LVTY *lv_arg_ty = Wty2llvmty(TY_mtype(arg_ty_idx), arg_ty_idx);
          //param_type_list.push_back(lv_arg_ty);
          tyidxvec.push_back(arg_ty_idx);
        } else {
          Put_wty2lvty(idx, nullptr);
          //          Has_fwd_tyref(TRUE, idx); - SC
          return nullptr;
        }
        ++indx;
      }
    }

    if ((Testing_mode() & TESTFORMAL) != TESTDEFAULT) {
      // do not collect sign_ext_list since there is no LVFUNC created here
      Create_formal_parm_type(make_TY_IDX(idx), param_type_list, tyidxvec, NULL);
    }

    func_ty = llvm::FunctionType::get(lv_ret_ty, param_type_list, false);
    Put_wty2lvty(idx, func_ty);

    return func_ty;
  }
  case KIND_POINTER: {
    TY_IDX pointee_ty_idx = ty->Pointed();
    if (DoesMapHasThisType(pointee_ty_idx)) {
      Is_Trace(Tracing_enabled, (TFile, "Handling POINTER, index [%d]", idx));

      LVTY *pointee_ty = Wty2llvmty(TY_mtype(Ty_Table[pointee_ty_idx]),
                                                pointee_ty_idx, 0);
      Put_wty2lvty(idx, pointee_ty);
      return pointee_ty;
    } else {
      Put_wty2lvty(idx, nullptr);
      return nullptr;
      //      Has_fwd_tyref(TRUE, idx); - SC
    }
  }
  case KIND_ARRAY: {
    TY_IDX e_ty_idx = ty->Etype();
    if (!DoesMapHasThisType(e_ty_idx)) {
      Put_wty2lvty(idx, nullptr);
      //      Has_fwd_tyref(TRUE, idx); - SC
      return nullptr;
    }
    ARB_HANDLE arb(ty->Arb());
    INT dim = ARB_dimension(arb);
    FmtAssert(dim == 1, ("Insert_ty_entry: dim should be 1 for now"));

    INT64 size = 0;
    for (INT i = 0; i < dim; i++) {
      size = (arb[i].Entry()->Ubnd_val() - arb[i].Entry()->Lbnd_val()) + 1;
      Is_Trace(Tracing_enabled, (TFile, "Insert_ty_entry: handleing array dim, dim [%lld]\n", size));
    }

    auto elem_ty = Ty_Table[e_ty_idx];
    LVTY *lv_elem_ty = Wty2llvmty(TY_mtype(elem_ty), e_ty_idx);
    auto array_ty = llvm::ArrayType::get(lv_elem_ty, size);
    Put_wty2lvty(idx, array_ty);
    return array_ty;
  }
  case KIND_STRUCT: {
    std::vector<LVTY *> fields;
    std::string name = "";
    if (tyname) name = std::string(tyname);

    FLD_HANDLE fld = TY_fld(*ty);
    UINT last_FLD_ofst = 0;
    while (!fld.Is_Null()) {  // process each field
      TY_IDX fty_idx = FLD_type(fld);
      LVTY * lv_ty = nullptr;
      if (DoesMapHasThisType(fty_idx)) {
        const auto &wn_ty = Ty_Table[fty_idx];

        // handle bit-field
        if (FLD_is_bit_field(fld) !=0) {
          // get MTYPE of field
          TYPE_ID fld_mtype;
          if (FLD_bsize(fld) == 0) { // special case, for aligning purpose
            UINT skippedBytes = FLD_ofst(fld) - last_FLD_ofst;
            switch (skippedBytes) {
            case 0: FmtAssert(FALSE, ("TY2llvm::operator(): illegal presence of 0-sized bitfield")); break;
            case 1: fld_mtype = MTYPE_U1; break;
            case 2: fld_mtype = MTYPE_U2; break;
            case 3:
            case 4: fld_mtype = MTYPE_U4; break;
            default: fld_mtype = MTYPE_U8; break;
            }
          } else { // normal case
            UINT bfalign = MTYPE_bit_size(wn_ty.mtype);
            while (RoundDown(FLD_ofst(fld)*8 + FLD_bofst(fld) + FLD_bsize(fld) - 1, bfalign) !=
                   RoundDown(FLD_ofst(fld)*8 + FLD_bofst(fld), bfalign)) {
              bfalign *= 2;
            }
            switch (bfalign) {
              case 8: fld_mtype = MTYPE_U1; break;
              case 16: fld_mtype = MTYPE_U2; break;
              case 32: fld_mtype = MTYPE_U4; break;
              case 64: fld_mtype = MTYPE_U8; break;
            }
          }
          fld_mtype = Mtype_TransferSize(fld_mtype, wn_ty.mtype);
          lv_ty = Wty2llvmty(fld_mtype, fty_idx);
          // FmtAssert(FALSE, ("TY2llvm::operator, NYI bit-field"));
        } else {
          lv_ty = Wty2llvmty(TY_mtype(wn_ty), fty_idx);
        }

        fields.push_back(lv_ty);
      } else {   // fix up later
        Put_wty2lvty(idx, llvm::StructType::create(Context(), name));
  //        Has_fwd_tyref(TRUE);
        return nullptr;
      }

      // not handle bit-field now
      // if (FLD_is_bit_field(fld) != 0) {
      //   FmtAssert(FALSE, ("Insert_ty_entry: NYI bit-field"));
      // }

      fld = FLD_next (fld);
    }

    if (fields.empty()) {
      LVTY *i8 = llvm::IntegerType::get(Context(), 8);
      fields.push_back(i8);
    }

    auto struct_ty = llvm::StructType::create(Context(), fields, name);
    Put_wty2lvty(idx, struct_ty);
    return struct_ty;
  }
  default: FmtAssert(FALSE, ("Insert_ty_entry: NYI %s", Kind_Name(TY_kind(*ty))));
  }
  return nullptr;
}

struct TY2PTR {
  UINT        _idx;
  UINT       *_ptridx;
  WHIRL2llvm *_whirl2llvm;
  TY2PTR(UINT idx, UINT *pidx, WHIRL2llvm *w):
    _idx(idx), _ptridx(pidx), _whirl2llvm(w) {}

  void operator() (UINT idx, TY *ty) const;
};

void TY2PTR::operator() (UINT idx, TY *ty) const {
  if (*_ptridx == 0) {
    if ((TY_kind(*ty) == KIND_POINTER) &&
        (TY_pointed(*ty) == _idx)) {
      UINT lo = _idx & 0xff;
      *_ptridx = (idx << 8) | lo;
    }
  }
}

struct TY2llvm {
  WHIRL2llvm *whirl2llvm;
  TY2llvm(WHIRL2llvm *w) : whirl2llvm(w) {}

  void operator() (UINT idx, TY *ty) const;
};

void TY2llvm::operator() (UINT idx, TY *ty) const {
  char *tyname = (TY_name_idx(*ty) == 0) ? NULL : TY_name(*ty);

  if (tyname && tyname[0] == '.') {
    char *newName = (char *)alloca(strlen(tyname) + 1);
    strcpy(newName, tyname);
    newName[0] = '_';
    tyname = newName;
  }

  Is_Trace(Tracing_enabled, (TFile, "Types [%d]: %s size %lld Mtype %s, kind %d\n",
                             idx, tyname, ty->size, Mtype_Name(TY_mtype(*ty)), TY_kind(*ty)));
  whirl2llvm->Insert_ty_entry(idx, ty);
}

// =============================================================================
//
//
// =============================================================================
struct FixForwardTypeRef {
  WHIRL2llvm *whirl2llvm;
  FixForwardTypeRef(WHIRL2llvm *w) : whirl2llvm(w) {}

  void operator() (UINT idx, TY *ty) const;
};

void FixForwardTypeRef::operator() (UINT idx, TY *ty) const {
  auto kind = TY_kind(*ty);
  char *tyname = (TY_name_idx(*ty) == 0) ? NULL : TY_name(*ty);
  if (tyname && tyname[0] == '.') {
    char *newName = (char *)alloca(strlen(tyname) + 1);
    strcpy(newName, tyname);
    newName[0] = '_';
    tyname = newName;
  }


  auto &type_map = whirl2llvm->Get_wty2lvty();
  auto res = type_map.find(idx);
  if (res != type_map.end()) {
    auto ty = res->second;
    if (ty != nullptr) {
      if (!ty->isStructTy()) {
        return;
      }
      else if (!llvm::cast<llvm::StructType>(ty)->isOpaque()) return;
    }
  } else {
    return;
  }

  if (kind == KIND_POINTER || kind == KIND_ARRAY || kind == KIND_STRUCT || kind == KIND_FUNCTION)
    Is_Trace(Tracing_enabled, (TFile, "FixForwardTypeRef::Types [%d]: %s size %lld Mtype %s\n",
                             idx, tyname, ty->size, Mtype_Name(TY_mtype(*ty))));
  switch (kind) {
    case KIND_POINTER: {
      TY_IDX pointee_ty_idx = ty->Pointed();
      if (whirl2llvm->DoesMapHasThisType(pointee_ty_idx)) {
        LVTY *pointee_ty = whirl2llvm->Wty2llvmty(TY_mtype(Ty_Table[pointee_ty_idx]), pointee_ty_idx, 0);
        whirl2llvm->Set_wty2lvty(idx, pointee_ty);
      } else {

        Is_Trace(Tracing_enabled, (TFile, " set true, Pointer [%d] pointee ty index %x\n", idx, pointee_ty_idx));
        whirl2llvm->Has_fwd_tyref(TRUE, idx);
      }
      return;
    }
    case KIND_ARRAY: {
      TY_IDX e_ty_idx = ty->Etype();
      if (!whirl2llvm->DoesMapHasThisType(e_ty_idx)) {
        Is_Trace(Tracing_enabled, (TFile, " set true, Array\n"));
        whirl2llvm->Has_fwd_tyref(TRUE, idx);
        return;
      }
      ARB_HANDLE arb(ty->Arb());
      INT dim = ARB_dimension(arb);
      FmtAssert(dim == 1, ("TY2llvm::operator(): dim should be 1 for now"));

      INT64 size = 0;
      for (INT i = 0; i < dim; i++) {
        size = (arb[i].Entry()->Ubnd_val() - arb[i].Entry()->Lbnd_val()) + 1;
        Is_Trace(Tracing_enabled, (TFile, "TY2llvm::operator(): handleing array dim, dim [%lld]\n", size));
      }

      auto elem_ty = Ty_Table[e_ty_idx];
      LVTY *lv_elem_ty = whirl2llvm->Wty2llvmty(TY_mtype(elem_ty), e_ty_idx);
      auto array_ty = llvm::ArrayType::get(lv_elem_ty, size);
      whirl2llvm->Set_wty2lvty(idx, array_ty);
      return;
    }
    case KIND_FUNCTION: {
      LVTY *lv_ret_ty = nullptr;
      if (TY_tylist(*ty) == 0) {
        lv_ret_ty = LVTY::getVoidTy(whirl2llvm->Context());
      } else {
        TY_IDX rettype_idx = Tylist_Table[TY_tylist(*ty)];
        if (whirl2llvm->DoesMapHasThisType(rettype_idx)) {
          lv_ret_ty = whirl2llvm->Wty2llvmty(TY_mtype(rettype_idx), rettype_idx);
        } else {
          Is_Trace(Tracing_enabled, (TFile, " set true Func [%d]\n", idx));  
          whirl2llvm->Has_fwd_tyref(TRUE);
          return;
        }
      }

      std::vector<LVTY *> param_type_list;
      if (TY_tylist(*ty) != 0) {
        TYLIST_IDX indx = TY_tylist(*ty);
        ++indx;
        while (Tylist_Table[indx] != 0) {
          TY_IDX arg_ty_idx = Tylist_Table[indx];
          if (whirl2llvm->DoesMapHasThisType(arg_ty_idx)) {
            LVTY *lv_arg_ty = whirl2llvm->Wty2llvmty(TY_mtype(arg_ty_idx), arg_ty_idx);
            param_type_list.push_back(lv_arg_ty);
          } else {
            Is_Trace(Tracing_enabled, (TFile, " set true Parmlist [%x]\n", idx));
            whirl2llvm->Has_fwd_tyref(TRUE);
            return;
          }
          ++indx;
        }
      }

      LVTY *func_ty = llvm::FunctionType::get(lv_ret_ty, param_type_list, false);
      whirl2llvm->Set_wty2lvty(idx, func_ty);
      return;
    }
    case KIND_STRUCT: {
      std::vector<LVTY *> fields;

      FLD_HANDLE fld = TY_fld(*ty);
      UINT last_FLD_ofst = 0;
      while (!fld.Is_Null()) {  // process each field
        TY_IDX fty_idx = FLD_type(fld);
        LVTY * lv_ty = nullptr;
        if (whirl2llvm->DoesMapHasThisType(fty_idx)) {
          auto wn_ty = Ty_Table[fty_idx];
          lv_ty = whirl2llvm->Wty2llvmty(TY_mtype(wn_ty), fty_idx);
        } else {
          // fix later
          Is_Trace(Tracing_enabled, (TFile, " set true Struct [%d]\n", idx));
          whirl2llvm->Has_fwd_tyref(TRUE);
          return;
        }
        fields.push_back(lv_ty);
        fld = FLD_next (fld);
      }
      
      auto struct_ty = llvm::cast<llvm::StructType>(res->second);
      struct_ty->setBody(fields);
      return;
    }
  
    default: return;
  }
}


// =============================================================================
// get the pointer type of type
// =============================================================================
TY_IDX
WHIRL2llvm::Get_ptrtype(TY_IDX type)
{
  TY_IDX retv = 0;
  if (Mload_ty(type)) {
    For_all (Ty_Table, TY2PTR(type, &retv, this));
    if (retv == 0) {
      retv = Make_Pointer_Type(type);
      //Is_True(retv != 0, ("WHIRL2llvm::Get_ptrtype: can't find Pointer type for TY%d", type>>8));
    }
  }
  else
    retv = Make_Pointer_Type(type);
  return retv;
}

// =============================================================================
//
// wn is a expression that computes to a function pointer; find the TY_IDX of
// the KIND_FUNCTION TY node
//
// =============================================================================
TY_IDX
WHIRL2llvm::GetFuncType(WN *wn) {
  switch (WN_operator(wn)) {
  case OPR_LDA: {
    ST *st = &St_Table[WN_st_idx(wn)];
    FmtAssert(st->sym_class == CLASS_FUNC, ("GetFuncType: found non-function symbol"));
    return PU_prototype(Pu_Table[st->u2.pu]);
  }
  case OPR_LDID: {
    ST *st = &St_Table[WN_st_idx(wn)];
    TY_IDX st_ty_idx = st->u2.type;
    if (st->sym_class == CLASS_PREG) {
      Is_Trace(Tracing_enabled, (TFile, "GetFuncType : "));
      Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
      TY *st_ty = &Ty_Table[WN_ty(wn)];
      return st_ty->Pointed();
    } else {
      if (WN_field_id(wn) == 0) {
        TY *st_ty = &Ty_Table[st_ty_idx];
        return st_ty->Pointed();
      } else {
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(st_ty_idx, WN_field_id(wn), cur_field_id);
        TY *fld_ty = &Ty_Table[FLD_type(fld)];
        return fld_ty->Pointed();
      }
    }
    break;
  }
  case OPR_ILOAD: {
    TY_IDX iload_ty_idx = WN_load_addr_ty(wn);
    TY *iload_ty = &Ty_Table[iload_ty_idx];
    if (WN_field_id(wn) == 0) {
      return iload_ty->Pointed();
    } else {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(iload_ty->Pointed(), WN_field_id(wn), cur_field_id);
      TY *fld_ty = &Ty_Table[FLD_type(fld)];
      return fld_ty->Pointed();
    }
  }
  case OPR_SELECT:
  case OPR_CSELECT: return GetFuncType(WN_kid1(wn));
  default: FmtAssert(FALSE, ("GetFuncType: cannot find ICALL functype type"));
  }
  return 0;
}

template<typename OP>
LVVAL      *CreateCMP(LVVAL *lhs, LVVAL *rhs, OP op) {
  LVVAL *cmp = op(lhs, rhs);
  return cmp;
}

// =============================================================================
// Refactored LLVM cmp generation
// =============================================================================
enum FUNCUNIT { INTUNIT, FPUNIT };
typedef llvm::CmpInst::Predicate LVOPR;

template<OPERATOR _OPR, FUNCUNIT _UNIT> LVOPR Get_ilvopr(bool is_signed = false);
template<> LVOPR Get_ilvopr<OPR_EQ, INTUNIT>(bool is_signed) { return LVOPR::ICMP_EQ;  }
template<> LVOPR Get_ilvopr<OPR_NE, INTUNIT>(bool is_signed) { return LVOPR::ICMP_NE;  }
template<> LVOPR Get_ilvopr<OPR_GT, INTUNIT>(bool is_signed) { return is_signed ? LVOPR::ICMP_SGT : LVOPR::ICMP_UGT; }
template<> LVOPR Get_ilvopr<OPR_LT, INTUNIT>(bool is_signed) { return is_signed ? LVOPR::ICMP_SLT : LVOPR::ICMP_ULT; }
template<> LVOPR Get_ilvopr<OPR_GE, INTUNIT>(bool is_signed) { return is_signed ? LVOPR::ICMP_SGE : LVOPR::ICMP_UGE; }
template<> LVOPR Get_ilvopr<OPR_LE, INTUNIT>(bool is_signed) { return is_signed ? LVOPR::ICMP_SLE : LVOPR::ICMP_ULE; }
template<OPERATOR _OPR, FUNCUNIT _UNIT> LVOPR Get_flvopr();
template<> LVOPR Get_flvopr<OPR_EQ, FPUNIT>() { return LVOPR::FCMP_OEQ; }
template<> LVOPR Get_flvopr<OPR_NE, FPUNIT>() { return LVOPR::FCMP_ONE; }
template<> LVOPR Get_flvopr<OPR_GT, FPUNIT>() { return LVOPR::FCMP_OGT; }
template<> LVOPR Get_flvopr<OPR_LT, FPUNIT>() { return LVOPR::FCMP_OLT; }
template<> LVOPR Get_flvopr<OPR_GE, FPUNIT>() { return LVOPR::FCMP_OGE; }
template<> LVOPR Get_flvopr<OPR_LE, FPUNIT>() { return LVOPR::FCMP_OLE; }

template<OPERATOR _OPR> 
struct GEN_CMP {
  WHIRL2llvm *_wl;
  TYPE_ID     _tyid;
  LVOPR       _lvopr;
  GEN_CMP(WHIRL2llvm *wl, TYPE_ID t): _wl(wl), _tyid(t) {
    _lvopr = MTYPE_is_integral(t)?
      Get_ilvopr<_OPR, INTUNIT>(MTYPE_is_signed(t)) : Get_flvopr<_OPR, FPUNIT>();
  }
  LVVAL *operator() (LVVAL *lhs, LVVAL *rhs) {
    return (MTYPE_is_integral(_tyid)) ?
      _wl->Lvbuilder()->CreateICmp(_lvopr, lhs, rhs):
      _wl->Lvbuilder()->CreateFCmp(_lvopr, lhs, rhs);
  }
};

LVVAL*
WHIRL2llvm::CreateCompare(WN *wn, LVVAL *lhs, LVVAL *rhs)
{
  OPERATOR opr = WN_operator(wn);
  LVVAL   *cmp = nullptr;
  TYPE_ID  tyid = WN_desc(wn);
  Create_bin_operands(wn, &lhs, &rhs, TRUE);
  switch (opr) {
  case OPR_EQ: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_EQ>(this, tyid)); // after refactor
    break;
  }
  case OPR_NE: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_NE>(this, tyid)); // after refactor
    break;
  }
  case OPR_LT: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_LT>(this, tyid)); // after refactor
    break;
  }
  case OPR_GT: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_GT>(this, tyid)); // after refactor
    break;
  }
  case OPR_LE: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_LE>(this, tyid)); // after refactor
    break;
  }
  case OPR_GE: {
    cmp = CreateCMP(lhs, rhs, GEN_CMP<OPR_GE>(this, tyid)); // after refactor
    break;
  }
  defautl: 
    FmtAssert(FALSE, ("CreateCompare: not supported compare operator %s", OPERATOR2name(opr)));
  }
  FmtAssert(cmp != nullptr, ("CreateCompare: create %s instruction failed", OPERATOR2name(opr)));
  return cmp;
}

LVVAL*
WHIRL2llvm::CreateUnary(WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  LVVAL   *unr = nullptr;
  LVVAL   *opnd = EXPR2llvm(WN_kid0(wn));
  LVTY *op_type = opnd->getType();
  FmtAssert(opnd, ("CreateUnary: Create operand of %s failed", OPERATOR2name(opr)));
  switch (opr) {
  case OPR_NEG: {
    if (op_type->isIntOrIntVectorTy()) {
      unr = Lvbuilder()->CreateNeg(opnd);
    } else if (op_type->isFPOrFPVectorTy()) {
      unr = Lvbuilder()->CreateFNeg(opnd);
    } else {
      FmtAssert(FALSE, ("CreateUnary: not supported operand type %s", op_type->getTypeID()));
    }
    break;
  }
  case OPR_ABS: {
    if (op_type->isIntegerTy()) {
      LVVAL *zero = Lvbuilder()->getIntN(opnd->getType()->getIntegerBitWidth(), 0);
      LVVAL *lt0 = Lvbuilder()->CreateICmpSLT(opnd, zero);
      LVVAL *neg_val = Lvbuilder()->CreateNeg(opnd);
      unr = Lvbuilder()->CreateSelect(lt0, neg_val, opnd);
    } else if (op_type->isFloatingPointTy()) {
      LVFUNC *TheFn = llvm::Intrinsic::getDeclaration(Module(), llvm::Intrinsic::fabs, {op_type});
      unr = Lvbuilder()->CreateCall(TheFn, {opnd});
    } else {
      FmtAssert(FALSE, ("CreateUnary: only support int and float"));
    }
    break;
  }
  case OPR_BNOT: {
    // ~a === xor a, -1
    LVVAL *neg_one = Lvbuilder()->getIntN(opnd->getType()->getIntegerBitWidth(), -1);
    unr = Lvbuilder()->CreateXor(neg_one, opnd);
    break;
  }
  case OPR_SQRT: {
    LVTY *double_ty = Lvbuilder()->getDoubleTy();
    LVTYVEC argstype = std::vector<LVTY*>{double_ty};
    LVFUNC   *func = Module()->getFunction("sqrt");
    LVFUNCTY *lvfuncty = LVFUNCTY::get(double_ty, argstype, false);
    LVVALVEC arglist = {opnd};

    if (func == nullptr) {
      func = LVFUNC::Create(lvfuncty,
                            llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                            "sqrt",
                            Module());
      func->setCallingConv(llvm::CallingConv::C);
    }
    FmtAssert(func, ("CreateUnary, function \"sqrt\" is nullptr"));

    // handle parameter type
    CastToTargetType(wn, opnd, double_ty, false);
    LVCALL *call = Lvbuilder()->CreateCall(lvfuncty, func, arglist);
    SetCallInstAttrs(call, func);
    unr = call;
    break;
  }
  case OPR_ALLOCA: {
    INT64 size = WN_const_val(WN_kid0(wn));
    LVTY *array_ty = llvm::ArrayType::get(Lvbuilder()->getInt8Ty(), size);
    unr = Lvbuilder()->CreateAlloca(array_ty, size);
    break;
  }
  case OPR_LNOT: {
    FmtAssert(opnd->getType()->isIntegerTy(), 
      ("CreateUnary: the type of operand should be boolean type"));
    if (!opnd->getType()->isIntegerTy(1)) {
      opnd = Lvbuilder()->CreateTrunc(opnd, Lvbuilder()->getInt1Ty());
    }
    unr = Lvbuilder()->CreateXor(opnd, Lvbuilder()->getTrue());
    break;
  }
  case OPR_RECIP:
  default:
    FmtAssert(FALSE, ("CreateUnary: operator %s work in progress", OPERATOR2name(opr)));
  }
  FmtAssert(unr != nullptr, ("CreateUnary: create unary op failed"));
  return unr;
}

LVVAL*
WHIRL2llvm::CreateBinary(WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  LVVAL   *bin = nullptr;
  LVVAL   *lhs, *rhs;
  Create_bin_operands(wn, &lhs, &rhs, TRUE);

  switch (opr) {
  case OPR_BAND:
    bin = Lvbuilder()->CreateAnd(lhs, rhs);
    break;
  case OPR_BIOR:
    bin = Lvbuilder()->CreateOr(lhs, rhs);
    break;
  case OPR_BXOR:
    bin = Lvbuilder()->CreateXor(lhs, rhs);
    break;
  case OPR_SHL:
    bin = Lvbuilder()->CreateShl(lhs, rhs);
    break;
  case OPR_ASHR:
    bin = Lvbuilder()->CreateAShr(lhs, rhs);
    break;
  case OPR_LSHR:
    bin = Lvbuilder()->CreateLShr(lhs, rhs);
    break;
  case OPR_ADD:
    if (lhs->getType()->isIntegerTy()) {
      bin = Lvbuilder()->CreateAdd(lhs, rhs);
    } else if (lhs->getType()->isFloatingPointTy()) {
      bin = Lvbuilder()->CreateFAdd(lhs, rhs);
    } else {
      FmtAssert(FALSE, ("CreateBinary: operands aren't int or float type"));
    }
    break;
  case OPR_MIN:
  case OPR_MAX: {
    LVVAL *cmp = nullptr;
    if (lhs->getType()->isIntegerTy()) {
      cmp = MTYPE_is_signed(WN_desc(wn)) ? 
              Lvbuilder()->CreateICmpSGT(lhs, rhs) :
              Lvbuilder()->CreateICmpUGT(lhs, rhs);
    } else if (lhs->getType()->isFloatingPointTy()) {
      cmp = Lvbuilder()->CreateFCmpOGT(lhs, rhs);
    } else {
      FmtAssert(FALSE, ("WHIRL2llvm::CreateBinary: can't handle current type"));
    }
    if (opr == OPR_MIN) {
      bin = Lvbuilder()->CreateSelect(cmp, rhs, lhs);
    } else {
      bin = Lvbuilder()->CreateSelect(cmp, lhs, rhs);
    }
    break;
  }
  default:
    FmtAssert(FALSE, ("WHIRL2llvm::CreateBinary, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  FmtAssert(bin != nullptr, ("WHIRL2llvm::CreateBinary, create binary %s failed", OPERATOR2name(opr)));
  return bin;
}

// =============================================================================
// If the size of the mtype (used in load/store) is the same as the size of 
// the struct, return the struct's type, otherwise, return a predefined
// type corresponding to mtype.
// =============================================================================
static inline TY_IDX
struct_memop_type (TYPE_ID mtype, TY_IDX struct_type)
{
    if (TY_size (struct_type) != MTYPE_byte_size (mtype))
      Set_TY_IDX_index (struct_type, TY_IDX_index(MTYPE_To_TY (mtype)));
    return struct_type;
}


// =============================================================================
// The struct consists of cxx empty struct members and other padding area, 
// whose contents are not defined.
// =============================================================================
static BOOL is_struct_content_padding(TY_IDX ty, INT32 offset, INT32 len)
{
  FLD_IDX fld_idx = Ty_Table[ty].Fld();
  if (fld_idx == 0) return TRUE;
  do {
    FLD_HANDLE fld(fld_idx);
    if (TY_kind(FLD_type(fld)) != KIND_STRUCT) 
    {
      INT32 myoffset, myend;
      myoffset = FLD_ofst(fld);
      myend= myoffset + TY_size(FLD_type(fld));
      if (myoffset <= offset && myend > offset ||
        myoffset < offset + len && myend >= offset + len ||
        offset <= myoffset && offset + len > myoffset)
        return FALSE;
    }else if (!is_struct_content_padding(FLD_type(fld), offset - FLD_ofst(fld), len ))
      return FALSE;
    if (FLD_last_field(fld))
      break;
    fld_idx++;
  } while (1);
  return TRUE;
} 


WNPAIR
WHIRL2llvm::Load_mload_actual(WN* mload, PLOC& ploc)
{
  // model after wn_lower.cxx / lower_mload_actual
  TY_IDX mloadTY = TY_Of_Parameter(mload);

  Setup_Struct_Output_Parameter_Locations(mloadTY);
  ploc = Get_Struct_Output_Parameter_Location(ploc);
 
  WN    *firstld = NULL;
  WN    *secondld = NULL;
  INT32  size, mloadOffset = 0; 
  WN    *mloadSize = WN_kid1(mload);

  if (WN_operator(mloadSize) != OPR_INTCONST) {
    DevWarn("mload_actual size is not INTCONST");
    if (WN_operator(mloadSize) == OPR_CVTL) {
      mloadSize = WN_kid0(mloadSize);
    }
  }
  size = WN_const_val(mloadSize);
  if (size<=0 || WN_operator(mloadSize) != OPR_INTCONST)
  {
    DevWarn("size of mload actual should be > 0");
    size = TY_size(Ty_Table[mloadTY]);
    DevWarn("adjusting size of (%s) to TY_size= %d",
      TY_name(Ty_Table[mloadTY]), size);
    mloadSize = WN_Intconst(Integer_type, size);
  }

  Is_True((WN_opcode(mload) == OPC_MLOAD),
    ("expected MLOAD node, not %s", OPCODE_name(WN_opcode(mload))));

  WN       *addr = WN_COPY_Tree(WN_kid0(mload));
  // create addrN assignment to hold the address of the mload
  // Adjust the address if there is an offset in the mload
  PREG_NUM  addrN = Create_Preg(Pointer_type, NULL);
  ST       *preg = MTYPE_To_PREG(Pointer_type);
  {
    if (WN_load_offset(mload)) {
      addr = WN_Add(Pointer_type,
                    addr,
                    WN_Intconst(Pointer_type, WN_load_offset(mload)));
#if 0
      // we need to track what memory is being accessed when storing
      // an lda into a preg.  So if tree has an lda, or indirects to lda
      // through another preg, put that in preg table.
      WN *lda = Find_Lda (addr);
      if (lda) Set_Preg_Lda (addrN, lda);
      {
        WN *stid;
        stid = WN_Stid(TY_mtype(Ty_Table[ST_type(preg)]), addrN, preg, Pointer_type, addr);
      }
#endif
    }
  }

  while (PLOC_is_nonempty(ploc)) {
    if (PLOC_on_stack(ploc)) {
      /*
       *  structure is already on the stack, nothing to do
       */
      return std::make_pair(nullptr, nullptr);
    } else {
      PREG_NUM regNo = PLOC_reg(ploc);
      ST      *reg   = Preg_Offset_Is_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID  type  = TY_mtype(Ty_Table[ST_type(reg)]);
      WN      *load;
      INT32    todo = size - mloadOffset;

      // load  = WN_LdidPreg(type, regNo);
      // Is_Trace(Tracing_enabled, (TFile, "Replace it with actual args: "));
      // Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, ldid));
      if (PLOC_size(ploc) < MTYPE_size_reg(type)
          && type == MTYPE_F8 && PLOC_size(ploc) == 4) {
        // can happen in reality for X86-64
        // need to copy a smaller size than default reg-size
        DevWarn("actual_mload: switch from mtype_f8 to mtype_f4");

        type = MTYPE_F4;
        reg = MTYPE_To_PREG(type);
      }

      if (PLOC_size(ploc) < MTYPE_size_reg(type) && type == MTYPE_F8 && 
          is_struct_content_padding(mloadTY, mloadOffset, PLOC_size(ploc))) {
        // void MSTRUCT in SSE register, simply ignore 
        mloadOffset += PLOC_size(ploc);
        ploc = Get_Struct_Output_Parameter_Location(ploc);
        continue;
      }

      // special case "small" structs (or remainder of struct)
      // we will try not to run off the end of the structure (as bad)
      if (todo < MTYPE_alignment(type)) {
        TYPE_ID  quantum;
        INT32    newAlign, shiftn;

        Is_True(Preg_Offset_Is_Int(regNo), ("mload actual->reg(size/alignment problem"));
        newAlign = nearest_power_of_two(todo);
        quantum = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));

        //load = WN_IloadLdid(quantum, mloadOffset,
        //                    struct_memop_type (quantum, mloadTY), preg,
        //                    addrN); 
        load = WN_Iload(quantum, mloadOffset,
                        struct_memop_type (quantum, mloadTY), WN_COPY_Tree(addr));

        if (Target_Byte_Sex == BIG_ENDIAN) {
          shiftn = MTYPE_size_reg(type) - MTYPE_size_reg(quantum);
          load = WN_Shl(type, load, WN_Intconst(type, shiftn));
        }
      }
      else {
        //load = WN_IloadLdid(type, mloadOffset,
        //                    struct_memop_type (type, mloadTY), preg,
        //                    addrN); 
        load = WN_Iload(type, mloadOffset,
                        struct_memop_type (type, mloadTY), WN_COPY_Tree(addr));
      }
      Is_Trace(Tracing_enabled, (TFile, "load_mload_actual generated the value load:\n"));
      Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, load));
      if (firstld == NULL)
        firstld = load;
      else
        secondld = load;

      mloadOffset += PLOC_size(ploc);
    }
    ploc = Get_Struct_Output_Parameter_Location(ploc);
  }
  return std::make_pair(firstld, secondld);
}

// =============================================================================
// Actual args traversal
// =============================================================================
template <class OP>
void For_actual_args(WN *wn, WHIRL2llvm *wl, const OP &op)
{
  if ((wl->Testing_mode() & TESTACTUAL) == TESTDEFAULT || wl->Intrc_as_call()) {
    for (UINT i = 0; i < WN_kid_count(wn); i++) {
      WN *parm_wn = WN_kid(wn, i);

      if (WN_operator(parm_wn) != OPR_PARM)
        break; // OPR_ICALL and a few other OPERATORS's last kid has other means

      Is_Trace(Tracing_enabled, (TFile, "For_actual_args peeking : "));
      Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, parm_wn));

      op(parm_wn, wl);
    }
    return;
  }

  ST    *callee_st = (WN_has_sym(wn))? WN_st(wn) : NULL;
  TY_IDX call_ty = (WN_operator_is(wn, OPR_ICALL) ? WN_ty(wn) :
                    ST_pu_type(callee_st));
  PLOC ploc = Setup_Output_Parameter_Locations(call_ty);
  RNAITER rnaiter = wl->Find_rna(wn);
  //Is_True(!wl->Rnaiter_end(rnaiter), ("For_actual_args on %s got rnaiter_end", ST_name(callee_st)));

  for (INT i = 0; i < WN_kid_count(wn); i++) {
    WN      *parm = WN_actual(wn, i);
    if (WN_operator(parm) != OPR_PARM)
        break; // OPR_ICALL and a few other OPERATORS's last kid has other means

    TYPE_ID  parmType = WN_rtype(parm);

    Is_Trace(Tracing_enabled, (TFile, "For_actual_args peeking : "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, parm));

    ploc = Get_Output_Parameter_Location(TY_Of_Parameter(parm));

    bool on_stack = PLOC_on_stack(ploc);
    WN *actual = WN_operator_is(parm, OPR_PARM) ? WN_kid0(parm) : parm;
    if (MTYPE_is_m(parmType) && (WN_operator(actual) != OPR_LDID) && !on_stack) {
      WNPAIR lo_parm = wl->Load_mload_actual(actual, ploc);
      if (!wl->Rnaiter_end(rnaiter))
        rnaiter->Add_arg(lo_parm);
      if (lo_parm.first)
        op(lo_parm.first, wl);
      if (lo_parm.second)
        op(lo_parm.second, wl);
    } else {
#if 0
      PREG_NUM regNo = PLOC_reg(ploc);
      ST      *reg   = Preg_Offset_Is_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID  type  = TY_mtype(Ty_Table[ST_type(reg)]);
      WN      *ldid  = WN_LdidPreg(type, regNo);
      if (wl->Whirl_level() != ML)
        parm = ldid; // TODO, only replace if input is not M WHIRL
#endif
      Is_Trace(Tracing_enabled, (TFile, "Replace it with actual args: "));
      Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, parm));
      if (!wl->Rnaiter_end(rnaiter))
        rnaiter->Add_arg(std::make_pair(parm, nullptr));

      if (WN_operator(actual) == OPR_MLOAD) { // Hack while use x8664 calling convention for RISCV
        op(WN_kid0(actual), wl);
      } else {
        op(parm, wl);
      }
    }
  }

  Is_Trace(Tracing_enabled, (TFile, "For_actual_args completed :\n "));
  if (!wl->Rnaiter_end(rnaiter))
    Is_Trace_cmd(Tracing_enabled, rnaiter->Print(TFile));
}

struct COLLECT_VALUE {
  LVVALVEC   *lvvalvec;
  COLLECT_VALUE(LVVALVEC *lvvec) : lvvalvec(lvvec) {}

  void operator() (WN *wn, WHIRL2llvm *wl) const {
    LVVAL *val = wl->EXPR2llvm(wn);

    // FIXME: we should remove this hardcode for va_arg on RISCV
    if (WN_operator(wn) == OPR_PARM) {
      wn = WN_kid0(wn);
    }
    if (WN_has_sym(wn)) {
      TY_IDX tyidx = WN_type(wn);
      if (TY_kind(tyidx) == KIND_ARRAY) {
        tyidx = TY_etype(tyidx);
        if ((TY_kind(tyidx) == KIND_STRUCT) && (TY_name(tyidx) == std::string("__va_list_tag"))) {
          // FmtAssert(FALSE, ("COLLECT_VALUE::operator(): NYI for __va_list_tag"));
          val = wl->Lvbuilder()->CreateLoad(wl->GetLVPtrTy(), val);
        }
      }
    }

    lvvalvec->push_back(val);
  }
};

struct COLLECT_LVTY {
  LVTYVEC    *lvtyvec;
  COLLECT_LVTY(LVTYVEC *ltvec) : lvtyvec(ltvec) {}
 
 void operator() (WN *wn, WHIRL2llvm *wl) const {
    auto val = wl->Wty2llvmty(WN_rtype(wn), WN_ty(wn));
    lvtyvec->push_back(val);
  }
};

struct COLLECT_RNA {
  WHIRL2llvm *_wl;
  COLLECT_RNA(WHIRL2llvm *wl) : _wl(wl) {}
 
  void operator() (WN *wn) const {
    OPERATOR opr = WN_operator(wn);
    if (OPERATOR_is_call(opr)) {
      WN *retv = WN_next(wn);
      retv = (retv && WN_operator(retv) != OPR_STID)? NULL : retv;
      _wl->RnaBld()->Append_rnalst(RNA(wn, retv));
    }
  }
};

struct GEN_JMPTBL {
  WHIRL2llvm *_wl;
  GEN_JMPTBL(WHIRL2llvm *wl) : _wl(wl) {}
 
 void operator() (WN *wn) const {
    OPERATOR opr = WN_operator(wn);
    if (opr == OPR_XGOTO) {
      _wl->Gen_jmp_table(wn);
    }
 }
};

void
WHIRL2llvm::Create_mload_formal(LVTYVEC& argstype, TY_IDX idx, PLOC& ploc, char *name, SIGNVEC *info_list)
{
  INT32   size, offset = 0; 
  Setup_Struct_Input_Parameter_Locations(idx);
  ploc = Get_Struct_Input_Parameter_Location(ploc);
  size = TY_size(Ty_Table[idx]);

  while (PLOC_is_nonempty(ploc)) {
    if (PLOC_on_stack(ploc)) {
      //  structure is already on the stack, nothing specific to do
      // Add the type to the argument list.
      TYPE_ID type = TY_mtype(idx);
      LVTY   *lvty = Wty2llvmty(type, idx, MTYPE_bit_size(type));

      // convert this struct type to a pointer type
      argstype.push_back(GetLVPtrTy());

      if (name != NULL) {
        Parm_name().push_back(AGGMAP(name, ONSTACK, idx));
        Is_Trace(Tracing_enabled,
                 (TFile, "    Create_mload_formal(%d), creates %s <- PREG%d\n",
                  idx>>8, name, ONSTACK));
      }
      if (info_list) info_list->push_back(EXT_FLG::NONE);
      return;
    } else {
      PREG_NUM  regNo = PLOC_reg(ploc);
      ST       *reg = Preg_Offset_Is_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID   type = TY_mtype(ST_type(reg));
      INT32     todo = size - offset;

      if (PLOC_size(ploc) < MTYPE_size_reg(type)
          && type == MTYPE_F8 && PLOC_size(ploc) == 4) {
        type = MTYPE_F4;
        reg = MTYPE_To_PREG(type);
      }
      if (PLOC_size(ploc) < MTYPE_size_reg(type) && type == MTYPE_F8 && 
          Is_struct_content_padding(idx, offset, PLOC_size(ploc))) {
        // void MSTRUCT in SSE register, simply ignore
        offset += PLOC_size(ploc);
        ploc = Get_Struct_Input_Parameter_Location(ploc);
        continue;
      }

      // special case "small" structs (or remainder of struct)
      // to use type closer to size of remaining todo
      if (todo < MTYPE_alignment(type)) {
        Is_Trace(Tracing_enabled,
                 (TFile, "Create_mload_formal mtype_alignment of type %d is %d\n", type,
                  MTYPE_alignment(type)));
        TYPE_ID quantum;
        INT32   newAlign;

        Is_True(Preg_Offset_Is_Int(regNo),
                ("mload actual->reg(size/alignment problem"));
        newAlign = nearest_power_of_two(todo);
        type = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));
      }

      // Add the type to the argument list.
      LVTY   *lvty = Wty2llvmty(type, Be_Type_Tbl(type), MTYPE_bit_size(type));
      argstype.push_back(lvty);

      // get EXT_FLG info
      if (info_list) {
        info_list->push_back(Get_ext_flag(lvty, type));
      }

      // create the name for the sub-field mload as needed
      if (name != NULL) {
        char *name_w_modifier = Syn_name_w_modifier(name, offset);
        Parm_name().push_back(AGGMAP(name_w_modifier, regNo, ST_type(reg)));
        Is_Trace(Tracing_enabled,
                 (TFile, "    Create_mload_formal(%d) creates %s <- PREG%d\n", idx>> 8,  name_w_modifier, regNo));
      }
      offset += PLOC_size(ploc);
    }
    ploc = Get_Struct_Input_Parameter_Location(ploc);
  }
}


EXT_FLG  WHIRL2llvm::Get_ext_flag(LVTY *lvty, TYPE_ID tyid) {
  if (lvty->isIntegerTy()) {
    if (MTYPE_is_signed(tyid)) {
      return EXT_FLG::SEXT;
    } else {
      return EXT_FLG::ZEXT;
    }
  } else {
    return EXT_FLG::NONE;
  }
}

void
WHIRL2llvm::Set_func_attr(TY_IDX putyidx, LVFUNC *func, SIGNVEC *sign_list)
{
  TY_IDX    ret_idx = TY_ret_type(putyidx);
  LVFUNCTY *lvfuncty = func->getFunctionType();
  LVTY     *ret_type = lvfuncty->getReturnType();

  // set attributes
  LVATTR target_feature = LVATTR::get(Context(), "target-features", "+a,+c,+d,+f,+m,+relax");
  func->addFnAttr(target_feature);
  func->addFnAttr(LVATTR::NoUnwind);

  // set return attribute
  if (ret_type->isIntegerTy()) {
    TYPE_ID tyid = TY_mtype(ret_idx);
    if (MTYPE_is_integral(tyid)) {
      if (MTYPE_is_signed(tyid)) {
        func->addRetAttr(LVATTR::SExt);
      } else if (MTYPE_is_unsigned(tyid)) {
        func->addRetAttr(LVATTR::ZExt);
      }
    }
    else
      ; // struct returned through register
  }

  for (auto i = 0; i < sign_list->size(); i++) {
    if ((*sign_list)[i] == EXT_FLG::NONE) continue;
    func->addParamAttr(i,
                       ((*sign_list)[i] == EXT_FLG::SEXT) ? 
                       LVATTR::SExt :
                       LVATTR::ZExt);
  }
}

// =============================================================================
// Add LVTY to argstype list and update PLOC if the caller pass in the record
// =============================================================================
LVTY*
WHIRL2llvm::Create_ret_type(TY_IDX putyidx, LVTYVEC& argstype, SIGNVEC *info_list, PLOC *ploc)
{
  TY_IDX ret_idx = TY_ret_type(putyidx);
  LVTY  *ret_type = Wty2llvmty(TY_mtype(ret_idx), ret_idx);

  RETURN_INFO return_info = Get_Return_Info(ret_idx, Use_Simulated, FALSE);
  if (RETURN_INFO_return_via_first_arg(return_info)) {
    // fake first parm
    // This happens, for example, if a complex result is returned for ia32.
    // Create an istore; an mstore with a single complex value seemed
    // to confuse things later on.

    LVTY *fake_param_ty = GetLVPtrTy();
    if (ploc != NULL) {
      *ploc = Get_Input_Parameter_Location(Get_ptrtype(ret_idx));
      argstype.push_back(fake_param_ty);

      // add extend info which must match the argstype size
      if (info_list != NULL)
        info_list->insert(info_list->begin(), 
                          Get_ext_flag(fake_param_ty, TY_mtype(ret_idx)));
    }
    ret_type = llvm::Type::getVoidTy(Context());
  }
  else {
    // return via 1 or more return registers
    // we change the original struct return type to the type of PREG
    // use just one register to see how llvm respond
    TYPE_ID mtype = RETURN_INFO_mtype(return_info, 0);
    ST     *preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
    ret_type = Wty2llvmty(mtype, ST_type(preg_st));
  }

  return ret_type;
}

BOOL
WHIRL2llvm::Contain_lowered_vararg(TY_IDX putyidx)
{
  if (!TY_is_varargs(putyidx))
    return FALSE;
  return (Testing_mode() & TESTLOWERVARARG) != TESTDEFAULT;
}


// =============================================================================
//
// =============================================================================
void
WHIRL2llvm::Set_intrc_as_call(void)
{
  _testing_m |= TESTINTRC;
}

void
WHIRL2llvm::Reset_intrc_as_call(void)
{
  _testing_m ^= TESTINTRC;
}

BOOL
WHIRL2llvm::Intrc_as_call(void)
{
  return (_testing_m & TESTINTRC) == TESTINTRC;
}

LVVAL *
WHIRL2llvm::Handle_intrn_call(WN *wn) {
  bool is_stmt = (WN_operator(wn) == OPR_INTRINSIC_CALL) ? true : false;
  TYPE_ID  rtype = WN_rtype(wn);
  LVTY    *rettype = Wty2llvmty(rtype, Be_Type_Tbl(rtype));

  LVTYVEC  argstype;
  LVVALVEC arglist;
  Set_intrc_as_call();
  For_actual_args(wn, this, COLLECT_LVTY(&argstype));
  For_actual_args(wn, this, COLLECT_VALUE(&arglist));
  Reset_intrc_as_call();

  std::string func_name;
  switch(WN_intrinsic(wn)) {
    case INTRN_F8EXP:   func_name = "exp";     break;
    case INTRN_F8SIN:   func_name = "sin";     break;
    case INTRN_F8COS:   func_name = "cos";     break;
    case INTRN_STRLEN:  func_name = "strlen";  break;
    case INTRN_STRCPY:  func_name = "strcpy";  break;
    case INTRN_STRCMP:  func_name = "strcmp";  break;
    case INTRN_MEMCPY:  func_name = "memcpy";  break;
    case INTRN_MEMSET:  func_name = "memset";  break;
    case INTRN_MEMCMP:  func_name = "memcmp";  break;
    case INTRN_MEMCCPY: func_name = "memccpy"; break;
    case INTRN_MEMMOVE: func_name = "memmove"; break;
    default:
      FmtAssert(FALSE, ("WHIRL2llvm::Handle_intrn_call: unexpected intrinsic"));
  }

  LVFUNC   *func = Module()->getFunction(func_name);
  LVFUNCTY *lvfuncty = LVFUNCTY::get(rettype, argstype, false);
  if (func == nullptr) {

    func = llvm::Function::Create(lvfuncty,
                                  llvm::GlobalValue::LinkageTypes::ExternalLinkage,
                                  func_name,
                                  Module());
    func->setCallingConv(llvm::CallingConv::C);
  }

  FmtAssert(func, ("WHRIL2llvm::Handle_intrn_call, function %s is nullptr", func_name.c_str()));
  Handle_arg_diff_ty(wn, arglist, func);

  LVCALL *call = Lvbuilder()->CreateCall(lvfuncty, func, arglist);
  SetCallInstAttrs(call, func);
  
  // OPR_INTRINSIC_CALL is a STMT, but OPR_INTRINSIC is an EXPR
  if (is_stmt) {
    // Create the name of PREG that store value from return register
    LVALC *pname = nullptr;
    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy())) {
      pname = Collect_retval_pregname(wn);
    }

    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy()) && (pname != nullptr)) {
      auto ret_val = HandleStoreDifferentType(wn, call, pname->getAllocatedType());
      Lvbuilder()->CreateStore(ret_val, pname);
    }
    return nullptr;
  } else {
    return call;
  }
}

// =============================================================================
//
// =============================================================================
void
WHIRL2llvm::Collect_vararg_input_parm(PLOC ploc, LVTYVEC& argstype, SIGNVEC *info_list)
{
  INT num_vararg_xmms = 0;
  ST *xmm_save_temp_st;

  // x86-64 always needs to call Get_Vararg_Parameter_Location
  if( Is_Target_64bit() ){
    ploc = Get_Vararg_Input_Parameter_Location (ploc);
  } else if (PLOC_is_nonempty(ploc) && !PLOC_on_stack(ploc)) {
    // don't do if already reached stack params
    ploc = Get_Vararg_Input_Parameter_Location (ploc);
  }

  while (!PLOC_on_stack(ploc)) {

    // vararg registers must be integer registers
    if (Preg_Offset_Is_Float(PLOC_reg(ploc))) {
      if (num_vararg_xmms == 0)
        xmm_save_temp_st = Get_Vararg_Symbol (ploc);
      num_vararg_xmms++;

      ploc = Get_Vararg_Input_Parameter_Location (ploc);
      continue;
    }

    // get the symbol for the vararg formal
    ST	*st;
    st = Get_Vararg_Symbol (ploc);

    // WN *wn;
    // wn = WN_Ldid (Def_Int_Mtype, PLOC_reg(ploc), Int_Preg, ST_type(Int_Preg));
    // wn = WN_Stid (Def_Int_Mtype, 0, st, ST_type(st), wn);
    // WN_Set_Linenum (wn, current_srcpos);
    // WN_INSERT_BlockLast(block, wn);
    TY_IDX  tyidx = ST_type(st);
    TYPE_ID tyid = ST_mtype(st);

    LVTY   *lvty = Wty2llvmty(tyid, tyidx, MTYPE_bit_size(TY_mtype(Ty_Table[tyidx])));
    argstype.push_back(lvty);
    Parm_name().push_back(AGGMAP(ST_name(st), PLOC_reg(ploc), tyidx));

    Is_Trace(Tracing_enabled,
             (TFile, "Collect_vararg_formal_parm creates %s <- PREG%d\n",
              ST_name(st), PLOC_reg(ploc)));

    // get EXT_FLG
    if (info_list) {
      info_list->push_back(Get_ext_flag(lvty, TY_mtype(tyidx)));
    }

    ploc = Get_Vararg_Input_Parameter_Location (ploc);
  }
}

// =============================================================================
// Collect parm type from FUNC_ENTRY's IDNAME list
// Integrate the calling convetion defined by targ_sim.h
// =============================================================================
LVTY*
WHIRL2llvm::Collect_formal_parm_type(LVTYVEC& argstype, WN *func_wn, SIGNVEC *info_list)
{
  TY_IDX putyidx = ST_pu_type(WN_st(func_wn));
  PLOC   ploc = Setup_Input_Parameter_Locations(putyidx);

  LVTY *ret_type =
    Create_ret_type(putyidx, argstype, info_list, NULL);

  INT   argcount = 0;
  WN   *idname_wn = WN_kid(func_wn, argcount);
  while (WN_opcode(idname_wn) == OPC_IDNAME) {
    ploc = Get_Input_Parameter_Location(WN_type(idname_wn));

    ST     *arg_st = WN_st(idname_wn);
    TY_IDX  idx = WN_type(idname_wn);
    bool on_stack = PLOC_on_stack(ploc);
    if (Mload_ty(idx) && !on_stack) {
      Create_mload_formal(argstype, idx, ploc, ST_name(arg_st), info_list);
    }
    else {
      TY_IDX  tyidx = ST_type(arg_st);
      TYPE_ID tyid = ST_mtype(arg_st);
      if (on_stack) {  // synthesize the pointer type
        tyidx = Get_ptrtype(tyidx);
        tyid = TY_mtype(tyidx);
      }
      LVTY   *lvty = Wty2llvmty(tyid, tyidx, MTYPE_bit_size(TY_mtype(Ty_Table[tyidx])));
      argstype.push_back(lvty);
      Parm_name().push_back(AGGMAP(ST_name(arg_st), (on_stack)? ONSTACK : PLOC_reg(ploc), tyidx));

      Is_Trace(Tracing_enabled,
               (TFile, "Collect_formal_parm_type creates %s <- PREG%d\n",
                ST_name(arg_st), (on_stack)? ONSTACK : PLOC_reg(ploc)));

      // get EXT_FLG
      if (info_list) {
        info_list->push_back(Get_ext_flag(lvty, TY_mtype(idx)));
      }
    }

    argcount++;
    idname_wn = WN_kid(func_wn, argcount);
  }

  if (Contain_lowered_vararg(putyidx))
    Collect_vararg_input_parm(ploc, argstype, info_list);

  return ret_type;
}

// =============================================================================
// A twin copy of Collect_formal_parm_type
// =============================================================================
LVTY*
WHIRL2llvm::Create_formal_parm_type(TY_IDX putyidx, LVTYVEC& argstype, TYIDXVEC& tyidxvec, SIGNVEC *info_list)
{
  PLOC ploc = Setup_Input_Parameter_Locations(putyidx);

  LVTY *ret_type = Create_ret_type(putyidx, argstype, info_list, &ploc);

  TYIDXVEC::iterator it;
  for (it = tyidxvec.begin(); it != tyidxvec.end(); ++it) {
    TY_IDX idx = *it;
    ploc = Get_Input_Parameter_Location(idx);
    bool on_stack = PLOC_on_stack(ploc);
    if (Mload_ty(idx) && !on_stack) {
      Create_mload_formal(argstype, idx, ploc, NULL, info_list);
    }
    else {
      TY_IDX tyidx = idx;
      if (on_stack) {  // synthesize the pointer type
        tyidx = Get_ptrtype(idx);
      }
      LVTY *lvty = Wty2llvmty(TY_mtype(tyidx), tyidx, MTYPE_bit_size(TY_mtype(idx)));
      argstype.push_back(lvty);

      // get EXT_FLG
      if (info_list) {
        info_list->push_back(Get_ext_flag(lvty, TY_mtype(idx)));
      }
    }
  } // for tyidxvec iterator
  return ret_type;
}

WN*
WHIRL2llvm::Ref_func_retreg(WN *wn) {
  if (wn == NULL)
    return NULL;
  if (IsCallResReg(wn))
    return wn;
  for (UINT i = 0; i < WN_kid_count(wn); i++) {
    WN *retv = Ref_func_retreg(WN_kid(wn, i));
    if (retv != NULL) // return when found
      return retv;
  }
  return nullptr;
}

// =============================================================================
// Match behavior of wn_lower.cxx/lower_return_ldid 
// =============================================================================
TYPE_ID
WHIRL2llvm::Adjust_return_ldid_mtype(TYPE_ID mtype, PREG_NUM& pregno)
{
  switch (mtype) {
    case MTYPE_I8:
    case MTYPE_U8:
      pregno = First_Int_Preg_Return_Offset;
      return mtype;
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      pregno = First_Int_Preg_Return_Offset;
      return MTYPE_I4;
    case MTYPE_F4:
    case MTYPE_F8:
    case MTYPE_F10:
    case MTYPE_FQ:
    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_C10:
    case MTYPE_CQ:
      pregno = First_Float_Preg_Return_Offset;
      return MTYPE_F8;
    default:
      FmtAssert(FALSE, ("mtype %d not handled.", mtype));
  }
  return MTYPE_UNKNOWN;
}

LVALC*
WHIRL2llvm::Collect_retval_pregname(WN *wn)  {
  WN *retv_wn = WN_next(wn);
  TYALC ret_reg;

  // Handles output from Low WHIRL first, High WHIRL later
  if ((WN_operator(retv_wn) == OPR_STID) && IsPreg(retv_wn)) {
    // create slot for return value
    retv_wn = Ref_func_retreg(retv_wn);
    Is_Trace(Tracing_enabled, (TFile, "Collect_retval_pregname but also Get_preg for : "));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, retv_wn));
    if (retv_wn == nullptr)
      return nullptr;
    ret_reg = Get_preg(retv_wn, TRUE);// WN_desc(WN_kid0(retv_wn)), WN_ty(WN_kid0(retv_wn)));
  }
  else {
    // The WN_next does not have the save return value statement due to
    // forward substitution optimization in earlier phases
    // compose the return preg from the return type of call node
    FmtAssert(OPERATOR_is_call(WN_operator(wn)) || WN_operator(wn) == OPR_INTRINSIC_OP,
              ("Collect_retval_pregname: unexpected operator"));
    TYPE_ID mtype = WN_rtype(wn);
    if ((OPERATOR_is_call(WN_operator(wn)) ||
         WN_operator(wn) == OPR_INTRINSIC_OP) &&
        ! MTYPE_is_m(mtype)) {
      PREG_NUM pregno;
      mtype = Adjust_return_ldid_mtype(mtype, pregno);
      retv_wn = WN_LdidPreg( mtype, pregno);
    }
    else
      retv_wn = Ref_func_retreg(retv_wn);
    if (retv_wn){
      ret_reg = Get_preg(retv_wn, TRUE);
    } else {
      return nullptr;
      // FmtAssert(FALSE, ("Collect_retval_pregname: unexpected Successor of func call %s",
      //                   OPERATOR2name(WN_operator(retv_wn))));
    }
  }
  return ret_reg.second;
}


// =============================================================================
// Get_function_ty create the LLVM::FunctionType out of WN node
// =============================================================================
LVFUNCTY*
WHIRL2llvm::Get_function_ty(WN *wn, SIGNVEC *sign_info)
{
  LVTYVEC  argstype;
  TY_IDX   func_tyidx;
  TY_IDX   ret_tyidx;
  LVTY    *rettype = nullptr;
  WN      *func_wn = NULL;
  BOOL     is_icall = wn != NULL && WN_operator(wn) == OPR_ICALL;
  if (is_icall) {
    func_wn = WN_kid(wn, WN_kid_count(wn)-1);
    func_tyidx = GetFuncType(func_wn);
  } else {
    func_wn = (wn == NULL)? Cur_func() : wn;
    func_tyidx = ST_type(WN_st(func_wn));
  }
  ret_tyidx = TY_ret_type(func_tyidx);

  switch (WN_operator(func_wn)) {
  case OPR_FUNC_ENTRY: {
    rettype = Collect_formal_parm_type(argstype, func_wn, sign_info);
    break;
  }
  case OPR_CALL: {
    For_actual_args(func_wn, this, COLLECT_LVTY(&argstype));
    break;
  }
  default:
    if (is_icall) {
      For_actual_args(wn, this, COLLECT_LVTY(&argstype));
    }
    else {
      FmtAssert(FALSE, ("Get_function_ty does not handle %s yet",
                        OPERATOR2name(WN_operator(func_wn))));
    }
    break;
  }
  // TODO: handle vararg 

  if (rettype == nullptr)
    rettype = Wty2llvmty(TY_mtype(ret_tyidx), ret_tyidx);
  LVFUNCTY *ret = LVFUNCTY::get(rettype, argstype, false);
  return ret;
}

// the following are orignially from targ_sim.h
enum ARGPREG {
  FIRSTINTARG = First_Int_Preg_Param_Offset,
  LASTINTARG  = Last_Int_Preg_Param_Offset,
  MAX_INTARG_CNT = LASTINTARG - FIRSTINTARG + 1,
  FIRSTFLTARG = First_Float_Preg_Param_Offset,
  LASTFLTARG  = Last_Float_Preg_Param_Offset,
};

// Returns the number of actual args
INT32
WHIRL2llvm::Collect_call_actuals(WN *str, RNAITER& actuals)
{
  WN   *cand;
  INT32 i;

  // Traverse up to MAX_ARG_CNT of WN node
  for (i = 0, cand = str; i < MAX_INTARG_CNT; ++i, cand = WN_next(cand)) {
    // if (! Is_int_parm_reg(cand) )
    if (WN_operator(cand) != OPR_STID) // temp fix for stid to preg1
      break;
    actuals->Add_actual(cand);
  }
  Is_True(OPERATOR_is_call(WN_operator(cand)), ("Collect_call_actual, cannot find call"));
  if (OPERATOR_is_call(WN_operator(cand))) {
    actuals->Lowered(cand);
    WN *retv = WN_next(cand);
    if (retv && (WN_operator(retv) == OPR_STID)) // conventionally, it is the store of return value"
      actuals->Lo_retv(retv);
    Is_Trace(Tracing_enabled, (TFile, "Collect_call_actuals:\n"));
    Is_Trace_cmd(Tracing_enabled, actuals->Print(TFile));

    return i;
  }
  return -1;  // failed to find call
}

BOOL
WHIRL2llvm::Is_lowered_actual(WN *stid)
{
  Is_True(WN_operator(stid) == OPR_STID, ("Is_lowered_actual handles STID only"));
  if (stid == NULL)
    return FALSE;
  if (WN_operator(WN_next(stid)) == OPR_RETURN)
    return FALSE;

  WN *ld = WN_kid0(stid);
  OPERATOR opr = WN_operator(ld);
  if (opr == OPR_CVTL || opr == OPR_CVT)  // copyin might have CVT/CVTL
    ld = WN_kid0(ld);

  if (OPERATOR_is_scalar_load(WN_operator(ld))) {

    if (!OPERATOR_is_scalar_store(WN_operator(stid)))
      return FALSE;

    if (!Is_int_parm_reg(stid))
      return FALSE;

    return TRUE;
  }
  return FALSE;
}


BOOL
WHIRL2llvm::Is_float_parm_reg(WN *ldstr)
{
  OPERATOR opr = WN_operator(ldstr);
 
  if ((OPERATOR_is_scalar_load(opr) ||
       OPERATOR_is_scalar_store(opr)) &&
      WN_st(ldstr)->sym_class == CLASS_PREG) {
    INT32 offset = WN_offset(ldstr);
    return (offset >= FIRSTFLTARG && offset <= LASTFLTARG);
  }
  return FALSE;
}

BOOL
WHIRL2llvm::Is_int_parm_reg(WN *ldstr)
{
  OPERATOR opr = WN_operator(ldstr);

  if ((OPERATOR_is_scalar_load(opr) ||
       OPERATOR_is_scalar_store(opr)) &&
      WN_st(ldstr)->sym_class == CLASS_PREG) {
    // TODO: use definition in targ_sim.h
    INT32 offset = WN_offset(ldstr);
    return (offset >= FIRSTINTARG && offset <= LASTINTARG);
  }
  return FALSE;
}

BOOL
WHIRL2llvm::Is_rhs_inparm_ld(WN *stmt)
{
  Is_True(WN_operator(stmt) == OPR_STID,
          ("Is_rhs_inparm_ld takes STID node only"));
  WN *data = WN_kid0(stmt);
  OPERATOR opr = WN_operator(data);
  if (opr == OPR_CVTL || opr == OPR_CVT)  // copyin might have CVT/CVTL
    data = WN_kid0(data);
  
  if (!OPERATOR_is_scalar_load(WN_operator(data)))
    return FALSE;

  // return FALSE if the rhs of current statement is not inparm 
  if (!Is_int_parm_reg(data) && !Is_float_parm_reg(data))
    return FALSE;

  // continue checking for possible saving return value
  WN *prevstmt = WN_prev(stmt);
  if (prevstmt == NULL || WN_operator(prevstmt) == OPR_COMMENT)  // 1st STID in function
    return TRUE;
      
  if (WN_operator(prevstmt) != OPR_STID)
    return FALSE;

  data = WN_kid0(prevstmt);

  opr = WN_operator(data);
  if (opr == OPR_CVTL || opr == OPR_CVT)  // copyin might have CVT/CVTL
    data = WN_kid0(data);

  if (!OPERATOR_is_scalar_load(WN_operator(data)))
    return FALSE;

  // previous statement is not STID with inparm in rhs
  if (Is_int_parm_reg(data) || Is_float_parm_reg(data))
    return TRUE;
  // previous statement might be store inparm preg into formal param
  if (WN_st(data)->sym_class != CLASS_PREG)
    return FALSE;

  if (!Is_preg4parm(WN_offset(data)))
    return FALSE;

  return TRUE;
}

LVVAL*
WHIRL2llvm::Save_to_inparm(WN *wn, const char *varname, LVVAL *rhs, INT parmidx)
{
  LVALC *arg_addr = nullptr;
  std::string reg_name = std::string(varname) + ".addr";

  arg_addr = Get_locvar(reg_name.c_str()).second;
  if (arg_addr == nullptr) {
    FmtAssert(WN_operator(wn) == OPR_STID, ("Save_to_inparm: wn's operator should be STID"));
#if 0
    TY_IDX tyidx = WN_ty(wn);
    // check if this PREG is used to store a field of struct
    if ((TY_kind(tyidx) == KIND_STRUCT) && (WN_field_id(wn) != 0)) {
      UINT cur_field_id = 0;
      FLD_HANDLE fld = FLD_get_to_field(tyidx, WN_field_id(wn), cur_field_id);
      tyidx = FLD_type(fld);
    }
    arg_addr = Create_locvar(WN_desc(wn), tyidx, nullptr, reg_name.c_str()).second;
#else
    const std::string parmname = Cur_lvfunc()->getArg(parmidx)->getName().str();
    BOOL  on_stack = FALSE;
    TY_IDX parmtype = Find_parm_type(parmname.c_str(), &on_stack);
    if (parmtype == MTYPE_UNKNOWN) {
      TY_IDX tyidx  = WN_ty(wn);
      // check if this PREG is used to store a field of struct
      if ((TY_kind(tyidx) == KIND_STRUCT) && (WN_field_id(wn) != 0)) {
        UINT cur_field_id = 0;
        FLD_HANDLE fld = FLD_get_to_field(tyidx, WN_field_id(wn), cur_field_id);
        parmtype = FLD_type(fld);
      }
      else if (TY_kind(tyidx) == KIND_POINTER)
        parmtype = tyidx;
    }
    arg_addr = Create_locvar(WN_desc(wn), parmtype, nullptr, reg_name.c_str()).second;
#endif
  }
  bool is_signed = MTYPE_is_signed(WN_desc(wn));
  Is_Trace(Tracing_enabled, (TFile, "Save_formal_on_stk gen store to %s for,\n", reg_name.c_str()));
  Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
  if (rhs) {
    rhs = HandleStoreDifferentType(wn, rhs, arg_addr->getAllocatedType(), is_signed);
    auto store = Lvbuilder()->CreateStore(rhs, arg_addr);
    store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
    return store;
  } else {
    llvm::StringRef parm_name = llvm::StringRef(varname);
    if (parm_name.startswith(PARM_PREG))
      parm_name = parm_name.drop_front(PARM_PREG.size());
    LVVAL *arg = Get_arg_by_name(parm_name.str().c_str());
    arg = HandleStoreDifferentType(wn, arg, arg_addr->getAllocatedType(), is_signed);
    auto store = Lvbuilder()->CreateStore(arg, arg_addr);
    store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
    return store;
  }
}


// =============================================================================
//
// Function object to be callable for generating variable load
// 
// =============================================================================
LVVAL* 
WHIRL2llvm::WN2llvmSymAct(WN *wn, ACTION act, LVVAL *rhs)
{
  FmtAssert(WN_has_sym(wn), ("WN2llvmSymAct wn doesn't has ST field"));
  ST    *st = WN_st(wn);

  ST_IDX st_idx = WN_st_idx(wn);
  char  *varname = ST_name(st);
  auto   offset = WN_offset(wn);
  TY_IDX ty_idx = ST_type(st);
  OPERATOR opr = WN_operator(wn);

  switch (st->sym_class) {
  case CLASS_BLOCK: // shin 07212022 INFO: itmaybe need to be refined
  case CLASS_VAR: {
    switch (st->storage_class) {
    case SCLASS_COMMON:
    case SCLASS_EXTERN:
    case SCLASS_FSTATIC:
    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL: {
      //if (ST_IDX_level(st_idx) == GLOBAL_SYMTAB) {
      auto gvar = Get_glbvar(varname);
      FmtAssert(gvar != nullptr, ("WN2llvmSymAct: get variable %s failed.", varname));
      switch (act) {
      case ACT_LD: {
        FmtAssert(opr == OPR_LDID, ("WN2llvmSymAct: WN node should be LDID"));
        LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
        if (offset != 0) Gen_displacement(wn, &gvar);
        auto load = Lvbuilder()->CreateLoad(ld_ty, gvar);
        load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
        return load;
      }
      case ACT_LDA: {
        // FmtAssert(offset == 0, ("WN2llvmSymAct: can't handle LDA with offset now"));
        if (WN_operator(wn) == OPR_LDA) {
          Gen_displacement(wn, &gvar);
        }
        return gvar;
      }
      case ACT_STR: {
        if (offset != 0) Gen_displacement(wn, &gvar);
        TYPE_ID desc = WN_desc(wn);
        LVTY *dest_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
        rhs = HandleStoreDifferentType(wn, rhs, dest_ty, MTYPE_is_signed(desc));
        auto store = Lvbuilder()->CreateStore(rhs, gvar);
        store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
        return store;
      }
      default:
        FmtAssert(FALSE, ("WN2llvmSymAct: shouldn't reach here"));
      } // switch act
    }  // end of GLOBAL_SYMTAB
    case SCLASS_FORMAL: {
      LVALC *arg_addr = nullptr;
      varname = Adjust_parm_name(varname, offset);
      BOOL on_stack = FALSE;
      TY_IDX parmtype = Find_parm_type(varname, &on_stack);

      std::string reg_name = std::string(varname) + ".addr";
      switch (act) {
      case ACT_LD:
      case ACT_LDA: {
        bool need_load = false;
        arg_addr = Get_locvar(reg_name.c_str()).second;
        if (arg_addr == NULL) {  // unlikely reachable condition in .O file
          // create a.addr for argument a
          auto lv_arg = Get_arg_by_name(varname);
          if (act == ACT_LD) {
            if (Mload_ty(ty_idx)) {
              arg_addr = Create_locvar(TY_mtype(parmtype), parmtype, nullptr, reg_name.c_str()).second;
            } else {
              arg_addr = Create_locvar(WN_desc(wn), ty_idx, nullptr, reg_name.c_str()).second;
            }
          } else { // ACT_LDA, why different?  Shouldn't only load it or not?
            if (Mload_ty(ty_idx)) {
              need_load = true;
              ty_idx = Make_Pointer_Type(ty_idx);
              arg_addr = Create_locvar(TY_mtype(ty_idx), ty_idx, nullptr, reg_name.c_str()).second;
            } else {
              arg_addr = Create_locvar(TY_mtype(ty_idx), ty_idx, nullptr, reg_name.c_str()).second;
            }
          }
          FmtAssert(arg_addr != nullptr, ("WN2llvmSymAct: create argument on stack failed"));
          // store a to a.addr
          Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct call saveIP, "));
          auto cur_pos = Lvbuilder()->saveIP();
          auto store_pos = _builder->LastAlloc()->getNextNonDebugInstruction();
          if (store_pos == nullptr) {
            Lvbuilder()->SetInsertPoint(Lventry());
          }
          else {
            Lvbuilder()->SetInsertPoint(store_pos);
          }

          LVPRINT(arg_addr, "arg_addr");
          Is_Trace(Tracing_enabled, (TFile, "then store in %s for:\n", reg_name.c_str()));
          lv_arg = HandleStoreDifferentType(wn, lv_arg, arg_addr->getAllocatedType(), MTYPE_is_signed(WN_desc(wn)));
          auto store = Lvbuilder()->CreateStore(lv_arg, arg_addr);
          store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          Lvbuilder()->restoreIP(cur_pos);
        } // create locvar

        Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen load %s for:\n", reg_name.c_str()));
        Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
        if (act == ACT_LDA && !need_load) {
          // caller will have to do adjustment when offset is not zero
          return arg_addr;
        } else {
          if (on_stack) {
            Is_Trace(Tracing_enabled,
                     (TFile, "WN2llvmSymAct gen load for Formal %s passed on stack\n", varname));
          }
          FmtAssert(opr == OPR_LDID, ("WN2llvmSymAct: WN node should be LDID"));
          LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
          auto load = Lvbuilder()->CreateLoad(ld_ty, arg_addr);
          load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          return load;
        }
      } // ACT_LD && ACT_LDA
      case ACT_STR: {
        arg_addr = Get_locvar(reg_name.c_str()).second;
        if (arg_addr == nullptr) {
            if (Mload_ty(ty_idx))
              arg_addr = Create_locvar(TY_mtype(parmtype), parmtype, nullptr, reg_name.c_str()).second;
            else
              arg_addr = Create_locvar(WN_desc(wn), ty_idx, nullptr, reg_name.c_str()).second;
        }
        bool is_signed = MTYPE_is_signed(WN_desc(wn));
        Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen store to %s for,\n", reg_name.c_str()));
        Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
        if (rhs) {
          rhs = HandleStoreDifferentType(wn, rhs, arg_addr->getAllocatedType(), is_signed);
          auto store = Lvbuilder()->CreateStore(rhs, arg_addr);
          store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          return store;
        } else {
          LVVAL *arg = Get_arg_by_name(varname);
          arg = HandleStoreDifferentType(wn, arg, arg_addr->getAllocatedType(), is_signed);
          auto store = Lvbuilder()->CreateStore(arg, arg_addr);
          store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          return store;
        }
      } // case ACT_STR
      } // switch act
    } // end of FORMAL 
    case SCLASS_PSTATIC: {
      std::string name_idx(varname);
      std::string pu_idx = std::to_string(ST_pu(WN_st(Cur_func())));
      name_idx += "_" + pu_idx + "_" + std::to_string(ST_index(st));
      auto gvar = Get_glbvar(name_idx.c_str());
      switch (act) {
        case ACT_LDA: {
          // non-zero offset shall be handled by the caller
          LVVAL *addr = gvar;

          // TODO: Is it necessary to check if the offset is zero?
          if (WN_operator(wn) == OPR_LDA && WN_offset(wn) != 0) {
            Gen_displacement(wn, &addr);
          }
          return addr;
        }
        case ACT_LD: {
          LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
          if (offset != 0) Gen_displacement(wn, &gvar);
          auto load = Lvbuilder()->CreateLoad(ld_ty, gvar);
          load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          return load;
        }
        case ACT_STR: {
          TYPE_ID desc = WN_desc(wn);
          LVTY *dest_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
          if (offset != 0) Gen_displacement(wn, &gvar);
          rhs = HandleStoreDifferentType(wn, rhs, dest_ty, MTYPE_is_signed(WN_desc(wn)));
          auto store = Lvbuilder()->CreateStore(rhs, gvar);
          return store;
        }
      }
      break;
    }
    case SCLASS_AUTO: {
      std::string name_idx(varname);
      /*if (st->storage_class == SCLASS_AUTO)*/
      name_idx += "_" + std::to_string(ST_index(st));
      auto var = Get_locvar(name_idx.c_str());
      FmtAssert(var.second != nullptr, ("WN2llvmSymAct: get variable %s failed.", name_idx.c_str()));
      Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen ld/st %s for:\n", name_idx.c_str()));
      Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
      if (act == ACT_LDA) {
        // non-zero offset shall be handled by the caller
        LVVAL *addr = var.second;

        // TODO: Is it necessary to check if the offset is zero?
        if (WN_operator(wn) == OPR_LDA/* && WN_offset(wn) != 0*/) { // -shin 081222
          Gen_displacement(wn, &addr);
        }

        return addr;
      } else if (act == ACT_LD) {
        LVVAL *addr = var.second;
        if (offset != 0) Gen_displacement(wn, &addr);
        FmtAssert(opr == OPR_LDID, ("WN2llvmSymAct: WN node should be LDID"));
        LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
#if 0
        if (MTYPE_is_m(WN_desc(wn))) {
          ld_ty = Wty2llvmty(WN_desc(wn), WN_ty(wn));
        } else {
          ld_ty = Wty2llvmty(WN_desc(wn), 0);
        }
#endif
        auto load = Lvbuilder()->CreateLoad(ld_ty, addr);
        load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
        return load;
      } else {
        LVVAL *target_addr = var.second;
        Gen_displacement(wn, &target_addr);
        Is_True(rhs != nullptr, ("WN2llvmSymAct: ACT_STR got NULL rhs while lhs is SCLASS_AUTO"));
        TYPE_ID desc = WN_desc(wn);
        LVTY *dest_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
#if 0
        if (MTYPE_is_m(desc)) {
          dest_ty = Wty2llvmty(desc, WN_ty(wn));
        } else {
          dest_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
        }
#endif
        rhs = HandleStoreDifferentType(wn, rhs, dest_ty, MTYPE_is_signed(WN_desc(wn)));
        auto store =  Lvbuilder()->CreateStore(rhs, target_addr);
        store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
        return store;
      }
    } // SCLASS_AUTO
    case SCLASS_EH_REGION:
    case SCLASS_EH_REGION_SUPP:
    default:
      FmtAssert(FALSE, ("WN2llvmSymAct: this SCLASS %d %s not handled", st->storage_class, varname));
    } // st->storage_class
    break;
  } // CLASS_VAR
  case CLASS_PREG: {
    // this preg symbol might be mapped back to inparm !!!
    INT preg = WN_offset(wn);
    if (Is_preg4parm(preg)) {
      INT idx = Get_preg4parm_idx(preg);
      std::string argname = Cur_lvfunc()->getArg(idx)->getName().str();
      std::string preg_name = PARM_PREG + argname;
      Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen parm %s ld/st from %s_%d for:\n",
                                 preg_name.c_str(), varname, preg));
      Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
      switch (act) {
      case ACT_LD:
      case ACT_LDA: {
        std::string reg_name = preg_name + ".addr";
        LVVAL *arg_addr = Get_locvar(reg_name.c_str()).second;
        Is_True(arg_addr != NULL, ("WN2llvmSymAct: generate parm failed"));
        if (act == ACT_LDA)
          return arg_addr;
        else {
          FmtAssert(opr == OPR_LDID, ("WN2llvmSymAct: WN node should be LDID"));
          LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
          auto load = Lvbuilder()->CreateLoad(ld_ty, arg_addr);
          load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
          return load;
        }
      }
      case ACT_STR: {
        return Save_to_inparm(wn, preg_name.c_str(), rhs, idx);
        break;
      }
      } // of act
      break; // of CLASS_PREG
    }
    Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen preg ld/st %s_%d for:\n",
                               varname, WN_offset(wn)));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
    auto reg = Get_preg(wn, TRUE, WN_desc(wn), Be_Type_Tbl(WN_desc(wn)));
    FmtAssert(reg.second != nullptr, ("WN2llvmSymAct: generate preg failed"));
    if (act == ACT_LDA) {
      // TODO: offset
      FmtAssert(offset == 0, ("WN2llvmSymAct: can't handle LDA with offset now"));
      return reg.second;
    } else if (act == ACT_LD) {
      FmtAssert(opr == OPR_LDID, ("WN2llvmSymAct: WN node should be LDID"));
      LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
      auto load = Lvbuilder()->CreateLoad(ld_ty, reg.second);
      load->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
      return load;
    } else { // ACT_STR
      FmtAssert(rhs != nullptr, ("WN2llvmSymAct: rhs shouldn't be nullptr"));
      bool is_signed = MTYPE_is_signed(WN_desc(wn));
      // LVPRINT(rhs, "rhs");
      // LVPRINT(reg.second->getAllocatedType(), "target type");
      auto reg_ty = reg.second->getType();
      rhs = HandleStoreDifferentType(wn, rhs, reg.first, is_signed);
      auto store = Lvbuilder()->CreateStore(rhs, reg.second);
      store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
      return store;
    }
  }
  case CLASS_FUNC: {
    if (act == ACT_LDA) {
      LVFUNC *func = Module()->getFunction(varname);
      FmtAssert(func != nullptr, ("WN2llvmSymAct: get function failed"));
      return func;
    }
  }
  case CLASS_CONST: {
    ST *st = WN_st(wn);
    LVVAL* const_val = nullptr;
    if (const_val = ST2const(st)) {
      return const_val;
    }

    TCON &tcon = Tcon_Table[ST_tcon(st)];
    auto tcon_str_idx = TCON_str_idx(tcon);
    if (tcon_str_idx) {
      const char *str = Index_to_char_array(tcon_str_idx);
      Is_Trace(Tracing_enabled, (TFile, "WN2llvmSymAct gen const string %s for:\n", varname));
      Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
      const_val = Lvbuilder()->CreateGlobalString(str);
      ST2const(st, const_val);
      return const_val;
    }

    FmtAssert(FALSE, ("WN2llvmSymAct: sym_class CLASS_CONST has no llvm symbol"));
  }
  default:
    FmtAssert(FALSE, ("WN2llvmSymAct: can't handle %s of sym_class %d yet",
                      ST_name(st), st->sym_class));
  } // switch st->sym_class
  return nullptr;
}

// =============================================================================
//
// Function object to be callable from the symbol table traversal
//
// =============================================================================
struct ST2llvm {
  WHIRL2llvm *whirl2llvm;
  ST2llvm(WHIRL2llvm *w) : whirl2llvm(w) {}

  void operator() (UINT idx, ST *st) const;

  void SetZeroInitializer(LVGLBVAR* gvar, LVTY *lvty, ST *st) const {
    if (lvty->isIntegerTy()) {
      auto zero = llvm::ConstantInt::get(lvty, 0);
      gvar->setInitializer(zero);
    } else if (lvty->isPointerTy()) {
      auto null_ptr = llvm::ConstantPointerNull::get(llvm::dyn_cast<llvm::PointerType>(lvty));
      gvar->setInitializer(null_ptr);
    } else if (lvty->isAggregateType()) {
      auto zero_aggregate = llvm::ConstantAggregateZero::get(lvty);
      gvar->setInitializer(zero_aggregate);
    } else if (lvty->isFloatingPointTy()) {
      // TODO: handle negative zero 
      auto float_zero = llvm::ConstantFP::get(lvty, 0.0);
      gvar->setInitializer(float_zero);
    } else if (st->sym_class == CLASS_VAR) {
      // TODO: need to it its value to zero
      // un-initialized global resides in bss, hence no need to set to 0
      return;
    }
    else {
      FmtAssert(FALSE, ("ST2llvm::SetZeroInitializer(): this SCLASS %x %s not handled", st->storage_class, ST_name(st)));
    }
  }
};

void ST2llvm::operator() (UINT idx, ST *st) const {
  // skip this st if it has been handled
  if (whirl2llvm->ST2const_or_val(st)) {
    return;
  }

  if (st->storage_class == SCLASS_FORMAL) {
    return;    // Handled by function definition
  }
  if (st->sym_class == CLASS_CONST) {
    TCON &tcon = Tcon_Table[ST_tcon(st)];
    auto tcon_str_idx = TCON_str_idx(tcon);
    if ((TCON_ty(tcon) == MTYPE_STR) && tcon_str_idx) {
      const char *str = Index_to_char_array(tcon_str_idx);
      LVCONST *const_val = llvm::ConstantDataArray::getString(whirl2llvm->Context(), str);
      auto *GV = new llvm::GlobalVariable(*(whirl2llvm->Module()), const_val->getType(), true,
                              llvm::GlobalValue::PrivateLinkage, const_val);
      GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      GV->setAlignment(llvm::Align());
      whirl2llvm->ST2const(st, GV);
    }
    return;
  }

  if (st->sym_class == CLASS_VAR) {
    LVGLBVAR *gvar = nullptr;
    const char *varname = ST_name(st);
    // deal with embedded '.' character in varname
    TYPE_ID tyid = ST_mtype(st);
    TY_IDX tyidx = ST_type(st);
    // the following switch statement should match ST2llvm::operator() coverage
    switch (st->storage_class) {
    case SCLASS_PSTATIC: {
      // static variable in function
      std::string pu_idx = std::to_string(ST_pu(WN_st(whirl2llvm->Cur_func())));
      std::string name = std::string(varname) + "_" + pu_idx + "_" + std::to_string(ST_index(st));
      gvar = whirl2llvm->Create_glbvar(tyid, tyidx, name.c_str(), llvm::GlobalValue::InternalLinkage);
      if (INITV_IDX val = ST_has_initv(st)) {
        gvar->setInitializer(whirl2llvm->INITV2llvm(Initv_Table[val], ST_type(st)));
      } else {
        SetZeroInitializer(gvar, gvar->getType(), st);
      }

      if (ST_is_const_var(st)) {
        gvar->setConstant(true);
      }

      whirl2llvm->ST2lvval(st, gvar);

      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));
      break;
    }
    case SCLASS_AUTO: {
      // handle tmp var duplicate
      // INFO: hardcode for RISC-V va_arg
      TYALC local_var = whirl2llvm->Create_locvar(tyid, tyidx, NULL, varname, ST_index(st));
      whirl2llvm->ST2lvval(st, local_var.second);

      ST *base = ST_base(st);
      if ((base != st) && (ST_ofst(st) == 0)) {
        char *name = ST_name(base);
        if (!whirl2llvm->Has_locvar(base)) {
          TYALC alias = whirl2llvm->Create_locvar(local_var.first, nullptr, name, ST_index(base));
          whirl2llvm->ST2lvval(base, alias.second);
        }
    }

      break;
    }
    case SCLASS_EH_REGION:
    case SCLASS_EH_REGION_SUPP:
      FmtAssert(FALSE, ("ST2llvm::operator(): this SCLASS not handled"));
      break;
    case SCLASS_COMMON: {
      /* Global variable without initialization */
      LVTY *lvty = whirl2llvm->Wty2llvmty(tyid, tyidx);
      gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::CommonLinkage);
      // set init value as 0
      SetZeroInitializer(gvar, lvty, st);

      whirl2llvm->ST2lvval(st, gvar);

      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));
      break;
    }
    case SCLASS_EXTERN: {
      /* Global variable specified with extern */
      gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::ExternalLinkage);
      whirl2llvm->ST2lvval(st, gvar);
      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));
      break;
    }
    case SCLASS_FSTATIC: {
      // file static, treat it as SCLASS_DGLOBAL for now
      if (tyid == MTYPE_UNKNOWN)
        tyid = MTYPE_V;        // hack to side step WN_Lower bug, FIX WN_Lower
      INITV_IDX initv = ST_has_initv(st);
      if (initv) {
        if (INITV_kind(initv) == INITVKIND_LABEL) {
          LVTY *lvty = whirl2llvm->Wty2llvmty(tyid, tyidx);
          FmtAssert(lvty->isArrayTy(), ("ST2llvm: TY_kind(%d) should be array type", TY_kind(tyidx)));

          // convert the int array to ptr array
          lvty = llvm::ArrayType::get(whirl2llvm->GetLVPtrTy(), lvty->getArrayNumElements());

          gvar = whirl2llvm->Create_glbvar(lvty, varname, llvm::GlobalValue::InternalLinkage);
        } else {
          gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::InternalLinkage);
        }
      } else {
        gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::InternalLinkage);
        LVTY *lvty = gvar->getValueType();
        SetZeroInitializer(gvar, lvty, st);
      }

      if (ST_is_const_var(st)) {
        gvar->setConstant(true);
      }

      whirl2llvm->ST2lvval(st, gvar);

      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));
      break;
    }
    case SCLASS_UGLOBAL: {
      gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::ExternalLinkage);
      whirl2llvm->ST2lvval(st, gvar);
      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));

      LVTY *lvty = whirl2llvm->Wty2llvmty(tyid, tyidx);
      SetZeroInitializer(gvar, lvty, st);
      break;
    }
    case SCLASS_DGLOBAL: {
      /* Global variable with initialization */
      // TODO: set init value `g_var->setInitializer(llvm::Constant *)
      gvar = whirl2llvm->Create_glbvar(tyid, tyidx, varname, llvm::GlobalValue::ExternalLinkage);
      gvar->setDSOLocal(true);
      if (ST_is_const_var(st)) {
        gvar->setConstant(true);
      }

      whirl2llvm->ST2lvval(st, gvar);
      // set alignment
      gvar->setAlignment(llvm::MaybeAlign(TY_align(tyidx)));
      break;
    }
    default:
      FmtAssert(FALSE, ("ST2llvm::operator(): this SCLASS %x [%d] %s not handled", st->storage_class, idx, varname));
      break;
    }

    // create alias of the var
    // TODO: refactor this piece of code
    ST *base = ST_base(st);
    if (gvar && (base != st) && (ST_ofst(st) == 0)) {
      char *name = ST_name(base);

      // ignore section names
      if (base->storage_class == SCLASS_UNKNOWN) return;

      if (whirl2llvm->Module()->getGlobalVariable(name) == nullptr) {
        LVGLBVAR *alias = whirl2llvm->Create_glbvar(gvar->getValueType(), name, gvar->getLinkage());
        SetZeroInitializer(alias, alias->getValueType(), base);
        whirl2llvm->ST2lvval(base, alias);
      }
    }

    if (ST_is_const_var(*st) || TY_is_const(ST_type(st))) {
      // TODO, what todo?
    }
  } else if (st->sym_class == CLASS_FUNC) {
    if (ST_is_not_used(st))
      return;

    // TODO: nested function/procedure, forward declaration
    // create extern function
    const char *funcname = ST_name(st);
    TY_IDX tyidx = ST_type(st);

    // if (!TY_has_prototype(ST_pu_type(st))) return; // cannot continue -Shin 13May2022

    // create return type
    auto ret_idx = TY_ret_type(tyidx);
    LVTY *ret_type;

    // create argument type
    LVTYVEC  args_ty;
    SIGNVEC  is_signed_attrs;
    TYIDXVEC tyidxvec;

    if ((whirl2llvm->Testing_mode() & TESTFORMAL) != TESTDEFAULT) {
      auto indx = TY_parms(tyidx);
      while (Tylist_Table[indx] != 0)  {
        TY_IDX arg_ty_idx = Tylist_Table[indx];
        auto arg_ty = whirl2llvm->Wty2llvmty(TY_mtype(arg_ty_idx), arg_ty_idx);
        tyidxvec.push_back(arg_ty_idx);
        indx++;
      }
      ret_type = whirl2llvm->Create_formal_parm_type(tyidx, args_ty, tyidxvec, &is_signed_attrs);
    }
    else {
      ret_type = whirl2llvm->Wty2llvmty(TY_mtype(ret_idx), ret_idx);
      auto arg_idx = TY_parms(tyidx);
      while (Tylist_Table[arg_idx] != 0)  {
        TY_IDX arg_ty_idx = Tylist_Table[arg_idx];
        TYPE_ID arg_mtype = TY_mtype(arg_ty_idx);
        auto arg_ty = whirl2llvm->Wty2llvmty(arg_mtype, arg_ty_idx);
        args_ty.push_back(arg_ty);
        if (arg_ty->isIntegerTy()) {
          if (MTYPE_is_signed(arg_mtype)) {
            is_signed_attrs.push_back(EXT_FLG::SEXT);
          } else {
            is_signed_attrs.push_back(EXT_FLG::ZEXT);
          }
        } else {
          is_signed_attrs.push_back(EXT_FLG::NONE);
        }
        arg_idx++;
      }
    }

    auto func_ty = llvm::FunctionType::get(ret_type, args_ty, TY_is_varargs(tyidx));
    auto link = whirl2llvm->GetFuncLinkageType(st);

    auto func = llvm::Function::Create(func_ty, link, funcname, whirl2llvm->Module());

    // set calling convention
    if (PU_src_lang(ST_pu(st)) & PU_CXX_LANG ||
      PU_src_lang(ST_pu(st)) & PU_C_LANG) {
      func->setCallingConv(llvm::CallingConv::C);
    }
    FmtAssert(is_signed_attrs.size() == args_ty.size(),
              ("Calling Set_func_attr:idx(%d) function(%s) is_signed_attrs.size() != argstype.size()",
               tyidx, funcname));
    whirl2llvm->Set_func_attr(tyidx, func, &is_signed_attrs);

    // handle global ctors
    if (ST_is_glb_init_func(st)) {
      LVCONST *priority = whirl2llvm->Get_glb_init_priority(std::string(funcname));
      auto elem_ty = whirl2llvm->Get_glb_ctors_elem_ty();

      // { priority, init funciton, target object }
      LVCONSTVEC vals = {
        priority,
        func,
        llvm::ConstantPointerNull::get(whirl2llvm->GetLVPtrTy())
      };

      // we put all these entries into llvm.global_ctors later
      auto val = llvm::ConstantStruct::get(elem_ty, vals);
      whirl2llvm->Add_glb_init_info(val);
    }
  }
  else {
    // FmtAssert(FALSE, ("ST2llvm::operator(), NYI"));
  }
}


// =============================================================================
//
//
// =============================================================================
TYALC WHIRL2llvm::Get_preg(WN *wn, BOOL is_load, TYPE_ID mtype, TY_IDX ty_idx) {
  FmtAssert(IsPreg(wn), ("Get_preg: wn is not PREG"));

  std::string reg = GetRegName(wn);
  auto res = Get_locvar(reg.c_str());
  if (res.second) {
    Is_Trace(Tracing_enabled, (TFile, "Get_preg found %s for:\n", reg.c_str()));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
    return res;
  }
  if (!is_load && IsCallResReg(wn)) {
    TY_IDX ret_tyidx = TY_ret_type(ST_type(WN_st(Cur_func()))); // Cur_func() is def, not call
    auto tyid = TY_mtype(ret_tyidx);

    // unpack struct field
    if (MTYPE_is_m(tyid)) {
      ret_tyidx = WN_ty(wn);
      tyid = TY_mtype(ret_tyidx);
    }

    auto res1 = Create_locvar(tyid, ret_tyidx, NULL, reg.c_str());
    Is_Trace(Tracing_enabled, (TFile, "Get_preg gen retreg %s of %s type for storing:\n", Mtype_Name(tyid), reg.c_str()));
    Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
    RetVal(res1.second);
    return res1;
  }
  TYPE_ID tyid = WN_desc(wn);
  auto res1 = Create_locvar(((mtype == 0) ? tyid : mtype), ((ty_idx == 0) ? WN_ty(wn) : ty_idx), NULL, reg.c_str());

  FmtAssert(res1.second != nullptr, ("Get_preg: generate pseudo register failed"));
  Is_Trace(Tracing_enabled, (TFile, "Get_preg gen preg %s of %s type for loading:\n", reg.c_str(), Mtype_Name((mtype == 0) ? tyid : mtype)));
  Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
  return res1;
}

BOOL
WHIRL2llvm::Has_volatile_expr(WN *wn)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_has_1ty(opcode)) {
    if ( (WN_ty(wn) != (TY_IDX) 0 ) &&
         TY_is_volatile(WN_ty(wn)) )
      return TRUE;
  } else if (OPCODE_has_2ty(opcode)) {
    if ( (WN_ty(wn) != (TY_IDX) 0 ) &&
         TY_is_volatile(WN_ty(wn)) )
      return TRUE;
    if ( (WN_load_addr_ty(wn) != (TY_IDX) 0) &&
         TY_is_volatile(WN_load_addr_ty(wn)) )
      return TRUE;
  }

  if (!OPERATOR_is_leaf(WN_operator(wn))) {
    for (UINT i = 0; i < WN_kid_count(wn); i++) {
      WN *kid = WN_kid(wn, i);
      if (Has_volatile_expr(kid))
        return TRUE;
    }
  }

  return FALSE;
}

typedef struct FieldInfo {
  INT32       _field_id;
  INT32       _new_offset;
  FLD_HANDLE  _fld_handle;

  FieldInfo() : _field_id(0), _new_offset(-1), _fld_handle(FLD_HANDLE()) {}
  FieldInfo(UINT32 fld_id, FLD_HANDLE fld) :
    _field_id(fld_id), _new_offset(-1), _fld_handle(fld) {}
  INT32       Field_id(void)          { return _field_id;   }
  void        Field_id(INT32 fid)     { _field_id = fid;    }
  INT32       New_offset(void)        { return _new_offset; }
  void        New_offset(INT32 nofst) { _new_offset = nofst;}
  FLD_HANDLE& Fld_handle(void)        { return _fld_handle; }
  void        Fld_handle(FLD_HANDLE f){ _fld_handle = f;    }
} FLDINFO;

FLDINFO GetFieldId(WN *wn, TY_IDX struct_ty_idx, WN_OFFSET offset) {
  FieldInfo fld_info;
  if (TY_kind(struct_ty_idx) != KIND_STRUCT) return fld_info;

  auto struct_ty = Ty_Table[struct_ty_idx];
  FLD_HANDLE fld = TY_fld(struct_ty);
  UINT64 field_id = 1;

  // while (offset < 0) {
  //   offset += TY_size(struct_ty_idx);
  // }

  bool found = false;
  while (!fld.Is_Null()) {
    auto ofst = FLD_ofst(fld);
    if (ofst == offset) {
      found = true;
      fld_info.New_offset(0);
      break;
    } else if ((!FLD_next(fld).Is_Null() &&
                (FLD_ofst(FLD_next(fld)) > offset) && (ofst < offset)) ||
               (FLD_next(fld).Is_Null() &&
                (TY_size(FLD_type(fld)) + ofst) > offset && (ofst < offset))){
      /*
       * handle nested array and struct
       *  struct A{
       *    ...
       *    struct B {
       *      ...
       *      int res[4];
       *    } b;
       *  } a;
       *  
       *  a.b.res[1] = 3;
       */
      found = true;
      fld_info.New_offset( offset - ofst );

      TY_KIND ty_kind = TY_kind(FLD_type(fld));
      FmtAssert((ty_kind== KIND_STRUCT) || (ty_kind == KIND_ARRAY), 
        ("GetFieldId: this field should be a struct or an array"));
      break;
    } else {
      field_id++;
      fld = FLD_next(fld);
    }
  }
  if (found) {
    fld_info.Field_id(field_id);
    fld_info.Fld_handle(fld);

    Is_Trace(Tracing_enabled, (TFile, "Get field_id=%lld for : ", field_id));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
  } else {
    // can't found this field 
    // struct A { int arr[10]; }; int *b = A.arr[3];
    field_id = 0;
    Is_Trace(Tracing_enabled, (TFile, "Get field_id failed"));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));
  }

  return fld_info;
}

BOOL
WHIRL2llvm::Gen_displacement(WN *wn, LVVAL **base) {
  FmtAssert((*base)->getType()->isOpaquePointerTy(), 
    ("Gen_displacement: Type of the base should be OpaquePointerTy"));

  INT offset = WN_offset(wn);
  LVTY *i8_ty = Lvbuilder()->	getInt8Ty();

  // tmp = (i8*)(*base) + offset
  auto target_addr = Lvbuilder()->CreateGEP(i8_ty, *base, Lvbuilder()->getInt64(offset));
  LVPRINT(target_addr, "target_addr");

  *base = target_addr;
  return TRUE;
}

// =============================================================================
//
// EXPR2llvm generates LLVM instruction for a WHIRL expression
//
// =============================================================================
LVVAL *WHIRL2llvm::EXPR2llvm(WN *wn, WN *parent) {
  LVVAL *lhs, *rhs;
  OPERATOR opr = WN_operator(wn);
  ST *st = nullptr;
  if (WN_has_sym(wn)) st = WN_st(wn);

  Is_Trace(Tracing_enabled, (TFile, "Enter EXPR2llvm : "));
  Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));

  LVVAL *res = nullptr;

  switch (opr) {
  case OPR_INTCONST: {
    res =  llvm::ConstantInt::get(Wty2llvmty(WN_rtype(wn), WN_ty(wn)),
                                  WN_const_val(wn));
    break;
  }
  case OPR_PARM: {
    auto parm = EXPR2llvm(WN_kid0(wn), wn);
    auto real_ty = Wty2llvmty(WN_rtype(wn), WN_ty(wn));
    res =  parm;
    break;
  }
  case OPR_CONST: {
    // handle floating point and string constant values
    // floating point const, WHIRL use TCON to preserve its value
    FmtAssert(ST_sym_class(st) == CLASS_CONST, ("EXPR2llvm: OPR_CONST with non-const ST"));
    TCON &tcon = Tcon_Table[st->u1.tcon];
    TYPE_ID tyid = TCON_ty(tcon);
    LVTY *lvtype = Wty2llvmty(tyid, MTYPE_To_TY(tyid));
    FmtAssert(lvtype != nullptr, ("EXPR2llvm: get llvm type failed"));
    if (lvtype->isFloatTy()) {
      LVVAL *fp_const = llvm::ConstantFP::get(lvtype, tcon.vals.fval);
      res = fp_const;
    } else if (lvtype->isDoubleTy()) {
      LVVAL *db_const = llvm::ConstantFP::get(lvtype, tcon.vals.dval);
      res = db_const;
    } else {
      // TODO: handle string const
      // ConststrNode *conststrNode = fn->codeMemPool->New<ConststrNode>(PTY_ptr, static_cast<MIRStrConst *>(mirconst)->value);
      // return conststrNode;
      FmtAssert(FALSE, ("EXPR2llvm: can't handle string now"));
    }
    break;
  }
  case OPR_LDA: {
    res = WN2llvmSymAct(wn, ACT_LDA);
    break;
  }
  case OPR_LDA_LABEL: {
    INT32  labnum = WN_label_number(wn);
    char*  labname = LABEL_NUMBER2name(labnum);
    W2LBB* targbb = Get_w2lbb(labname);
    
    res = targbb->Blkaddr();
    break;
  }
  case OPR_LDID: {
    auto rtype = WN_rtype(wn);
    LVTY *lv_rtype;
    if (WN_opcode(wn) == OPC_MMLDID) {
      lv_rtype = Wty2llvmty(rtype, WN_ty(wn));
    }
    else {
      lv_rtype = Wty2llvmty(rtype, 0);
    }

      LVVAL *val = WN2llvmSymAct(wn, ACT_LD);
      res = HandleLoadImplicitCast(wn, val, lv_rtype, MTYPE_is_signed(WN_desc(wn)));
    break;
  }
  case OPR_LDBITS: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_MLOAD: {
    Is_True(parent != NULL, ("EXPR2llvm, must pass in parent for OPR_MLOAD"));
    auto rtype = WN_rtype(parent);
    auto lv_rtype = Wty2llvmty(rtype, WN_ty(parent));

    // 1. Load the base address of the ILOAD and make sure that the type of
    //    the loaded value is a pointer type.
    LVVAL *base    = EXPR2llvm(WN_kid0(wn));   // load up the base address
    LVTY  *base_ty = base->getType();

    // 2. Handling displacement of struct's field or array's index
    INT offset = WN_offset(wn);            // collect the offset

    if (offset != 0) {
      FmtAssert(Gen_displacement(wn, &base), ("EXPR2llvm, calling Gen_displacement failed"));
    }

    // 3. Create a load instruction to perform the actual iload
    LVTY *ld_ty = Wty2llvmty(WN_desc(wn), 0);
    auto val = Lvbuilder()->CreateLoad(ld_ty, base, ST_name(WN_st(WN_kid0(wn))));
    val->setAlignment(llvm::Align(TY_align(WN_ty(wn))));

    res = HandleLoadImplicitCast(wn, val, lv_rtype, MTYPE_is_signed(WN_desc(wn)));
    break;
  }
  case OPR_ILOAD: {
    TYPE_ID desc = WN_desc(wn);
    TYPE_ID rtype = WN_rtype(wn);
    LVTY *lv_rtype = nullptr;
    INT offset = WN_offset(wn);            // collect the offset
    lv_rtype = Wty2llvmty(rtype, MTYPE_To_TY(rtype));

    // 1. Load the base address of the ILOAD and make sure that the type of
    //    the loaded value is a pointer type.
    LVVAL *base   = EXPR2llvm(WN_kid0(wn));   // load up the base address
    // LVPRINT(base, "base");
    auto   wn_base_ty = Wty2llvmty(Pointer_type, WN_load_addr_ty(wn));
    LVTY  *base_ty = base->getType();
    // convert int to pointer
    if (base_ty->isIntegerTy()) {
      HandlePointerAndIntegerType(wn, &base, wn_base_ty, true);
      base_ty = base->getType();
    }

    if (offset != 0) {
      FmtAssert(Gen_displacement(wn, &base), ("EXPR2llvm, calling Gen_displacement failed"));
    }

    // LVPRINT(base, "iload base");
    // 3. Create a load instruction to perform the actual iload
    LVTY *ld_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
    auto val = Lvbuilder()->CreateLoad(ld_ty, base);
    val->setAlignment(llvm::Align(TY_align(WN_ty(wn))));

    res = HandleLoadImplicitCast(wn, val, lv_rtype, MTYPE_is_signed(WN_desc(wn)));
    break;
  }
  case OPR_NEG:
  case OPR_ABS:
  case OPR_SQRT:
  case OPR_BNOT:
  case OPR_LNOT:
  case OPR_ALLOCA: {
    res = CreateUnary(wn);
    break;
  }
  case OPR_RECIP: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_ILDA: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_CVT:   {
    LVVAL *opnd = EXPR2llvm(WN_kid0(wn));
    TYPE_ID mtype = WN_rtype(wn);
    TYPE_ID dtype = WN_desc(wn);
    LVTY *resty = Wty2llvmty(mtype, WN_ty(wn));
    
    if (MTYPE_is_float(dtype)) {
      if (resty->isIntegerTy()) {
        if (MTYPE_is_signed(mtype)) {
          opnd = Lvbuilder()->CreateFPToSI(opnd, resty);
        } else {
          opnd = Lvbuilder()->CreateFPToUI(opnd, resty);
        }
      } else if (resty->isFloatingPointTy()) {
        if (MTYPE_bit_size(mtype) > MTYPE_bit_size(dtype))
          opnd = Lvbuilder()->CreateFPExt(opnd, resty);
        else
          opnd = Lvbuilder()->CreateFPTrunc(opnd, resty);
      } else {
        FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, OPR_CVT of MTYPE(%d) not handled", dtype));
      }
    }
    else {
      if (resty->isIntegerTy()) {
        if (opnd->getType()->isIntegerTy()) {
          // integral type, if sizes are the same, it is a no-op
          auto opnd_width = opnd->getType()->getIntegerBitWidth();
          auto res_width  = resty->getIntegerBitWidth();
          if (opnd_width < res_width) {
            // sext/zext
            opnd = CreateExt(opnd, resty, MTYPE_is_signed(mtype));
          } else {
            // truncate
            opnd = CreateTrunc(opnd, resty);
          }
        } else if (opnd->getType()->isPointerTy()) {
          opnd = Lvbuilder()->CreatePtrToInt(opnd, resty);
        } else {
          FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm: OPR_CVT for MTYPE(%d) is not handled", dtype));
        }
      } else if (resty->isFloatingPointTy()) {
        if (opnd->getType()->isIntegerTy()) {
          if (MTYPE_is_signed(WN_desc(wn))) {
            opnd = Lvbuilder()->CreateSIToFP(opnd, resty);
          } else {
            opnd = Lvbuilder()->CreateUIToFP(opnd, resty);
          }
        } else {
          FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm: OPR_CVT for MTYPE(%d) NYI", dtype));
        }
      } else {
          FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm: OPR_CVT for MTYPE(%d) is not handled", mtype));
      }
    }
    res = opnd;
    break;
  }
  case OPR_RND:   {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_CEIL:  {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_FLOOR: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_TRUNC: {
    LVVAL *opnd = EXPR2llvm(WN_kid0(wn));
    TYPE_ID rtype = WN_rtype(wn);
    LVTY *res_ty = Wty2llvmty(rtype, MTYPE_To_TY(rtype));
    FmtAssert(res_ty->isIntegerTy(), ("WHIRL2llvm::EXPR2llvm, OPR_TRUNC of non-integer type"));

    if (MTYPE_is_signed(rtype)) {
      res = Lvbuilder()->CreateFPToSI(opnd, res_ty);
    } else {
      res = Lvbuilder()->CreateFPToUI(opnd, res_ty);
    }
    break;
  }
  case OPR_CVTL: {
    LVVAL  *opnd = EXPR2llvm(WN_kid0(wn));
    TYPE_ID mtype = WN_rtype(wn);
    TYPE_ID dtype = WN_desc(wn);
    LVTY   *resty = Wty2llvmty(mtype, WN_ty(wn));

    auto opnd_type = opnd->getType();
    Is_True(opnd_type->isIntegerTy(), ("EXPR2llvm: expect Integer Type for operand of CVTL"));
    auto opnd_width = opnd->getType()->getIntegerBitWidth();
    auto res_width = resty->getIntegerBitWidth();

    if (MTYPE_is_float(dtype)) {
      FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, OPR_CVT of float not handled"));
    }
    else {
      if (resty->isIntegerTy()) {
        if (opnd->getType()->isIntegerTy()) {
          // integral type, if sizes are the same, it is a no-op
          auto opnd_width = opnd->getType()->getIntegerBitWidth();
          auto res_width  = resty->getIntegerBitWidth();
          if (opnd_width < res_width) {
            // sext/zext
            opnd = CreateExt(opnd, resty, MTYPE_is_signed(mtype));
          } else {
            // truncate
            opnd = CreateTrunc(opnd, resty);
          }
        } else if (opnd->getType()->isPointerTy()) {
          opnd = Lvbuilder()->CreatePtrToInt(opnd, resty);
        }
      }
    }
    res = opnd;
    break;
  }
  case OPR_TAS: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_EXTRACT_BITS: {
    auto b_size = WN_bit_size(wn);
    auto b_offset = WN_bit_offset(wn);
    auto sub_expr = EXPR2llvm(WN_kid0(wn));
    if (b_offset > 0) {
      sub_expr = Lvbuilder()->CreateAShr(sub_expr, 
        llvm::ConstantInt::get(sub_expr->getType(), b_offset));
    }
    uint64_t mask = (1ULL << b_size) - 1;
    auto size = llvm::ConstantInt::get(sub_expr->getType(), mask);
    res = Lvbuilder()->CreateAnd(sub_expr, size);
    break;
  }
  // binary ops starts
  case OPR_SUB: {
    Create_bin_operands(wn, &lhs, &rhs, TRUE);
    if (lhs->getType()->isFloatingPointTy()) {
      res = Lvbuilder()->CreateFSub(lhs, rhs);
    } else if (lhs->getType()->isIntegerTy()) {
      res = Lvbuilder()->CreateSub(lhs, rhs);
    } else if (lhs->getType()->isPointerTy()) {
      // the two operands maybe pointer type after strength reduction
      lhs = Lvbuilder()->CreatePtrToInt(lhs, Lvbuilder()->getInt64Ty());
      rhs = Lvbuilder()->CreatePtrToInt(rhs, Lvbuilder()->getInt64Ty());
      res = Lvbuilder()->CreateSub(lhs, rhs);
    }
    break;
  }
  case OPR_MPY:{
    TYPE_ID resid = WN_rtype(wn);

    Create_bin_operands(wn, &lhs, &rhs, TRUE);
    if (MTYPE_is_integral(resid)) {
      res = Lvbuilder()->CreateMul(lhs, rhs);
    } else if (MTYPE_is_float(resid)) {
      res =  Lvbuilder()->CreateFMul(lhs, rhs);
    } else {      
      FmtAssert(FALSE, ("EXPR2llvm: can't handle this type for OPR_MPY"));
    }
    break;
  } 
  case OPR_DIV: {
    LVVAL *div = nullptr;
    TYPE_ID resid = WN_rtype(wn);
    Create_bin_operands(wn, &lhs, &rhs, TRUE);
    if (MTYPE_is_integral(resid)) {
      // use sdiv/udiv
      if (MTYPE_is_signed(resid)) {
        div = Lvbuilder()->CreateSDiv(lhs, rhs);
      } else {
        FmtAssert(MTYPE_is_unsigned(resid) , ("EXPR2llvm: type of div is not unsigned type"));
        div = Lvbuilder()->CreateUDiv(lhs, rhs);
      }
    } else if (MTYPE_is_float(resid)) {
      // use fdiv
      div = Lvbuilder()->CreateFDiv(lhs, rhs);
    }
    FmtAssert(div != nullptr, ("EXPR2llvm: create div instruction failed"));
    res = div;
    break;
  }
  case OPR_REM: {
    LVVAL *rem = nullptr;
    TYPE_ID resid = WN_rtype(wn);
    Create_bin_operands(wn, &lhs, &rhs, TRUE);
    if (MTYPE_is_integral(resid)) {
      // use srem/urem
      if (MTYPE_is_signed(resid)) {
        rem = Lvbuilder()->CreateSRem(lhs, rhs);
      } else {
        rem = Lvbuilder()->CreateURem(lhs, rhs);
      }
    } else if (MTYPE_is_float(resid)) {
      // use frem
      rem = Lvbuilder()->CreateFRem(lhs, rhs);
    }
    FmtAssert(rem != nullptr, ("EXPR2llvm: create rem instruction failed"));
    res = rem;
    break;
  }
  case OPR_LAND:
  case OPR_LIOR:
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  case OPR_MIN:
  case OPR_MAX:  
  case OPR_BAND:
  case OPR_BIOR:
  case OPR_BXOR:
  case OPR_SHL:
  case OPR_ASHR:
  case OPR_LSHR:
  case OPR_ADD: {
    res = CreateBinary(wn);
    break;
  }
  case OPR_COMPOSE_BITS: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_EQ:
  case OPR_NE:
  case OPR_LT:
  case OPR_GT:
  case OPR_LE:
  case OPR_GE: {
    res = CreateCompare(wn, lhs, rhs);
    break;
  }
  case OPR_RROTATE: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_COMMA: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_CAND: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_CIOR: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_SELECT: { 
    LVVAL *cond = EXPR2llvm(WN_kid0(wn));
    LVVAL *true_val = EXPR2llvm(WN_kid1(wn));
    LVVAL *false_val = EXPR2llvm(WN_kid2(wn));
    res = Lvbuilder()->CreateSelect(cond, true_val, false_val);
    break;
  }
  case OPR_CSELECT: {  // generate if-then-else
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_ARRAY: {
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_INTRINSIC_OP: {
    switch (WN_intrinsic(wn)) {
      case INTRN_EXPECT: {
        res = EXPR2llvm(WN_kid0(WN_kid0(wn)));
        break;
      }
      case INTRN_F8SIN:
      case INTRN_F8COS:
      case INTRN_F8EXP: {  // turn it into OPR_CALL with minimum set of actions
        res = Handle_intrn_call(wn);
        break;
      }
      default: {
        FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
      }
    }
    break;
  }
  default:
    FmtAssert(FALSE, ("WHIRL2llvm::EXPR2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  FmtAssert(res != nullptr, ("EXPR2llvm: generate expr failed"));

  // add debug info

  return res;
}

BOOL
WHIRL2llvm::Is_preg4parm(WN *data)
{
  BOOL its_preg4parm = FALSE;
  if (WN_operator(data) == OPR_LDID) {
    ST *rhs_st = WN_st(data);
    if (rhs_st->sym_class == CLASS_PREG)
      its_preg4parm = Is_preg4parm(WN_offset(data));
  }
  return its_preg4parm;
}

WN*
WHIRL2llvm::STMT2llvm(WN *wn, W2LBB *lvbb)
{
  OPERATOR opr = WN_operator(wn);
  Is_Trace(Tracing_enabled, (TFile, "Enter STMT2llvm : "));
  Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
  switch (opr) {
  case OPR_LABEL: {
    // set insert point
    char *labname = LABEL_NUMBER2name(WN_label_number(wn));
    W2LBB *bb = Get_w2lbb(labname);

    // connect to block
    auto cur_block = Lvbuilder()->GetInsertBlock();
    if (cur_block->empty()) {
      CreateBr(bb);
    } else {
      auto last_inst = &(cur_block->back());
      if (!llvm::isa<llvm::BranchInst>(last_inst) &&
          !llvm::isa<llvm::ReturnInst>(last_inst) &&
          !llvm::isa<llvm::SwitchInst>(last_inst) &&
          !llvm::isa<llvm::IndirectBrInst>(last_inst)) {
        CreateBr(bb);
      }
    }

    Lvbuilder()->SetInsertPoint(bb->Lvbb());
    break;
  }
  case OPR_GOTO: {
    INT32  labnum = WN_label_number(wn);
    char*  labname = LABEL_NUMBER2name(labnum);
    W2LBB* targbb = Get_w2lbb(labname);
    CreateBr(targbb);
    break;
  }
  case OPR_COMPGOTO: {
    auto num_entries = WN_num_entries(wn);
    auto cond = EXPR2llvm(WN_switch_test(wn));
    auto block = WN_switch_table(wn);
    auto default_goto = WN_switch_default(wn);
    FmtAssert(default_goto != nullptr, ("WHIRL2llvm::STMT2llvm: default block is nullptr"));
    auto default_label = LABEL_NUMBER2name(WN_label_number(default_goto));
    W2LBB* default_blk = Get_w2lbb(default_label);

    unsigned bits = cond->getType()->getIntegerBitWidth();;
    auto switch_inst = Lvbuilder()->CreateSwitch(cond, default_blk->Lvbb(), num_entries);
    auto it = WN_first(block);
    for (auto i = 0; i < num_entries; i++) {
      INT32  labnum = WN_label_number(it);
      char*  labname = LABEL_NUMBER2name(labnum);
      W2LBB* targbb = Get_w2lbb(labname);
      switch_inst->addCase(Lvbuilder()->getIntN(bits, i), targbb->Lvbb());
      it = WN_next(it);
    }
    break;
  }
  case OPR_XGOTO: {
    INT32 num_entries = WN_num_entries(wn);
    
    // create offset of jump table
    LVVAL *offset = EXPR2llvm(WN_kid0(wn));
    FmtAssert(offset->getType()->isIntegerTy(), 
      ("WHIRL2llvm::STMT2llvm: offset type is not integer"));

    WN *block = WN_switch_table(wn);

    // this is an FSTATIC variable
    ST *jmp_table_st = WN_st(wn);
    LVVAL *table = Get_glbvar(ST_name(jmp_table_st));
    FmtAssert(table != nullptr, ("WHIRL2llvm::STMT2llvm: jmp_table is nullptr"));
    LVGLBVAR *jmp_table = llvm::dyn_cast<LVGLBVAR>(table);
    FmtAssert(jmp_table != nullptr, ("WHIRL2llvm::STMT2llvm: jmp_table is not a GlobalVariable"));

    // caculate the target address of XGOTO
    // 1. add offset to base_addr
    std::vector<LVVAL *> idxs {
      Lvbuilder()->getInt32(0),
      offset
    };
    LVPRINT(jmp_table, "jmp_table");
    LVVAL *tmp = Lvbuilder()->CreateGEP(jmp_table->getValueType(), jmp_table, idxs);

    // 2. load jmp index
    auto cond = Lvbuilder()->CreateLoad(GetLVPtrTy(), tmp);

    // get target blocks
    auto indirectbr = Lvbuilder()->CreateIndirectBr(cond, num_entries);
    auto it = WN_first(block);
    for (auto i = 0; i < num_entries; i++) {
      INT32  labnum = WN_label_number(it);
      char *labname = LABEL_NUMBER2name(labnum);
      W2LBB *targbb = Get_w2lbb(labname);
      indirectbr->addDestination(targbb->Lvbb());
      it = WN_next(it);
    }
    break;
  }
  case OPR_FALSEBR:
  case OPR_TRUEBR: {
    // process compare expression
    LVVAL *cond = EXPR2llvm(WN_if_test(wn));
    // prepare the branch targets
    W2LBB* fallthrubb = Create_w2lbb(LABEL_NUMBER2name(0));
    INT32  labnum = WN_label_number(wn);
    char*  labname = LABEL_NUMBER2name(labnum);
    W2LBB* targbb = Get_w2lbb(labname);

    // convert cond to i1
    cond = CreateTrunc(cond, LVTY::getInt1Ty(Context()));

    // Create branch operation
    if (opr == OPR_TRUEBR) {
      Lvbuilder()->CreateCondBr(cond, targbb->Lvbb(), fallthrubb->Lvbb());
    } else {
      Lvbuilder()->CreateCondBr(cond, fallthrubb->Lvbb(), targbb->Lvbb());
    }
    Lvbuilder()->SetInsertPoint(fallthrubb->Lvbb());
    break;
  }
  case OPR_STID: {
    if (Is_lowered_actual(wn) &&
        Testing_mode() & TESTACTUAL) {
      Is_Trace(Tracing_enabled, (TFile, "STMT2llvm handling load for actual arg : "));
      Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
      RNAITER actual = Find_rna(wn);
#ifdef Is_True_On
      if (Rnaiter_end(actual)) {
        Is_Trace(Tracing_enabled, (TFile, "For Debugging, rnaiter is the end\n"));
      }
#endif
      if (!Rnaiter_end(actual) && actual->Lowered() == NULL) { // actuals not collected yet
        INT32  actuals = Collect_call_actuals(wn, actual);
      }
    }

    auto st = WN_st(wn);
    auto data = WN_kid0(wn);
    auto offset = WN_offset(wn);
    if (Is_rhs_inparm_ld(wn)) {
      // make a notation in WHIRL2llvm::Builder() for later use
      if (st->sym_class == CLASS_PREG) {
        Add_preg4parm(WN_offset(wn), WN_offset(data));
      }

      if (!Mload_ty(ST_type(st)) || (st->sym_class == CLASS_VAR)) {
        // save to inparm
        WN2llvmSymAct(wn, ACT_STR, nullptr);
        break;
      }
    }

    LVVAL *rhs = EXPR2llvm(data);
    WN2llvmSymAct(wn, ACT_STR, rhs);
    break;
  }
  case OPR_ISTORE: {
    LVVAL *rhs = EXPR2llvm(WN_kid0(wn));
    LVVAL *istr_base = EXPR2llvm(WN_kid1(wn));
    INT    offset = WN_offset(wn);

    LVTY  *istr_base_ty = istr_base->getType();

    // convert it to pointer type
    if (istr_base_ty->isIntegerTy()) {
      HandlePointerAndIntegerType(wn, &istr_base, GetLVPtrTy(), true);
      istr_base_ty = istr_base->getType();
    }

    if (offset != 0) {
      Gen_displacement(wn, &istr_base);
    }

    TYPE_ID desc = WN_desc(wn);
    LVTY *dest_ty = Wty2llvmty(desc, MTYPE_To_TY(desc));
    rhs = HandleStoreDifferentType(wn, rhs, dest_ty, MTYPE_is_signed(desc));

    // LVPRINT(rhs, "ISTORE DATA");
    // LVPRINT(istr_base, "ISTORE ADDR");
    auto store = Lvbuilder()->CreateStore(rhs, istr_base);
    store->setAlignment(llvm::Align(TY_align(WN_ty(wn))));
    break;
  }
  case OPR_MSTORE: {
  }
  case OPR_IF: {
  }
  case OPR_DO_WHILE:
  case OPR_WHILE_DO: {
  }
  case OPR_SWITCH: {
    FmtAssert(FALSE, ("WHIRL2llvm::STMT2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  // ===========================================================================
  case OPR_CALL: {
    // Interface chosen from IRBuilder.h:
    // CallInst *CreateCall(FunctionType *FTy, Value *Callee,
    //                 ArrayRef<Value *> Args = None, const Twine &Name = "",
    //                 MDNode *FPMathTag = nullptr)
    //
    // Identify the Function Object of the callee.
    // The WN_st of the call node should point to the function declaration
    // Since WHIRL does not maintain the declaration info fully, the type of 
    // arguments are collected from the actual argument list in this call node.
    // TODO: WHIRL2llvm to cache function delcarations
    LVFUNC   *func = GetFunction(wn);
    LVFUNCTY *lvfuncty = func->getFunctionType();

    // Traverse the whirl call node to generate argument list
    LVVALVEC  arglist;
    For_actual_args(wn, this, COLLECT_VALUE(&arglist));

    CALLEE_KIND callee_kind = Callee_kind(wn, func);

    switch (callee_kind) {
    case CK_NORMAL: {
      Handle_arg_diff_ty(wn, arglist, func);
      break;
    }
    case CK_BUILTIN: {
      if (func->getName() == "__builtin_unreachable") {
        Lvbuilder()->CreateUnreachable();
        return wn;
      } else {
        FmtAssert(FALSE, ("WHIRL2llvm::STMT2llvm, builtin function %s not handled", func->getName()));
      }
      break;
    }
    case CK_VARARG: {      
      auto arg_ty = arglist[0]->getType();
      auto param = WN_kid(wn, 0);

      for (int i = 0; i < lvfuncty->getNumParams(); i++) {
        LVTY *formal_ty = lvfuncty->getParamType(i);
        bool is_signed = func->getArg(i)->hasSExtAttr();
        arglist[i] = CastToTargetType(wn, arglist[i], formal_ty, is_signed);
      }
      break;
    }
    }

    // Generate call instruction with the CreateCall function
    LVCALL *call = Lvbuilder()->CreateCall(lvfuncty, func, arglist);

    // set call attributes
    SetCallInstAttrs(call, func);

    // Register the PREG created in LLVM symbol table in our own
    // Put_locvar(pname,(LVALC*) val, Wty2llvmty(WN_rtype(wn)));

    // Create the name of PREG that store value from return register
    LVALC *pname = nullptr;
    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy())) {
      pname = Collect_retval_pregname(wn);
    }

    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy()) && (pname != nullptr)) {
      auto ret_val = HandleStoreDifferentType(wn, call, pname->getAllocatedType());
      Lvbuilder()->CreateStore(ret_val, pname);
    }

    break;
  }
  // ===========================================================================
  case OPR_ICALL: {
    // TODO: OPR_ICALL is slightly different from OPR_CALL, seek refactor oppo!!
    WN       *funcptr = WN_kid(wn, WN_kid_count(wn)-1);
    LVVAL    *func = EXPR2llvm(funcptr); // call EXPR2llvm seems not right- shin 06232022

    Is_True(func->getType()->isIntegerTy(),
            ("EXPR2llvm: callee address should be integer"));
    HandlePointerAndIntegerType(wn, &func, GetLVPtrTy(), true);

    TY_IDX func_ty = WN_ty(wn);
    LVFUNCTY *lvfuncty = llvm::dyn_cast<LVFUNCTY>(Wty2llvmty(TY_mtype(func_ty), func_ty));
    FmtAssert(lvfuncty != nullptr, ("EXPR2llvm: lvfuncty should be llvm::FunctionType"));

    // Traverse the whirl call node to generate argument list
    LVVALVEC  arglist;
    For_actual_args(wn, this, COLLECT_VALUE(&arglist));

    // Create the name of PREG that store value from return register
    LVALC *pname = NULL;
    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy())) {
      pname = Collect_retval_pregname(wn);
    }

    // Handle type difference between arguments and parameters
    Handle_arg_diff_ty(wn, arglist, lvfuncty);

    // Generate call instruction with the CreateCall function
    LVVAL *val = Lvbuilder()->CreateCall(lvfuncty, func, arglist);

    // FmtAssert(FALSE, ("STMT2llvm, operator %s work in progress", OPERATOR2name(opr)));

    if (!(WN_rtype(wn) == MTYPE_V || lvfuncty->getReturnType()->isVoidTy()) && (pname != nullptr)) {
      Lvbuilder()->CreateStore(val, pname);
    }

    break;
  }
  case OPR_INTRINSIC_CALL: {
    INTRINSIC intrinsicID = WN_intrinsic(wn);
    switch(intrinsicID) {
      case INTRN_VA_START: {
        // convert to @llvm.va_start()

        // %1 = alloca ptr
        LVVAL *args = EXPR2llvm(WN_kid0(wn));

        // call void @llvm.va_start(%2)
        LVFUNC *TheFn =
            llvm::Intrinsic::getDeclaration(Module(), llvm::Intrinsic::vastart);
        Lvbuilder()->CreateCall(TheFn, {args});
        break;
      }
      case INTRN_MEMCMP:
      case INTRN_MEMCPY:
      case INTRN_MEMSET:
      case INTRN_STRCPY:
      case INTRN_STRCMP:
      case INTRN_STRLEN:
      case INTRN_MEMCCPY:
      case INTRN_MEMMOVE: {
        LVVAL *res = Handle_intrn_call(wn);
        FmtAssert(res == nullptr, ("STMT2llvm: INTRINSIC_CALL should return nullptr"));
        break;
      }
      default: {
        FmtAssert(FALSE, ("WHIRL2llvm::STMT2llvm, intrinsic(%d) not handled", intrinsicID));
      }
    }
    break;
  }
  case OPR_EVAL: {
    if (Has_volatile_expr(WN_kid0(wn))) {
      EXPR2llvm(WN_kid0(wn));
    }
    break;
  }
  case OPR_RETURN_VAL: {
    LVVAL *val = EXPR2llvm(WN_kid0(wn));
    FmtAssert(val != nullptr, ("STMT2llvm: generate return value failed."));
    Lvbuilder()->CreateRet(val);
    break;
  }
  case OPR_RETURN: {
    LVVAL *retval = NULL;
    WN *prev = WN_prev(wn);
    if (WN_operator(prev) == OPR_STID && IsCallResReg(prev)) {

      TY_IDX ret_tyidx = TY_ret_type(ST_type(WN_st(Cur_func())));
      auto tyid = TY_mtype(ret_tyidx);
      auto ret_type = Cur_lvfunc()->getReturnType();
      LVVAL *ret_val = nullptr;

      // pack struct/array return value
      if (ret_type->isStructTy()) {
        WN *tail = prev;
        // preg_I8_1 and preg_I8_7
        while (WN_offset(prev) != First_Int_Preg_Return_Offset) {
          FmtAssert(WN_operator(prev) == OPR_STID, ("STMT2llvm: return value stmt should be STID"));
          FmtAssert(WN_offset(prev) > First_Int_Preg_Return_Offset,
            ("STMT2llvm: STID offset(%d) should be greater than %d", 
              WN_offset(prev), First_Int_Preg_Return_Offset));
          prev = WN_prev(prev);
        }

        LVVALVEC elems;
        while (prev) {
          auto cur_reg = Get_preg(prev);
          auto cur_elem = Lvbuilder()->CreateLoad(cur_reg.first, cur_reg.second);
          elems.push_back(cur_elem);
          if (prev != tail) prev = WN_next(prev);
          else break;
        }


        auto struct_ty = llvm::cast<llvm::StructType>(ret_type);
        FmtAssert(struct_ty->getNumElements() == elems.size(), 
          ("STMT2llvm: elems size(%d) is different with return type%(%d)", 
            elems.size(), struct_ty->getNumElements()));

        Lvbuilder()->CreateAggregateRet(&elems[0], struct_ty->getNumElements());
        
      } else if (ret_type->isVoidTy()) {
        Lvbuilder()->CreateRetVoid();
      } else {
        auto ret_preg = Get_preg(prev);
        ret_val = Lvbuilder()->CreateLoad(ret_preg.first, ret_preg.second);

        bool is_signed = MTYPE_is_signed(WN_desc(prev));
        ret_val = CastToTargetType(prev, ret_val, ret_type, is_signed);
        Lvbuilder()->CreateRet(ret_val);
      }
    }
    else {
      if (!Cur_lvfunc()->getReturnType()->isVoidTy()) {
        llvm::Type *retType = Cur_lvfunc()->getReturnType();
        if (retType->isFloatingPointTy()) {
          retval = llvm::ConstantFP::get(retType, 0);
        } else if (retType->isIntegerTy()) {
          retval = llvm::ConstantInt::get(retType, 0);
        } else {
        }
        Lvbuilder()->CreateRet(retval);
      } else {
        Lvbuilder()->CreateRetVoid();
      }
    }
    break;
  }     
  case OPR_COMMENT: {
    break; // skip it
  }
  case OPR_AGOTO: {
    LVVAL *dest = EXPR2llvm(WN_kid0(wn));
    Lvbuilder()->CreateIndirectBr(dest);

    // get potential dest arrays from WN_kids(wn)
    FmtAssert(FALSE, ("STMT2llvm: operator %s works in progress", OPERATOR2name(opr)));
    break;
  } 
  case OPR_REGION: {
    FmtAssert(FALSE, ("WHIRL2llvm::STMT2llvm, operator %s not handled", OPERATOR2name(opr)));
    break;
  }
  case OPR_PREFETCH: {
    DevWarn("Stmt2llvm: ingore PREFETCH stmt now");
    break;
  }
  case OPR_PRAGMA: {
    DevWarn("Stmt2llvm: ingore PRAGMA stmt now");
    break;
  }
  case OPR_ASM_STMT: {
    DevWarn("Stmt2llvm: ASM_STMT %s not handled", OPERATOR2name(opr));
    break;
  }
  case OPR_DEALLOCA: {
    DevWarn("Stmt2llvm: operator %s not handled", OPERATOR2name(opr));
    break;
  }
  default:
    FmtAssert(FALSE, ("WHIRL2llvm::STMT2llvm, operator %s not handled", OPERATOR2name(opr)));

    break;
  }
  return wn;  // return the original wn
}


// =============================================================================
//
// BLOCK2llvm, nullIfEmpty deal with fall through BB for else part
//
// =============================================================================
W2LBB *WHIRL2llvm::BLOCK2llvm(WN *wn, W2LBB *w2lbb, bool nullIfEmpty) {
  FmtAssert(WN_opcode(wn) == OPC_BLOCK,
            ("BLOCK2llvm: node is not BLOCK but is %s", OPCODE_name(WN_opcode(wn))));
  WN *wn2 = WN_first(wn);
  if (nullIfEmpty && wn2 == NULL) {  // error, return
    return NULL;
  }

  // set insert point
  Lvbuilder()->SetInsertPoint(w2lbb->Lvbb());

  // caller will Create lvbb before calling this function
  // SRCPOS2llvm(wn, block);
  while (wn2 != NULL) {
    wn2 = STMT2llvm(wn2, w2lbb);
    wn2 = WN_next(wn2);
  }
  return w2lbb;
}

template <class OP>
void For_all_wnexpr(WN *wn, const OP &op)
{
  INT i;
  WN * wn2;

  op(wn);

  for (i = 0; i < WN_kid_count(wn); i++) {
    wn2 = WN_kid(wn,i);
    if (wn2) {
      OPCODE opc = WN_opcode(wn2);
      if ((OPCODE_FIRST <= opc && opc <= OPCODE_LAST) &&
          (OPCODE_is_expression(opc) || OPCODE_is_call(opc))) {
        For_all_wnexpr(wn2, op);
      } else if (opc == OPC_BLOCK
          && ((WN_operator(wn) == OPR_RCOMMA && i == 1)
           || (WN_operator(wn) == OPR_COMMA && i == 0))) {
        For_all_wnstmt(wn2, op);
      } else {
        FmtAssert(FALSE, ("For_all_wnexpr, opcode %d not an expression", opc));
      }
    } else {
      FmtAssert(FALSE, ("For_all_wnexpr, null-expression"));
    }
  }
}

template <class OP>
void For_all_wnstmt(WN *wn, const OP &op)
{
  if (wn == NULL) return;

  INT i;
  WN *wn2;

  op(wn);
  OPCODE opc = WN_opcode(wn);
  switch (opc) {
    case OPC_BLOCK:
      wn2 = WN_first(wn);
      while (wn2 != NULL) {
        For_all_wnstmt(wn2, op);
        wn2 = WN_next(wn2);
      }
      break;
    case OPC_REGION:
      For_all_wnstmt(WN_region_exits(wn), op);
      For_all_wnstmt(WN_region_pragmas(wn), op);
      For_all_wnstmt(WN_region_body(wn), op);
      break;
    case OPC_LABEL:
      if ( WN_label_loop_info(wn) != NULL ) {
        For_all_wnstmt(WN_label_loop_info(wn), op);
      }
      break;
    case OPC_IF:
      For_all_wnexpr(WN_if_test(wn), op);
      if ( WN_then(wn) )
        For_all_wnstmt(WN_then(wn), op);
      if ( WN_else(wn) )
        For_all_wnstmt(WN_else(wn), op);
      break;
    case OPC_DO_LOOP:
      For_all_wnexpr(WN_index(wn), op);
      For_all_wnstmt(WN_start(wn), op);
      For_all_wnexpr(WN_end(wn), op);
      For_all_wnstmt(WN_step(wn), op);
      if ( WN_do_loop_info(wn) )
        For_all_wnstmt( WN_do_loop_info(wn), op );
      For_all_wnstmt( WN_do_body(wn), op );
      break;
    case OPC_WHILE_DO:
      For_all_wnexpr(WN_kid(wn, 0), op);
      For_all_wnstmt(WN_kid(wn, 1), op );
      break;
    case OPC_DO_WHILE:
      For_all_wnstmt( WN_kid(wn, 1), op );
      For_all_wnexpr(WN_kid(wn, 0), op);
      break;
    case OPC_LOOP_INFO:
      if ( WN_loop_induction(wn) != NULL ) {
        For_all_wnexpr(WN_loop_induction(wn), op);
      }
      if ( WN_loop_trip(wn) != NULL ) {
        For_all_wnexpr(WN_loop_trip(wn), op);
      }
      break;
    case OPC_COMPGOTO:
    case OPC_SWITCH:
    case OPC_XGOTO:
    case OPC_WHERE:
      For_all_wnexpr(WN_kid(wn,0), op);
      For_all_wnstmt( WN_kid(wn, 1), op );
      if (WN_kid_count(wn) > 2)
        For_all_wnstmt( WN_kid(wn, 2), op );
      break;
    case OPC_EXC_SCOPE_BEGIN:
      for (i = 0; i < WN_kid_count(wn); i++)
        For_all_wnstmt( WN_kid(wn, i), op );
      break;
    case OPC_ASM_STMT:
      For_all_wnstmt( WN_kid(wn, 0), op );
      For_all_wnstmt( WN_kid(wn, 1), op );
      for (i = 2; i < WN_kid_count(wn); i++) {
        For_all_wnexpr(WN_kid(wn,i), op);
      }
      break;
    default:
      {
        INT last_is_expr = TRUE;
        OPCODE opc2;
        for (i = 0; i < WN_kid_count(wn); i++) {
          wn2 = WN_kid(wn,i);
          if (wn2) {
            opc2 = WN_opcode(wn2);
            if (opc2 == 0) {
              FmtAssert(FALSE, ("For_all_wnstmt, WN opcode 0"));
            } else if ( OPCODE_is_expression(opc2)) {
              For_all_wnexpr(wn2, op);
            } else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {
              For_all_wnstmt(wn2, op);
            } else {
              FmtAssert(FALSE, ("### error: unknown opcode type %d",opc2));
            }
          } // if (wn2)
        } // loop on all kids of wn
      } // default
  } // end of switch (opc)
}

WN*
WHIRL2llvm::Lower_puwn(PU_Info *pu_info, WN *wn)
{
  Current_PU_Info = pu_info;
  Be_preg_tab_ptr = CXX_NEW(BE_PREG_TAB(Malloc_Mem_Pool), Malloc_Mem_Pool);
  Scope_tab[CURRENT_SYMTAB].preg_tab->Register(Be_preg_tab);

  LOWER_ACTIONS actions;

  if (Whirl_level() == ML) {
    // lowering action can only be done once, hence choose carefully
    Is_Trace(Tracing_enabled, (TFile, "Lowering for ML input\n"));
    actions =
      LOWER_SCF      |
      LOWER_MSTORE   |
      LOWER_INLINE_INTRINSIC |
      LOWER_QUAD     |
      LOWER_MLDID_MSTID |
      LOWER_MADD;
    
    wn = WN_Lower(wn, actions, NULL, "WHIRL2llvm lowering Middle Level Whirl");
  }

  if (Whirl_level() == VHL) {
    VHO_Struct_Opt = TRUE;
    wn = VHO_Lower_Driver(pu_info, wn);
  }

  if (Whirl_level() == VHL || Whirl_level() == HL) {
    wn = WN_Lower(wn, LOWER_RETURN_VAL, NULL,
                  "WHIRL2llvm lowering RETURN_VAL lowering");

    wn = WN_Lower(wn, LOWER_MLDID_MSTID, NULL,
                  "Lower MLDID/MSTID when not running WOPT");

    Initialize_Stack_Frame (wn);

    actions =
      LOWER_FIELD_OFFSET |
      LOWER_COMPLEX |
      LOWER_BASE_INDEX |
      LOWER_ARRAY |
      LOWER_ALL_MAPS |
      LOWER_IO_STATEMENT |
      LOWER_SHORTCIRCUIT |
      LOWER_BITS_OP |
      LOWER_SIMPLIFY_BIT_OP |
      LOWER_TO_MEMLIB;

    if (!TY_is_varargs(ST_pu_type((WN_st(wn)))) ||
        (Testing_mode() & TESTLOWERVARARG) != TESTDEFAULT) {
      actions =
        actions |
        LOWER_ENTRY_EXIT |
        LOWER_INLINE_INTRINSIC |
        LOWER_TO_CG;
    }

    wn = WN_Lower(wn, actions, NULL, "WHIRL2llvm lowering High Level Whirl");
  }

  WN_verifier(wn);
  Is_Trace(Tracing_enabled, (TFile, "%sAfter Lowering %s\n%s", DBar,
                             ST_name(WN_st(wn)), DBar));
  Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));

  For_all_wnstmt(WN_func_body(wn), GEN_JMPTBL(this));

  return wn;
}

void
W2LBUILDER::Print_rnalst(FILE *fp)
{
  fprintf(fp, "RNA List, cnt = %d:\n", (INT)_rnavec.size());
  RNAITER it;
  for (it = _rnavec.begin(); it != _rnavec.end(); ++it)
    it->Print(fp);
}


RNAITER
W2LBUILDER::Find_rna(WN *wn)
{
  Is_Trace(Tracing_enabled, (TFile, "Find_rna with WHIRL node: "));
  Is_Trace_cmd(Tracing_enabled, fdump_wn(TFile, wn));
  RNAITER it;
  for (it = _rnavec.begin(); it != _rnavec.end(); ++it) {
    RNA cand = (*it);
    if (cand == wn) {
      return it;
    }
  }
  return _rnavec.end();
}


// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::TCON2llvm(TCON_IDX tc, TY_IDX ty_idx) {
  TCON &tcon = Tcon_Table[tc];
  LVTY *lvty = Wty2llvmty(TCON_ty(tcon), ty_idx);
  switch (TCON_ty(tcon)) {
    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4: {
      return llvm::ConstantInt::get(lvty, (INT64)tcon.vals.ival.v0);
    }
    case MTYPE_B:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:{
      return llvm::ConstantInt::get(lvty, (UINT64)tcon.vals.uval.u0);
    }
    case MTYPE_I8:
    case MTYPE_U8: {
      return llvm::ConstantInt::get(lvty, tcon.vals.llval.ll0);
    }
    case MTYPE_F4:
      return llvm::ConstantFP::get(lvty, tcon.vals.fval);
    case MTYPE_F8:
      return llvm::ConstantFP::get(lvty, tcon.vals.dval);
    case MTYPE_STRING: {
      INT slen = tcon.vals.sval.len;
      char *bytes = Index_to_char_array(tcon.vals.sval.cp);
      return llvm::ConstantDataArray::getString(Context(), llvm::StringRef(bytes), true);
    }
    default:
      FmtAssert(FALSE, ("TCON2llvm: mtype %s NYI", MTYPE_name(TCON_ty(tcon))));
  }
  return nullptr;
}

// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::INITV2llvm4field(INITV_IDX idx, const FLD_HANDLE &fld, LVTY *field_ty) {
  LVCONST *field = nullptr;
  INITVKIND kind = INITV_kind(idx);
  if (!FLD_is_bit_field(fld)) {
    field = INITV2llvm(Initv_Table[idx], FLD_type(fld));
    // LVPRINT(field, "field");
    // LVPRINT(field_ty, "field_ty");
    FmtAssert(field->getType() == field_ty, ("INITV2llvm4field: field type is wrong"));
  } else {
    FmtAssert(FALSE, ("INITV2llvm4struct: NYI bit-field now"));
  }

  FmtAssert(field != nullptr, ("INITV2llvm4field: create field init value faield"));
  return field;
}

// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::INITV2llvm4struct(INITV_IDX idx, TY_IDX ty_idx, llvm::StructType *struct_ty) {
  llvm::Constant *struct_const = nullptr;
  LVPRINT(struct_ty, "struct type");
  std::vector<LVCONST *> fields;
  FLD_HANDLE fld = TY_fld(ty_idx);
  for (int cnt = 0; idx != 0; idx = INITV_next(idx)) {
    // ignore padding now
    if (INITV_kind(idx) == INITVKIND_PAD) {
      if (fld.Entry() == nullptr) continue;
      TY_IDX fld_ty = FLD_type(fld);
      TY_KIND fld_ty_kind = TY_kind(fld_ty);
      if (fld_ty_kind != KIND_STRUCT && fld_ty_kind != KIND_ARRAY) {
        continue;
      }
    }
    FmtAssert(!fld.Is_Null(), ("INITV2llvm4struct: fld is null"));
    if (!FLD_is_bit_field(fld)) {
      // dump_ty(fld.Entry()->type);
      fields.push_back(INITV2llvm4field(idx, fld, struct_ty->getElementType(cnt)));
      fld = FLD_next(fld);
      cnt++;
    } else {
      FmtAssert(FALSE, ("INITV2llvm4struct: NYI bit-field now"));
    }
  }

  FmtAssert(struct_ty->getNumElements() == fields.size(), ("INITV2llvm4struct: field number is wrong"));
  struct_const = llvm::ConstantStruct::get(struct_ty, fields);

  FmtAssert(struct_const != nullptr, ("INITV2llvm4struct: create struct initial value failed"));
  return struct_const;
}

// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::INITV2llvm4pad(INITV_IDX idx, TY_IDX ty_idx) {
  return INITV2llvm4pad(Initv_Table[idx], ty_idx);
}

LVCONST *WHIRL2llvm::INITV2llvm4pad(const INITV &initv, TY_IDX ty_idx) {
  LVCONST *pad = nullptr;
  TY_KIND ty_kind = TY_kind(ty_idx);
  if (ty_kind == KIND_ARRAY || ty_kind == KIND_STRUCT) {
    LVTY *aggregate_ty = Wty2llvmty(TY_mtype(ty_idx), ty_idx);
    pad = llvm::ConstantAggregateZero::get(aggregate_ty);
  } else {
    FmtAssert(ty_kind == KIND_SCALAR,
      ("INITV2llvm4pad: ty_kind(%u) should be KIND_SCALAR(%u)", ty_kind, KIND_SCALAR));
    FmtAssert(MTYPE_is_integral(TY_mtype(ty_idx)), 
      ("INITV2llvm4pad: ty_mtype(%u) should be integer", TY_mtype(ty_idx)));

    LVTY *pad_ty = Wty2llvmty(TY_mtype(ty_idx), ty_idx);
    pad = llvm::ConstantInt::get(pad_ty, 0);
  }
  return pad;
}

// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::INITV2llvm4zero(const INITV &initv, TY_IDX ty_idx) {
  LVCONST *zero = nullptr;
  TYPE_ID mtype = TY_mtype(ty_idx);
  LVTY *lvty = Wty2llvmty(mtype, ty_idx);
  switch(TY_kind(ty_idx)) {
    case KIND_STRUCT:
    case KIND_ARRAY: {
      zero = llvm::ConstantAggregateZero::get(lvty);
      break;
    }
    case KIND_POINTER: {
      FmtAssert(lvty->isPointerTy(), ("INITV2llvm4zero: lvty should be pointer type"));
      zero = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(lvty));
      break;
    }
    case KIND_SCALAR: {
      if (MTYPE_is_integral(mtype)) {
        zero = llvm::ConstantInt::get(lvty, 0);
      } else {
        zero = llvm::ConstantFP::get(lvty, 0.0);
      }
      break;
    }
    default: {
      FmtAssert(FALSE, ("INITV2llvm4zero: TY_KIND(%d) NYI", TY_kind(ty_idx)));
    }
  }
  return zero;
}


// =============================================================================
//
// =============================================================================
LVCONST *WHIRL2llvm::INITV2llvm(const INITV &initv, TY_IDX ty_idx) {
  llvm::Constant *init = nullptr;
  INITVKIND initv_kind = INITV_kind(initv);
  TY_KIND ty_kind = TY_kind(ty_idx);

  switch (initv_kind) {
  case INITVKIND_ZERO: {
    LVTY *lvty = Wty2llvmty(TY_mtype(ty_idx), ty_idx);
    if (ty_kind == KIND_POINTER) {
      FmtAssert(lvty->isPointerTy(), ("INITV2llvm: lvty should be pointer type"));
      init = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(lvty));
    } else if (ty_kind == KIND_SCALAR) {
      init = llvm::ConstantInt::get(lvty, 0);
    } else {
      FmtAssert(FALSE, ("INITV2llvm: INITVKIND_ZERO NYI for type_kind(%d) NYI", ty_kind));
    }
    break;
  }
  case INITVKIND_ONE: {
    init = llvm::ConstantInt::get(Wty2llvmty(TY_mtype(ty_idx), ty_idx), 1);
    break;
  }
  case INITVKIND_VAL:
    init = TCON2llvm(INITV_tc(initv), ty_idx);
    break;
  case INITVKIND_BLOCK: {
    INITV_IDX idx = INITV_blk(initv);
    if (ty_kind == KIND_ARRAY) {
      TY *array_ty = &Ty_Table[ty_idx];
      TY_IDX elem_ty = array_ty->Etype();
      UINT64 elem_size = TY_size(elem_ty);
      UINT64 len = TY_size(*array_ty) / elem_size;
      llvm::ArrayType *lv_array_ty = llvm::ArrayType::get(Wty2llvmty(TY_mtype(elem_ty), elem_ty), len);

      std::vector<LVCONST *> vals;
      while (idx) {
        LVCONST *elem = nullptr;
        INITV &initv0 = Initv_Table[idx];
        if (INITV_kind(initv0) != INITVKIND_PAD) {
          elem = INITV2llvm(initv0, elem_ty);
        } else {
          if ((initv0.Pad() % elem_size) == 0) {
            elem = INITV2llvm4zero(initv0, elem_ty);
          } else {
            FmtAssert(FALSE, ("INITV2llvm: PAD in array NYI"));
          }
        }
        if (elem != nullptr) vals.push_back(elem);
        idx = INITV_next(initv0);
      }

      LVTY *lv_elem_ty = lv_array_ty->getElementType();
      for (std::size_t i = 0; i < vals.size(); i++) {
        LVTY *val_ty = vals[i]->getType();
        if (val_ty != lv_elem_ty) {
          // handle different type
          if (val_ty->isPointerTy() && lv_elem_ty->isPointerTy()) {
            vals[i] = llvm::ConstantExpr::getBitCast(vals[i], lv_elem_ty);
          } else {
            FmtAssert(FALSE, ("INITV2llvm: different type NYI"));
          }
        }
      }
      init = llvm::ConstantArray::get(lv_array_ty, vals);
    } else {
      FmtAssert(ty_kind == KIND_STRUCT,
         ("INITV2llvm: current type is(%s %u), should be struct type", TY_name(ty_idx), ty_idx));
      llvm::StructType *struct_ty = llvm::cast<llvm::StructType>(Wty2llvmty(TY_mtype(ty_idx), ty_idx));
      return INITV2llvm4struct(idx, ty_idx, struct_ty);
    }
    break;
  }
  case INITVKIND_PAD: {
    init = INITV2llvm4pad(initv, ty_idx);
    break;
  }
  case INITVKIND_SYMOFF: {
    FmtAssert(ST_CLASS(INITV_st(initv)) != CLASS_CONST, ("INITV_IDX2mpl: SYMOFF symbol cannot be CLASS_CONST"));
    ST *st = &St_Table[INITV_st(initv)];
    LVGLBVAR *sym = nullptr;
    switch (st->sym_class) {
    case CLASS_CONST:{
      sym = llvm::cast<LVGLBVAR>(ST2const(st));
      if (sym->getType()->isPointerTy()) {
        FmtAssert(sym->getValueType()->isArrayTy(), ("INITV_IDX2mpl: SYMOFF symbol should be pointer to array"));
        std::vector<LVVAL*> idxs;
        idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context()), 0));
        idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context()), 0));
        init = llvm::ConstantExpr::getGetElementPtr(
          sym->getInitializer()->getType(),
          sym,
          idxs);
      } else {
          FmtAssert(FALSE,
            ("INITV2llvm: create init kind %d NYI, failed ty_idx = 0x%x", initv_kind, ty_idx));
      }
      break;
    }
    case CLASS_VAR: {
      sym = llvm::cast<LVGLBVAR>(ST2lvval(st));
      if (sym->getType()->isPointerTy()) {
        LVTY *val_ty = sym->getValueType();
        if (val_ty->isArrayTy()) {
          std::vector<LVVAL*> idxs;
          idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context()), 0));
          idxs.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(Context()), 0));
          init = llvm::ConstantExpr::getGetElementPtr(val_ty, sym, idxs);
        } else {
          init = sym;
        }
      } else {
        FmtAssert(FALSE,
          ("INITV2llvm: create init kind %d NYI, failed ty_idx = 0x%x", initv_kind, ty_idx));
      }
      break;
    }
    case CLASS_FUNC: {
      init = ST2func(st);
      FmtAssert(init != nullptr, ("INITV2llvm: can't found function \"%s\"", ST_name(st)));
      break;
    }
    default:
      FmtAssert(FALSE,
        ("INITV2llvm: create init kind %d NYI, failed ty_idx = 0x%x", initv_kind, ty_idx));
    }
    break;
  }
  case INITVKIND_LABEL: {
    LABEL_IDX labnum = INITV_lab(initv);
    char*  labname = LABEL_NUMBER2name(labnum);
    W2LBB* targbb = Get_w2lbb(labname);
    LVBB*  lvbb = targbb->Lvbb();

    // create label array
    std::vector<llvm::Constant *> labels;
    labels.push_back(LVBLKADDR::get(Cur_lvfunc(), lvbb));
    INITV_IDX initv0 = INITV_next(initv);
    while (initv0) {
      labnum = INITV_lab(initv0);
      labname = LABEL_NUMBER2name(labnum);
      targbb = Get_w2lbb(labname);
      labels.push_back(LVBLKADDR::get(Cur_lvfunc(), targbb->Lvbb()));
      initv0 = INITV_next(initv0);
    }
    
    llvm::ArrayType *array_ty = llvm::ArrayType::get(labels[0]->getType(), labels.size());
    return llvm::ConstantArray::get(array_ty, labels);
  }
  
  default: 
    FmtAssert(FALSE,
        ("INITV2llvm: create init kind %d not supported, failed ty_idx = 0x%x", initv_kind, ty_idx));
    break;
  }

  return init;
}

struct INITO2llvm {
  WHIRL2llvm *whirl2llvm;
  INITO2llvm(WHIRL2llvm *w) : whirl2llvm(w) {}

  void operator() (UINT idx, INITO *inito) const;
};

void INITO2llvm::operator() (UINT idx, INITO *inito) const {
  const ST *st = &St_Table[inito->st_idx];
  const INITV &initv = Initv_Table[inito->val];

  llvm::Constant *init = whirl2llvm->INITV2llvm(initv, ST_type(st));
  
  switch (st->storage_class) {
    case SCLASS_FSTATIC:
    case SCLASS_DGLOBAL: {
      LVGLBVAR *gvar = llvm::cast<LVGLBVAR>(whirl2llvm->Get_glbvar(ST_name(st)));
      gvar->setInitializer(init);
      break;
    }
  }
  // dump_st(st);
}

struct ST_ATTR2llvm {
  WHIRL2llvm *whirl2llvm;
  ST_ATTR2llvm(WHIRL2llvm *w) : whirl2llvm(w) {}

  void operator() (UINT idx, ST_ATTR *st_attr) const;
};

void ST_ATTR2llvm::operator() (UINT idx, ST_ATTR *st_attr) const {
  if (st_attr->kind == ST_ATTR_SECTION_NAME) {
    std::string section_name(&Str_Table[st_attr->Get_section_name()]);
    LVVAL *val = whirl2llvm->ST2const_or_val(&St_Table[st_attr->st_idx]);
    if (auto gvar = llvm::dyn_cast<LVGLBVAR>(val)) {
      gvar->setSection(section_name);
    } else if (auto func = llvm::dyn_cast<LVFUNC>(val)) {
      func->setSection(section_name);

    } else {
      FmtAssert(FALSE, ("ST_ATTR2llvm: only handle GLOBAL and FUNC for now"));
    }
  } else {
    FmtAssert(FALSE, ("ST_ATTR2llvm: kind(%d) not supported", st_attr->kind));
  }
}

// =============================================================================
//
// =============================================================================
void WHIRL2llvm::FUNC_ENTRY2llvm(PU_Info *pu, WN *wn) {
  Set_current_function(ST_name(WN_st(wn)));
  Cur_func(wn);
  // LVFUNCTY *lvfuncty = Get_function_ty();
  Gen_func(wn);

  WN *block_wn = WN_func_body(wn);
  if (block_wn == NULL) {
    return;
  }
  W2LBUILDER w2lbuilder(NULL); // Create_w2lbb need this definition
  Builder(&w2lbuilder); // Create_w2lbb need this definition

  W2LBB     *entry_w2lbb = Create_w2lbb((char*)"entry");
  LVBB      *entry_bb = entry_w2lbb->Lvbb(); 
  Lventry(entry_bb);
  LVBUILDER  lvbuilder(entry_bb);
  w2lbuilder.Builder(&lvbuilder);

  // set the source position for this function
  // WN_Get_Linenum(block_wn);

  // translate local symbol table
  // all local variable and PREG are to be declared here.
  SYMTAB_IDX level = PU_lexical_level(WN_st(Cur_func()));
  // dump to see what are added to the symtab an type table

  For_all_entries(*Scope_tab[level].st_tab, ST2llvm(this), 1);

  // translate the initializations
  For_all_entries(*Scope_tab[level].inito_tab, INITO2llvm(this), 1);

  BLOCK2llvm(WN_func_body(wn), entry_w2lbb);
  Flush_cur_func();
  Is_Trace_cmd(Tracing_enabled, Print_local_symtab (TFile, Scope_tab[level]));
  Is_Trace(Tracing_enabled, (TFile, "print global symbol table"));
  Is_Trace_cmd(Tracing_enabled, Print_global_symtab (TFile));

  Is_Trace(Tracing_enabled, (TFile, "%sEnd processing function %s\n%s", DBar,
                             ST_name(WN_st(wn)), DBar));
}

// =============================================================================
/* Binary to ASCII conversion */
// =============================================================================
static void
ir_b2a_process_PUs (PU_Info *pu_tree, WHIRL2llvm *whirl2llvm)
{
  PU_Info *pu;
  WN *wn;

  for (pu = pu_tree; pu != NULL; pu = PU_Info_next (pu)) {
    if (pu != pu_tree) {  // first pu already read to determine the language
      Read_Local_Info (MEM_pu_nz_pool_ptr, pu);
    }

    wn = PU_Info_tree_ptr(pu);

    // check the file needs lowering
    whirl2llvm->Inc_cur_pu_num();
    if (whirl2llvm->Skip_cur_pu()) continue;

    if (Tracing_enabled)
      printf("Translating %s(%d)\n", ST_name(WN_st(wn)), whirl2llvm->Cur_pu_num());
    Is_Trace(Tracing_enabled, (TFile, "%sStart processing function %s\n%s", DBar,
                               ST_name(WN_st(wn)), DBar));
    Is_Trace_cmd(Tracing_enabled, fdump_tree(TFile, wn));

    W2LBUILDER rna_builder(NULL);
    whirl2llvm->RnaBld(&rna_builder);
    //rna_builder.Stmt2rnalst(WN_func_body(wn));
    For_all_wnstmt(WN_func_body(wn), COLLECT_RNA(whirl2llvm));
    Is_Trace(Tracing_enabled, (TFile, "Dump call list before lowering: "));
    Is_Trace_cmd(Tracing_enabled, rna_builder.Print_rnalst(TFile));

    wn = whirl2llvm->Lower_puwn(pu, wn);

    whirl2llvm->FUNC_ENTRY2llvm(pu, wn);

#if 0 //code below should be uncommented for nested procedure support
    if (PU_Info_child(pu)) {
      ir_b2a_process_PUs(PU_Info_child(pu), whirl2llvm);
    }

    SYMTAB_IDX level = PU_lexical_level (&St_Table[PU_Info_proc_sym (pu)]);
    Print_local_symtab (stdout, Scope_tab[level]);
#endif
  }
}

void WHIRL2llvm::Create_inc_dirs() {
  DST_DIR_IDX      idx = DST_get_include_dirs();
  mUINT16          num = 0;
  DST_INCLUDE_DIR *dir;

  STRVEC &inc_dirs = Inc_dirs();
  inc_dirs.push_back(""); // the first element is dummy
   
  if (!DST_IS_NULL(idx)) {
     dir = DST_DIR_IDX_TO_PTR(idx);
  } else {
     dir = NULL;
  }

  while(dir != NULL) {
    num += 1;

    // DST_put_string_attribute(" path", DST_INCLUDE_DIR_path(dir));
    inc_dirs.push_back(DST_STR_IDX_TO_PTR(DST_INCLUDE_DIR_path(dir)));
    
    idx = DST_INCLUDE_DIR_next(dir);
    if (!DST_IS_NULL(idx)) {
	    dir = DST_DIR_IDX_TO_PTR(idx);
    } else {
	    dir = NULL;
    }
  }
}

void WHIRL2llvm::Create_w2lfiles() {
  DST_FILE_IDX  idx = DST_get_file_names();
  mUINT16       num = 0;
  DST_FILE_NAME *f;

  W2LFILEVEC &w2lfiles = W2L_files();
  w2lfiles.push_back(W2LFILE(NULL)); // the first element is dummy
   
  if (!DST_IS_NULL(idx))
     f = DST_FILE_IDX_TO_PTR(idx);
  else
     f = NULL;

  while(f != NULL) {
    num += 1;

    W2LFILE file(f);
    std::string name = DST_STR_IDX_TO_PTR(DST_FILE_NAME_name(f));
    std::string path = Inc_dirs()[DST_FILE_NAME_dir(f)];
    LVDIFILE *difile = DIBuilder()->createFile(name, path);
    file.Lvdi_file(difile);
    w2lfiles.push_back(file);

    idx = DST_FILE_NAME_next(f);
    if (!DST_IS_NULL(idx)) {
      f = DST_FILE_IDX_TO_PTR(idx);
    } else {
      f = NULL;
    }
  }
}

void WHIRL2llvm::Init_debug_info() {
  // create include directories
  Create_inc_dirs();

  // create file info entries
  Create_w2lfiles();

  // create debug info for compile unit
  DST_INFO *cu_info = DST_INFO_IDX_TO_PTR(DST_get_compile_unit());
  DST_COMPILE_UNIT *attr = DST_ATTR_IDX_TO_PTR(DST_INFO_attributes(cu_info), DST_COMPILE_UNIT);
  char *producer = DST_STR_IDX_TO_PTR(DST_COMPILE_UNIT_producer(attr));

  LVDIFILE *di_file = W2L_files()[1].Lvdi_file();
  // LVDICU *cu = this->DIBuilder()->createCompileUnit(
  //   DW_LANG_C, 
  //   di_file,
  //   producer, 
  //   1, "", 0
  // );
  // this->DICU(cu);
}

LVDILOC *WHIRL2llvm::SRC_POS2diloc(const USRCPOS &) {
  FmtAssert(FALSE, ("SRC_POS2diloc NYI"));
  return nullptr;
}

LVDILOC *WHIRL2llvm::SRC_POS2diloc(INT64 linenum) {
	USRCPOS srcpos;
  USRCPOS_srcpos(srcpos) = linenum;

  mUINT16 filenum = USRCPOS_filenum(srcpos);
  // LVDILOC *diloc = LVDILOC::get(
  //   USRCPOS_linenum(srcpos),            // line number
  //   USRCPOS_column(srcpos),             // column number
  //   W2L_files()[filenum].Lvdi_file(),   // DIFile
  //   NULL                                // Scope
  // );
  // return diloc;
}

static BOOL ENABLE_GVN = false;
static BOOL ENABLE_MEM2REG = false;
static BOOL ENABLE_SIMP_CFG = false;
static BOOL ENABLE_SIMP_INST = false;
static BOOL ENABLE_INST_COMBINE = false;
static BOOL ENABLE_DGE = false;
static BOOL ENABLE_BDCE = false;

// =============================================================================
/* Binary to ASCII conversion */
// =============================================================================
static void
ir_b2a (char *global_file,
        char *input_file,
        INT skip_before,
        INT skip_after,
        INT testing_mode,
        INT targ_byte_sex) {
    PU_Info *pu_tree;

    if (global_file == NULL) {
  (void)Open_Input_Info (input_file);
    } else {
        (void)Open_Global_Input (global_file);
        (void)Open_Local_Input (input_file);
    }

    Initialize_Symbol_Tables (FALSE);
    New_Scope (GLOBAL_SYMTAB, Malloc_Mem_Pool, FALSE);
    pu_tree = Read_Global_Info (NULL);

    // Set to 64 bit default for now
    ABI_Name = "n64";
    Preconfigure();
    Configure();
    // set target endianess
    Target_Byte_Sex = targ_byte_sex;
    Same_Byte_Sex = ( Target_Byte_Sex == Host_Byte_Sex );

    // Setup backend context for WN_Lower
    Initialize_Special_Global_Symbols ();

    IR_reader_init();
    IR_Dwarf_Gen_File_Table(FALSE);  // this will set srcFileTable
    WHIRL2llvm driver(input_file, skip_before, skip_after, testing_mode);

    // set the language based on the first PU
    if (pu_tree != NULL) {
      PU_Info *pu = pu_tree;
      Read_Local_Info (MEM_pu_nz_pool_ptr, pu);
      WN *wn = PU_Info_tree_ptr(pu);
      ST_IDX func_st_idx = WN_st_idx(wn);
      ST *func_st = &St_Table[func_st_idx];
    }

    // create debug info for compile unit
    // driver.Init_debug_info();

    // translate all the types
    int tmpi = 0;
    driver.Ign_fwd_tyref(TRUE);
    For_all (Ty_Table, TY2llvm(&driver));
    do {
      driver.Ign_fwd_tyref(FALSE);  // need to resolve FWD issues
      driver.Has_fwd_tyref(FALSE);
      For_all (Ty_Table, FixForwardTypeRef(&driver));
      BOOL tmp = driver.Has_fwd_tyref();
      Is_Trace(Tracing_enabled, (TFile, "Fwd ref is %d in loop %d\n", tmp, tmpi ));
      tmpi++;
    } while(driver.Has_fwd_tyref());

    // translate global symbols
    For_all (St_Table, GLOBAL_SYMTAB, ST2llvm(&driver));

    // translate the function bodies
    driver.Setup_be_scope();
    ir_b2a_process_PUs(pu_tree, &driver);

    // translate the initializations
    For_all (Inito_Table, GLOBAL_SYMTAB, INITO2llvm(&driver));

    // process the global ST_ATTR_TAB
    For_all (St_Attr_Table, GLOBAL_SYMTAB, ST_ATTR2llvm(&driver));

    // initialize the llvm.global_ctors array
    driver.Register_llvm_glb_ctors();

    Free_Input_Info ();

    llvm::Triple ModuleTriple(driver.Module()->getTargetTriple());
    // Add an appropriate TargetLibraryInfo pass for the module's triple.
    llvm::TargetLibraryInfoImpl TLII(ModuleTriple);

    llvm::FunctionPassManager FPM;
    llvm::LoopAnalysisManager LAM;
	  llvm::FunctionAnalysisManager FAM;
	  llvm::CGSCCAnalysisManager CGAM;
	  llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    // Register our TargetLibraryInfoImpl.
    FAM.registerPass([&] { return llvm::TargetLibraryAnalysis(TLII); });

    llvm::ModulePassManager MPM;
    // PB.parsePassPipeline(MPM, "");

    if (ENABLE_MEM2REG) {
      // Promote allocas to registers.
      FPM.addPass(llvm::PromotePass());
    }

    if (ENABLE_SIMP_INST) {
      FPM.addPass(llvm::InstSimplifyPass());
    }

    if (ENABLE_GVN) {
      FPM.addPass(llvm::GVNPass());
    }

    if (ENABLE_SIMP_CFG) {
      FPM.addPass(llvm::SimplifyCFGPass());
    }

    if (ENABLE_INST_COMBINE) {
      FPM.addPass(llvm::BDCEPass());
      FPM.addPass(llvm::InstCombinePass());
    }

    if (ENABLE_DGE) {
      MPM.addPass(llvm::GlobalDCEPass());
    }

    FPM.addPass(llvm::DCEPass());

    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
    MPM.run(*driver.Module(), MAM);

    driver.Module()->print(llvm::outs(), nullptr);
    std::error_code EC;
    auto out = new llvm::raw_fd_ostream(std::string(input_file) + ".ll", EC);
    driver.Module()->print(*out, nullptr);
} // ir_b2a

// =============================================================================
// =============================================================================
extern BOOL
file_exists (char *path)
{
  INT st;
  struct stat sbuf;
  st = stat(path, &sbuf);
  if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
    return FALSE;
  else
    return TRUE;
}

// =============================================================================
// =============================================================================
static BOOL
Has_Extension (const char *name,  /* The filename to check */
         const char *ext)   /* The extension to look for */
{
  INT16 nlen = strlen(name);
  INT16 elen = strlen(ext);

  /* If ext is longer than name, no chance: */
  if ( elen > nlen ) return FALSE;

  /* Otherwise compare the tail of name to ext: */
  return ( strcmp ( &name[nlen-elen], ext ) == 0 );
} /* Has_Extension */

void
WHIRL2llvm::Set_whirl_level(const char *in)
{
  if (Has_Extension(in, ".B")) { Whirl_level(VHL); return; }
  if (Has_Extension(in, ".I")) { Whirl_level(VHL); return; }
  if (Has_Extension(in, ".N")) { Whirl_level(HL);  return; }
  if (Has_Extension(in, ".O")) { Whirl_level(ML);  return; }
  // if (Has_Extension(in, ".o")) { Whirl_level(VHL);  return; }
}

// =============================================================================
// =============================================================================
static void
usage (char *progname)
{
  fprintf (stderr, "Usage: %s [-a] <Whirl IR>\n", progname);
  exit (1);
}

int main (INT argc, char *argv[])
{
    register char *progname;
    register INT a2b, b2a, sel, all;
    register INT skip_before = 0;
    register INT skip_after = INT32_MAX;
    register INT testing_mode =
      TESTREDEF | TESTACTUAL | TESTFORMAL | TESTJMPTBL/* | TESTLOWERVARARG*/;
    register INT targ_byte_sex = LITTLE_ENDIAN;
    char *infile;

    MEM_Initialize();
    Set_Error_Tables (Phases, host_errlist);
    Init_Error_Handler (10);
    Set_Error_File(NULL);
    Set_Error_Line(ERROR_LINE_UNKNOWN);
    WHIRL_Mldid_Mstid_On = TRUE;
    Pointer_Size = 8;  // this is because Configure_Target() was not called

    progname = basename (argv[0]);
    // weird linux bug with basename where it doesn't strip the leading /
    if (*progname == '/') ++progname;

    Read_Global_Data = NULL;
    if (argc < 2)
      usage(progname);
    INT binarg = 1;
    if (*argv[binarg] == '-') {
      if (strncmp(argv[binarg], "-a", 2) == 0) {
        use_binary = FALSE;
      } else {
        usage(progname);
      }
      binarg++;
    }
    if (binarg == argc)
        usage(progname);
    if (!file_exists(argv[binarg]))
        usage(progname);
    infile = argv[binarg];
    Set_current_file(infile);

    binarg++;

    // handle pass
    while (binarg < argc) {
      if (strncmp(argv[binarg], "-mem2reg", 8) == 0) {
        ENABLE_MEM2REG = true;
      } else if (strncmp(argv[binarg], "-simpcfg", 8) == 0) {
        ENABLE_SIMP_CFG = true;
      } else if (strncmp(argv[binarg], "-simpinst", 9) == 0) {
        ENABLE_SIMP_INST = true;
      } else if (strncmp(argv[binarg], "-gvn", 4) == 0) {
        ENABLE_GVN = true;
      } else if (strncmp(argv[binarg], "-instcombine", 12) == 0) {
        ENABLE_INST_COMBINE = true;
      } else if (strncmp(argv[binarg], "-dge", 4) == 0) {
        ENABLE_DGE = true;
      } else if (strncmp(argv[binarg], "-bdce", 5) == 0) {
        ENABLE_BDCE = true;
      } else if (strncmp(argv[binarg], "-all", 4) == 0) {
        ENABLE_MEM2REG = ENABLE_SIMP_CFG = ENABLE_SIMP_CFG = 
        ENABLE_GVN = ENABLE_INST_COMBINE = ENABLE_DGE = ENABLE_BDCE = true;
      } else if (strncmp(argv[binarg], "-skip_before", 12) == 0) {
        skip_before = atoi(argv[++binarg]);
      } else if (strncmp(argv[binarg], "-skip_after", 11) == 0) {
        skip_after = atoi(argv[++binarg]);
      } else if (strncmp(argv[binarg], "-targ_byte_sex", 14) == 0) {
        targ_byte_sex = (*(argv[++binarg]) == 'B')? BIG_ENDIAN : LITTLE_ENDIAN;
      } else if (strncmp(argv[binarg], "-disable_trace", 14) == 0) {
        ENABLE_TRACE = false;
      } else if (strncmp(argv[binarg], "-disable", 8) == 0) {
        testing_mode ^= atoi(argv[++binarg]);
      } else if (strncmp(argv[binarg], "-enable", 7) == 0) {
        testing_mode |= atoi(argv[++binarg]);
      } else {
        // can't match
      }
      binarg++;
    }

    Set_trace_file(Create_trc_filename(infile));
    ir_b2a (Read_Global_Data, infile, skip_before, skip_after, testing_mode, targ_byte_sex);

    _Exit (0);
} /* main */


// =============================================================================
/* Dummy definitions to satisify references from routines that got pulled
 * in by the header files but are never called
 */
// =============================================================================
void
Signal_Cleanup (INT sig) { 
}
const char *
Host_Format_Parm (INT kind, MEM_PTR parm)
{
    fprintf (stderr, "Internal: Host_Format_Parm () not implemented\n");
    return "";
}

INT8 Debug_Level = 0;
