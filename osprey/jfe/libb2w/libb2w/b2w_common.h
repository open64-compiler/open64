/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#ifndef B2W_COMMON_H
#define B2W_COMMON_H

//*****************************************************************************
// the JNI implementation front end
//  -- transform Bytecode to whirl through libspin
//*****************************************************************************

//=============================================================================
// JNI Declarations
//=============================================================================
#include "io_xcalibyte_BGenDriver.h"

//=============================================================================
// From libstdc++/libc++
//=============================================================================
#include    <stdio.h>
#include    <stdlib.h>
#include    <sys/stat.h>
#include    <unordered_map>
#include    <stdarg.h>
#include    <vector>
#include    <string>

//=============================================================================
// Global Framework From Common/com
//=============================================================================
#include    "defs.h"               // for all basic typedefs
#include    <cmplr_segmented_array.h>
#include    "glob.h"
#include    "erglob.h"
#include    "errors.h"
#include    "symtab.h"
#include    "const.h"
#include    "pu_info.h"
#include    "config.h"
#include    "file_util.h"           // for Last_Pathname_Component
#include    "java_defs.h"

//=============================================================================
// Whirl API
//=============================================================================
#include    "wn.h"
#include    "wn_util.h"
#include    "wn_simp.h"
#include    "ir_reader.h"
#include    "ir_bwrite.h"
#include    "c_int_model.h"
#include    "dwarf_DST_dump.h"
#include    "mempool.h"
#include    "tracing.h"
#include    "util.h"

//=============================================================================
// For config related (Machine/Target/Host related)
//=============================================================================
#include    "targ_sim.h"
#include    "config_opt.h"	        // for Div_Split_Allowed
#include    "config_debug.h"
#include    "config_list.h"
#include    "config_targ_opt.h"
#include    "controls.h"
#include    "erlib.h"
#include    "flags.h"
#include    <cmplrs/rcodes.h>


#if defined(BUILD_OS_DARWIN)
#include    <limits.h>
#else       /* defined(BUILD_OS_DARWIN) */
#include    <values.h>
#endif      /* defined(BUILD_OS_DARWIN) */
#include    <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include    <darwin_elf.h>
#include <map>

#else       /* defined(BUILD_OS_DARWIN) */
#include    <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */


typedef char *      CHPTR;
extern  BOOL        List_Enabled;
extern  INT         Opt_Level;
extern  BOOL        Enable_WFE_DFE;
extern  BOOL        Disable_Simplification_For_FE;
extern  BOOL        TARGET_64BIT;
extern  void        Process_TLS_Stress_Model(const char *p);
extern  BOOL        gv_cond_expr;
extern  void        B2W_print_backtrace();

/* ============================================================================
 * Local data.
 * ==========================================================================*/
#ifdef  MONGOOSE_CIF
extern  mUINT32     Cif_Level;
#endif

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION      ".B"	 /* ACIR file */
#define	ERR_FILE_EXTENSION      ".e"	 /* Error file */
#define	TRC_FILE_EXTENSION      ".t"	 /* Trace file */
#define	CLASS_FILE_EXTENSION    ".class" /* Class file */

//=============================================================================
//    From Wgen_Decl Prefix ended.
//=============================================================================
extern  const char *ABI_Name;

//=============================================================================
// From wgen_misc.cxx
//=============================================================================
extern  BOOL        c_omit_external;

//=============================================================================
// Made by Jason, for tracing and logging purpose.
//=============================================================================
#define Tracing()               Tracing_Enabled
#define Is_Trace_Ex(cond,parm)  if(cond) { fprintf parm; fflush(TFile); }
#define NULLPTR                 NULL
#define STANDARD_STR_LEN        1000
#define B2W_INIT_NO_MASK        0
#define B2W_INIT_MASK           1
#define B2W_INIT_SYM_MASK       2
#define B2W_INIT_I1_CST_MASK    3
#define B2W_INIT_I2_CST_MASK    4
#define B2W_INIT_I4_CST_MASK    5
#define B2W_INIT_I8_CST_MASK    6
#define B2W_INIT_F4_CST_MASK    7
#define B2W_INIT_F8_CST_MASK    8
#define B2W_INIT_STR_CST_MASK   9
#define B2W_INIT_PAD_MASK       10
#define B2W_INIT_U1_CST_MASK    21
#define B2W_INIT_U2_CST_MASK    22
#define B2W_INIT_U4_CST_MASK    23
#define B2W_INIT_U8_CST_MASK    24
#define B2W_INIT_OFST_MASK      31
#define B2W_INIT_LABEL_MASK     35
#define B2W_INIT_TCON_U4_MASK   40
#define B2W_INIT_TCON_U8_MASK   40
#define B2W_INIT_START_BLK      1000
#define B2W_INIT_END_BLK        2000
#define B2W_INIT_ENTER_BLK_MASK 1500
#define B2W_INIT_EXIT_BLK_MASK  2500
#define B2W_INIT_NOP            3000
//extern  PU_Info               *PU_Tree_Root;

#define B2W_FLAG_KIND_EXPORT            1
#define B2W_FLAG_KIND_SCLASS            2
#define B2W_FLAG_KIND_TY_FLAGS          3
#define B2W_FLAG_KIND_STB               4
#define B2W_FLAG_KIND_STFLAG            5
#define B2W_FLAG_KIND_CLEAR_STFLAG      6
#define B2W_FLAG_KIND_ST_EXT_FLAG       7
#define B2W_FLAG_KIND_CLEAR_ST_EXT_FLAG 8
#define B2W_FLAG_KIND_ST_TY             9
#define B2W_FLAG_KIND_SYM_SRCPOS        10

#define B2W_LVL_ALL             0xFFFFFFFF
#define B2W_LVL_ALL_EXEC        0x1
#define B2W_LVL_VERBOSE         0x2
#define B2W_LVL_INSERT          0x4
#define B2W_LVL_WN_MINOR        0x8
#define B2W_LVL_DEBUG           0x10
#define B2W_LVL_WN_MIDDLE       0x20
#define B2W_LVL_TRACE           0x40
#define B2W_LVL_WN_IMP          0x80
#define B2W_LVL_INFO            0x100
#define B2W_LVL_FLOW            0x200
#define B2W_LVL_DATA            0x400
#define B2W_LVL_WN_MAP          0x800
#define B2W_LVL_EH              0x1000
#define B2W_LOG(level)          (B2W_CONTEXT::Get_log_level() & level)

// Failing Echo Shortcuts
#define B2W_fail_symbol(sym,expr)    (B2W_get_stptr(symbol)->Print(stderr), expr);
#define B2W_fail_type(type,expr)     (B2W_get_typtr(type)->Print(stderr), expr);
#define Is_True_Type(cond, parm, type)  if(!cond){B2W_get_typtr(type)->Print(stderr);}; Is_True(cond, parm);
#define Is_True_Symbol(cond, parm, sym)  if(!cond){B2W_get_stptr(symbol)->Print(stderr);};  Is_True(cond, parm);
#ifdef Is_True
#define Is_Valid(cond, parm) if(!(cond)){ \
    B2W_print_backtrace(); \
    Is_True(cond, parm); \
}
#endif

union B2W_INIT_DATA{
  UINT64      uintData;
  double      doubleData;
  float       floatData;
  char        charDatas[8];
};

enum INITIAL_VAL_K{
  IV_INT_CST       = 1,
  IV_FLOAT_CST     = 2,
  IV_DOUBLE_CST    = 3,
  IV_COMPLEX_CST   = 4,
  IV_STRING_CST    = 5,
  IV_VECTOR_CST    = 6,
  IV_CONVERT_EXPR  = 7,
  IV_NOP_EXPR      = 8,
  IV_ADDR_EXPR     = 9,
  IV_PLUS_EXPR     = 10,
  IV_CONSTRUCTOR   = 11
};

enum FILE_SYSTEM{
    FILE_SYSTEM_UNIX = 1,
    FILE_SYSTEM_WINDOWS = 2,
};

using   std::pair;
typedef std::vector<WN*>    WN_VECCXX;
typedef std::vector<UINT>   U32VEC;
typedef U32VEC::iterator    U32VECI;
typedef MEM_POOL *          MPPTR;
typedef WN_MAP_TAB *        MTABPTR;
typedef pair<CHPTR,UINT>    CUPAIR;
typedef std::vector<CUPAIR> CUVEC;
typedef pair<CUPAIR,UINT>   CUPAIRUPAIR;
typedef std::vector<CUPAIRUPAIR> CUUPAIRVEC;
typedef enum INITIAL_VAL_K  INIVK;
typedef const char *        CCHPTR;
typedef long double         FLOAT16;
typedef double              FLOAT8;
typedef std::vector<INT64>  I64VEC;
typedef std::vector<UINT64> U64VEC;

typedef std::unordered_map<std::string, TY_IDX> NAME_TY_MAP;
typedef std::unordered_map<std::string, ST_IDX> NAME_ST_MAP;
typedef std::unordered_map<UINT64,UINT64>       U6464MAP;
typedef std::pair<CHPTR, UINT64>                CHU64P;
typedef std::unordered_map<WN*, WN*>            W2WMAP;

class B2W_CONTEXT{
private:
  static PU_Info         *_pu_root;
  static MPPTR            _mem_pool_ptr;
  static PU_Info        **_pu_info_tab;
  static WN_VECCXX        _curr_entry_wn;      //Not WN_VECTOR
  static WN_VECCXX        _pu_stmt_list;
  static CHPTR            _current_file_name;
  static INT              _max_file_name_len;
  static CUVEC            _dir_dst_list;
  static CUUPAIRVEC       _file_dst_list;
  static UINT             _current_dir;
  static UINT             _current_file;
  static INITO_IDX        _last_initv;
  static INT              _anon_num;
  static INT              _log_level;
  static U6464MAP         _temporaries;
  static UINT64           _temps_count;
  static UINT             _align_bytes;  // in bytes
  static W2WMAP           _wn_parent_map;
  static DST_IDX          _comp_unit_idx;
public:
  static void        Cleanup_files (BOOL, BOOL);
  static void        Init_context  (INT abi_bit_width);
  static CCHPTR      Prepare_Source (CHPTR file_name);
  static PU_Info    *Get_pu_info_tab (UINT pos);
  static void        Set_pu_info_tab (UINT pos, PU_Info * ptr);
  static void        Push_current_entry_wn(WN *wn);
  static void        Pop_current_entry_wn();
  static WN         *Current_entry_wn();
  static UINT        Get_align_width();
  static MPPTR       Get_pool();                 // set with set_pool
  static void        Set_pool(MPPTR);
  static PU_Info    *Get_pu_root();
  static void        Set_pu_root(PU_Info * root);
  // Stmt List inside PU
  static void        Push_pu_stmt(WN * stmt);
  static WN         *Pop_pu_stmt();
  static WN         *Pu_last_stmt();
  static void        Set_file(CCHPTR absolute_path, FILE_SYSTEM file_system_flag);
  static CUVEC      *Get_dir_dst_list();         //List of added DST dirs
  static CUUPAIRVEC *Get_file_dst_list();        //List of added DST files
  static UINT        Get_file_num();             //Current file number
  static void        Set_last_initv(INITO_IDX idx);
  static INITO_IDX   Get_last_initv();
  static CHPTR       Get_Name(CHPTR flawed_str);
  static INT         Get_default_align();
  static INT         Get_log_level();
  static void        Set_log_level(INT level);
  static void       *Get_temporaries(UINT64 val); // Returns the saved CONTENT
  static UINT64      Set_temporaries(void * data);// Returns the generated TEMP_ID
  static UINT        Get_target_ptr_size();
  static void        Set_wn_parent(WN *child, WN *parent);
  static void        Set_wn_parent_direct(WN *child, WN *parent);
  static WN         *Get_wn_parent(WN* child, BOOL is_assert);
                                             // is_assert for not returning null
  static void        Clear_wn_parent_map();
  static void        Set_comp_unit_dst(DST_IDX idx);
  static DST_IDX     Get_comp_unit_dst();
  static std::string Get_file_base_name(std::string &file_path, FILE_SYSTEM fs_flag);
  static std::string Get_file_folder_name(std::string &basicString, FILE_SYSTEM fs_flag);
};

#endif
