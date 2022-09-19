/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//
// Created by xc5 on 15/7/2018.
//

#include "jgen_include.h"
#include "jgen_global.h"
#include "err_host.tab"
#include <sstream>

#ifndef TARG_MIPS
BOOL TARGET_64BIT = TRUE;
#else
BOOL TARGET_64BIT = FALSE;  // 11953: On MIPS, n32 is default abi
#endif

int Debug_Level = 1;
// an_error_severity error_threshold = es_warning;

/* Static data:	command	line information: */
 char Dash [] = "-";

/* Internal flags: */
// BOOL Echo_Flag =	FALSE;	/* Echo command	lines */
 BOOL Delete_IR_File = FALSE;	/* Delete SGIR file when done */
 BOOL Run_vsaopt = TRUE;


     /****
// KEY
bool Did_Not_Terminate_Region = FALSE;


 gs_t *deferred_function_stack;
 INT32 deferred_function_i;
 INT32 deferred_function_max;
 BOOL flag_keep_inline_functions = FALSE;
 UINT32 WGEN_PU_count = 0;
 BOOL finish_alias = FALSE;

// keep pu and pu_info
extern PU_Info *PU_Tree_Root;
 PU_Info *PU_Info_Table     [258] = {0};


 ST      *Return_Address_ST [258] = {0};
 BOOL map_mempool_initialized = FALSE;
 MEM_POOL Map_Mem_Pool;

      ****/
int trace_verbose = 0;
// to manage the current function
 gs_t curr_namespace_decl = NULL;

 int __ctors = 0;
 int __dtors = 0;

 gs_t *func_decl_stack = NULL;
 INT func_decl_stack_size = 0;
 INT func_decl_stack_top = -1;


int  WGEN_Keep_Zero_Length_Structs = TRUE;
PU_Info *PU_Tree_Root = NULL;
int Reg_Parm_Count = 0;
BOOL SSE_Reg_Parm = FALSE;


//BOOL wgen_invoke_inliner = FALSE;
//int lineno = 0;
//char *Spin_File_Name = NULL;
//FILE *Spin_File = NULL;
//BOOL flag_no_common = FALSE;
//int pstatic_as_global = 0;
//int emit_exceptions = -1;
//BOOL opt_regions = 0;
//BOOL lang_cplus = FALSE;
//BOOL c_omit_external = TRUE;
//BOOL keep_inline_functions=FALSE;
//BOOL gen_pic_code = FALSE;
//BOOL tls_stress_test = FALSE;

// The following taken from gnu/flags.h
enum debug_info_level
{
    DINFO_LEVEL_NONE,     /* Write no debugging info.  */
    DINFO_LEVEL_TERSE,    /* Write minimal info to support tracebacks only.  */
    DINFO_LEVEL_NORMAL,   /* Write info for all declarations (and line table). */
    DINFO_LEVEL_VERBOSE   /* Write normal info plus #define/#undef info.  */
};


void get_err_tables(){
 Set_Error_Tables ( Phases, host_errlist );
}


void Cleanup_Files(BOOL a, BOOL dotofile){

}


static std::vector<WN*> curr_entry_wn;
static void Push_Current_Entry_WN(WN *wn) { curr_entry_wn.push_back(wn); }
static void Pop_Current_Entry_WN() { curr_entry_wn.pop_back(); }

extern void logger(string str){
  cout << str << endl;
}

extern void logger(const char *str){
  cout << str << endl;
}

using std::stringstream;
string int2str(int a){
  stringstream ss;
  ss<<a;
  return ss.str();
};
string long2str(long a){
 stringstream ss;
 ss<<a;
 return ss.str();
};
string ll2str(long long a){
 stringstream ss;
 ss<<a;
 return ss.str();
};
string char2str(char a){
 stringstream ss;
 ss<<a;
 return ss.str();
};
string double2str(double a){
 stringstream ss;
 ss<<a;
 return ss.str();
};

namespace JGEN{
 BOOL Config::Keep_Zero_length_structs = FALSE;
 bool Config::treat_static_as_global = FALSE;
 bool Config::need_inliner = FALSE;
 bool Config::processing_function_prototypes = FALSE;
 bool Config::expanding_function_definition = FALSE;
 bool Config::do_not_parse_common = FALSE; //flag_no_common
 bool Config::lang_cplus = FALSE;
 bool Config::lang_oop = FALSE; // lang_java || lang_cplusˆ
}