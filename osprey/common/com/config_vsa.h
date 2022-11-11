/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// =============================================================================
// =============================================================================
//
// Module: opt_vsa.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_vsa.cxx,v $
//
// Revision history:
//  Spring 2018 - Original Version
//
// =============================================================================
//
// Description:
//
// Declare global flag variables for -VSA group options.
// This file is included in common/com/config.cxx, but should not be
// otherwise used outside of VSA.
//
// =============================================================================
// =============================================================================


#pragma hdrstop
#ifndef config_vsa_INCLUDED
#define config_vsa_INCLUDED

#include "flags.h"

extern BOOL VSA_Uiv;
extern BOOL VSA_Aob;
extern BOOL VSA_Aofb;
extern BOOL VSA_Npd;
extern BOOL VSA_Rbc;
extern BOOL VSA_Ubf;
extern BOOL VSA_Uaf;
extern BOOL VSA_Msf;
extern BOOL VSA_Dbz;
extern BOOL VSA_Ddv;
extern BOOL VSA_Ral;
extern BOOL VSA_Dbf;
extern BOOL VSA_Udr;
extern BOOL VSA_Fam;
extern BOOL VSA_Fmt;
extern BOOL VSA_Ecb;
extern BOOL VSA_Tnh;
extern BOOL VSA_Ddc;
extern BOOL VSA_Rcd;
extern BOOL VSA_Scb;
extern BOOL VSA_Sse;
extern BOOL VSA_Uic;
extern BOOL VSA_Cda;
extern BOOL VSA_Vra;
extern BOOL VSA_Hor;
extern BOOL VSA_Hor_Unification; // Unify HOR
extern BOOL VSA_Rvsa;
extern BOOL VSA_Xsca;            // Report coding standard compliance issue
extern BOOL VSA_Customized_Rule; // Turn on or off customized rule "CUS-n"
extern BOOL VSA_Xcalibyte_Rule;  // Turn on or off xcalibyte rule like 'CSS/CSL/etc"
extern BOOL VSA_Multi_Thread_Rule; // Turn on or off multi-thread rules
extern BOOL VSA_Muchi;
extern BOOL VSA_Not_Muchi;
extern BOOL VSA_Alias_Intent;    // Apply intent to alias rules
extern BOOL VSA_Alias_Local_Lda; // Is LDA of local variable aliased with call
extern BOOL VSA_Vra_Mayzero;     // report NPD/DBZ if vra says value may be zero
extern BOOL VSA_Vra_Ivar;        // Support ivar in value range analysis
extern BOOL VSA_Param_Uiv;       // report UIV on parameter, merged into VSA_Warn_On_Param
extern BOOL VSA_Extern_Uiv;      // report UIV on call to extern with LDA without def
extern BOOL VSA_Prop_Vardef;     // perform propagation in vsa vardef phase
extern BOOL VSA_Vsym_Uiv;        // report UIV on vsym
extern BOOL VSA_Vsym_Npd;        // report NPD on vsym
extern BOOL VSA_NPD_Nullbase;    // report NPD on null base
extern BOOL VSA_Udr_Null;        // report UDR on free(NULL)
extern BOOL VSA_MSF_Addr_Pass;   // report MSF on memory been address passed
extern BOOL VSA_Vsym_Memcall;    // proactive creation of vsym at malloc/free call
extern BOOL VSA_Global_Var_Zero; // assume global var is zero
extern BOOL VSA_EH;              // EH support
extern BOOL VSA_EH_LEVEL;        // EH check callby level count
extern INT  VSA_EH_Spec_Default; // 0: throw nothing, 1: throw anything
extern BOOL VSA_Devirt_Aggr;     // Aggressive devirtualization
extern INT  VSA_Devirt_Aggr_Threshold; // Threshold for max devirt candidate for the given type
extern INT  VSA_Connect_Icall_Threshold; // Threshold for max icall candidates
extern BOOL VSA_Global_Muchi;          // Create mu chi for global var if it's only referenced in callee
extern BOOL VSA_Global_Fld_Muchi;      // Create mu chi for global var field
extern BOOL VSA_Global_Vor_Muchi;      // Create mu chi for VOR of global var
extern BOOL VSA_Global_Fld_Vor_Muchi;  // Create mu chi for VOR of global var fields
extern BOOL VSA_HOA_Prop;              // heap obj annotation propagation
extern BOOL VSA_Src_Tainted;           // treat memcpy source data tainted
extern BOOL VSA_Tainted_Int32C;        // report INT32-C on tainted variable
extern BOOL VSA_ST_Modified;           // Analyze is global variable is modified
extern BOOL VSA_HVA_Delay_Ho_Rename;   // Delay HO renaming in New HO/VO analysis
extern BOOL VSA_New_HVA;               // New HO/VO analysis
extern INT  VSA_New_HVA_Round;         // max iteration round for new HO/VO
extern BOOL VSA_New_HVA_Compat;        // New HO/VO analysis compatible with old one
extern BOOL VSA_New_HVA_Unification;   // Perform HO unification in New HO/VO analysis
extern BOOL VSA_New_Heap_Checker;      // VSA new heap checker
extern BOOL VSA_New_Npd_Checker;       // VSA new npd checker, for testing
extern BOOL VSA_New_Uiv_Checker;       // VSA new uiv checker
extern BOOL VSA_New_Cprop;             // VSA new constant propagation
extern BOOL VSA_Prop_Values;           // VSA propagate values
extern BOOL VSA_New_Cselect_Lower;     // VSA uses new cselect lower
extern BOOL VSA_Value_Graph;           // VSA uses value graph to check if control dependencies are satisfied
extern BOOL VSA_Value_Graph_Lazy;      // Lazy evaluate the value graph
extern BOOL VSA_Warn_On_Global;        // Will 'M' warning be reported on global
extern BOOL VSA_Warn_On_Param;         // Will 'M' warning be reported on param
extern BOOL VSA_Warn_On_Retval;        // Will 'M' warning be reported on return value
extern BOOL VSA_Warn_On_This;          // Will UIV/NOD warning be reported on c++ 'this'
extern BOOL VSA_Enable_Lib_Check;      // Will check libc/STL/BOOST implementations
extern BOOL VSA_Demo;                  // For demo only
extern BOOL VSA_Cd_Filter;             // Control dependency filter
extern BOOL VSA_Experimental;          // VSA experimental features
extern BOOL VSA_Check_Main_Entry;      // VSA check main entry feature
extern UINT VSA_Call_Stack_Size_Max;   // VSA check call stack size limit
extern UINT VSA_Call_Stack_Level_Max;  // VSA check call stack level limit
extern BOOL VSA_Java_Tmp;
extern BOOL VSA_Check_Devirt;
extern BOOL VSA_Compiletime_Triage;    // Vsa time triaging
extern BOOL VSA_Model_Lda;             // VSA memory model for LDA for variables
extern BOOL VSA_Maybe_Report;          // VSA report issue maybe happen
extern BOOL VSA_Ignore_Dup;           // VSA ignore duplicate report with same sink
extern STRING VSA_Authen_Token;   // Protection token for MiddleWare authentication
extern STRING VSA_Authen_Server;  // Protection server addr (ip/hostname) to Relay server
                                  //  (via Gateway if applicable)
extern BOOL VSA_Single_Report;    // output report in one .v file for xfa
extern BOOL VSA_Output_Json;      // output .v file in JSON
extern BOOL VSA_Output_Cbor;      // output .v file in Cbor
extern BOOL VSA_Output_Msgid;     // output message id in .v file
extern INT  VSA_Json_Version;     // output JSON .v format version
extern INT  VSA_Json_Path_Max;    // max path in JSON
extern INT  VSA_Path_Head_Max;    // max path for head part
extern INT  VSA_Path_Tail_Max;    // max path for tail part
extern UINT VSA_Du_Mem_High;      // high watermark for D-U memory usage in MB
extern UINT VSA_Du_Mem_Low;       // low watermark for D-U memory usage in MB
extern BOOL VSA_Emit_Whirl;       // emit whirl when VSA is on
extern BOOL VSA_CXX_Intrn;        // use c++ intrinisc instead of template instantiation
extern BOOL VSA_Checker_SE;       // symbolic execution in checker
extern BOOL VSA_Builtin_Jni;      // JNI builtin rules
extern BOOL VSA_Builtin_Recursion;// builtin recursion check
extern BOOL VSA_Builtin_CertC;    // CERTC builtin rules
extern BOOL VSA_Builtin_CertJ;    // CERTJ builtin rules
extern BOOL VSA_RBC_Exception;    // enable rule exceptions
extern BOOL VSA_Enable_DCE;       // Perform dead code elimination in VSA
extern BOOL VSA_Enable_FSM;       // FSM checks
extern BOOL VSA_Enable_TAG_OLD;   // old TAG checks
extern BOOL VSA_Enable_TAG;       // TAG checks
extern BOOL VSA_Enable_Prop_Tag;  // TAG propagation when no callee
extern BOOL VSA_Enable_Chi_Prop_Tag; // TAG propagation through statement chi
extern BOOL VSA_Enable_May_Tag;   // TAG may result
extern BOOL VSA_Enable_Taint_Model;// Taint param notion
extern BOOL VSA_Ignore_Asm;       // ignore ASM stmt during checking
extern BOOL VSA_Check_RT_version; // check rt version
extern BOOL VSA_Authen_Enabled;   // Protection: whether to turn on all procedures related;
extern BOOL VSA_Load_All_Sym;
#ifdef Is_True_On
extern BOOL VSA_Authen_Skip_Patch;// Protection: whether to skip code patches
#endif
extern BOOL VSA_OReload;          // Protection: whether to reload option seed
extern BOOL VSA_TCheck;           // Protection: whether to check time period check
extern BOOL VSA_Dump_Intrn_Table;
extern OPTION_LIST *VSA_Reg_Map;  // register map for embedded system
extern BOOL VSA_Enable_Java_Anno; // Java annotation
extern BOOL VSA_Filter_Include;   // Filter /usr/include directory
extern BOOL VSA_Alert_Huge_Path;  // Alert path exceed normal level
extern STRING VSA_Magic_Id;       // passed to the tooling environment
extern INT VSA_Issue_Id;          // passed in by the tooling environment

extern BOOL VSA_Checker_Aggr;             // aggressive checker
extern BOOL VSA_Checker_Vfr_Exact_Match;  // Checker match vfrs exactly at function boundary
extern INT  VSA_Checker_Max_Callby;       // U-D traverse max callby cnt
extern INT  VSA_Checker_Max_Callee;       // U-D traverse max callee cnt
extern INT  VSA_Checker_Max_Frame;        // U-D traverse max frame cnt
extern INT  VSA_Checker_Max_Srcpos_Child; // U-D traverse max srcpos child cnt
extern INT  VSA_Checker_Max_Srcpos_Data;  // U-D traverse max srcpos data cnt in each child
extern INT  VSA_Checker_Max_Path;         // Max path from the same source

extern INT  VSA_Issue_Certainty_Maybe;    // always report NPD/HEAP/etc to MAYBE

extern STRING VSA_VCG_Cg_Fname;           // File name to dump VSA Call graph to VCG

enum CSA_TARGET_ENV {
  VSA_ENV_APP,                            // scan user application
  VSA_ENV_USER_LIB,                       // scan user library
  VSA_ENV_SYS_LIB,                        // scan system library
  VSA_ENV_KERNEL,                         // scan operating system kernel
  VSA_ENV_DRIVER,                         // scan device driver
};

extern INT  VSA_Target_Env;               // VSA target env: app, lib, kernel, driver, etc
#endif /* config_vsa_INCLUDED */

