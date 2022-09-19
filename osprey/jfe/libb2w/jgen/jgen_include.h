//
// Created by xc5 on 10/7/2018.
//

#ifndef OSPREY_JGEN_INCLUDE_H
#define OSPREY_JGEN_INCLUDE_H

//******************************************************
// the driver for pathgcc front end
//  -- transform spin to whirl through libspin
//******************************************************
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <cmplrs/rcodes.h>
#include "gspin-wgen-interface.h"
#include "defs.h"
#include "glob.h"
#include "erglob.h"

//#include "err_host.tab"

#include "errors.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "config.h"
#include "file_util.h" // for Last_Pathname_Component
#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
//#include "wgen_decl.h"
#include "c_int_model.h"
#include "dwarf_DST_dump.h"
#include "targ_sim.h" // PUSH_RETURN_ADDRESS_ON_STACK
#include "wgen_omp_directives.h"

/***  Wgen _ Misc Header Start    **/
#include "config.h"
#include "config_opt.h"	// for Div_Split_Allowed
#include "config_debug.h"
#include "config_list.h"
#include "config_targ_opt.h"
#include "controls.h"
#include "erglob.h"
#include "erlib.h"
#include "file_util.h"
#include "flags.h"
#include "glob.h"
#include "mempool.h"
#include "tracing.h"
#include "util.h"
#include "errors.h"
#include <stdarg.h>

#include "wn.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "symtab.h"
#include "pu_info.h"
#include "ir_reader.h"
#include "ir_bwrite.h"
#include "c_int_model.h"
#include <unordered_map>


extern "C" {
#include "gspin-wgen-interface.h"
}

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */


extern BOOL List_Enabled;
extern INT Opt_Level;
extern BOOL Enable_WFE_DFE;
extern BOOL Disable_Simplification_For_FE;

extern BOOL TARGET_64BIT;
//extern BOOL wgen_invoke_inliner;
//extern int lineno ;
//extern char *Spin_File_Name ;
//extern FILE *Spin_File ;
//extern BOOL flag_no_common ;
//extern int pstatic_as_global ;
//extern int emit_exceptions;
//extern BOOL opt_regions ;
//extern BOOL lang_cplus ;
//extern BOOL c_omit_external ;
//extern BOOL keep_inline_function;
//extern BOOL gen_pic_code ;
//extern BOOL tls_stress_test ;
extern void Process_TLS_Stress_Model(const char *p);
//extern BOOL enable_cxx_openmp ;
//extern gs_t program;
extern INT Debug_Level ;	/* -gn: debug level */

/***

   Starting the wgen_decl variable(wgen_misc.h)

 ***/

extern void Initialize_IRB (void);	/* In lieu of irbutil.h */
extern void WGEN_Omp_Init (void);
// When region optimization is enabled using -foptimize-regions, we try not
// to close a region as soon as a call stmt finishes. We try to keep it open
// and include as many calls as possible.
// If we got an opportunity but did not close a region in WGEN_Stmt_Append,
// we set the following flag.


/* Specify how much debugging info to generate.  */
//extern enum debug_info_level;
// End gnu/flags.h data decl

extern BOOL gv_cond_expr;

extern BOOL JGEN_expanding_function_definition;


/* ====================================================================
 *
 * Local data.
 *
 * ====================================================================
 */

# define MAX_MSG_LEVEL 2
# define DEF_MSG_LEVEL 2

#ifdef MONGOOSE_CIF

extern mUINT32 Cif_Level;       	/* CIF level */

#define MAX_CIF_LEVEL 3
#define DEF_CIF_LEVEL 2
#endif /* MONGOOSE_CIF */

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION ".B"	/* ACIR file */
#define	IRD_FILE_EXTENSION ".D"	/* Intermediate data file */
#define	ERR_FILE_EXTENSION ".e"	/* Error file */
#define	LST_FILE_EXTENSION ".l"	/* Listing file */
#define	TRC_FILE_EXTENSION ".t"	/* Trace file */
#define DSTDUMP_FILE_EXTENSION ".fe.dst" /* DST dump-file extension */

// *****  Misc End

        // ST

#define JGEN_ST_VAR 1000000
#define JGEN_ST_FUNC 1000001
#define JGEN_ST_METHOD 1000002
#define JGEN_ST_PARM 1000003
#define JGEN_ST_CLASS 1020001
#define JGEN_ST_PACKAGE 1030001


        //TY

#define JGEN_TYPE_VOID 20000000
#define JGEN_TYPE_BOOLEAN 20000001
#define JGEN_TYPE_OFFSET 20000002
#define JGEN_TYPE_INTEGER 20000003
#define JGEN_TYPE_ENUMERATION 20000004
#define JGEN_TYPE_CHAR 20000005
#define JGEN_TYPE_BYTE 20000006
#define JGEN_TYPE_FLOAT 20000007
#define JGEN_TYPE_DOUBLE 20000008
#define JGEN_TYPE_NUMBER 20000009
#define JGEN_TYPE_HANDLE 20000010
#define JGEN_TYPE_POINTER 20000011
#define JGEN_TYPE_ARRAY 20000012
#define JGEN_TYPE_RECORD 20000013
#define JGEN_TYPE_METHOD 20000014
#define JGEN_TYPE_FUNCTION 20000015
#define JGEN_TYPE_UNION 20000016
#define JGEN_TYPE_PACKAGE 20000017

/*
 *  No longer used as Json_Visitor actually knows what means in JGEN_NODE_DECL;
 */
#define JGEN_DECL_CLASS 30000003
#define JGEN_DECL_METHOD 30000004
#define JGEN_DECL_UNKNOWN_KIND 30000099
#define JGEN_DECL_VAR 30009999
#define JGEN_DECL_BLOCK 30000007
#define JGEN_DECL_STMT 30000020
#define JGEN_DECL_OPERATOR 30000026


#define JGEN_ST_FLAG_MASK_PUBLIC 0
#define JGEN_ST_FLAG_MASK_WEAK 0

typedef unsigned int U32U;
typedef unsigned long long U64U;

#include <string>
#include <iostream>
using std::string;
using std::cout;
using std::endl;



#endif //OSPREY_JGEN_INCLUDE_H
