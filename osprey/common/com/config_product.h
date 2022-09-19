/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#ifndef config_product_INCLUDED
#define config_product_INCLUDED

// ============================================================================
// Define product prefix
// ============================================================================
#ifdef BUILD_MASTIFF
#define PRODUCT_NAME_PREFIX        "XCALIBYTE"
#define C_DRIVER_NAME              "xvsa"
#define CPP_DRIVER_NAME            "xvsa++"
#else
#define PRODUCT_NAME_PREFIX        "OPEN64"
#define C_DRIVER_NAME              "opencc"
#define CPP_DRIVER_NAME            "openCC"
#endif

// ============================================================================
// Define phase executable  names
// ============================================================================
#ifdef BUILD_MASTIFF
#define PHASE_SPIN_CC1_NAME        "mapfec"
#define PHASE_SPIN_CC1PLUS_NAME    "mapfex"
#define PHASE_WGEN_NAME            "mapirg"
#define PHASE_INLINE_NAME          "mapinl"
#define PHASE_BE_NAME              "mapcbe"
#define PHASE_IPL_NAME             "mapdls"
#else
#define PHASE_SPIN_CC1_NAME        "cc1"
#define PHASE_SPIN_CC1PLUS_NAME    "cc1plus"
#define PHASE_WGEN_NAME            "wgen"
#define PHASE_SPIN_CC142_NAME      "cc142"
#define PHASE_SPIN_CC1PLUS42_NAME  "cc1plus42"
#define PHASE_WGEN42_NAME          "wgen42"
#define PHASE_INLINE_NAME          "inline"
#define PHASE_BE_NAME              "be"
#define PHASE_IPL_NAME             "ipl"
#endif

// ============================================================================
// Define component shared library names
// ============================================================================
#ifdef BUILD_MASTIFF
#define BE_SO_NAME                 "maccom.so"
#define WOPT_SO_NAME               "macdfa.so"
#define LNO_SO_NAME                "maclpa.so"
#define VSA_SO_NAME                "macvsa.so"
#define IPL_SO_NAME                "macdls.so"
#define CG_SO_NAME                 "macdcg.so"
#define RBC_SO_NAME                "macrbc.so"
#else
#define BE_SO_NAME                 "be.so"
#define WOPT_SO_NAME               "wopt.so"
#define LNO_SO_NAME                "lno.so"
#define VSA_SO_NAME                "vsa.so"
#define IPL_SO_NAME                "ipl.so"
#define CG_SO_NAME                 "cg.so"
#define RBC_SO_NAME                "rbc.so"
#endif

#define WHIRL2C_SO_NAME            "whirl2c.so"
#define WHIRL2F_SO_NAME            "whirl2f.so"

#ifdef TARG_IA64
#define ORC_ICT_SO_NAME            "orc_ict.so"
#define ORC_INTEL_SO_NAME          "orc_intel.so"
#endif

// ============================================================================
// Define intermediate file names or extensions
// ============================================================================
#ifdef BUILD_MASTIFF
#define IRB_FILE_EXTENSION         ".R"
#else
#define IRB_FILE_EXTENSION         ".B"
#endif

#define CRASHFILE_NAME_TEMPLATE    "open64_crash_XXXXXX"
#define BE_DSTDUMP_FILE_EXTENSION  ".be.dst"
#define C_FILE_EXTENSION           ".c"
#define ERR_FILE_EXTENSION         ".e"
#define IRD_FILE_EXTENSION         ".D"
#define FTN_FILE_EXTENSION         ".f"
#define FE_DSTDUMP_FILE_EXTENSION  ".fe.dst"
#define IRA_FILE_EXTENSION         ".ir"
#define LST_FILE_EXTENSION         ".lst"
#define ASM_FILE_EXTENSION         ".s"
#define TRC_FILE_EXTENSION         ".t"
#define TLOG_FILE_EXTENSION        ".tlog"
#ifdef TARG_UWASM
#define OBJ_FILE_EXTENSION         ".uwm"
#else
#define OBJ_FILE_EXTENSION         ".o"
#endif


#endif /* config_product_INCLUDED */

