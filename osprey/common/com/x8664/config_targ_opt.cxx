/*
 *  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2007. PathScale, LLC.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * $Source: common/com/x8664/SCCS/s.config_targ_opt.cxx $
 *
 * Revision history:
 *  26-Feb-97 - Original Version, extracted from config.c.
 *
 * Description:
 *
 * Configure the -TARG group (included in config.c).
 * See config_targ_opt.h for usage conventions.
 * See config_targ.* for more general target configuration support.
 *
 * NOTE:  There is an approximate distinction between -TARG option
 * group flags and their configuration (in this file), and more generic
 * target configuration (in config_targ.c).  Note that the related
 * header file config_targ.h is included in config.h, and hence in most
 * source files, whereas config_targ_opt.h is only included directly, so
 * putting new -TARG option-related variables in here is to be
 * preferred to putting them in config_targ.[hc].
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_targ_opt.h"

/* ====================================================================
 * List of global variables that are set by the -TARG option group
 * ====================================================================
 */

/* General target control: */
const char *ABI_Name = NULL;		/* -TARG:abi=xxx */
const char *ISA_Name = NULL;		/* -TARG:isa=xxx */
const char *Processor_Name = NULL;	/* -TARG:processor=xxx */
static char * Platform_Name = NULL;
INT16 Target_FPRs = 0;		/* -TARG:fp_regs=nn */
BOOL Pure_ABI = FALSE;		/* Avoid non-ABI constructs? */

BOOL Target_MMX = TRUE;        /* -TARG:mmx=on/off */
BOOL Target_MMX_Set = FALSE;

BOOL Target_SSE = TRUE;        /* -TARG:sse=on/off */
BOOL Target_SSE_Set = FALSE;

BOOL Target_SSE2 = TRUE;        /* -TARG:sse2=on/off */
BOOL Target_SSE2_Set = FALSE;

BOOL Target_SSE3 = FALSE;       /* -TARG:sse3=on/off */
BOOL Target_SSE3_Set = FALSE;

// bug 4405
BOOL Target_3DNow = FALSE;       // -TARG:3dnow=on/off
BOOL Target_3DNow_Set = FALSE;

BOOL Target_SSE4a = FALSE;       // -TARG:sse4a=on/off
BOOL Target_SSE4a_Set = FALSE;

BOOL Target_SSSE3 = FALSE;       // -TARG:ssse3=on/off
BOOL Target_SSSE3_Set = FALSE;

BOOL Target_SSE41 = FALSE;       // -TARG:sse41=on/off
BOOL Target_SSE41_Set = FALSE;

BOOL Target_SSE42 = FALSE;       // -TARG:sse42=on/off
BOOL Target_SSE42_Set = FALSE;

BOOL Target_AES = FALSE;         // -TARG:aes=on/off
BOOL Target_AES_Set = FALSE;

BOOL Target_PCLMUL = FALSE;      // -TARG:pclmul=on/off
BOOL Target_PCLMUL_Set = FALSE;

BOOL Target_AVX = FALSE;         // -TARG:avx=on/off
BOOL Target_AVX_Set = FALSE;

BOOL Target_XOP = FALSE;         // -TARG:xop=on/off
BOOL Target_XOP_Set = FALSE;

BOOL Target_FMA = FALSE;         // -TARG:fma=on/off
BOOL Target_FMA_Set = FALSE;

BOOL Target_FMA4 = FALSE;        // -TARG:fma4=on/off
BOOL Target_FMA4_Set = FALSE;


// bug 4327
int Target_x87_Precision = 80;	// -TARG:x87_precision=32/64/80

/* Fault handling: */
BOOL Force_FP_Precise_Mode = FALSE;	/* Force precise FP traps? */
BOOL Force_Memory_Dismiss = FALSE;	/* Force mem fault dismissal? */
BOOL Force_Page_Zero = FALSE;		/* Force mapping page zero? */
BOOL Force_SMM = FALSE;			/* Force sequential memory? */
char *FP_Excp_Max = NULL;		/* Max FP trap enables */
char *FP_Excp_Min = NULL;		/* Min FP trap enables */
BOOL Flush_To_Zero = FALSE;		/* suppress fp underflow */

/* Miscellaneous target instruction features: */
BOOL Madd_Allowed = FALSE;		/* Generate madd instructions? */
BOOL Force_Jalr = FALSE;	/* Force calls via address in register */
static BOOL Slow_CVTDL_Set = FALSE;

BOOL Itanium_a0_step = FALSE;
BOOL SYNC_Allowed = TRUE;
BOOL Slow_CVTDL = FALSE;

/* Target machine specification options.  This group defines the target
 * ABI, ISA, processor, and FPR count.  It should also be the home for
 * options like specifying processor revisions.
 */
static OPTION_DESC Options_TARG[] = {
  { OVK_NAME,	OV_VISIBLE,	FALSE, "abi",		"ab",
    0, 0, 0, &ABI_Name,		NULL,
    "Specify the ABI to follow" },
  { OVK_NAME,	OV_VISIBLE,	FALSE, "isa",		"is",
    0, 0, 0, &ISA_Name,		NULL,
    "Specify the instruction set architecture to use" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "mmx",		"mmx",
    0, 0, 0,	&Target_MMX,	&Target_MMX_Set,
    "Enable MMX extensions" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "sse",		"sse",
    0, 0, 0,	&Target_SSE,	&Target_SSE_Set,
    "Enable SSE extensions" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "sse2",		"sse2",
    0, 0, 0,	&Target_SSE2,	&Target_SSE2_Set,
    "Enable SSE2 extensions" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "sse3",		"sse3",
    0, 0, 0,	&Target_SSE3,	&Target_SSE3_Set,
    "Enable SSE3 extensions" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "3dnow",		"3dnow",
    0, 0, 0,	&Target_3DNow,	&Target_3DNow_Set,
    "Enable 3DNow extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "sse4a",         "sse4a",
    0, 0, 0,    &Target_SSE4a,  &Target_SSE4a_Set,
    "Enable SSE4a extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "ssse3",         "ssse3",
    0, 0, 0,    &Target_SSSE3,  &Target_SSSE3_Set,
    "Enable SSSE3 extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "sse41",         "sse41",
    0, 0, 0,    &Target_SSE41,  &Target_SSE41_Set,
    "Enable SSE41 extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "sse42",         "sse42",
    0, 0, 0,    &Target_SSE42,  &Target_SSE42_Set,
    "Enable SSE42 extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "aes",           "aes",
    0, 0, 0,    &Target_AES,    &Target_AES_Set,
    "Enable AES extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "pclmul",        "pclmul",
    0, 0, 0,    &Target_PCLMUL, &Target_PCLMUL_Set,
    "Enable PCMUL extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "avx",           "avx",
    0, 0, 0,    &Target_AVX,    &Target_AVX_Set,
    "Enable AVX extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "xop",           "xop",
    0, 0, 0,    &Target_XOP,    &Target_XOP_Set,
    "Enable XOP extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "fma",           "fma",
    0, 0, 0,    &Target_FMA,    &Target_FMA_Set,
    "Enable FMA3 extensions" },
  { OVK_BOOL,   OV_VISIBLE,     FALSE, "fma4",          "fma4",
    0, 0, 0,    &Target_FMA4,   &Target_FMA4_Set,
    "Enable FMA4 extensions" },

  { OVK_INT32,	OV_VISIBLE,	FALSE, "x87-precision", "x87-precision",
    80, 32, 80, &Target_x87_Precision,	NULL,
    "Specify the precision of x87 floating-point calculations (32, 64, or 80)"},
  { OVK_NAME,	OV_VISIBLE,	FALSE, "platform",	"pl",
    0, 0, 0, &Platform_Name,	NULL,
    "Specify the target platform" },
  { OVK_NAME,	OV_VISIBLE,	FALSE, "processor",	"pr",
    0, 0, 0, &Processor_Name,	NULL,
    "Specify the target microprocessor" },

  /* Miscellaneous features: */
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "dismiss_mem_faults",	"dis",
    0, 0, 0,	&Force_Memory_Dismiss, NULL,
    "Force kernel to ignore memory faults (SIGSEGV/SIGBUS)" },
  { OVK_NAME,	OV_VISIBLE,	FALSE, "exc_max",	"exc_ma",
    0, 0, 0,	&FP_Excp_Max,	NULL,
    "Specify the only floating point exceptions which may be trapped" },
  { OVK_NAME,	OV_VISIBLE,	FALSE, "exc_min",	"exc_mi",
    0, 0, 0,	&FP_Excp_Min,	NULL,
    "Specify any floating point exceptions which must be trapped" },
  { OVK_BOOL,	OV_SHY,		FALSE, "force_jalr",	"",
    0, 0, 0,	&Force_Jalr,	NULL,
    "Force use of JALR instruction for all subprogram calls" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "flush_to_zero",	"",
    0, 0, 0,	&Flush_To_Zero, NULL,
    "Suppress floating point underflow exceptions" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "fp_precise",	"fp_p",
    0, 0, 0,	&Force_FP_Precise_Mode, NULL,
    "Force the processor into precise floating point mode" },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "fp_regs",	"fp_r",
    32, 16, 32, &Target_FPRs,	NULL,
    "Specify number of FP registers to use (16 or 32)" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "madd",		"",
    0, 0, 0,	&Madd_Allowed,	NULL,
    "Specify whether to generate MADD instructions" },
  { OVK_BOOL,	OV_SHY,		FALSE, "page_zero",	"",
    0, 0, 0,	&Force_Page_Zero, NULL,
    "Force the kernel to map page zero into address space" },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "slow_cvtdl", "",
    0, 0, 0,	&Slow_CVTDL,	&Slow_CVTDL_Set,
    "" },
  { OVK_BOOL,	OV_SHY,		FALSE, "seq_memory",	"seq",
    0, 0, 0,	&Force_SMM,	NULL,
    "Force the processor into sequential memory mode" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "sync", "",
    0, 0, 0,	&SYNC_Allowed,	NULL,
    "Specify whether to generate SYNC instructions" },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "pure",	"pu",
    0, 0, 0,	&Pure_ABI,	NULL,
    "Generate pure ABI-compliant code" },

  { OVK_BOOL,   OV_INTERNAL,    FALSE, "ma0_step", "",
    0, 0, 0,    &Itanium_a0_step, NULL,
    "" },

  /* Unimplemented options: */
  /* Obsolete options: */

  { OVK_COUNT }		/* List terminator -- must be last */
};


/* ====================================================================
 *
 * Configure_Source_TARG
 *
 * Same as Configure_Source except here we handle target specifics.
 *
 * ====================================================================
 */
static void
Configure_Source_TARG ( char *filename )
  /**  NOTE: filename CAN BE NULL */
{
}
