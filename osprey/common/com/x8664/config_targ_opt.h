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
 * Module: config_targ_opt.h
 * $Revision: 1.7 $
 * $Date: 04/12/21 14:57:25-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/common/com/x8664/SCCS/s.config_targ_opt.h $
 *
 * Revision history:
 *  11-Apr-96 - Original Version
 *
 * Description:
 *
 * External definitions for the -TARG group.
 *
 * Some of these variables are also defined in config.h or
 * MIPS/config_targ.h, for historical reasons.  In order to separate
 * the headers and minimize dependencies on changes to this one, new
 * group members should be defined only here, and their users should
 * explicitly include this file instead of having it indirectly
 * included (e.g. via config.h or MIPS/config_targ.h).  We should also
 * work towards removing most of the definitions from those headers.
 *
 * Exported variables should have names prefixed by "TARG_" to
 * facilitate moving them to a pushable struct later if desired.
 * See config_debug.[hc] for an example.
 *
 * ====================================================================
 * ====================================================================
 */

/* This file was named as config_TARG.h. 
 * Change it into config_targ_opt.h because windows can not distiguish
 * between "config_targ.h" and "config_TARG.h". */

#ifndef config_targ_opt_INCLUDED
#define config_targ_opt_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_targ_opt_h_rcs_id = "$Source: /home/bos/bk/kpro64-pending/common/com/x8664/SCCS/s.config_targ_opt.h $ $Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ====================================================================
 * List of global variables that are set by the -TARG option group
 * These appear only here, requiring explicit inclusion of this file.
 * ====================================================================
 */

/* General target control: */
extern const char *ABI_Name;		/* -TARG:abi=xxx */
extern const char *Processor_Name;	/* -TARG:processor=xxx */
extern const char *ISA_Name;		/* -TARG:isa=xxx */
extern INT16 Target_FPRs;	/* Number of target FP registers */
extern BOOL Pure_ABI;		/* Avoid non-ABI constructs? */

/* Fault handling: */
extern BOOL Force_FP_Precise_Mode;	/* Force precise FP traps? */
extern BOOL Force_Memory_Dismiss;	/* Force mem fault dismissal? */
extern BOOL Force_Page_Zero;		/* Force mapping page zero? */
extern BOOL Force_SMM;			/* Force sequential memory? */
extern char *FP_Excp_Max;		/* Max FP trap enables */
extern char *FP_Excp_Min;		/* Min FP trap enables */

/* Force calls to be indirect (i.e. use address in register)? */
extern BOOL Force_Jalr;

/* Miscellaneous target instruction features: */
extern BOOL Madd_Allowed;	/* Generate madd instructions? */
extern BOOL SYNC_Allowed;
extern BOOL Slow_CVTDL;

extern BOOL Target_MMX;        /* Generate mmx instructions? */
extern BOOL Target_MMX_Set;

extern BOOL Target_SSE;        /* Generate sse instructions? */
extern BOOL Target_SSE_Set;

extern BOOL Target_SSE2;        /* Generate sse2 instructions? */
extern BOOL Target_SSE2_Set;

extern BOOL Target_SSE3;        /* Generate sse3 instructions? */
extern BOOL Target_SSE3_Set;

extern BOOL Target_3DNow;       // Generate 3DNow instructions?
extern BOOL Target_3DNow_Set;

extern BOOL Target_SSE4a;       // Generate 3DNow instructions?
extern BOOL Target_SSE4a_Set;

extern BOOL Target_SSSE3;       // Generate SSSE3 instructions?
extern BOOL Target_SSSE3_Set;

extern BOOL Target_SSE41;       // Generate SSE41 instructions?
extern BOOL Target_SSE41_Set;

extern BOOL Target_SSE42;       // Generate SSE42 instructions?
extern BOOL Target_SSE42_Set;

extern BOOL Target_AES;         // Generate AES instructions?
extern BOOL Target_AES_Set;

extern BOOL Target_PCLMUL;      // Generate PCLMUL instructions?
extern BOOL Target_PCLMUL_Set;

extern BOOL Target_AVX;         // Generate AVX instructions?
extern BOOL Target_AVX_Set;

extern BOOL Target_XOP;         // Generate XOP instructions?
extern BOOL Target_XOP_Set;

extern BOOL Target_FMA;         // Generate FMA3 instructions?
extern BOOL Target_FMA_Set;

extern BOOL Target_FMA4;        // Generate FMA4 instructions?
extern BOOL Target_FMA4_Set;


extern int Target_x87_Precision;	// precision of x87 calculations

/* Itanium options: */
extern BOOL Itanium_a0_step;	/* a0 version of itanium chip */

/* ====================================================================
 * List of global variables that are set by the -TARG option group
 * These also appear in config_targ.h, and are implicitly included by
 * including config.h.  They should be removed from there when it is
 * convenient.
 * ====================================================================
 */

#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_targ_opt_INCLUDED */
