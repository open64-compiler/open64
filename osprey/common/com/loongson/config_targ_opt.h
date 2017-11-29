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
 * $Revision:
 * $Date:
 * $Author:
 * $Source:
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

#ifndef config_targ_opt_INCLUDED
#define config_targ_opt_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_targ_opt_h_rcs_id = "$Source $Revision";
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
extern char *ABI_Name;		/* -TARG:abi=xxx */
extern char *Processor_Name;	/* -TARG:processor=xxx */
extern char *ISA_Name;		/* -TARG:isa=xxx */
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
