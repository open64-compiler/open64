/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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
 *
 * Module: erfe.h
 * $Revision: 1.6 $
 * $Date: 05/06/21 17:21:29-07:00 $
 * $Author: gautam@jacinth.keyresearch $
 * $Source: kgccfe/SCCS/s.erfe.h $
 *
 * Revision history:
 *  12-Sep-89 - Original Version
 *  11-Mar-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Define the compiler front end error codes for use with the error
 * message handler errors.c.  The associated error descriptors may be
 * found in the file erfe.desc.
 *
 * ====================================================================
 * ====================================================================
 */


#ifndef erfe_INCLUDED
#define erfe_INCLUDED

#ifdef _KEEP_RCS_ID
static char *erfe_rcs_id = "$Source: kgccfe/SCCS/s.erfe.h $ $Revision: 1.6 $";
#endif /* _KEEP_RCS_ID */

#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_FE	EP_FE*1000

#define EC_Not_Same_Type EC_BASE_FE+0		/* str */
#define EC_Not_Typed	EC_BASE_FE+1		/* str */
#define EC_Dup_Decl	EC_BASE_FE+2		/* str */
#define EC_Not_Class	EC_BASE_FE+3		/* str */
#define EC_Undeclared	EC_BASE_FE+4		/* str */
#define EC_Intcon_Expected EC_BASE_FE+5		/* str */
#define EC_Fltcon_Expected EC_BASE_FE+6		/* str */
#define EC_Return_Num	EC_BASE_FE+7		/* int, int */
#define EC_Return_Type	EC_BASE_FE+8		/* str */
#define EC_Missing_Spec EC_BASE_FE+9		/* str */
#define EC_Sets_Num	EC_BASE_FE+10		/* int, int */
#define EC_Sets_Type	EC_BASE_FE+11		/* str */
#define EC_Skip_Stmt    EC_BASE_FE+12		/* str */
#define EC_Spec_Prev	EC_BASE_FE+13		/* str */
#define EC_Int_Expected EC_BASE_FE+14		/* str,str */
#define EC_Spec_Lib	EC_BASE_FE+15		/* str */
#define EC_Invalid_Regno EC_BASE_FE+16		/* str, int */
#define EC_GE_Only	EC_BASE_FE+20		/* str */
#define EC_PE_Only	EC_BASE_FE+21		/* str */
#define EC_Diff_Decl	EC_BASE_FE+22		/* str */
#define EC_Flag_Var	EC_BASE_FE+23		/* stab */

#ifdef TARG_NVISA
#define EC_Inline_Prototype	EC_BASE_FE+24		/* str */
#define EC_Inline_Ptr		EC_BASE_FE+25		/* str */
#define EC_Inline_Return_Values EC_BASE_FE+26		/* str */
#define EC_Inline_Parameters	EC_BASE_FE+27		/* str */
#endif

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS) || \
    defined(FRONT_END_FORTRAN)
#define EC_Null_Backptr	EC_BASE_FE+100		/* tree,str */
#endif /* FRONT_END_C || FRONT_END_CPLUSPLUS || FRONT_END_FORTRAN */

#ifdef KEY
#define EC_Unsupported_Type EC_BASE_FE+105	/* str */
#endif

#endif /* erfe_INCLUDED */
