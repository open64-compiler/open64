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


#ifndef erlib_INCLUDED
#define erlib_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: erlib.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/erlib.h,v $
 *
 * Revision history:
 *  08-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Define the program librarian and interprocedural analysis error
 * codes for use with the error message handler errors.c.  The
 * associated error descriptors may be found in the file erlib.desc.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *erlib_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_LIB	EP_LIB*1000

/* File manipulation error codes: */
#define EC_Lib_Exists	EC_BASE_LIB		/* str */
#define EC_Lib_Open	EC_BASE_LIB+1		/* str, err */
#define EC_Lib_Create	EC_BASE_LIB+2		/* str, err */
#define EC_Lib_Delete	EC_BASE_LIB+3		/* str, err */
#define EC_Lib_Close	EC_BASE_LIB+4		/* str, err */
#define EC_No_Lib	EC_BASE_LIB+5		/* str */
#define EC_Obj_Exists	EC_BASE_LIB+10		/* str */
#define EC_Obj_Open	EC_BASE_LIB+11		/* str, err */
#define EC_Obj_Create	EC_BASE_LIB+12		/* str, err */
#define EC_Obj_Delete	EC_BASE_LIB+13		/* str, err */
#define EC_Obj_Close	EC_BASE_LIB+14		/* str, err */
#define EC_No_Obj	EC_BASE_LIB+15		/* str */
#define EC_Invalidated	EC_BASE_LIB+16		/* str */
#define EC_Lib_Missing_Body EC_BASE_LIB+17	/* str */
#define EC_Lib_Invalid_PU	EC_BASE_LIB+18		/* str */
#define EC_Lib_Version	EC_BASE_LIB+19		/* int */
#define EC_Lib_Invalidating_File  EC_BASE_LIB+20	/* str */

/* Interprocedural analysis: */

/* Sparse bit vector manipulation: */
#define EC_SBV_Ill_Elmt	EC_BASE_LIB+50		/* int, int */

#ifdef __cplusplus
}
#endif
#endif /* erlib_INCLUDED */
