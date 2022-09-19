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


#ifndef ERRDESC_INCLUDED
#define ERRDESC_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: errdesc.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/errdesc.h,v $
 *
 * This header describes the structure used to create the error message
 * tables.  Do not reference the structure directly.  Only the
 * errors module should do that.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"

/* These error numbers are reported to users, and the initial number
 * should not interfere with those from the front-end.
 */
typedef enum {
  RAG_EN_FIRST = 2000,
  RAG_EN_NONE,		/* indicates error not assigned a number */
  RAG_EN_RTN_UNINIT,
  RAG_EN_FORMAL_REF,
  RAG_EN_UNINIT_REF,
  RAG_EN_FOLD_ARITH_TOO_BIG,
  RAG_EN_FOLD_ARITH_TOO_BIG2,
  RAG_EN_CONST_COPY_TOO_BIG,
  RAG_EN_CONST_COPY_TOO_BIG2,
  RAG_EN_GOPT_TOO_BIG,
  RAG_EN_GOPT_TOO_BIG2,
  RAG_EN_LNO,
  /* Add new warning numbers before this comment */
  RAG_EN_LAST
} Rag_Err_Num;

/* ====================================================================
 *
 * Error message descriptors
 *
 * ====================================================================
 */

/* Descriptor flag word masks: */
#define EM_User		0x8000	/* User error (vs. compiler) */
#define EM_Compiler	0x0000	/* Compiler error (vs. user) */
#define EM_Continuation	0x4000	/* Print message line only */
#define EM_Unknown	0x2000	/* Converted unknown code */
#define EM_Severity	0x000f	/* Severity level */

/* The maximum number of error parameters.  This value cannot be
 * changed (increased) without changing all of the error descriptors
 * and the error reporting routine interfaces.  Note, however, that
 * using continuation error messages are probably adequate for cases
 * with more parameters.
 */
#define MAX_ERR_PARMS	6

/* Define an error message descriptor: */
typedef struct {
    mINT16	ecode;		/* The error code */
    mINT16	flags;		/* Miscellaneous flags */
    Rag_Err_Num rag_errnum;	/* External error/warning number */
    char	*emsg;		/* The message format */
    mUINT8	parms;		/* The parameter count */
    mUINT8	kinds[MAX_ERR_PARMS];	/* Parameter types */
} ERROR_DESC;

/* The access functions: */
#define ED_code(p)		(p->ecode)
# define ED_phase(p)		(ED_code(p)/1000)
#define ED_flags(p)		(p->flags)
# define ED_user(p)		(ED_flags(p)&EM_User)
# define ED_compiler(p)		((ED_flags(p)&EM_User)==0)
# define ED_continuation(p)	(ED_flags(p)&EM_Continuation)
# define ED_unknown(p)		(ED_flags(p)&EM_Unknown)
# define ED_severity(p)		(ED_flags(p)&EM_Severity)
#define ED_format(p)		(p->emsg)
#define ED_parms(p)		(p->parms)
#define ED_kind(p,n)		(p->kinds[n])
#define ED_rag_errnum(p)	(p->rag_errnum)

/* Define the error descriptor table: */
typedef struct error_desc_table {
    INT		phase;		/* The phase number of this array */
    ERROR_DESC *descriptors;	/* The list of error descriptors */
    char       *name;		/* The phase name */
} ERROR_DESC_TABLE;

#ifdef __cplusplus
}
#endif
#endif /* ERRDESC_INCLUDED */
