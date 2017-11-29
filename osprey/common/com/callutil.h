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


#ifndef callutil_INCLUDED
#define callutil_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif


/* ====================================================================
 * ====================================================================
 *
 * Module: callutil.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/callutil.h,v $
 *
 * Revision history:
 *  07-Jun-92 - Original Version (extracted from stab.h)
 *
 * Description:
 *
 * This module contains the external interfaces (data structures and
 * prototypes) for routines which provide utility services related to
 * calls, entries, and returns.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *callutil_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/com/callutil.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/* Incomplete types to keep ANSI happy: */
struct arg_copy_list;	/* see irbcall.c */
struct bb;
struct einfo;
struct fld;
struct insch;
struct nd;
struct st;
struct stch;
struct ty;
#ifdef BACK_END
struct op;
#endif /* BACK_END */

#ifdef OLDCODE
/* Methods for returning a function result value: */
typedef enum {
  RSTYLE_NONE,		/* no result */
  RSTYLE_REG,		/* in one TN */
  RSTYLE_REG_PAIR,	/* in two TNs */
  RSTYLE_VIA_FIRST_ARG,	/* caller passes buffer address as first arg */
  RSTYLE_VIA_RETURN_ARG,
  RSTYLE_INT_REGS,
  RSTYLE_FLOAT_REGS,
  RSTYLE_VIA_COMMON_BUF, /* callee puts value in a common buffer */
  RSTYLE_VIA_LOCAL_BUF	/* callee returns address of local buf */
} RSTYLE;

/* How should a function result be returned?  These routines are
 * (potentially) language-specific, and therefore restricted to the
 * front ends.
 */
#ifdef FRONT_END
  extern RSTYLE How_To_Return ( struct ty * );
#ifdef OLDCODE
  /* In FEs' aux_misc.c: */
  extern RSTYLE How_To_Return_Type ( struct ty *, struct nd * );
#endif /* OLDCODE */
#endif
#endif /* OLDCODE */

/* Return the TN for the return address: */
extern struct tn * Get_Return_Address_TN
			( struct bb *bb, BOOL gen_load );

/* Find the adjustment of the given TN (usually SP_TN) in the entry
 * or exit code (bounded by BB_entry_sp_adj_.../BB_exit_sp_adj_...).
 */
extern struct insch * Find_Entry_TN_Adjustment_INSCH
			( struct bb *bb, struct tn *mod_tn );
extern struct insch * Find_Exit_TN_Adjustment_INSCH
			( struct bb *bb, struct tn *mod_tn );
#ifdef BACK_END
extern struct op * Find_Entry_TN_Adjustment_OP
			( struct bb *bb, struct tn *mod_tn );
extern struct op * Find_Exit_TN_Adjustment_OP
			( struct bb *bb, struct tn *mod_tn );
#endif /* BACK_END */

/* Mark an ST as referenced by a nested subprogram: */
extern void Mark_Nested_Reference ( struct st *st );

/* ====================================================================
 *
 * ARG_TY_LIST
 *
 * This structure provides the interface to the function
 * Calc_Arg_Offsets which calculates offsets of arguments.
 *
 * It is used for finding offsets (TNs) both of formals and of actuals.
 *
 * ====================================================================
 */

#ifdef FRONT_END_FORTRAN
/* Various treatment styles for multiple-entry formal parameters: */
typedef enum {
  FSTYLE_NONE,		/* Not yet determined */
  FSTYLE_NOMOVE,	/* No movement required */
  FSTYLE_RADICAL,	/* Move to radical copied formal */
  FSTYLE_MEMORY		/* Move to memory copied formal */
} FSTYLE;
#endif /* FRONT_END_FORTRAN */

/* Each argument may consist of a number of components (normally one,
 * except for struct/class arguments):
 */
typedef struct arg_ty_comp {
  struct arg_ty_comp * next;	/* Next in list */
  struct ty * ctype;	/* Component type */
  INTSC	 csize;		/* Component size */
  struct nd * cnd;	/* Node for accessing component */
  struct tn * tn;	/* TN for component (i.e. parm register) */
  struct tn * tn_val;	/* TN where component is loaded (for call) */
  INTSC mem_ofst;	/* Memory offset for component if in memory */
  INT16 parm_reg;	/* Parameter register to use if in register */
  TOP op_store;		/* Store operation to use if in register */
  TOP op_copy;		/* Copy operation to use if in register */
} ARG_TY_COMP;

/* ATC field access macros: */
#define ATC_next(a)	((a)->next)
#define ATC_ctype(a)	((a)->ctype)
#define ATC_csize(a)	((a)->csize)
#define ATC_cnd(a)	((a)->cnd)
#define ATC_cnd(a)	((a)->cnd)
#define ATC_tn(a)	((a)->tn)
#define ATC_tn_val(a)	((a)->tn_val)
#define ATC_mem_ofst(a)	((a)->mem_ofst)
#define ATC_parm_reg(a)	((a)->parm_reg)
#define ATC_op_store(a)	((a)->op_store)
#define ATC_op_copy(a)	((a)->op_copy )

/* An argument type list contains information about the full parameter,
 * and the first element of a list of components.  We handle such lists
 * as an array, so there is no explicit list link.
 */
typedef struct arg_ty_list {
  struct st *sym;	/* Formal parameter symbol (NULL for calls) */
  struct ty *type;	/* Parameter type */
  struct nd * pnd;	/* Parameter node */
  ARG_TY_COMP atc;	/* First (usually only) component */
  mINT8 flags;		/* Attributes */
  mINT8 fstyle;		/* Multiple-entry formal treatment style */
  struct st *cform;	/* Copied formal symtab entry */
  struct einfo *entry;	/* Entry this ATL belongs to */
  struct arg_copy_list *copies;	/* Head of copies list */
  struct arg_ty_list *next_copy;	/* Next copy in list */
} ARG_TY_LIST;

/* Attribute flag masks: */
#define ATL_IS_REF	0x01	/* Reference parameter */
#define ATL_IS_IN_TN	0x02	/* Argument is in TN (register) */
#define ATL_ON_COPIES	0x04	/* Argument is on copy list ... */
#define ATL_IS_VARARGS	0x08	/* Argument is from varargs ... */
#define ATL_DUP_FLOAT	0x10	/* Duplicate float varargs argument ... */

/* ATL field access macros: */
#define ATL_next(a)	((a)+1)
#define ATL_sym(a)	((a)->sym)
#define ATL_type(a)	((a)->type)
#define ATL_pnd(a)	((a)->pnd)
#define ATL_atc(a)	(&((a)->atc))
# define ATL_ctype(a)	 (ATC_ctype(ATL_atc(a)))
# define ATL_csize(a)	 (ATC_csize(ATL_atc(a)))
# define ATL_cnd(a)	 (ATC_cnd(ATL_atc(a)))
# define ATL_tn(a)	 (ATC_tn(ATL_atc(a)))
# define ATL_tn_val(a)	 (ATC_tn_val(ATL_atc(a)))
# define ATL_mem_ofst(a) (ATC_mem_ofst(ATL_atc(a)))
# define ATL_parm_reg(a) (ATC_parm_reg(ATL_atc(a)))
# define ATL_op_store(a) (ATC_op_store(ATL_atc(a)))
# define ATL_op_copy(a)  (ATC_op_copy(ATL_atc(a)))
#define ATL_flags(a)	((a)->flags)
#define ATL_fstyle(a)	((a)->fstyle)
#define ATL_cform(a)	((a)->cform)
#define ATL_entry(a)	((a)->entry)
#define ATL_copies(a)	((a)->copies)
#define ATL_next_copy(a) ((a)->next_copy)

#define ATL_is_ref(a)		(ATL_flags(a) & ATL_IS_REF)
#define Set_ATL_is_ref(a)	(ATL_flags(a) |= ATL_IS_REF)
#define Reset_ATL_is_ref(a)	(ATL_flags(a) &= ~ATL_IS_REF)
#define ATL_is_in_tn(a)		(ATL_flags(a) & ATL_IS_IN_TN)
#define Set_ATL_is_in_tn(a)	(ATL_flags(a) |= ATL_IS_IN_TN)
#define Reset_ATL_is_in_tn(a)	(ATL_flags(a) &= ~ATL_IS_IN_TN)
#define ATL_on_copies(a)	(ATL_flags(a) & ATL_ON_COPIES)
#define Set_ATL_on_copies(a)	(ATL_flags(a) |= ATL_ON_COPIES)
#define Reset_ATL_on_copies(a)	(ATL_flags(a) &= ~ATL_ON_COPIES)
#define ATL_is_varargs(a)	(ATL_flags(a) & ATL_IS_VARARGS)
#define Set_ATL_is_varargs(a)	(ATL_flags(a) |= ATL_IS_VARARGS)
#define ATL_dup_float(a)	(ATL_flags(a) & ATL_DUP_FLOAT)
#define Set_ATL_dup_float(a)	(ATL_flags(a) |= ATL_DUP_FLOAT)

/* --------------------------------------------------------------------
 * Subprogram call utilities.
 * --------------------------------------------------------------------
 */

/* Determine whether two fields in a KIND_STRUCT overlap. */
BOOL No_Overlap ( struct fld *f1, struct fld *f2 );

/* Given a node for a PU, identify the type of the routine: */
extern struct ty * Identify_Callee_Type ( struct nd *pu );

/* What is the actual type of a formal parameter?  Adjust for default
 * promotions or reference parameters:
 */
extern struct ty * Formal_Parameter_Type ( struct st *parm );

/* Given a reference formal, construct a symbol representing its base
 * address, and convert its addressing to reflect that:
 */
extern struct st * Make_Formal_Base_Symbol ( struct st *formal_sym );

/* The two functions below make an ARG_TY_LIST for the formals
 * of a function or for a call. The ARG_TY_LIST is terminated
 * by am ARG_TY_LIST record having ATL_ty() == NULL. ATL_is_ref
 * is set to TRUE if arg is reference arg.  In this case ATL_type()
 * is the dedicated type associated with Pointer_Mtype.  Otherwise
 * (for a value parameter), ATL_type gives the type of the value
 * parameter. 
 *
 * These two routines essentially allow use of the single routine
 * Calc_Arg_Offsets above to determine the offsets of both formals
 * and actuals.
 */
extern ARG_TY_LIST *Prepare_Atl_Of_Call ( struct nd *t );
extern ARG_TY_LIST *Prepare_Atl_Of_Formals ( struct nd *t,
					     struct stch *args );

extern void Trace_ATL ( ARG_TY_LIST *atl );
extern void Trace_ATL_List ( struct nd *t, ARG_TY_LIST *atl );

#ifdef __cplusplus
}
#endif
#endif /* callutil_INCLUDED */
