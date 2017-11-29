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


#ifndef wn2f_INCLUDED
#define wn2f_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:42 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/wn2f.h,v $
 *
 * Revision history:
 *  07-Apr-1995 - Original Version
 *
 * Description:
 *
 *   WN2F_CONTEXT: Information about the context in which a translation
 *           of a WN subtree into Fortran occurs.
 *
 *   WN2F_STATUS: The status of a translation of a WN subtree into 
 *           Fortran.
 *
 *   WN2F_Can_Assign_Types:
 *           This determines whether or not a value of type t1 can
 *           be used anywhere we expect a value of type t2.  When
 *           this condition is TRUE, yet t1 is different from t2,
 *           we expect the implicit Fortran type coersion to transform
 *           an object of one type to the other.
 *
 *   WN2F_Stmt_Newline:
 *           Use this to start each statement on a new line.
 *
 *   WN2F_Address_Of:
 *           Generates an expression to explicitly take the address 
 *           of the lvalue constituted by the tokens in the given
 *           token-buffer.
 *
 *   WN2F_Offset_Symref:
 *           Generate code to access the memory location denoted by
 *           the object type, based on the base-symbol, its given
 *           address type and the offset from the base of the symbol.
 *
 *   WN2F_Offset_Memref:
 *           Generate code to access the memory location denoted by
 *           the object type, based on the base-address expression,
 *           its given type and the offset from the base-address.
 *
 *   WN2F_initialize: This initializes any WN to Fortran translation
 *           and must always be called prior to any WN2F_translate()
 *           call.
 *
 *   WN2F_finalize: This finalizes any WN to Fortran translation
 *           and should be called after all processing related
 *           to a whirl2f translation is complete.
 *
 *   WN2F_translate:  Translates a WN subtree into a sequence of Fortran
 *           tokens, which are added to the given TOKEN_BUFFER.
 *
 *   WN2F_Emit_End_Stmt: a utility to insert an END statement for
 *            an f90 program which contains nested PUs
 *
 *   WN2F_Sum_Offsets:  Sums any ADD nodes encountered in an address tree
 *
 * ====================================================================
 * ====================================================================
 */


    /* ---- Result status and context of a WN to Fortran translation ---- */
    /* ------------------------------------------------------------------ */

typedef mUINT32 WN2F_STATUS;
#define EMPTY_WN2F_STATUS (WN2F_STATUS)0

typedef struct WN2F_Context
{
   mUINT32 flags;
   WN     *wn;
} WN2F_CONTEXT;
#define INIT_WN2F_CONTEXT {0, NULL}
#define reset_WN2F_CONTEXT(c) ((c).flags = 0U, (c).wn = NULL)

/* Indicates to a block translation that this is the body of a PU.
 */
#define WN2F_CONTEXT_NEW_PU 0x000000001
#define WN2F_CONTEXT_new_pu(c) ((c).flags & WN2F_CONTEXT_NEW_PU)
#define set_WN2F_CONTEXT_new_pu(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_NEW_PU)
#define reset_WN2F_CONTEXT_new_pu(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_NEW_PU)

/* Indicates to a block translation that an induction-step must be
 * inserted at the end of this block, before the loop-termination
 * label.
 */
#define WN2F_CONTEXT_INSERT_INDUCTION 0x000000002
#define WN2F_CONTEXT_insert_induction(c)\
   ((c).flags & WN2F_CONTEXT_INSERT_INDUCTION)
#define WN2F_CONTEXT_induction_stmt(c) (c).wn
#define set_WN2F_CONTEXT_induction_step(c, stmt)\
   ((c).flags = (c).flags | WN2F_CONTEXT_INSERT_INDUCTION,\
    (c).wn = stmt)
#define reset_WN2F_CONTEXT_induction_step(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_INSERT_INDUCTION,\
    (c).wn = NULL)

/* Indicates that we expect to dereference an address expression.
 * LDA or ARRAY nodes should not be translated unless this flag has
 * been set, other than when we can use an "address of" operator
 * in Fortran.
 */
#define WN2F_CONTEXT_DEREF_ADDR 0x000000004
#define WN2F_CONTEXT_deref_addr(c) ((c).flags & WN2F_CONTEXT_DEREF_ADDR)
#define set_WN2F_CONTEXT_deref_addr(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_DEREF_ADDR)
#define reset_WN2F_CONTEXT_deref_addr(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_DEREF_ADDR)

/* Indicates that we should not start the next statement on a new
 * line.  This only needs to be taken into account for statement
 * types where it is an issue.
 */
#define WN2F_CONTEXT_NO_NEWLINE 0x000000008
#define WN2F_CONTEXT_no_newline(c) ((c).flags & WN2F_CONTEXT_NO_NEWLINE)
#define set_WN2F_CONTEXT_no_newline(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_NO_NEWLINE)
#define reset_WN2F_CONTEXT_no_newline(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_NO_NEWLINE)

/* This flag indicates that we are in a context where we expect the
 * arguments to the current expression to evaluate to logically typed
 * values.
 */
#define WN2F_CONTEXT_HAS_LOGICAL_ARG 0x00000010
#define WN2F_CONTEXT_has_logical_arg(c)\
   ((c).flags & WN2F_CONTEXT_HAS_LOGICAL_ARG)
#define set_WN2F_CONTEXT_has_logical_arg(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_HAS_LOGICAL_ARG)
#define reset_WN2F_CONTEXT_has_logical_arg(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_HAS_LOGICAL_ARG)

/* This flag indicates that we are in a context where we expect the
 * current expression to evaluate to a logically typed arg.
 */
#define WN2F_CONTEXT_IS_LOGICAL_ARG 0x00000020
#define WN2F_CONTEXT_is_logical_arg(c)\
   ((c).flags & WN2F_CONTEXT_IS_LOGICAL_ARG)
#define set_WN2F_CONTEXT_is_logical_arg(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_IS_LOGICAL_ARG)
#define reset_WN2F_CONTEXT_is_logical_arg(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_IS_LOGICAL_ARG)

/* This flag indicates that we are in a context where we need not
 * enclose a Fortran expression in parenthesis (subexpressions may
 * still be enclosed in parenthesis.
 */
#define WN2F_CONTEXT_NO_PARENTHESIS 0x00000020
#define WN2F_CONTEXT_no_parenthesis(c)\
   ((c).flags & WN2F_CONTEXT_NO_PARENTHESIS)
#define set_WN2F_CONTEXT_no_parenthesis(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_NO_PARENTHESIS)
#define reset_WN2F_CONTEXT_no_parenthesis(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_NO_PARENTHESIS)

/* This flag indicates whether or not a Fortran IO control-list
 * should be emitted in keyword form.
 */
#define WN2F_CONTEXT_KEYWORD_IOCTRL 0x00000040
#define WN2F_CONTEXT_keyword_ioctrl(c)\
   ((c).flags & WN2F_CONTEXT_KEYWORD_IOCTRL)
#define set_WN2F_CONTEXT_keyword_ioctrl(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_KEYWORD_IOCTRL)
#define reset_WN2F_CONTEXT_keyword_ioctrl(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_KEYWORD_IOCTRL)

/* This flag indicates whether or not we are inside a Fortran IO statement.
 */
#define WN2F_CONTEXT_IO_STMT 0x00000080
#define WN2F_CONTEXT_io_stmt(c)\
   ((c).flags & WN2F_CONTEXT_IO_STMT)
#define set_WN2F_CONTEXT_io_stmt(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_IO_STMT)
#define reset_WN2F_CONTEXT_io_stmt(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_IO_STMT)

/* This flag indicates whether or not to dereference IO_ITEMS.
 */
#define WN2F_CONTEXT_DEREF_IO_ITEM 0x00000100
#define WN2F_CONTEXT_deref_io_item(c)\
   ((c).flags & WN2F_CONTEXT_DEREF_IO_ITEM)
#define set_WN2F_CONTEXT_deref_io_item(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_DEREF_IO_ITEM)
#define reset_WN2F_CONTEXT_deref_io_item(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_DEREF_IO_ITEM)

/* This flag indicates whether or not to replace an OPC_LABEL
 * item with an IOC_VARFMT_ORIGFMT item.
 */
#define WN2F_CONTEXT_ORIGFMT_IOCTRL 0x00000200
#define WN2F_CONTEXT_origfmt_ioctrl(c)\
   ((c).flags & WN2F_CONTEXT_ORIGFMT_IOCTRL)
#define set_WN2F_CONTEXT_origfmt_ioctrl(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_ORIGFMT_IOCTRL)
#define reset_WN2F_CONTEXT_origfmt_ioctrl(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_ORIGFMT_IOCTRL)

/* This flag indicates whether or not it is safe to remove an
 * stid where the lhs is identical to the rhs.
 */
#define WN2F_CONTEXT_EMIT_STID 0x00000400
#define WN2F_CONTEXT_emit_stid(c)\
   ((c).flags & WN2F_CONTEXT_EMIT_STID)
#define set_WN2F_CONTEXT_emit_stid(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_EMIT_STID)
#define reset_WN2F_CONTEXT_emit_stid(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_EMIT_STID)


/* This flag indicates whether or not a pragma directive can apply
 * to an explicit region.  A pragma directive that can only apply
 * to an explicit region in source-code must be ignored if the 
 * region to which it belongs is not emitted.
 */
#define WN2F_CONTEXT_EXPLICIT_REGION 0x00000800
#define WN2F_CONTEXT_explicit_region(c)\
   ((c).flags & WN2F_CONTEXT_EXPLICIT_REGION)
#define set_WN2F_CONTEXT_explicit_region(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_EXPLICIT_REGION)
#define reset_WN2F_CONTEXT_explicit_region(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_EXPLICIT_REGION)


/* this flag indicates that formatted IO is being processed 
 * It allows interpretation of craylibs IO_NONE specifiers
 */
#define WN2F_CONTEXT_FMT_IO 0x00001000
#define WN2F_CONTEXT_fmt_io(c)\
   ((c).flags & WN2F_CONTEXT_FMT_IO)
#define set_WN2F_CONTEXT_fmt_io(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_FMT_IO)
#define reset_WN2F_CONTEXT_fmt_io(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_FMT_IO)


/* this flag indicates that IO processing deals with craylibs
 * not f77 mips libs.
 */
#define WN2F_CONTEXT_CRAY_IO 0x00002000
#define WN2F_CONTEXT_cray_io(c)\
   ((c).flags & WN2F_CONTEXT_CRAY_IO)
#define set_WN2F_CONTEXT_cray_io(c)\
   ((c).flags = (c).flags | WN2F_CONTEXT_CRAY_IO)
#define reset_WN2F_CONTEXT_cray_io(c)\
   ((c).flags = (c).flags & ~WN2F_CONTEXT_CRAY_IO)


    /* ---- Utilities to aid in WN to Fortran translation ---- */
    /* ------------------------------------------------------- */

// >> WHIRL 0.30: replaced OPC_LNOT, OPC_LAND, OPC_LIOR by OPC_B and OPC_I4 variants
// TODO WHIRL 0.30: get rid of OPC_I4 variants.
#define WN2F_expr_has_boolean_arg(opc) \
   ((opc) == OPC_BLNOT || (opc) == OPC_BLAND || (opc) == OPC_BLIOR || \
    (opc) == OPC_I4LNOT || (opc) == OPC_I4LAND || (opc) == OPC_I4LIOR)
// << WHIRL 0.30: replaced OPC_LNOT, OPC_LAND, OPC_LIOR by OPC_B and OPC_I4 variants


#define WN2F_Can_Assign_Types(t1, t2) \
   ((TY_Is_Array(t1) && TY_is_character(t1) && \
     TY_Is_Array(t2) && TY_is_character(t2)) || \
    Stab_Identical_Types(t1, t2, \
			 FALSE, /*check_quals*/ \
			 FALSE, /*check_scalars*/ \
			 TRUE)) /*ptrs_as_scalars*/


void WN2F_Stmt_Newline(TOKEN_BUFFER tokens,
		       const char  *label,
		       SRCPOS       srcpos,
		       WN2F_CONTEXT context);


extern void WN2F_Address_Of(TOKEN_BUFFER tokens);

extern WN2F_STATUS
   WN2F_Offset_Memref(TOKEN_BUFFER tokens,
		      WN          *addr,        /* Base address */
		      TY_IDX       addr_ty,     /* type of base-address */
		      TY_IDX       object_ty,   /* type of object referenced */
		      STAB_OFFSET  addr_offset, /* offset from base */
		      WN2F_CONTEXT context);

extern WN2F_STATUS
   WN2F_Offset_Symref(TOKEN_BUFFER tokens,
		      ST          *addr,        /* Base symbol */
		      TY_IDX       addr_ty,     /* type of base-symbol-addr */
		      TY_IDX       object_ty,   /* type of object referenced */
		      STAB_OFFSET  addr_offset, /* offset from base */
		      WN2F_CONTEXT context);

extern WN_OFFSET 
   WN2F_Sum_Offsets(WN *addr);

extern void 
   WN2F_Emit_End_Stmt(TOKEN_BUFFER tokens,BOOL start) ;


/* the preamble to put out for comments eg: CSGI$ start 1 */

extern  const char * sgi_comment_str ;

   /* -------- Facilities to effect a WN to Fortran translation --------- */
   /* ------------------------------------------------------------------- */

extern void WN2F_initialize(void);
extern void WN2F_finalize(void);

extern WN2F_STATUS
WN2F_translate(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context);

void  WN2F_dump_context( WN2F_CONTEXT c) ;


#endif /* wn2f_INCLUDED */
