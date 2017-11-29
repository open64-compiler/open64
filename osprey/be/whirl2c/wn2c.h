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


#ifndef wn2c_INCLUDED
#define wn2c_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: wn2c.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.wn2c.h $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *   STATUS: The status of a translation of a WN subtree into C
 *           is represented as a collection of flags.  Note that
 *           the STATUS_is_lvalue flag can only be returned in 
 *           response to CONTEXT_needs_lvalue request.  While
 *           a CONTEXT is passed down onto the processing of
 *           nested subtrees, the STATUS is passed up.
 *
 *   WN2C_initialize: This initializes any WN to C translation
 *           and must always be called prior to any WN2C_translate()
 *           call.
 *
 *   WN2C_finalize: This finalizes any WN to C translation
 *           and should be called after all procesing related
 *           to a whirl2c translation is complete.
 *
 *   WN2C_translate:  Translates a WN subtree into a sequence of C
 *           tokens, which are added to the given TOKEN_BUFFER.
 *
 *   WN2C_translate_structured_types:  Translates all struct/union/class
 *           types to C.  Typically, this function is called once prior
 *           to a sequence of calls to WN2C_translate.
 *
 *   WN2C_translate_file_scope_defs: Translates all file-level 
 *           variable, function and constant declarations to C.  The
 *           Current_Symtab must be set to the global symbol table
 *           prior to a call to this function.  Typically, this 
 *           function is called once after all calls to 
 *           WN2C_translate, since we then have flagged all 
 *           referenced entities.
 *
 *   WN2C_memref_lhs: Given the necessary attributes, translate
 *          the lft-hand-side of an ISTORE or the address of
 *          an ILOAD into C.
 *
 *   WN2C_istore_lhs: Given the necessary attributes, translate
 *          the lft-hand-side of an STID statement into C.
 *
 * ====================================================================
 * ====================================================================
 */

    /* ---------- Result status of a WN to C translation ----------- */

typedef mUINT32 STATUS;

/* Status flags and their access macros */
#define EMPTY_STATUS            0x000000000  
#define STATUS_LVALUE           0x000000001
                                  /* Subexpression evaluated to an lvalue */
#define STATUS_ARRAY_AS_ADDRESS 0x000000002
                                  /* Subexpression evaluated to array value */
#define STATUS_BLOCK            0x000000004
                                  /* The last statement was a block */

#define STATUS_is_lvalue(s) ((s) & STATUS_LVALUE)
#define STATUS_set_lvalue(s) ((s) = (s) | STATUS_LVALUE)
#define STATUS_reset_lvalue(s) ((s) = (s) & ~STATUS_LVALUE)

#define STATUS_is_array_as_address(s) ((s) & STATUS_ARRAY_AS_ADDRESS)
#define STATUS_set_array_as_address(s) ((s) = (s) | STATUS_ARRAY_AS_ADDRESS)
#define STATUS_reset_array_as_address(s) ((s) = (s) & ~STATUS_ARRAY_AS_ADDRESS)

#define STATUS_is_block(s) ((s) & STATUS_LVALUE)
#define STATUS_set_block(s) ((s) = (s) | STATUS_LVALUE)
#define STATUS_reset_block(s) ((s) = (s) & ~STATUS_LVALUE)


    /* -------- Facilities to effect a WN to C translation --------- */

extern void WN2C_initialize(void);
extern void WN2C_finalize(void);
extern BOOL WN2C_new_symtab(void);
extern STATUS WN2C_translate(TOKEN_BUFFER tokens, const WN *wn, CONTEXT context);
extern void WN2C_translate_structured_types(void);
extern STATUS WN2C_translate_file_scope_defs(CONTEXT context);

extern void WN2C_memref_lhs(TOKEN_BUFFER tokens,
			    TY_IDX      *memref_typ,
			    const WN    *lhs,
			    STAB_OFFSET  memref_ofst,
			    TY_IDX       memref_addr_ty, 
			    TY_IDX       memref_object_ty, 
			    MTYPE        dtype,
			    CONTEXT      context);

extern void WN2C_stid_lhs(TOKEN_BUFFER tokens,
			  TY_IDX      *stored_typ,
			  const ST    *lhs_st,
			  STAB_OFFSET  stid_ofst,
			  TY_IDX       stid_ty, 
			  MTYPE        dtype,
			  CONTEXT      context,
			  UINT         field_id = 0);

#endif /* wn2c_INCLUDED */
