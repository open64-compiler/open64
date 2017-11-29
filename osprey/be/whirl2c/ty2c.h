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


#ifndef ty2c_INCLUDED
#define ty2c_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: ty2c.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:42:00-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.ty2c.h $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    For complex types we use the Complex_Type, Double_Complex_Type,
 *    Quad_Complex_Type, Complex_Type_Split and Complex_Type_Inverted
 *    ... all of which is defined in "stab.h".
 *
 *    TY2C_initialize:
 *    TY2C_finalize:
 *       This will initialize and terminate usage of this module().
 *       Initialization sets any internal state used in tokenizing
 *       types, and will also write to file a set of predefined
 *       typedef declarations, including the complex types defined
 *       in stab.h.
 *
 *    TY2C_translate:
 *       Add a C language type corresponding to the given TY node
 *       into the token_buffer, assuming the TOKEN_BUFFER already
 *       holds a construct to be declared of this type.  The type
 *       may surround the current contents of the given TOKEN_BUFFER
 *       as though these contents where declared to be of the given
 *       type; i.e. for an array or function type.  The qualifiers
 *       are not written out for the top-level ty, but will be written
 *       for component tys.
 *
 *    TY2C_translate_qualified:
 *       Same as TY2C_translate, but the qualifiers of the top-level
 *       ty will be written out.
 *
 *    TY2C_get_field_info:
 *       Return information about a field within the given struct/union 
 *       type at the given offset and of the given type or mtype.  If a
 *       field of the given TY is found, it will be selected; otherwise
 *       the first field of the given mtype will be selected.  If 
 *       no appropriate field is found found_fld will be NULL and
 *       the returned TOKEN_BUFFER is uninitialized; otherwise, the 
 *       field and a token buffer denoting the field selection
 *       will be returned.  Note that we can use this routine to
 *       search for a field of a given TY only, by setting the mtype
 *       to MTYPE_V.
 *
 *    TY2C_builtin:
 *       Returns TRUE if the given ty is given a builtin special name
 *       in whirl2c (i.e. it is a type not to be declared).
 *
 *    TY2C_Complex_Realpart_Name:
 *    TY2C_Complex_Imagpart_Name:
 *       Denotes the names of the real and imaginary components in
 *       a complex (struct) type.
 *
 * ====================================================================
 * ====================================================================
 */

extern void TY2C_initialize(CONTEXT context);
extern void TY2C_finalize(void);

extern void TY2C_translate(TOKEN_BUFFER decl_tokens,
                           TY_IDX ty,
                           CONTEXT context);
extern void TY2C_translate_unqualified(TOKEN_BUFFER decl_tokens, TY_IDX ty);

typedef struct TY2C_Fld_Info
{
   TOKEN_BUFFER select_tokens;
   FLD_HANDLE   found_fld;
} TY2C_FLD_INFO;

extern TY2C_FLD_INFO 
   TY2C_get_field_info(TY_IDX struct_ty,  /* base type */
		       TY_IDX field_ty,   /* preferred field type */
		       MTYPE  field_mty,  /* preferred field mtype */
		       INT64  offset);

extern BOOL TY2C_builtin(TY_IDX ty);

extern const char *TY2C_Complex_Realpart_Name;
extern const char *TY2C_Complex_Imagpart_Name;

#endif /* ty2c_INCLUDED */
