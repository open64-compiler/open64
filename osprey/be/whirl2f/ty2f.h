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


#ifndef ty2f_INCLUDED
#define ty2f_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: ty2f.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/ty2f.h,v $
 *
 * Revision history:
 *    13-Apr-95 - Original Version
 *
 * Description:
 *
 *    TY2F_translate: 
 *       Add a Fortran language type corresponding to the given TY node
 *       into the token_buffer, assuming the TOKEN_BUFFER already
 *       holds a construct to be declared of this type.  The type
 *       may surround the current contents of the given TOKEN_BUFFER
 *       as though these contents where declared to be of the given
 *       type; i.e. for an array or function type.  The qualifiers
 *       are not written out for the top-level ty, but will be written
 *       for component tys.  After this call, TY2F_Prepend_Structures()
 *       should be called in an appropriate context (such as 
 *       ST2F_translate()).
 *
 *    TY2F_Translate_ArrayElt:
 *       Translates an array offset into a (multi-dimensional) array
 *       indexing operation, based on a given array-type.
 *
 *    TY2F_Translate_Common:
 *       Translates a common-block into Fortran.  After this call, 
 *       TY2F_Prepend_Structures() should be called in an appropriate
 *       context (such as ST2F_translate()).
 *
 *    TY2F_Translate_Equivalence:
 *       Translates an equivalence spec into Fortran.  After this call, 
 *       TY2F_Prepend_Structures() should be called in an appropriate
 *       context (such as ST2F_translate()).  If alt_return==TRUE, this
 *       equivalence represents the return-variables for alternate return
 *       points in a function, and they will be treated as such.
 *
 *    TY2F_Prepend_Structures:
 *       When TY2F_translate(), TY2F_Translate_Common(), or 
 *       TY2F_Translate_Equivalence() are invoked, a number of
 *       records may be translated in the process.  These will be
 *       declared in a buffer internal to ty2f, and this routine
 *       must be called to prepend the contents of this internal
 *       buffer to a given buffer.  We cannot prepend these 
 *       structures to the buffer passed to the "TY2F_translate()" 
 *       routine, since we do not know in what context that translation
 *       occurs (e.g. it may called for a fld declaration).
 *
 *     TY2F_Fld_Separator: adds the appropriate field separator for
 *                         structure components
 *
 * KIND_STRUCT field information
 * ------------------------------
 *
 *   TY2F_Free_Fld_Path:  Having called Stab_Get_Fld_Path(), we can
 *      reuse the path-elements by invoking TY2F_Free_Fld_Path()
 *      before the next call to TY2F_Get_Fld_Path().
 *
 *   TY2F_Get_Fld_Path:  Returns a path through the substructure of
 *      the given KIND_STRUCT to a field element that was found to
 *      best match the given type and offset.  While a matching type
 *      need not be of the same btype, it must always have the same
 *      offset, type-size, and type-alignment.  NULL is returned when
 *      no matching field is found.
 *
 *   TY2F_Translate_Fld_Path:  Given a path to a field, append a field
 *      selection string as dictated by Fortran syntax to access this
 *      field.  It is assumed that the token_buffer already contains
 *      the tokens for the base-record expression.  Note that we use
 *      the pointee name to dereference a pointer record elements
 *      (after emitting a warning since we do not expect record elements
 *      to be pointers).
 *
 *    TY2F_Dump_Fld_Path: dump to stdout...
 *
 *    TY2F_Point_At_Path: look within a path for a FLD with the given offset.
 *
 * ====================================================================
 * ====================================================================
 */

extern void TY2F_translate(TOKEN_BUFFER tokens, TY_IDX ty);
extern void TY2F_Translate_ArrayElt(TOKEN_BUFFER tokens,
				    TY_IDX       arr_ty,
				    STAB_OFFSET  arr_ofst);
extern void TY2F_Translate_Common(TOKEN_BUFFER tokens, 
				  const char   *name, 
				  TY_IDX        ty);
extern void TY2F_Translate_Equivalence(TOKEN_BUFFER tokens,
				       TY_IDX        ty,
				       BOOL          alt_return);

extern void TY2F_Prepend_Structures(TOKEN_BUFFER tokens);


   /*------- Facilities to get information about FLDs -------*/
   /*--------------------------------------------------------*/
   
typedef struct Fld_Path_Info FLD_PATH_INFO;
struct Fld_Path_Info
{
   FLD_HANDLE     fld;       /* Field on the path */
   BOOL           arr_elt;   /* An element in an array field? */
   STAB_OFFSET    arr_ofst;  /* Offset of element within an array field */
   WN *           arr_wn;    /* if arr_elt, optional OPC_ARRAY with subscripts */
   FLD_PATH_INFO *next;      /* Next field on the path */
};

extern FLD_PATH_INFO * TY2F_Free_Fld_Path(FLD_PATH_INFO *fld_path);


extern FLD_PATH_INFO * TY2F_Get_Fld_Path(TY_IDX    struct_ty, 
					 TY_IDX    object_ty,
					 STAB_OFFSET offset);

extern void TY2F_Translate_Fld_Path(TOKEN_BUFFER   tokens, 
				    FLD_PATH_INFO *fld_path,
				    BOOL           deref,  
				    BOOL           member_of_common,
				    BOOL           name_as_is,
				    WN2F_CONTEXT   context);


extern void TY2F_Fld_Separator(TOKEN_BUFFER tokens);
extern FLD_HANDLE TY2F_Last_Fld(FLD_PATH_INFO *fld_path);

extern void TY2F_Dump_Fld_Path(FLD_PATH_INFO *fld_path) ;
extern FLD_PATH_INFO * TY2F_Point_At_Path(FLD_PATH_INFO * path, STAB_OFFSET off);

#endif /* ty2f_INCLUDED */

