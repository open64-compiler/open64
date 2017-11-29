/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
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
 * Module: ty2f.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/ty2f.cxx,v $
 *
 * Revision history:
 *  12-Apr-95 - Original Version
 *
 * Description:
 *
 *   Translates a TY entry to a Fortran type.
 *
 * ====================================================================
 * ====================================================================
 */
#include "whirl2f_common.h"
#include "PUinfo.h"
#include "wn2f.h"
#include "ty2f.h"
#include "st2f.h"
#include "tcon2f.h"
#include "wn2f_load_store.h"

/* TY2F_Handler[] maps a TY_kind to a function that translates
 * a type of the given kind into Fortran.  Should the ordinal
 * numbering of the KIND change in "../common/com/stab.h", then
 * a corresponding change must be made here.
 */

typedef void (*TY2F_HANDLER_FUNC)(TOKEN_BUFFER, TY_IDX);
static void TY2F_invalid(TOKEN_BUFFER decl_tokens, TY_IDX ty);
static void TY2F_scalar(TOKEN_BUFFER decl_tokens, TY_IDX ty);
static void TY2F_array(TOKEN_BUFFER decl_tokens, TY_IDX ty);
static void TY2F_struct(TOKEN_BUFFER decl_tokens, TY_IDX ty);
static void TY2F_pointer(TOKEN_BUFFER decl_tokens, TY_IDX ty);
static void TY2F_void(TOKEN_BUFFER decl_tokens, TY_IDX ty) ;

static const TY2F_HANDLER_FUNC 
   TY2F_Handler[KIND_LAST/*TY_KIND*/] =
{
   &TY2F_invalid,   /* KIND_INVALID */
   &TY2F_scalar,    /* KIND_SCALAR */
   &TY2F_array,     /* KIND_ARRAY */
   &TY2F_struct,    /* KIND_STRUCT */
   &TY2F_pointer,   /* KIND_POINTER */
   &TY2F_invalid,   /* KIND_FUNCTION */
   &TY2F_void,      /* KIND_VOID */
}; /* TY2F_Handler */

/* detect parts of f90 dope vectors which should be output. Most are I4 boundaries */
/* except the bofst >16 - just for num_dims */

#define NOT_BITFIELD_OR_IS_FIRST_OF_BITFIELD(f) \
  (!FLD_is_bit_field(f) || (FLD_is_bit_field(f) && (FLD_bofst(f) == 0) || FLD_bofst(f) > 16))

/*---------------------- A few utility routines -----------------------*/
/*---------------------------------------------------------------------*/

	
static void
TY2F_Append_Array_Bnd_Ph(TOKEN_BUFFER decl_tokens, 
			 ST_IDX       arbnd)
{
   char ptr_string[128];
   const char * p = "%s";

   // Would prefer
   //   sprintf(ptr_string, p, W2CF_Symtab_Nameof_St(ST_ptr(arbnd)));
   // but mfef77 assigns auto varbls of same name in skip list. So 
   // assignments to bounds  don't get printed & the bounds look a 
   // little odd. eg: try
   //   subroutine xxx(bbb,k1,k2)
   //   integer bbb(k1,3:k2)
   //
   // mfef90 uses unique names, so ST_name is ok.

   sprintf(ptr_string, p, ST_name(ST_ptr(arbnd)));
   Append_Token_String(decl_tokens, ptr_string);

} /* TY2F_Append_Array_Bnd_Ph */

static void
TY2F_Append_ARB(TOKEN_BUFFER decl_tokens, ARB_HANDLE arb)
{
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;
   
   /* All array acceses have been normalized to assume arrays with
    * bounds based at 1 (Fortran default), so we do the same thing here.
    * There is no need to emit the lower bound, since 1 is the default
    * anyway:
    *
    *   TCON2F_translate(decl_tokens, 
    *	                 Host_To_Targ(MTYPE_I8, 1LL),
    *	                 FALSE *is_logical*);
    *	Append_Token_Special(decl_tokens, ':');
    */

   /* Append the upper-bound */
   if (ARB_const_lbnd(arb) && /* Constant lower bound */
       ARB_const_ubnd(arb))   /* Constant upper bound */
   {
      if (ARB_ubnd_val(arb) - ARB_lbnd_val(arb) >= 0)
	 TCON2F_translate(decl_tokens, 
			  Host_To_Targ(MTYPE_I8, 
				       ARB_ubnd_val(arb) - 
				       ARB_lbnd_val(arb) + 1LL),
			  FALSE /*is_logical*/);
      else
	 Append_Token_Special(decl_tokens, '*');
	 
   }
   else
   {
      /* We have some combination of non-constant bounds, so we try to
       * normalize these to account for index-expressions that have been
       * normalized to "1" based indices.
       */
      if ((!ARB_const_lbnd(arb) && ARB_lbnd_var(arb) == (ST_IDX) 0) ||
               (!ARB_const_ubnd(arb) && ARB_ubnd_var(arb) == (ST_IDX) 0))
      {
	 Append_Token_Special(decl_tokens, '*');
      }
      else if (ARB_const_ubnd(arb))
      {
	 TCON2F_translate(decl_tokens, 
			  Host_To_Targ(MTYPE_I8,
				       ARB_ubnd_val(arb) + 1LL),
			  FALSE /*is_logical*/);
         Append_Token_Special(decl_tokens, '-');
         Append_Token_Special(decl_tokens, '(');
	 set_WN2F_CONTEXT_no_parenthesis(context);
	 TY2F_Append_Array_Bnd_Ph(decl_tokens, 
				  ARB_lbnd_var(arb));
	 Append_Token_Special(decl_tokens, ')');
      }
      else if (ARB_const_lbnd(arb))
      {
	 BOOL zero_lbnd = (ARB_lbnd_val(arb) - 1LL == 0LL);

         if (!zero_lbnd)
	 {
	    Append_Token_Special(decl_tokens, '(');
	    set_WN2F_CONTEXT_no_parenthesis(context);
	 }
	 TY2F_Append_Array_Bnd_Ph(decl_tokens, 
				  ARB_ubnd_var(arb));
         if (!zero_lbnd)
	 {
	    Append_Token_Special(decl_tokens, ')');
	    Append_Token_Special(decl_tokens, '-');
	    TCON2F_translate(decl_tokens, 
			     Host_To_Targ(MTYPE_I8,
					  ARB_lbnd_val(arb) - 1LL),
			     FALSE /*is_logical*/);
	 }
      }
      else
      {
         set_WN2F_CONTEXT_no_parenthesis(context);
         Append_Token_String(decl_tokens, "1");
         Append_Token_Special(decl_tokens, '+');
         Append_Token_Special(decl_tokens, '(');
	 TY2F_Append_Array_Bnd_Ph(decl_tokens,
				  ARB_ubnd_var(arb));
         Append_Token_Special(decl_tokens, ')');
         Append_Token_Special(decl_tokens, '-');
         Append_Token_Special(decl_tokens, '(');
	 TY2F_Append_Array_Bnd_Ph(decl_tokens,
				  ARB_lbnd_var(arb));
         Append_Token_Special(decl_tokens, ')');
      }
   } /* Constant bounds */
} /* TY2F_Append_ARB */

static BOOL
TY2F_is_character(TY_IDX ty)
{
  while (TY_kind(ty) == KIND_ARRAY)
    ty = TY_etype(ty);

  return TY_is_character(ty);
}
/*------ Utilities for accessing and declaring KIND_STRUCT FLDs ------
 *---------------------------------------------------------------------*/

#define FLD_INFO_ALLOC_CHUNK 16
static FLD_PATH_INFO *Free_Fld_Path_Info = NULL;


static BOOL
TY2F_Pointer_To_Dope(TY_IDX ty)
{
  /* Is this a pointer to a dope vector base */

  return (strcmp(TY_name(TY_pointed(ty)),".base.") == 0) ;

}
static FLD_PATH_INFO *
New_Fld_Path_Info(FLD_HANDLE fld)
{
   /* Allocates a new FLD_PATH_INFO, reusing any that have earlier
    * been freed up.  Dynamic allocation occurs in chunks of 16
    * (FLD_INFO_ALLOC_CHUNK) FLD_PATH_INFOs at a time.
    */
   FLD_PATH_INFO *fld_info;
   
   if (Free_Fld_Path_Info != NULL)
   {
      fld_info = Free_Fld_Path_Info;
      Free_Fld_Path_Info = fld_info->next;
   }
   else
   {
      INT info_idx;
      
      /* Allocate a new chunk of path infos, and put all except the
       * first one on the free-list.
       */
      fld_info = TYPE_ALLOC_N(FLD_PATH_INFO, FLD_INFO_ALLOC_CHUNK);
      fld_info[FLD_INFO_ALLOC_CHUNK-1].next = Free_Fld_Path_Info;
      for (info_idx = FLD_INFO_ALLOC_CHUNK-2; info_idx > 0; info_idx--)
	 fld_info[info_idx].next = &fld_info[info_idx+1];
      Free_Fld_Path_Info = &fld_info[1];
   }

   fld_info->next = NULL;
   fld_info->arr_elt = FALSE;
   fld_info->arr_ofst = 0;
   fld_info->arr_wn = NULL;
   fld_info->fld = fld;
   return fld_info;
} /* New_Fld_Path_Info */

static STAB_OFFSET
TY2F_Fld_Size(FLD_HANDLE this_fld, mUINT64  max_size)
{
  /* Returns the size of the field, taking into account the offset
   * to the next (non-equivalence) field and the maximum field-size
   * (based on the structure size).
   */
  
  mUINT64 fld_size = TY_size(FLD_type(this_fld));

  /* Restrict the fld_size to the max_size */
  if (fld_size > max_size)
    fld_size = max_size;
  
  /* If this_fld is an equivalence field, then just return the current
   * fld_size (cannot be any different), otherwise search for a non-
   * equivalent next_fld at a higher offset.
   * TODO: mfef90 & mfef77 set the flag slightly differently in COMMON.
   * this really works only for mfef77.
   */

  if (!FLD_equivalence(this_fld))
    {
      FLD_ITER fld_iter = Make_fld_iter(this_fld);

      if (!FLD_last_field (fld_iter)) 
      {
	++fld_iter;
	BOOL found = FALSE;
	mUINT64 noffset = 0; 

	do
	{
	   FLD_HANDLE next_fld (fld_iter);

	   if (!FLD_is_bit_field(next_fld)) 
	     if (!(FLD_equivalence(next_fld) || FLD_ofst(this_fld) >= FLD_ofst(next_fld)))
	     {
	       found  = TRUE;
	       noffset =  FLD_ofst(next_fld) ;
	       break ;
	     }
	 } while (!FLD_last_field (fld_iter ++ )) ;

	if (found) 
	  if (fld_size > noffset - FLD_ofst(this_fld))
	    fld_size = noffset - FLD_ofst(this_fld) ;
      }
    }
  return fld_size;
} /* TY2F_Fld_Size */


static FLD_PATH_INFO *
Select_Best_Fld_Path(FLD_PATH_INFO *path1,
		     FLD_PATH_INFO *path2,
		     TY_IDX         desired_ty,
		     mUINT64        desired_offset)
{
   /* PRECONDITION: Both paths must be non-NULL and lead to a field
    *    at the desired_offset.
    *
    * Try to find the best of two paths to a field.  This routine
    * will be called for EVERY field at every place where a struct,
    * union, or equivalence field is accessed, so efficiency is of
    * uttmost importance.  The best path is returned, while the other
    * on is freed up.
    */
   FLD_PATH_INFO *best_path;
   mUINT64        offs1, offs2;
   FLD_PATH_INFO *p1, *p2;
   TY_IDX         t1,  t2;
   
   ASSERT_DBG_FATAL(path1 != NULL && path2 != NULL,
		    (DIAG_W2F_UNEXPEXTED_NULL_PTR, 
		     "path1 or path2", "Select_Best_Fld_Path"));
   
   /* Find the last field on each path */
   offs1 = FLD_ofst(path1->fld) + path1->arr_ofst;
   for (p1 = path1; p1->next != NULL; p1 = p1->next)
      offs1 += FLD_ofst(p1->next->fld) + p1->next->arr_ofst;
   offs2 = FLD_ofst(path2->fld) + path2->arr_ofst;
   for (p2 = path2; p2->next != NULL; p2 = p2->next)
      offs2 += FLD_ofst(p2->next->fld) + p2->next->arr_ofst;

   ASSERT_DBG_FATAL(offs1 == desired_offset && offs2 == desired_offset,
		    (DIAG_W2F_UNEXPEXTED_OFFSET,
		     offs1, "Select_Best_Fld_Path"));

   /* Get the element type (either the field type or the type of an
    * array element.
    */
   if (p1->arr_elt)
      t1 = TY_AR_etype(FLD_type(p1->fld));
   else
      t1 = FLD_type(p1->fld);
   if (p2->arr_elt)
      t2 = TY_AR_etype(FLD_type(p2->fld));
   else
      t2 = FLD_type(p2->fld);

   /* Compare types, in order of increasing accuracy */
   if (TY_mtype(t1) == TY_mtype(desired_ty) &&
       TY_mtype(t2) != TY_mtype(desired_ty))
      best_path = path1;
   else if (TY_mtype(t2) == TY_mtype(desired_ty) &&
	    TY_mtype(t1) != TY_mtype(desired_ty))
      best_path = path2;
   else if (Stab_Identical_Types(t1, desired_ty,
				 FALSE,  /* check_quals */
				 TRUE,   /* check_scalars */
				 FALSE)) /* ptrs_as_scalars */
      best_path = path1; /* path2 cannot possibly be any better */
   else if (Stab_Identical_Types(t2, desired_ty,
				 FALSE,  /* check_quals */
				 TRUE,   /* check_scalars */
				 FALSE)) /* ptrs_as_scalars */
      best_path = path2;
   else
      best_path = path1;

   /* Free up the path not chosen */
   if (best_path == path1)
      TY2F_Free_Fld_Path(path2);
   else
      TY2F_Free_Fld_Path(path1);

   return best_path;
} /* Select_Best_Fld_Path */


static FLD_PATH_INFO *
Construct_Fld_Path(FLD_HANDLE   fld,
		   TY_IDX    struct_ty,
		   TY_IDX    desired_ty,
		   mUINT64   desired_offset,
		   mUINT64   max_fld_size)
{
   /* Returns the field path through "fld" found to best match the 
    * given offset and type.  As a minimum requirement, the offset 
    * must be as desired and the type must have the desired size
    * and alignment (with some concessions allowed for substrings).
    * The path is terminate with a NULL next pointer.  When no 
    * field matches the desired type and offset, NULL is returned.
    */
   FLD_PATH_INFO    *fld_path;
   const mUINT64     fld_offset = FLD_ofst(fld);
   TY_IDX            fld_ty = FLD_type(fld);
   BOOL              is_array_elt = FALSE;
   STAB_OFFSET       ofst_in_fld = 0;

   /* This field cannot be on the path to a field with the given
    * attributes, unless the desired_offset is somewhere within
    * the field.
    */
#if DBGPATH
   printf (" Construct: fld %s, struct %s, desired %s , des off %d \n",
	   FLD_name(fld),
	   TY_name(struct_ty),
	   TY_name(desired_ty),
	   desired_offset);
#endif

   if (desired_offset < fld_offset ||
       desired_offset >= (fld_offset + TY_size(fld_ty)))
   {
      /* This field cannot be on the path to a field with the given
       * attributes, since the desired_offset is nowhere within
       * the field.
       */
      fld_path = NULL;
#if DBGPATH
      printf ("     found NULL\n");
#endif
   }
   else if (TY_Is_Array(fld_ty) && TY_is_character(fld_ty) &&
	    TY_Is_Array(desired_ty) && TY_is_character(desired_ty))
   {
#if DBGPATH
      printf ("     found char substring\n");
#endif
      /* A match is found! */
      ofst_in_fld = (desired_offset - fld_offset)/TY_size(TY_AR_etype(fld_ty));
      ofst_in_fld *= TY_size(TY_AR_etype(fld_ty));
      if ((ofst_in_fld + TY_size(desired_ty)) > TY_size(fld_ty))
      {
	 fld_path = NULL; /* The string does not fit */
      }
      else
      {
	 fld_path = New_Fld_Path_Info(fld);
	 if (TY_size(fld_ty) != TY_size(desired_ty))
	 {
	    fld_path->arr_elt = TRUE;
	    fld_path->arr_ofst = ofst_in_fld;
	 } 
      }
   }
   else
   {
      /* See if the field we are looking for may be an array element */
      is_array_elt = (TY_Is_Array(fld_ty) &&
		      (TY_Is_Structured(TY_AR_etype(fld_ty))||
		       TY2F_is_character(fld_ty) ||
		       Stab_Identical_Types(TY_AR_etype(fld_ty), desired_ty,
					    FALSE,   /* check_quals */
					    FALSE,   /* check_scalars */
					    TRUE))); /* ptrs_as_scalars */
#if DBGPATH
      printf ("     is_array = %d, fld_ty %s \n",is_array_elt,TY_name(fld_ty));
#endif

      if (is_array_elt)
      {
	 fld_ty = TY_AR_etype(fld_ty);
	 ofst_in_fld =
	    ((desired_offset - fld_offset)/TY_size(fld_ty)) * TY_size(fld_ty);
      }

      if (TY_Is_Structured(fld_ty) &&
	  !Stab_Identical_Types(fld_ty, desired_ty,
				FALSE,  /* check_quals */
				FALSE,  /* check_scalars */
				TRUE))  /* ptrs_as_scalars */
      {
#if DBGPATH
	printf ("     recurse \n");
#endif
	 FLD_PATH_INFO *fld_path2 = 
	    TY2F_Get_Fld_Path(fld_ty, desired_ty, 
			      desired_offset - (fld_offset+ofst_in_fld));
	 
	 /* If a matching path was found, attach "fld" to the path */
	 if (fld_path2 != NULL)
	 {
	    if (TY_split(Ty_Table[fld_ty]))
	       fld_path = fld_path2; /* A stransparent substructure */
	    else
	    {
	       fld_path = New_Fld_Path_Info(fld);
	       fld_path->arr_elt = is_array_elt;
	       fld_path->arr_ofst = ofst_in_fld;
	       fld_path->next = fld_path2;
	    }
	 }
	 else
	 {
	    fld_path = NULL;
	 }
      }
      else /* This may be a field we want to take into account */
      {
	 const STAB_OFFSET fld_size = TY2F_Fld_Size(fld, max_fld_size);

	 /* We only match a field with the expected size, offset
	  * and alignment.
	  */
         
	 if (desired_offset != fld_offset+ofst_in_fld || /* unexpected ofst */
	     fld_size < (TY_size(fld_ty)+ofst_in_fld) || /* unexpected size */
	     TY_align(struct_ty) < TY_align(fld_ty))     /* unexpected align */
	 {
#if DBGPATH
	    printf ("     account - miss\n");
#endif
	    fld_path = NULL;
	 }
	 else /* A match is found! */
	 {
#if DBGPATH
	   printf ("     account - match\n");
#endif
	    fld_path = New_Fld_Path_Info(fld);
	    fld_path->arr_elt = is_array_elt;
	    fld_path->arr_ofst = ofst_in_fld;
	 }/*if*/
      } /*if*/
   } /*if*/

   return fld_path;
} /* Construct_Fld_Path */


static const char * 
TY2F_Fld_Name(FLD_HANDLE fld, 
	      BOOL       common_or_equivalence,
	      BOOL       alt_return_name)
{
   /* Since fields may be accessed in an unqualified manner in Fortran,
    * e.g. for common block members and equivalences, so we need to treat
    * them similar to the way we would treat regular objects.
    */
   const char *fld_name;

   if (common_or_equivalence && !alt_return_name)
      fld_name = W2CF_Symtab_Nameof_Fld(fld);
   else
   {
      fld_name = WHIRL2F_make_valid_name(FLD_name(fld),FALSE);
      if (fld_name == NULL || *fld_name == '\0')
	 fld_name = W2CF_Symtab_Nameof_Fld(fld);
   }
   return fld_name;
} /* TY2F_Fld_Name */


/*------ Utilities for accessing and declaring KIND_STRUCTs ------
 *----------------------------------------------------------------*/

/* Local buffer to hold Fortran STRUCTURE declarations, which
 * should be appended to this buffer in the order in which
 * they are encountered.
 */
static TOKEN_BUFFER TY2F_Structure_Decls = NULL;


static void
TY2F_Equivalence(TOKEN_BUFFER tokens, 
		 const char  *equiv_name, 
		 const char  *fld_name,
		 STAB_OFFSET  fld_ofst)
{
   /* Append one equivalence statement to the tokens buffer,
    * keeping in mind that the equiv_name is based at index 1.
    */
   Append_Token_String(tokens, "EQUIVALENCE");
   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, equiv_name); /* equiv_name at given offset */
   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, Number_as_String(fld_ofst+1, "%lld"));
   Append_Token_Special(tokens, ')');
   Append_Token_Special(tokens, ',');
   Append_Token_String(tokens, fld_name);   /* fld_name at offset zero */
   Append_Token_Special(tokens, ')');
} /* TY2F_Equivalence */


static void
TY2F_Equivalence_FldList(TOKEN_BUFFER tokens, 
                         FLD_HANDLE   fldlist,
                         UINT         equiv_var_idx,
                         mUINT64      ofst,
                         BOOL        *common_block_equivalenced)
{
  FLD_ITER fld_iter = Make_fld_iter(fldlist);

  do 
    {
      FLD_HANDLE fld (fld_iter);

      if (TY_split(Ty_Table[FLD_type(fld)]))
	{
	  TY2F_Equivalence_FldList(tokens, 
				   TY_flist(Ty_Table[FLD_type(fld)]),
				   equiv_var_idx,
				   ofst + FLD_ofst(fld),
				   common_block_equivalenced);
	}
      else if (FLD_equivalence(fld) || !*common_block_equivalenced)
	{
	  Append_F77_Indented_Newline(tokens, 1, NULL/*label*/);
	  TY2F_Equivalence(tokens,
			   W2CF_Symtab_Nameof_Tempvar(equiv_var_idx),
			   TY2F_Fld_Name(fld_iter, TRUE/*equiv*/, FALSE/*alt_ret*/),
			   ofst + FLD_ofst(fld));
	  if (!FLD_equivalence(fld))
	    *common_block_equivalenced = TRUE;
	}

    }
  while (!FLD_last_field (fld_iter++)) ;

} /* TY2F_Equivalence_FldList */


static void
TY2F_Equivalence_List(TOKEN_BUFFER tokens, 
		      const TY_IDX struct_ty)
{
   /* Append a nameless EQUIVALENCE specification statement for
    * each equivalence field in the given struct.  Declare a 
    * dummy symbol as an array of INTEGER*1 elements to represent
    * the structure and each EQUIVALENCE specification will then 
    * equivalence a field to this dummy-symbol at the field offset.
    *
    * Group these declarations together by prepending each 
    * declaration (including the first one) with a newline.
    *
    * For COMMON blocks, it is also necessary to emit one element
    * that is not an equivalence!
    */
   TY_IDX     equiv_ty;
   UINT       equiv_var_idx;
   BOOL       common_block_equivalenced = FALSE;

   /* Declare an INTEGER*1 array (or CHARACTER string?) variable
    * to represent the whole equivalenced structure. Don't unlock
    * the tmpvar, or a similar equivalence group (ie: TY) will 
    * get the same temp.
    */

   equiv_ty = Stab_Array_Of(Stab_Mtype_To_Ty(MTYPE_I1), TY_size(struct_ty));
   equiv_var_idx = Stab_Lock_Tmpvar(equiv_ty, &ST2F_Declare_Tempvar);

   /* Relate every equivalence field to the temporary variable.
    */
   TY2F_Equivalence_FldList(tokens, 
                            TY_flist(Ty_Table[struct_ty]),
                            equiv_var_idx,
                            0, /* Initial offset */
                            &common_block_equivalenced);

} /* TY2F_Equivalence_List */

static void
TY2F_Translate_Structure(TY_IDX ty)
{
   TOKEN_BUFFER fld_tokens, struct_tokens;
   FLD_ITER     fld_iter;
   const UINT   current_indent = Current_Indentation();
   TY& ty_rt  = Ty_Table[ty];
   
   ASSERT_DBG_FATAL(TY_kind(ty_rt) == KIND_STRUCT, 
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(ty_rt), "TY2F_Translate_Structure"));

   /* Emit structure header */
   Set_Current_Indentation(PUinfo_local_decls_indent);
   struct_tokens = New_Token_Buffer();

   if (WN2F_F90_pu) {
      Append_Token_String(struct_tokens, "TYPE ");
      Append_Token_String(struct_tokens, W2CF_Symtab_Nameof_Ty(ty));
   } else {
      Append_Token_String(struct_tokens, "STRUCTURE");
      Append_Token_String(struct_tokens, 
			  Concat3_Strings("/", W2CF_Symtab_Nameof_Ty(ty), "/"));
   }

   /* Emit structure body */
   Increment_Indentation();
   FLD_IDX flist = ty_rt.Fld();

   if (flist != 0) {
     fld_iter = Make_fld_iter(TY_flist(ty_rt));
     do
       {
	 FLD_HANDLE fld (fld_iter);

	 /* if it's a bitfield, then assume it's part of a dope vector & */
	 /* just put out the name of the first bitfield in this I4       */

	 if(NOT_BITFIELD_OR_IS_FIRST_OF_BITFIELD(fld_iter))
	   {
	     /* See if this field starts a map or a union */

	     Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
	     if (FLD_begin_union(fld))
	       {
		 Append_Token_String(struct_tokens, "UNION");
		 Increment_Indentation();
		 Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
	       }
	     else if (FLD_begin_map(fld))
	       {
		 Append_Token_String(struct_tokens, "MAP");
		 Increment_Indentation();
		 Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
	       }

	     /* Declare this field */

	     fld_tokens = New_Token_Buffer();
	     Append_Token_String(fld_tokens, 
				 TY2F_Fld_Name(fld_iter,
					       FALSE/*common*/, 
					       FALSE/*alt_ret_name*/));

  	     TY2F_translate(fld_tokens, FLD_type(fld));

	     Append_And_Reclaim_Token_List(struct_tokens, &fld_tokens);

	     /* See if this field terminates a map or union */
	     if (FLD_end_union(fld))
	       {
		 Decrement_Indentation();
		 Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
		 Append_Token_String(struct_tokens, "END UNION");
	       }
	     else if (FLD_end_map(fld))
	       {
		 Decrement_Indentation();
		 Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
		 Append_Token_String(struct_tokens, "END MAP");
	       }
	   }       
       } while (!FLD_last_field (fld_iter++)) ;
   }
   /* Emit structure tail */
   Decrement_Indentation();

   Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
   if (WN2F_F90_pu) {
      Append_Token_String(struct_tokens, "END TYPE");
   } else {
      Append_Token_String(struct_tokens, "END STRUCTURE");
   }
   Append_F77_Indented_Newline(struct_tokens, 1, NULL/*label*/);
   
   if (TY2F_Structure_Decls == NULL)
      TY2F_Structure_Decls = New_Token_Buffer();

   Set_Current_Indentation(current_indent);
   Append_And_Reclaim_Token_List(TY2F_Structure_Decls, &struct_tokens);

} /* TY2F_Translate_Structure */


static void
TY2F_Translate_EquivCommon_PtrFld(TOKEN_BUFFER tokens, FLD_HANDLE fld)
{
   /* Declare the pointee and the pointer field of the common/eqivalence
    * block.
    */
   TOKEN_BUFFER decl_tokens = New_Token_Buffer();
   const char  *pointee_name = W2CF_Symtab_Nameof_Fld_Pointee(fld);
   const char  *fld_name = TY2F_Fld_Name(fld, 
					 TRUE/*comm,equiv*/, 
					 FALSE/*alt_ret_name*/);

   Append_Token_String(decl_tokens, pointee_name);
   TY2F_translate(decl_tokens, TY_pointed(FLD_type(fld)));
   Append_F77_Indented_Newline(decl_tokens, 1, NULL/*label*/);

   /* Declare the pointer type */
   Append_Token_String(decl_tokens, "POINTER");
   Append_Token_Special(decl_tokens, '(');
   Append_Token_String(decl_tokens, fld_name);
   Append_Token_Special(decl_tokens, ',');
   Append_Token_String(decl_tokens, pointee_name);
   Append_Token_Special(decl_tokens, ')');
   Append_And_Reclaim_Token_List(tokens, &decl_tokens);
} /* TY2F_Translate_EquivCommon_PtrFld */

static void
TY2F_Declare_Common_Flds(TOKEN_BUFFER tokens,
			 FLD_HANDLE   fldlist, 
			 BOOL         alt_return, /* Alternate return points */
			 BOOL        *is_equiv)   /* out */
{
  FLD_ITER fld_iter = Make_fld_iter(fldlist);

  /* Emit specification statements for every element of the
   * common block, including equivalences.
   */  

  do
    {
      FLD_HANDLE fld (fld_iter);
      TY_IDX ty = FLD_type(fld);

      /* Determine whether or not the common-block contains any
       * equivalences (must all be at the top level).
       */

      *is_equiv = *is_equiv || FLD_equivalence(fld);
      
      /* Declare as specified in the symbol table */
      if (TY_split(Ty_Table[ty]))
	{
	  /* Treat a full split element as a transparent data-structure */

	  TY2F_Declare_Common_Flds(tokens,
				   TY_flist(Ty_Table[ty]),
				   alt_return,
				   is_equiv);
	}
      else if (TY_Is_Pointer(ty))
	{
	  TY2F_Translate_EquivCommon_PtrFld(tokens, fld_iter);
	}
      else /* Non-pointer common field */
	{
	  TOKEN_BUFFER decl_tokens = New_Token_Buffer();
	  Append_Token_String(decl_tokens, 
			      TY2F_Fld_Name(fld_iter,
					    TRUE/*common/equivalence*/, 
					    alt_return/*alt_ret_name*/));
	  TY2F_translate(decl_tokens, FLD_type(fld));
	  Append_And_Reclaim_Token_List(tokens, &decl_tokens);
	}
      Append_F77_Indented_Newline(tokens, 1, NULL/*label*/);

    } while (!FLD_last_field (fld_iter++)) ;
} /* TY2F_Declare_Common_Flds */

static void
TY2F_List_Common_Flds(TOKEN_BUFFER tokens, FLD_HANDLE fldlist)
{
  FLD_ITER fld_iter = Make_fld_iter(fldlist);

  do
    {
      FLD_HANDLE fld (fld_iter);
      TY & ty  = Ty_Table[FLD_type(fld)];       

      if (TY_split(ty))
	{
	  /* Treat a full split element as a transparent data-structure */

	  TY2F_List_Common_Flds(tokens, TY_flist(ty));
	}
      else if (!FLD_equivalence(fld))
      {
	Append_Token_String(tokens, 
			    TY2F_Fld_Name(fld_iter,
					  TRUE/*common*/, 
					  FALSE/*alt_ret_name*/));
      }
      
      if (!FLD_last_field(fld)) 
      {
	FLD_ITER  next_iter = fld_iter ;
	FLD_HANDLE next (++next_iter);
        if (!FLD_equivalence(next))
	  Append_Token_Special(tokens, ',');
      }

    } while (!FLD_last_field (fld_iter++)) ;

} /* TY2F_List_Common_Flds */

/*------------- Hidden routines to declare variable types -------------*/
/*---------------------------------------------------------------------*/

static void
TY2F_invalid(TOKEN_BUFFER decl_tokens, TY_IDX ty)
{
   ASSERT_DBG_FATAL(FALSE, 
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(Ty_Table[ty]), 
		     "TY2F_invalid"));
   Prepend_Token_String(decl_tokens, "<TY2F_invalid>");
} /* TY2F_invalid */

static void
TY2F_scalar(TOKEN_BUFFER decl_tokens, TY_IDX ty_idx)
{
   const char *base_name;
   INT64 kind_type;
   const char * kind_spec;
   TY&   ty = Ty_Table[ty_idx];
   MTYPE mt = TY_mtype(ty);

   ASSERT_DBG_FATAL(TY_kind(ty) == KIND_SCALAR, 
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(ty), 
		     "TY2F_scalar"));

   if (TY_is_character(ty))
   {
      base_name = "CHARACTER";
   }
   else if (TY_is_logical(ty))
   {
      base_name = "LOGICAL";
   }
   else switch(mt)
   {
   case MTYPE_U1:
   case MTYPE_U2:
   case MTYPE_U4:
   case MTYPE_U8:
      /* Strictly speaking unsigned integers not supported in Fortran,
       * but we are lenient and treat them as the signed equivalent.
       */
   case MTYPE_I1:
   case MTYPE_I2:
   case MTYPE_I4:
   case MTYPE_I8:
      base_name = "INTEGER";
      break;
      
   case MTYPE_F4:
   case MTYPE_F8:
   case MTYPE_F10:
   case MTYPE_F16:
   case MTYPE_FQ:
      base_name = "REAL";
      break;
      
   case MTYPE_C4:
   case MTYPE_C8:
   case MTYPE_C10:
   case MTYPE_C16:
   case MTYPE_CQ:
      base_name = "COMPLEX";
      break;
      
   case MTYPE_M:
      base_name = "memory block";
      break;

#ifdef KEY
#if defined(TARG_X8664)
   case MTYPE_V16I1:
   case MTYPE_V16I2:
   case MTYPE_V16I4:
   case MTYPE_V16I8:
   case MTYPE_V16F4:
   case MTYPE_V16F8:
      base_name = "REAL";
      break;
#endif
#endif /* KEY */
   default:
      ASSERT_DBG_FATAL(FALSE,
		       (DIAG_W2F_UNEXPECTED_BTYPE, 
			MTYPE_name(mt), 
			"TY2F_scalar"));
   } /* switch(TY_btype(ty) */

   if (TY_size(ty) > 0)
   {
      if (WN2F_F90_pu) {
	 if (MTYPE_is_complex(mt)) {
	    kind_type = TY_size(ty) / 2;
	 } else {
	    kind_type = TY_size(ty);
	 }
	 kind_spec = Concat3_Strings("(",Number_as_String(kind_type, "%lld"),")");
	 Prepend_Token_String(decl_tokens,Concat2_Strings(base_name,kind_spec));
      } else {
	 Prepend_Token_String(
			      decl_tokens, 
			      Concat3_Strings(base_name,
					      "*", 
					      Number_as_String(TY_size(ty), "%lld")));
      }
   }
   else
   {
     if (mt == MTYPE_M)
      Prepend_Token_String(decl_tokens, ".mblock.");
     else
    {
       
      ASSERT_DBG_FATAL(TY_is_character(ty),
		       (DIAG_W2F_UNEXPECTED_TYPE_SIZE,
			TY_size(ty),"TY2F_scalar"));
      Prepend_Token_String(decl_tokens, "CHARACTER*(*)");
    }
   }
} /* TY2F_scalar */


static void
TY2F_array(TOKEN_BUFFER decl_tokens, TY_IDX ty_idx)
{
  TY& ty = Ty_Table[ty_idx] ;

   ASSERT_DBG_FATAL(TY_kind(ty) == KIND_ARRAY,
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND,
		     TY_kind(ty), "TY2F_array"));

   if (TY_is_character(ty))
   {
      /* A character string...
       */
      if (TY_size(ty) > 0) /* ... of known size */
	 Prepend_Token_String(
	    decl_tokens,
	    Concat2_Strings("CHARACTER*",
			    Number_as_String(TY_size(ty), "%lld")));
      else /* ... of unknown size */
	 Prepend_Token_String(decl_tokens, "CHARACTER*(*)");
   }
   else
   {
      /* A regular array, so prepend the element type and append
       * the index bounds.
       */
	
     ARB_HANDLE arb_base = TY_arb(ty);
     INT32       dim = ARB_dimension(arb_base) - 1  ;

      /* Do not permit pointers as elements of arrays, so just use
       * the corresponding integral type instead.  We do not expect
       * such pointers to be dereferenced anywhere.
       */
      if (TY_Is_Pointer(TY_AR_etype(ty)))
	 TY2F_translate(decl_tokens,
			Stab_Mtype_To_Ty(TY_mtype(TY_AR_etype(ty))));
      else
	 TY2F_translate(decl_tokens, TY_AR_etype(ty));

      Append_Token_Special(decl_tokens, '(');

      while (dim >= 0) 
      {
	ARB_HANDLE arb = arb_base[dim];

	TY2F_Append_ARB(decl_tokens, arb);

	if (dim-- > 0) 
	  Append_Token_Special(decl_tokens, ',');
      } 

      Append_Token_Special(decl_tokens, ')');
   }
} /* TY2F_array */


static void
TY2F_struct(TOKEN_BUFFER decl_tokens, TY_IDX ty)
{
  /* Structs are supported by VAX-Fortran and Fortran-90.  Note
   * that we here emit a RECORD declaration, while we expect
   * the STRUCTURE to have been declared through a call to
   * TY2F_Translate_Structure().
   */
  TY & ty_rt = Ty_Table[ty];

  ASSERT_DBG_FATAL(TY_kind(ty_rt) == KIND_STRUCT, 
		   (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		    TY_kind(ty_rt), "TY2F_struct"));

  if (!TY_is_translated_to_c(ty))
    {
      TY2F_Translate_Structure(ty);
      Set_TY_is_translated_to_c(ty); /* Really, translated to Fortran, not C */
    }

  if (!WN2F_F90_pu) {
    Prepend_Token_String(decl_tokens, 
			 Concat3_Strings("/", W2CF_Symtab_Nameof_Ty(ty), "/"));
    Prepend_Token_String(decl_tokens, "RECORD");
  } else {
    Prepend_Token_String(decl_tokens, 
			 Concat3_Strings("(", W2CF_Symtab_Nameof_Ty(ty), ")"));
    Prepend_Token_String(decl_tokens, "TYPE");
  }      
} /* TY2F_struct */


static void
TY2F_pointer(TOKEN_BUFFER decl_tokens, TY_IDX ty)
{
   if (!WN2F_F90_pu) {
      /* Pointer types in Fortran can only occur in a Pointer specification
       * statement.  We do not expect this routine to be called, since we
       * expect pointer types to be handled by ST2F_decl_var().
       */
      ASSERT_DBG_WARN(FALSE, 
		      (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		       TY_kind(ty), "TY2F_pointer"));
      
      Append_Token_Special(decl_tokens, ')');
      Prepend_Token_Special(decl_tokens, '(');
      Prepend_Token_String(decl_tokens, "POINTER");

   } else {

      /* Is a dope vector base address? Put out an integer large enough */
      /* to hold an address for now. Don't really want POINTER because  */
      /* implies cray/f90 pointer instead of address slot               */

      if (TY2F_Pointer_To_Dope(ty))
      {
	TY2F_translate(decl_tokens,Be_Type_Tbl(Pointer_Mtype));
      } 
      else
      {
	/* avoid recursive type declarations */

	if (TY_kind(TY_pointed(ty)) == KIND_STRUCT)
	{
 	  TY2F_translate(decl_tokens,Be_Type_Tbl(Pointer_Mtype));

	} else
	  TY2F_translate(decl_tokens,TY_pointed(ty));
      }
   }
} /* TY2F_pointer */

static void
TY2F_void(TOKEN_BUFFER decl_tokens, TY_IDX ty_idx)
{
  TY& ty = Ty_Table[ty_idx];

  ASSERT_DBG_FATAL(TY_kind(ty) == KIND_VOID, 
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(ty), 
		     "TY2F_void"));

   Prepend_F77_Indented_Newline(decl_tokens, 1, NULL/*label*/);
   Prepend_Token_String(decl_tokens, "! <Void Type>");
} /* TY2F_void */

/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/

void
TY2F_translate(TOKEN_BUFFER tokens, TY_IDX ty)
{
   /* Dispatch the translation-task to the appropriate handler function.
    */
   TY2F_Handler[TY_kind(Ty_Table[ty])](tokens, ty);
} /* TY2F_translate */

void 
TY2F_Translate_ArrayElt(TOKEN_BUFFER tokens, 
			TY_IDX       arr_ty_idx,
			STAB_OFFSET  arr_ofst)
{
  TOKEN_BUFFER idx_tokens = New_Token_Buffer();
  STAB_OFFSET  idx;
  INT32        dim;
  ARB_HANDLE   arb;
  
  ASSERT_FATAL(TY_Is_Array(arr_ty_idx), 
	       (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		TY_kind(arr_ty_idx), "TY2F_Translate_ArrayElt"));
  
  Append_Token_Special(tokens, '(');
  if (TY_Is_Character_String(arr_ty_idx))
    {
      /* Character strings can only be indexed using the substring notation
	*/
      Append_Token_String(tokens, Number_as_String(arr_ofst+1, "%lld"));
      Append_Token_Special(tokens, ':');
      Append_Token_String(tokens, Number_as_String(arr_ofst+1, "%lld"));
    }
  else /* Regular array indexing */
    {
      /* Emit the indexing expressions for each dimension, taking note
       * that Fortran employs column-major array layout, meaning the 
       * leftmost indexing expression (dim==0) represents array elements
       * layed out in contiguous memory locations.
       */

      ARB_HANDLE arb_base = TY_arb(arr_ty_idx);
      dim = ARB_dimension(arb_base) - 1 ; 

      while ( dim >= 0)
      {
	ARB_HANDLE arb = arb_base[dim];

	if (arr_ofst == 0)
	  {
	    Prepend_Token_String(idx_tokens, Number_as_String(1LL, "%lld"));
	  }
	else if (ARB_const_stride(arb)) /* Constant stride */
	  {
	    idx = arr_ofst/ARB_stride_val(arb) + 1;
	    Prepend_Token_String(idx_tokens, Number_as_String(idx, "%lld"));
	    arr_ofst -= (arr_ofst/ARB_stride_val(arb))*ARB_stride_val(arb);
	  }
	else
	  {
	    Append_Token_String(idx_tokens, "*");
	  }
	if (dim-- > 0)
	  Prepend_Token_Special(idx_tokens, ',');
      }
  Append_And_Reclaim_Token_List(tokens, &idx_tokens);
}
Append_Token_Special(tokens, ')');
} /* TY2F_Translate_ArrayElt */

void
TY2F_Translate_Common(TOKEN_BUFFER tokens, const char *name, TY_IDX ty_idx)
{
  TY& ty = Ty_Table[ty_idx];

  BOOL  is_equiv = FALSE;
  
  ASSERT_DBG_FATAL(TY_kind(ty) == KIND_STRUCT, 
		   (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		    TY_kind(ty), "TY2F_Translate_Common"));

  /* Emit specification statements for every element of the
   * common block, including equivalences.
   */
  TY2F_Declare_Common_Flds(tokens, 
			   TY_flist(ty),
			   FALSE, /*alt_return*/
			   &is_equiv);

  /* Emit the common block specification statement, excluding
   * equivalences, where the name is already in a valid form and 
   * can be emitted as is without a call to W2CF_Symtab_Nameof_Ty().
   */

  Append_Token_String(tokens, "COMMON");
  if (name != NULL && *name != '\0')
    Append_Token_String(tokens, Concat3_Strings("/", name, "/"));
  TY2F_List_Common_Flds(tokens, TY_flist(ty));
  
  /* Emit equivalences, if there are any */

  if (is_equiv)
    TY2F_Equivalence_List(tokens, ty_idx /*struct_ty*/);

} /* TY2F_Translate_Common */


void
TY2F_Translate_Equivalence(TOKEN_BUFFER tokens, TY_IDX ty_idx, BOOL alt_return)
{
   /* When alt_return==TRUE, this represents an alternate return variable,
    * in which case we should declare the elements of the equivalence
    * with unmangled names and ignore the fact that they are in an
    * equivalence.  The first element in such an alternate return is
    * the function/subprogram return-variable, which we should never
    * declare.
    */

  TY& ty = Ty_Table[ty_idx];

  FLD_HANDLE first_fld;
  BOOL is_equiv;
   
   ASSERT_DBG_FATAL(TY_kind(ty) == KIND_STRUCT, 
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(ty), "TY2F_Translate_Equivalence"));

   if (alt_return)
   {
      first_fld = FLD_next(TY_flist(ty)); /* skip func_entry return var */
   }
   else
   {
      first_fld = TY_flist(ty);
   }

   /* Emit specification statements for every element of the
    * equivalence block.
    */  
   TY2F_Declare_Common_Flds(tokens, 
			    first_fld,
			    alt_return,
			    &is_equiv);  /* Redundant in this call */

   if (!alt_return)
      TY2F_Equivalence_List(tokens, ty_idx /*struct_ty*/);

} /* TY2F_Translate_Equivalence */

void 
TY2F_Prepend_Structures(TOKEN_BUFFER tokens)
{
   if (TY2F_Structure_Decls != NULL)
      Prepend_And_Reclaim_Token_List(tokens, &TY2F_Structure_Decls);
} /* TY2F_Prepend_Structures */


FLD_PATH_INFO * 
TY2F_Free_Fld_Path(FLD_PATH_INFO *fld_path)
{
   FLD_PATH_INFO *free_list;
   
   if (fld_path != NULL)
   {
      free_list = Free_Fld_Path_Info;
      Free_Fld_Path_Info = fld_path;
      while (fld_path->next != NULL)
	 fld_path = fld_path->next;
      fld_path->next = free_list;
   }
   return NULL;
} /* TY2F_Free_Fld_Path */


FLD_PATH_INFO * 
TY2F_Get_Fld_Path(const TY_IDX struct_ty, 
		  const TY_IDX object_ty,
		  STAB_OFFSET offset)
{
  FLD_PATH_INFO  *fld_path;
  FLD_PATH_INFO  *fld_path2 = NULL;
  TY & s_ty = Ty_Table[struct_ty] ;
  FLD_ITER fld_iter ;
  
  
  ASSERT_DBG_FATAL(TY_kind(s_ty) == KIND_STRUCT,
		   (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		    TY_kind(s_ty),
		    "TY2F_Get_Fld_Path"));
  
  /* Get the best matching field path into fld_path2 */

  fld_iter = Make_fld_iter(TY_flist(s_ty));

  do 
    {
      FLD_HANDLE fld (fld_iter);

      if (NOT_BITFIELD_OR_IS_FIRST_OF_BITFIELD(fld_iter)) 
      {
	fld_path = Construct_Fld_Path(fld_iter,
				      struct_ty,
				      object_ty,
				      offset,
				      TY_size(s_ty));
	if (fld_path2 == NULL)
	  fld_path2 = fld_path;
	else if (fld_path != NULL)
	  fld_path2 = Select_Best_Fld_Path(fld_path2,
					   fld_path,
					   object_ty,
					   offset);
      }
    } while (!FLD_last_field (fld_iter++)) ;

  /* POSTCONDITION: fld_path2 points to the best match found */

  return fld_path2;

} /* TY2F_Get_Fld_Path */

void
TY2F_Translate_Fld_Path(TOKEN_BUFFER   tokens,
			FLD_PATH_INFO *fld_path, 
			BOOL           deref, 
			BOOL           member_of_common, 
			BOOL           alt_ret_name,
			WN2F_CONTEXT   context)
{
   /* Append the name of each field to the tokens, separated them
    * from each other by the field-selection operator ('.').  The
    * first name on the path may optionally be emitted in unclobbered 
    * form, as it may represent an alternate return point.
    */
   while (fld_path != NULL)
   {
      FLD_HANDLE f (fld_path->fld);
      if (deref && TY_Is_Pointer(FLD_type(f)))
	 Append_Token_String(tokens, W2CF_Symtab_Nameof_Fld_Pointee(f));
      else
	  Append_Token_String(tokens, 
			      TY2F_Fld_Name(f,
					    member_of_common,
					    alt_ret_name));

      member_of_common = FALSE; /* Can only be true first time around */

      /* if an array element, form the subscript list. If an OPC_ARRAY */
      /* provides the subscripts, use it o/w use offset                */

      if (fld_path->arr_elt) 
	{
	  if (fld_path->arr_wn != NULL)
	      WN2F_array_bounds(tokens,fld_path->arr_wn,FLD_type(f),context);	  
	  else
	      TY2F_Translate_ArrayElt(tokens,FLD_type(f),fld_path->arr_ofst);
	}

      /* Separate fields with the dot-notation. */

      fld_path = fld_path->next;
      if (fld_path != NULL)
      {
	 TY2F_Fld_Separator(tokens) ;
	 alt_ret_name = FALSE; /* Only applies to first field on the path */
      }
    } /* while */

} /* TY2F_Translate_Fld_Path */



extern void
TY2F_Fld_Separator(TOKEN_BUFFER tokens)
{
  /* puts out the appropriate structure component separator*/

  char p = '.' ;

  if (WN2F_F90_pu) 
       p =  '%';

  Append_Token_Special(tokens,p);
}

extern FLD_HANDLE
TY2F_Last_Fld(FLD_PATH_INFO *fld_path)
{
  FLD_HANDLE f = FLD_HANDLE () ;

  while (fld_path != NULL)
    {
      f = fld_path->fld;
      fld_path = fld_path->next ;
    }

  return f ;
}

extern FLD_PATH_INFO * 
TY2F_Point_At_Path(FLD_PATH_INFO * path, STAB_OFFSET off)
{
  /* given a fld path, return a pointer to */
  /* the slot at the given offset          */

  while (path != NULL)
  {
    if (FLD_ofst(path->fld) >= off)
      break ;

    path=path->next;
  }
  return path;
}

extern void
TY2F_Dump_Fld_Path(FLD_PATH_INFO *fld_path)
{
  printf ("path ::");
  while (fld_path != NULL)
    {
      FLD_HANDLE f = fld_path->fld;

      printf ("%s(#%d)",TY2F_Fld_Name(f,FALSE,FALSE),f.Idx ());

      if (fld_path->arr_elt)
	printf (" array");

      if (fld_path->arr_ofst)
	printf (" offset 0x%x",(mINT32) fld_path->arr_ofst);

      if (fld_path->arr_wn != NULL)
	printf (" tree 0x%p",fld_path->arr_wn);

      printf (" ::");
      fld_path = fld_path->next ;
    }
  printf ("\n");
}

