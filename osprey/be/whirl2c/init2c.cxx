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
 * Module: init2c.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/whirl2c/init2c.cxx,v $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    Translates initializers (INITOs) to C.  Exports the
 *    function:
 *
 *        INITO2C_translate()
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/be/whirl2c/init2c.cxx,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#include "whirl2c_common.h"
#include "st2c.h"
#include "ty2c.h"
#include "tcon2c.h"
#include "init2c.h"


            /* Hidden Utility functions */
            /*--------------------------*/

#define INITV_repeat(x) \
       ((INITV_kind(Initv_Table[x]) == INITVKIND_ZERO ||  \
         INITV_kind(Initv_Table[x]) == INITVKIND_ONE ||  \
         INITV_kind(Initv_Table[x]) == INITVKIND_VAL) ?  \
                 INITV_repeat2(Initv_Table[x]) : INITV_repeat1(Initv_Table[x]))

inline TCON
TCON_For_Initv(INITV_IDX initv)
{
   TCON tcon;

   switch (INITV_kind(initv))
   {
      case INITVKIND_ZERO:
         tcon = Host_To_Targ(INITV_mtype(Initv_Table[initv]), 0);
         break;
      case INITVKIND_ONE:
         tcon = Host_To_Targ(INITV_mtype(Initv_Table[initv]), 1);
         break;
      case INITVKIND_VAL:
         tcon = INITV_tc_val(Initv_Table[initv]);
         break;
      default:
         Is_True(FALSE, ("Unexpected initv kind in TCON_For_Initv()"));
         break;
   }
   return tcon;
} /* TCON_For_Initv */

static void 
INIT2C_Next_Initv(INITV_IDX *inv, INT *inv_repeat)
{
   /* Get the next initializer.
    */
   (*inv_repeat)--;
   if (*inv_repeat <= 0)
   {
      *inv = INITV_next(*inv);
      *inv_repeat = (*inv != 0) ? INITV_repeat(*inv) : 0;
   }
} /* INIT2C_Next_Initv */


static void
INITV2C_translate(TOKEN_BUFFER tokens, 
		  TY_IDX       ty, 
		  INITV_IDX    initv); /* Defined below */


static void
INITV2C_symbol(TOKEN_BUFFER tokens, 
	       TY_IDX       ty, 
	       INITV_IDX    initv)
{
   /* Translate a INITVKIND_SYMOFF and INITVKIND_SYMIPLT to C, ignoring repeats, where
    * the data location being initialized is typed as "ty".
    */
   TOKEN_BUFFER  tmp_tokens;
   CONTEXT       dummy_context;
   BOOL          use_implicit_array_addressing, casted_address;
   ST_IDX        initv_st = INITV_st(Initv_Table[initv]);
   TY_IDX        sym_ty = (ST_class(initv_st) == CLASS_FUNC) ? 
                          ST_pu_type(initv_st) : ST_type(initv_st);
   TY2C_FLD_INFO fld_info;
   fld_info.select_tokens = NULL;
   fld_info.found_fld = FLD_HANDLE();

   Is_True(TY_Is_Pointer_Or_Scalar(ty),
	   ("Unexpected lhs type in INITV2C_symbol()"));
   Is_True(ST_sym_class(St_Table[initv_st]) != CLASS_PREG, 
	   ("Unexpected st class in INITV2C_symbol()"));      
      
   /* See if we can use implicit array addressing (only relevant for 
    * zero offset).
    */
   use_implicit_array_addressing = 
      (TY_Is_Pointer(ty)   &&
       TY_Is_Array(sym_ty) &&
       Stab_Identical_Types(TY_pointed(ty), 
			    TY_AR_etype(sym_ty), 
			    FALSE,   /*check_quals*/
			    TRUE,    /*check_scalars*/
			    FALSE)); /*ptrs_as_scalars*/

   /* When using implicit_array_addressing, the symbol we address
    * is an element of the array.
    */
   if (use_implicit_array_addressing)
      sym_ty = TY_AR_etype(sym_ty);

   /* Generate a type cast to a non-pointer or incompatible pointer
    * type.
    */
   casted_address = (!use_implicit_array_addressing &&
		     (!TY_Is_Pointer(ty) ||
		      !Stab_Identical_Types(TY_pointed(ty), 
					    sym_ty, 
					    FALSE,    /*check_quals*/
					    TRUE,     /*check_scalars*/
					    FALSE))); /*ptrs_as_scalars*/
   if (casted_address)
   {
      tmp_tokens = New_Token_Buffer();
      TY2C_translate_unqualified(tmp_tokens, ty);
      WHIRL2C_parenthesize(tmp_tokens);
      Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
   }

   /* Get the address expression */
   tmp_tokens = New_Token_Buffer();
   CONTEXT_reset(dummy_context);
   if (INITV_ofst(Initv_Table[initv]) == 0)
   {
      if (ST_sym_class(St_Table[initv_st]) == CLASS_CONST)
      {
	 TCON2C_translate(tmp_tokens, STC_val(&St_Table[initv_st]));
      }
      else
      {
	 if (!use_implicit_array_addressing)
	    Append_Token_Special(tmp_tokens, '&');
	 ST2C_use_translate(tmp_tokens, &St_Table[initv_st], dummy_context);
      }
   }
   else if (INITV_ofst(Initv_Table[initv]) % TY_size(sym_ty) == 0)
   {
      /* Use indexing expression to do the address calculation */
      ST2C_use_translate(tmp_tokens, &St_Table[initv_st], dummy_context);
      Prepend_Token_Special(tmp_tokens, '&');

      Append_Token_Special(tmp_tokens, '[');
      Append_Token_String(tmp_tokens, 
      Number_as_String(INITV_ofst(Initv_Table[initv])/TY_size(sym_ty), "%lld"));
      Append_Token_Special(tmp_tokens, ']');
   }
   else /* Need to use byte or struct offsets */
   {     
      /* We fall into this case for struct selection and
       * more complex expression, such as "a[17].field3[21]".
       */
      ST2C_use_translate(tmp_tokens, &St_Table[initv_st], dummy_context);
      Prepend_Token_Special(tmp_tokens, '&');

      if (TY_Is_Struct(sym_ty) && TY_Is_Pointer(ty))
      {
	 /* Try to use field selection */
	 fld_info = TY2C_get_field_info(sym_ty,
					TY_pointed(ty),
					MTYPE_V,  /* Only find field of ty */
					INITV_ofst(Initv_Table[initv]));
	 if (!fld_info.found_fld.Is_Null ())
	 {
	    Append_Token_Special(tmp_tokens, '.');
	    Append_And_Reclaim_Token_List(tmp_tokens, &fld_info.select_tokens);
	 }
      }
      
      if (fld_info.found_fld.Is_Null ())
      {
	 /* Cast to a (char*), do offset calculation, and cast to (void*) */
	 Prepend_Token_Special(tmp_tokens, ')');
	 Prepend_Token_Special(tmp_tokens, '*');
	 Prepend_Token_String(tmp_tokens, "char");
	 Prepend_Token_Special(tmp_tokens, '(');
	 Append_Token_Special(tmp_tokens, '+');
	 Append_Token_String(tmp_tokens, 
			     Number_as_String(INITV_ofst(Initv_Table[initv]),
						      "%lld"));
	 WHIRL2C_parenthesize(tmp_tokens);
	 if (!casted_address)
	 {
	    WHIRL2C_parenthesize(tmp_tokens);
	    Prepend_Token_Special(tmp_tokens, ')');
	    Prepend_Token_Special(tmp_tokens, '*');
	    Prepend_Token_String(tmp_tokens, "void");
	    Prepend_Token_Special(tmp_tokens, '(');
	 }
      }
   }
   Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
} /* INITV2C_symbol */


static void
INITV2C_val(TOKEN_BUFFER tokens,
	    TY_IDX       ty, 
	    INITV_IDX    initv)
{
   /* Translate an INITVKIND_ZERO, INITVKIND_ONE, or INITVKIND_VAL to C,
    * ignoring repeats, where the data location being initialized is
    * typed as "ty".
    */
   TOKEN_BUFFER tmp_tokens;
   TCON tcon = TCON_For_Initv(initv);
   
   Is_True(TY_Is_Pointer_Or_Scalar(ty) || TY_Is_Array_Of_Chars(ty),
	   ("Unexpected lhs type in INITV2C_val()"));
      
   /* Make sure the types are assignment compatible, by casting pointer
    * constants when necessary.  No casting is necessary for NULL
    * pointers.
    */
   if (TY_Is_Pointer(ty) && Targ_To_Host(tcon) != 0LL)
   {
      tmp_tokens = New_Token_Buffer();
      TY2C_translate_unqualified(tmp_tokens, ty);
      WHIRL2C_parenthesize(tmp_tokens);
      Append_And_Reclaim_Token_List(tokens, &tmp_tokens);
   }

   /* Translate the constant value */
   TCON2C_translate(tokens, tcon);
} /* INITV2C_val */


static void
INITV2C_array_dimension(TOKEN_BUFFER tokens,
			TY_IDX       etype, 
			INITV_IDX    next_initv,
			INT          this_dim, /* one based dimension */
			INT          num_dims)
{
   INITV_IDX inv;
   INT    repeat;

   /* The front-end may generate multi-dimensional arrays for arrays
    * of arrays.  
    *
    * Walk through the list of (comma separated) initializers 
    * for the aggregate initializer, ignoring padding.
    */
   Append_Token_Special(tokens, '{');
   for (inv = next_initv; inv != 0; inv = INITV_next(inv))
   {
      /* Handle repetitions */
      for (repeat = INITV_repeat(inv); repeat > 0; repeat--)
      {
	 if (INITV_kind(Initv_Table[inv]) != INITVKIND_PAD)
	 {
	    /* Avoid preceeding the first initializer with a comma */
	    if (inv != next_initv || repeat < INITV_repeat(inv))
	       Append_Token_Special(tokens, ',');
	    
	    if (this_dim < num_dims &&
                INITV_kind(Initv_Table[inv]) == INITVKIND_BLOCK)
	       INITV2C_array_dimension(tokens, 
				       etype, 
				       INITV_blk(inv),
				       this_dim+1,
				       num_dims);
	    else
	       INITV2C_translate(tokens, etype, inv);
	 } /* if */
      } /* for */
   } /* for */
   Append_Token_Special(tokens, '}');
} /* INITV2C_array_dimension */


static void
INITV2C_block_array(TOKEN_BUFFER tokens,
		    TY_IDX       ty, 
		    INITV_IDX    initv)
{
   /* Translate a INITVKIND_BLOCK for an array to C, where the 
    * location being initialized is typed as "ty" (KIND_ARRAY).
    */
   INITV_IDX first_inv = INITV_blk(initv);
   TY_IDX    etype = TY_AR_etype(ty);
   INT    ndims = TY_AR_ndims(ty);
   
   if (!TY_Is_Array(etype) && !TY_Is_String(etype) &&
       INITV_kind(Initv_Table[first_inv]) == INITVKIND_VAL &&
       TCON_ty(INITV_tc_val(Initv_Table[first_inv])) == MTYPE_STRING)
   {
      Is_True((TY_size(ty) ==
               Targ_String_Length(INITV_tc_val(Initv_Table[first_inv]))) ||
	      (INITV_next(first_inv) != 0 &&
	       INITV_kind(Initv_Table[INITV_next(first_inv)]) == INITVKIND_PAD),
	      ("Expected padding to follow incomplete string initializer"));
      INITV2C_translate(tokens, ty, first_inv);
   }
   else /* Not incomplete string initializer */
   {
      if (first_inv != 0 && 
	  INITV_kind(Initv_Table[first_inv]) == INITVKIND_PAD &&
	  INITV_next(first_inv) == 0)
      {
	 /* Handle the special case of just having padding as an 
	  * initializer.
	  */
	 Append_Token_Special(tokens, '{');
	 Append_Token_String(tokens, "0L");
	 Append_Token_Special(tokens, '}');
      }
      else
      {
	 INITV2C_array_dimension(tokens, etype, first_inv,  1, ndims);
      } /* if */
   } /* if */
} /* INITV2C_block_array */


static void
INITV2C_struct_fill(TOKEN_BUFFER tokens,
		    INITV_IDX   *current_inv, 
		    INT         *inv_repeat,
		    INT64       *current_offset, 
		    INT64        desired_offset)
{
   /* Using the inv sequence of initializers, initialize
    * a byte-array with bytes up to the desired offset.
    * Thus, the number of bytes initialized will be limited
    * by the inv list and the "desired_offset-current_offset".
    * Padding will be viewed as zero initializers.  Update
    * the current_inv, the inv_repeat, and the current_offset.
    */
   union Tcon_Bytes
   {
      struct
      {
	 UINT32 u1;
	 UINT32 u2;
	 UINT32 u3;
	 UINT32 u4;
	 UINT32 u5;
	 UINT32 u6;
	 UINT32 u7;
	 UINT32 u8;
      } val;
      mUINT8 byte[32];
   } tcon_bytes;

   INT      pad_byte;
   INT      tcon_size;
   TCON     tcon;
   BOOL do_paren = TRUE;
   
   Is_True(*current_inv != 0 && *current_offset < desired_offset,
	   ("Incorrect call to INITV2C_struct_fill()"));

   if(*current_offset < desired_offset && *current_inv != 0 &&
       INITV_kind(Initv_Table[*current_inv]) == INITVKIND_PAD)
     do_paren = FALSE;

   if(do_paren)
     Append_Token_Special(tokens, '{');
   while (*current_offset < desired_offset && *current_inv != 0)
   {
      /* Expect only padding and integral val INITVs */
      switch (INITV_kind(Initv_Table[*current_inv]))
      {
      case INITVKIND_UNK:
	 Is_True(FALSE, ("Unknown initv kind in INITV2C_struct_fill()"));
	 break;

      case INITVKIND_SYMOFF:
	 Is_True(FALSE, ("SYMOFF initv kind in INITV2C_struct_fill()"));
	 break;

#ifdef TARG_IA64	 
      case INITVKIND_SYMIPLT:
	 Is_True(FALSE, ("SYMIPLT initv kind in INITV2C_struct_fill()"));
	 break;
#endif

      case INITVKIND_ZERO:
      case INITVKIND_ONE:
      case INITVKIND_VAL:
         do_paren = TRUE;
         tcon = TCON_For_Initv(*current_inv);
	 tcon_size = TY_size(Stab_Mtype_To_Ty(TCON_ty(tcon)));
	 if (tcon_size < sizeof(tcon.vals.uval.u0))
	 {
	    /* Shift the relevant bytes into the appropriate position */
	    tcon_bytes.val.u1 = 
	       (tcon.vals.uval.u0 << 
		8*(sizeof(tcon.vals.uval.u0) - tcon_size));
	 }
	 else
	 {
	    /* Assign all eight 32 bits words of the tcon value to 
	     * tcon_bytes.
	     */
	    tcon_bytes.val.u1 = tcon.vals.uval.u0;
	    tcon_bytes.val.u2 = tcon.vals.uval.u1;
	    tcon_bytes.val.u3 = tcon.vals.uval.u2;
	    tcon_bytes.val.u4 = tcon.vals.uval.u3;
	    tcon_bytes.val.u5 = tcon.cmplxval.ival.v0;
	    tcon_bytes.val.u6 = tcon.cmplxval.ival.v1;
	    tcon_bytes.val.u7 = tcon.cmplxval.ival.v2;
	    tcon_bytes.val.u8 = tcon.cmplxval.ival.v3;
	 }
	 
	 /* The tcon bytes of relevance are now in tcon_bytes */
	 for (pad_byte = 0; pad_byte < tcon_size; pad_byte++)
	 {
	 }
	 *current_offset += tcon_size;
	 break;

      case INITVKIND_BLOCK:
	 Is_True(FALSE, ("BLOCK initv kind in INITV2C_struct_fill()"));
	 break;

      case INITVKIND_PAD:
	 /* View padding as zero initialized */
	 for (pad_byte = INITV_pad(Initv_Table[*current_inv]);
              pad_byte > 0;
              pad_byte--)
	 {
	    Append_Token_String(tokens, "0");
	    if (pad_byte > 1)
	       Append_Token_Special(tokens, ',');
	 }
	 *current_offset += INITV_pad(Initv_Table[*current_inv]);
	 break;

      default:
	 Is_True(FALSE, ("Unexpected initv kind in INITV2C_struct_fill()"));
	 break;

      } /* switch */
      
      INIT2C_Next_Initv(current_inv, inv_repeat);
      
      /* Seperate the last filler byte emitted from the next one
       * with a comma.
       */
      if (*current_offset < desired_offset && *current_inv != 0)
	 Append_Token_Special(tokens, ',');

   } /*while*/
   if(do_paren)
     Append_Token_Special(tokens, '}');
} /* INITV2C_struct_fill */


static void
INITV2C_block_struct(TOKEN_BUFFER tokens,
		     TY_IDX       ty, 
		     INITV_IDX    initv)
{
   /* Translate a INITVKIND_BLOCK for a struct to C, where the
    * location being initialized is typed as "ty" (KIND_STRUCT).
    */
   INITV_IDX     inv, first_inv = INITV_blk(initv);
   INT           inv_repeat;
   INT64         local_offset;
   FLD_HANDLE    fld;
   TY2C_FLD_INFO fld_info;
   BOOL          a_field_is_initialized = FALSE;
   BOOL          do_comma = TRUE;

   /* Walk through the list of (comma separated) initializers 
    * for the aggregate initializer, ignoring padding.
    */
   Append_Token_Special(tokens, '{');
   if (first_inv != 0 && 
       INITV_kind(Initv_Table[first_inv]) == INITVKIND_PAD &&
       INITV_next(first_inv) == 0)
   {
      /* Handle the special case of just having padding as an 
       * initializer.
       */
      Append_Token_String(tokens, "0L");
   }
   else
   {
      /* Walk through the list of structure fields, initializing
       * field separating padding, but leaving padding at the end
       * of a struct uninitialized.
       */
      local_offset = 0;
      inv = first_inv;
      inv_repeat =  (inv != 0) ? INITV_repeat(inv) : 0;
      for (fld = TY_flist(Ty_Table[ty]); 
	   !fld.Is_Null () && inv != 0; 
	   fld = FLD_next(fld))
      {
	 if (FLD_ofst(fld) >= local_offset)
	 {
	    /* This field does not overlap any previous fields.
	     */
	    fld_info = TY2C_get_field_info(ty,
					   FLD_type(fld),
					   MTYPE_V,
					   FLD_ofst(fld));
	    if (!fld_info.found_fld.Is_Null ())
	    {
	       /* This field is declared in the C output.
		*/
	       Reclaim_Token_Buffer(&fld_info.select_tokens);
	       if (FLD_ofst(fld) > local_offset)
	       {
		  /* Insert padding before this field.
		   */
		  if (a_field_is_initialized)
		     Append_Token_Special(tokens, ',');
		  else
		     a_field_is_initialized = TRUE;
		  if (INITV_kind(Initv_Table[inv]) == INITVKIND_PAD)
		    do_comma = FALSE;
		  INITV2C_struct_fill(tokens,
				      &inv, &inv_repeat,
				      &local_offset, FLD_ofst(fld));
	       }
	       if (inv != 0)
	       {
		  /* An initializer exists for this field.
		   */
		  if (a_field_is_initialized)
		   if(do_comma)
		     Append_Token_Special(tokens, ',');
		   else
		    do_comma = TRUE;
		  else
		     a_field_is_initialized = TRUE;

		  /* Work around a Fortran bug! TODO: Rewrite this to
		   * be more similar to whirl2f and to work for all cases.
		   */
		  if (PU_src_lang(Get_Current_PU()) == PU_F77_LANG &&
		      (INITV_kind(Initv_Table[inv]) == INITVKIND_ZERO ||
		       INITV_kind(Initv_Table[inv]) == INITVKIND_ONE  ||
		       INITV_kind(Initv_Table[inv]) == INITVKIND_VAL))
		  {
		     INT fld_size = TY_size(Ty_Table[FLD_type(fld)]);
		     TY_IDX inv_ty;

		     if (TY_Is_Array(FLD_type(fld)) ||
			 TY_Is_Structured(FLD_type(fld)))
		     {
			Append_Token_Special(tokens, '{');
		     }

		     while (inv != 0                         &&
			    fld_size > 0                     &&
			    (INITV_kind(Initv_Table[inv]) == INITVKIND_ZERO ||
			     INITV_kind(Initv_Table[inv]) == INITVKIND_ONE  ||
			     INITV_kind(Initv_Table[inv]) == INITVKIND_VAL))
		     { 
			inv_ty = Stab_Mtype_To_Ty(TCON_ty(TCON_For_Initv(inv)));
			INITV2C_translate(tokens, inv_ty, inv);
			fld_size -= TY_size(inv_ty);
			INIT2C_Next_Initv(&inv, &inv_repeat);
			if (fld_size > 0)
			   Append_Token_Special(tokens, ',');
		     }
		     if (TY_Is_Array(FLD_type(fld)) ||
			 TY_Is_Structured(FLD_type(fld)))
		     {
			Append_Token_Special(tokens, '}');
		     }
		  }
		  else
		  {
		     INITV2C_translate(tokens, FLD_type(fld), inv);
		     INIT2C_Next_Initv(&inv, &inv_repeat);
		  }

		  local_offset += TY_size(FLD_type(fld));
	       } /*if*/
	    } /*if*/
	 } /*if*/
      } /*for*/
      
      /* Explicitly apply any outstanding initializer.  We can here
       * Assert that (fld!=0 xor inv!=0).
       */
      if (local_offset < TY_size(ty) && inv != 0)
      {
	 if (a_field_is_initialized)
	    Append_Token_Special(tokens, ',');
	 INITV2C_struct_fill(tokens,
			     &inv, &inv_repeat,
			     &local_offset, TY_size(ty));
      } /*if*/
   } /*if*/
   Append_Token_Special(tokens, '}');
} /* INITV2C_block_struct */


static void
INITV2C_block_union(TOKEN_BUFFER tokens,
		    TY_IDX       ty, 
		    INITV_IDX    initv)
{
   /* Translate a INITVKIND_BLOCK for a union to C, where the
    * location being initialized is typed as "ty" (KIND_STRUCT).
    */
   INITV_IDX first_inv = INITV_blk(initv);
   INITV_IDX next_inv = INITV_next(first_inv);

   /* Make sure only one element of the union is initialized; allow a
    * second initializer for the union only if it is for padding.
    */
   Is_True((next_inv == 0 ||
            (INITV_kind(Initv_Table[next_inv]) == INITVKIND_PAD &&
             INITV_next(next_inv) == 0)),
	   ("Expected an initializer only for the first element of a union"));

   /* Initialize the first element of the union */
   Append_Token_Special(tokens, '{');
   INITV2C_translate(tokens, FLD_type(TY_flist(Ty_Table[ty])),
                     first_inv);
   Append_Token_Special(tokens, '}');
} /* INITV2C_block_union */


static void
INITV2C_translate(TOKEN_BUFFER tokens, 
		  TY_IDX       ty, 
		  INITV_IDX    initv)
{
   switch (INITV_kind(Initv_Table[initv]))
   {
   case INITVKIND_UNK:
      Is_True(FALSE, ("Unknown initv kind in INITV2C_translate()"));
      break;

   case INITVKIND_SYMOFF:
#ifdef TARG_IA64
   case INITVKIND_SYMIPLT:
#endif
      INITV2C_symbol(tokens, ty, initv);
      break;
   case INITVKIND_ZERO:
   case INITVKIND_ONE:
   case INITVKIND_VAL:
      /* May be a scalar or a string initializer */
      INITV2C_val(tokens, ty, initv);
      break;

   case INITVKIND_BLOCK:
      /* Expect an array, a union or a struct ty.  Assert on
       * anything else!  Note that a string may occur as a
       * block initializer when it has padding.
       */
      if (TY_Is_Array(ty))
	 INITV2C_block_array(tokens, ty, initv);
      else if (TY_Is_Union(ty))
	 INITV2C_block_union(tokens, ty, initv);
      else if (TY_Is_Struct(ty))
	 INITV2C_block_struct(tokens, ty, initv);
      else
	 Is_True(FALSE, ("Unexpected aggregate type in INITV2C_translate()"));
      break;

   case INITVKIND_PAD:
      /* Simply ignore padding */
      break;
   
   default:
      Is_True(FALSE, ("Unexpected initv kind in INITV2C_translate()"));
      break;

   } /* switch */
} /* INITV2C_translate */

   
void
INITO2C_translate(TOKEN_BUFFER tokens, INITO_IDX inito)
{
   /* There is one INITO denoting the object initializer.  The INITO
    * points to the object it references (ST*), and the INITO_ofst
    * attribute (offset in data section) has no significance for 
    * the translation to C.
    */
   if (Stab_Is_Common_Block(INITO_st(inito)) ||
       Stab_Is_Equivalence_Block(INITO_st(inito)))
   {
      Append_Token_Special(tokens, '{');
      INITV2C_translate(tokens, ST_type(INITO_st(inito)), INITO_val(inito));
      Append_Token_Special(tokens, '}');
   }
   else
      INITV2C_translate(tokens, ST_type(INITO_st(inito)), INITO_val(inito));
} /* INITO2C_translate */
