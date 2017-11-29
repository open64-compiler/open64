/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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
 * Module: init2f.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/init2f.cxx,v $
 *
 * Revision history:
 *  15-June-95 - Original Version
 *
 * Description:
 *
 *    Translates initializers (INITOs) to Fortran DATA statements.
 *    Exports the function:
 *
 *        INITO2F_translate()
 *
 *    Note that the function parameter initv_times has two meanings.
 *
 *    When used to get the next initv (this not being padding) it
 *    indicates how many of the repeat counts we have used up for
 *    the current initv; when the repeat count is always one (1),
 *    the initv_idx advances while initv_times remains at zero (0).
 *
 *    When used to skip padding it indicates how much of the next 
 *    padding value has already been accounted for in number of bytes.
 *
 *    This dual meaning is possible since we either are in a padding-
 *    skipping mode or we are processing non-padding initvs, where 
 *    these modes are exclusive as far as the initv_times counter is 
 *    concerned.  Once a complete padding has been skipped, "initv_times"
 *    should have been set to zero (0), thus having prepared for a 
 *    subsequent call to INIT2F_Next_Initv().
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/init2f.cxx,v $ $Revision: 1.1 $";
#endif

#include "whirl2f_common.h"
#include "PUinfo.h"
#include "st2f.h"
#include "wn2f.h"
#include "ty2f.h"
#include "tcon2f.h"
#include "init2f.h"


/*------------------- Buffer to hold Data Statements -------------------*/
/*----------------------------------------------------------------------*/
 
/* Is initialized when entering a PU block and reclaimed 
 * when exiting a PU block.
 */
extern TOKEN_BUFFER Data_Stmt_Tokens;  /* Defined in wn2f.c */


/*--------------------------- Utility Routines -------------------------*/
/*----------------------------------------------------------------------*/


#define OFFSET_IS_IN_FLD(fld, ofst) \
   (FLD_ofst(fld) == ofst || \
    (ofst > FLD_ofst(fld) && (ofst - FLD_ofst(fld) < TY_size(FLD_type(fld)))))


static void
Set_Tcon_Value(TCON *tcon, MTYPE mtype, INT typesize, char *bytes)
{
   typedef struct Tcon_Value
   {
      union
      {
	 INT8      i1;
	 UINT8     u1;
	 INT16     i2;
	 UINT16    u2;
	 INT32     i4;
	 UINT32    u4;
	 INT64     i8;
	 UINT64    u8;
	 float     f[2];
	 double    d[2];
         long double ld;
	 QUAD_TYPE q;
      } val1;
      union
      {
	 float     f;
	 double    d;
         long double ld;
	 QUAD_TYPE q;
      } val2;
   } TCON_VALUE;

   union
   {
      char       byte[sizeof(TCON_VALUE)];
      TCON_VALUE val;
   }   rep;
   INT i;

   INT k = 0 ;

   if  (typesize < 4) 
     k = 4 - typesize;

   for (i = 0; i < typesize ; i++)
      rep.byte[i+k] = bytes[i];

   switch (mtype)
   {
   case MTYPE_I1:
     rep.val.val1.i1 = ( rep.val.val1.i1 << 24) >> 24 ; /* sign extend */
     *tcon = Host_To_Targ(mtype, rep.val.val1.i1);
      break;

   case MTYPE_I2:
     rep.val.val1.i2 = ( rep.val.val1.i2 << 16) >> 16 ;
     *tcon = Host_To_Targ(mtype, rep.val.val1.i2);
     break;

   case MTYPE_I4:
      *tcon = Host_To_Targ(mtype, rep.val.val1.i4);
      break;

   case MTYPE_I8:
      *tcon = Host_To_Targ(mtype, rep.val.val1.i8);
      break;

   case MTYPE_U1:
      *tcon = Host_To_Targ(mtype, rep.val.val1.u1);
      break;

   case MTYPE_U2:
      *tcon = Host_To_Targ(mtype, rep.val.val1.u2);
      break;

   case MTYPE_U4:
      *tcon = Host_To_Targ(mtype, rep.val.val1.u4);
      break;

   case MTYPE_U8:
      *tcon = Host_To_Targ(mtype, rep.val.val1.u8);
      break;

   case MTYPE_F4:
      /* TODO: export Host_To_Targ_Float_4() from be.so 
       */
      *tcon = Host_To_Targ_Float(mtype, rep.val.val1.f[0]);
      break;

   case MTYPE_F8:
      *tcon = Host_To_Targ_Float(mtype, rep.val.val1.d[0]);
      break;

#if defined(TARG_X8664) || defined(TARG_IA64)
   case MTYPE_F10:
      *tcon = Host_To_Targ_Float_10(mtype, rep.val.val1.ld);
      break;
#endif

   case MTYPE_FQ:
     *tcon = Host_To_Targ_Quad(rep.val.val1.q);
     break;

   case MTYPE_C4:
     *tcon = Host_To_Targ_Complex_4 (mtype,rep.val.val1.f[0],rep.val.val1.f[1]);
     break;

   case MTYPE_C8: 
     *tcon = Host_To_Targ_Complex (mtype,rep.val.val1.d[0],rep.val.val1.d[1]);
     break;

#if defined(TARG_X8664) || defined(TARG_IA64)
   case MTYPE_C10:     
     *tcon = Host_To_Targ_Complex_10 (mtype, rep.val.val1.ld,rep.val.val2.ld);
     break;
#endif

   case MTYPE_CQ:     
     *tcon = Host_To_Targ_Complex_Quad (rep.val.val1.q,rep.val.val2.q);
     break;

   default:
      ASSERT_DBG_FATAL(FALSE,
		       (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
			mtype, "Set_Tcon_Value"));
      break;
   }
} /* Set_Tcon_Value */


static void
INIT2F_Prepend_Equivalence(TOKEN_BUFFER tokens,
			   TOKEN_BUFFER name1_tokens,
			   UINT         tmpvar_idx)
{
   /* Generate an equivalence declaration in the "tokens" buffer,
    * where a temporary variable is equivalenced to some other
    * memory reference.
    */
   Prepend_F77_Indented_Newline(tokens, 1, NULL/*label*/);
   Prepend_Token_Special(tokens, ')');
   Prepend_Token_String(tokens, W2CF_Symtab_Nameof_Tempvar(tmpvar_idx));
   Prepend_Token_Special(tokens, ',');
   Prepend_And_Copy_Token_List(tokens, name1_tokens);
   Prepend_Token_Special(tokens, '(');
   Prepend_Token_String(tokens, "EQUIVALENCE");
} /* INIT2F_Prepend_Equivalence */


static void 
INIT2F_Append_Initializer(TOKEN_BUFFER  tokens, 
			  TOKEN_BUFFER *init_tokens,
			  INT           repeat)
{
   /* Given the tokens for an initializer value or memory reference,
    * indicate a repeat-factor ('*') and preceede this initializer
    * with a comma if the "tokens" buffer is non-empty.
    */
   if (repeat > 1)
   {
      Prepend_Token_Special(*init_tokens, '*');
      Prepend_Token_String(*init_tokens, Number_as_String(repeat, "%llu"));
   }
   if (!Is_Empty_Token_Buffer(tokens))
      Append_Token_Special(tokens, ',');
   Append_And_Reclaim_Token_List(tokens, init_tokens);
} /* INIT2F_Append_Initializer */

static UINT16
INIT2F_choose_repeat(const INITV& initv)
{
  UINT16 rep = 0 ; 

  switch(INITV_kind(initv))
    {
    case INITVKIND_ZERO:
    case INITVKIND_ONE:
    case INITVKIND_VAL:
      rep = INITV_repeat2(initv);
      break;

    default:
      rep = INITV_repeat1(initv);
      break;
    }

  return rep ;
}

static void 
INIT2F_Next_Initv(const INITV& initv,
		  UINT  *initv_idx,
		  UINT  *initv_times)
{

   /* Only use this to get the next initv when the current
    * initv is *not* an INITVKIND_PAD.  For padding use
    * INIT2F_Skip_Padding() instead.
    */
   if (*initv_times+1 < INIT2F_choose_repeat(initv))
   {
      (*initv_times)++;
   }
   else
   {
      *initv_times = 0;
      (*initv_idx)++;
   }
} /* INIT2F_Append_Initializer */

static void 
INIT2F_Skip_Padding(INITV_IDX    *initv_array,
		    TY_IDX       object_ty,   /* Padding occurs in this type */
		    STAB_OFFSET *ofst,        /* offset from object_ty base */
		    UINT        *initv_idx)   /* Index to a padding initv */
{
   /* Note that padding is skipped on a byte-by-byte basis, where 
    * the bytes skipped are indicated by the pad_used (initv_times)
    * variable.
    */
   INITV_IDX initv;

   for (initv = initv_array[*initv_idx];
	(*ofst < TY_size(object_ty) &&
	 initv != (INITV_IDX) 0     &&
	 INITV_kind(Initv_Table[initv]) == INITVKIND_PAD);
	initv = initv_array[++(*initv_idx)])
   {
      *ofst += INITV_pad(Initv_Table[initv])*INIT2F_choose_repeat(Initv_Table[initv]);
   }
   if (*ofst < TY_size(object_ty) && initv == (INITV_IDX) 0)
      *ofst = TY_size(object_ty); /* To handle bugs in WHIRL INITV structure */
} /* INIT2F_Skip_Padding */

static UINT
INIT2F_Number_Of_Initvs(INITV_IDX initv)
{
  UINT   count = 0;
  UINT64 rep;

  while (initv != 0) 
    {
      INITV& ini = Initv_Table[initv];

      if (INITV_kind(ini) == INITVKIND_BLOCK)
	{
	  for (rep = 1; rep <= INIT2F_choose_repeat(ini) ; rep++)
	    count += INIT2F_Number_Of_Initvs(INITV_blk(ini));
	}
      else
	count += 1;

      initv = INITV_next(initv);
    }  
  return count;
} /* INIT2F_Number_Of_Initvs */

static void
INIT2F_Collect_Initvs(INITV_IDX *initv_array, UINT *initv_idx, INITV_IDX initv)
{
   UINT64 rep;

   while (initv != (INITV_IDX) 0)
   {
      if (INITV_kind(Initv_Table[initv]) == INITVKIND_BLOCK)
	 for (rep = 1; rep <= INIT2F_choose_repeat(Initv_Table[initv]); rep++)
	    INIT2F_Collect_Initvs(initv_array, initv_idx, INITV_blk(Initv_Table[initv]));
      else
	 initv_array[(*initv_idx)++] = initv;

      initv = INITV_next(initv);
   }
} /* INIT2F_Collect_Initvs */

static INITV_IDX  *
INIT2F_Get_Initv_Array(ST *st, INITO_IDX first_inito)
{
  /* Allocate an array of INITV_IDXs, and initialize it to hold all
   * top-level INITVs applying to the given ST.  The array must be
   * freed by the caller when it is no longer used.  Flatten out
   * any nested INITV_blocks.
   */

  UINT    number_of_initvs = 1;
  INITV_IDX *initv_array;
  UINT i ;

  /* Count the initv's for this object */

  INITO  *ini = &Inito_Table[first_inito] ;

  FOREACH_INITO(ST_level(st),ini,i) 
   {
      if (INITO_st(ini) == st)
        number_of_initvs += INIT2F_Number_Of_Initvs(INITO_val(*ini));
   }
   
   /* Allocate and initialize the initv array for this object */

   initv_array = TYPE_ALLOC_N(INITV_IDX, number_of_initvs);
   initv_array[number_of_initvs-1] = (INITV_IDX) 0; /* terminator */
   number_of_initvs = 0;

  ini = &Inito_Table[first_inito] ;

  FOREACH_INITO(ST_level(st),ini,i) 
    {
      if (INITO_st(ini) == st)
         INIT2F_Collect_Initvs(initv_array, &number_of_initvs, INITO_val(*ini));
     }
   return initv_array;

} /* INIT2F_Get_Initv_Array */

/*--------- Routines to organize and handle each kind of INITV ---------*
 *----------------------------------------------------------------------*/

static TY_IDX
INITVKIND_ty(INITV_IDX initv_idx)
{
  /* Determine what type of initializer we have.
   */
  INITV& initv = Initv_Table[initv_idx] ;
  TY_IDX initv_ty;

  switch (INITV_kind(initv)) 
    {
    case INITVKIND_VAL:
      if (TCON_ty(INITV_tc_val(initv)) == MTYPE_STRING)
	{
	  initv_ty = Stab_Array_Of(Stab_Mtype_To_Ty(MTYPE_U1),
				   Targ_String_Length(INITV_tc_val(initv)));
	  Set_TY_is_character(Ty_Table[initv_ty]);
	}
      else
	initv_ty = Stab_Mtype_To_Ty(TCON_ty(INITV_tc_val(initv)));
      break;

    case INITVKIND_SYMOFF:

      /* A pointer type, we have no idea what pointer type if 
       * the symbol is a structure.
       */
      if (TY_Is_Structured(ST_type(INITV_st(initv))))
	initv_ty = Stab_Pointer_To(Void_Type);
      else
	initv_ty = Stab_Pointer_To(ST_type(INITV_st(initv)));
      break;

    case INITVKIND_ZERO:
    case INITVKIND_ONE:
      initv_ty = Be_Type_Tbl(INITV_mtype(initv));
      break;

    default:
      ASSERT_DBG_FATAL(FALSE,
		       (DIAG_W2F_UNEXPECTED_INITV, 
			INITV_kind(initv), "INITVKIND_ty"));

    }

  return initv_ty;

} /* INITVKIND_ty */

static void
INITVKIND_symoff(TOKEN_BUFFER tokens,
		 INT          repeat,
		 ST          *st,
		 STAB_OFFSET  ofst,
		 TY_IDX       object_ty)
{
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;
   TOKEN_BUFFER symref_tokens = New_Token_Buffer();

   WN2F_Offset_Symref(symref_tokens,
		      st,
		      Stab_Pointer_To(ST_type(st)),
		      object_ty,
		      ofst,
		      context);
   WN2F_Address_Of(symref_tokens);
   INIT2F_Append_Initializer(tokens, &symref_tokens, repeat);
} /* INITVKIND_symoff */

static void
INITVKIND_val(TOKEN_BUFFER tokens, 
	      INT          repeat,
	      TCON        *tcon,
	      TY_IDX       object_ty)
{
   /* Translate the constant value and prepend the repeat count.
    * TODO: handle logical values correctly.
    */
   TOKEN_BUFFER val_tokens = New_Token_Buffer();

   if (TCON_ty(*tcon) == MTYPE_STRING && 
       !TY_Is_Array(object_ty) && !TY_Is_String(object_ty))
   {
      /* Special case to handle some F90 initializers
       */
      if (TY_Is_Scalar(object_ty))
      {
	 char *strbase = Targ_String_Address(*tcon);
	 INT   strlen = Targ_String_Length(*tcon);
	 INT   stridx;
	 INT   repeatcount = 0;
	 TCON  t;
	 char *valp = (TY_Is_Complex(object_ty)? 
		       (char *)&t.cmplxval : 
		       (char *)&t.vals);

	 while (repeatcount++ < repeat)
	 {
	    stridx = 0;
	    while (stridx < strlen)
	    {
	       Set_Tcon_Value(&t, 
			      TY_mtype(object_ty), 
			      TY_size(object_ty), 
			      &strbase[stridx]);
	       TCON2F_translate(val_tokens, t, TY_is_logical(Ty_Table[object_ty]));
	       stridx += TY_size(object_ty);
	       if (stridx < strlen)  
		  Append_Token_Special(val_tokens, ',');
 
	    }
	 }
      }
   }
   else
   {
      /* The normal case, where INITVs match the object initialized
       */
      TCON2F_translate(val_tokens, *tcon, TY_is_logical(Ty_Table[object_ty]));
   }
   INIT2F_Append_Initializer(tokens, &val_tokens, repeat);
} /* INITVKIND_val */


/* put out integer one/zero/t/f for initvs */

static const char * one_consts[6]  = { "1", ".TRUE.", "1_1", "1_2" , "1_4", "1_8"} ;
static const char * zero_consts[6] = { "0", ".FALSE.","0_1", "0_2" , "0_4", "0_8"} ;

static void
INITVKIND_const(TOKEN_BUFFER tokens, 
		 INT          repeat,
		 const char** tbl,   
		 TY_IDX       ty)
{
  const char *p = tbl[0];

  TOKEN_BUFFER val_tokens = New_Token_Buffer();

  if (TY_is_logical(Ty_Table[ty])) 
    p = tbl[1];
  else {

    if (WN2F_F90_pu) {
      switch (TY_mtype(ty)) {
      case MTYPE_I1:  p = tbl[2]; break;
      case MTYPE_I2:  p = tbl[3]; break;
      case MTYPE_I4:  p = tbl[4]; break;
      case MTYPE_I8:  p = tbl[5]; break;
      }
    }
  }
  Append_Token_String(val_tokens,p);
  INIT2F_Append_Initializer(tokens, &val_tokens, repeat);
}


static void
INITVKIND_translate(TOKEN_BUFFER tokens, 
		    INITV_IDX    initv_idx,
		    TY_IDX       object_ty,
		    UINT         repeat)
{
  INITV& initv = Initv_Table[initv_idx];

  switch (INITV_kind(initv))
   {
   case INITVKIND_SYMOFF:
      INITVKIND_symoff(tokens,
		       repeat,
		       &St_Table[INITV_st(initv)], 
		       INITV_ofst(initv), 
		       object_ty);
      break;
	 
   case INITVKIND_VAL:
      INITVKIND_val(tokens, repeat, &Tcon_Table[INITV_tc(initv)], object_ty);
      break;
      
   case INITVKIND_ONE:
      INITVKIND_const(tokens, repeat, one_consts, object_ty);
      break;

   case INITVKIND_ZERO:
      INITVKIND_const(tokens, repeat, zero_consts, object_ty);
      break;

   default:
      ASSERT_DBG_WARN(FALSE, (DIAG_W2F_UNEXPECTED_INITV,
			      INITV_kind(initv), "INITV2F_ptr_or_scalar"));
      break;
   }
} /* INITVKIND_translate */

/*----------- Utilities for character string initialization ------------*
 *----------------------------------------------------------------------*/

static void
INIT2F_Translate_Char_Ref(TOKEN_BUFFER  tokens, /* Append reference here */
			  ST           *base_object,
			  TY_IDX        array_etype, /* array element type */
			  STAB_OFFSET   base_ofst,   /* ofst to array */
			  STAB_OFFSET   array_ofst,  /* ofst within array */
			  STAB_OFFSET   string_ofst, /* ofst within string */
			  UINT          string_size,
			  WN2F_CONTEXT  context)
{
   /* Translate a reference to a substring of size "string_size" at 
    * offset:
    *
    *     base_ofst + array_ofst + string_ofst
    *
    * within the "base_object".
    */

   /* Generate the array indexing expression */
   WN2F_Offset_Symref(tokens,
		      base_object,
		      Stab_Pointer_To(ST_type(base_object)),
		      array_etype,
		      base_ofst + array_ofst,
		      context);

   /* Generate the substring expression */
   if (string_size != TY_size(array_etype))
   {
      Append_Token_Special(tokens, '(');
      Append_Token_String(tokens, 
			  Number_as_String(string_ofst+1, "%llu"));
      Append_Token_Special(tokens, ':');
      Append_Token_String(tokens, 
			  Number_as_String(string_ofst+string_size, "%llu"));
      Append_Token_Special(tokens, ')');
   }
} /* INIT2F_Translate_Char_Ref */


/*------------------ Utilities for array initialization ----------------*
 *----------------------------------------------------------------------*/

typedef struct Array_Segment
{
   INITV_IDX   *initv_array;  /* Array of initializers */
   BOOL        missing_padding; /* Reached unexpected end of initv sequence */
   UINT        num_initvs;   /* Number of initializing elements */
   UINT        first_idx;    /* Index of first initializer */
   UINT        last_idx;     /* Index of last initializer */
   UINT        first_repeat; /* Times the first initv should be repeated */
   UINT        last_repeat;  /* Times the last initv should be repeated */
   STAB_OFFSET start_ofst;   /* Offset to start of initialized array segment */
   STAB_OFFSET end_ofst;     /* Offset to end of initialized array segment */
   TY_IDX      atype;        /* Array type */
   TY_IDX      etype;        /* Array element type */
} ARRAY_SEGMENT;


static BOOL
INIT2F_is_string_initv(INITV&  ini, TY_IDX ty)
{
  BOOL res = FALSE;

  if (INITV_kind(ini) == INITVKIND_VAL) 
  {
    res = (TCON_ty(INITV_tc_val(ini)) == MTYPE_STRING &&
	   TY_size(ty) > 0                   && /* necessary? */
	   TY_size(ty) < Targ_String_Length(INITV_tc_val(ini))) ;

  }
  return res ;
}

static ARRAY_SEGMENT
INIT2F_Get_Array_Segment(INITV_IDX   *initv_array, /* in */
			 UINT        *initv_idx,   /* in out*/
			 UINT        *initv_times, /* in out*/
			 TY_IDX       object_type, /* in */
			 STAB_OFFSET *object_ofst) /* in out*/
{
   /* Get a consecutive sequence of initializers for a consecutive
    * sequence of array elements.  Note that object_ofst will be
    * set to the offset of the initv element following the array
    * from the base of the array.  Initv_idx and initv_times will
    * be updated to point to the initv immediately following the
    * array segment.
    */
   const UINT    first_already_repeated = *initv_times;
   STAB_OFFSET   max_ofst;
   ARRAY_SEGMENT aseg;
   INITV_IDX     initv;

   /* Get the immediately available information */
   aseg.initv_array = initv_array;
   aseg.num_initvs = 0;            /* To be calculated */
   aseg.first_idx = *initv_idx;
   aseg.last_idx = aseg.first_idx; /* To be calculated */
   aseg.start_ofst = *object_ofst;
   aseg.atype = object_type;
   aseg.etype = TY_AR_etype(object_type);
   
   /* Walk though the initializers until we reach the last initv
    * belonging to this array segment.  I.e. the in/out parameters
    * will be updated to refer to the initializer immediately
    * following this array segment, while "repeated" and "idx"
    * denote the last initv belonging to this segment.
    */
   initv = initv_array[aseg.first_idx];
   max_ofst = TY_size(object_type);
   while (max_ofst > *object_ofst &&
	  initv != (INITV_IDX) 0 
          && INITV_kind(Initv_Table[initv]) != INITVKIND_PAD)
   {

      INITV& ini = Initv_Table[initv];
      aseg.num_initvs++;
      aseg.last_idx = *initv_idx;
      aseg.last_repeat = *initv_times+1;

      if (INIT2F_is_string_initv(ini,aseg.etype))
      {
	 /* Special case for F90 - it creates unsigned words for DATA */

         if (!WN2F_F90_pu) 
         {
		 ASSERT_DBG_WARN(FALSE, 
			 (DIAG_W2F_UNEXPECTED_INITV,
			  TCON_ty(INITV_tc_val(ini)),
			  "[character string exceeds size of element type] "
			  "INIT2F_Get_Array_Segment"));
         }
	 *object_ofst += Targ_String_Length(INITV_tc_val(ini));
      }
      else if (TY_is_character(Ty_Table[aseg.etype]) && 
	       TCON_ty(INITV_tc_val(ini)) == MTYPE_STRING)
      {
	 *object_ofst += Targ_String_Length(INITV_tc_val(ini));
      }
      else
	 *object_ofst += TY_size(aseg.etype);

      /* Get the next initv and advance the external idx and times to refer
       * to this next initv.
       */
      INIT2F_Next_Initv(ini, initv_idx, initv_times);
      initv = initv_array[*initv_idx];
   }

   if (max_ofst > *object_ofst && initv == (INITV_IDX) 0)
   {
      aseg.missing_padding = TRUE;
      ASSERT_DBG_WARN(FALSE, 
		      (DIAG_W2F_UNEXPEXTED_NULL_PTR, 
		       "initv (missing padding for object initializer?)",
		       "INIT2F_Get_Array_Segment"));
   }
   else
      aseg.missing_padding = FALSE;

   /* Wrap up the array-segment attributes by getting the offset to
    * the initv immediately following the segment and the repeat 
    * factors on the first and last initv in the segment (the other 
    * initvs being repeated to their full extent).
    */
   aseg.end_ofst = *object_ofst;
   if (aseg.last_idx > aseg.first_idx)
   {
      aseg.first_repeat = 
	 INIT2F_choose_repeat(Initv_Table[initv_array[aseg.first_idx]]) - first_already_repeated;
   }
   else /* aseg.last_idx == aseg.first_idx */
   {
      aseg.first_repeat = aseg.last_repeat - first_already_repeated;
      aseg.last_repeat = aseg.first_repeat;
   }

   return aseg;
} /* INIT2F_Get_Array_Segment */

static void
INIT2F_Translate_Array_Value(TOKEN_BUFFER         tokens,
			     const ARRAY_SEGMENT *aseg)
{
   UINT   initv_idx, repeat;
   INITV_IDX initv;

   for (initv_idx = aseg->first_idx; initv_idx <= aseg->last_idx; initv_idx++)
   {
      /* Get the initv and the repeat factor */
      initv = aseg->initv_array[initv_idx];
      if (initv_idx == aseg->first_idx)
	 repeat = aseg->first_repeat;
      else if (initv_idx == aseg->last_idx)
	 repeat = aseg->last_repeat;
      else
	 repeat = INIT2F_choose_repeat(Initv_Table[initv]);

      /* Do the initialization */
      INITVKIND_translate(tokens, initv, aseg->etype, repeat);
   } /* for */
} /* INIT2F_Translate_Array_Value */

static void
INIT2F_Implied_DoLoop(TOKEN_BUFFER  tokens,        /* Append to this buffer */
		      TOKEN_BUFFER *abase_tokens,  /* Array-base reference */
		      const ARRAY_SEGMENT *aseg)   /* Array segment info */
{
   /* Use an implied do-loop to initialize array elements from
    * index "aseg->start_ofst/TY_size(aseg->etype)" to index
    * "aseg->end_ofst/TY_size(aseg->etype)", where the difference
    * between these indices should be exactly "aseg->num_initvs-1".
    *
    * We assume all arrays have been normalized to be stride 1 arrays,
    * although, if necessary, we can easily modify this later to 
    * handle larger strides (TODO?).  Also, it may be worthwhile to
    * extend this to handle initialization of an array of substrings.
    * Currently, we only handle initialization of an array of complete
    * strings by means of an implied do-loop (TODO?).
    */
   const UINT   current_indent = Current_Indentation();
   TOKEN_BUFFER aref_tokens;
   UINT         ivar_idx, avar_idx;
   const char  *ivar_name;
   TY_IDX       atype;

   /* Declare the induction variable */
   ivar_idx = Stab_Lock_Tmpvar(Stab_Mtype_To_Ty(MTYPE_I8),
			       &ST2F_Declare_Tempvar);

   /* Put the array reference tokens in aref_tokens */
   aref_tokens = New_Token_Buffer();
   if (TY_AR_ndims(aseg->atype) > 1)
   {
      /* The implied do-loop only operates over a one-dimensional array,
       * so use an equivalence if the array is not one-dimensional.
       */
      atype = Stab_Array_Of(aseg->etype, 
			    TY_size(aseg->atype)/TY_size(aseg->etype));
      avar_idx = Stab_Lock_Tmpvar(atype, &ST2F_Declare_Tempvar);
      Set_Current_Indentation(PUinfo_local_decls_indent);
      INIT2F_Prepend_Equivalence(Data_Stmt_Tokens, *abase_tokens, avar_idx);
      Reclaim_Token_Buffer(abase_tokens);
      Set_Current_Indentation(current_indent);

      Append_Token_String(aref_tokens, W2CF_Symtab_Nameof_Tempvar(avar_idx));
      Stab_Unlock_Tmpvar(avar_idx);
   }
   else
   {
      Append_And_Reclaim_Token_List(aref_tokens, abase_tokens); 
   }
   
   /* Generate the implied do-loop */
   ivar_name = W2CF_Symtab_Nameof_Tempvar(ivar_idx);
   Append_Token_Special(tokens, '(');
   Append_And_Reclaim_Token_List(tokens, &aref_tokens);
   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, ivar_name);
   Append_Token_Special(tokens, ')');

   Append_Token_Special(tokens, ',');
   Append_Token_String(tokens, ivar_name);
   Append_Token_Special(tokens, '=');
   Append_Token_String(tokens, 
	    Number_as_String(aseg->start_ofst/TY_size(aseg->etype) + 1,
			     "%llu"));
   Append_Token_Special(tokens, ',');
   Append_Token_String(tokens, 
		       Number_as_String(aseg->end_ofst/TY_size(aseg->etype),
					"%llu"));
   Append_Token_Special(tokens, ',');
   Append_Token_String(tokens, Number_as_String(1, "%llu"));
   Append_Token_Special(tokens, ')');

   Stab_Unlock_Tmpvar(ivar_idx);
} /* INIT2F_Implied_DoLoop */

static void
INIT2F_Translate_Array_Ref(TOKEN_BUFFER         tokens, 
			   ST                  *base_object,
			   STAB_OFFSET          base_ofst,
			   const ARRAY_SEGMENT *aseg)
{
   /* The greatest complication here arises when the array element type
    * is a character string, since for this case the aseg->num_initvs
    * indicates the number of INITVs in the segment, not the number of 
    * array elements that are initialized, and the first and/or last
    * array element may be substring initializations.  We handle
    * such cases specially.
    */
   const STAB_OFFSET esize = TY_size(aseg->etype);
   STAB_OFFSET       ofst; /* Current offset when traversing array segment */
   WN2F_CONTEXT      context = INIT_WN2F_CONTEXT;
   TOKEN_BUFFER      abase_tokens, aref_tokens;
   UINT              first_idx = aseg->first_idx;
   INITV_IDX         first_initv = aseg->initv_array[first_idx];


   if (aseg->num_initvs == 1 &&
       INIT2F_is_string_initv(Initv_Table[first_initv],aseg->etype))
   {
      /* Use an implied do-loop to do this special F90 initialization */

      abase_tokens = New_Token_Buffer();
      WN2F_Offset_Symref(abase_tokens,
			 base_object,
			 Stab_Pointer_To(ST_type(base_object)),
			 aseg->atype,
			 base_ofst,
			 context);

      aref_tokens = New_Token_Buffer();
      INIT2F_Implied_DoLoop(aref_tokens,  /* Append loop to this buffer */
			    &abase_tokens,/* Array-base reference tokens */
			    aseg);        /* Array segment information */
      INIT2F_Append_Initializer(tokens, &aref_tokens, 1);
   }
   else if (aseg->start_ofst % TY_size(aseg->etype) != 0 ||
	    aseg->end_ofst % TY_size(aseg->etype) != 0   ||
	    (!aseg->missing_padding &&
	     aseg->num_initvs != 
	     (aseg->end_ofst - aseg->start_ofst)/TY_size(aseg->etype)))
   {
      /* Special handling for substring initialization, where initv_repeat
       * accounts for how many times the current initv has already been
       * repeated.
       */
      UINT      initc, substring_size;
      UINT      initv_idx = first_idx;
      INITV_IDX ini_idx = first_initv;
      UINT      initv_repeat = INIT2F_choose_repeat(Initv_Table[ini_idx]) - aseg->first_repeat;

      ofst = aseg->start_ofst;
      for (initc = 1; initc <= aseg->num_initvs; initc++)
      {
	 INITV&    initv = Initv_Table[ini_idx];
	 substring_size = Targ_String_Length(INITV_tc_val(initv));
	 aref_tokens = New_Token_Buffer();
	 INIT2F_Translate_Char_Ref(aref_tokens,
				   base_object,
				   aseg->etype,        /* array element type */
				   base_ofst,          /* offset to array */
				   (ofst/esize)*esize, /* array element ofst */
				   ofst%esize,         /* string offset */
				   substring_size,     /* string size */
				   context);
	 INIT2F_Append_Initializer(tokens, &aref_tokens, 1);
	 if (initc < aseg->num_initvs) {
	   INIT2F_Next_Initv(initv, &initv_idx, &initv_repeat);
	   ini_idx = aseg->initv_array[initv_idx];
         }
	 ofst += substring_size;
      }
   }
   else /* Each initv corresponds to exactly one array element */
   {
      /* Translate the array base reference */
      abase_tokens = New_Token_Buffer();
      WN2F_Offset_Symref(abase_tokens,
			 base_object,
			 Stab_Pointer_To(ST_type(base_object)),
			 aseg->atype,
			 base_ofst,
			 context);

      /* Append indexing expression.
       */
      if (aseg->num_initvs*TY_size(aseg->etype) == TY_size(aseg->atype))
      {
	 /* The whole array is initialized, so nothing else need be done */
	 INIT2F_Append_Initializer(tokens, &abase_tokens, 1);
      }
      else if (aseg->num_initvs > 4)
      {
	 /* Use an implied do-loop to do the initialization */
	 aref_tokens = New_Token_Buffer();
	 INIT2F_Implied_DoLoop(aref_tokens,  /* Append loop to this buffer */
			       &abase_tokens,/* Array-base reference tokens */
			       aseg);        /* Array segment information */
	 INIT2F_Append_Initializer(tokens, &aref_tokens, 1);
      }
      else if (aseg->num_initvs > 0)
      {
	 INT elt;

	 /* Refer to each array element separately */
	 ofst = aseg->start_ofst;
	 for (elt = 0; elt < aseg->num_initvs; elt++)
	 {
	    aref_tokens = New_Token_Buffer();
	    Append_And_Copy_Token_List(aref_tokens, abase_tokens);
	    TY2F_Translate_ArrayElt(aref_tokens, aseg->atype, ofst);
	    INIT2F_Append_Initializer(tokens, &aref_tokens, 1);
	    ofst += TY_size(aseg->etype);
	 }
	 Reclaim_Token_Buffer(&abase_tokens);
      }
   }
} /* INIT2F_Translate_Array_Ref */

/*--------- Routines to handle initialization for various types --------*
 *----------------------------------------------------------------------*/

static void
INIT2F_translate(TOKEN_BUFFER lhs_tokens,
		 TOKEN_BUFFER rhs_tokens,
		 ST          *base_object, /* Top level object */
		 STAB_OFFSET  base_ofst,   /* Offset from top level base */
		 STAB_OFFSET *object_ofst, /* Offset within object type */
		 TY_IDX       object_ty,   /* Sub-object type at base_ofst */
		 INITV_IDX   *initv_array, /* The initv array */
		 UINT        *initv_idx,   /* next initv for sub-object */
		 UINT        *initv_times); /* times initv already repeated */

static void
INIT2F_ptr_or_scalar(TOKEN_BUFFER lhs_tokens,
		     TOKEN_BUFFER rhs_tokens,
		     ST          *base_object,
		     STAB_OFFSET  base_ofst,
		     STAB_OFFSET *object_ofst,
		     TY_IDX       object_ty,
		     INITV_IDX   *initv_array,
		     UINT        *initv_idx,
		     UINT        *initv_times)
{
   /* Initialization of a pointer or a scalar object, which means
    * the INITV must be INITVKIND_SYMOFF or INITVKIND_VAL (not
    * INITVKIND_PAD or INITVKIND_block).
    */
   INITV&       initv = Initv_Table[initv_array[*initv_idx]];
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;
   TOKEN_BUFFER sym_tokens;
   
   ASSERT_DBG_WARN(*object_ofst == 0, 
		   (DIAG_W2F_UNEXPEXTED_OFFSET, 
		    *object_ofst, "INITV2F_ptr_or_scalar"));


   INITVKIND_translate(rhs_tokens, 
		       initv_array[*initv_idx],
		       object_ty,
                       1) ;

   INIT2F_Next_Initv(initv, initv_idx, initv_times);

   /* Get the lhs of the initializer */
   sym_tokens = New_Token_Buffer();
   WN2F_Offset_Symref(sym_tokens,
		      base_object,
		      Stab_Pointer_To(ST_type(base_object)),
		      object_ty,
		      base_ofst,
		      context);
   INIT2F_Append_Initializer(lhs_tokens, &sym_tokens, 1);

   /* object_ofst denotes the offset from the base of this object */
   *object_ofst += TY_size(object_ty);

} /* INIT2F_ptr_or_scalar */


static void
INIT2F_array(TOKEN_BUFFER lhs_tokens,
	     TOKEN_BUFFER rhs_tokens,
	     ST          *base_object,
	     STAB_OFFSET  base_ofst,
	     STAB_OFFSET *object_ofst,
	     TY_IDX       object_ty,
	     INITV_IDX   *initv_array,
	     UINT        *initv_idx,
	     UINT        *initv_times)
{
   /* Initialization of an array, which is not a character string.
    * We have several choices as to how to do the initialization,
    * where options are (in order of preference) initialization of
    * the whole array, an implied do-loop initialization, or
    * initialization of individual array elements.
    */

   ARRAY_SEGMENT a_segment;

   ASSERT_DBG_FATAL(TY_Is_Array(object_ty) && !TY_is_character(object_ty),
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(object_ty), "INITV2F_array"));

   INIT2F_Skip_Padding(initv_array, 
		       object_ty, 
		       object_ofst, 
		       initv_idx);
   while (*object_ofst < TY_size(object_ty))
   {
      /* Translate each non-padding initializer segment into a sub-array
       * initialization.
       */

      INITV&  initv = Initv_Table[initv_array[*initv_idx]];

      /* Get the last consecutive initv and the array segment-size
       * implied by this consecutive sequence of initializers.
       */
      a_segment = 
	 INIT2F_Get_Array_Segment(initv_array, 
				  initv_idx, 
				  initv_times, 
				  object_ty, 
				  object_ofst);

      /* Translate the rhs, i.e. the array-elements of this segment.
       */
      INIT2F_Translate_Array_Value(rhs_tokens, &a_segment);
      
      /* Translate the lhs, i.e. the array segment being initialized.
       */
      INIT2F_Translate_Array_Ref(lhs_tokens, 
				 base_object,
				 base_ofst,
				 &a_segment);

      /* Skip padding before initializing remaining array segments.
       */
      INIT2F_Skip_Padding(initv_array, 
			  object_ty, 
			  object_ofst, 
			  initv_idx);

      /* object_ofst denotes the offset from the base of 
       * this object
       */
   } /* while */

} /* INIT2F_array */

static void
INIT2F_substring(TOKEN_BUFFER lhs_tokens,
		 TOKEN_BUFFER rhs_tokens,
		 ST          *base_object,
		 STAB_OFFSET  base_ofst,
		 STAB_OFFSET *object_ofst,
		 TY_IDX       object_ty,
		 INITV_IDX   *initv_array,
		 UINT        *initv_idx,
		 UINT        *initv_times)
{
   /* Initialization of an array, which is a character string.
    * We have a couple of choices as to how to do the initialization,
    * where options are (in order of preference) initialization of
    * the whole string, or initialization of a substring.
    */
   STAB_OFFSET  substring_size;
   TOKEN_BUFFER substring_tokens;
   WN2F_CONTEXT context = INIT_WN2F_CONTEXT;

   ASSERT_DBG_FATAL((TY_Is_String(object_ty) || 
		     TY_Is_Array_Of_Chars(object_ty)),
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(object_ty), "INITV2F_substring"));

   INIT2F_Skip_Padding(initv_array, 
		       object_ty,
		       object_ofst,
		       initv_idx);

   if (*object_ofst < TY_size(object_ty))
   {
      /* Append the substring value to the rhs */

      INITV_IDX initv = initv_array[*initv_idx];
      INITV&      ini = Initv_Table[initv];

      INITVKIND_translate(rhs_tokens, initv, object_ty, 1);

      /* Append the substring reference to the lhs */

      substring_size = Targ_String_Length(INITV_tc_val(ini));
      substring_tokens = New_Token_Buffer();
      INIT2F_Translate_Char_Ref(substring_tokens,
				base_object,
				object_ty,        /* character string type */
				base_ofst,        /* offset to array */
				0,                /* array element ofst */
				*object_ofst,     /* string offset */
				substring_size,  /* string size */
				context);
      INIT2F_Append_Initializer(lhs_tokens, &substring_tokens, 1);
      INIT2F_Next_Initv(ini, initv_idx, initv_times);
      *object_ofst += substring_size;
   } /* if */
} /* INIT2F_substring */

static void
INIT2F_structured(TOKEN_BUFFER lhs_tokens,
		  TOKEN_BUFFER rhs_tokens,
		  ST          *base_object,
		  STAB_OFFSET *object_ofst,
		  TY_IDX       object_ty,
		  INITV_IDX   *initv_array,
		  UINT        *initv_idx,
		  UINT        *initv_times)
{
   /* Initialization of a structure or a member of a structure.  The
    * kind of structure may be a common, equivalence, or a RECORD
    * block.  The initializer will be a sequence of INITVKIND_SYMOFFs,
    * INITVKIND_VALs and INITVKIND_PADs.
    */
   TY_IDX         initv_ty;
   STAB_OFFSET    fld_ofst;
   FLD_PATH_INFO *fpath;
   
   ASSERT_DBG_FATAL(TY_Is_Structured(object_ty),
		    (DIAG_W2F_UNEXPECTED_TYPE_KIND, 
		     TY_kind(object_ty), "INITV2F_structured"));
   
   /* Find the initializer for each field that has one, first skipping
    * past any padding.
    */
   INIT2F_Skip_Padding(initv_array, object_ty, object_ofst, initv_idx);
   while (*object_ofst < TY_size(object_ty))
   {   
      /* Determine what type of initializer we have */ 
      initv_ty = INITVKIND_ty(initv_array[*initv_idx]);

      /* Find the field that best matches this type.  This will be done
       * at each level of path down nested structures and as such will be
       * extremely inefficient, but we do not expect more than one level
       * of nesting for Fortran initializers (Fortran RECORDs may not
       * occur in DATA statements).
       */
      fpath = TY2F_Get_Fld_Path(object_ty, initv_ty, *object_ofst);
      {
	FLD_HANDLE fld;

	if (fpath == NULL || fpath->fld.Is_Null ())
	{
	    /* Could not find a suitable path so just assume the first field
	     * that may contain the value.
	     */

	    FLD_ITER fld_iter = Make_fld_iter (TY_fld(Ty_Table[object_ty]));

	    do 
	      {
		fld = FLD_HANDLE (fld_iter);
	      } while (!FLD_last_field (fld_iter++) && 
		       !OFFSET_IS_IN_FLD(fld, *object_ofst)) ;
	} else
	  fld = fpath->fld; 

	if (fpath != NULL)
	  TY2F_Free_Fld_Path(fpath);
	
	/* Translate the initialization of this field:  We rely on only
         * one level fields here, so the offset within the found field
	 * will be the total of [offset - FLD_ofst(fld)].
	 */
	fld_ofst = *object_ofst - FLD_ofst(fld);
	INIT2F_translate(lhs_tokens,
			 rhs_tokens,
			 base_object,
			 FLD_ofst(fld),
			 &fld_ofst,     /* return ofst from base of field */
			 FLD_type(fld),
			 initv_array,
			 initv_idx,
			 initv_times);
	
	/* Skip padding before initializing remainding fields.
	  */
	*object_ofst = FLD_ofst(fld) + fld_ofst;
	INIT2F_Skip_Padding(initv_array, 
			    object_ty, 
			    object_ofst, 
			    initv_idx);
      }
   } /* while */
} /* INIT2F_structured */

static void
INIT2F_translate(TOKEN_BUFFER lhs_tokens,
		 TOKEN_BUFFER rhs_tokens,
		 ST          *base_object, /* Top level base-object */
		 STAB_OFFSET  base_ofst,   /* Offset from top level base */
		 STAB_OFFSET *object_ofst, /* Offset from base_member */
		 TY_IDX       object_ty,   /* Base_member type at base_ofst */
		 INITV_IDX   *initv_array, /* The initv array */
		 UINT        *initv_idx,   /* next initv for sub-object */
		 UINT        *initv_times) /* times initv already repeated */
{
   if (TY_Is_Structured(object_ty))
   {
      INIT2F_structured(lhs_tokens,
			rhs_tokens,
			base_object,
			object_ofst,
			object_ty,
			initv_array,
			initv_idx,
			initv_times);
   }
   else if (TY_Is_Array(object_ty))
   {
      if (TY_is_character(Ty_Table[object_ty]))

	 INIT2F_substring(lhs_tokens,
			  rhs_tokens,
			  base_object,
			  base_ofst,
			  object_ofst,
			  object_ty,
			  initv_array,
			  initv_idx,
			  initv_times);
      else
	 INIT2F_array(lhs_tokens,
		      rhs_tokens,
		      base_object,
		      base_ofst,
		      object_ofst,
		      object_ty,
		      initv_array,
		      initv_idx,
		      initv_times);
   }
   else if (TY_Is_Pointer_Or_Scalar(object_ty))
   {
      INIT2F_ptr_or_scalar(lhs_tokens,
			   rhs_tokens,
			   base_object,
			   base_ofst,
			   object_ofst,
			   object_ty,
			   initv_array,
			   initv_idx,
			   initv_times);
   }
   else
      ASSERT_DBG_WARN(FALSE, 
		      (DIAG_W2F_UNEXPECTED_SYMBOL, "INITV2F_translate"));
} /* INIT2F_translate */


/*------------------------- Exported Routines --------------------------*/
/*----------------------------------------------------------------------*/

void
INITO2F_translate(TOKEN_BUFFER tokens, INITO_IDX inito)
{
   /* Create a DATA statement, followed by a newline character,
    * provided the object initialized is not a RECORD type (for
    * which the initializer should be noted on the type, not on
    * the object).
    */
   TOKEN_BUFFER lhs_tokens = New_Token_Buffer(); /* memloc initialized */
   TOKEN_BUFFER rhs_tokens = New_Token_Buffer(); /* initializer values */
   UINT         initv_idx = 0;
   UINT         initv_times = 0;
   TY_IDX       object_ty = ST_type(INITO_st(inito));
   STAB_OFFSET  object_ofst = 0;
   INITV_IDX    *initv_array;

   ASSERT_DBG_FATAL(!TY_Is_Structured(object_ty)          ||
		    Stab_Is_Common_Block(INITO_st(inito)) ||
		    Stab_Is_Equivalence_Block(INITO_st(inito)),
		    (DIAG_W2F_UNEXPECTED_SYMBOL, "INITO2F_translate"));
   
   /* There may be a list of INITO's initializing the same object, so
    * accumulate the INITV's immediately under this list of INITOs into
    * a single array of INITV's to aid the following computation.  All
    * INITVKIND_BLOCK initvs will have been flattened out, so we only
    * have INITVKIND_VAL, INITVKIND_SYMOFF, and INITVKIND_PAD in this
    * array.
    */
   initv_array = INIT2F_Get_Initv_Array(INITO_st(inito), inito);
   
   /* Activate an initialization based on the kind of object to be
    * initialized.  We expect the INITO list for this object to cover
    * the entire extent of the object.
    */
   INIT2F_translate(lhs_tokens,
		    rhs_tokens,
		    INITO_st(inito), /* Top level object */
		    0,               /* Offset from top level base */
		    &object_ofst,    /* Offset within object type */
		    object_ty,       /* Sub-object type at base-offset */
		    initv_array,     /* The initv array */
		    &initv_idx,      /* first initv for sub-object */
		    &initv_times);   /* times initv already repeated */

   /* Combine the lhs and the rhs and free up the initv array.
    */
   FREE(initv_array);
   Append_F77_Indented_Newline(tokens, 1, NULL/*label*/);
   Append_Token_String(tokens, "DATA");
   Append_And_Reclaim_Token_List(tokens, &lhs_tokens);
   Append_Token_Special(tokens, '/');
   Append_And_Reclaim_Token_List(tokens, &rhs_tokens);
   Append_Token_Special(tokens, '/');
} /* INITO2F_translate */
