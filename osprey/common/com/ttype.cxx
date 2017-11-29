/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: ttype.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/ttype.cxx,v $
 *
 * Revision history:
 *  26-May-89 - Original version of header.
 *  12-Jun-91 - Integrated from Josie
 *
 * Description:
 *
 * Map tree nodes to the types of the objects represented.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "config.h"
#include "erglob.h"

#include "strtab.h"
#include "stab.h"
#include "opcode.h"
#include "targ_const.h"
#include "const.h"
#include "wn_core.h"
#include "ttype.h"

#ifdef FRONT_F90
#include "wn.h"
#include "wn_simp.h"
#endif

TPDEF *Global_Tpdefs = NULL;

#ifdef FRONT_END_FORTRAN

TY *Fe_Type_Tbl_[FETYPE_LAST+1];

/* ====================================================================
 *
 * FE_Type_Name
 *
 * Return a printable (character string) representation of
 * the FETYPE number of input TY.
 *
 * ====================================================================
 */

char *
FE_Type_Name ( TY *ty )
{
  if ( ty == NULL )  return "<null TY>";
  if ( TY_kind(ty) != KIND_SCALAR )  return Kind_Name(TY_kind(ty));

  switch ( TY_fe_btype(ty) ) {
    case FETYPE_BAD:	return "FETYPE_BAD";
    case FETYPE_UNK:	return "FETYPE_UNK";
    case FETYPE_NONE:	return "FETYPE_NONE";
    case FETYPE_L1:	return "FETYPE_L1";
    case FETYPE_L2:	return "FETYPE_L2";
    case FETYPE_L4:	return "FETYPE_L4";
    case FETYPE_L8:	return "FETYPE_L8";
    case FETYPE_I1:	return "FETYPE_I1";
    case FETYPE_I2:	return "FETYPE_I2";
    case FETYPE_I4:	return "FETYPE_I4";
    case FETYPE_I8:	return "FETYPE_I8";
    case FETYPE_R4:	return "FETYPE_R4";
    case FETYPE_R8:	return "FETYPE_R8";
    case FETYPE_R16:	return "FETYPE_R16";
    case FETYPE_C8:	return "FETYPE_C8";
    case FETYPE_C16:	return "FETYPE_C16";
    case FETYPE_C32:	return "FETYPE_C32";
    case FETYPE_CH:	return "FETYPE_CH";
  }

  return "<unknown scalar TY>";
}
#endif /* FRONT_END_FORTRAN */


/* ====================================================================
 *
 * TY *TY_Of_Expr (WN *expr)
 *
 * Return the ty for a whirl expression 
 *
 * ====================================================================
 */

TY_IDX
TY_Of_Expr (const WN *expr)
{
    TY_IDX type;

    switch (WN_operator(expr)) {
    case OPR_PARM:
	type = WN_ty(expr);
	break;
    case OPR_IDNAME:
	type = WN_type(expr);
	break;
    case OPR_MLOAD:
	type = TY_pointed (Ty_Table[WN_ty (expr)]);
	break;
    default:
	type = MTYPE_To_TY(WN_rtype(expr));
	break;
    }

    TYPE_ID mtype = TY_mtype (type);

    if (MTYPE_is_complex (mtype)) 
	return MTYPE_To_TY (mtype);
    
    return type;
}

/* ====================================================================
 *
 * TY_Of_Parameter (WN *expr)
 *
 * Return the ty for a whirl expression , accounting for the 
 * SCLASS_FORMAL_REF lie (in the context in which this routine is called
 * the formal ref will eventually be dereferenced)
 *
 * ====================================================================
 */
TY_IDX
TY_Of_Parameter (WN *expr)
{
    TY_IDX type;

    type = TY_Of_Expr (expr);

    if ((WN_has_sym(expr))) {
	if (WN_sclass(expr) == SCLASS_FORMAL_REF)
	    return Make_Pointer_Type(type);
    }

    return type;
}


/* ====================================================================
 *
 * Is_Float_Type / Is_Float_Node
 *
 * Determine whether the given type (the type of the object represented
 * by the given tree node) is a floating point type.  For this purpose,
 * the Fortran complex types are considered floating point.
 *
 * ====================================================================
 */

BOOL
Is_Float_Type ( TY_IDX ty )
{
    TYPE_ID tid;

    /* Determine whether it's floating point: */
    switch (TY_kind (ty)) {
    case KIND_SCALAR:
	tid = TY_mtype (ty);
	if (tid > 0 && tid <= MTYPE_LAST)
	    return MTYPE_float(tid);
	break;

    }
    return FALSE;
}


#ifndef MONGOOSE_BE
/* ====================================================================
 *
 * Similar_BE_Types
 *
 * Routine to compare two types. This routine should be called to
 * determine whether two TY records are equivalent from a back end
 * implementation point of view, i.e. whether they have the same bit
 * representation (TY_mtype).  It ignores diffences of volatile and
 * const qualifiers. 
 *
 * WARNING:  This routine is currently used to decide whether a
 * field/member is of an appropriate type for use in struct/class
 * decomposition for parameter passing in registers.  It is therefore
 * irrelevant how it behaves except for scalar and pointer types.  If
 * other users are identified, those cases can be modified as
 * appropriate for the new users.
 *
 * ====================================================================
 */

BOOL
Similar_BE_Types ( TY_IDX t1_idx, TY_IDX t2_idx )
{
  /* Do a quick check for identical types: */
  if ( t1_idx == t2_idx )
    return TRUE;

  TY& t1 = Ty_Table [t1_idx];
  TY& t2 = Ty_Table [t2_idx];

  /* Insist on valid kinds: */
  if ( TY_kind(t1) == 0 || TY_kind(t2) == 0 )
    return FALSE;
  
  switch (TY_kind(t1)) {

    case KIND_SCALAR:
    case KIND_POINTER:
      return TY_mtype(t1) == TY_mtype(t2) &&
	     TY_size(t1) == TY_size(t2);

    default:
      return FALSE;
  }
}
#endif /* MONGOOSE_BE */

/* ====================================================================
 *
 * Equivalent_Types
 *
 * Routine to compare two types. This routine should be called to
 * determine whether two TY records are equivalent.
 * 
 * There are several cases where comparing TY pointers directly
 * can yield FALSE even though the types are the same for purposes
 * of assignment or comparison.  Using Equivalent_Types to compare
 * two types deals with all these cases.
 *
 *   1) If two types have the same machine representation (be type)
 *	except that one is volatile, const or restrict qualified and
 *	the other is not.  Such differences between types should
 *	usually be ignored by the back-end.  (QUAL_IGNORE)
 *
 *   2) If two types have the same machine representation (be type)
 *	but differ in the volatile, const and/or restrict qualifiers
 *	AND this difference is significant to the back-end.
 *	(QUAL_CONSIDER)
 *
 *   3) To the front-ends, there are more qualifiers which are important
 *	to determining type equivalency.  Only if these and the volatile,
 *	const and restrict qualifiers are identical can the two types be
 *	considered equivalent.  (QUAL_FULL)
 *
 * ====================================================================
 */


BOOL
Equivalent_Types (TY_IDX t1, TY_IDX t2, QUAL_CHECK consider_qualifiers)
{

    /* Quick check for identical types: */
    if ( t1 == t2 )
	return TRUE;

    const TY& ty1 = Ty_Table[t1];
    const TY& ty2 = Ty_Table[t2];

    /* Insist on identical, valid, TY_kinds: */
    if (TY_kind (ty1) != TY_kind (ty2) || TY_kind (ty1) == KIND_INVALID)
	return FALSE;
    
    BOOL match_q =
	(consider_qualifiers == QUAL_IGNORE ||
	 ((TY_is_volatile (t1) == TY_is_volatile (t2) &&
	   TY_is_const (t1) == TY_is_const (t2) &&
	   TY_is_restrict (t1) == TY_is_restrict (t2)) &&
	  (consider_qualifiers != QUAL_FULL ||
	   (TY_align_exp (t1) == TY_align_exp (t2) &&
	    TY_is_character(ty1)==TY_is_character(ty2) &&
	    TY_is_logical(ty1)==TY_is_logical(ty2)))));

    switch ( TY_kind (ty1)) {

    case KIND_VOID:
	return match_q;

    case KIND_SCALAR:
	return (TY_mtype (ty1) == TY_mtype (ty2) &&
		TY_size (ty1) == TY_size (ty2) &&
		match_q);
		
    case KIND_POINTER:
	return match_q && Equivalent_Types (TY_pointed (ty1),
					    TY_pointed (ty2),
					    consider_qualifiers);

    case KIND_FUNCTION:
	return match_q && Equivalent_Types (Tylist_Table[TY_tylist (ty1)],
					    Tylist_Table[TY_tylist (ty2)],
					    consider_qualifiers);

    case KIND_ARRAY:
         return (match_q && 
                 Equivalent_Types (TY_etype (ty1), TY_etype (ty2),
                                   consider_qualifiers) &&
                 ARB_are_equivalent(TY_arb(ty1), TY_arb(ty2)));

    case KIND_STRUCT:
#ifdef KEY
	match_q = (TY_return_in_mem (t1) == TY_return_in_mem (t2) &&
		   TY_copy_constructor (t1) == TY_copy_constructor (t2) &&
		   match_q);
#endif
	return TY_fld (ty1) == TY_fld (ty2) && match_q;

    default:
	ErrMsg ( EC_Invalid_Case, "Equivalent_Types", __LINE__ );
	return FALSE;  /* not needed but silences return w/o value warnings */
    }
} // Equivalent_Types


#ifndef MONGOOSE_BE
#endif /* MONGOOSE_BE */

