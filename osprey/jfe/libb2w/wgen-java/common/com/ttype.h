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


#ifndef ttype_INCLUDED
#define ttype_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
/* ====================================================================
 * ====================================================================
 *
 * Module: ttype.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/ttype.h,v $
 *
 * Revision history:
 *  26-May-89 - Original Version of header.  Added content.
 *  12-Jun-91 - Integrated from Josie
 *
 * Description:
 *
 * Routines to map tree nodes to the types of the objects represented.
 *
 * ====================================================================
 * ====================================================================
 */


#include "srcpos.h"	/* get definition of SRCPOS */

/* Dummy declarations of structs for prototypes: */
class WN;

#ifdef FRONT_END
/* Return a character string representation of the input TY */
extern char *FE_Type_Name(TY *ty);
#endif /* FRONT_END */

/* return the ty for a whirl expression */
extern TY_IDX TY_Of_Expr (const WN *expr);

/* return the ty for a whirl expression accounting for SCLASS_FORMAL_REF */
extern TY_IDX TY_Of_Parameter(WN *expr);

/* Determine whether the given type is floating point.  For this
 * purpose, the Fortran complex and double complex types are considered
 * floating point.
 */
extern BOOL Is_Float_Type (TY_IDX t);

/* ====================================================================
 *       QUALIFIED TYPES: 
 *
 * The TY record has two qualifying flags TY_is_volatile and
 * TY_is_const.
 *
 * The features to note are:
 *
 *  - The qualified versions of TY records have TY_is_volatile or 
 *    TY_is_const flags on. 
 *
 *  - The qualified and non qualified versions of same types share 
 *    TY_flist, TY_arinfo and enum_const lists. sharing of field list 
 *    (TY_flist) is important to be able to tell when two struct kind 
 *    TY records are same except for volatile or const bits. eg:
 *	 struct a { int i,j; }; volatile struct a a_v;
 *    will have two TYs of KIND_STRUCT. They will share FLDs, i.e., 
 *    their TY_flist will point to smae FLD. They will have same TY_name 
 *    (same pointer). TY_size and TY_align will be same. Only differences 
 *    will be in TY_is_volatile bit, TY_id, TY_tynum (used by mtob) and 
 *    TY_next (used for chaining TYs).
 *
 *  - Because More than one TYs may share TY_flist, mtob and btom have 
 *    been changed to maintain this sharing across .B file interface.
 *    Look at comment in mtob.c for important restrictions on how TY 
 *    records must be numbered.
 *
 *    The way Steve makes type records and the way I tran them, guarantees 
 *    that the non qualified versions of qualified TYs will always exist, 
 *    even if they are not used. However, at present there is no easy way 
 *    of finding the corresponding non-qualified versions of qualified 
 *    types or finding any qualified version of a non-qualified type. 
 *    In fact, given 'volatile int p; volatile int q;' Steve makes 
 *    two a_type records, one for p and one for q. I may do same. 
 *    Compilation of programs having const or volatile may get hit by 
 *    the cost of a search.
 *
 *    It is even more important to not compare TY pointers directly. 
 *
 *    Coffsdb is going to ignore all TY records having TY_is_volatile 
 *    or TY_is_const bit set. i.e., it is not going to dump them to .s 
 *    file. Since the TY_name of a qulified TY is same as the TY_name 
 *    of corresponding non-qualified TY, the assembler will make links 
 *    to the non qualified type.  Mtob will need to find 
 *    corresponding-non-qualified TYs for purpose of ordering TYs; for 
 *    now we do it the long way: searching through all exisiting types.
 *
 * ====================================================================
 */

/* Given a qualified type, find a corresponding unqualified type: */
extern TY *Find_Unqualified_Type ( TY * );

/* Check whether two types are equivalent (similar). Two types are
 * equivalent (similar) if they are semantically identical (have the
 * same BE representation).  In particular:
 *  Two scalar types are equivalent (similar) if their btypes and sizes
 *	are the same. 
 *  Two pointer types are similar (equivalent) if their btypes are the
 *	same (and they point to equivalent types). 
 *  A scalar type and a pointer type with the same btype are treated as
 *	similar.
 *  Two array types are equivalent if their bounds are the same and 
 *	their elements are equivalent. 
 *  Two function types are equivalent if return equivalent types.
 *  Two struct or class types are equivalent only if their field lists
 *	are same-pointer, i.e. if TY_clist of the two are the identical
 *	pointers.
 *  Array, function, struct, and class types are never treated as
 *  similar.
 *
 * Additional qualifiers must match (or don't matter) depending on the
 * QUAL_CHECK parameter.  The options are:
 *	QUAL_IGNORE - no qualifiers (volatile, const, restrict,
 *		      is_character & is_logical) matter.
 *	QUAL_CONSIDER - only the volatile, const, restrict qualifiers
 *			are tested and must be the same for the types
 *			to match.
 *	QUAL_FULL - full front-end checking of all qualifiers is done
 *		    and must match for the types to be equivalent.
 *
 */
typedef enum {
  QUAL_IGNORE	= 1,
  QUAL_CONSIDER	= 2,
  QUAL_FULL     = 3
} QUAL_CHECK;
extern BOOL Equivalent_Types ( TY_IDX, TY_IDX, QUAL_CHECK );
extern BOOL Similar_BE_Types ( TY_IDX, TY_IDX );

/* records to keep track of typedef names for debug info */
typedef struct tpdef {
  char *name; 
  TY   *type;
  struct tpdef *next;
  BOOL is_dumped;
} TPDEF;

#define TPDEF_name(t)  ((t)->name)
#define TPDEF_type(t)  ((t)->type)
#define TPDEF_next(t)  ((t)->next)
#define TPDEF_is_dumped(t)  ((t)->is_dumped)

extern TPDEF *Global_Tpdefs, *Local_Tpdefs;

#define FOR_ALL_GLOBAL_TPDEFS(tp) \
	for ( tp = Global_Tpdefs; tp; tp = TPDEF_next(tp) )

#ifdef __cplusplus
}
#endif
#endif /* ttype_INCLUDED */
