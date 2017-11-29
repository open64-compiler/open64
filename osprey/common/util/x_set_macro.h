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
 * Module: x_set_macro.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:56 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/x_set_macro.h,v $
 *
 * Revision history:
 *  05-05-93 - Original Version
 *
 * Description:
 *
 *      Template for macro interface to sets of objects.  This is
 *      processed by common/util/gen_x_set into a particular
 *      instantiation for a particular base type.  See below for a
 *      complete interface documentation.
 *
 * ====================================================================
 * ====================================================================
 */

BEGIN

#ifndef #uset#_INCLUDED
#define #uset#_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif


#include "bitset.h"	/* our clients do not need to know about it */

typedef BS #uset#;

BEGIN SUBUNIVERSES
class #uset#_SUBUNIVERSE;
END SUBUNIVERSES

#define #uset#_CHOOSE_FAILURE ((#base_type#)BS_CHOOSE_FAILURE)

/* This macro will maps BS_ELTs to their corresponding base_type elements,
 * allowing for the possibility that #num_elt# may not be defined on
 * BS_CHOOSE_FAILURE and maintaining the mapping from BS_CHOOSE_FAILURE to
 * #uset#_CHOOSE_FAILURE.   In other words:
 *
 *      #uset#_bs_elt_bas_type(x) ==>
 *              #uset#_CHOOSE_FAILURE if x == BS_CHOOSE_FAILURE
 *              #num_elt#(x)          otherwise
 *
 * The complexity arises from the possibility that x may be an expression,
 * instead of a simple variable.  In that case we can't evaluate it twice,
 * since it may have side effects or be expensive to evaluate.  So we'll
 * assign it to a file scoped static and select the result depending on
 * whether it's a choose failure.  Not so great, huh?
 *
 * TODO: Allow an optional straightforward implementation for mappings that
 * map BS_CHOOSE_FAILURE to #uset#_CHOOSE_FAILURE.
 */
inline #base_type#
#uset#_bs_elt_base_type( BS_ELT x )
{
  if ( x == BS_CHOOSE_FAILURE )
    return #uset#_CHOOSE_FAILURE;
  else
    return #num_elt#(x);
}


/* A subuniverse version as well. */
BEGIN SUBUNIVERSES
inline #base_type#
#uset#_bs_elt_base_type_sub( BS_ELT x, #uset#_SUBUNIVERSE* sub )
{
  if ( x == BS_CHOOSE_FAILURE )
    return #uset#_CHOOSE_FAILURE;
  else
    return #num_elt_sub#(x,sub);
}
END SUBUNIVERSES

#define #uset#_Create BS_Create
#define #uset#_Size_Alloc_Size BS_Size_Alloc_Size
#define #uset#_Alloc_Size BS_Alloc_Size
#define #uset#_ClearD BS_ClearD
#define #uset#_Create_Empty BS_Create_Empty
#define #uset#_Range BS_Range
#define #uset#_RangeD BS_RangeD

#define #uset#_Singleton(e,p)                                           \
    BS_Singleton(#elt_num#(e),(p))

BEGIN SUBUNIVERSES
#define #uset#_SingletonS(e,p,s)                                        \
    BS_Singleton(#elt_num_sub#((e),(s)),(p))
END SUBUNIVERSES

#define #uset#_SingletonD(s,e,p)                                        \
    BS_SingletonD((s),#elt_num#(e),(p))

BEGIN SUBUNIVERSES
#define #uset#_SingletonDS(s,e,p,sub)                                   \
    BS_SingletonD((s),#elt_num_sub#((e),(sub)),(p))
END SUBUNIVERSES

#define #uset#_Universe BS_Universe
#define #uset#_UniverseD BS_UniverseD
#define #uset#_Copy BS_Copy
#define #uset#_CopyD BS_CopyD

#define #uset#_Choose(x)                                                \
  #uset#_bs_elt_base_type(BS_Choose(x))

#define #uset#_ChooseS(x,s)                                             \
  #uset#_bs_elt_base_type_sub(BS_Choose(x),s)

#define #uset#_Intersection_Choose(x1,x2)                               \
  #uset#_bs_elt_base_type(BS_Intersection_Choose((x1),(x2)))

#define #uset#_Intersection_ChooseS(x1,x2,s)                            \
  #uset#_bs_elt_base_type_sub(BS_Intersection_Choose((x1),(x2)),(s))

#define #uset#_Choose_Next(x,e)                                         \
  #uset#_bs_elt_base_type(BS_Choose_Next((x),#elt_num#(e)))

BEGIN SUBUNIVERSES
/* Needs to evaluate "sub" twice, so it has to be a function:
 */
extern #base_type# #uset#_Choose_NextS(
  #uset#*,
  #base_type#,
  #uset#_SUBUNIVERSE*
);
END SUBUNIVERSES

#define #uset#_Intersection_Choose_Next(x1,x2,e)                        \
  #uset#_bs_elt_base_type(BS_Intersection_Choose_Next((x1),(x2),#elt_num#(e)))

BEGIN SUBUNIVERSES
/* Needs to evaluate "sub" twice, so it has to be a function:
 */
extern #base_type# #uset#_Intersection_Choose_NextS(
  #uset#*,
  #uset#*,
  #base_type#,
  #uset#_SUBUNIVERSE*
);
END SUBUNIVERSES

#define #uset#_Choose_Range(x,l,h)                                      \
  #uset#_bs_elt_base_type(BS_Choose_Range((x),(l),(h)))

#define #uset#_Choose_RangeS(x,l,h,s)                                   \
  #uset#_bs_elt_base_type_sub(BS_Choose_Range((x),(l),(h)),(s))

#define #uset#_Difference BS_Difference
#define #uset#_DifferenceD BS_DifferenceD

#define #uset#_Difference1(s,e,p)                                       \
    BS_Difference1(s,#elt_num#(e),(p))

BEGIN SUBUNIVERSES
#define #uset#_Difference1S(s,e,p,sub)                                  \
    BS_Difference1(s,#elt_num_sub#((e),(sub)),(p))
END SUBUNIVERSES

#define #uset#_Difference1D(s,e)                                        \
    BS_Difference1D(s,#elt_num#(e))

BEGIN SUBUNIVERSES
#define #uset#_Difference1DS(s,e,sub)                                   \
    BS_Difference1D(s,#elt_num_sub#((e),(sub)))
END SUBUNIVERSES

#define #uset#_Intersection BS_Intersection
#define #uset#_IntersectionD BS_IntersectionD
#define #uset#_Size BS_Size
#define #uset#_Union BS_Union
#define #uset#_UnionD BS_UnionD

#define #uset#_Union1(s,x,p)                                            \
    BS_Union1((s),#elt_num#(x),(p))

#define #uset#_UnionD_Intersection(s1,s2,s3,p)                          \
    BS_UnionD_Intersection((s1),(s2),(s3),(p))

BEGIN SUBUNIVERSES
#define #uset#_Union1S(s,x,p,sub)                                       \
    BS_Union1((s),#elt_num_sub#((x),(sub)),(p))
END SUBUNIVERSES

#define #uset#_Union1D(s,x,p)                                           \
    BS_Union1D((s),#elt_num#(x),(p))

BEGIN SUBUNIVERSES
#define #uset#_Union1DS(s,x,p,sub)                                      \
    BS_Union1D((s),#elt_num_sub#((x),(sub)),(p))
END SUBUNIVERSES

#define #uset#_ContainsP BS_ContainsP
#define #uset#_EmptyP BS_EmptyP
#define #uset#_EqualP BS_EqualP
#define #uset#_IntersectsP BS_IntersectsP

#define #uset#_MemberP(set,x) BS_MemberP((set),#elt_num#(x))

BEGIN SUBUNIVERSES
#define #uset#_MemberPS(set,x,s) BS_MemberP((set),#elt_num_sub#((x),(s)))
END SUBUNIVERSES

#define #uset#_Intersection_MemberP(set1,set2,x)                        \
    BS_Intersection_MemberP((set1),(set2),#elt_num#(x))

BEGIN SUBUNIVERSES
#define #uset#_Intersection_MemberPS(set1,set2,x,s)                     \
    BS_Intersection_MemberP((set1),(set2),#elt_num_sub#((x),(s)))
END SUBUNIVERSES

#define #uset#_Print BS_Print

#define FOR_ALL_#uset#_members(set,x)	\
    for (x = #uset#_Choose(set);	\
	 x != #uset#_CHOOSE_FAILURE;	\
	 x = #uset#_Choose_Next(set,x))

#ifdef __cplusplus
}
#endif
#endif /* #uset#_INCLUDED */
