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
 * Module: x_set_func.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/x_set_func.h,v $
 *
 * Revision history:
 *  05-05-93 - Original Version
 *
 * Description:
 *
 *      Template for functional interface to sets of objects.  This is
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


#include "bitset.h"	/* to avoid clients having to know about it */

typedef struct dont_use_this_tag_#lset# #uset#;

BEGIN SUBUNIVERSES
class #uset#_SUBUNIVERSE;
END SUBUNIVERSES

#define #uset#_CHOOSE_FAILURE ((#base_type#) -1)
#define #uset#_ITER_DONE      ((#base_type#) -1)

extern #uset# *#uset#_Create(
  size_t    size,
  MEM_POOL *pool
);
extern size_t #uset#_Size_Alloc_Size(
  size_t    size
);
extern size_t #uset#_Alloc_Size(
  #uset# *set
);
extern #uset# *#uset#_ClearD(
  #uset# *set
);
extern #uset# *#uset#_Create_Empty(
  size_t size,
  MEM_POOL *pool
);
extern #uset# *#uset#_Range(
  BS_ELT      low,
  BS_ELT      high,
  MEM_POOL   *pool
);
extern #uset# *#uset#_RangeD(
  #uset#     *set,
  BS_ELT      low,
  BS_ELT      high,
  MEM_POOL   *pool
);
extern #uset# *#uset#_Singleton(
  #base_type# element,
  MEM_POOL   *pool
);
extern #uset# *#uset#_SingletonD(
  #uset#     *set,
  #base_type# element,
  MEM_POOL   *pool
);
BEGIN SUBUNIVERSES
extern #uset# *#uset#_SingletonS(
  #base_type# element,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
);
extern #uset# *#uset#_SingletonDS(
  #uset#     *set,
  #base_type# element,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
);
END SUBUNIVERSES
extern #uset# *#uset#_Universe(
  size_t    size,
  MEM_POOL *pool
);
extern #uset# *#uset#_UniverseD(
  #uset#   *set,
  size_t    size,
  MEM_POOL *pool
);
extern #uset# *#uset#_Copy(
  #uset#   *set,
  MEM_POOL *pool
);
extern #uset# *#uset#_CopyD(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
);
extern #base_type# #uset#_Choose(
  #uset# *set
);
extern #base_type# #uset#_Intersection_Choose(
  #uset# *set1,
  #uset# *set2
);
extern #base_type# #uset#_Choose_Next(
  #uset#     *set,
  #base_type# x
);
extern #base_type# #uset#_Intersection_Choose_Next(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x
);
extern #base_type# #uset#_Choose_Range(
  #uset#   *set,
  BS_ELT    low,
  BS_ELT    high
);
BEGIN SUBUNIVERSES
extern #base_type# #uset#_ChooseS(
  #uset# *set,
  #uset#_SUBUNIVERSE *sub
);
extern #base_type# #uset#_Intersection_ChooseS(
  #uset# *set1,
  #uset# *set2,
  #uset#_SUBUNIVERSE *sub
);
extern #base_type# #uset#_Choose_NextS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
);
extern #base_type# #uset#_Intersection_Choose_NextS(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
);
extern #base_type# #uset#_Choose_RangeS(
  #uset#   *set,
  BS_ELT    low,
  BS_ELT    high,
  #uset#_SUBUNIVERSE *sub
);
END SUBUNIVERSES
extern #uset# *#uset#_Difference(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
);
extern #uset# *#uset#_DifferenceD(
  #uset# *set1,
  #uset# *set2
);
extern #uset# *#uset#_Difference1(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
);
extern #uset# *#uset#_Difference1D(
  #uset#     *set,
  #base_type# x
);
BEGIN SUBUNIVERSES
extern #uset# *#uset#_Difference1S(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
);
extern #uset# *#uset#_Difference1DS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
);
END SUBUNIVERSES
extern #uset# *#uset#_Intersection(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
);
extern #uset# *#uset#_IntersectionD(
  #uset# *set1,
  #uset# *set2
);
extern size_t #uset#_Size(
  #uset# *set
);
extern #uset# *#uset#_Union(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
);
extern #uset# *#uset#_UnionD(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
);
extern #uset# *#uset#_Union1(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
);
extern #uset# *#uset#_Union1D(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
);
extern #uset# *#uset#_UnionD_Intersection(
  #uset#     *set1,
  #uset#     *set2,
  #uset#     *set3,
  MEM_POOL   *pool
);
BEGIN SUBUNIVERSES
extern #uset# *#uset#_Union1S(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
);
extern #uset# *#uset#_Union1DS(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
);
END SUBUNIVERSES
extern BOOL #uset#_ContainsP(
  #uset# *set1,
  #uset# *set2
);
extern BOOL #uset#_EmptyP(
  #uset# *set
);
extern BOOL #uset#_EqualP(
  #uset# *set1,
  #uset# *set2
);
extern BOOL #uset#_IntersectsP(
  #uset# *set1,
  #uset# *set2
);
extern BOOL #uset#_MemberP(
  #uset#     *set,
  #base_type# x
);
extern BOOL #uset#_Intersection_MemberP(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x
);
BEGIN SUBUNIVERSES
extern BOOL #uset#_MemberPS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
);
extern BOOL #uset#_Intersection_MemberPS(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
);
END SUBUNIVERSES
extern void #uset#_Print(
  #uset# *set,
  FILE   *f
);

#ifdef __cplusplus
}
#endif
#endif /* #uset#_INCLUDED */
