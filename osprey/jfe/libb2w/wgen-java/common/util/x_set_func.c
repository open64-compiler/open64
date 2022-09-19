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
 * Module: x_set_function.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/x_set_func.c,v $
 *
 * Revision history:
 *  05-05-93 - Original Version
 *
 * Description:
 *
 *      Template for functional implementation of sets of objects.
 *      This is processed by common/util/gen_x_set into a particular
 *      instantiation for a particular base type.  See below for a
 *      complete interface documentation.
 *
 * ====================================================================
 * ====================================================================
 */

BEGIN

#include "bitset.h"
#include "#filename#.h"

#uset# *#uset#_Create(
  size_t    size,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Create(size,pool);
}

size_t #uset#_Size_Alloc_Size(
  size_t    size
)
{
  return BS_Size_Alloc_Size(size);
}

size_t #uset#_Alloc_Size(
  #uset# *set
)
{
  return BS_Alloc_Size((BS*) set);
}

#uset# *#uset#_ClearD(
  #uset# *set
)
{
  return (#uset#*) BS_ClearD((BS*) set);
}

#uset# *#uset#_Create_Empty(
  size_t size,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Create_Empty(size,pool);
}

#uset# *#uset#_Range(
  BS_ELT      low,
  BS_ELT      high,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_Range(low,high,pool);
}

#uset# *#uset#_RangeD(
  #uset#     *set,
  BS_ELT      low,
  BS_ELT      high,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_RangeD((BS*) set,low,high,pool);
}

#uset# *#uset#_Singleton(
  #base_type# element,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_Singleton(#elt_num#(element),pool);
}

#uset# *#uset#_SingletonD(
  #uset#     *set,
  #base_type# element,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_SingletonD((BS*) set,#elt_num#(element),pool);
}

BEGIN SUBUNIVERSES
#uset# *#uset#_SingletonS(
  #base_type# element,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_Singleton(#elt_num_sub#(element,sub),pool);
}

#uset# *#uset#_SingletonDS(
  #uset#     *set,
  #base_type# element,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_SingletonD((BS*) set,#elt_num_sub#(element,sub),pool);
}

END SUBUNIVERSES
#uset# *#uset#_Universe(
  size_t    size,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Universe(size,pool);
}

#uset# *#uset#_UniverseD(
  #uset#   *set,
  size_t    size,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_UniverseD((BS*) set,size,pool);
}

#uset# *#uset#_Copy(
  #uset#   *set,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Copy((BS*) set,pool);
}

#uset# *#uset#_CopyD(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_CopyD((BS*) set1,(BS*) set2,pool);
}

#base_type# #uset#_Choose(
  #uset# *set
)
{
  BS_ELT elt = BS_Choose((BS*) set);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt#(elt);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Intersection_Choose(
  #uset# *set1,
  #uset# *set2
)
{
  BS_ELT elt = BS_Intersection_Choose((BS*) set1, (BS*) set2);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt#(elt);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Choose_Next(
  #uset#     *set,
  #base_type# x
)
{
  BS_ELT elt = BS_Choose_Next((BS*) set, #elt_num#(x));
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt#(elt);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Intersection_Choose_Next(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x
)
{
  BS_ELT elt = BS_Intersection_Choose_Next((BS*) set1, (BS*) set2,
                                                      #elt_num#(x));
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt#(elt);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Choose_Range(
  #uset#   *set,
  BS_ELT    low,
  BS_ELT    high
)
{
  BS_ELT elt = BS_Choose_Range((BS*) set,low,high);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt#(elt);
  else
    return #uset#_CHOOSE_FAILURE;
}

BEGIN SUBUNIVERSES
#base_type# #uset#_ChooseS(
  #uset# *set,
  #uset#_SUBUNIVERSE *sub
)
{
  BS_ELT elt = BS_Choose((BS*) set);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt_sub#(elt,sub);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Intersection_ChooseS(
  #uset# *set1,
  #uset# *set2,
  #uset#_SUBUNIVERSE *sub
)
{
  BS_ELT elt = BS_Intersection_Choose((BS*) set1, (BS*) set2);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt_sub#(elt,sub);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Choose_NextS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
)
{
  BS_ELT elt = BS_Choose_Next((BS*) set, #elt_num_sub#(x,sub));
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt_sub#(elt,sub);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Intersection_Choose_NextS(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
)
{
  BS_ELT elt = BS_Intersection_Choose_Next((BS*) set1, (BS*) set2,
                                                      #elt_num_sub#(x,sub));
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt_sub#(elt,sub);
  else
    return #uset#_CHOOSE_FAILURE;
}

#base_type# #uset#_Choose_RangeS(
  #uset#   *set,
  BS_ELT    low,
  BS_ELT    high,
  #uset#_SUBUNIVERSE *sub
)
{
  BS_ELT elt = BS_Choose_Range((BS*) set,low,high);
  if ( elt != BS_CHOOSE_FAILURE )
    return #num_elt_sub#(elt,sub);
  else
    return #uset#_CHOOSE_FAILURE;
}

END SUBUNIVERSES
#uset# *#uset#_Difference(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Difference((BS*) set1,(BS*) set2,pool);
}

#uset# *#uset#_DifferenceD(
  #uset# *set1,
  #uset# *set2
)
{
  return (#uset#*) BS_DifferenceD((BS*) set1,(BS*) set2);
}

#uset# *#uset#_Difference1(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_Difference1((BS*) set,#elt_num#(x),pool);
}

#uset# *#uset#_Difference1D(
  #uset#     *set,
  #base_type# x
)
{
  return (#uset#*) BS_Difference1D((BS*) set,#elt_num#(x));
}

BEGIN SUBUNIVERSES
#uset# *#uset#_Difference1S(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_Difference1((BS*) set,#elt_num_sub#(x,sub),pool);
}

#uset# *#uset#_Difference1DS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_Difference1D((BS*) set,#elt_num_sub#(x,sub));
}

END SUBUNIVERSES
#uset# *#uset#_Intersection(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Intersection((BS*) set1,(BS*) set2,pool);
}

#uset# *#uset#_IntersectionD(
  #uset# *set1,
  #uset# *set2
)
{
  return (#uset#*) BS_IntersectionD((BS*) set1,(BS*) set2);
}

size_t #uset#_Size(
  #uset# *set
)
{
  return BS_Size((BS*) set);
}

#uset# *#uset#_Union(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_Union((BS*) set1,(BS*) set2,pool);
}

#uset# *#uset#_UnionD(
  #uset#   *set1,
  #uset#   *set2,
  MEM_POOL *pool
)
{
  return (#uset#*) BS_UnionD((BS*) set1,(BS*) set2,pool);
}

#uset# *#uset#_Union1(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_Union1((BS*) set,#elt_num#(x),pool);
}

#uset# *#uset#_Union1D(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool
)
{
  return (#uset#*) BS_Union1D((BS*) set,#elt_num#(x),pool);
}

#uset# *#uset#_Union1D_Intersection( #uset# *set1, #uset# *set2, #uset# *set3,
                                     MEM_POOL *pool )
{
  return (#uset#*) BS_UnionD_Intersection((BS*)set1,(BS*)set2,(BS*)set3,pool);
}

BEGIN SUBUNIVERSES
#uset# *#uset#_Union1S(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_Union1((BS*) set,#elt_num_sub#(x,sub),pool);
}

#uset# *#uset#_Union1DS(
  #uset#     *set,
  #base_type# x,
  MEM_POOL   *pool,
  #uset#_SUBUNIVERSE *sub
)
{
  return (#uset#*) BS_Union1D((BS*) set,#elt_num_sub#(x,sub),pool);
}

END SUBUNIVERSES
BOOL #uset#_ContainsP(
  #uset# *set1,
  #uset# *set2
)
{
  return BS_ContainsP((BS*) set1,(BS*) set2);
}

BOOL #uset#_EmptyP(
  #uset# *set
)
{
  return BS_EmptyP((BS*) set);
}

BOOL #uset#_EqualP(
  #uset# *set1,
  #uset# *set2
)
{
  return BS_EqualP((BS*) set1,(BS*) set2);
}

BOOL #uset#_IntersectsP(
  #uset# *set1,
  #uset# *set2
)
{
  return BS_IntersectsP((BS*) set1,(BS*) set2);
}

BOOL #uset#_MemberP(
  #uset#     *set,
  #base_type# x
)
{
  return BS_MemberP((BS*) set,#elt_num#(x));
}

BOOL #uset#_Intersection_MemberP(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x
)
{
  return BS_Intersection_MemberP((BS*) set1, (BS*) set2, #elt_num#(x));
}

BEGIN SUBUNIVERSES
BOOL #uset#_MemberPS(
  #uset#     *set,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
)
{
  return BS_MemberP((BS*) set,#elt_num_sub#(x,sub));
}

BOOL #uset#_Intersection_MemberPS(
  #uset#     *set1,
  #uset#     *set2,
  #base_type# x,
  #uset#_SUBUNIVERSE *sub
)
{
  return BS_Intersection_MemberP((BS*) set1, (BS*) set2, #elt_num_sub#(x,sub));
}

END SUBUNIVERSES

void #uset#_Print(
  #uset# *set,
  FILE   *f
)
{
  BS_Print((BS*) set,f);
}
