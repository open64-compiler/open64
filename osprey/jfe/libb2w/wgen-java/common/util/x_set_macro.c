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
 * Module: x_set_macro.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/x_set_macro.c,v $
 *
 * Revision history:
 *  05-05-93 - Original Version
 *
 * Description:
 *
 *      Template for macro helper functions for sets of objects.
 *      This is processed by common/util/gen_x_set into a particular
 *      instantiation for a particular base type.
 *
 * ====================================================================
 * ====================================================================
 */

BEGIN

BEGIN SUBUNIVERSES
#include "bitset.h"
#include "#filename#.h"

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

END SUBUNIVERSES

