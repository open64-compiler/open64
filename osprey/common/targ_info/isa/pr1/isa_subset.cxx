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

//  
//  Generate ISA subset desdriptions
/////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Intel1 instructions
//
// Within each ISA_SUBSET instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//
//  $Revision: 1.1 $
//  $Date: 2005/07/27 02:18:12 $
//  $Author: kevinlo $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/isa/pr1/isa_subset.cxx,v $

#include <stddef.h>
#include "topcode.h"
#include "isa_subset_gen.h"

main()
{
  ISA_Subset_Begin("pr1");
  ISA_SUBSET pr1 = ISA_Subset_Create(NULL,"pr1");

/* ====================================================================
 *             pr11 Instructions (includes fictional ops)
 * ====================================================================
 */
  for (int i = 0; i < TOP_count; ++i)
    Instruction(pr1, static_cast<TOP>(i));

  ISA_Subset_End();
  return 0;
}

