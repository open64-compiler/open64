/*
 * Copyright (C) 2011 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdint.h>
#include <stdio.h>
#include "soe.h"
#include "wn.h"

#define F90_LOWER_INTERNAL
#include "pu_info.h"
#include "f90_lower.h"
#include "ipa_lno_util.h"

// Needed to make MAT package work

#ifdef SHARED_BUILD
template <> MEM_POOL* MAT<mINT32>::_default_pool = NULL;
#endif


//----------------------------------------------------------------------------------
//
// DIR_FLAG F90_Lower_Analyze_Triplet (INT64 l, INT64 s1, INT64 s2, INT64 e, BOOL e_present, MEM_POOL *mp)
//
// l - difference of lower bounds (LHS - RHS)
// s1,s2 - strides for LHS and RHS respectively
// e - extent of triplets
// e_present - TRUE if E is present
// mp - MEM_POOL to use as the working pool
//
// Solves the system:
// l + s1 n1 = s2 n2
// n1 >= 0, n2 >= 0
// n1 < e, n2 < e (if e is known)
//
// To see if there are any solutions for n1, n2. 
// If there are no solutions, the triplets are DIR_DONTCARE
// If there is solution, we add the constraints
// n1 <= n2 and return DIR_POSITIVE if this causes it do be inconsisttent 
// n1 >= n2 and return DIR_NEGATIVE if this causes it do be inconsisttent 
// else return DIR_NEGATIVE
//

DIR_FLAG F90_Lower_Analyze_Triplet (INT64 l, INT64 s1in, INT64 s2in, INT64 e, BOOL e_present, 
				    MEM_POOL *mp)
{
   INT32 s1,s2;
   INT32 row[2];
   SYSTEM_OF_EQUATIONS s(1,1,2,mp);
   
   // make sure the s1 and s2 fit in 32 bits

   s1 = s1in;
   s2 = s2in;
   if (s1 != s1in || s2 != s2in) return (DIR_UNKNOWN);
   
   /* Enter the equality condition */
   row[0] = s1;
   row[1] = -s2;
   s.Add_Eq(row,-l);
      
   /* Enter the basic constraints */
   row[0] = -1;
   row[1] = 0;
   s.Add_Le(row,0);
   row[0] = 0;
   row[1] = -1;
   s.Add_Le(row,0);

   if (e_present) {
      /* Add constraints on E */
      row[0] = 1;
      row[1] = 0;
      s.Add_Le(row,e-1);
      row[0] = 0;
      row[1] = 1;
      s.Add_Le(row,e-1);
   }

   if (!s.Is_Consistent()) {
      // System is independent
      return (DIR_DONTCARE);
   }

   /* Add constraint n1 <= n2 */
   row[0] = 1;
   row[1] = -1;
   s.Add_Le(row,0);
   if (!s.Is_Consistent()) {
      return (DIR_POSITIVE);
   }

   // replace constraint with one running in the other direction

   s.Remove_Last_Le();
   /* Add constraint n2 <= n1 */
   row[0] = -1;
   row[1] = 1;
   s.Add_Le(row,0);

   if (!s.Is_Consistent()) {
      return (DIR_NEGATIVE);
   }
   
   // Unfortunately we have to give up
   return (DIR_UNKNOWN);
}
