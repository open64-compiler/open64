/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007, 2008.  Pathscale, LLC. All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
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


//  
//  Generate PROCESSOR properties information
///////////////////////////////////////
//  $Revision$
//  $Date$
//  $Author$
//  $Source$

#include <stddef.h>
#include "targ_proc.h"
#include "proc_properties_gen.h"

int main()
{
  PROC_PROPERTY 
    branch_delay_slot,		/* branch delay slot */
    same_cycle_branch_shadow,   /* execute branch shadow parallel with branch */
    out_of_order, 		/* out of order execution */
    superscalar,		/* multiple insts per cycle */
    bundles,			/* executes insts as sequence of bundles */
    delayed_exception,		/* has delayed exception support */
    fast_recip;			/* recip inst is fast */

  PROC_Properties_Begin ("x8664");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

  /* Does the target have branch delay slots?
   */
  branch_delay_slot = PROC_Property_Create( "has_branch_delay_slot" );
  Processor_Group( branch_delay_slot, 
		   PROCESSOR_UNDEFINED );

  /* Can the branch shadow be executed in the same cycle as the branch on
   * the target?
   */
  same_cycle_branch_shadow = PROC_Property_Create ("has_same_cycle_branch_shadow");
  Processor_Group (same_cycle_branch_shadow, 
		   PROCESSOR_UNDEFINED);

  /* Is the target an out-of-order machine?
   */
  out_of_order = PROC_Property_Create ("is_out_of_order");
  Processor_Group (out_of_order, 
		   PROCESSOR_opteron,
                   PROCESSOR_barcelona,
                   PROCESSOR_orochi,
		   PROCESSOR_em64t,
		   PROCESSOR_core,
		   PROCESSOR_wolfdale,
		   PROCESSOR_UNDEFINED);

  /* Can the current target issue multiple instructions per cycle?
   */
  superscalar = PROC_Property_Create ("is_superscalar");
  Processor_Group (superscalar,
		   PROCESSOR_opteron,
                   PROCESSOR_barcelona,
                   PROCESSOR_orochi,
		   PROCESSOR_em64t,
		   PROCESSOR_core,
		   PROCESSOR_wolfdale,
		   PROCESSOR_UNDEFINED);

  /* Does the target execute insts as sequence of bundles, or require 
   * bundle alignment? The info is used to align instructions to bundles, 
   * resolve any bundle packing requirements, etc...
   */
  bundles = PROC_Property_Create ("has_bundles");
  Processor_Group (bundles,
		   PROCESSOR_UNDEFINED);

  /* Does the target support delayed_exception mechanism, i.e ability to
   * suppress possible exceptions for speculative instructions with
   * delayed recovery mechanism.
   */
  delayed_exception = PROC_Property_Create ("has_delayed_exception");
  Processor_Group (delayed_exception,
		   PROCESSOR_UNDEFINED);

  /* Does the target have a fast recip instruction? 
   * i.e. is it profitable to convert a/b -> a*recip(b)
   */
  fast_recip = PROC_Property_Create ("has_fast_recip");
  Processor_Group (fast_recip,
		   PROCESSOR_UNDEFINED);
  
  PROC_Properties_End();
  return 0;
}
