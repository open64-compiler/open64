/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

//  
//  Generate PROCESSOR properties information
///////////////////////////////////////
//  $Revision: 1.2 $
//  $Date: 2006/01/05 10:38:07 $
//  $Author: weitang $
//  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/targ_info/proc/MIPS/proc_properties.cxx,v $

#include <stddef.h>
#include "targ_proc.h"
#include "proc_properties_gen.h"

main()
{
  PROC_PROPERTY 
    branch_delay_slot,		/* branch delay slot */
    same_cycle_branch_shadow,   /* execute branch shadow parallel with branch */
    out_of_order, 		/* out of order execution */
    superscalar,		/* multiple insts per cycle */
    bundles,			/* executes insts as sequence of bundles */
    delayed_exception,		/* has delayed exception support */
    fast_recip;			/* recip inst is fast */

  PROC_Properties_Begin ("MIPS");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

  /* Does the target have branch delay slots?
   */
  branch_delay_slot = PROC_Property_Create ("has_branch_delay_slot");
  Processor_Group (branch_delay_slot, 
#if !defined(TARG_SL)
	  		PROCESSOR_sb1,
	  		PROCESSOR_r10000,
#endif
			PROCESSOR_UNDEFINED);

  /* Can the branch shadow be executed in the same cycle as the branch on
   * the target?
   */
  same_cycle_branch_shadow = PROC_Property_Create ("has_same_cycle_branch_shadow");
  Processor_Group (same_cycle_branch_shadow, 
#if !defined(TARG_SL)
	  		PROCESSOR_sb1,
#endif
			PROCESSOR_UNDEFINED);

  /* Is the target an out-of-order machine?
   */
  out_of_order = PROC_Property_Create ("is_out_of_order");
  Processor_Group (out_of_order, 
#if !defined(TARG_SL)
	  		PROCESSOR_r10000,
#endif
			PROCESSOR_UNDEFINED);

  /* Can the current target issue multiple instructions per cycle?
   */
  superscalar = PROC_Property_Create ("is_superscalar");
  Processor_Group (superscalar,
#if defined(TARG_SL)
      PROCESSOR_sl2_pcore,
      PROCESSOR_sl2_mcore,
      PROCESSOR_sl1_pcore,
      PROCESSOR_sl1_dsp,
      PROCESSOR_sl5,        
#else
      PROCESSOR_sb1,
      PROCESSOR_r10000,
#endif
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
#if !defined(TARG_SL)
	  		PROCESSOR_sb1,
	  		PROCESSOR_r10000,
#endif
			PROCESSOR_UNDEFINED);

  PROC_Properties_End();
  return 0;
}
