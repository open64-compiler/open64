/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*
 * Copyright 2003, 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


// This may look like C code, but it is really -*- C++ -*-
//
// ====================================================================
// ====================================================================
//
// Module: config_targ_cache.cxx
// Revision history:
//  14-Nov-96 - Original Version, copied from cache_parameters.cxx.
//
// Description:
//
// Target specific logic for config_cache.cxx.
//
// ====================================================================
// ====================================================================

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "errors.h"
#include "config_cache.h"
#include "config_lno.h"
#include "config_targ.h"
#include "config_platform.h"
 

void MHD::Initialize()
{
  Non_Blocking_Loads      = TRUE;
  Loop_Overhead_Memref    = 1;
  TLB_NoBlocking_Model    = TRUE;

  // Bugs 2471, 2580, 2627, 3093
  Loop_Overhead_Base      = 18;
  TLB_Trustworthiness     = 21;

  switch (Target) {
  case TARGET_opteron:
  case TARGET_anyx86:
    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     64*1024, 		// Size
                     64,		// Line Size
                     11,		// Clean Miss Penalty
                     11,		// Dirty Miss Penalty
                     2,			// Associativity
		     32,		// TLB Entries
                     4*1024,		// Page Size
                     50,		// TLB Clean Miss Penalty ?
                     50,		// TLB Dirty Miss Penalty ?
                     6.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     50);		// Pct_Excess_Writes_Nonhidable ?
    break;
  case TARGET_athlon64:
    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     64*1024, 		// Size
                     64,		// Line Size
                     11,		// Clean Miss Penalty
                     11,		// Dirty Miss Penalty
                     2,			// Associativity
		     32,		// TLB Entries
                     4*1024,		// Page Size
                     50,		// TLB Clean Miss Penalty ?
                     50,		// TLB Dirty Miss Penalty ?
                     6.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     50);		// Pct_Excess_Writes_Nonhidable ?
    break;
  case TARGET_athlon:
    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     64*1024, 		// Size
                     64,		// Line Size
                     27,		// Clean Miss Penalty
                     27,		// Dirty Miss Penalty
                     2,			// Associativity
		     32,		// TLB Entries
                     4*1024,		// Page Size
                     5,			// TLB Clean Miss Penalty ?
                     5,			// TLB Dirty Miss Penalty ?
                     2.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     50);		// Pct_Excess_Writes_Nonhidable ?
    break;
  // TODO: Fine-tune pentium4/xeon/em64t parameters
  case TARGET_pentium4:
  case TARGET_xeon:
    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     8*1024, 		// Size
                     64,		// Line Size
                     27,		// Clean Miss Penalty
                     27,		// Dirty Miss Penalty
                     4,			// Associativity
		     64,		// TLB Entries
                     4*1024,		// Page Size
                     5,			// TLB Clean Miss Penalty ?
                     5,			// TLB Dirty Miss Penalty ?
                     2.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     50);		// Pct_Excess_Writes_Nonhidable ?
    break;
  case TARGET_em64t:
    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     64*1024, 		// Size
                     64,		// Line Size
                     11,		// Clean Miss Penalty
                     11,		// Dirty Miss Penalty
                     2,			// Associativity
		     32,		// TLB Entries
                     4*1024,		// Page Size
                     50,		// TLB Clean Miss Penalty ?
                     50,		// TLB Dirty Miss Penalty ?
                     6.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     50);		// Pct_Excess_Writes_Nonhidable ?
    break;
  default:
    FmtAssert(FALSE, ("Cannot handle L1 for %s in MHD::Initialize\n",
		      Targ_Name(Target)));
    break;
  }
  
  switch (Target) {
  case TARGET_opteron:
  case TARGET_anyx86:
    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     1*1024*1024, 
                     64, 
                     150,
                     200, // ? 
                     16,  
                     512,
                     4*1024, 
                     50, // ?
                     50, // ?
                     LNO_Run_Prefetch ? 1.8: 1.0,  // ?
                     LNO_Run_Prefetch ? 0.7 : 0.1,  // ?
                     LNO_Run_Prefetch ? 0.3 : 0.05, // ?
                     LNO_Run_Prefetch ? 25 : 50);  // ?
    break;
  case TARGET_athlon64:
    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     1*1024*1024, 
                     64, 
                     150,
                     200, // ? 
                     16,  
                     512,
                     4*1024, 
                     50, // ?
                     50, // ?
                     LNO_Run_Prefetch ? 1.8: 1.0,  // ?
                     LNO_Run_Prefetch ? 0.7 : 0.1,  // ?
                     LNO_Run_Prefetch ? 0.3 : 0.05, // ?
                     LNO_Run_Prefetch ? 25 : 50);  // ?
    break;
  case TARGET_athlon:
    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     1*512*1024, 
                     64, 
                     103,
                     103, // ? 
                     16,  
                     256,
                     4*1024, 
                     52, // ?
                     52, // ?
                     LNO_Run_Prefetch ? 1.8: 1.0,  // ?
                     LNO_Run_Prefetch ? 0.7 : 0.1,  // ?
                     LNO_Run_Prefetch ? 0.3 : 0.05, // ?
                     LNO_Run_Prefetch ? 25 : 50);  // ?
    break;
  // TODO: Fine-tune pentium4/xeon/em64t parameters
  case TARGET_pentium4:
  case TARGET_xeon:
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     1*1024*1024, 
                     64, 
                     103,
                     103, // ? 
                     16,  
                     256,
                     4*1024, 
                     52, // ?
                     52, // ?
                     LNO_Run_Prefetch ? 1.8: 1.0,  // ?
                     LNO_Run_Prefetch ? 0.7 : 0.1,  // ?
                     LNO_Run_Prefetch ? 0.3 : 0.05, // ?
                     LNO_Run_Prefetch ? 25 : 50);  // ?
    break;
  case TARGET_em64t:
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     1*1024*1024, 
                     64, 
                     150,
                     200, // ? 
                     16,  
                     512,
                     4*1024, 
                     50, // ?
                     50, // ?
                     LNO_Run_Prefetch ? 1.8: 1.0,  // ?
                     LNO_Run_Prefetch ? 0.7 : 0.1,  // ?
                     LNO_Run_Prefetch ? 0.3 : 0.05, // ?
                     LNO_Run_Prefetch ? 25 : 50);  // ?
    break;
  default:
    FmtAssert(FALSE, ("Cannot handle L2 for %s in MHD::Initialize\n",
		      Targ_Name(Target)));
    break;
  }

#ifdef Is_True_On
  if (LNO_Verbose)
    printf ("Target Processor: %s. %lld (%d), %lld (%d)\n", 
	    Targ_Name(Target),
	    L[0].Effective_Size, L[0].Line_Size,
	    L[1].Effective_Size, L[1].Line_Size);
#endif
}
