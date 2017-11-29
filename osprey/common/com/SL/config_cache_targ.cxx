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
  if (Is_Target_Sb1()) {  

    Non_Blocking_Loads      = TRUE;
    Loop_Overhead_Base      = 18;
    Loop_Overhead_Memref    = 1;
    TLB_Trustworthiness     = 75;
    TLB_NoBlocking_Model    = TRUE;

    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     32*1024, 		// Size
                     32,		// Line Size
                     12,		// Clean Miss Penalty
                     12,		// Dirty Miss Penalty
                     4,			// Associativity
                     64, 		// TLB Entries
                     32*1024, // TODO 		// Page Size
                     50,// TODO		// TLB Clean Miss Penalty ?
                     50,// TODO		// TLB Dirty Miss Penalty ?
                     3.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     100);		// Pct_Excess_Writes_Nonhidable ?

    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     32*1024*16, 
                     32, 
                     80,
                     80,
                     4,  
                     -1, 
                     -1, 
                     -1, 
                     -1,
                     (LNO_Run_Prefetch ? 1.8 : 1.0),  // ?
                     (LNO_Run_Prefetch ? 0.7 : 0.1),  // ?
                     (LNO_Run_Prefetch ? 0.3 : 0.05), // ?
                     (LNO_Run_Prefetch ? 25  : 50));  // ?

#ifdef Is_True_On
    if (LNO_Verbose)
      printf ("Target Processor: TARGET_MIPS. %lld (%d), %lld (%d)\n", 
              L[0].Effective_Size, L[0].Line_Size,
              L[1].Effective_Size, L[1].Line_Size);
#endif
  } else if (Is_Target_R10K()) {
	  
    Non_Blocking_Loads      = TRUE;
    Loop_Overhead_Base      = 18;
    Loop_Overhead_Memref    = 1;
    TLB_Trustworthiness     = 75;
    TLB_NoBlocking_Model    = TRUE;

    L[0] = MHD_LEVEL(MHD_TYPE_CACHE, 	// Type
                     32*1024, 		// Size
                     32,		// Line Size
                     10,		// Clean Miss Penalty
                     18,		// Dirty Miss Penalty
                     2,			// Associativity
                     60, 		// TLB Entries
                     32*1024, // TODO 		// Page Size
                     50,// TODO		// TLB Clean Miss Penalty ?
                     50,// TODO		// TLB Dirty Miss Penalty ?
                     3.0,		// Typical Outstanding Loads ?
                     0.8,		// Load_OP_Overlap_1 ?
                     0.4,		// Load_OP_Overlap_2 ?
                     100);		// Pct_Excess_Writes_Nonhidable ?

    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE, 
                     32*1024*16*8, 
                     128, 
                     40,
                     66,
                     2,  
                     -1, 
                     -1, 
                     -1, 
                     -1,
                     3,
                     0.7,
                     0.3,
                     (LNO_Run_Prefetch ? 25  : 50));  // ?

#ifdef Is_True_On
    if (LNO_Verbose)
      printf ("Target Processor: TARGET_MIPS. %lld (%d), %lld (%d)\n", 
              L[0].Effective_Size, L[0].Line_Size,
              L[1].Effective_Size, L[1].Line_Size);
#endif
  }
#ifdef TARG_SL
  else if (Is_Target_Sl1_pcore() ||  Is_Target_Sl1_dsp()) {
    Non_Blocking_Loads      = TRUE;
    Loop_Overhead_Base      = 18;
    Loop_Overhead_Memref    = 1;
    TLB_Trustworthiness     = 75;
    TLB_NoBlocking_Model    = TRUE;

    L[0] = MHD_LEVEL(MHD_TYPE_CACHE,    // Type
                     32*1024,           // Size
                     32,                // Line Size
                     10,                // Clean Miss Penalty
                     18,                // Dirty Miss Penalty
                     2,                 // Associativity
                     60,                // TLB Entries
                     32*1024, // TODO           // Page Size
                     50,// TODO         // TLB Clean Miss Penalty ?
                     50,// TODO         // TLB Dirty Miss Penalty ?
                     3.0,               // Typical Outstanding Loads ?
                     0.8,               // Load_OP_Overlap_1 ?
                     0.4,               // Load_OP_Overlap_2 ?
                     100);              // Pct_Excess_Writes_Nonhidable ?

    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE,
                     32*1024*16*8,
                     128,
                     40,
                     66,
                     2,
                     -1,
                     -1,
                     -1,
                     -1,
                     3,
                     0.7,
                     0.3,
                     (LNO_Run_Prefetch ? 25  : 50));  // ?

#ifdef Is_True_On
    if (LNO_Verbose)
      printf ("Target Processor: TARGET_SL1. %lld (%d), %lld (%d)\n",
              L[0].Effective_Size, L[0].Line_Size,
              L[1].Effective_Size, L[1].Line_Size);
#endif

  } else if (Is_Target_Sl2_pcore() || Is_Target_Sl2_mcore()) {
    Non_Blocking_Loads      = TRUE;
    Loop_Overhead_Base      = 18;
    Loop_Overhead_Memref    = 1;
    TLB_Trustworthiness     = 75;
    TLB_NoBlocking_Model    = TRUE;

    L[0] = MHD_LEVEL(MHD_TYPE_CACHE,    // Type
                     32*1024,           // Size
                     32,                // Line Size
                     10,                // Clean Miss Penalty
                     18,                // Dirty Miss Penalty
                     2,                 // Associativity
                     60,                // TLB Entries
                     32*1024, // TODO           // Page Size
                     50,// TODO         // TLB Clean Miss Penalty ?
                     50,// TODO         // TLB Dirty Miss Penalty ?
                     3.0,               // Typical Outstanding Loads ?
                     0.8,               // Load_OP_Overlap_1 ?
                     0.4,               // Load_OP_Overlap_2 ?
                     100);              // Pct_Excess_Writes_Nonhidable ?

    // TODO: this might be too generous: in multiple processor situations,
    // there is a cost to loading the shared bus/memory.
    L[1] = MHD_LEVEL(MHD_TYPE_CACHE,
                     32*1024*16*8,
                     128,
                     40,
                     66,
                     2,
                     -1,
                     -1,
                     -1,
                     -1,
                     3,
                     0.7,
                     0.3,
                     (LNO_Run_Prefetch ? 25  : 50));  // ?

#ifdef Is_True_On
    if (LNO_Verbose)
      printf ("Target Processor: TARGET_SL2. %lld (%d), %lld (%d)\n",
              L[0].Effective_Size, L[0].Line_Size,
              L[1].Effective_Size, L[1].Line_Size);
#endif

  }else if (Is_Target_Sl5()) {
    //TODO
  }
#endif 
  else {
    FmtAssert(FALSE, ("Unknown target in MHD::Initialize\n"));
  }
}
