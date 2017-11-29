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


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: cg_swp_options.h
 *
 *   This file defines the SWP_OPTION data structure.
 *   The global variable SWP_Default is initialized using
 *   the SWP_OPTION constructore.  The SWP_Default is modified
 *   by the command line options (see cgdriver.cxx).
 *
 *   PU_Configure() is called once per PU.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cg_swp_options_INCLUDED
#define cg_swp_options_INCLUDED "cg_swp_options.h"

#include "cg_flags.h"
#include "cg_swp.h"

struct SWP_OPTIONS {

  // Options accessible from command line (See cgdriver.cxx)
  //
  INT32 Sched_Direction;
  INT32 Heuristics;
  INT32 Opt_Level;
#ifdef TARG_IA64
  BOOL  Enable_Do_Loop;
#endif
  BOOL  Enable_While_Loop;
  BOOL  Enable_Bundling;
  BOOL  Enable_Post_Incr;
  INT32 Min_Unroll_Times;
  INT32 Max_Unroll_Times;
  INT32 Min_Unroll_Times_Set;
  INT32 Max_Unroll_Times_Set;
#ifdef TARG_IA64
  INT32 Load_Cache_Miss_Ratio;
  INT32 Load_Cache_Miss_Latency;
#endif
  INT32 Critical_Threshold;
  BOOL  Prep_Only;
  BOOL  Min_Retry; 
  BOOL  Implicit_Prefetch;
  BOOL  Implicit_Prefetch_Set;
  BOOL  Predicate_Promotion;
  BOOL  Enable_BRP;
#ifdef TARG_IA64
  INT32 FB_Prob1;	// percentage of lower bound of feedback prob
  INT32 FB_Prob2;	// combined with frequency
  INT32 FB_Freq;	// lower bound of feedback freqency
#endif
  INT32 OPS_Limit;

  // Options not accessible from command line
  //  -  some are basically compile-time constants;
  //  -  some are derivatives of the command line options.
  //     e.g. opt_level controls the Budget and Max_II, II_Incr.
  //
  INT32 Budget;
  INT32 Max_Schedule_Incr;
  double Max_II_Alpha;
  double Max_II_Beta;
  double II_Incr_Alpha;
  double II_Incr_Beta;
  INT32 Grainy_Resources_Length;

  // Debugging options
  BOOL Enable_Workaround;
  INT32 Starting_II;

  // Constructor to setup default values
  //
  SWP_OPTIONS() 
  {
    // Scheduling Direction
    //   0 - bidirection
    //   1 - top-down
    //   2 - bottom-up
    Sched_Direction = 0; 

    // SWP Heuristics
    //   0 - resource-scaled slack 
    //   1 - lstart
    //   2 - (-estart)
    Heuristics = 0;

    // SWP Opt Level
    //   0 - fast (cannot exceed the initial estart/lstart range)
    //   1 - slow (can push start/stop to earlier/later cycles)
    //   2 - even slower
    //   3 - 
    //    
    //   The amount of trials for each OP is SWP_Budget * Opt_Level
    //  
    Opt_Level = 2;

#ifdef TARG_IA64
    // Enable SWP of do-loop
    Enable_Do_Loop = TRUE;
#endif  
    // Enable SWP of while-loop
    //  - will be disabled in PU_Configure() if the architecture
    //    does not support while-loop SWP.
    //
    Enable_While_Loop = TRUE;

    // Enable SWP bundling and grouping of operations to minimize the number
    // of cycles in a modulo scheduled loop-kernel:
    //  - has no effect for architectures where bundling is not an issue.
    //
    Enable_Bundling = TRUE;

    // Budget
    Budget = 10;

    // Maximum changes of START/STOP cycles
    Max_Schedule_Incr = 20;
  
    // Max II limit and II increments for failed SWP
    Max_II_Alpha = 2;
    Max_II_Beta  = 2.0;

    // control II Incr amount
    II_Incr_Alpha = -10;
    II_Incr_Beta = 1.1;

    // Grainy Resources
    //   OPs that uses more than 'Grainy_Resources_Length' are considered
    //   difficult to schedule.  Therefore they are given priority to schedule.
    Grainy_Resources_Length = 10;

#ifdef TARG_IA64
    // Adjust Load_Latency for cache missed load
    Load_Cache_Miss_Ratio = 100;
    Load_Cache_Miss_Latency = 20;  // should be proc dependent!
#endif
    // default max unrolling == 8
    Min_Unroll_Times = 1;
    Max_Unroll_Times = (CG_opt_level > 2) ? 8 : 4;  

    // enable postincr form
    Enable_Post_Incr = TRUE;

    // For debugging - hardware/simulator workaround
    Enable_Workaround = FALSE;

    // For debugging - use Starting_II as the first II for scheduling
    Starting_II = 0;

    // if a resource is 90% utilized, it is 
    // considered critical
    Critical_Threshold = 90;

    // Execute the SWP preparation, but skip the modulo scheduler
    Prep_Only = FALSE;

    // Minimize retry / backtracking at the slight expense of
    // register presure
    Min_Retry = TRUE;

    // Use implicit prefetch (will be disabled if target doesn't support)
    Implicit_Prefetch = TRUE;
    
    // Predicate promotion
    Predicate_Promotion = TRUE;

    // Generation of branch predict instructions (brp.loop.imp)
    Enable_BRP = TRUE;

#ifdef TARG_IA64
    // Percentage of lower bound of feedback prob
    FB_Prob1 = 10; 	// prob = 0.1

    // Same as FB_Prob1, but used in combination with Freq
    FB_Prob2 = 50;	// prob = 0.5

    // Lower bound of feedback frequency
    FB_Freq = 100000;
#endif
    // Max number of OPs which should be SWPed
    OPS_Limit = SWP_OPS_LIMIT;
  }

  // PU Configure:
  //   Modify options based on target and loop information
  //
  void PU_Configure();

};


// SWP_Default is initialized by the default SWP_OPTION constructor
// and then modified by the command line options
//
extern SWP_OPTIONS SWP_Options;

#endif
