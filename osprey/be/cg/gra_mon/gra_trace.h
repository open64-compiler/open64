/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

//  GRA_TRACE
/////////////////////////////////////
//
//  Description:
//
//      Tracing functions for GRA.
//
//  Exported functions:
//
//      Some of these functions take an initial argument <detail>.  When this
//      argument is present, it is used as a cutoff for information too
//      detailed to be generally useful.  Use <detail> == 0 for high level
//      things and 1 for internal debugging grunge.
//
//      void GRA_Trace_Initialize()
//          Call at the start of compilation.
//
//      void GRA_Trace( INT detail, const char *fmt, ... )
//          Trace to TFile the formated trace message derived by printf rules
//          from <fmt> and the following arguments.  Notice that this is
//          unconditional except for that <detail> bust be below the cutoff.
//
//      void GRA_Trace_Color_LRANGE( const char* str, LRANGE* lrange )
//          Trace the value of <str> and then a description of <lrange> if
//          coloring is being traced and the tracing is at high detail.
//
//      void GRA_Trace_Color_REGION( GRA_REGION* region )
//          Trace the start of coloring of a previously allocated <region>.
//
//      void GRA_Trace_Grant( GRA_BB* gbb, ISA_REGISTER_CLASS cl, REGISTER reg )
//          Trace a grant of the register <reg> in class <cl> to <gbb> if
//          grants are being traced.
//
//      void GRA_Trace_Split( INT detail, const char *fmt, ... )
//          Trace if splitting is being traced and detail is below the
//          cutoff.
//
//      void GRA_Trace_Color( INT detail, const char *fmt, ... )
//          Trace if coloring is being traced and detail is below the
//          cutoff.
//
//      void GRA_Trace_Place( INT detail, const char *fmt, ... )
//          Trace if placement is being traced and detail is below the
//          cutoff.
//
//      void GRA_Trace_Place_LRANGE_GBB( const char* str, const LRANGE* lrange,
//                                                        const GRA_BB* gbb )
//          Trace if high detail placement is being traced.  Print a
//          representation of the <lrange> and <gbb> following by the comment
//          <str>.
//
//      void GRA_Trace_Preference_Attempt( const LRANGE* lrange0,
//                                         const LRANGE* lrange1,
//                                         BOOL outcome )
//          An attempt has been made to allocate the same register to
//          <lrange0> as was allocated to <lrange1> and <outcome> is TRUE or
//          FALSE depending on whether it succeeded.  Write this to the trace
//          as appropriate.
//
//      void GRA_Trace_Preference_Copy( const LRANGE* lrange0,
//                                      const LRANGE* lrange1,
//                                      const GRA_BB* gbb )
//          A preference is established between <lrange0> and <lrange1>
//          because of a copy in <gbb>.  Write this to the trace as
//          appropriate.
//
//      void GRA_Trace_Preference_Conflict( const LRANGE* lrange0,
//                                      const LRANGE* lrange1,
//                                      const GRA_BB* gbb )
//	    Trace when conflict found between lranges in a preferencing
//	    copy in a block.
//
//      void GRA_Trace_Possible_Preference_Copy( const LRANGE* lrange0,
//                                              const LRANGE* lrange1,
//                                              const GRA_BB* gbb )
//	    Trace preferences found that may not be allowed later due to
//	    conflicts.
//
//	void GRA_Trace_Global_Preference_Failure( const LRANGE* lrange0,
//                                                const LRANGE* lrange1,
//                                                const GRA_BB* gbb )
//	    Preference failed due to conflict detected in gbb.
//
//	void GRA_Trace_Global_Preference_Success( const LRANGE* lrange0,
//                                                const LRANGE* lrange1)
//	    No conflict found between global TNs with a possible preference.
//
//      void GRA_Trace_Spill_Stats( float freq_restore_count,
//                                  INT   restore_count,
//                                  float freq_spill_count,
//                                  INT   spill_count,
//                                  float priority_count)
//          Trace the absolute number and the frequency weighted number of
//          spills and restores introduced by GRA in the current PU.
//
//      void GRA_Trace_LRANGE_Stats( INT32 complement_count,
//                                   INT32 region_count,
//                                   INT32 local_count,
//                                   INT32 split_count )
//          Trace the counts of the three different types of LRANGE.
//
//	void GRA_Trace_Regs_Stats( ISA_REGISTER_CLASS rc,
//				   REGISTER_SET regs_all, 
//			   	   REGISTER_SET &regs_used );
//          Trace how many regs used by GRA out of how many available regs
//
//      void GRA_Trace_Wired_Local_Count( INT32 count )
//          Trace the counts of wirted locals.
//
//      void GRA_Trace_Complement_LRANGE_Neighbors( LRANGE *lr )
//          Trace the neighbors of LRANGE (in terms of TN's)
//
//	BOOL GRA_Trace_Loops()
//	    Return TRUE if loop tracing requested.
//
//	void GRA_Trace_Home_Removal(OP *op)
//	    Print op's for homing loads/stores as they are removed 
//	    during spilling.
//
//	void GRA_Trace_Grant_Unused_Caller_Saved()
//	    Print message indicating that following grants are found by
//	    looking for unused caller saved registers in blocks.
//
//	void GRA_Trace_Split_Removing_Block(GRA_BB* gbb) 
//	    Print message when block removed from split
//
//	void GRA_Trace_Split_Add_Priority(GRA_BB* gbb, BOOL is_store) 
//	void GRA_Trace_Split_Sub_Priority(GRA_BB* gbb, BOOL is_store) 
//	    Print message when blocks add to or deduct from lrange priority
//
//	void GRA_Trace_Split_Priority_On(const char* msg)
//	    Turn on above splitting if tracing splits, and print msg
//	void GRA_Trace_Split_Priority_Off()
//	    Turn off above splitting
//
//	void GRA_Trace_LRANGE_Allocate(LRANGE* lrange)
//	    Trace allocation of <lrange>
//
/////////////////////////////////////


//  $Revision: 1.6 $
//  $Date: 05/12/05 08:59:10-08:00 $
//  $Author: bos@eng-24.pathscale.com $
//  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_trace.h $


#ifndef GRA_TRACE_INCLUDED
#define GRA_TRACE_INCLUDED
#ifndef GRA_TRACE_RCS_ID
#define GRA_TRACE_RCS_ID
#ifdef _KEEP_RCS_ID
static char *gra_trace_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/gra_mon/SCCS/s.gra_trace.h $ $Revision: 1.6 $";
#endif
#endif

#include <defs.h>
#include "op.h"
#include "tn.h"					   
#include "gra_lrange.h"
#include "gra_bb.h"
#include "gra_region.h"
#include "gra_lunit.h"
#include "register.h"

void GRA_Trace_Initialize(void);
void GRA_Trace( INT detail, const char *fmt, ... );
void GRA_Trace_Color_LRANGE( const char* str, LRANGE* lrange );
void GRA_Trace_Color_REGION( GRA_REGION* region );
void GRA_Trace_Grant( GRA_BB* gbb, ISA_REGISTER_CLASS cl, REGISTER reg );
void GRA_Trace_Split( INT detail, const char *fmt, ... );
void GRA_Trace_Color( INT detail, const char *fmt, ... );
void GRA_Trace_Place( INT detail, const char *fmt, ... );
void GRA_Trace_Place_LRANGE_GBB(const char* str, LRANGE* lrange, GRA_BB* gbb );
void GRA_Trace_Preference_Attempt( LRANGE* lrange0,
                                   LRANGE* lrange1,
				   GRA_REGION* region,
                                   BOOL outcome );
void GRA_Trace_Preference_Copy(LRANGE* lrange0,
                               LRANGE* lrange1,
                               GRA_BB* gbb );
void GRA_Trace_Possible_Preference_Copy(LRANGE* lrange0,
					LRANGE* lrange1,
					GRA_BB* gbb );
void GRA_Trace_Global_Preference_Failure(LRANGE* lrange0,
                                         LRANGE* lrange1,
                                         GRA_BB* gbb );
void GRA_Trace_Global_Preference_Success(LRANGE* lrange0,
                                         LRANGE* lrange1);
void GRA_Trace_Spill_Stats( float freq_restore_count, INT restore_count,
                            float freq_spill_count, INT spill_count,
			    float priority_count );
void GRA_Trace_LRANGE_Stats( INT32 complement_count,
                             INT32 region_count,
                             INT32 local_count,
                             INT32 split_count );
void GRA_Trace_Regs_Stats( ISA_REGISTER_CLASS rc,
			   REGISTER_SET regs_all, 
			   REGISTER_SET &regs_used );
void GRA_Trace_Wired_Local_Count( INT32 count );
void GRA_Trace_Complement_LRANGE_Neighbors( LRANGE* lr, GRA_REGION* region );
BOOL GRA_Trace_Loops();
void GRA_Trace_Homing(TN *tn, BB* bb);					   
void GRA_Trace_Home_Removal( TN* tn, GRA_BB *gbb, OP *op );
void GRA_Init_Trace_Memory();					   
void GRA_Trace_Memory(const char *string);
void GRA_Trace_Memory_Realloc(const char *string);
void GRA_Trace_Memory_Counts();
void GRA_Trace_Split_LUNIT_Spill(LUNIT *lunit);
BOOL GRA_Trace_Check_Splits();
void GRA_Trace_Grant_Unused_Caller_Saved();
void GRA_Trace_Split_Removing_Block(GRA_BB* gbb);
void GRA_Trace_Split_Add_Priority(GRA_BB* gbb, BOOL is_store);
void GRA_Trace_Split_Sub_Priority(GRA_BB* gbb, BOOL is_store);
#ifdef KEY
void GRA_Trace_Split_Reclaim_Add_Priority(GRA_BB* gbb, BOOL is_store,
					  float priority);
void GRA_Trace_Split_Reclaim_Sub_Priority(GRA_BB* gbb, BOOL is_store,
					  float priority);
#endif
void GRA_Trace_Split_Priority_On(const char* msg);
void GRA_Trace_Split_Priority_Off();
void GRA_Trace_Preference_Conflict(LRANGE* lrange0,
				   LRANGE* lrange1,
				   GRA_BB* gbb );
void GRA_Trace_LRANGE_Allocate(LRANGE* lrange);
#ifdef KEY
void GRA_Trace_LRANGE_Choose(LRANGE* lrange, REGISTER_SET allowed);
void GRA_Trace_LRANGE_Choose_Reclaimable(LRANGE* lrange, REGISTER_SET allowed);
#endif

#endif
