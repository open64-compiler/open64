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
 * Module: timing.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:39-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.timing.cxx $
 *
 * Revision history:
 *  06-Feb-90 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  03-Feb-93 - Add enable flag to avoid system calls when not tracing.
 *
 * Description:
 *
 * This module provides phase/function timing for the Muse compiler.
 * It is based on the program-independent functionality provided by
 * resource.c (via resource.h).
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "resource.h"
#include "timing.h"
#include "tracing.h"
#include "resource.h"

/* If this flag is FALSE, don't do anything: */
static BOOL Enabled = FALSE;

/* Array of timing accumulators: */
static RSTATE *timers[T_LAST+1];
#define Timer(i)	timers[i]

/* Count of compilation units, so we can avoid doing a summary report
 * for a compilation consisting of a single unit:
 */
static INT CU_Count = 0;

/* ====================================================================
 *
 * Reset_Timers
 *
 * Reset all of the timers for a new source file.
 *
 * ====================================================================
 */

void
Reset_Timers (void)
{
  INT i;

  if ( Enabled ) {
    for ( i=0; i <= T_LAST; i++ ) Resource_Accum (Timer(i), RR_Clear);
  }
}

/* ====================================================================
 *
 * Initialize_Timing
 *
 * Initialize the timing structures used by the Muse compiler.  In
 * particular, initialize the resource module (with its overall timer),
 * and allocate the timing accumulators used by the compiler.
 *
 * ====================================================================
 */

void
Initialize_Timing ( BOOL enable )
{

  if ( Enabled = enable ) {

    /* Initialize the resource accounting module: */
    Resource_Init ();

    /* Allocate the timers: */
    Timer ( T_BE_Comp ) =
	    Resource_Alloc ( "Total Back End", NULL );
    Timer ( T_BE_PU_Comp ) =
	    Resource_Alloc ( "Back End on PU", NULL );
    Timer ( T_BE_PU_CU ) =
	    Resource_Alloc ( "Back End on PU", Timer(T_BE_PU_Comp) );
    Timer ( T_ReadIR_Comp ) =
	    Resource_Alloc ( "  Reading IR", NULL );
    Timer ( T_ReadIR_CU ) =
	    Resource_Alloc ( "  Reading IR", Timer(T_ReadIR_Comp) );
    Timer ( T_Lower_Comp ) =
	    Resource_Alloc ( "  Lowering WHIRL", NULL );
    Timer ( T_Lower_CU ) =
	    Resource_Alloc ( "  Lowering WHIRL", Timer(T_Lower_Comp) );
    Timer ( T_ORI_Comp ) =
	    Resource_Alloc ( "  Olimit Region Insertion", NULL );
    Timer ( T_ORI_CU ) =
	    Resource_Alloc ( "  Olimit Region Insertion", Timer(T_ORI_Comp) );

    Timer ( T_Preopt_Comp ) =
	    Resource_Alloc ( "Pre-optimize", NULL );
    Timer ( T_Preopt_CU ) =
	    Resource_Alloc ( "Pre-optimize", Timer(T_Preopt_Comp) );
    Timer ( T_Wopt_Comp ) =
	    Resource_Alloc ( "Global optimize", NULL );
    Timer ( T_Wopt_CU ) =
	    Resource_Alloc ( "Global optimize", Timer(T_Wopt_Comp) );
    Timer ( T_LNO_Comp ) =
	    Resource_Alloc ( "Loop Nest Optimization", NULL );
    Timer ( T_LNO_CU ) =
	    Resource_Alloc ( "Loop Nest Optimization", Timer(T_LNO_Comp) );

    Timer ( T_W2C_Comp ) =
	    Resource_Alloc ( "WHIRL To C", NULL );
    Timer ( T_W2C_CU ) =
	    Resource_Alloc ( "WHIRL To C", Timer(T_W2C_Comp) );
    Timer ( T_W2F_Comp ) =
	    Resource_Alloc ( "WHIRL To Fortran", NULL );
    Timer ( T_W2F_CU ) =
	    Resource_Alloc ( "WHIRL To Fortran", Timer(T_W2F_Comp) );

    Timer ( T_CodeGen_Comp ) =
	    Resource_Alloc ( "Total Code Generator", NULL );
    Timer ( T_CodeGen_CU ) =
	    Resource_Alloc ( "Total Code Generator", Timer(T_CodeGen_Comp) );

    Timer ( T_GLRA_Comp ) =
	    Resource_Alloc ( "  Global Live Range Analysis", NULL );
    Timer ( T_GLRA_CU ) =
	    Resource_Alloc ( "  Global Live Range Analysis", Timer(T_GLRA_Comp) );
    Timer ( T_Localize_Comp ) =
	    Resource_Alloc ( "  Localize", NULL );
    Timer ( T_Localize_CU ) =
	    Resource_Alloc ( "  Localize", Timer(T_Localize_Comp) );
    Timer ( T_Expand_Comp ) =
	    Resource_Alloc ( "  Code Expansion", NULL );
    Timer ( T_Expand_CU ) =
	    Resource_Alloc ( "  Code Expansion", Timer(T_Expand_Comp) );
    Timer ( T_SWpipe_Comp ) =
	    Resource_Alloc ( "  Software pipelining", NULL );
    Timer ( T_SWpipe_CU ) =
	    Resource_Alloc ( "  Software pipelining", Timer(T_SWpipe_Comp) );
    Timer ( T_GCM_Comp ) =
	    Resource_Alloc ( "  Global Code Motion", NULL );
    Timer ( T_GCM_CU ) =
	    Resource_Alloc ( "  Global Code Motion", Timer(T_GCM_Comp) );
    Timer ( T_EBO_Comp ) =
	    Resource_Alloc ( "  Extended Block Optimization", NULL );
    Timer ( T_EBO_CU ) =
	    Resource_Alloc ( "  Extended BLock Optimization", Timer(T_EBO_Comp) );
    Timer ( T_CFLOW_Comp ) =
	    Resource_Alloc ( "  Control Flow Optimization", NULL );
    Timer ( T_CFLOW_CU ) =
	    Resource_Alloc ( "  Control Flow Optimization", Timer(T_CFLOW_Comp) );
    Timer ( T_Loop_Comp ) =
	    Resource_Alloc ( "  CG Loop", NULL );
    Timer ( T_Loop_CU ) =
	    Resource_Alloc ( "  CG Loop", Timer(T_Loop_Comp) );
    Timer ( T_Freq_Comp ) =
	    Resource_Alloc ( "  Compute Frequency", NULL );
    Timer ( T_Freq_CU ) =
	    Resource_Alloc ( "  Compute Frequency", Timer(T_Freq_Comp) );
    Timer ( T_HBF_Comp ) =
	    Resource_Alloc ( "  HyperBlock Formation", NULL );
    Timer ( T_HBF_CU ) =
	    Resource_Alloc ( "  HyperBlock Formation", Timer(T_HBF_Comp) );
    Timer ( T_Sched_Comp ) =
	    Resource_Alloc ( "  HyperBlock Scheduling", NULL );
    Timer ( T_Sched_CU ) =
	    Resource_Alloc ( "  HyperBlock Scheduling", Timer(T_Sched_Comp) );
    Timer ( T_THR_Comp ) =
	    Resource_Alloc ( "  Tree-Height Reduction", NULL );
    Timer ( T_THR_CU ) =
	    Resource_Alloc ( "  Tree-Height Reduction", Timer(T_THR_Comp) );
    Timer ( T_LRA_Comp ) =
	    Resource_Alloc ( "  Local Register Allocation", NULL );
    Timer ( T_LRA_CU ) =
	    Resource_Alloc ( "  Local Register Allocation", Timer(T_LRA_Comp));
    Timer ( T_GRA_Comp ) =
	    Resource_Alloc ( "  Global Register Allocation", NULL );
    Timer ( T_GRA_CU ) =
	    Resource_Alloc ( "  Global Register Allocation", Timer(T_GRA_Comp));
    Timer ( T_Emit_Comp ) =
	    Resource_Alloc ( "  Assembly Code Emission", NULL );
    Timer ( T_Emit_CU ) =
	    Resource_Alloc ( "  Assembly Code Emission", Timer(T_Emit_Comp) );
    Timer ( T_Region_Finalize_Comp ) =
	    Resource_Alloc ( "  Region Finalize", NULL );
    Timer ( T_Region_Finalize_CU ) =
	    Resource_Alloc ( " Region Finalize", Timer(T_Region_Finalize_Comp));
    Timer ( T_CalcDom_Comp) =
	    Resource_Alloc ( "   Calculate_Dominators", NULL );
    Timer ( T_CalcDom_CU) =
	    Resource_Alloc ( "  Calculate_Dominators", Timer(T_CalcDom_Comp));
    Timer ( T_WSSA_EMIT_Comp) =
	    Resource_Alloc ( "   WHIRL SSA PreOPT Emitter", NULL );
    Timer ( T_WSSA_EMIT_CU) =
	    Resource_Alloc ( "  WHIRL SSA PreOPT Emitter", Timer(T_WSSA_EMIT_Comp));

  }
}

/* ====================================================================
 *
 * Clear_Timer / Start_Timer / Stop_Timer
 *
 * Clear, start, or stop a timing accumulator.
 *
 * ====================================================================
 */

#ifndef MONGOOSE_BE
void
Clear_Timer ( INT Timer_ID )
{
  if ( Enabled ) {
    Resource_Accum ( Timer(Timer_ID), RR_Clear );
  }
}
#endif /* MONGOOSE_BE */

void
Start_Timer ( INT Timer_ID )
{
  if ( Enabled ) {
    Resource_Accum ( Timer(Timer_ID), RR_Start );
  }
}

void
Stop_Timer ( INT Timer_ID )
{
  if ( Enabled ) {
    Resource_Accum ( Timer(Timer_ID), RR_Stop );
  }
}

double Get_User_Time(INT Timer_ID)
{
  if (Enabled) {
    RSTATE *r;
    // Call Resource_Accum to update the RSTATE data structure
    Resource_Accum ( Timer(Timer_ID), RR_Stop );
    Resource_Accum ( Timer(Timer_ID), RR_Start );
    r = Timer(Timer_ID);
    TIME_INFO *utime = Get_Time ( r, RR_Delta_User );
    return utime->secs + 0.000001 * utime->usecs;
  }
  return 0.0;
}

/* ====================================================================
 *
 * Add_Timer_To_Parent
 *
 * Add a timer's accumulated time to its parent and clear.
 *
 * ====================================================================
 */

static void
Add_Timer_To_Parent ( INT Timer_ID )
{
  if ( Enabled ) {
    Resource_Accum ( Timer(Timer_ID), RR_End );
  }
}


/* ====================================================================
 *
 * Report_Delta_Time
 *
 * Report the delta times for a resource, in the format:
 *	name			nnn.nnnu nnn.nnns nnn.nne
 *
 * ====================================================================
 */

void
Report_Delta_Time (
  FILE *file,
  INT Timer_ID )
{
  const char *name;
  TIME_INFO *utime, *stime, *etime;
  RSTATE *r = Timer(Timer_ID);
  INT mem;

  if ( Enabled ) {
    name = Get_Timer_Name (r);
    utime = Get_Time ( r, RR_Delta_User );
    stime = Get_Time ( r, RR_Delta_System );
    etime = Get_Time ( r, RR_Delta_Elapsed );
    mem = Get_Memory ( r, RR_Delta_Memory );

#if (1)
    fprintf ( file, "%-32s  %4d.%06du  %4d.%06ds  %4d.%06de",
	      name,
	      utime->secs, utime->usecs,
	      stime->secs, stime->usecs,
	      etime->secs, etime->usecs );
#else
    fprintf ( file, "%-32s  %4d.%03du  %4d.%03ds  %4d.%02de",
	      name,
	      utime->secs, utime->usecs/1000,
	      stime->secs, stime->usecs/1000,
	      etime->secs, etime->usecs/10000 );
#endif
    if ( mem ) fprintf ( file, "  %7dm", mem );
    fprintf ( file, "\n" );
  }
}

void
Report_CG_Region_Timing (FILE *file, char *name)
{
  if ( ! Enabled ) return;
  if ( file == NULL || Get_Trace ( TKIND_INFO, TINFO_CTIME ) ) return;

  fprintf ( file, "%s%s: CG Timing Report:\n\n", DBar, name);
  Report_Delta_Time ( file, T_CodeGen_CU );
  Report_Delta_Time ( file, T_Expand_CU );
  Report_Delta_Time ( file, T_Localize_CU );
  Report_Delta_Time ( file, T_GLRA_CU );
  Report_Delta_Time ( file, T_EBO_CU );
  Report_Delta_Time ( file, T_CFLOW_CU );
  Report_Delta_Time ( file, T_Loop_CU );
  Report_Delta_Time ( file, T_CalcDom_CU );
  Report_Delta_Time ( file, T_SWpipe_CU );
  Report_Delta_Time ( file, T_Freq_CU );
  Report_Delta_Time ( file, T_GRA_CU );
  Report_Delta_Time ( file, T_LRA_CU );
  Report_Delta_Time ( file, T_HBF_CU );
  Report_Delta_Time ( file, T_Sched_CU );
  Report_Delta_Time ( file, T_THR_CU );
  Report_Delta_Time ( file, T_GCM_CU );
  Report_Delta_Time ( file, T_Region_Finalize_CU );
  fprintf ( file, "%s\n", DBar );
}

/* ====================================================================
 *
 * Finish_BE_Timing
 *
 * Accumulate the compilation unit timers into their parent
 * accumulators and report the results if file is not NULL.
 *
 * ====================================================================
 */
void
Finish_BE_Timing (
  FILE *file,
  char *name )
{
  if ( Enabled ) {
    /* Increment the compilation unit count: */
    ++CU_Count;

    /* Report if requested: */
    if ( file != NULL && ! Get_Trace ( TKIND_INFO, TINFO_CTIME ) )  {
	fprintf ( file,
		  "%s%s (#%d): Back End Timing Report:\n\n",
		  DBar, name, CU_Count );

	Report_Delta_Time ( file, T_BE_PU_CU );
	Report_Delta_Time ( file, T_ReadIR_CU );
	Report_Delta_Time ( file, T_Lower_CU );
	Report_Delta_Time ( file, T_ORI_CU );

	fprintf(file, "\n");
	Report_Delta_Time ( file, T_Preopt_CU );
	Report_Delta_Time ( file, T_WSSA_EMIT_CU );

	Report_Delta_Time ( file, T_LNO_CU );
	Report_Delta_Time ( file, T_Wopt_CU );
	Report_Delta_Time ( file, T_W2C_CU );
	Report_Delta_Time ( file, T_W2F_CU );
	
	fprintf(file, "\n");
	Report_Delta_Time ( file, T_CodeGen_CU );
	Report_Delta_Time ( file, T_Expand_CU );
	Report_Delta_Time ( file, T_Localize_CU );
	Report_Delta_Time ( file, T_GLRA_CU );
	Report_Delta_Time ( file, T_EBO_CU );
	Report_Delta_Time ( file, T_CFLOW_CU );
	Report_Delta_Time ( file, T_Loop_CU );
	Report_Delta_Time ( file, T_CalcDom_CU);
	Report_Delta_Time ( file, T_SWpipe_CU );
	Report_Delta_Time ( file, T_Freq_CU );
	Report_Delta_Time ( file, T_GRA_CU );
	Report_Delta_Time ( file, T_LRA_CU );
	Report_Delta_Time ( file, T_HBF_CU );
	Report_Delta_Time ( file, T_Sched_CU );
	Report_Delta_Time ( file, T_THR_CU );
	Report_Delta_Time ( file, T_GCM_CU );
	Report_Delta_Time ( file, T_Emit_CU );
	Report_Delta_Time ( file, T_Region_Finalize_CU );
	fprintf ( file, "%s\n", DBar );
    }
    
    /* Add the per-CU times to the per-compilation times: */
    Add_Timer_To_Parent ( T_BE_PU_CU );
    Add_Timer_To_Parent ( T_ReadIR_CU );
    Add_Timer_To_Parent ( T_Lower_CU );
    Add_Timer_To_Parent ( T_ORI_CU );

    Add_Timer_To_Parent ( T_Preopt_CU );
    Add_Timer_To_Parent ( T_WSSA_EMIT_CU );
    Add_Timer_To_Parent ( T_LNO_CU );
    Add_Timer_To_Parent ( T_Wopt_CU );

    Add_Timer_To_Parent ( T_W2C_CU );
    Add_Timer_To_Parent ( T_W2F_CU );
    
    Add_Timer_To_Parent ( T_CodeGen_CU );
    Add_Timer_To_Parent ( T_Expand_CU );
    Add_Timer_To_Parent ( T_Localize_CU );
    Add_Timer_To_Parent ( T_GLRA_CU );
    Add_Timer_To_Parent ( T_EBO_CU );
    Add_Timer_To_Parent ( T_CFLOW_CU );
    Add_Timer_To_Parent ( T_Loop_CU );
    Add_Timer_To_Parent ( T_CalcDom_CU );
    Add_Timer_To_Parent ( T_SWpipe_CU );
    Add_Timer_To_Parent ( T_Freq_CU );
    Add_Timer_To_Parent ( T_GRA_CU );
    Add_Timer_To_Parent ( T_LRA_CU );
    Add_Timer_To_Parent ( T_HBF_CU );
    Add_Timer_To_Parent ( T_Sched_CU );
    Add_Timer_To_Parent ( T_THR_CU );
    Add_Timer_To_Parent ( T_GCM_CU );
    Add_Timer_To_Parent ( T_Emit_CU );
    Add_Timer_To_Parent ( T_Region_Finalize_CU );
  }
}

/* ====================================================================
 *
 * Finish_Compilation_Timing
 *
 * Report the compilation timings if file is not NULL.
 *
 * ====================================================================
 */

void
Finish_Compilation_Timing (
  FILE *file,
  char *source )
{
  if ( Enabled ) {
    /* Report if requested: */
    if ( file != NULL ) {
	fprintf ( file, "%s\n%s: Compilation Timing Report\n",
		  DBar, source );

	fprintf ( file, "\n" );
	Report_Delta_Time ( file, T_BE_Comp );
	fprintf ( file, "\n" );
	Report_Delta_Time ( file, T_BE_PU_Comp );
	Report_Delta_Time ( file, T_ReadIR_Comp );
	Report_Delta_Time ( file, T_Lower_Comp );
	Report_Delta_Time ( file, T_ORI_Comp );

	fprintf ( file, "\n" );
	Report_Delta_Time ( file, T_Preopt_Comp );
	Report_Delta_Time ( file, T_WSSA_EMIT_Comp );
	Report_Delta_Time ( file, T_LNO_Comp );
	Report_Delta_Time ( file, T_Wopt_Comp );
	Report_Delta_Time ( file, T_W2C_Comp );
	Report_Delta_Time ( file, T_W2F_Comp );
	
	fprintf ( file, "\n" );
	Report_Delta_Time ( file, T_CodeGen_Comp );
	Report_Delta_Time ( file, T_Expand_Comp );
	Report_Delta_Time ( file, T_Localize_Comp );
	Report_Delta_Time ( file, T_GLRA_Comp );
	Report_Delta_Time ( file, T_EBO_Comp );
	Report_Delta_Time ( file, T_CFLOW_Comp );
	Report_Delta_Time ( file, T_Loop_Comp );
	Report_Delta_Time ( file, T_CalcDom_Comp );
	Report_Delta_Time ( file, T_SWpipe_Comp );
	Report_Delta_Time ( file, T_Freq_Comp );
	Report_Delta_Time ( file, T_GRA_Comp );
	Report_Delta_Time ( file, T_LRA_Comp );
	Report_Delta_Time ( file, T_HBF_Comp );
	Report_Delta_Time ( file, T_Sched_Comp );
	Report_Delta_Time ( file, T_THR_Comp );
	Report_Delta_Time ( file, T_GCM_Comp );
	Report_Delta_Time ( file, T_Emit_Comp );
	Report_Delta_Time ( file, T_Region_Finalize_Comp );
	fprintf ( file, "%s\n", DBar );
    }
  }
}
