/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cflow.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:20-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cflow.h $
 *
 * Revision history:
 *  23-Jan-92 - Original Version
 *
 * Description:
 *
 *   External interface to the control flow transformations. Note
 *   that the interaces described in dominate.h and freq.h are considered
 *   to fall under the "cflow" umbrella.
 *
 * Variables:
 *
 *   BOOL CFLOW_Trace
 *	Enable all cflow tracing.
 *
 *   BOOL CFLOW_Trace_Detail
 *	Enable verbose tracing.
 *
 *   BOOL CFLOW_Trace_Unreach
 *	Enable tracing of unreachable basic block removal.
 *
 *   BOOL CFLOW_Trace_Branch
 *	Enable tracing of redundant branch removal.
 *
 *   BOOL CFLOW_Trace_Merge
 *	Enable tracing of basic block merging.
 *
 *   BOOL CFLOW_Trace_Reorder
 *	Enable tracing of basic block reordering.
 *
 *   BOOL CFLOW_Trace_Freq_Order
 *	Enable tracing of basic block frequency-guided ordering.
 *
 *   BOOL CFLOW_Trace_Freq
 *	Enable tracing of frequency estimates.
 *
 *   BOOL CFLOW_Trace_Clone
 *	Enable tracing of BB cloning.
 *
 *   BOOL CFLOW_Trace_Dom
 *	Enable tracing of BB dominator set calculation.
 *
 * Constants:
 *
 *   INT CFLOW_UNREACHABLE
 *   INT CFLOW_BRANCH
 *   INT CFLOW_MERGE
 *   INT CFLOW_REORDER
 *   INT CFLOW_FREQ_ORDER
 *   INT CFLOW_CLONE
 *	Enables a particular optimization.
 *
 *   INT CFLOW_ALL_OPTS
 *	Perform all optimizations (an OR of the above constants).
 *
 *   INT CFLOW_OPT_ALL_BR_TO_BCOND
 *	When performing branch optimizations, always convert a branch
 *	to a conditional branch, even if it expands code size.
 *
 *   INT CFLOW_FILL_DELAY_SLOTS
 *	Implies that the input has delay slots filled, and that cflow
 *	should maintain this property.
 *
 * Utilities:
 *
 *   void CFLOW_Initialize(void)
 *	Perform one-time initialization.
 *
 *   void CFLOW_Optimize(INT32 flags, const char *phase_name)
 *	Perform control flow based optimizations according to the
 *	<flags> mask. <phase_name> is a string which identifies
 *	this cflow pass -- it is only used in messages.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cflow_INCLUDED
#define cflow_INCLUDED

extern BOOL CFLOW_Trace;
extern BOOL CFLOW_Trace_Detail;
extern BOOL CFLOW_Trace_Unreach;
extern BOOL CFLOW_Trace_Branch;
extern BOOL CFLOW_Trace_Merge;
extern BOOL CFLOW_Trace_Reorder;
extern BOOL CFLOW_Trace_Clone;
extern BOOL CFLOW_Trace_Freq_Order;
extern BOOL CFLOW_Trace_Freq;
extern BOOL CFLOW_Trace_Dom;
#ifdef TARG_IA64
extern BOOL CFLOW_Trace_Empty_BB_Elim;
#endif

/* "flags" for CFLOW_Optimize:
 */
#define CFLOW_UNREACHABLE		(0x00000001)
#define CFLOW_BRANCH			(0x00000002)
#define CFLOW_MERGE			(0x00000004)
#define CFLOW_REORDER			(0x00000008)
#define CFLOW_CLONE			(0x00000010)
#define CFLOW_FREQ_ORDER		(0x00000020)
#define CFLOW_OPT_ALL_BR_TO_BCOND	(0x00000040)
#define CFLOW_FILL_DELAY_SLOTS		(0x00000080)
#define CFLOW_IN_CGPREP			(0x00000100)
#if defined(TARG_SL)
#define CFLOW_COLD_REGION		(0x00000200)
#define CFLOW_HOT_REGION		(0x00000400)
#define CFLOW_ALL_OPTS \
	(CFLOW_UNREACHABLE|CFLOW_BRANCH|CFLOW_MERGE|CFLOW_REORDER\
	|CFLOW_FREQ_ORDER|CFLOW_CLONE|CFLOW_COLD_REGION|CFLOW_HOT_REGION)
#else
//      This is a late unique opt, and so is excluded from all opts
#define CFLOW_BR_FUSE			(0x00000200)
#define CFLOW_ALL_OPTS \
	(CFLOW_UNREACHABLE|CFLOW_BRANCH|CFLOW_MERGE|CFLOW_REORDER\
	|CFLOW_FREQ_ORDER|CFLOW_CLONE)
#endif
extern void CFLOW_Optimize(INT32 flags, const char *phase_name);
extern void CFLOW_Initialize(void);

#ifdef TARG_IA64
extern void CFLOW_Delete_Empty_BB(void);
#endif

#if defined(TARG_MIPS) && !defined(TARG_SL)
extern void CFLOW_Fixup_Long_Branches(void);
#endif

#endif /* cflow_INCLUDED */
