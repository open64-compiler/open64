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
 *  Module: cg_loop_recur.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:21-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_loop_recur.h $
 *
 *  Revision history:
 *   03-Mar-94 - Original Version
 *
 *  Synopsis:
 *
 *      Try to reduce the impact of recurrences.
 *
 *
 *  Interface Description:
 *
 *    Exported functions:
 *
 *	BOOL CG_LOOP_Fix_Recurrences(BB *loop_body)
 *	  Requires: CG_LOOP_Make_Strongly_Connected_Components has been
 *		    called to create CG_LOOP_SCC_Vec[CG_LOOP_SCC_Count], and
 *		    CG_LOOP_Calculate_Max_Costs_And_Min_II has been called
 *		    to calculate CG_LOOP_min_ii and CG_LOOP_res_min_ii.
 *	  Attempt to reduce the impact of recurrences on the best possible
 *	  cyclical schedule length (minimum iteration interval), taking into
 *	  account the min II required to meet the resource requirements of
 *	  <loop_body>.  Outputs are possible code transformations, and a BOOL
 *	  telling whether the current SCCs and CG_LOOP_[res_]min_ii variables
 *	  are now out of date.
 *
 *	UINT32 CG_LOOP_Max_Recurrence_Cycles(BB *loop_body)
 *	  Requires: CG_LOOP_Make_Strongly_Connected_Components has been
 *		    called to create CG_LOOP_SCC_Vec[CG_LOOP_SCC_Count].
 *	  Assuming the effect of fixable recurrences in <loop_body>
 *	  can be mitigated later by CG_LOOP_Fix_Recurrences, find the
 *	  longest recurrence that should exist after fixing and return
 *	  its length in cycles.  This is necessarily an estimate in
 *	  some cases since we don't accurately predict the effect of
 *	  fixing some recurrences.
 *
 * ====================================================================
 * ==================================================================== */

#ifndef cg_loop_recur_INCLUDED
#define cg_loop_recur_INCLUDED

#ifdef _KEEP_RCS_ID
static const char cg_loop_recur_rcs_id[] = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_loop_recur.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

class CG_LOOP;

BOOL CG_LOOP_Fix_Recurrences(BB *loop_body);

UINT32 CG_LOOP_Max_Recurrence_Cycles(BB *loop_body);

extern void Fix_Recurrences_Before_Unrolling(CG_LOOP& cl);

extern void Fix_Recurrences_After_Unrolling(CG_LOOP& cl);

#endif
