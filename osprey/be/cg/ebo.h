/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo.h
 *  $Revision: 1.7 $
 *  $Date: 05/12/05 08:59:06-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.ebo.h $
 *
 *  Revision comments:
 *
 *  29-May-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Extended Block Optimizer module.
 *
 *  Quick summary of what this module  provides:
 *	- recognize an extended block sequence.
 *	- perform simple peephole types of optimizations on the
 *	  instructions in the sequence.
 *  general optimizations:
 *	- forward propagation of constants
 *	- redundant expression elimination
 *	- dead expression elimination
 *
 *  Interface:
 *
 *	void EBO_Init()
 *	  Initialization routine that should be called at the start
 *	  of each invocation of CG.
 *
 *	void EBO_Pre_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform prior to scheduling the region specified
 *	  by <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_before_unrolling(BB_REGION *bbr)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Requires: all OPs in the region have omega information available.
 *	  Analyze and transform before unrolling and piplining a region
 *	  that is specified by the block lists that are provided.  A single
 *	  entry block is assumed; the exit block list is intended to be a
 *	  list of the exit TARGET blocks from the region.  The exit list
 *	  of blocks will NOT be processed.
 *
 *	void EBO_after_unrolling(BB_REGION *bbr)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform during unrolling and piplining a region
 *	  that is specified by the block lists that are provided.  A single
 *	  entry block is assumed; the exit block list is intended to be a
 *	  list of the exit TARGET blocks from the region.  The exit list
 *	  of blocks will NOT be processed.
 *
 *	void EBO_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform for scheduling the region specified by
 *	  <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_Post_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Apply peephole optimizations after all register allocation
 *	  and before the last scheduling pass on the region specified
 *	  by <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_Finalize()
 *	  Termination routine that should be called at the end
 *	  of each invocation of CG.
 *
 *      INT32 EBO_Opt_Level
 *        This flag is used to control use of the EBO routines until
 *        final debugging can be completed.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef EBO_INCLUDED
#define EBO_INCLUDED

class LOOP_DESCR;

void EBO_Init(void);

void EBO_Pre_Process_Region(RID *rid);

void EBO_before_unrolling(BB_REGION *bbr);

void EBO_after_unrolling(BB_REGION *bbr, LOOP_DESCR *loop, INT loop_iter_size);

void EBO_Process_Region(RID *rid);

void EBO_Post_Process_Region(RID *rid);

#ifdef KEY
void EBO_Post_Process_Region_2(RID *rid);
#endif

void EBO_Compute_To( BB *bb );

void EBO_Finalize(void);

#ifdef TARG_IA64
extern BOOL OP_ld_st_unat(OP *op);
#endif
extern INT32 EBO_Opt_Level_Default;
extern INT32 EBO_Opt_Level;
#ifdef KEY
extern BOOL EBO_can_delete_branch_delay_OP;
extern BOOL EBO_no_liveness_info_available;
extern INT32 EBO_Opt_Mask;
#define EBO_CAN_MERGE_INTO_OFFSET 	0x1
#define EBO_COMBINE_L1_L2_PREFETCH  	0x2
#define EBO_DELETE_SUBSET_MEM_OP  	0x4
#define EBO_DELETE_MEMORY_OP      	0x8
#define EBO_DELETE_DUPLICATE_OP   	0x10
#define EBO_CONVERT_IMM_ADD       	0x20
#define EBO_CONVERT_OPERAND0      	0x40
#define EBO_CONVERT_IMM_AND       	0x80
#define EBO_CONVERT_IMM_OR        	0x100
#define EBO_CONVERT_IMM_XOR       	0x200
#define EBO_CONVERT_IMM_CMP       	0x400
#define EBO_CONVERT_IMM_MUL       	0x800
#define EBO_CONSTANT_OPERAND1     	0x1000
#define EBO_RESOLVE_CONDITIONAL_BRANCH	0x2000
#define EBO_FOLD_CONSTANT_EXPRESSION	0x4000
#define EBO_DELETE_UNWANTED_PREFETCHES	0x8000
#define EBO_SPECIAL_SEQUENCE		0x10000
#define EBO_COMPOSE_ADDR		0x20000
#define EBO_MERGE_MEMORY_ADDR		0x40000
#define EBO_CHECK_LOADBW_EXECUTE	0x80000
#define EBO_TEST_IS_REPLACED		0x100000
#define EBO_LEA_INSERTION		0x200000
#define EBO_MOVE_EXT_IS_REPLACED        0x400000
#define EBO_REDUNDANCY_ELIMINATION      0x800000
#define EBO_LOAD_EXECUTION              0x1000000
#define EBO_CONSTANT_OPERAND0     	0x2000000
#define EBO_FOLD_LOAD_DUPLICATE     	0x4000000
#endif
extern BOOL  CG_skip_local_ebo;
extern bool Op_has_side_effect(OP *op);

#endif /* EBO_INCLUDED */
