/*
 * Copyright (C) 2008 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo_special.h
 *  $Revision: 1.25 $
 *  $Date: 05/12/05 08:59:06-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.ebo_special.h $
 *
 *  Revision comments:
 *
 *  17-June-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  A couple of utility routines are included here because they require machine
 *  specific code.
 *
 *    EBO_Copy_Operand
 *         An index to the copied operand is returned, if the OP can be
 *         viewed as just moving bits from one location to another, and a -1,
 *         if not.
 *
 *    EBO_Can_Merge_Into_Offset
 *         If the OP is an add, subtract, or logical or with a small
 *         immediated constant, it may be possible to move the constant
 *         into the offset field of a memory operation and use the
 *         other operand as the index.
 *
 *  Special case optimizations are performed with the other routines.
 *  This includes simple things like transforming "x+0" into "x" and
 *  more complicated patterns such as "(x+1)+1" into "x+2".
 *
 *  As a gerneral philosophy, instructions that are redundant are not
 *  removed.  New instructions are inserted after them that redefine
 *  the result TN of the optimized instruction.  It is then left to
 *  the general dead-code eliminator to remove the unneeded definition.
 *
 *  These routines will return a TRUE value if the original OP has been
 *  optimized and can be deleted, and a FALSE value if the original value
 *  must be kept in the program.
 *
 *  delete_duplicate_op
 *	This routine handles the general case when a redundant operation
 *	is recognized.
 *
 *	The first argument is a pointer to the redundant
 *	OP, and the second operand is the EBO_OP_INFO entry for the
 *	preceeding matching entry.
 *
 *  delete_subset_mem_op
 *	This routine is called to delete a load instruction that has
 *	been matched with a store instruction.  This routine handles
 *	the case where the data being loaded is a subset of the data
 *	that was stored, thus requiring shifts and masking operations.
 *
 *	The first argument is a pointer to the redundant load OP,
 *	and the second operand is the EBO_OP_INFO entry for the
 *	preceeding matching store. The thrid argument is the offset of
 *	the memory addressed by the preceeding instruction and the fourth
 *	argument is the offset used in the redundantinstruction.
 *
 *  delete_reload_across_dependency
 *	This routine is called when the following sequence is found:
 *		store into location x
 *		store into location y
 *		load from location x (which could ansl be location y)
 *	The routine will attempt to generate a compare of the addreses
 *	x and y, and will then select the proper value to use to replace
 *	the load from x.
 *
 *  combine_adjacent_loads
 *	This routine merges two load instructions that reference adjacent
 *	memory locations into a single paired-load instruction.
 *
 *  Resolve_Conditional_Branch
 *	This routine attempts to determine which direction a conditional
 *	branch will go when the condition can be determined at
 *	compile time.
 *
 *  Fold_Constant_Expression
 *	This routine attempts to evaluate the instruction when
 *	all the operands have been determined to be constants.
 *
 *  Constant_Operand0
 *	This routine looks for special case optimizations when the
 *	first operand of the instruction is a constant.
 *
 *  Constant_Operand1
 *	This routine looks for special case optimizations when the
 *	second operand of the instruction is a constant.
 *
 *  Special_Sequence 
 *	This routine looks for special case optimizations involving
 *	a sequence of operations.
 *
 * =======================================================================
 * =======================================================================
 */

INT EBO_Copy_Operand (OP *op);

BOOL delete_duplicate_op (OP *op,
                          EBO_TN_INFO **opnd_tninfo,
                          EBO_OP_INFO *opinfo
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
                          , EBO_TN_INFO **actual_tninfo = NULL
#endif
			  );

BOOL combine_adjacent_loads(OP *op,
                            EBO_TN_INFO **opnd_tninfo,
                            EBO_OP_INFO *opinfo,
                            INT64 pred_offset,
                            INT64 succ_offset);

BOOL delete_subset_mem_op(OP *op,
                          EBO_TN_INFO **opnd_tninfo,
                          EBO_OP_INFO *opinfo,
                          INT64 pred_offset,
                          INT64 succ_offset);

BOOL delete_reload_across_dependency (OP *op,
                                 EBO_TN_INFO **opnd_tninfo,
                                 EBO_OP_INFO *opinfo,
                                 EBO_OP_INFO *intervening_opinfo);

BOOL Resolve_Conditional_Branch (OP *op, TN **opnd_tn);

BOOL Fold_Constant_Expression (OP *op,
                               TN **opnd_tn,
                               EBO_TN_INFO **opnd_tninfo);

BOOL Constant_Operand0 (OP *op,
                        TN **opnd_tn,
                        EBO_TN_INFO **opnd_tninfo);

BOOL Constant_Operand1 (OP *op,
                        TN **opnd_tn,
                        EBO_TN_INFO **opnd_tninfo);

BOOL Special_Sequence (OP *op,
                       TN **opnd_tn,
                       EBO_TN_INFO **opnd_tninfo);

#ifdef TARG_MIPS
void Redundancy_Elimination ();
#endif
#ifdef TARG_X8664

class LOOP_DESCR;

void Update_op_must_not_be_moved( OP*, EBO_TN_INFO** );
BOOL EBO_Merge_Memory_Addr( OP*, TN**, EBO_TN_INFO**, EBO_TN_INFO** );
BOOL EBO_Not_Load_Exec_Opnd( OP* );
BOOL EBO_Fold_Lea_Const_Component( OP* );
BOOL EBO_Opt_Const_Array( OP*, LOOP_DESCR*, INT );
BOOL EBO_Is_FMA4( TOP );
BOOL EBO_Is_FMA3( TOP );
BOOL EBO_Disassociate_FMA( OP* );
BOOL EBO_Associate_FMA( OP*, EBO_TN_INFO** );
BOOL EBO_Load_Execution( OP*, TN**, EBO_TN_INFO**, int );
BOOL EBO_Lea_Insertion( OP*, TN**, EBO_TN_INFO** );
BOOL EBO_Fold_Load_Duplicate( OP*, TN**, EBO_TN_INFO** );
void Lea_Insertion ();
void Init_Load_Exec_Map( BB*, MEM_POOL* );
BOOL Delete_Unwanted_Prefetches( OP* );
BOOL EBO_Can_Eliminate_Zero_Opnd_OP(OP *);
void Counter_Merge();
void Peel_Spills();
#endif /* TARG_X8664 */

BOOL EBO_Can_Merge_Into_Offset (OP *op);

#ifdef KEY
void EBO_Special_Start( MEM_POOL* );
void EBO_Special_Finish();
#if !defined(TARG_IA64) && !defined(TARG_SL)
BOOL Combine_L1_L2_Prefetches( OP*, TN**, EBO_TN_INFO** );
#endif
#endif // KEY
