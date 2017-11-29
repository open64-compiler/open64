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
 *  Module: cg_spill.h
 *  $Revision: 1.6 $
 *  $Date: 05/12/05 08:59:04-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_spill.h $
 *
 *  Description:
 *  ============
 *
 *  Reserved prefix: 
 *
 *	CGSPILL
 *
 *  Purpose:
 *
 *  	This module provides utility routines for allocating spill locations
 *	and generating spill code in the code generator. To allow reuse of 
 *	spill locations for local uses, it maintains a pool of local spill
 *	locations. 
 *
 *  Initialization:
 *
 *	CGSPILL_Initialize_For_PU (void)
 *	CGSPILL_Finalize_For_PU (void)
 *
 *	    Initializations/finalizations to be performed at the
 *	    start/end of each PU.
 *
 *	CGSPILL_Reset_Local_Spills (void)
 *
 *	    Reset the local spill locations so that all of them are marked
 *	    as being available. This should be called at the start of each
 *	    basic block in LRA and at the start of each loop in SWP.
 *
 *
 *  Access Functions:
 *
 *    The access routines provide the following functionality:
 *
 *  	- Creating spill location:
 *  	- Generate ops to load/store from a spill location:
 *  	- Add spill ops to a basic block:
 *
 *	The creation of a spill location is specific to the client. The
 *	following clients are recognized: 
 *
 *	      typedef enum {
 *		  CGSPILL_GRA,		Global Register Allocator
 *		  CGSPILL_LCL,  	TN Localization phase
 *		  CGSPILL_LRA,		Local Register Allocator
 *		  CGSPILL_SWP   	Software Pipeliner
 *	      } CGSPILL_CLIENT;
 *  
 *
 *	BOOL CGSPILL_TN_Can_Be_Spilled ( const TN* tn )
 *
 *          Do we know how to spill <tn>?  In not, it will cause an error to
 *          call CGSPILL_Get_TN_Spill_Location and pass <tn>.
 *
 *	ST *CGSPILL_Get_TN_Spill_Location (
 *	    TN *tn,
 *	    CGSPILL_CLIENT client)
 *
 *	    Returns a ST corresponding to the spill location for <tn>.
 *	    For CGSPILL_GRA, CGSPILL_LCL and CGSPILL_LRA kinds, it checks
 *	    if TN_spill(tn) is set and returns that. Otherwise, it 
 *	    allocates a new spill location. For CGSPILL_SWP, a new spill
 *	    is always returned since SWP maintains spill locations internally.
 *	    For LRA and SWP, a local spill location is allocated.
 *
 *	void CGSPILL_Cost_Estimate (
 *	    TN *tn, 
 *	    ST *mem_loc,
 *	    float *store_cost, 
 *	    float *restore_cost)
 *
 *	    Estimates the cost of spilling and restoring <tn> to or
 *	    from <mem_loc> (<mem_loc> can be NULL if a spill location
 *	    is not known). The estimates are returned in *store_cost 
 *	    and *restore_cost respectively.  What units do the costs 
 *	    represent?  This is a little tricky. Currently, they are 
 *	    instructions with a fraction added for memory references.
 *
 *	void CGSPILL_Load_From_Memory (
 *	    TN *tn, 
 *	    ST *mem_loc,
 *	    OPS *ops)
 *
 *	    Generates instructions to load <tn> from the memory location
 *	    <mem_loc>. The instructions generated are put into <ops>.
 *	    For rematerializable tns, the instruction sequence
 *	    generated reloads the constant value into <tn>.
 *
 *	void CGSPILL_Store_To_Memory (
 *	    TN *tn, 
 *	    ST *mem_loc,
 *	    OPS *ops)
 *
 *	    Generates instructions to store <tn> to the memory location
 *	    <mem_loc>. The instructions generated are put into <ops>.
 *	    For rematerializable tns, no instructions are added to <ops>.
 *
 *	void CGSPILL_Prepend_Ops (
 *	    BB *bb,
 *	    OPS *ops)
 *
 *	    Insert spill <ops> at the start of <bb>. If <bb> is an entry
 *	    block, the <ops> are inserted after the SP adjustment OP.
 *
 *	void CGSPILL_Insert_Ops_Before (
 *	    BB *bb,
 *	    OP *point,
 *	    OPS *ops)
 *
 *          Insert spill <ops> in <bb> before <point>. If <bb> is an exit
 *	    block and <point> is after the SP adjustment, the <ops> are
 *	    inserted before the SP adjustment OP. If <point> is a delay
 *	    slot instruction, the <ops> are inserted before the branch.
 *
 *	void CGSPILL_Append_Ops (
 *	    BB *bb,
 *	    OPS *ops)
 *
 *	    Insert spill <ops> at the end of <bb>. If <bb> is an exit
 *	    block, the <ops> are inserted just before the SP adjustment.
 *	    If <bb> is terminated with a branch or a call instruction,
 *	    the <ops> are inserted before the branch. The routine
 *	    takes care of the case where the delay slot instruction
 *	    might be defining a TN that is referenced in <ops>.
 *
 *	void CGSPILL_Insert_Ops_After (
 *	    BB *bb,
 *	    OP *point,
 *	    OPS *ops)
 *
 *          Insert spill <ops> in <bb> after <point>. If <bb> is an entry
 *	    basic block and <point> is before the SP adjustment OP, the 
 *	    <ops> are inserted after the SP adjustment OP.
 *
 *	void CGSPILL_Insert_Ops_After_Last_Def (
 *	    TN *tn,
 *	    BB *bb,
 *	    OPS *ops)
 *	    Insert spill ops after the last def of tn in bb.
 *
 *	void CGSPILL_Insert_Ops_Before_First_Use (
 *	    TN *tn,
 *	    BB *bb,
 *	    OPS *ops)
 *	    Insert spill ops before the first use of tn in bb.
 *
 *	void CGSPILL_Force_Rematerialization ( void )
 *	void CGSPILL_Force_Rematerialization_For_BB ( BB *bb )
 *	    Force rematerialization of each use of a rematerializable
 *	    TN at each use, for debugging.
 *
 *	BOOL CGSPILL_Is_Spill_Location (ST *mem_loc)
 *	    Return a boolean that indicates if the specified symbol
 *	    is a spill location.
 *
 *	BOOL CGSPILL_Is_Spill_Op (OP *op)
 *	    Return a boolean that indicates if the specified OP is
 *	    a spill or restore operation.
 *
 *	ST *CGSPILL_OP_Spill_Location (OP *op)
 *	    If <op> is a spill or restore, return the symbol for
 *	    the associated spill location.
 *
 *	void CGSPILL_Attach_Lda_Remat(
 *	    TN *tn, 
 *	    TYPE_ID typ, 
 *	    INT64 offset, 
 *	    ST *st)
 *	void CGSPILL_Attach_Intconst_Remat(TN *tn, INT64 val)
 *	void CGSPILL_Attach_Floatconst_Remat(TN *tn, TYPE_ID typ, double val)
 *	void CGSPILL_Attach_Const_Remat(TN *tn, TYPE_ID typ, ST *st)
 *	    Make <tn> rematerializable and attatch the appropriate
 *	    info (whirl) to be able to rematerialize it. The routines
 *	    are to be used when creating new OPs after the whirl2ops
 *	    phase.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef cgspill_INCLUDED
#define cgspill_INCLUDED

typedef enum {
  CGSPILL_GRA,
  CGSPILL_LCL,
  CGSPILL_LRA,
  CGSPILL_SWP,
  CGSPILL_LGRA
} CGSPILL_CLIENT;

#define CGSPILL_DEFAULT_STORE_COST 1.25F
#define CGSPILL_DEFAULT_RESTORE_COST 1.25F

extern void CGSPILL_Initialize_For_PU (void);
extern void CGSPILL_Finalize_For_PU (void);
extern void CGSPILL_Reset_Local_Spills (void);
extern BOOL CGSPILL_TN_Can_Be_Spilled ( const TN* tn );
extern ST * CGSPILL_Get_TN_Spill_Location (TN *tn, CGSPILL_CLIENT client);
extern BOOL CGSPILL_Is_Spill_Location (ST *mem_loc);
extern ST * CGSPILL_OP_Spill_Location (OP *op);
inline BOOL CGSPILL_Is_Spill_Op (OP *op)
{
  return CGSPILL_OP_Spill_Location(op) != (ST *)0;
}
extern void CGSPILL_Cost_Estimate (TN *tn, ST *mem_loc, float *store_cost,
				   float *restore_cost, CGSPILL_CLIENT client);
extern void CGSPILL_Load_From_Memory (TN *tn, ST *mem_loc, OPS *ops,
				      CGSPILL_CLIENT client, BB *bb);
extern void CGSPILL_Store_To_Memory (TN *tn, ST *mem_loc, OPS *ops,
				     CGSPILL_CLIENT client, BB *bb);
extern void CGSPILL_Prepend_Ops (BB *bb, OPS *ops);
extern void CGSPILL_Insert_Ops_Before (BB *bb, OP *point, OPS *ops);
extern void CGSPILL_Append_Ops (BB *bb, OPS *ops);
extern void CGSPILL_Insert_Ops_After (BB *bb, OP *point, OPS *ops);
extern void CGSPILL_Insert_Ops_After_Last_Def (TN *tn, BB *bb, OPS *ops);
extern void CGSPILL_Insert_Ops_Before_First_Use (TN *tn, BB *bb, OPS *ops);
extern void CGSPILL_Force_Rematerialization (void);
extern void CGSPILL_Force_Rematerialization_For_BB (BB *bb);

extern void CGSPILL_Attach_Lda_Remat(TN *tn, TYPE_ID typ, INT64 offset, ST *st);
extern void CGSPILL_Attach_Intconst_Remat(TN *tn, INT64 val);
extern void CGSPILL_Attach_Floatconst_Remat(TN *tn, TYPE_ID typ, double val);
extern void CGSPILL_Attach_Const_Remat(TN *tn, TYPE_ID typ, ST *st);

#ifdef TARG_IA64
extern void st_2_st_spill(OPS *ops, BOOL force);
extern void ld_2_ld_fill(OPS *ops, BOOL force);
#endif

#ifdef KEY
extern void CGSPILL_Inc_Restore_Count (ST *spill_loc);

// Keep track of the spills and restores for a spill symbol, so that EBO knows
// which spills are dead after it deletes restores.  Only need to handle the
// case single spill OP because EBO can't handle multiple stores.
class SPILL_SYM_INFO {
private:
  INT32 spill_count;
  INT32 restore_count;
  OP *spill_op;		// Valid only if spill_count is 1.
  INT32 used_by_load_exe : 1;
public:
  SPILL_SYM_INFO() {
    spill_count = 0;
    restore_count = 0;
    spill_op = NULL;
    used_by_load_exe = 0;
  }
  INT32 Spill_Count() { return spill_count; }
  void Inc_Spill_Count() { spill_count++; }
  INT32 Restore_Count() { return restore_count; }
  void Inc_Restore_Count() { restore_count++; }
  OP *Spill_Op() { return spill_op; }
  void Set_Spill_Op(OP *op) { spill_op = op; }
  BOOL Used_By_Load_Exe() { return used_by_load_exe ? TRUE : FALSE; }
  void Set_Used_By_Load_Exe() { used_by_load_exe = 1; }
};

// Map ST to spill sym info.
typedef hash_map<ST_IDX,
		 SPILL_SYM_INFO,
		 __gnu_cxx::hash<ST_IDX>,
		 std::equal_to<ST_IDX> > SPILL_SYM_INFO_MAP;

SPILL_SYM_INFO &CGSPILL_Get_Spill_Sym_Info (ST *spill_loc);
#endif

#endif /* cgspill_INCLUDED */
