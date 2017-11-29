/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 *  Module: cg_swp_target.cxx
 *  $Revision: 1.8 $
 *  $Date: 05/12/05 08:59:04-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_swp_target.h $
 *
 *  Description:
 *  ============
 *
 *  SWP Target Specific 
 *
 *  Here are various miscellaneous functions to provide machine dependent
 *  information.
 *
 *  void SWP_Exp_COPY(result, opnd, OPS *ops)
 *    Generate a copy from opnd to result.   Different from Exp_COPY(),
 *    it supports copy of predicates.
 *
 *  void SWP_Loop_Init_Fini(bool is_doloop,
 *		  	    TN *trip_count_tn,
 *			    INT stage_count,
 *			    TN *label_tn,
 *			    OPS *prolog_ops,
 *			    OPS *body_ops
 *                          OPS *epilog_ops);
 *
 *    Generate the branch operation needed by SWP.  Generate br.ctop for
 *    doloop.  Generate br.wtop for while-loop.  Generate the init/fini
 *    for rotating register bases.
 *  
 *  BOOL Prepare_Loop_For_SWP(CG_LOOP& cl)
 *    Prepare a loop for SWP, including removing all predicate
 *    invariants, and removing non-definite dependence.   If some 
 *    non-definite dependence cannot be removed, then return FALSE.
 *    Convert to postincr form if necessary.
 *
 *  void Convert_While_Loop_to_Fully_Predicated_Form(CG_LOOP& cl, bool trace)
 *    Convert a while-loop into the fully predicated form.
 *    Require CG_LOOP_Info.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef CG_SWP_TARGET_INCLUDED
#define CG_SWP_TARGET_INCLUDED

#include "tn.h"
#include "op.h"

class CG_LOOP;
class CG_LOOP_DEF;

// Define BASE_UPDATE kind
//
enum BASE_UPDATE {
  NO_BASE_UPDATE  = 0,  // OP does not have base-update variant
  REG_BASE_UPDATE = 1,  // OP has register base-update variant
  IMM_BASE_UPDATE = 2,  // OP has immediate base-update variant
};

#ifdef TARG_IA64

// The following functions defined in ia64/cg_swp_target.cxx.

extern void SWP_Exp_COPY(TN *result, TN *opnd, OPS *ops);

extern INT32 SWP_Max_Slots_Per_Cycle();

void SWP_Loop_Init_Fini(bool is_doloop,
			INT stage_count,
			OPS *prolog_ops,
			OPS *body_ops,
			OPS *epilog_ops);

extern BOOL Prepare_Loop_For_SWP_1(CG_LOOP& cl, bool trace);

extern BOOL Prepare_Loop_For_SWP_2(CG_LOOP& cl, bool trace);

extern void Convert_While_Loop_to_Fully_Predicated_Form(CG_LOOP& cl);

extern BOOL Remove_Non_Definite_Dependence(CG_LOOP &cl, bool cg_loop_init, bool trace);

extern BASE_UPDATE OP_base_update_kind(OP *op);

extern INT32 OP_base_opnd_num(OP *op);

extern INT32 OP_base_res_num(OP *op);

extern INT32 OP_incr_opnd_num(OP *op);
 
extern BOOL Imm_Value_In_Range(OP *op, INT64 imm);

extern void Gen_SWP_Branch(CG_LOOP &cl, bool is_doloop);

extern void Gen_SWP_Branch_Predict(BB *body, BB *prolog, BB *epilog);

extern void Undo_SWP_Branch(CG_LOOP &cl, bool is_doloop);

extern void Gen_Implicit_Prefetches(CG_LOOP &cl, bool trace);

inline TN* Base_update_tn(OP *op)
{
  if (OP_load(op)) {
    if (OP_results(op) == 2)
      return OP_result(op,1);
  } else if (OP_store(op)) {
    if (OP_results(op) == 1)
      return OP_result(op,0);
  }
  return NULL;
}

#else

// The following functions are NOPs for MIPS and IA-32

#define SWP_Exp_COPY(x,y,z)  Exp_COPY(x,y,z)

inline INT32 SWP_Max_Slots_Per_Cycle() {return 1;}


void inline 
SWP_Loop_Init_Fini(bool is_doloop,
		   INT stage_count,
		   OPS *prolog_ops,
		   OPS *body_ops,
		   OPS *epilog_ops)
{ 
}


inline BOOL Prepare_Loop_For_SWP_1(CG_LOOP& cl, bool trace)
{
  return FALSE;
}

inline BOOL Prepare_Loop_For_SWP_2(CG_LOOP& cl, bool trace)
{
  return FALSE;
}

inline void Convert_While_Loop_to_Fully_Predicated_Form(CG_LOOP& cl)
{
}

inline BOOL Remove_Non_Definite_Dependence(CG_LOOP &cl, bool cg_loop_init, bool trace)
{
  return TRUE;
}

inline BASE_UPDATE OP_base_update_kind(OP *op) 
{
  return NO_BASE_UPDATE;
}

inline BOOL Imm_Value_In_Range(OP *op, INT64 imm) 
{
  return FALSE;
}

inline void Gen_SWP_Branch(CG_LOOP &cl, bool is_doloop) { }

inline void Gen_SWP_Branch_Predict(BB *body, BB *prolog, BB *epilog) { }

inline void Undo_SWP_Branch(CG_LOOP &cl, bool is_doloop) { }

inline void Gen_Implicit_Prefetches(CG_LOOP &cl, bool trace) { }

inline TN* Base_update_tn(OP *op) { return NULL; }

#endif

#endif
