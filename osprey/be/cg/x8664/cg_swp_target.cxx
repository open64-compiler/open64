/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 *  $Revision: 1.5 $
 *  $Date: 04/12/21 14:57:08-08:00 $
 *  $Author: bos@eng-25.internal.keyresearch.com $
 *  $Source: /home/bos/bk/kpro64-pending/be/cg/x8664/SCCS/s.cg_swp_target.cxx $
 *
 * =======================================================================
 * ======================================================================= */

#include <stdint.h>
#define USE_STANDARD_TYPES
#include <map>
#include "defs.h"
#include "mempool.h"
#include "tn.h"
#include "tn_set.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "op_map.h"
#include "cgexp.h"
#include "cgtarget.h"
#include "register.h"
#include "cg_loop.h"
#include "cg_swp_options.h"
#include "cg_swp.h"
#include "cg_swp_target.h"
#include "tracing.h"
#include "pf_cg.h"
#include "cg_loop.h"
#include "calls.h"
#include "tag.h"

/* ====================================================================
 *
 *  Convert all invariant predicate into a computation 
 *
 * ====================================================================
 */
void Remove_Invariant_Predicates(CG_LOOP& cl, bool trace)
{
}


/* ====================================================================
 *
 *  Convert all p0 conditional cmp into unconditional form
 *
 * ====================================================================
 */
void Unc_Promotion(CG_LOOP& cl, bool trace)
{
}



/* ====================================================================
 *
 *  Remove MOVL to avoid special template requirement and register 
 *  requirement.
 *
 * ====================================================================
 */
void Hoist_MOVL(CG_LOOP& cl, bool trace)
{
}


// Construct a data structure to locate all defs and uses of a TN,
// using the OP_VECTOR::index.   The properties of the OP_VECTOR::index
// is that it's ordered.
// 
struct TN_DU {
  typedef OP_VECTOR::index_type index_type;
  vector<index_type> defs;
  vector<index_type> uses;

  bool TN_is_invariant() const {
    return defs.size() == 0;
  }

  // Returns true if the TN is not modified in the range [first,last)
  bool TN_unchanged(index_type first, index_type last) {
    for (int i = 0; i < defs.size(); i++) {
      index_type t = defs[i];
      if (first <= t && t < last)
	return false;
    }
    return true;
  }

  // Returns true if the TN can be assigned a non-rotating register
  bool TN_can_use_non_rotating_reg(TN *tn, OP_VECTOR& op_vec) {
    
    // d is set to the earliest definition, set to MAX_INT if there is no definitions
    index_type d = defs.size() > 0 ? defs[0] : INT32_MAX;

    // an omega 0 use is always OK
    // an omega 1 use is OK if the use is before the earliest definition
    // an omega >1 use is never OK

    for (int j = 0; j < uses.size(); j++) {
      index_type u = uses[j];
      OP *op = op_vec[u];

      for (int i = 0; i < OP_opnds(op); i++) {
	if (tn == OP_opnd(op, i)) {
	  int omega = OP_omega(op, i);
	  if (omega >= 1) {
	    if (omega > 1)
	      return false;
	    if (d < u)  // omega == 1
	      return false;
	  }
	}
      }
    }
    return true;
  }
};


// Construct a TN to TN_DU mapping.
//
struct TN_DU_MAP {

  typedef std::map<TN *, TN_DU>::iterator iterator;
  std::map<TN *, TN_DU> TN_DU_map;

  iterator begin() {
    return TN_DU_map.begin();
  }

  iterator end() {
    return TN_DU_map.end();
  }

  TN_DU& operator[](TN *tn) {
    return TN_DU_map[tn];
  }

  // Build a TN_DU data structure for each TN
  // referenced in the BB.   And also assign an OP-number
  // to each OP *.   The TN_DU represents all occurrences
  // of defs and uses of the TN using the OP-number.
  //
  TN_DU_MAP(OP_VECTOR& op_vec, bool trace) {

    for (INT op_num = 0; op_num < op_vec.size(); op_num++) {
      OP *op = op_vec[op_num];
      INT i;
      for (i = 0; i < OP_results(op); i++) {
	TN *tn = OP_result(op,i);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    !TN_is_const_reg(tn)) {
	  if (TN_DU_map.find(tn) == TN_DU_map.end()) 
	    TN_DU_map[tn] = TN_DU();
	  TN_DU_map[tn].defs.push_back(op_num);
	}
      }
      for (i = 0; i < OP_opnds(op); i++) {
	TN *tn = OP_opnd(op,i);
	if (TN_is_register(tn) && 
	    !TN_is_dedicated(tn) &&
	    !TN_is_const_reg(tn)) {
	  if (TN_DU_map.find(tn) == TN_DU_map.end()) 
	    TN_DU_map[tn] = TN_DU();
	  TN_DU_map[tn].uses.push_back(op_num);
	}
      }
    }

    // Trace the TN_DU data structure
    if (trace) {
      for (iterator it = TN_DU_map.begin(); it != TN_DU_map.end(); it++) {
	TN *tn = (*it).first;
	TN_DU &lrs = (*it).second;
	fprintf(TFile, "Remove_Non_Definite_Dependence: TN_DU of TN%d: defs={", TN_number(tn));
	{
	  for (int i = 0; i < lrs.defs.size(); i++) {
	    fprintf(TFile, "%d", lrs.defs[i]);
	    if (i != lrs.defs.size()-1) fputc(',', TFile);
	  }
	}
	fprintf(TFile, "}, uses={");
	{
	  for (int i = 0; i < lrs.uses.size(); i++) {
	    fprintf(TFile, "%d", lrs.uses[i]);
	    if (i != lrs.uses.size()-1) fputc(',', TFile);
	  }
	}
	fprintf(TFile, "}\n");
      }
    }
  }
};



/* ====================================================================
 *
 * tn_is_needed_in_epilog
 *
 *   Using the increment feature on memory ops will cause the new
 *   value of the index to destroy the previous value.  When we need
 *   the previous value to complete the epilog sequence, we need to
 *   use rotating registers to save the previous value, or decrement
 *   the value in the epilog before it is used (not yet implemented).
 *
 * ====================================================================
 */
BOOL static tn_is_needed_in_epilog (TN *tn)
{ 
  return FALSE;
}



/* ====================================================================
 *
 * OP_owns_the_base_TN
 *
 *   If there is no other dependence memop using the base TN,
 *   then this OP owns it.
 *
 * ====================================================================
 */
bool OP_owns_the_base_TN(OP *op, TN *base, TN_DU& tn_du, OP_VECTOR& op_vec) 
{
  return true;
}



// Return the base update form of the OP
//
BASE_UPDATE
OP_convertible_to_base_update(OP *op)
{
  return NO_BASE_UPDATE;
}


// Returns the address base opnd
INT OP_base_opnd_num(OP *op)
{
  return -1;
}


// Returns the address base opnd
INT OP_base_res_num(OP *op)
{
  return -1;
}


// Returns the address base opnd
INT OP_imm_opnd_num(OP *op)
{
  return -1;
}


INT32 OP_incr_opnd_num(TOP top)
{
  // the last operand
  return (TOP_fixed_opnds(top) - 1);
}

INT32 OP_incr_opnd_num(OP *op)
{
  // the last operand
  return (OP_opnds(op) - 1);
}


static INT32 Num_defs(BB *body, TN *def_tn)
{
  return 0;
}


BOOL Mem_stride_ge_access(OP *op, INT64 stride)
{
  return FALSE;
}


/* ====================================================================
 *
 *   Marks OP that has no cross-iteration aliasing.
 *
 *     need to run after unrolling and before postincr form 
 *
 * ====================================================================
 */
void Init_OP_no_ci_alias(CG_LOOP& cl, BOOL trace)
{
}


// Identify and delete the increment operation.
// The increment operation will be combined into the op's
// base-update form.
//
static TN *
Identify_and_delete_incr(BB *bb, OP *memop, INT base_opnd_num, BASE_UPDATE up) 
{
  return NULL;
}


void Convert_OP_to_base_update_form(BB *body, OP *op, TN *incr, BASE_UPDATE up,
				    INT base_opnd_num, bool trace)
{
}


/* ====================================================================
 *
 *  Convert_Post_Incr
 *   try to convert load/store into their base update form
 *   it must also update the OP_omega information
 *
 * ====================================================================
 */
void Gen_Post_Incr_Memop(CG_LOOP& cl, bool trace)
{
}


/* ====================================================================
 *
 *  Expand_Simulated_Ops
 *
 * ====================================================================
 */
static BOOL Expand_Simulated_Ops(CG_LOOP& cl, bool trace)
{
  return FALSE;
}


TOP Get_Predicated_Form(TOP top)
{
  return TOP_UNDEFINED;
}
