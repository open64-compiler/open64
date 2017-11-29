/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_estr.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_estr.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//   Data structures and functions for Strength Reduction based on SSA.
//
// ====================================================================
// ====================================================================


#ifndef opt_estr_INCLUDED
#define opt_estr_INCLUDED "opt_estr.h"

class BB_NODE;
class CFG;
class EXP_OCCURS;
class PHI_NODE;

#include "opt_htable.h"

class STR_RED {
private:
  CFG              *_cfg;
  CODEMAP          *_htable;
  BOOL              _tracing;
  STACK<STMTREP *>  _repaired_statements;

  CFG *Cfg(void) const
    { return _cfg; }
  CODEMAP *Htable(void) const
    { return _htable; }
  BOOL Tracing(void) const
    { return _tracing; }

  // Determine if the coderep is a variable defined by an IV-update
  BOOL Defined_by_iv_update(const CODEREP *use_cr, const CODEREP *def_cr,
			    CODEREP *invar, BB_NODE *use_bb,
			    const CODEREP *cand_expr,
			    BOOL aggstr_cand = FALSE) const;

  // Determine if the coderep is a variable defined by an IV-update
  // (or chain of updates), and that the final rhs of the iv-update is
  // defined in a block that dominates def_bb.
  BOOL Defined_by_iv_update_no_def(CODEREP *use_cr, 
				   BB_NODE *def_bb, CODEREP **def_cr,
				   CODEREP *invar, BB_NODE *use_bb,
				   const CODEREP *cand_expr,
				   BOOL aggstr_cand = FALSE) const;

  // Return the induction variable in this candidate that we are 
  // strength-reducing, as well as the multiplier (null if 1).
  // Called only when the def occur is phi-result.
  void Find_iv_and_mult_phi_res(const EXP_OCCURS *def, CODEREP **iv_def,
				const EXP_OCCURS *use, CODEREP **iv_use,
				CODEREP **multiplier) const;

  // Determine if "last" is defined by a chain of IV updates, one of
  // which has "first" on its RHS.
  BOOL Updated_by_iv_update(const CODEREP *first,
			    const CODEREP *last,
			          CODEREP *invar,
			          BB_NODE *innermost_use_bb,
			    const CODEREP *cand_expr,
			    BOOL aggstr_cand) const;

  // Determine if an expression is loop-invariant for its occurrence
  // in BB.
  BOOL Is_const_or_loop_invar(CODEREP *expr, BB_NODE *bb) const;

  // Determine if the CVT is a linear function, and the value being
  // converted can thus be used as an iv.
  BOOL Is_cvt_linear(const CODEREP *cr) const;
  BOOL Is_implicit_cvt_linear (MTYPE, const CODEREP *cr) const;

  // Does the lhs var match the rhs var/expr?  If so, return the
  // rhs var, else return NULL.  May strip off CVTs, etc.
  CODEREP *Matches_lhs( const CODEREP *lhs, const CODEREP *rhs )const;

  // Determine if bb1 is in the same loop as bb2, or if bb1 is in a
  // loop enclosed by bb2's loop (if any)
  BOOL In_same_or_lower_nesting( BB_NODE *bb1, BB_NODE *bb2 ) const;

  // private functions so they cannot be used incorrectly.
  STR_RED(const STR_RED&);
  STR_RED& operator = (const STR_RED&);

public:
  // NOTE: if adding any parameters, create a private version of
  // this constructor with void parameter so it can't be used.
  STR_RED(CFG *cfg, CODEMAP *htable, MEM_POOL *pool, BOOL tracing) :
    _cfg(cfg), _htable(htable), _tracing(tracing),
    _repaired_statements(pool)
      { };

  ~STR_RED(void)
    { };

  // Determine if the opcode is strength-reducible
  BOOL Candidate_opc(const OPCODE opc) const
    { 
      switch ( OPCODE_operator(opc) ) {
      case OPR_MPY:
      case OPR_ADD:
      case OPR_SUB:
      case OPR_NEG:
      case OPR_CVT:
	return MTYPE_is_integral(OPCODE_rtype(opc)) &&
	       ! MTYPE_is_vector(OPCODE_rtype(opc));
      default:
	return FALSE;
      }
    }

  // Determine if the use occurrence is a strength-reduction candidate
  BOOL Candidate(const CODEREP *cr,
		 const CODEREP *def_opnd0, const CODEREP *def_opnd1,
		 BB_NODE *def_bb,
		 CODEREP *use_opnd0, CODEREP *use_opnd1,
		 BB_NODE *use_bb) const;

  // Determine if the use occurrence is a strength-reduction candidate
  // but assume that the def occurrence is in a phi result in the
  // def_bb.
  BOOL Candidate_phi_res(const CODEREP *cr,
			 BB_NODE *def_bb,
			 CODEREP *use_opnd0, CODEREP *use_opnd1,
			 BB_NODE *use_bb) const;

  // Determine if the statement is an IV update;
  // Called during Phi-Insertion step.
  BOOL Determine_iv_update(STMTREP *stmt, CODEREP **updated) const;

  // Determine if one of the phi opnds is defined by an IV update;
  // Called during Phi-Insertion step.
  BOOL Determine_iv_update_phi(PHI_NODE *phi, const CODEREP *cand_expr) const;

  // Return the induction variable in this candidate that we are 
  // strength-reducing, as well as the multiplier (null if 1)
  void Find_iv_and_mult(const EXP_OCCURS *def, CODEREP **iv_def,
			const EXP_OCCURS *use, CODEREP **iv_use,
			CODEREP **multiplier) const;


  // Returns the threshold adjusted by taking into account the content of the
  // innermost loop that bb belongs to
  INT Local_autoaggstr_reduction_threshold(BB_NODE *bb) const;

  // Determine if the statement updates an induction variable, and
  // return the induction variable being updated (one on rhs), the
  // increment amount, and whether or not it's an increment or
  // decrement
  BOOL Find_iv_and_incr(STMTREP *stmt, CODEREP **updated_iv,
			CODEREP **incr_amt, BOOL *is_add,
			BOOL aggstr_cand = FALSE) const;

  // Find the definition of the variable, following the u-d chain
  // until we get a real store to the variable, and return its rhs.
  // Used if the rhs of an iv-update has been CSE'd
  CODEREP *Find_real_defs_rhs(const CODEREP *var) const;

  // Functions to keep track of whether an injury caused by this
  // statement has been repaired. The corresponding STMTREP member
  // functions should be called only from here, nowhere else.
  void Set_repaired(STMTREP *);
  BOOL Repaired(const STMTREP *stmt) const { return stmt->Repaired(); }

  void Perform_per_expr_cleanup(void);

  BOOL Update_happens_rarely_enough(      BB_NODE *update_bb,
				          BB_NODE *innermost_use_bb,
				    const CODEREP *use_expr) const;

};

#endif  // opt_estr_INCLUDED
