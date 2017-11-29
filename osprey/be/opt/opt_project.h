/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_project.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_project.h,v $
//
// Revision history:
//  26-MAR-97 rkennedy - Original Version
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
// Description: Routines for special handling of operations that are
//              "projectable" in the sense of OPR_DIVREM.
//
// An operation is said to be projectable if the following hold:
//   it produces a K-tuple of results (K > 1);
//   each component of the K-tuple can be expressed and computed
//     individually at lower cost than the cost of computing the
//     K-tuple all at once;
//   the cost of computing the K-tuple all at once is less than the
//     total cost of computing all its components separately; and
//   the operation of projecting the K-tuple to any of its individual
//     components is very efficient.
//
// Note: In current WHIRL, K is always 2.
//
// Examples:
//   OPR_DIVREM is a projectable WHIRL operation because OPR_DIVREM
//   costs more than OPR_DIV or OPR_REM alone, but less than the two
//   together. Its projections are OPR_DIVPART and OPR_REMPART.
//
//   The CIS intrinsic op is a projectable operation whose projections
//   are OPR_REALPART and OPR_IMAGPART.
//
//   OPC_C*MPY is not a projectable operation. It meets all the
//   conditions except one: there is no succinct enough WHIRL
//   representation of the single-component computation. "Succinct
//   enough" is defined here as "generatable by the routine
//   Uncombine_operations in opt_combine.{cxx|h}." If we were willing
//   to make Uncombine_operations more complicated so it could
//   generate the computation of the real part of a complex multiply,
//   we could treat OPC_C*MPY as a projectable operation.
//
// The point of recognizing and handling projectable operations is so
// we can maximize CSE opportunities and yet fall back on computing
// individual components whenever only one component turns out to be
// required. Early in the optimizer, operations that transparently
// correspond to projections of projectable operations are converted
// to those projections (e.g., OPR_DIV is converted to
// OPR_DIVPART(OPR_DIVREM)). After optimization, if only one
// projection of a projectable operation's result is actually taken
// (e.g., if a particular DIVREM operation's result is used only by
// OPR_DIVPART), we would like to convert the projection back to the
// single-component computation (e.g., the OPR_DIV operation).
//
// This no-CSE situation can manifest itself in two ways:
//   In the first, EPRE found no CSE involving the projectable
// operation, so the projection of the projectable operation remains a
// single intact CODEREP expression tree at emit time. In this case
// nothing sophisticated is required: the emitter recombines the
// projection with the projectable operation by calling
// Uncombine_operations (sic) to simplify the WHIRL after it has been
// emitted by Gen_exp_wn.
//   In the second, EPRE found some redundancy. The fact that the
// projectable operation is used only by one of its projections means
// that EPRE will have found the same redundancy in the projection
// as in the projectable operation. EPRE will have produced code
// like the following:
//
//  DIVREM(a, b)
// STID t_i    [ store to version i of t ]
// ... possibly more code here, but no use of t ...
//   LDID t_i  [ load version i of t ]
//  DIVPART
// STID u_j    [ store to version j of u ]
//
// Since there is no REMPART(t_i) in the program, there will be no
// other live use of t_i in the program. Note that t_i may still be
// used as an operand of a dead phi (which will be marked dead by
// dead-store elimination in DCE). In this situation, the emitter is
// given information by DCE guaranteeing that the STID to t_i need not
// be emitted. When the emitter sees the projection of t_i, it uses
// the SSA use-def information to find the expression on the RHS of
// the STID that got skipped, and substitutes that RHS for the use of
// t_i when building the WHIRL expression tree. Later,
// Uncombine_operations will combine the projection with the
// projectable operation as in the simple case above. The substitution
// for t_i is known to be safe because t is known to be an EPRE
// temporary and the lifetimes of such EPRE temporaries do not include
// any assignments to operands of the expressions they hold.
//
// To supply the emitter with the appropriate guarantees about which
// STID's need not be emitted, DCE computes the number of live uses of
// the LHS of each store of a projectable operation. If there is more
// than one use of such a version of a variable, at least one of the
// following cases must hold:
//   1. the variable is not an EPRE temp (in this case the emitter will
//      not skip/substitude the STID regardless);
//   2. more than one different projection of the variable's version is
//      taken in the program;
//   3. EPRE did not eliminate all the redundancy for the projectable
//      operation; or
//   4. the variable is used as an operand of a live phi.
// So we get the safety and code quality we want if DCE simply counts
// live uses of the LHS of each STID whose RHS is a projectable
// operation. A count of 1 will enable skipping/substitution in the
// emitter. If DCE sees a use as a phi operand, the count is set to
// 2, ensuring that no skip/substition will happen. (A more
// sophisticated emitter implementation could perform the substitution
// if there was only a single use as a phi operand, but it would have
// to have more sophisticated techniques to follow the use-def chain
// through phi.) On seeing any real use of the LHS, DCE bumps the
// STID's count by 1.
//
// ====================================================================
// ====================================================================


#ifndef opt_project_INCLUDED
#define opt_project_INCLUDED "opt_project.h"

#ifndef opt_htable_INCLUDED
#include "opt_htable.h"
#endif

// Is the given WHIRL OPERATOR always projectable?
inline BOOL
Projectable_operation(const OPERATOR opr)
{
  return (opr == OPR_DIVREM ||
	  opr == OPR_MINMAX);
}

// Is the given WHIRL OPCODE always projectable?
inline BOOL
Projectable_operation(const OPCODE opc)
{
  return Projectable_operation(OPCODE_operator(opc));
}

// Is the given intrinsic a combined COS/SIN computation?
inline BOOL
Sin_cos_intrinsic(const INTRINSIC intrinsic)
{
  return (intrinsic == INTRN_F4CIS  ||
	  intrinsic == INTRN_F4CISe ||
	  intrinsic == INTRN_F8CIS  ||
	  intrinsic == INTRN_F8CISe ||
	  intrinsic == INTRN_FQCIS  ||
	  intrinsic == INTRN_FQCISe);
}

// Is the root of the given CODEREP a projectable operation?
inline BOOL
Projectable_operation(const CODEREP *const cr)
{
#ifdef TARG_X8664
  return (Projectable_operation(cr->Op()) || OPCODE_rtype(cr->Op()) == MTYPE_V16C8 ||
#else
  return (Projectable_operation(cr->Op()) ||
#endif
	  (OPCODE_operator(cr->Op()) == OPR_INTRINSIC_OP &&
	   Sin_cos_intrinsic((INTRINSIC) cr->Intrinsic())));
}

// Can the given WHIRL OPERATOR ever project a projectable operation?
inline BOOL
Projection_operation(const OPERATOR opr)
{
  return (opr == OPR_DIVPART  ||
	  opr == OPR_REMPART  ||
	  opr == OPR_MINPART  ||
	  opr == OPR_MAXPART  ||
	  opr == OPR_REALPART ||
	  opr == OPR_IMAGPART);
}

// Can the given WHIRL OPCODE ever project a projectable operation?
inline BOOL
Projection_operation(const OPCODE opc)
{
  return Projection_operation(OPCODE_operator(opc));
}

// Projectable operations that need recombining can show up only in
// two situations: below a matching projection, or on the RHS of an
// STID to an EPRE temporary. This function indicates whether the
// statement stores a projectable operation to a temporary.
//
// Note that user code may contain other stores of projectable
// operations (i.e., stores to variables that aren't EPRE temps). We
// don't try to factor out any projections of such projectable
// operations.
inline BOOL
Stores_proj_op_to_temp(const STMTREP  *const stmt,
		       const OPT_STAB *const opt_stab)
{
  if (OPCODE_operator(stmt->Op()) == OPR_STID &&
      opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id())->EPRE_temp() &&
      stmt->Rhs()->Kind() == CK_OP &&
      Projectable_operation(stmt->Rhs())) {
    Is_True(ST_class(opt_stab->Aux_stab_entry(stmt->Lhs()->Aux_id())->St()) == CLASS_PREG,
	    ("Stores_proj_to_temp: Store of projectable op must "
	     "be to PREG"));
    return TRUE;
  }
  else {
    return FALSE;
  }
}

extern STMTREP *Proj_defstmt(const CODEREP *, const OPT_STAB *);

#endif
