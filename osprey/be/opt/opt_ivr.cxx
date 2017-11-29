/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_ivr.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_ivr.cxx,v $
//
// Revision history:
//  28-JAN-95 shin - Original Version
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
//   IVR::IVR
//         IVR::Get_my_regionstart
//         IVR::Find_parallel_pragma_stmt
//       IVR::Find_associated_parallel_pragma
//     IVR::Is_mp_with_same_mp_pragma
//       IVR::Find_associated_parallel_pragma *
//     IVR::Preprocess_mp_pragma_list
//         IVR::Generate_step
//       IVR::Ident_all_iv_cands
//         IVR::Satisfy_primary_IV
//           Init_expr_cost
//         Primary_IV_preference
//       IVR::Choose_primary_IV
//         IV_EXPR
//         IVR::Replace_IV_with_invar
//           IVR::Replace_IV_with_invar *
//         IVR::Compute_trip_count
//       IVR::Determine_trip_IV_and_exit_count
//         CR_is_equivalent
//         Is_IV_cand_in_parent_loop
//       IVR::Update_exit_stmt
//       IVR::Replace_secondary_IV
//     IVR::Convert_all_ivs
//     IVR::Update_mp_pragma_list
//   IVR::Process_one_loop
//   IVR::~IVR
// COMP_UNIT::Do_iv_recognition
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_ivr_CXX	"opt_ivr.cxx"
static char *rcs_id = 	opt_ivr_CXX"$Revision: 1.19 $";
#endif /* _KEEP_RCS_ID */

#define USE_STANDARD_TYPES

#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "wn.h"
#include "wn_util.h"
#include "region_util.h"

#include "opt_base.h"
#include "opt_bb.h"
#include "bb_node_set.h"
#include "opt_main.h"
#include "opt_ivr.h"
#include "opt_util.h"
#include "opt_wn.h"
#include "opt_mu_chi.h"
#include "opt_fold.h"
#include "stab.h"

// provided for compatibility

inline void
Set_Preg_Name(PREG_NUM preg_num, const char *const name)
{
  Is_True(preg_num > Last_Dedicated_Preg_Offset,
          ("Set_Preg_Name: Cannot set name of dedicated PREG"));
  Set_PREG_name(Preg_Table[preg_num - Last_Dedicated_Preg_Offset], name);
}


/************************************************************************

IVR issues
----------

1.  Introduction of primary IV (pv 469578)

The bug in pv 469578 is due to IVR replacing an I8 IV in the loop-exit
condition with an I4 and the IV values eventually exceed MAXINT.  It
is fixed by disabling a secondary IV replacement if sizeof(secondary
IV) > sizeof(primary IV) and sizeof(trip IV) > sizeof(primary IV).
The objective is not to replace a 'long' or '64bit ptr' with 'int' by
IVR unless the range of 'int' is sufficient.  The range is sufficient
if the trip IV is I4 and therefore cannot have more than 2^32 distinct
values.

After the fix, all not secondary IV can be replaced with the primary
IV.  There is a need to introduce two IVs: one I4 and one I8.  The current 
implementation has two limitations:  

a. The primary IV is introduced during loop normalization and it is
too early.  At that time, the size of primary IV cannot be decided
(and hence always I4).

b. Loop normalization can only add one IV because of the DO-loop form.

  
2.  Inverse slt conversion (pv 447872).

It is only used by LNO when we converts while-loop to do-loop.  It
essentially converts p != q; p++ to p < q; p++.  It can be divided
into 2 case.
  
The first case is when p,q are pointers.  The loop executes zero times
if q is NULL or q < p after inverse slt conversion.  In the O0
implementation, the loop executes (unsigned) q - init(p) times.  From
the language definition, the exit condition should never be TRUE
because there is no way to advance a pointer of object X to point to
object Y or NULL.  If the loop does not have an early exit, I can
argue that the program is illegal because p will be advanced to some
point beyond the object it pointed to.  The patch for 447872 is indeed
by disabling inverse slt if an early exit is detected.  The is a
reasonable patch because LNO does not look at any early-exit loops.

An alternative is to replace the loop-exit conditions with
   a)  (unsigned)(q - p) > 0 
   b)  primary_iv - primary_init_value < trip_count
Both cases guarantees correctness, but unfortunately introduces
another performance issue.  For the first form, strength reduction
introduce one extra register and one extra addition to compute (q-p).
For the second form (if the trip IV is incremented by and the loop
exit conditions looks like i * 4 < (trip_bound - trip_init) / 4), LFTR
generates (trip_bound - trip_init) / 4 * 4.  Notice the x/4*4 is not
simplified.

None of these are unsolvable, but support from str-red and LFTR is
required for this scheme to work.  I consider this not useful until
LNO starts to optimize early-exit loops.

The second case is when p,q are integers.  e.g,
    for (i = MAXINT; i != -MAXINT; i++)  executes two times.
    for (i = MAXINT; i < -MAXINT; i++)  executes zero times.

IVR has code to check for overflow if the bounds are constants.
However, it the initial values and upper bound are variables, IVR will
generate incorrect code.  Strength reduction and LFTR have the same
bug when the induction variable can overflow.  It seems that the
alternate solution can guarantee corrections by producing

    for (i = MAXINT; (unsigned) (-MAXINT - i) > 0; i++) { i * 4; }

But if LFTR can optimize this, then it would need to transform
(-MAXINT - i) into (-MAXINT * 4) - t and introduce another overflow.

So my conclusion is that we cannot have both correctness and
performance at the same time.  If IVR is always correct, then either
performance is gone or the overflow bug will show up in LFTR.


3.  Loop exit values (pv 453841)

i == n if the loop is exited at the loop-merge block.
Suneel pointed out that the loop exit value should only be computed
if the loop has no early exit, mainly because there is no existing
optimization to use the result.

  for (i = 0; i < n; i += 1) {
    if (a[i] == v) break;
  }
  if (i < n) { ... }

If the compiler can duplicate (i < n) and folds with i=n at the
loop-merge block, it is desirable to keep the loop exit value.
Since we don't have this, I'll disable loop exit values computation
for early exit loops.


4.  Replacement of secondary IV in mainopt (pv 434162)

This was the result of two disasters:
a.  replacing secondary IV (common/com/config_wopt.c) was disabled!
    I re-enable it already;
b.  loop normalization was disabled (code commented out in opt_loop.cxx);

With new ESTR, we should be able to use U4 as the primary IV because 
U4 is not difference I4 and strength reduction has been enhanced to optimize
U4 IVs.  

However, there is no enough time in v7.2 to test the combinations.


************************************************************************/



// ====================================================================
// Define a class to deal with IV_EXPR
// Call graph:
//     Eval_to_const
//   Find_IV
//     Get_const
//   Find_step
// Init
//
// Perhaps could use LINEAR_FUNCTION from opt_loop.cxx instead.
// ====================================================================


enum IV_EXPR_FLAGS {
  IV_EXPR_NONE  = 0,
  IV_EXPR_VALID = 1,
};


class IV_EXPR {
  INT32    _flags;
  BB_LOOP  *_loop;
  CODEREP  *_iv_expr;
  CODEREP  *_based_iv;
  INT64    _step;

  BOOL     Iv_expr_valid(void)          { return _flags & IV_EXPR_VALID; }
  BB_LOOP *Loop(void)                   { return _loop; }
  CODEREP *Iv_expr(void)                { return _iv_expr; }

  void  Set_flags(INT32 flags)          { _flags = flags; }
  void  Set_iv_expr_valid(void)         { _flags |=  IV_EXPR_VALID; }
  void  Reset_iv_expr_valid(void)       { _flags &= ~IV_EXPR_VALID; }
  void  Set_loop(BB_LOOP *loop)         { _loop = loop; }
  void  Set_iv_expr(CODEREP *iv_expr)   { _iv_expr = iv_expr; }
  void  Set_based_iv(CODEREP *based_iv) { _based_iv = based_iv; }
  void  Set_step(INT64 step)            { _step = step; }

  BOOL     Eval_to_const(CODEREP *);
  INT64    Get_const(CODEREP *);
  CODEREP *Find_IV(CODEREP *);
  INT64    Find_step(CODEREP *);

public:

  IV_EXPR(void)  {};
  ~IV_EXPR(void) {};
  
  CODEREP *Based_iv(void)         { return _based_iv; }
  INT64    Step(void)             { return _step; }
  BOOL     Valid_expr(void)       { return Iv_expr_valid() && Based_iv() != NULL && Step() != 0; }

  void Init(CODEREP *iv_expr, BB_LOOP *loop)  {
    Set_loop(loop);
    Set_iv_expr(iv_expr);
    Set_based_iv (NULL);
    Set_step(0);

    // Set IV_EXPR_VALID before calling Find_IV.
    // Find_IV will reset it if anything is wrong!
    Set_flags(IV_EXPR_VALID);
    CODEREP *cr = Find_IV(iv_expr);

    if (Iv_expr_valid()) {
      Set_based_iv (cr);
      Set_step ( Find_step(iv_expr) );
    }
  }
};


// Does the expr evalute to a numeric constant?
//
BOOL
IV_EXPR::Eval_to_const(CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_VAR:
  case CK_IVAR:
    return FALSE;

  case CK_CONST:
    return TRUE;

  case CK_OP:
    OPERATOR opr = cr->Opr();
    switch (opr) {
    case OPR_PAREN:
    case OPR_NEG:
      return Eval_to_const(cr->Opnd(0));

    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
      return Eval_to_const(cr->Opnd(0)) && Eval_to_const(cr->Opnd(1));
    }  // switch (opr)

  } // switch (cr->Kind())

  return FALSE;
}


//    Set IV_EXPR_VALID before calling this routine.
//    If IV_EXPR_VALID is still set and the return value is non-NULL,
//    an IV found.
//
CODEREP *
IV_EXPR::Find_IV(CODEREP *iv_expr)
{
  switch (iv_expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
    return NULL;
    
  case CK_IVAR:
    if (!Loop()->Invariant_cr(iv_expr))
      Reset_iv_expr_valid();
    return NULL;

  case CK_VAR:
    if (Loop()->Invariant_cr(iv_expr)) 
      return NULL;
    else
      return iv_expr;
    
  case CK_OP:
    OPERATOR opr = iv_expr->Opr();
    switch (opr) {
    case OPR_PAREN:
    case OPR_NEG:
      return Find_IV(iv_expr->Opnd(0));

    case OPR_MPY:
      {
        CODEREP *opnd0_has_iv = Find_IV(iv_expr->Opnd(0));
        CODEREP *opnd1_has_iv = Find_IV(iv_expr->Opnd(1));

        if (opnd0_has_iv && !opnd1_has_iv && Eval_to_const(iv_expr->Opnd(1)))
          return opnd0_has_iv;
        if (opnd1_has_iv && !opnd0_has_iv && Eval_to_const(iv_expr->Opnd(0)))
          return opnd1_has_iv;
	
	Reset_iv_expr_valid();
        return NULL;   // if both have conflicting IV
      }

    case OPR_ADD:
    case OPR_SUB:
      {
        CODEREP *opnd0_has_iv = Find_IV(iv_expr->Opnd(0));
        CODEREP *opnd1_has_iv = Find_IV(iv_expr->Opnd(1));
	if (!opnd0_has_iv && !opnd1_has_iv)
	  return NULL;
        if (opnd0_has_iv && !opnd1_has_iv)
          return opnd0_has_iv;
        if (opnd1_has_iv && !opnd0_has_iv)
          return opnd1_has_iv;
	if (opnd0_has_iv && opnd1_has_iv && opnd0_has_iv == opnd1_has_iv)
	  return opnd0_has_iv;

	Reset_iv_expr_valid();
        return NULL;   // if both has IV or neither has I
      }

    default:
      if (!Loop()->Invariant_cr(iv_expr))
	Reset_iv_expr_valid();
      return NULL;

    } // switch (opr)

  } // switch (iv_expr->Kind())

  // for anything that is not handled.
  Reset_iv_expr_valid();
  return NULL;
}


//  Compute a constant expression.
INT64 
IV_EXPR::Get_const(CODEREP *cr)
{
  switch (cr->Kind()) {
  case CK_LDA:
  case CK_VAR:
  case CK_IVAR:
    return 0;

  case CK_CONST:
    return cr->Const_val();

  case CK_OP:
    OPERATOR opr = cr->Opr();
    switch (opr) {
    case OPR_PAREN:
      return Get_const(cr->Opnd(0));

    case OPR_NEG:
      return (- Get_const(cr->Opnd(0)));
      
    case OPR_ADD:
      return Get_const(cr->Opnd(0)) + Get_const(cr->Opnd(1));

    case OPR_SUB:
      return Get_const(cr->Opnd(0)) - Get_const(cr->Opnd(1));

    case OPR_MPY:
      return Get_const(cr->Opnd(0)) * Get_const(cr->Opnd(1));

    }  // switch (opr)
    
  } // switch (iv_expr->Kind())

  Is_True(FALSE, ("Get_const:  improper formatted IV expr."));
  return 0;
}



//  Evaluating the iv_expr of the form  iv * a + b  where a is a CK_CONST.
//  Setting the IV to 1 and all other variables to 0.
//
INT64
IV_EXPR::Find_step(CODEREP *iv_expr)
{
  INT32 kid0;
  INT32 kid1;
  switch (iv_expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
    return 0;

  case CK_IVAR:
    Is_True(Loop()->Invariant_cr(iv_expr),
	    ("Find_step:  Improper formatted IV EXPR."));
    return 0;

  case CK_VAR:
    if (iv_expr == Based_iv())
      return 1;
    else {
      Is_True(Loop()->Invariant_cr(iv_expr),
	      ("Find_step:  Improper formatted IV EXPR."));
      return 0;
    }
    
  case CK_OP:
    OPERATOR opr = iv_expr->Opr();
    switch (opr) {
    case OPR_PAREN:
      return Find_step(iv_expr->Opnd(0));
      
    case OPR_NEG:
      return (- Find_step(iv_expr->Opnd(0)));
      
    case OPR_ADD:
      kid0 = Find_step(iv_expr->Opnd(0));
      kid1 = Find_step(iv_expr->Opnd(1));
      return kid0 + kid1;
      
    case OPR_SUB:
      kid0 = Find_step(iv_expr->Opnd(0));
      kid1 = Find_step(iv_expr->Opnd(1));
      return kid0 - kid1;

    case OPR_MPY:
      kid0 = Find_step(iv_expr->Opnd(0));
      kid1 = Find_step(iv_expr->Opnd(1));
      if (kid0 != 0 && kid1 == 0) 
	return kid0 * Get_const(iv_expr->Opnd(1));
      if (kid1 != 0 && kid0 == 0)
	return kid1 * Get_const(iv_expr->Opnd(0));
      if (kid0 == 0 && kid1 == 0)
	return 0;
      
      Is_True(FALSE, ("Find_step:  Improper formatted IV EXPR."));
      return 0;

    default:
      Is_True(Loop()->Invariant_cr(iv_expr),
	      ("Find_step:  Improper formatted IV EXPR."));
      return 0;  
    }  // switch (opr)
  } // switch (iv_expr->Kind())

  Is_True(FALSE, ("Find_step:  Improper formatted IV EXPR."));
  return 0;
}


// ====================================================================


// A function similar to Expand_expr.  Reset the DONT_PROP flag
// along the expanded expr.
//
void
IVR::Reset_dont_prop(CODEREP *cr, const BB_LOOP *loop)
{
  switch (cr->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
  case CK_IVAR: 
    return;
  case CK_VAR:
    {
      // check if it is volatile
      if (cr->Is_var_volatile())
	return;

    // check if it is a physical register
      ST *s = Opt_stab()->St(cr->Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(cr->Offset())))
	return;
    
      if (loop->Invariant_cr(cr)) // no need to expand loop invariants
	return;
    
      cr->Reset_flag(CF_DONT_PROP);
      if (cr->Defstmt() != NULL &&
	  !cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI))) 
	if (cr->Defstmt()->Rhs()->Propagatable_for_ivr(Opt_stab())) 
	  Reset_dont_prop(cr->Defstmt()->Rhs(), loop);
 
      return;
    }

  case CK_OP:
    for  (INT32 i = 0; i < cr->Kid_count(); i++) 
      Reset_dont_prop( cr->Opnd(i), loop);
    return;
  }
  return;
}


// Expand an expression (try to expand it into a vars
// defined by phi, chi, or is loop-invariant.
//
//  limit is the number of VARs can be expanded.
//
CODEREP *
CODEMAP::Expand_expr(CODEREP *cr, const BB_LOOP *loop, INT32 *limit)
{
  switch (cr->Kind()) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
  case CK_IVAR: 
    return NULL;  // no change
  case CK_VAR:
    if (*limit > 0) {
      // check if it is volatile
      if (cr->Is_var_volatile())
	return NULL;

    // check if it is a physical register
      ST *s = sym->St(cr->Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(cr->Offset())))
	return NULL;

      if (loop->Invariant_cr(cr)) // no need to expand loop invariants
	return NULL;

      if (cr->Is_flag_set(CF_DEF_BY_PHI)) {
	PHI_NODE *phi = cr->Defphi();
	BB_NODE *bb = cr->Defbb();
	CODEREP *identical_rhs = NULL;
	BB_NODE *pred; BB_LIST_ITER bb_iter;
	FOR_ALL_ELEM (pred, bb_iter, Init(bb->Pred())) {
	  CODEREP *tmp = phi->OPND(bb_iter.Idx());
	  STMTREP *defstmt = tmp->Get_defstmt();
	  if (defstmt == NULL || ! OPERATOR_is_scalar_store (defstmt->Opr()) ||
	      tmp->Is_flag_set(CF_DEF_BY_CHI)) {
	    identical_rhs = NULL;
	    break;
	  }
	  if (identical_rhs == NULL) 
	    if (defstmt->Rhs()->Propagatable_for_ivr(Opt_stab()))
	      identical_rhs = defstmt->Rhs();
	    else 
	      break;
	  else if (identical_rhs != defstmt->Rhs()) {
	    identical_rhs = NULL; 
	    break;
	  }
	}
	if (identical_rhs != NULL) {
	  CODEREP *expr = Expand_expr(identical_rhs, loop, limit);
	  --(*limit);
	  if (expr)
	    return expr;
	  else
	    return identical_rhs;
	}
      } else if (cr->Defstmt() != NULL && !cr->Is_flag_set(CF_DEF_BY_CHI)) {
	if (cr->Defstmt()->Rhs()->Propagatable_for_ivr(Opt_stab())) {
	  CODEREP *expr = Expand_expr(cr->Defstmt()->Rhs(), loop, limit);
	  --(*limit);
	  if (expr)
	    return expr;
	  else
	    return cr->Defstmt()->Rhs();
	}
      }
    }
    return NULL;

  case CK_OP:
    {
      CODEREP *newcr = Alloc_stack_cr(cr->Kid_count());
      BOOL need_rehash = FALSE;
      CODEREP *expr;
      FOLD ftmp;
      newcr->Copy(*cr);
      for  (INT32 i = 0; i < cr->Kid_count(); i++) {
	expr = Expand_expr( cr->Opnd(i), loop, limit);
	if (expr) {
	  need_rehash = TRUE;
	  newcr->Set_opnd(i, expr);
	} else
	  newcr->Set_opnd(i, cr->Opnd(i));
      }
      if (need_rehash) {
	expr = ftmp.Fold_Expr(newcr);
	if (expr == NULL)
	  expr = Rehash(newcr);
	return expr;
      }
    }
    return NULL;
  }
  return NULL;
}


// Determine the current coderep for this aux_id.  Search between
// start and stop BB_NODEs.
static CODEREP *
Find_cur_cr(IDTYPE aux_id,
            const BB_NODE *start /*inclusive*/,
            const BB_NODE *stop /*inclusive*/)
{
  PHI_NODE *phi; 
  PHI_LIST_ITER phi_iter;
  BB_NODE *stop2 = stop->Idom();
  for (const BB_NODE *bb=start; bb!=NULL && bb!=stop2; bb=bb->Idom()) {
    STMTREP *stmt;
    STMTREP_ITER stmt_iter(((BB_NODE *)bb)->Stmtlist());
    FOR_ALL_NODE_REVERSE(stmt, stmt_iter, Init()) {
      if (stmt->Has_chi()) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	CHI_LIST *chi_list = stmt->Chi_list();
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  if (cnode->Aux_id() == aux_id)
	    return cnode->RESULT();
	}
      }
      if (OPERATOR_is_scalar_store (stmt->Opr()) &&
	  stmt->Lhs()->Aux_id() == aux_id)
	return stmt->Lhs();
    }
    FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
      if (phi->Aux_id() == aux_id)
	return phi->RESULT();
    }
  }
  return NULL;
}


//  Test if an expression can be propagated through the path starting
//  from src to dest. -- used by IVR
BOOL
CODEREP::Propagatable_along_path(const BB_NODE *dest /* inclusive */,
                                 const BB_NODE *src /* inclusive */) const
{
  CODEREP *cur_cr;

  switch (Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return TRUE;
  case CK_VAR: 
    cur_cr = Find_cur_cr(Aux_id(), dest, src);
    return (cur_cr == NULL || cur_cr == this);
  case CK_IVAR:
    return FALSE;
  case CK_OP:
    for  (INT32 i = 0; i < Kid_count(); i++) 
      if (! Opnd(i)->Propagatable_along_path(dest, src))
	return FALSE;
    if (Opr() == OPR_INTRINSIC_OP
#ifdef KEY
	|| Opr() == OPR_PURE_CALL_OP
#endif
       )
      return FALSE;
    return TRUE;
  }
  return FALSE;
}


//  Test if an expression can be propagated into the loop
//   -- used by IVR
BOOL
CODEREP::Propagatable_into_loop(const BB_LOOP *loop) const
{
  switch (Kind()) {
  case CK_LDA:
  case CK_CONST:
  case CK_RCONST:
    return TRUE;
  case CK_VAR: 
    // check that there is no phi function defining this var in the loop
    { PHI_LIST_ITER phi_iter;
      PHI_NODE     *phi;
      FOR_ALL_ELEM (phi, phi_iter, Init(loop->Header()->Phi_list())) {
	if (phi->Aux_id() == Aux_id())
	  return FALSE;
      }
      return TRUE;
    }
  case CK_IVAR:
    return FALSE;
  case CK_OP:
    for  (INT32 i = 0; i < Kid_count(); i++) 
      if (! Opnd(i)->Propagatable_into_loop(loop))
	return FALSE;
    if (Opr() == OPR_INTRINSIC_OP
#ifdef KEY
	|| Opr() == OPR_PURE_CALL_OP
#endif
#if defined(TARG_IA32) || defined(TARG_X8664)
	|| Opr() == OPR_SELECT
	|| Opr() == OPR_MIN
	|| Opr() == OPR_MAX
	|| Opr() == OPR_MINMAX
#endif
       )
      return FALSE;
    return TRUE;
  }
  return FALSE;
}


BOOL
CODEREP::Propagatable_for_ivr(OPT_STAB *sym) const
{
  switch (kind) {
  case CK_LDA: 
  case CK_CONST: 
  case CK_RCONST:
    return TRUE;
  case CK_VAR:
    {
      // check if it is volatile
      if (Is_var_volatile())
	return FALSE;
      // check if it is a physical register
      ST *s = sym->St(Aux_id());
      if ((ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(Offset())))
	return FALSE;
      return TRUE;
    }
  case CK_IVAR: return FALSE;
  case CK_OP:
    if (OPCODE_is_volatile(Op()))
      return NOT_PROPAGATABLE;
    for  (INT32 i = 0; i < Kid_count(); i++) {
      if (! Opnd(i)->Propagatable_for_ivr(sym))
	return FALSE;
    }
    if (! Op_can_be_propagated(Op(), sym->Phase())) 
      return FALSE;
    // Reference 644395 for situations when INTRINSIC_OP cannot be
    // copy propagated.
    if (Opr() == OPR_INTRINSIC_OP
#ifdef KEY
	|| Opr() == OPR_PURE_CALL_OP
#endif
       )
      return FALSE;
    return TRUE;
  }
  return FALSE;
}


// Generate a loop invariant expression that has the same value as CR by
// either expanding the expression or by making a copy.
// The expr generated will be used in the loop.
// 
CODEREP *
CODEMAP::Convert_to_loop_invar(CODEREP *cr, BB_LOOP *loop)
{
  if (cr->Kind() == CK_VAR && cr->Is_flag_set(CF_IS_ZERO_VERSION)) 
    return NULL;

  if (cr->Kind() == CK_LDA ||
      cr->Kind() == CK_CONST ||
      cr->Kind() == CK_RCONST)
    return cr;

  if (loop->Invariant_cr(cr) &&
      cr->Propagatable_for_ivr(Opt_stab()) &&
      cr->Propagatable_into_loop(loop))
    return cr;
  
  if (cr->Kind() == CK_VAR &&
      !cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
      cr->Defstmt() != NULL) {
	CODEREP *new_expr = cr->Defstmt()->Rhs();
	if (new_expr->Propagatable_for_ivr(Opt_stab()) &&
	    new_expr->Propagatable_into_loop(loop) &&
	    new_expr->Propagatable_along_path(loop->Header()->Idom(),
                                              cr->Defbb()->Idom()))
	  return new_expr;
      }

  // Try to generate a copy of the initial value into a temp
  MTYPE temp_type;
#ifdef KEY
  MTYPE temp_rtype; 
#endif
  if ( cr->Kind() == CK_VAR ) {
    // come up with a preg-sized mtype
    temp_type = TY_mtype(ST_type(MTYPE_To_PREG(cr->Dsctyp())));
#ifdef KEY
    temp_rtype = TY_mtype(ST_type(MTYPE_To_PREG(cr->Dtyp())));
#endif
#ifdef KEY // bug 11467
    if (temp_type == MTYPE_BS)
      temp_type = temp_rtype;
#endif
  }
  else {
    temp_type = cr->Dtyp();
#ifdef KEY
    temp_rtype = cr->Dtyp();
#endif
  }
#ifdef KEY
// Bug 1640
  static INT Temp_Index = 0;
  UINT len = strlen("_temp_") + 17;
  char *new_str = (char *) alloca (len);
  sprintf(new_str, "%s%d", "_temp_", Temp_Index++);
  IDTYPE new_temp = Opt_stab()->Create_preg( temp_type, new_str );
#else
  IDTYPE new_temp = Opt_stab()->Create_preg( temp_type, new_str );
#endif
  Add_new_auxid_to_entry_chis(new_temp, Cfg(), this, Opt_stab());
#ifdef KEY
  CODEREP *new_cr = Add_def(new_temp, 1, NULL, temp_rtype, temp_type,
			    Opt_stab()->Aux_stab_entry(new_temp)->St_ofst(),
			    MTYPE_To_TY(cr->Dtyp()), 0, TRUE);
#else
  CODEREP *new_cr = Add_def(new_temp, 1, NULL, temp_type, temp_type,
			    Opt_stab()->Aux_stab_entry(new_temp)->St_ofst(),
			    MTYPE_To_TY(cr->Dtyp()), 0, TRUE);
#endif

  Insert_var_phi(new_cr, loop->Preheader());

  // perform the copy propagation here to reduce the interaction
  // with the DONT_PROP flag.
  if (cr->Kind() == CK_VAR &&
      !cr->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
      !cr->Is_var_volatile() &&
      cr->Defstmt() != NULL &&
      cr->Defstmt()->Bb() == loop->Preheader() &&
      cr->Defstmt()->Rhs()->Propagatable_for_ivr(Opt_stab())) {

    STMTREP *stmt = cr->Defstmt()->Rhs()->Create_cpstmt(new_cr,
							Mem_pool());
    loop->Preheader()->Insert_stmtrep_before(stmt, cr->Defstmt());

  } else {
    STMTREP *stmt = cr->Create_cpstmt(new_cr, Mem_pool());
    loop->Preheader()->Append_stmtrep(stmt);
  }

  return new_cr;
}


// ====================================================================
//   IVR::Generate_step
// IVR::Ident_all_iv_cands
// ====================================================================


// Generate the step expression from the IV cand (nv - iv)
//
CODEREP*
IVR::Generate_step(CODEREP *nv, CODEREP *iv) const
{
  CODEREP *delta = NULL;
  MTYPE   dtype = nv->Dtyp();
  
  // deal with the simple form (nv = iv + expr || nv = expr + iv)
  if (nv->Kind() == CK_OP) {
    if (nv->Opr() == OPR_ADD) {
      for (INT i = 0; i < nv->Kid_count(); i++)
	if (nv->Get_opnd(i) == iv) {
	  delta = (i == 0) ? nv->Get_opnd(1) : nv->Get_opnd(0);
	  break;
	}
    } else if (nv->Opr() == OPR_SUB &&
               nv->Get_opnd(0) == iv) {
      if (nv->Get_opnd(1)->Kind() == CK_CONST) {
	delta = Htable()->Add_const(dtype,  - nv->Get_opnd(1)->Const_val());
      } else {
	CODEREP *zero = Htable()->Add_const(dtype, 0);
	CODEREP *opnd1 = nv->Get_opnd(1);   // no type conversion
                                            // needed (as in iv -
                                            // opnd1) 
	delta = Htable()->Add_bin_node_and_fold(nv->Op(), zero, opnd1);
      }
    }
  }
  
  
  // The step expression contains the induction variable, either the
  // simplifier is not good enough, or it is not a valid candidate.
  if (delta && delta->Contains(iv))
    return NULL;
  
  return delta;
}

#define IS_MP_LOOP(loop) (loop->Is_flag_set(LOOP_IS_MP) || loop->Is_flag_set(LOOP_IS_PDO))

//  This is the initial screening for all IVs in this loop.
//  The IV must satisfy the following conditions:
//    1. it is in the phi-list of startbb of the loop.
//    2. the phi is not dead
//    3. the phi does not define a zero version
//    4. it must be a scalar variable
//    5. the opnd of the phi is not a zero version
//    6. the increment must be defined by a statement (not chi, phi)
//    7. incr is not a volatile stmt
//    8. the step expression must be an invariant.
//    9. init is not a zero version
//   10. init is not a volatile stmt
//   11. if it's an MP loop, this variable cannot be reduction variable.
void
IVR::Ident_all_iv_cands(const BB_LOOP *loop, const BB_NODE *bb) 
{ 
  BOOL is_mp_loop = IS_MP_LOOP(loop);

  if (bb->Pred()->Len() != 2) return;
  
#ifdef KEY
#ifdef Is_True_On
  static INT32 ivr_cand_idx = 0;
#endif
#endif
  //  Iterate through each phi-node
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (! phi->Live()) continue;
    CODEREP *res = phi->RESULT();
    
    // must be scalar variable
    if (res->Kind() != CK_VAR) continue;

    // screen out the reduction if this is an MP loop
    if (is_mp_loop) {
      AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(res->Aux_id());
      if (sym->Mp_reduction()) continue;
    }
    
    if (res->Is_flag_set(CF_IS_ZERO_VERSION)) {
      Warn_todo("IVR:  deal with zero version as initial value.");
      continue;
    }
    
    BB_NODE *pred;
    BB_LIST_ITER bb_iter;
    FOR_ALL_ELEM (pred, bb_iter, Init(bb->Pred())) {
      
      // skip IV-cand that are zero versions
      CODEREP *incr = phi->OPND(bb_iter.Idx());
      if (incr->Is_flag_set(CF_IS_ZERO_VERSION)) continue;
      
      // must be defined by a statement
      if (incr->Is_flag_set(CF_DEF_BY_CHI))
        continue;
      
      // must be integral type
      // -- it the type of the init is not known, never mind.
      if (!MTYPE_is_integral(incr->Dtyp())) continue;
      
      // Look for the variant opnd to be the incr value
      if (loop->Invariant_cr(incr)) continue;
      
      // the incr cannot be a volatile stmt (i.e., cannot contain
      // volatile var) it is sufficient to check just the incr stmt
      // here because expand_expr will not expand volatiles.
#ifdef KEY 
// BUG 510
      if (!incr->Is_flag_set(CF_DEF_BY_PHI) && incr->Defstmt() &&
	  incr->Defstmt()->Volatile_stmt()) break;
#else
      if (!incr->Is_flag_set(CF_DEF_BY_PHI) &&
	  incr->Defstmt()->Volatile_stmt()) break;
#endif
      
      // Expand the phi opnds (but do not modify the program)
      INT32 limit = WOPT_Enable_IVR_Expand_Limit;
      CODEREP *expr = Htable()->Expand_expr(incr, loop, &limit);
      if (expr == NULL) expr = incr;
      
      // Try to generate a step expression
      CODEREP *step = Generate_step(expr, res);
      // Skip if the step is not invariant expression
      if (step == NULL || !loop->Invariant_cr(step)) break;
      
      CODEREP *init = (bb_iter.Idx() == 0) ? phi->OPND(1) : phi->OPND(0);
      
      if (init->Is_flag_set(CF_IS_ZERO_VERSION)) break;
      
      // init cannot be volatile
      if (init->Defstmt() && init->Defstmt()->Volatile_stmt()) break;

      // the type of the IV is the type of increment expr.
      MTYPE dtype;
      if (incr->Defstmt()) 
	dtype = incr->Defstmt()->Rhs()->Dtyp();
      else
	dtype = incr->Defphi()->OPND(0)->Defstmt()->Rhs()->Dtyp();
      
      // *** update type: make sure the type of init and incr matches
      // TODO: this shouldn't be necessary, if we did it right when we
      // build htable. 
      if (init->Dsctyp() != MTYPE_UNKNOWN && 
	  MTYPE_size_min(init->Dsctyp()) != MTYPE_size_min(incr->Dsctyp()) ) 
	break;
      init = init->Var_type_conversion(Htable(), incr->Dtyp(),
				       incr->Dsctyp(), incr->Lod_ty(),
				       incr->Field_id());

      IV_CAND *new_cand =
        CXX_NEW(IV_CAND(phi, init, incr, step, dtype), Mem_pool());
#ifdef KEY
#ifdef Is_True_On
      if (WOPT_Enable_Ivr_Cand_Limit != -1 && ivr_cand_idx >= WOPT_Enable_Ivr_Cand_Limit)
        break;
      ivr_cand_idx++;
#endif
#endif
      iv_cand_container.push_back(new_cand);
      break;
    }
  }
  if (_trace)
    Print_all_iv_cand(TFile);
}


// ====================================================================
//   IVR::Satisfy_primary_IV
//     Init_expr_cost
//   Primary_IV_preference
// IVR::Choose_primary_IV
// ====================================================================


// Describe the criteria for an IV to be selected as the primary IV
//   1.  size >= 4 bytes (for correctness)
//   2.  must be stride 1 (for correctness)
//   3.  init-value must be small so that it would not overflow
//       also the init-value should be likely to fit in the 
//       immediate field of ILOAD/ISTORE. (for performance)
BOOL
IVR::Satisfy_primary_IV(const IV_CAND *iv_cand, BOOL must_be_I4) const
{
  // Its init value must be propagatable.
  if (iv_cand->Init_value() == NULL)
    return FALSE;
  // size must be larger than I4/U4/I8/U8 
  AUX_STAB_ENTRY *psym =
    Opt_stab()->Aux_stab_entry(iv_cand->Var()->Aux_id());
  if (psym->Byte_size() < 4) {
    if (psym->St() == NULL || ST_sclass(psym->St()) != SCLASS_REG)
      return FALSE;
  }
  if (iv_cand->Step_value()->Kind() != CK_CONST)
    return FALSE;
  if (iv_cand->Step_value()->Const_val() != 1)
    return FALSE;

  // do not choose an unsigned number as primary IV.
  // I4 is best because I4->U4, I4->I8, I4->U8 conversion are noops.
  // I8 is OK when there is no I4 available because I8->U8 is noop.
  if (iv_cand->Dtype() != MTYPE_I4)
    if (must_be_I4)
      return FALSE;
    else if (iv_cand->Dtype() != MTYPE_I8)
      return FALSE;
     
  return TRUE;
}


const INT32 MIN_SCORE = -10000;

// Set CK_IVAR and operators such as DIV to be expensive
// because strength reduction cannot handle these operators.
// They should not be selected if there is other choice.
//
static INT32
Init_expr_cost(CODEREP *cr)
{
  if (cr == NULL) return -MIN_SCORE;

  switch (cr->Kind()) {
  case CK_VAR:
    return 2;
  case CK_CONST:
    if (cr->Const_val() == 0) 
      return 0;
    else
      return 1;
  case CK_IVAR:
    return -MIN_SCORE;
  case CK_OP:
    {
      OPERATOR opr = cr->Opr();
      if (opr == OPR_ADD || opr == OPR_SUB)
	return Init_expr_cost(cr->Opnd(0)) + Init_expr_cost(cr->Opnd(1));
      else if (opr == OPR_NEG)
	return Init_expr_cost(cr->Opnd(0));
      else
	return -MIN_SCORE;
    }
  }
  return -MIN_SCORE;
}


static INT32
Primary_IV_preference(IV_CAND *iv, OPT_STAB *opt_stab)
{
  AUX_ID aux_id = iv->Var()->Aux_id();
  AUX_STAB_ENTRY *psym = opt_stab->Aux_stab_entry(aux_id);
  INT32 score = 1;


  if (!psym->Points_to()->No_alias())  // has alias, unlikely to be dead
    score += 100;

  if (ST_sclass(psym->St()) != SCLASS_REG)
    score += 10;

  score -= Init_expr_cost(iv->Init_value());

  return score > MIN_SCORE ? score : MIN_SCORE;
}


//  Choose an IV to be the primary induction variable based on the
//  following preference: 
//    1.  Must be the index variable if the loop is a DO-loop
//    2.  Must be a strip-1 IV
//    3.  Initial value must be small (to avoid possibility of overflow)
//    4.  Preference:
//        a. Select an aliased variable (because it is unlikely to be
//           deleted) 
//        b. Select an alias-free local variable  (so that no new preg
//            are created) 
//        c. Select a preg.
//        d. Select I4 instead of I8.
IV_CAND*
IVR::Choose_primary_IV(const BB_LOOP *loop)
{
  WN           *index = loop->Index();
  
  vector<IV_CAND*>::iterator iv_cand_iter;

  if (Phase() != MAINOPT_PHASE) {

    if (index != NULL) {

      // PREOPT primary IV selection (either I4 or I8)
      for (iv_cand_iter = iv_cand_container.begin(); 
	   iv_cand_iter != iv_cand_container.end();
	   iv_cand_iter++) {
	IV_CAND *cur_iv = *iv_cand_iter;
	AUX_ID cur_aux_id = cur_iv->Var()->Aux_id();
	if (Opt_stab()->St(cur_aux_id) == WN_sym(index) &&
	    Opt_stab()->St_ofst(cur_aux_id) == WN_offset(index) &&
	    Satisfy_primary_IV(cur_iv, FALSE)) {
	  cur_iv->Set_is_primary();
	  return cur_iv;
	}
      }
      return NULL;
    } else {
      // generate a primary IV
      STMTREP *stmt = loop->End()->Last_stmtrep();
      if (stmt == NULL || stmt->Rhs() == NULL || stmt->Rhs()->Kind() != CK_OP)
	return NULL;
      
      const OPCODE test_opc = stmt->Rhs()->Op();
      MTYPE   mtype = MTYPE_I4;
      if (OPCODE_is_compare(test_opc) && 
	  MTYPE_size_min(OPCODE_desc(test_opc)) > 32) {
	mtype = MTYPE_I8;
      }

      IDTYPE new_temp = Opt_stab()->Create_preg( mtype, "whiledo_var" );

#ifdef KEY // bug 5778
      if (ST_class(Opt_stab()->St(new_temp)) != CLASS_PREG &&
	  Phase() != MAINOPT_PHASE && loop->Body()->MP_region()) {
	// add a WN_PRAGMA_LOCAL pragma to the enclosing OMP parallel region
	BB_NODE *regionbb = loop->Body();
	while (regionbb != NULL && regionbb->Kind() != BB_REGIONSTART)
	  regionbb = Cfg()->Find_enclosing_parallel_region_bb(regionbb);
	if (regionbb != NULL) { // there is an enclosing OMP parallel region
	  WN *pragmawn = WN_CreatePragma(WN_PRAGMA_LOCAL, 
	  				 Opt_stab()->St(new_temp), 0, 0);
          STMTREP *pragmastmt = regionbb->Add_stmtnode(pragmawn, 
						       Htable()->Mem_pool());
	  pragmastmt->Set_orig_wn(pragmawn);
	}
      }
#endif

      Add_new_auxid_to_entry_chis(new_temp, Cfg(), Htable(), Opt_stab());

      CODEREP *init_cr = Htable()->Add_def(new_temp, 1, NULL, mtype, mtype,
					   Opt_stab()->Aux_stab_entry(new_temp)->St_ofst(),
					   MTYPE_To_TY(mtype), 0, TRUE);
      
      CODEREP *phi_cr = Htable()->Add_def(new_temp, 2, NULL, mtype, mtype,
					  Opt_stab()->Aux_stab_entry(new_temp)->St_ofst(),
					  MTYPE_To_TY(mtype), 0, TRUE);

      CODEREP *incr_cr = Htable()->Add_def(new_temp, 3, NULL, mtype, mtype,
					   Opt_stab()->Aux_stab_entry(new_temp)->St_ofst(),
					   MTYPE_To_TY(mtype), 0, TRUE);

      PHI_NODE *phi = loop->Header()->Phi_list()->
	New_phi_node(new_temp, Htable()->Ssa()->Mem_pool(), loop->Header());

      phi_cr->Set_flag(CF_DEF_BY_PHI);
      phi_cr->Set_defphi(phi);
      phi->Reset_dse_dead();
      phi->Reset_dce_dead();
      phi->Set_res_is_cr();
      phi->Set_live();
      phi->Set_result(phi_cr);
      phi->Set_incomplete();
      
      CODEREP *init_value = Htable()->Add_const(mtype, 0);
      STMTREP *init_stmt = init_value->Create_cpstmt(init_cr, Htable()->Mem_pool());
      Loop()->Preheader()->Append_stmtrep(init_stmt);
      init_stmt->Set_bb(Loop()->Preheader());
      init_stmt->Set_linenum(Loop()->Preheader()->Linenum());

      OPCODE addop = OPCODE_make_op(OPR_ADD, mtype, MTYPE_V);
      CODEREP *step = Htable()->Add_const(mtype, 1);
      CODEREP *incr_rhs = Htable()->Add_bin_node_and_fold(addop, phi_cr, step);

      STMTREP *incr_stmt =
	incr_rhs->Create_cpstmt(incr_cr, Htable()->Mem_pool());
// Bug 2569
# ifdef KEY
      Loop()->Loopback()->Append_stmt_before_branch(incr_stmt);
# else
      Loop()->Loopback()->Append_stmtrep(incr_stmt);
# endif
      incr_stmt->Set_bb(Loop()->Loopback());

      Htable()->Enter_var_phi_hash(phi);
      Htable()->Insert_var_phi(phi->RESULT(), phi->Bb());
      Htable()->Insert_var_phi(incr_stmt->Lhs(), incr_stmt->Bb());
      Htable()->Insert_var_phi(init_stmt->Lhs(), init_stmt->Bb());

      phi->Set_opnd(Loop()->Preheader_pred_num(), init_cr);
      phi->Set_opnd(Loop()->Loopback_pred_num(),  incr_cr);

      IV_CAND *new_cand =
        CXX_NEW(IV_CAND(phi, init_cr, incr_cr, step, mtype), Mem_pool());
      new_cand->Set_is_primary();
      new_cand->Set_init_value( init_value );

      ivr_generated_primary = TRUE;

      iv_cand_container.push_back(new_cand);

      if (_trace) {
	fprintf(TFile, "IVR: generate primary IV with aux-id %d\n", new_temp);
	fprintf(TFile, "IVR: insert phi at BB%d, init at BB%d, incr at BB%d\n",
		phi->Bb()->Id(), init_stmt->Bb()->Id(), incr_stmt->Bb()->Id());
      }

      return new_cand;
    }

  } else {

    // MAINOPT primary IV selection

    INT32 max_score = MIN_SCORE - 1;
    IV_CAND *selected_iv = NULL;

    for (iv_cand_iter = iv_cand_container.begin(); 
	 iv_cand_iter != iv_cand_container.end();
	 iv_cand_iter++) {
      IV_CAND *cur_iv = *iv_cand_iter;
      if (Satisfy_primary_IV(cur_iv, TRUE)) {
	INT32 score = Primary_IV_preference(cur_iv, Opt_stab());
	if (score > max_score) {
	  max_score = score;
	  selected_iv = cur_iv;
	}
      }
    }
    
    if (selected_iv != NULL) 
      return selected_iv;  // found an I4 primray IV
    
    if (WOPT_Enable_I8_Primary_IV) {  // try to find an I8 primary IV
      max_score = MIN_SCORE - 1;
      for (iv_cand_iter = iv_cand_container.begin(); 
	   iv_cand_iter != iv_cand_container.end();
	   iv_cand_iter++) {
	IV_CAND *cur_iv = *iv_cand_iter;
	if (Satisfy_primary_IV(cur_iv, FALSE)) {
	  INT32 score = Primary_IV_preference(cur_iv, Opt_stab());
	  if (score > max_score) {
	    max_score = score;
	    selected_iv = cur_iv;
	  }
	}
      }
    }

    return selected_iv;   // didn't find any
  }
}


// ====================================================================
//   IVR::Replace_IV_with_invar
//   IVR::Compute_trip_count
// IVR::Determine_trip_IV_and_exit_count
// ====================================================================


//  Replace all occurence of the IV with the 'replacement' value.
//
CODEREP *
IVR::Replace_IV_with_invar(CODEREP *iv_expr,
                                CODEREP *based_iv,
                                CODEREP *replacement)
{
  CODEREP *kid0;
  CODEREP *kid1;
  switch (iv_expr->Kind()) {
  case CK_LDA:
  case CK_CONST:
    return NULL;
    
  case CK_VAR:
    if (iv_expr == based_iv)
      return replacement;
    else
      return NULL;
    
  case CK_OP:
    switch (iv_expr->Opr()) {
    case OPR_PAREN:
    case OPR_NEG:
      kid0 = Replace_IV_with_invar(iv_expr->Opnd(0), based_iv, replacement);
      if (kid0 != NULL) 
	return Htable()->Add_unary_node(iv_expr->Op(), kid0);
      return NULL;
      
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
      kid0 = Replace_IV_with_invar(iv_expr->Opnd(0), based_iv, replacement);
      kid1 = Replace_IV_with_invar(iv_expr->Opnd(1), based_iv, replacement);
      if (!kid0 && !kid1)
	return NULL;
      if (kid0 && !kid1) 
	return Htable()->Add_bin_node_and_fold(iv_expr->Op(), kid0,
                                               iv_expr->Opnd(1)); 
      if (kid1 && !kid0)
	return Htable()->Add_bin_node_and_fold(iv_expr->Op(),
                                               iv_expr->Opnd(0),
                                               kid1); 

      if (iv_expr->Opr() == OPR_MPY) {
	Is_True(FALSE,
		("Replace_IV_with_invar:  improper formatted IV expr."));
      }

      return Htable()->Add_bin_node_and_fold(iv_expr->Op(), kid0, kid1);

    default:
      return NULL;
    } // switch 

  case CK_IVAR:
    return NULL; // must be loop invariant.

  } // switch (iv_expr->Kind())
  
  Is_True(FALSE, ("Replace_IV_with_invar:  improper formatted IV expr."));
  return NULL;
}


// Determine the trip count expression based on the comparison, step
// and init value.
//   -- assuming loops executes at least once.
//   -- negative trip count means cannot identify trip count.
//   -- trip count expression is always MTYPE_I4.
//
CODEREP *
IVR::Compute_trip_count(const OPCODE cmp_opc,
			BB_LOOP *loop,
			CODEREP *bound,
			CODEREP *var,
			CODEREP *init,
			CODEREP *step,
			CODEREP *iv_expr,
			CODEREP *based_iv,
			INT64    new_step,
			BOOL     test_at_entry,
			STMTREP *cmp_stmt,
			BOOL     swapped,
			IV_CAND *primary_cand)
{
  // In order to deal with more complicated comparisons, e.g.
  //      iv * a + b < c
  // The concept of IV variable is generalized to IV expr.
  // IV expr is defined by the following grammar:
  //   iv_expr --> iv_var |
  //               iv_expr +- invariant  |
  //               iv_expr * ck_const |
  //               - iv_expr |
  //               (iv_expr)
  //
  // IV_expr represents an composite inducation variable that is based
  // on the simple induction variable in its expr.
  //  Suppose iv_expr = based_iv * a + b < c
  //  Suppose the init(iv), step(iv) and bound(iv) represents the init, 
  //  step, and bound of the simple IV.
  //  Then the init, step, and bound of the composite IV is
  //    init(iv_expr) = init(based_iv) * a + b
  //    step(iv_expr) = step(based_iv) * a
  //    bound(iv_expr) = bound(based_iv)
  //

  
  BOOL perform_slt_opt;
  BOOL perform_loop_exit_opt;
  CODEREP *const NO_TRIP_COUNT = NULL;

  BOOL need_guard = ((loop->Flags() & LOOP_PRE_REPEAT) == LOOP_PRE_REPEAT ||
		     (loop->Flags() & LOOP_REPEAT) == LOOP_REPEAT);

  if (iv_expr != based_iv) {
    // disable slt and loop_exit optimizations
    //   - slt does not know how to deal with composite IV yet.
    //   - loop_exit should have already finished in the preopt phase
    //     where the composite IV is generated as byproduct of loop
    //     normalization.
    //
    perform_slt_opt = (new_step == 1 || new_step == -1);
    perform_loop_exit_opt = FALSE;

    // update init, step, bound
    
    //  replace occurence of based_iv in iv_expr with init.
    init = Replace_IV_with_invar(iv_expr, based_iv, init);
    Is_True(new_step != 0, ("Compute_trip_count: step is 0."));

    OPCODE mulop = OPCODE_make_op(OPR_MPY, step->Dtyp(), MTYPE_V);
    step = Htable()->Add_bin_node_and_fold(mulop, step,
                                           Htable()->Add_const(step->Dtyp(),
                                                               new_step));

    if (_trace) {
      fprintf(TFile, "Perform trip count computation for composite IV.\n");
      fprintf(TFile, "Composite iv_expr: \n");
      iv_expr->Print(0,TFile);
      fprintf(TFile, "Composite based_iv:  \n");
      based_iv->Print(0,TFile);
      fprintf(TFile, "Composite init:  \n");
      init->Print(0,TFile);
      fprintf(TFile, "Composite step:  \n");
      step->Print(0,TFile);
      fprintf(TFile, "Composite bound: \n");
      bound->Print(0,TFile);
    }
    
  } else {
    perform_loop_exit_opt = TRUE;
    perform_slt_opt = TRUE;
  }

  // Perform cannonicalization of comparisons.
  // The following sections expects the test condition (TRUEBR) to be 
  // in the form trip_iv < y or trip_iv > y for test-at-entry loop, and
  // trip_iv >= y or trip_iv <= y for test-at-exit loop.
  //
  //  adjustment:      What is needed to add to y to convert into the
  //                   above form?
  //  direction:       Based on the comparison, should the trip IV be
  //                   increasing(UP),  
  //                   decreasing(DOWN), or doesn't matter(DONT_CARE)?
  //  allow_reminder:  Is remainder allowd if the step is not 1?
  //
  //  need_slt_opt:    Should we perform slt optimization?
  //  slt_adjustment:  The comparision can be simply changed to NE/EQ if
  //                   the comparsion is LT,GT.  What is the adjustment added
  //                   the trip_bound side to convert the LE,GE into LT,GT. 
  //                   For example,  x < y ==> x != y,
  //                                 x <= y ==> x != y + 1 * step.
  // 
  INT32 adjustment;
  INT32 slt_adjustment;  // delta for the bound expr if slt opt is performed
  BOOL  allow_remainder;
  enum {UP, DOWN, DONT_CARE} direction;
#ifndef TARG_X8664 
  BOOL  need_slt_opt = TRUE;
#else
  BOOL  need_slt_opt = FALSE;
#endif
  BOOL  need_inv_slt_opt = FALSE;
  BOOL  apply_cxx_pointer_rule = FALSE;
  if (test_at_entry) {
    // for test at entry
    // the test condition is the exit condition, 
    // its negation is the loop-back condition.
    // the trip count formula is based on <= or >=, compute the adjustment.
    switch (OPCODE_operator(cmp_opc)) {
    case OPR_GE:
      direction = UP;
      allow_remainder = TRUE;
      adjustment = -1;  // !(x >= y) ==> x < y ==> x <= y - 1
      break;
    case OPR_LE:
      direction = DOWN;
      allow_remainder = TRUE;
      adjustment = 1;   // !(x <= y) ==> x > y ==> x >= y + 1
      break;
    case OPR_LT:
      direction = DOWN;
      allow_remainder = TRUE;
      adjustment = 0;   // !(x < y) ==> x >= y
      break;
    case OPR_GT:
      direction = UP;
      allow_remainder = TRUE;
      adjustment = 0;   // !(x > y) ==> x <= y
      break;
    case OPR_EQ:
      direction = DONT_CARE;
      allow_remainder = FALSE;
      adjustment = 0;  //  !(x == y) ==> x != y
      need_slt_opt = FALSE;
      apply_cxx_pointer_rule = TRUE;
      //
      //  Patch pv447872.  See comments at the beginning of this file.
      //
      //  Do not perform inverse slt for loops that may exit early.
      //     need_inv_slt_opt = TRUE;
      //
      need_inv_slt_opt = !loop->Exit_early();
      break;
    case OPR_NE:
    default:
      return NO_TRIP_COUNT;
    }
  } else {
    // for test at exit
    // the test condition are loop-back conditions
    switch (OPCODE_operator(cmp_opc)) {
    case OPR_GE:
      direction = DOWN;
      allow_remainder = TRUE;
      adjustment = 0;  //  x >= y
      slt_adjustment = -1;
      break;
    case OPR_LE:
      direction = UP;
      allow_remainder = TRUE;
      adjustment = 0;   //  x <= y
      slt_adjustment = 1;
      break;
    case OPR_LT:
      direction = UP;
      allow_remainder = TRUE;
      adjustment = -1;  //  x < y ==> x <= y - 1
      slt_adjustment = 0;
      break;
    case OPR_GT:
      direction = DOWN;
      allow_remainder = TRUE;
      adjustment = 1;   //  x > y ==> x >= y + 1
      slt_adjustment = 0;
      break;
    case OPR_NE:
      direction = DONT_CARE;
      allow_remainder = FALSE;
      adjustment = 0;  //  x != y
      need_slt_opt = FALSE;
      apply_cxx_pointer_rule = TRUE;
      break;
    case OPR_EQ:
#ifdef KEY // bug 10986: special case where trip count is 1
      if (init == bound &&
	  step->Kind() == CK_CONST && step->Const_val() != 0)
        return Htable()->Add_const(MTYPE_I4, 1);
      // fall thru
#endif
    default:
      return NO_TRIP_COUNT;
    }
  }

  // Compute if the (bound - init) is divisible by step.
  // Remember the result here.  The result will be used in slt opt
  // and computing trip count when the comparison operator is NE or EQ.
  // diff_divisable_by_step is TRUE  if (bound - init) % step == 0.
  //
  BOOL diff_divisable_by_step =
    bound->Divisable(step, Opt_stab()) && init->Divisable(step, Opt_stab());
  if (apply_cxx_pointer_rule &&
      PU_src_lang(Get_Current_PU()) == PU_CXX_LANG &&
      !loop->Exit_early() &&
      !diff_divisable_by_step &&
      step->Kind() == CK_CONST &&
      var->Divisable(step, Opt_stab())) {
    diff_divisable_by_step = TRUE;
  }
  if (!diff_divisable_by_step &&
      step->Kind() == CK_CONST &&
      bound->Kind() == CK_CONST && 
      init->Kind() == CK_CONST &&
      step->Const_val() != 0) {
    INT64 diff = bound->Const_val() - init->Const_val();
    diff_divisable_by_step = ((diff % step->Const_val()) == 0);
  }


  // entry test is always valid with DO loops!
  BOOL valid_entry_test = loop->Test_at_entry() || loop->Has_entry_guard();

  // more testing with WHILE loops and REPEAT loops
  if (!valid_entry_test) {
    // generated init cmp_opc bound
    CODEREP *entry_test =
      Htable()->Add_bin_node_and_fold(cmp_opc, init, bound, cmp_stmt->Bb());

    if (entry_test->Kind() == CK_CONST) {
      if (entry_test->Const_val() != 0)  // TRUE
	valid_entry_test = TRUE;
    } else {
      // TODO:  should make this independent on the CFG layout!
      if (Loop()->Preheader()->Pred()->Len() == 1) {
	BB_NODE *bb = Loop()->Preheader()->Pred()->Node();
	STMTREP *br = bb->Branch_stmtrep();
	if (bb->Next() == Loop()->Preheader() &&   // entry test falls thru
                                              // to do head 
	    br != NULL && 
	    br->Op() == OPC_FALSEBR) {
	  CODEREP *rhs = br->Rhs();
	  if (rhs->Kind() == CK_OP) {
            switch (rhs->Kid_count()) {
            case 1:
              rhs = Htable()->Add_unary_node(rhs->Op(), rhs->Opnd(0));
              break;
            case 2:
              rhs = Htable()->Add_bin_node_and_fold(rhs->Op(),
                                                    rhs->Opnd(0),
                                                    rhs->Opnd(1));
              break;
            default:
              break;
            }
          }
	  if (rhs == entry_test) // same condition
	    valid_entry_test = TRUE;
	}
      }
    }
  }

  // still can't find the entry test
  if (!valid_entry_test && 
      init->Kind() == CK_VAR &&
      !init->Is_flag_set((CR_FLAG)(CF_DEF_BY_PHI|CF_DEF_BY_CHI)) &&
      init->Defstmt() != NULL) {
    // generated init cmp_opc bound
    CODEREP *entry_test =
      Htable()->Add_bin_node_and_fold(cmp_opc, init->Defstmt()->Rhs(),
                                      bound, cmp_stmt->Bb());
    if (entry_test->Kind() == CK_CONST) {
      if (entry_test->Const_val() != 0)  // TRUE
	valid_entry_test = TRUE;
    } else {
      // TODO:  should make this independent on the CFG layout!
      if (Loop()->Preheader()->Pred()->Len() == 1) {
	BB_NODE *bb = Loop()->Preheader()->Pred()->Node();
	STMTREP *br = bb->Branch_stmtrep();
	if (bb->Next() == Loop()->Preheader() &&   // entry test falls thru
                                              // to do head 
	    br != NULL && 
	    br->Op() == OPC_FALSEBR) {
	  CODEREP *rhs = br->Rhs();
	  if (rhs->Kind() == CK_OP)
            switch (rhs->Kid_count()) {
            case 1:
              rhs = Htable()->Add_unary_node(rhs->Op(), rhs->Opnd(0));
              break;
            case 2:
              rhs = Htable()->Add_bin_node_and_fold(rhs->Op(),
                                                    rhs->Opnd(0),
                                                    rhs->Opnd(1));
              break;
            default:
              break;
            }
	  if (rhs == entry_test) // same condition
	    valid_entry_test = TRUE;
	}
      }
    }
  }

  // If the entry test is missing, override this two flags!
  // Fix 354340: check if it is OK to perform slt opt, i.e., if the
  //             entry and exit test matches 
  if (!valid_entry_test) {
    perform_slt_opt = FALSE;
    need_guard = TRUE;
  }

  //  The loop form have been normalized to   for (i = a; i <= b; i += c).
  //  The trip count can be represented as  (b - a + c) / c.
  //  Since a == init,  b == bound + adjustment, c == step,
  //  The trip count is (bound - init + step + adjustmen) / step.
  //     
  CODEREP *trip_count = NO_TRIP_COUNT;
#if defined(TARG_SL) || defined(TARG_PPC32)
  if (!diff_divisable_by_step) {
    if (step->Kind() == CK_CONST && step->Const_val() < 0) {
      return NO_TRIP_COUNT;
    }
  }
#endif

  // Always compute trip expr in signed integer.  This prevents a
  // problem if the step is unsigned (-1).  In the subsequent code,
  // there is a call to fixup_type to fixup the opnd type to be
  // compatible with the tripcount type.  The unsigned(-1) will be
  // converted to signed(-1).  If the trip count type is not signed,
  // the trip count will be zero because any expression divided by
  // unsigned(-1) becomes zero.
  //
  // MTYPE   tripcount_type = primary_cand->Dtype();
  // Is_True(tripcount_type == MTYPE_I4 || tripcount_type == MTYPE_I8,
  //	  ("IVR: trip count type is wrong."));
  MTYPE   tripcount_type;
  if (primary_cand == NULL)
    tripcount_type = (MTYPE_size_min(based_iv->Dtyp()) ==
                      MTYPE_size_min(MTYPE_I4)) ? MTYPE_I4 : MTYPE_I8;
  else
    tripcount_type = (MTYPE_size_min(primary_cand->Dtype()) ==
                      MTYPE_size_min(MTYPE_I4)) ? MTYPE_I4 : MTYPE_I8;

  if (allow_remainder) {
    if (bound->Kind() == CK_CONST && 
	init->Kind() == CK_CONST && 
	step->Kind() == CK_CONST) {
      if (step->Const_val() != 0) {
	INT64 diff =
          bound->Const_val() - init->Const_val() + step->Const_val() +
            adjustment;
	trip_count =
          Htable()->Add_const(tripcount_type, (diff / step->Const_val()));
      }
    } else if ((step->Kind() == CK_CONST && step->Const_val() != 0) ||
	       WOPT_Enable_Generate_Trip_Count == 2 ||
	       (WOPT_Enable_Generate_Trip_Count == 1 &&
		IS_FORTRAN &&
		(loop->Flags() & LOOP_DO) != 0)) {
      //
      //  Either the command line options guarantees the step is non-zero
      //  or the language (Fortran DO-loops) guarantees that.
      //
		 
      //  The tmp_cr = (bound - init + step + adjustment) computation
      //  is carried using the dtype of the comparsion.
      //  The tmp_cr / step is carried using tripcount_type (MTYPE_I4). 
      //  This is important because the type of step can be unsigned (-1).
      //
      MTYPE  dtype = OPCODE_desc(cmp_opc);
      OPCODE addop = OPCODE_make_op(OPR_ADD, dtype, MTYPE_V);
      OPCODE subop = OPCODE_make_op(OPR_SUB, dtype, MTYPE_V);
      CODEREP *one = Htable()->Add_const(dtype, 1);

      if (step->Kind() == CK_CONST && step->Const_val() < 0) {

	// calculate  (init - bound + (-step) - adjustment) / (-step)
	CODEREP *neg_step = Htable()->Add_const(dtype, - step->Const_val());

	CODEREP *tmp_cr = Htable()->Add_bin_node_and_fold(subop, init,
                                                          bound);
	tmp_cr = Htable()->Add_bin_node_and_fold(addop, tmp_cr, neg_step);
	if (adjustment == 1)
	  tmp_cr = Htable()->Add_bin_node_and_fold(subop, tmp_cr, one);
	else if (adjustment == -1)
	  tmp_cr = Htable()->Add_bin_node_and_fold(addop, tmp_cr, one);
		   
	OPCODE divop = OPCODE_make_op(OPR_DIV, tripcount_type, MTYPE_V);
	trip_count =
          Htable()->Add_bin_node_and_fold(divop,
                                          tmp_cr->Fixup_type(tripcount_type,
                                                             Htable()),
                                          neg_step->Fixup_type(tripcount_type,
                                                               Htable()));

      } else {

	// calculate   (bound - init + step + adjustment) / step
		   
	CODEREP *tmp_cr =
          Htable()->Add_bin_node_and_fold(subop, bound, init);
	tmp_cr = Htable()->Add_bin_node_and_fold(addop, tmp_cr, step);
	if (adjustment == 1)
	  tmp_cr = Htable()->Add_bin_node_and_fold(addop, tmp_cr, one);
	else if (adjustment == -1)
	  tmp_cr = Htable()->Add_bin_node_and_fold(subop, tmp_cr, one);
		   
#ifdef TARG_SL
        //step > 0, if tmp_cr is unsigned, set tripcount_type unsigned
        tripcount_type = (tmp_cr->Dtyp() == MTYPE_U4) ? MTYPE_U4 : tripcount_type;
#endif		   
	OPCODE divop = OPCODE_make_op(OPR_DIV, tripcount_type, MTYPE_V);
	trip_count =
          Htable()->Add_bin_node_and_fold(divop,
                                          tmp_cr->Fixup_type(tripcount_type,
                                                             Htable()),
                                          step->Fixup_type(tripcount_type,
                                                           Htable()));
      }
    }
  } else {
    Is_True(adjustment == 0,
            ("IVR: when allow_remainder is FALSE, adjustment must be 0."));

    // Since adjustment is 0.  The expression (bound - init + step +
    // adjustment) is divisable by step if (bound - init) is divisable
    // by step.

    if (diff_divisable_by_step) {
      MTYPE  dtype = OPCODE_desc(cmp_opc);

      if (step->Kind() == CK_CONST && step->Const_val() < 0) {
	// (init - bound) / (-step)
	OPCODE subop = OPCODE_make_op(OPR_SUB, dtype, MTYPE_V);
	CODEREP *diff = Htable()->Add_bin_node_and_fold(subop, init, bound);
#ifdef KEY // bug 3738
	if (MTYPE_byte_size(var->Dsctyp()) < 4) {
	  diff = Htable()->Add_unary_node(
	  			OPCODE_make_op(OPR_CVTL, dtype, MTYPE_V), diff);
	  diff->Set_offset(MTYPE_size_min(var->Dsctyp()));
	}
#endif
	CODEREP *neg_step = Htable()->Add_const(tripcount_type, -
                                                step->Const_val());

	OPCODE divop = OPCODE_make_op(OPR_DIV, tripcount_type, MTYPE_V);
	trip_count =
          Htable()->Add_bin_node_and_fold(divop, 
                                          diff->Fixup_type(tripcount_type,
                                                           Htable()), 
                                          neg_step);
      } else {
	//  (bound - init) / step
	OPCODE subop = OPCODE_make_op(OPR_SUB, dtype, MTYPE_V);
	CODEREP *diff = Htable()->Add_bin_node_and_fold(subop, bound, init);

	OPCODE divop = OPCODE_make_op(OPR_DIV, tripcount_type, MTYPE_V);
	trip_count =
          Htable()->Add_bin_node_and_fold(divop, 
                                          diff->Fixup_type(tripcount_type,
                                                           Htable()), 
                                          step->Fixup_type(tripcount_type,
                                                           Htable()));
      }
    }
  }

  if (trip_count) {
    CODEREP *tmp = Htable()->Canon_rhs(trip_count);
    if (tmp) trip_count = tmp;
  }

  if (_trace) {
    fprintf(TFile,
	    "IVR   direction is %s; allow remainder is %s;"
            "adjustment = %d;\n"
	    "IVR   slt_adjustment = %d; divisable by step = %s;"
            "need_guard = %s; valid_entry_test = %s.\n",
	    direction == UP ? "UP" : "DOWN",
	    allow_remainder ? "T" : "F",
	    adjustment,
	    slt_adjustment,
	    diff_divisable_by_step ? "T" : "F",
	    need_guard ? "T" : "F",
	    valid_entry_test ? "T" : "F");
    fprintf(TFile, "Bound: \n");
    bound->Print(0, TFile);
    fprintf(TFile, "Init: \n");
    init->Print(0, TFile);
    if (trip_count) {
      fprintf(TFile, "Trip count: \n");
      trip_count->Print(0, TFile);
    }
    else
      fprintf(TFile, "Trip count: NULL\n");
  }

  // check if the direction change makes any sense
  if (step->Kind() == CK_CONST) {
    switch (direction) {
    case UP:
      if (step->Const_val() < 0) return NO_TRIP_COUNT;
      break;
    case DOWN:
      if (step->Const_val() > 0) return NO_TRIP_COUNT;
      break;
    case DONT_CARE:
      break;
    }
  }

  // Fix 645056:
  //  IVR computes the trip count of loop of the form
  //   i=a
  //   do {
  //     i++;
  //   } while (i != b)
  //
  //  by deriving it from
  //
  //   i=a
  //   while (i != b) {
  //     i++;
  //   } 
  //  trip_count = b - a; and then add 1 to trip count.
  //  This is wrong when a == b.
  if (need_guard) {
    if (trip_count && 
	trip_count->Kind() == CK_CONST &&
	trip_count->Const_val() == 0)
      return NO_TRIP_COUNT;
    if (init == bound) 
      // in case the simplifier failed to simplify x - x to zero.
      return NO_TRIP_COUNT;
  }
  
  // *********************************************************************
  //    Should we perform slt optimization
  // *********************************************************************

  // Do not perform slt opt for preopt
  if (Phase() == MAINOPT_PHASE && !test_at_entry && need_slt_opt &&
      perform_slt_opt) {

    Is_True(cmp_stmt->Opr() == OPR_TRUEBR, 
	    ("slt optimization only understands TRUEBR."));

    OPCODE slt_cmp_opc = OPCODE_make_op(OPR_NE, OPCODE_rtype(cmp_opc),
                                        OPCODE_desc(cmp_opc)); 
    CODEREP *new_cond = NULL;
    MTYPE  dtype = OPCODE_desc(cmp_opc);

    if (diff_divisable_by_step) {
      CODEREP *trip_var;
      CODEREP *trip_bound;
      CODEREP *cond = cmp_stmt->Rhs();
      // if the opcode are different, then there must be a swapping earlier
      // in order to swap the trip IV to the LHS of the comparison.
      if (!swapped) {
	trip_var = cond->Opnd(0);
	trip_bound = cond->Opnd(1);
      } else {
	trip_var = cond->Opnd(1);
	trip_bound = cond->Opnd(0);
      }

      if (iv_expr == based_iv)
	Is_True(trip_var->Kind() == CK_VAR, ("trip var is not CK_VAR."));

      OPCODE addop = OPCODE_make_op(OPR_ADD, dtype, MTYPE_V);
      if (slt_adjustment == 1)
	trip_bound = Htable()->Add_bin_node_and_fold(addop,
                                                     trip_bound,
                                                     step);
      else if (slt_adjustment == -1)
	trip_bound = Htable()->Add_bin_node_and_fold(addop,
                                                     trip_bound,
                                                     step);
      new_cond = Htable()->Add_bin_node_and_fold(slt_cmp_opc,
                                                 trip_var,
                                                 trip_bound,
                                                 cmp_stmt->Bb());
    }
    //  the trip count is a constant, but (bound - init) is not a
    //  multiple of step. can adjust the upper bound to make it
    //  divisable
    else if (step->Kind() == CK_CONST) {
      Warn_todo("SLT: adjust the upper bound to make"
		" (bound - init) divisable by the step.");
    }

    // SLT optimization:  update the last statement with new_cond
    if (new_cond) {
      cmp_stmt->Set_rhs(new_cond);
      Inc_slt_counter();
    }
  }

  // ************************************************************************
  //    Should perform inverse slt opt
  // ************************************************************************
  
  if ((Phase() == PREOPT_PHASE || 
       Phase() == PREOPT_LNO_PHASE ||
       Phase() == PREOPT_IPA0_PHASE ||
       Phase() == PREOPT_IPA1_PHASE) && 
      test_at_entry && need_inv_slt_opt && perform_slt_opt &&
      diff_divisable_by_step && step->Kind() == CK_CONST &&
      cmp_stmt->Opr() == OPR_FALSEBR) {
    
    CODEREP *trip_var;
    CODEREP *trip_bound;
    CODEREP *cond = cmp_stmt->Rhs();
    // if the opcode are different, then there must be a swapping earlier
    // in order to swap the trip IV to the LHS of the comparison.
    if (!swapped) {
      trip_var = cond->Opnd(0);
      trip_bound = cond->Opnd(1);
    } else {
      trip_var = cond->Opnd(1);
      trip_bound = cond->Opnd(0);
    }
    
    // Test if trip_var is a pointer or signed.
    TY_IDX ty = trip_var->Kind() == CK_VAR ? trip_var->Lod_ty() : 0;
    if (WOPT_Enable_Aggressive_Doloop_Promotion ||
	MTYPE_is_signed(OPCODE_desc(cmp_opc)) ||   // not unsigned value
	(ty != 0 && TY_kind(ty) == KIND_POINTER)) { // or a pointer
      
      OPCODE inv_slt_cmp_opc =
	OPCODE_make_op(step->Const_val() > 0 ? OPR_LT : OPR_GT,
		       OPCODE_rtype(cmp_opc), OPCODE_desc(cmp_opc));
      CODEREP *new_cond =
        Htable()->Add_bin_node_and_fold(inv_slt_cmp_opc, trip_var,
                                        trip_bound,
                                        cmp_stmt->Bb());

    // SLT optimization:  update the last statement with new_cond
      if (new_cond) {
	cmp_stmt->Set_rhs(new_cond);
      }
    }    
  }
  
  if (trip_count == NO_TRIP_COUNT) return NO_TRIP_COUNT;

  // ************************************************************************
  //    Should we generate a MIN(trip_count,1) to guard the trip count expr
  // ************************************************************************
  if (need_guard) {
    OPCODE min_op = OPCODE_make_op(OPR_MAX, trip_count->Dtyp(), MTYPE_V);
    CODEREP *one = Htable()->Add_const(trip_count->Dtyp(), 1);
    trip_count = Htable()->Add_bin_node_and_fold(min_op, trip_count, one);
  }

  // ************************************************************************
  //    Generate a trip-count to attach to the LOOP_INFO?
  // ************************************************************************
  
  if (Phase() == MAINOPT_PHASE) {
    // Generate an EVAL statement for the trip count expression if the
    // expression is a constant, it does not need to be optimized,
    // there is no need to enter in the BB. 
    if (trip_count->Kind() != CK_CONST) {
      STMTREP *loop_info_stmt = CXX_NEW(STMTREP(OPC_EVAL),
                                        Htable()->Mem_pool());
      loop_info_stmt->Set_rhs(trip_count);
      loop->Preheader()->Append_stmtrep(loop_info_stmt);
      loop->Set_trip_count_stmt(loop_info_stmt);
    } else 
      loop->Set_trip_count_expr(trip_count);
    Inc_trip_counter();
  }


  // ************************************************************************
  //    Should we return the exit-value of the index variable?
  // ************************************************************************

  // if not perform loop_exit_opt 
  if (!perform_loop_exit_opt)  return NO_TRIP_COUNT;

  // if step is 0, then the loop is a infinite-loop
  if (step->Kind() != CK_CONST) return NO_TRIP_COUNT;

  if (trip_count->Kind() == CK_CONST)  {
    // the init and bound relation are funny, don't bother to guess
    // the trip count
    if (trip_count->Const_val() < 0) return NO_TRIP_COUNT;
  }
  return trip_count;
}


// Determine the trip count for the loop
void
IVR::Determine_trip_IV_and_exit_count(BB_LOOP *loopinfo,
				      IV_CAND **trip_iv_found,
				      IV_CAND *primary) 
{
  // Compute the trip count with the primary IV candidate.
  // return the trip count if it is a constant

  STMTREP *stmt = loopinfo->End()->Last_stmtrep();

  Is_True(stmt->Opr() == OPR_TRUEBR || stmt->Opr() == OPR_FALSEBR,
	  ("Don't know how to handle loop end block without"
	   " OPR_TRUEBR/FALSEBR"));

  // stop if the tree is not cannonicalized
  CODEREP *cond = stmt->Rhs();

    // Force simplification of trip-count expression because
    // icopy might not be able simplify all.
  if (cond->Kind() == CK_OP && OPCODE_is_compare(cond->Op())) {
    CODEREP *tmp_cond = Htable()->Add_bin_node_and_fold(cond->Op(),
							cond->Opnd(0),
							cond->Opnd(1),
							stmt->Bb());
    if (cond != tmp_cond) {
      cond = tmp_cond;
      stmt->Set_rhs(cond);
    }
  }

  if (cond->Kind() != CK_OP) return;
  if (cond->Kid_count() != 2) return;
  
  IV_CAND     *trip_cand;
  BOOL        found = FALSE;
  CODEREP     *trip_bound;
  CODEREP     *trip_iv_expr;
  OPCODE      cmp_op = cond->Op();
  BOOL        swapped = FALSE;

  // negate the comparison for FALSEBR;
  // the rest of code only deal with TRUEBR.
  if (stmt->Opr() == OPR_FALSEBR) {
    switch (OPCODE_operator(cmp_op)) {
    case OPR_LT:
      cmp_op=OPCODE_make_op(OPR_GE,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_LE:
      cmp_op=OPCODE_make_op(OPR_GT,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_GT:
      cmp_op=OPCODE_make_op(OPR_LE,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_GE:
      cmp_op=OPCODE_make_op(OPR_LT,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_NE:
      cmp_op=OPCODE_make_op(OPR_EQ,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_EQ:
      cmp_op=OPCODE_make_op(OPR_NE,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    default:
      return;
    }
  }


  // Convert_iload_to_loop_invariant  not completely implemented!

  //  Fix 280430: use stronger analysis to idenity loop bound

  if (WOPT_Enable_Aggr_Invariant) {
    if (!loopinfo->Invariant_cr(cond->Opnd(0)) &&
	!loopinfo->Invariant_cr(cond->Opnd(1))) {
      Htable()->Convert_iload_to_loop_invariant(loopinfo, cond);
    }
  }

  // Canonicalize the comparison of any form into trip_iv_expr cmpop
  // trip_bound: make sure the IV in the LHS and the invariant part is
  // in the RHS. 
  //
  if (loopinfo->Invariant_cr(cond->Opnd(0))) {
    swapped = TRUE;
    trip_bound = cond->Opnd(0);
    trip_iv_expr = cond->Opnd(1);
    // swap LHS and RHS
    switch (OPCODE_operator(cmp_op)) {
    case OPR_LT:
      cmp_op=OPCODE_make_op(OPR_GT,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_LE:
      cmp_op=OPCODE_make_op(OPR_GE,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_GT:
      cmp_op=OPCODE_make_op(OPR_LT,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    case OPR_GE:
      cmp_op=OPCODE_make_op(OPR_LE,OPCODE_rtype(cmp_op),OPCODE_desc(cmp_op));
      break;
    }
  } else {
    swapped = FALSE;
    trip_bound = cond->Opnd(1);
    trip_iv_expr = cond->Opnd(0);
  }


  // Determine the based-iv of the trip-iv-expr.
  IV_EXPR  ivx;
  ivx.Init(trip_iv_expr, loopinfo);

  // Lookup the iv_cand that is corresponding to the IV in the comparison.
  CODEREP *based_iv = NULL;
  INT64    new_step  = 0;
  if (ivx.Valid_expr()) {
    based_iv = ivx.Based_iv();
    new_step = ivx.Step();
    IDTYPE trip_aux_id = based_iv->Aux_id();
    vector<IV_CAND*>::iterator iv_cand_iter;
    for (iv_cand_iter = iv_cand_container.begin(); 
	 iv_cand_iter != iv_cand_container.end();
	 iv_cand_iter++) {
      trip_cand  = *iv_cand_iter;
      if (trip_aux_id == trip_cand->Var()->Aux_id() &&
	  (Opt_stab()->Aux_stab_entry(trip_aux_id)->Mclass() &
	   MTYPE_CLASS_UNSIGNED_INTEGER)) {
	found = TRUE;
	break;
      }
    }
  } 
  
  if (_trace) {
    if (found) 
      fprintf(TFile, "Trip IV is %d.\n", trip_cand->Var()->Aux_id());
    else
      fprintf(TFile, "Trip IV is not found.\n");
  }

  // cannot locate trip IV
  if (!found) return;
  *trip_iv_found = trip_cand;

  loopinfo->Set_iv(trip_cand->Var());

  // make sure the init is loop invariant
  CODEREP *trip_init =
    Htable()->Convert_to_loop_invar(trip_cand->Init_value(), loopinfo);

  if (!loopinfo->Invariant_cr(trip_cand->Step_value())) return;
  if (!loopinfo->Invariant_cr(trip_bound)) return;

  // Sometimes the step and bound are loop-invariants, but no
  // propagatable into the loop.  Convert_to_loop_invar will assign
  // the expr to preg.

  CODEREP *trip_step =
    Htable()->Convert_to_loop_invar(trip_cand->Step_value(), loopinfo);
  trip_bound = Htable()->Convert_to_loop_invar(trip_bound, loopinfo);

  if (trip_init == NULL || trip_step == NULL || trip_bound == NULL)
    return;

#ifdef KEY // bug 13728: if there is wraparound, do not continue
  if (trip_init->Kind() == CK_CONST && trip_step->Kind() == CK_CONST &&
      trip_bound->Kind() == CK_CONST) {
    if (MTYPE_signed(trip_cand->Var()->Dtyp()))
      if (trip_step->Const_val() > 0) {
	if (trip_init->Const_val() > trip_bound->Const_val())
	  return;
      }
      else {
	if (trip_init->Const_val() < trip_bound->Const_val())
	  return;
      }
    else
      if (trip_step->Const_val() > 0) {
	if ((UINT64)trip_init->Const_val() > (UINT64)trip_bound->Const_val())
	  return;
      }
      else {
	if ((UINT64)trip_init->Const_val() < (UINT64)trip_bound->Const_val())
	  return;
      }
  }
#endif

  if (loopinfo->Test_at_entry()) {
    // the following trip equations only applies to DO_LOOP in preopt
    // the opr represents the exit condition, so it the init-value satisfy
    // the exit conditions, then the trip count is 0.

    CODEREP *trip_init_adjusted = trip_init;
    if (trip_iv_expr != based_iv) 
      trip_init_adjusted = Replace_IV_with_invar(trip_iv_expr, based_iv,
						 trip_init);

    CODEREP *cmp = swapped ?
      Htable()->Add_bin_node_and_fold(cond->Op(), trip_bound,
				      trip_init_adjusted) :
      Htable()->Add_bin_node_and_fold(cond->Op(), trip_init_adjusted,
				      trip_bound);

    if (cmp->Kind() == CK_CONST) {
      // if the loop-exit-cond is TRUE, the loop is not executed.
      if ((stmt->Opr()==OPR_TRUEBR && cmp->Const_val()!=0)||
	  (stmt->Opr()==OPR_FALSEBR && cmp->Const_val()==0)) 
	Set_trip_count(Htable()->Add_const(MTYPE_I4, 0));
      else
	Set_trip_count(Compute_trip_count(cmp_op, loopinfo, trip_bound,
					  trip_cand->Var(), trip_init,
					  trip_step,
					  trip_iv_expr, based_iv, new_step,
					  TRUE, stmt, swapped, primary));
    } else {
      // if the initial loop-exit-cond is not determined,
      //   do not generate the trip count. 
      // Generate the guarded trip count in order to process nested loops.
      Set_trip_count(NULL);

      CODEREP *trip_count = Compute_trip_count(cmp_op, loopinfo, trip_bound,
					       trip_cand->Var(), trip_init,
					       trip_step, trip_iv_expr,
					       based_iv, new_step,
					       TRUE, stmt, swapped, primary);
      if (trip_count != NULL &&
	  loopinfo->Invariant_cr(cmp) &&
	  stmt->Opr() == OPR_FALSEBR) {  // Do-loop always use FALSEBR

	OPCODE select_op = OPCODE_make_op(OPR_SELECT,
					  trip_count->Dtyp(), MTYPE_V);
	CODEREP *zero = Htable()->Add_const(MTYPE_I4, 0);
	CODEREP *select = Htable()->Add_tertiary_node(select_op, cmp,
						      trip_count, zero);

	Set_entry_test(cmp);
	
	// simplify to (a > b) ? a : b
	//             (a >= b) ? a : b
	//             (a < b) ? b : a 
	//             (a <= b) ? b : a 
	//  into max(a,b) because LNO can optimize MAX but cannot optimize
	//  SELECT.
	switch (cmp->Opr()) {
	case OPR_GT:
	case OPR_GE:
	  if (cmp->Opnd(0) == select->Opnd(1) && 
	      cmp->Opnd(1) == select->Opnd(2)) {
	    select = Htable()->Add_bin_node_and_fold(
	      OPCODE_make_op(OPR_MAX, trip_count->Dtyp(), MTYPE_V),
	      select->Opnd(1), select->Opnd(2));
	  }
	  break;
	case OPR_LT:
	case OPR_LE:
	  if (cmp->Opnd(0) == select->Opnd(2) && 
	      cmp->Opnd(1) == select->Opnd(1)) {
	    select = Htable()->Add_bin_node_and_fold(
	      OPCODE_make_op(OPR_MAX, trip_count->Dtyp(), MTYPE_V),
	      select->Opnd(1), select->Opnd(2));
	  }
	  break;
	}
	Set_guarded_trip_count(select);

	if (_trace) {
	  fprintf(TFile, "IVR: no regular trip count generated.\n");
	  fprintf(TFile, "IVR: generated guarded trip count: \n");
	  Guarded_trip_count()->Print(0, TFile);
	  fprintf(TFile, "IVR: entry test of the loop is: \n");
	  Entry_test()->Print(0, TFile);
	}
      } else
	Set_guarded_trip_count(NULL);
    }

  } else if (loopinfo->Test_at_exit()) {

    Is_True(stmt->Opr() == OPR_TRUEBR,
            ("slt opt only works with TRUEBR."));

    Set_trip_count(Compute_trip_count(cmp_op, loopinfo, trip_bound,
				      trip_cand->Var(), trip_init,
				      trip_step,
				      trip_iv_expr, based_iv, new_step,
				      FALSE, stmt, swapped, primary));

#ifdef Is_True_On
    // make sure we generate loop info for every DO loop
    if (Phase() == MAINOPT_PHASE &&
        (loopinfo->Flags() & LOOP_DO) == LOOP_DO) {
      if (loopinfo->Trip_count_stmt() == NULL &&
          loopinfo->Trip_count_expr() == NULL)
	DevWarn("Trip count is not generated for a DO-loop.");
    }
#endif
  }   // other kinds of loops (not handled)
}


// Replace an secondary IV by a function of the primary IV
//   -- startbb is where the phi function locates
void
IVR::Replace_secondary_IV(const IV_CAND *primary,
                               const IV_CAND *secondary,
                               BB_NODE *startbb,
                               BB_LOOP *loop)
{
  // Construct the Assignment statement with the equation:
  // alt = init_alt + (step_alt/step_prim) * (prim - init_prim)

  Is_True(primary->Step_value()->Kind() == CK_CONST &&
          primary->Step_value()->Const_val() == 1,
	  ("primary IV is not stride 1."));


  // use primary->Dtype(), so that this expr can be CSEed later.
  OPCODE subop = OPCODE_make_op(OPR_SUB, primary->Dtype(), MTYPE_V);
  CODEREP *iter_count =
    Htable()->Add_bin_node_and_fold(subop, primary->Var(),
				    primary->Init_value());

  // switch to use secondary->Dtype().
  MTYPE dtype = secondary->Dtype();
  iter_count = iter_count->Fixup_type(dtype, Htable());

  // Because step_prim is always 1.
  // so the equation is
  // alt = init_alt + step_alt * (prim - init_prim)

  // A problem arise if step_alt is unsigned and is -ve.
  // change the equation to  alt = init_alt - (-step_alt) * (prim - init_prim)

  CODEREP *second_iv;
  if (secondary->Step_value()->Kind() == CK_CONST && 
      secondary->Step_value()->Const_val() < 0) {
	OPCODE mulop = OPCODE_make_op(OPR_MPY, dtype, MTYPE_V);
	CODEREP *new_step =
          Htable()->Add_const(dtype, - secondary->Step_value()->Const_val());
	CODEREP *iter_inc =
          Htable()->Add_bin_node_and_fold(mulop, iter_count, new_step);
	OPCODE subop = OPCODE_make_op(OPR_SUB, dtype, MTYPE_V);
	second_iv =
          Htable()->Add_bin_node_and_fold(subop, secondary->Init_value(),
					  iter_inc);

      } else {
	OPCODE mulop = OPCODE_make_op(OPR_MPY, dtype, MTYPE_V);
	CODEREP *iter_inc =
          Htable()->Add_bin_node_and_fold(mulop, iter_count,
                                          secondary->Step_value()); 
	OPCODE addop = OPCODE_make_op(OPR_ADD, dtype, MTYPE_V);
	second_iv =
          Htable()->Add_bin_node_and_fold(addop, iter_inc,
					  secondary->Init_value());
      }

  STMTREP *newstmt =
    second_iv->Create_cpstmt(secondary->Var(), Htable()->Mem_pool());
  newstmt->Set_ivr_introduced();
  secondary->Var()->Reset_flag(CF_DEF_BY_PHI);
  secondary->Var()->Reset_flag(CF_DONT_PROP);
  
  secondary->Var()->Set_defstmt(newstmt);

  newstmt->Set_bb(startbb);
  startbb->Prepend_stmtrep(newstmt);

  // secondary->Incr()->Reset_flag(CF_DONT_PROP);
  //  this must be done after the BB of the newstmt be set.
// Bug #564
#ifdef KEY
  if (!loop->Exit_early())
    Reset_dont_prop(secondary->Incr_var(), loop);
#else
  Reset_dont_prop(secondary->Incr_var(), loop);
#endif

  // Replace the phi result by a zero version
  // Generate a new version.
  IDTYPE aux_id = secondary->Var()->Aux_id();
  CODEREP *dummy_cr = Htable()->Add_def(aux_id, 0, NULL,
					MTYPE_UNKNOWN, MTYPE_UNKNOWN,
					Opt_stab()->St_ofst(aux_id),
					0, 0, TRUE);
  dummy_cr->Set_flag(CF_MADEUP_TYPE);
  dummy_cr->Set_flag(CF_DEF_BY_PHI);
  dummy_cr->Set_defphi(secondary->Phi());
  secondary->Phi()->Set_result(dummy_cr);
  secondary->Phi()->Set_dse_dead();
  secondary->Phi()->Reset_live();
  Inc_ivr_counter();
}


// ====================================================================
// Update_exit_stmt
// Is_IV_cand_in_parent_loop
// CR_is_equivalent
// ====================================================================


//  Determine if the IV in the inner loop is also an IV of the parent loop.
//  If yes, IVR needs to update the exit statement with the guarded trip count.
//
//  This implementation does not allow the IV to have any increment/decrement in
//  the outer loop.  It is possible to relax condition (3) and (4) to allow
//  updates.  Wait until a test case shows up.
//
static BOOL
Is_IV_cand_in_parent_loop(BB_LOOP *loop, CODEREP *inner_loop_init, CODEREP *exit_stmt_lhs)
{
  // 1. loop has a parent 
  // 2. find the phi with same aux-id as cr in the parent loop
  // 3. the IV init value of the inner loop equals the phi 
  //    result.
  // 4. the lhs of the exit stmt in the inner loop equals to one of the phi opnds.
  //
  BB_LOOP *parent_loop = loop->Parent();
  if (parent_loop == NULL) return FALSE;
  
  // workaround stupid BB_LOOP bugs
  if (parent_loop->Preheader() == NULL) {
    DevWarn("IVR found a BB_LOOP with undefined Dohead!");
    return FALSE;
  }
  
  BB_NODE *bb = parent_loop->Header();
  if (bb->Pred()->Len() != 2) return FALSE;
  
  //  Iterate through each phi-node
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  PHI_NODE     *found_phi = NULL;
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    if (! phi->Live()) continue;
    CODEREP *res = phi->RESULT();
    // must be scalar variable
    if (res->Kind() != CK_VAR) continue;
    if (res->Aux_id() == exit_stmt_lhs->Aux_id()) {
      found_phi = phi; 
      break;
    }
  }
  if (found_phi == NULL) return FALSE;

  if (found_phi->RESULT() != inner_loop_init) return FALSE;

  if (exit_stmt_lhs != found_phi->OPND(0) &&
      exit_stmt_lhs != found_phi->OPND(1)) return FALSE;

  return TRUE;
}


// Two CR are equivalent (has same value) if cr reaches def_cr
// through a use-def chain composed of identity assignments.
//
static BOOL
CR_is_equivalent(CODEREP *cr, CODEREP *def_cr)
{
  STMTREP *dstmt;
  while (TRUE) {
    if (cr->Is_flag_set(CF_IS_ZERO_VERSION))
      return FALSE;
    if (cr == def_cr) 
      return TRUE;
    if (cr->Is_flag_set(CF_DEF_BY_PHI)) 
      return FALSE;
    if (cr->Is_flag_set(CF_DEF_BY_CHI)) {
      dstmt = cr->Defstmt();
      if (dstmt != NULL && 
	  dstmt->Is_identity_assignment_removable()) {
	cr = cr->Defchi()->OPND();
      } else
	return FALSE;
    } else {
      if ((dstmt = cr->Defstmt()) != NULL && 
	  dstmt->Is_identity_assignment_removable()) {
	cr = dstmt->Rhs();
      } else
	return FALSE;
    }
  }
}


// Update the value of the secondary IV at the merge block
//
void
IVR::Update_exit_stmt(const IV_CAND *secondary,
		      BB_NODE *bb_merge,
		      BB_LOOP *loop)
{
  if (Trip_count() == NULL && 
      Guarded_trip_count() == NULL) 
    return;
  
  // Search for the redundant assignment introduced in an earlier phase.
  STMTREP *exit_stmt = NULL;
  STMTREP_ITER stmt_iter(bb_merge->Stmtlist());
  STMTREP *stmt;

  CODEREP *match_val = NULL;
  if (loop->Test_at_entry()) 
    match_val = secondary->Var();
  else if (loop->Test_at_exit()) 
    match_val = secondary->Incr_var();

  if (match_val) {
    FOR_ALL_NODE(stmt, stmt_iter, Init()) {
      if (stmt->Is_identity_assignment_removable()) {
	if (stmt->Rhs() == match_val ||
	    CR_is_equivalent(stmt->Rhs(), match_val)) {
	  exit_stmt = stmt;
	  break;
	}
      }
    }
  }
  if (exit_stmt == NULL) return;

  CODEREP *trip_count;
  if (Trip_count() != NULL)
    trip_count = Trip_count();
  else if (Guarded_trip_count() != NULL &&
	   Is_IV_cand_in_parent_loop(loop, secondary->Init_var(), stmt->Lhs())) 
    trip_count = Guarded_trip_count();
  else if (Trip_count_primary_IV() != NULL) 
    trip_count = Trip_count_primary_IV();
  else
    return;

  //  Compute the equation  exit_val = init_val + trip_count * step 
  //
  //  If step is -ve,
  //    change the equation to exit_val = init_val - trip_count * (-step)
  // 
  MTYPE dtype = secondary->Dtype();
  trip_count = trip_count->Fixup_type(dtype, Htable());
    
  OPCODE mulop = OPCODE_make_op(OPR_MPY, dtype, MTYPE_V);
  CODEREP *exit_value;

  if (secondary->Step_value()->Kind() == CK_CONST &&
      secondary->Step_value()->Const_val() < 0) {
    OPCODE subop = OPCODE_make_op(OPR_SUB, dtype, MTYPE_V);
    CODEREP *new_step =
      Htable()->Add_const(dtype, - secondary->Step_value()->Const_val());
    CODEREP *iter_inc =
      Htable()->Add_bin_node_and_fold(mulop, trip_count, new_step);
    exit_value =
      Htable()->Add_bin_node_and_fold(subop, secondary->Init_value(), iter_inc);
  } else {
    OPCODE addop = OPCODE_make_op(OPR_ADD, dtype, MTYPE_V);
    CODEREP *iter_inc =
      Htable()->Add_bin_node_and_fold(mulop, trip_count,
				      secondary->Step_value());
    exit_value =
      Htable()->Add_bin_node_and_fold(addop, secondary->Init_value(),
				      iter_inc);
  }

  exit_stmt->Set_rhs(exit_value);
  Inc_exit_value_counter();
}

// This function is to canonicalize loop-exit brach. The necessity can 
// be explained by the following example:
//
//   for (int i = n ; i < m; i++) { ... }
// 
// IVR introduces a var, say <prim_iv>, serveing as primary IV, making <i> 
// secondary IV. Replace_secondary_IV() will replace all secondary IVs with 
// <prim_iv>'s linear expr. In this case, the result would be:
//
//   for (int prim_iv = 0 ; (prim_iv + n) < m; prim_iv++) { ... }
//
//  The loop-exit branch is awkward. It is desirable to be in the form of:
//      "prim_iv < m-n"
//  This function is to serve for that purpose.
//
void
IVR::Canon_loop_end_br (BB_LOOP* loop, IV_CAND* prim_iv_cand)
{
  if (!prim_iv_cand->Init_value () ||
      prim_iv_cand->Init_value()->Kind () != CK_CONST ||
      prim_iv_cand->Init_value()->Const_val () != 0) {
    // TODO: handle these cases. These cases are possible if IVR is invoked 
    //   in WOPT phase.
    return;
  }

  STMTREP *stmt = loop->End()->Last_stmtrep();
  if (stmt->Opr() != OPR_TRUEBR && stmt->Opr() != OPR_FALSEBR)
    return; 

  IV_CAND* second_iv_cand = NULL; 
  CODEREP* up_bound = NULL;

  CODEREP* cmp = stmt->Rhs ();
  if (cmp->Kind() != CK_OP || !OPERATOR_is_compare (cmp->Opr ())) {
    return;
  }
  CODEREP* kid_0 = cmp->Opnd (0);
  CODEREP* kid_1 = cmp->Opnd (1);

  // step 1: Figure out which secondary IV is involed in the branch,
  //    in the mean time, figure out which operand is up-bound.
  //
  for (vector<IV_CAND*>::iterator iter = iv_cand_container.begin(); 
       iter != iv_cand_container.end(); iter++) {
    
    IV_CAND *iv = *iter;
    if (iv->Var ()) {
      IDTYPE aux_id = iv->Var()->Aux_id();   
      if (kid_0->Kind () == CK_VAR && aux_id == kid_0->Aux_id ()) {
        up_bound = kid_1;
        second_iv_cand = iv; 
        break;
      } else if (kid_1->Kind () == CK_VAR && aux_id == kid_1->Aux_id ()) {
        up_bound = kid_0;
        second_iv_cand = iv; 
        break;
      }
    }
  }

  if (!second_iv_cand)
    return;

  CODEREP* second_iv = second_iv_cand->Var ();
  CODEREP* prim_iv = prim_iv_cand->Var ();
  if (second_iv->Aux_id () == prim_iv->Aux_id ()) {
    // don't bother. It is already canonilocalized
    //
    return;
  }

  if (second_iv_cand->Step_value () == NULL ||
      second_iv_cand->Step_value ()->Kind () != CK_CONST ||
      second_iv_cand->Step_value ()->Const_val () != 1) {
    // TODO: handle non-unit stride
    return;
  }
  
  MTYPE second_iv_ty = second_iv->Dtyp ();
  MTYPE prim_iv_ty = prim_iv->Dtyp ();

  if (MTYPE_is_signed (second_iv_ty) != MTYPE_is_signed (prim_iv_ty) ||
      MTYPE_byte_size (second_iv_ty) != MTYPE_byte_size (prim_iv_ty)) {
    // TODO: handle these cases.
    return;
  }

  // step 2: generate normalized comparision: "<prim_iv> <cmp_op> expr"
  //   or "<expr> <cmp_op> <prim_iv> depending on the order of orignial 
  //   comparision.
  //
  OPCODE subop = OPCODE_make_op (OPR_SUB, prim_iv->Dtyp (), MTYPE_V);
  CODEREP* norm_iv = Htable()->Add_bin_node_and_fold (subop, 
                                up_bound, second_iv_cand->Init_value());
                                     
  OPCODE cmp_op = OPCODE_make_op (cmp->Opr (), prim_iv->Dtyp(), prim_iv->Dtyp());

  CODEREP* new_cmp;
  if (up_bound == kid_1) {
    new_cmp = Htable()->Add_bin_node_and_fold (cmp_op, prim_iv, norm_iv);
  } else {
    new_cmp = Htable()->Add_bin_node_and_fold (cmp_op, norm_iv, prim_iv);
  }

  // step 3: update the branch statement
  //
  stmt->Set_rhs (new_cmp);
}

// ====================================================================
//
// Convert_all_ivs is the driver for IVR on a particular loop.
//
// ====================================================================


void
IVR::Convert_all_ivs(BB_LOOP *loop)
{
  if (!loop->Well_formed()) return;
  if (loop->End() == NULL) return;  // skip non SCF loop for now.

  Init_loop(loop);   // initialize the per-loop fields.

  // Determine all the IV cands
  if ((loop->Flags() & LOOP_PRE_REPEAT) == LOOP_PRE_REPEAT)
    return;

  Ident_all_iv_cands(loop, loop->Header());

  vector<IV_CAND*>::iterator iv_cand_iter;
  for (iv_cand_iter = iv_cand_container.begin(); 
       iv_cand_iter != iv_cand_container.end();
       iv_cand_iter++) {
    IV_CAND *cur_iv = *iv_cand_iter;
    CODEREP *new_init = Htable()->Convert_to_loop_invar(cur_iv->Init_value(),
							loop);
    cur_iv->Set_init_value(new_init);
  }

  // Determine primary IV
  IV_CAND *primary = Choose_primary_IV(loop);

  // Determine trip count
  IV_CAND *trip_iv = NULL;
  Determine_trip_IV_and_exit_count(loop, &trip_iv, primary);
  CODEREP *trip_count = Trip_count();

  // ************************************************************************
  //    Update the BB_LOOP entry test condition
  // ************************************************************************
  loop->Set_entry_test( Entry_test() );

  if (_trace) {
    if (trip_count != NULL)  {
      if (trip_count->Kind() == CK_CONST)
	fprintf(TFile, "IVR: exit trip count is %lld\n",
                trip_count->Const_val());
      else {
	fprintf(TFile, "IVR: exit trip count is ");
	trip_count->Print(0,TFile);
      }
    } else
      fprintf(TFile, "IVR exit value is not determined.\n");
  }

  if (primary && trip_iv && ivr_generated_primary) {
    AUX_ID primary_id = primary->Var()->Aux_id();
    ST *trip_st = Opt_stab()->St(trip_iv->Var()->Aux_id());
#ifdef KEY
    if (ST_class(Opt_stab()->St(primary_id)) == CLASS_PREG)
#endif
    Set_Preg_Name((PREG_NUM) Opt_stab()->St_ofst(primary_id),
		  ST_name(trip_st));
    if (_trace)
      fprintf(TFile, "IVR: change preg %lld index name to %s.\n", 
	      Opt_stab()->St_ofst(primary_id),
	      ST_name(trip_st));
  }

  // Fix 454209:  if the loop executes exactly one and it is
  // MAINOPT_PHASE and the test is at the end, then remove the
  // loop-back edge to avoid moving invariant out of the loop or
  // perform strength reduction unnecessarily.
  if (Phase() == MAINOPT_PHASE &&
      trip_count != NULL &&
      trip_count->Kind() == CK_CONST &&
      trip_count->Const_val() == 1 &&
      loop->Test_at_exit()) {
    BB_NODE *test = loop->End();
    if (_trace) 
      fprintf(TFile,
	      "IVR: loop executes one -- removing loop back edge at BB%d\n", 
	      test->Id());
    Is_True(test->Next() == loop->Merge(), 
	    ("IVR:  loop->Merge() is not loop->Test()->Next()"));
    Cfg()->Remove_path(test, loop->Body());
    if ( Cfg()->Feedback() )
      Cfg()->Feedback()->Delete_edge( test->Id(), loop->Body()->Id() );

    loop->End()->Remove_stmtrep(test->Branch_stmtrep());
    Cfg()->Change_block_kind(test, BB_GOTO);
    Set_rebuild_loops();  // recompute the loop structure.
    return;
  }

  if (primary) {
    if (trip_count == NULL) {
      if (Phase() == PREOPT_PHASE || 
          Phase() == PREOPT_LNO_PHASE ||
          Phase() == PREOPT_IPA0_PHASE ||
          Phase() == PREOPT_IPA1_PHASE) {
        // generate trip count based on primary IV
        OPCODE subop = OPCODE_make_op(OPR_SUB, primary->Dtype(), MTYPE_V);
        Set_trip_count_primary_IV(Htable()->Add_bin_node_and_fold(subop,
								  primary->Var(),
								  primary->Init_value()));
      }
    } else {
      // Patch 453841:  disable update exit stmt for early exit loops
      //   for performance reasons.
      if (!loop->Exit_early()) {
	// update exit stmt for the primary
	Update_exit_stmt(primary, loop->Merge(), loop);
      }
    }
  }

  if (primary == NULL) return;

  // pv 382268, replace secondary IVs only for LNO
  if (!WOPT_Enable_Replace_Second_IV)
    return;

  // Fix 553115
  if (!WOPT_Enable_Replace_While_Loop_Second_IV &&
      loop->Exit_early())
    return;

  loop->Set_iv(primary->Var());
  if (_trace) { fprintf(TFile, "PRIMARY "); primary->Print(TFile); }

  Canon_loop_end_br (loop, primary);

  for (iv_cand_iter = iv_cand_container.begin(); 
       iv_cand_iter != iv_cand_container.end();
       iv_cand_iter++) {

    IV_CAND *secondary = *iv_cand_iter;
    // skip the primary
    
    if (secondary == primary) continue;

    AUX_ID aux_id = secondary->Var()->Aux_id();

    // skip secondary that are smaller than primary for performance reasons.
    //
    if (MTYPE_size_min(secondary->Dtype()) <
        MTYPE_size_min(primary->Dtype())) {
      if (_trace) 
	fprintf(TFile, "IVR: skip secondary IV sym%d because secondary"
		" size < primary size.\n", secondary->Var()->Aux_id());
      continue;
    }

#ifdef TARG_X8664  // skip secondary IVs that are MTYPE_U4 for performance
    		   // reason 
    if (secondary->Dtype() == MTYPE_U4) {
      if (_trace) 
	fprintf(TFile, "IVR: skip secondary IV sym%d because of type MTYPE_U4\n"
		, secondary->Var()->Aux_id());
      continue;
    }
#endif
    
    // Fix 469578:
    // Disable secondary IV replacement if
    //   sizeof(secondary IV) > sizeof(primary IV)
    // and sizeof(trip IV) > sizeof(primary IV).
    //
    // The objective is not to replace every 'long' or 'ptr' with 'int'
    // by IVR unless the range of 'int' is sufficient
    // (i.e., the trip count is less than 2^32).
    //
    // The required condition is sizeof(trip IV) <= sizeof(primary IV)
    // because if the loop exit condition is based on a I4/U4 IV, the
    // loop should not execute more than 2^32 times.
    //
    if ((MTYPE_size_min(secondary->Dtype())
	 > MTYPE_size_min(primary->Dtype())) &&
	(trip_iv == NULL ||
	 (MTYPE_size_min(trip_iv->Dtype())
	  > MTYPE_size_min(primary->Dtype())))) {
      if (_trace) 
	fprintf(TFile, "IVR: skip secondary IV sym%d because secondary IV"
		" size > primary IV size.\n"
		"and trip IV size > primary IV size\n",
		secondary->Var()->Aux_id());
      continue;
    }

    // Fix 428966:  fix the regression introducing when fixing 418175.
    // The problem was caused when an exit i=i was not inserted but the
    // second IV updated was moved from the loophead to loopbody.
    // This fix involves adding a new flag AUXF_DONT_REPLACE_IV to the
    // opt_stab.
    if (Opt_stab()->Aux_stab_entry(aux_id)->Dont_replace_iv()) {
      if (_trace)
	fprintf(TFile,
		"IVR: skip secondary IV sym%d because of Dont_replace_iv.\n", 
		secondary->Var()->Aux_id());
      continue;
    }

    if (secondary->Init_value() != NULL) {
      // Update_exit_stmt must run before Replace_secondary_IV because
      // it needs to access the phi node at the loop start and
      // Replace_secondary_IV is going to remove the phi node.
      Update_exit_stmt(secondary, loop->Merge(), loop);
      Replace_secondary_IV(primary, secondary, loop->Header(), loop);
    } else {
      if (_trace) 
	fprintf(TFile, "IVR skip secondary IV sym%d because it cannot"
		" find init value.\n", secondary->Var()->Aux_id());
      Warn_todo("IVR: Generate copy for non-loop-invariant init value.");
    }
  }
}


// ====================================================================
//
// Get_my_regionstart
// Find_parallel_pragma_stmt
// Find_associated_parallel_pragma
// Preprocess_mp_pragma_list
// Update_mp_pragma_list
// Is_mp_with_same_mp_pragma
//
// ====================================================================


inline BB_NODE*
IVR::Get_my_regionstart(BB_NODE *bb) const
{
  while (bb->Kind() != BB_REGIONSTART) bb = bb->Prev();
  return bb;
}


STMTREP*
IVR::Find_parallel_pragma_stmt(BB_NODE *bb)
{
  STMTREP     *stmt;
  STMTREP_ITER stmt_iter(bb->Stmtlist());
  FOR_ALL_NODE(stmt, stmt_iter, Init()) {
    WN *pragma = stmt->Orig_wn();
    if (stmt->Op() == OPC_PRAGMA &&
        WN_Pragma_is_Parallel((WN_PRAGMA_ID)WN_pragma(pragma))) {
          if (WN_pragma_arg1(pragma) != 0)
            return NULL;
          else
            return stmt;
        }
  }
  return NULL;
}


STMTREP*
IVR::Find_associated_parallel_pragma(BB_LOOP *loop, BB_NODE **contain_bb)
{
  STMTREP *retval = NULL;
  BB_NODE *bb;
  while ( retval == NULL && loop && IS_MP_LOOP(loop) ) {
    bb = Get_my_regionstart(loop->Start());
    Is_True(bb != NULL,
            ("IVR::Find_parallel_retval: cannot find region start"));

    retval = Find_parallel_pragma_stmt(bb);
    loop = loop->Parent();
  }
  if (retval == NULL)
    *contain_bb = NULL;
  else
    *contain_bb = bb;
  return retval;
}


STMTREP*
IVR::Preprocess_mp_pragma_list(BB_LOOP *loop, BB_NODE **bb)
{
  WN *index = loop->Index();

  STMTREP *stmt = Find_associated_parallel_pragma(loop, bb);
  if (stmt) {
    WN *pragma = stmt->Orig_wn();
    Opt_stab()->Init_mp_attribute();
    do {
      ST *st = WN_st(pragma);

      if (st) {
        AUX_ID vsym_id;
        if (ST_sclass(st) == SCLASS_FORMAL && !ST_is_value_parm(st))
          vsym_id = Opt_stab()->Find_vsym_with_base(st);
        else
          vsym_id =
            Opt_stab()->Find_sym_with_st_and_ofst(st,
                                                  WN_pragma_arg1(pragma));
        switch ( WN_pragma(pragma) ) {
        case WN_PRAGMA_LOCAL:
          Opt_stab()->Reset_mp_shared(vsym_id);
          break;
        case WN_PRAGMA_SHARED:
          // already initialized with shared
          break;
        case WN_PRAGMA_LASTLOCAL:
          Opt_stab()->Reset_mp_shared(vsym_id); // pv421156
          Opt_stab()->Set_mp_lastlocal(vsym_id);
          break;
        case WN_PRAGMA_REDUCTION:
          Opt_stab()->Reset_mp_shared(vsym_id);
          Opt_stab()->Set_mp_reduction(vsym_id);
        case WN_PRAGMA_FIRSTPRIVATE:
          Opt_stab()->Reset_mp_shared(vsym_id);
          Opt_stab()->Set_mp_firstprivate(vsym_id);
          break;
        }
      }
      stmt = stmt->Next();
      if (stmt) pragma = stmt->Orig_wn();
    } while (stmt && (stmt->Op() == OPC_PRAGMA||stmt->Op() == OPC_XPRAGMA));// pv419869
  }
  return stmt;
}


void
IVR::Update_mp_pragma_list(BB_LOOP *loop, BB_NODE *bb, STMTREP *stmt)
{
  WN *index = loop->Index();

  if (Phase() != MAINOPT_PHASE && index != NULL && _iv_cands != NULL) {
    IV_CAND      *primary_iv = NULL;
    BOOL          second_iv_in_shared = FALSE;
    BOOL          second_iv_in_lastlocal = FALSE;
    BOOL          second_iv_in_reduction = FALSE;
    BOOL          second_iv_in_firstprivate = FALSE;

    vector<IV_CAND*>::iterator iv_cand_iter;
    for (iv_cand_iter = iv_cand_container.begin(); 
	 iv_cand_iter != iv_cand_container.end();
	 iv_cand_iter++) {
      IV_CAND *cur_iv = *iv_cand_iter;
      if (cur_iv->Is_primary()) {
        primary_iv = cur_iv;
      }
      else {
        AUX_STAB_ENTRY *sym =
          Opt_stab()->Aux_stab_entry(cur_iv->Var()->Aux_id());
        if (sym->Mp_shared())
          second_iv_in_shared = TRUE;
        if (sym->Mp_lastlocal())
          second_iv_in_lastlocal = TRUE;
	if (sym->Mp_reduction())
	  second_iv_in_reduction = TRUE;
	if (sym->Mp_firstprivate())
	  second_iv_in_firstprivate = TRUE;
      }
    }
    // cannot find primary iv
    if (primary_iv == NULL) {
      Warn_todo("IVR::Update_mp_pragma_list: cannot find primary_iv");
      return;
    }

    AUX_ID prim_auxid = primary_iv->Var()->Aux_id();
    ST    *prim_st = Opt_stab()->St(prim_auxid);
    if (ST_sclass(prim_st) == SCLASS_REG) {
      INT32    prim_ofst = Opt_stab()->St_ofst(prim_auxid);
      WN      *pwn;
      if (second_iv_in_shared) {
        // add the primary iv in the pragma list
        pwn = WN_CreatePragma(WN_PRAGMA_SHARED,
                              prim_st,
                              prim_ofst,
                              0/*arg2*/);
        bb->Add_pragma(pwn, stmt, Htable()->Mem_pool());
      }
      if (second_iv_in_lastlocal) {
        // add the primary iv in the pragma list
        pwn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL,
                              prim_st,
                              prim_ofst,
                              0/*arg2*/);
        bb->Add_pragma(pwn, stmt, Htable()->Mem_pool());
      }
      if (second_iv_in_reduction) {
        // add the primary iv in the pragma list
        pwn = WN_CreatePragma(WN_PRAGMA_REDUCTION,
                              prim_st,
                              prim_ofst,
                              0/*arg2*/);
        bb->Add_pragma(pwn, stmt, Htable()->Mem_pool());
      }
      if (second_iv_in_firstprivate) {
        // add the primary iv in the pragma list
        pwn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE,
                              prim_st,
                              prim_ofst,
                              0/*arg2*/);
        bb->Add_pragma(pwn, stmt, Htable()->Mem_pool());
      }
    }
  }  
}


BOOL
IVR::Is_mp_with_same_mp_pragma(BB_LOOP *child, BB_LOOP *parent)
{
  BB_NODE *child_end_bb = child->End();
  BB_NODE *parent_end_bb = parent->End();

  INT child_rid_id = child_end_bb->Rid_id();
  INT parent_rid_id = parent_end_bb->Rid_id();

  if (child_rid_id == parent_rid_id) 
    return TRUE;

  BB_NODE *child_rgstart;
  BB_NODE *parent_rgstart;

  Find_associated_parallel_pragma(child, &child_rgstart);
  Find_associated_parallel_pragma(parent, &parent_rgstart);

  return (child_rgstart != NULL && child_rgstart == parent_rgstart);
}


// ====================================================================
//
// IVR::Process_one_loop invokes Convert_all_ivs on each loop in the
// program unit.  Nested loops are processed inside out, because extra
// induction variables can be recognized for the outer loop if the trip
// count of the inner loop can be determined first.  For MP loops,
// Process_one_loop also invokes Preprocess_mp_pragma_list and
// Update_mp_pragma_list.  IVR::Process_one_loop returns TRUE iff the
// current loop or one of its children ....
//
// ====================================================================


BOOL
IVR::Process_one_loop(BB_LOOP *loop)
{
  BOOL has_mp_do = FALSE;
  if (loop->Child()) {
    BB_LOOP_ITER loop_iter(loop->Child());
    BB_LOOP *child;
    FOR_ALL_NODE(child, loop_iter, Init()) {
      if (child->End() &&  // SCF info is available
	  IS_MP_LOOP(child) &&
	  // child is the MP region with it's own MP pragma
	  !Is_mp_with_same_mp_pragma(child, loop))
	has_mp_do = TRUE;
      if (Process_one_loop(child))
	has_mp_do = TRUE;
    }
  }

  // do not handle pdo loop
  // STREICH:  we could set LOOP_IS_PDO, but I don't because
  //           nobody ever set any of the mp loop flags, and
  //           I'm not sure what other effect it has.  So,
  //           I check for specific case
  if ( !WOPT_Enable_IVR_Outermost_Loop_Parallel_Region &&
       loop->End() &&  // SCF info is available
       Cfg()->Is_outermost_loop_in_parallel_region(loop,WN_PRAGMA_PDO_BEGIN) )
    {
      Is_Trace(Trace(),
	       (TFile, "IVR::Process_one_loop: skip loop at BB:%d",
		loop->Header()->Id()) );
      return has_mp_do;
    }

  // abort outer loop if inner loop is doacross, i.e. mp region
  if (! has_mp_do) {
    BB_NODE *bb;
    STMTREP *stmt;
    Inc_loop_counter();
    if (IS_MP_LOOP(loop))
      stmt = Preprocess_mp_pragma_list(loop, &bb);


    Convert_all_ivs(loop);

    if (IS_MP_LOOP(loop))
      Update_mp_pragma_list(loop, bb, stmt);
  }
  return has_mp_do;
}


// ====================================================================
//
// IVR class constructor and destructor allocate and deallocate the
// local IVR memory pool.  Also, the destructor logs IVR statistics.
//
// ====================================================================


// IVR constructor
IVR::IVR(COMP_UNIT *cu, BOOL trace)
{ 
  _iv_cands = NULL;
  _cu = cu;
  _htable = cu->Htable();
  _cfg    = cu->Cfg();
  _opt_stab = cu->Opt_stab();
  _rebuild_loops = FALSE;
  _trace = trace;
  Init_counters();
  OPT_POOL_Initialize(&_mem_pool, "IV RECOG pool", FALSE, IVR_DUMP_FLAG);
  OPT_POOL_Push(&_mem_pool, IVR_DUMP_FLAG); 
}


// IVR destructor
IVR::~IVR(void)
{ 
  Opt_tlog( "IVR", 0, "loops processed %d", Loop_counter() );
  Opt_tlog( "IVR", 0, "secondary IV replaced %d", Ivr_counter() );
  Opt_tlog( "IVR", 0, "slt opt %d", Slt_counter() );
  Opt_tlog( "IVR", 0, "exit-value %d; loopinfo tripcount generated %d",
	    Exit_value_counter(), Trip_counter() );

  OPT_POOL_Pop(&_mem_pool, IVR_DUMP_FLAG);  
  OPT_POOL_Delete(&_mem_pool, IVR_DUMP_FLAG);
}


// ====================================================================
//
// COMP_UNIT::Do_iv_recognition applies Induction Variable Recognition
// (IVR) to the entire COMP_UNIT.  It is the interface between IVR and
// the rest of the optimizer.
//
// ====================================================================


void
COMP_UNIT::Do_iv_recognition(void)
{
  BOOL trace_ivr = Get_Trace(TP_GLOBOPT, IVR_DUMP_FLAG );
  if (trace_ivr) {
    fprintf(TFile, "%sDump before IV recognition\n%s", DBar, DBar);
    Cfg()->Print(TFile);
    fprintf(TFile, "%sTrace in IV recognition\n%s", DBar, DBar );
  }

  // Identify loops
  BB_LOOP *loop_list = _cfg->Analyze_loops();
#ifdef KEY
#ifdef Is_True_On
  INT32 ivr_idx = 0;
#endif
#endif
  if (loop_list != NULL) {
    IVR iv_recog(this, trace_ivr);
    BB_LOOP_ITER loop_iter(loop_list);
    BB_LOOP *loop;

    FOR_ALL_NODE(loop, loop_iter, Init()){
#ifdef KEY
#ifdef Is_True_On
      if (WOPT_Enable_Ivr_Limit != -1 && ivr_idx >= WOPT_Enable_Ivr_Limit)
        break;
      ivr_idx++;
#endif
#endif
      iv_recog.Process_one_loop(loop);
    }

    if (iv_recog.Rebuild_loops()) {
      // rebuild the loop structure because some loops are disassembled.
      _cfg->Invalidate_loops();
      _cfg->Analyze_loops();
    }
  }

  if (trace_ivr) {
    fprintf(TFile, "%sDump after IV recognition\n%s", DBar, DBar );
    Cfg()->Print(TFile);
  }
}


// ====================================================================
//
// IV_CAND::Print displays one IV candidate
//
// IVR::Print_all_iv_cand displays all IV candidates
//
// ====================================================================


void
IV_CAND::Print(FILE *fp) const
{
  fprintf(fp, "IV_CAND: ");
  _phi->Print(_phi->Size(), fp);
  fprintf(fp, "          Init");
  _init_var->Print(1, fp);
  fprintf(fp, "          Step");
  _step_value->Print(15, fp);
}


void
IVR::Print_all_iv_cand(FILE *fp)
{
  vector<IV_CAND*>::iterator iv_cand_iter;
  for (iv_cand_iter = iv_cand_container.begin(); 
       iv_cand_iter != iv_cand_container.end();
       iv_cand_iter++) {
    IV_CAND *cur_iv = *iv_cand_iter;
    cur_iv->Print(fp);
  }
}


// ====================================================================
