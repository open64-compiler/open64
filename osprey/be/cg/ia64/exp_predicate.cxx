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


/* CGEXP routines for manipulating predicate registers */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "topcode.h"
#include "tn.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"

inline void Alloc_Result_TNs(TN * &tn, TN * &ctn)
{
  // note that if we create new tns, they are used in Exp_Pred_* routines,
  // but will not be seen outside those routines; if caller needs to
  // see the values, it must pass in a valid tn.
  if (tn == NULL) tn = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
  if (ctn == NULL) ctn = Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
}


void Exp_Pred_Set(TN *dest, TN *cdest, INT val, OPS *ops)
{
  TOP top;
  Is_True((val & -2) == 0, ("can't set a predicate to %d", val));
  Alloc_Result_TNs(dest, cdest);
  top = (val == 0) ? TOP_cmp_ne_unc : TOP_cmp_eq_unc;
  Build_OP(top, dest, cdest, True_TN, Zero_TN, Zero_TN, ops);
}


void Exp_Pred_Copy(TN *dest, TN *cdest, TN *src, OPS *ops)
{
  if (TN_is_true_pred(src)) {
    Exp_Pred_Set(dest, cdest, 1, ops);
    return;
  }

  Alloc_Result_TNs(dest, cdest);
  Build_OP(TOP_cmp_eq_unc, dest, cdest, src, Zero_TN, Zero_TN, ops);
}


void Exp_Pred_Complement(TN *dest, TN *cdest, TN *src, OPS *ops)
{
  if (TN_is_true_pred(src)) {
    Exp_Pred_Set(dest, cdest, !1, ops);
    return;
  }

  Alloc_Result_TNs(dest, cdest);
  Build_OP(TOP_cmp_eq_unc, dest, cdest, True_TN, Zero_TN, Zero_TN, ops);
  Build_OP(TOP_cmp_ne_and, dest, cdest, src, Zero_TN, Zero_TN, ops);
}


void Exp_Pred_Compare(TN *dest, TN *cdest, TN *src1, TN *src2, VARIANT variant,
		      OPS *ops)
{
  TOP cmp;

  Alloc_Result_TNs(dest, cdest);

  cmp = Pick_Compare_TOP(&variant, &src1, &src2, ops);
  FmtAssert(cmp != TOP_UNDEFINED, ("Exp_Pred_Compare: unexpected comparison"));
  if (TOP_is_flop(cmp)) {
    Build_OP(cmp, dest, cdest, True_TN,
	     Gen_Enum_TN(ECV_sf_s0), src1, src2, ops);
  } else {
    Build_OP(cmp, dest, cdest, True_TN, src1, src2, ops);
  }
}
