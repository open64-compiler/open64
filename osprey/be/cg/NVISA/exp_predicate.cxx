/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

/*

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

*/


/* CGEXP routines for manipulating predicate registers */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "tn.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"

inline void Alloc_Result_TNs(TN * &tn, TN * &ctn)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}


void Exp_Pred_Set(TN *dest, TN *cdest, INT val, OPS *ops)
{
  FmtAssert(val == 0 || val == 1, ("Can only set predicate to 0 or 1"));
  TOP opc = TOP_setp_ne_s32;
  TN *zero_tn = Expand_Mtype_Immediate_Into_Register (
        Gen_Literal_TN(0,4), MTYPE_I4, ops);
  TN *val_tn = Expand_Mtype_Immediate_Into_Register (
        Gen_Literal_TN(val,4), MTYPE_I4, ops);
  Build_OP (opc, dest, val_tn, zero_tn, ops);
}


void Exp_Pred_Copy(TN *dest, TN *cdest, TN *src, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}


void Exp_Pred_Complement(TN *dest, TN *cdest, TN *src, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}


void Exp_Pred_Compare(TN *dest, TN *cdest, TN *src1, TN *src2, VARIANT variant,
		      OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}
