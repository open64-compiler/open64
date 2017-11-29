/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

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
  FmtAssert(FALSE, ("Not Yet Implemented"));
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
