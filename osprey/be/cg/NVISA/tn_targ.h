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


/* ====================================================================
 * ====================================================================
 *
 *  TN utility routines which include target dependencies.
 *
 *  THIS FILE IS ONLY TO BE INCLUDE BY ../tn.h!!!!
 *
 * ====================================================================
 * ====================================================================
 */

//
// Generate TN to contain a general predicate value
//

#include "targ_sim.h"

inline TN*
Gen_Predicate_TN()
{
  return Build_RCLASS_TN(ISA_REGISTER_CLASS_predicate);
}

//
// No fcc registers for this arch.
//
inline BOOL TN_is_fcc_register (const TN *tn)
{
  return FALSE;
}

// always have to add REGISTER_MIN (1) to isa register numbers

inline TN*
Unused_TN (ISA_REGISTER_CLASS rc)
{
  // the unused tn is $r0 (which is register #1)
  return Build_Dedicated_TN( rc, 
        // assume is same reg for all classes
	ABI_PROPERTY_integer_unused_Register + REGISTER_MIN, 0 );
}

inline TN*
Tid_TN (INT i)
{
  FmtAssert((i >= 0 && i <= 2), ("Tid_TN must be 0,1,2"));
  return Build_Dedicated_TN(ABI_PROPERTY_tid_Register_Class, 
	ABI_PROPERTY_tid_First_Register + i + REGISTER_MIN, 0);
}

inline TN*
Ntid_TN (INT i)
{
  FmtAssert((i >= 0 && i <= 2), ("Ntid_TN must be 0,1,2"));
  return Build_Dedicated_TN(ABI_PROPERTY_ntid_Register_Class, 
	ABI_PROPERTY_ntid_First_Register + i + REGISTER_MIN, 0);
}

inline TN*
Ctaid_TN (INT i)
{
  FmtAssert((i >= 0 && i <= 2), ("Ctaid_TN must be 0,1,2"));
  return Build_Dedicated_TN(ABI_PROPERTY_ctaid_Register_Class, 
	ABI_PROPERTY_ctaid_First_Register + i + REGISTER_MIN, 0);
}

inline TN*
Nctaid_TN (INT i)
{
  FmtAssert((i >= 0 && i <= 2), ("Nctaid_TN must be 0,1,2"));
  return Build_Dedicated_TN(ABI_PROPERTY_nctaid_Register_Class, 
	ABI_PROPERTY_nctaid_First_Register + i + REGISTER_MIN, 0);
}

inline TN*
Clock_TN (void)
{
  return Build_Dedicated_TN(ABI_PROPERTY_clock_Register_Class, 
	ABI_PROPERTY_clock_Register + REGISTER_MIN, 0);
}

// what mtype best represents the given tn
inline TYPE_ID
Mtype_Of_TN (TN *tn)
{
  ISA_REGISTER_CLASS rc = TN_register_class(tn);
  switch (rc) {
  case ISA_REGISTER_CLASS_integer16: 
	// since regs are unsigned, use unsigned types
	if (TN_size(tn) == 1) return MTYPE_U1;
	else return MTYPE_U2;
  case ISA_REGISTER_CLASS_integer: return MTYPE_U4;
  case ISA_REGISTER_CLASS_integer64: return MTYPE_U8;
  case ISA_REGISTER_CLASS_float: return MTYPE_F4;
  case ISA_REGISTER_CLASS_float64: return MTYPE_F8;
  case ISA_REGISTER_CLASS_predicate: return MTYPE_B;
  default: FmtAssert(FALSE, ("unexpected rclass"));
  	return MTYPE_UNKNOWN;
  }
}
