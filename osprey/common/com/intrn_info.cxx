/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */

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

#include "intrn_info.h"
#include "errors.h"

// On many platforms, the native dynamic libm defines functions that
// we want to use, but it hides the leading underscore characters from
// us.

#if defined(TARG_X8664) || defined(TARG_IA32)
#define __
#else
#define __ "__"
#endif

const intrn_info_t intrn_info[] = {
  { /* NONE */
	NOT_BYVAL, NOT_PURE, SIDEEFFECTS, DOES_RETURN, NOT_ACTUAL, NOT_CGINTRINSIC, NOT_SLAVE,
	IRETURN_UNKNOWN, NULL, NULL, NULL},

/* All intrinsic info are moved to intrn_entry.def */
#define NEED_INTRN_INFO
#  include "intrn_entry.def"
#undef NEED_INTRN_INFO

  { /* LAST */
	NOT_BYVAL, NOT_PURE, SIDEEFFECTS, DOES_RETURN, NOT_ACTUAL, NOT_CGINTRINSIC, NOT_SLAVE,
	IRETURN_UNKNOWN, NULL, NULL, NULL},
};

TYPE_ID INTRN_Size_Mtype (const INTRINSIC id)
{
  switch (id) {
  case INTRN_COMPARE_AND_SWAP_I1:
  case INTRN_LOCK_TEST_AND_SET_I1:
  case INTRN_LOCK_RELEASE_I1:
  case INTRN_FETCH_AND_ADD_I1:
  case INTRN_ADD_AND_FETCH_I1:
  case INTRN_SUB_AND_FETCH_I1:
  case INTRN_OR_AND_FETCH_I1:
  case INTRN_XOR_AND_FETCH_I1:
  case INTRN_AND_AND_FETCH_I1:
  case INTRN_NAND_AND_FETCH_I1:
  case INTRN_FETCH_AND_SUB_I1:
  case INTRN_FETCH_AND_OR_I1:
  case INTRN_FETCH_AND_XOR_I1:
  case INTRN_FETCH_AND_AND_I1:
  case INTRN_FETCH_AND_NAND_I1:
        return MTYPE_I1;
  case INTRN_COMPARE_AND_SWAP_I2:
  case INTRN_LOCK_TEST_AND_SET_I2:
  case INTRN_LOCK_RELEASE_I2:
  case INTRN_FETCH_AND_ADD_I2:
  case INTRN_ADD_AND_FETCH_I2:
  case INTRN_SUB_AND_FETCH_I2:
  case INTRN_OR_AND_FETCH_I2:
  case INTRN_XOR_AND_FETCH_I2:
  case INTRN_AND_AND_FETCH_I2:
  case INTRN_NAND_AND_FETCH_I2:
  case INTRN_FETCH_AND_SUB_I2:
  case INTRN_FETCH_AND_OR_I2:
  case INTRN_FETCH_AND_XOR_I2:
  case INTRN_FETCH_AND_AND_I2:
  case INTRN_FETCH_AND_NAND_I2:
        return MTYPE_I2;
  case INTRN_COMPARE_AND_SWAP_I4:
  case INTRN_LOCK_TEST_AND_SET_I4:
  case INTRN_LOCK_RELEASE_I4:
  case INTRN_FETCH_AND_ADD_I4:
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_FETCH_AND_SUB_I4:
  case INTRN_FETCH_AND_OR_I4:
  case INTRN_FETCH_AND_XOR_I4:
  case INTRN_FETCH_AND_AND_I4:
  case INTRN_FETCH_AND_NAND_I4:
        return MTYPE_I4;
  case INTRN_COMPARE_AND_SWAP_I8:
  case INTRN_LOCK_TEST_AND_SET_I8:
  case INTRN_LOCK_RELEASE_I8:
  case INTRN_FETCH_AND_ADD_I8:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I8:
  case INTRN_FETCH_AND_SUB_I8:
  case INTRN_FETCH_AND_OR_I8:
  case INTRN_FETCH_AND_XOR_I8:
  case INTRN_FETCH_AND_AND_I8:
  case INTRN_FETCH_AND_NAND_I8:
  case INTRN_SYNCHRONIZE:
        return MTYPE_I8;
  default:
        FmtAssert(FALSE, ("Unexpected intrinsic %d", id));
        return MTYPE_UNKNOWN;
  }
}
