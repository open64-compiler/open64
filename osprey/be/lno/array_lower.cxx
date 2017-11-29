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


// -*-C++-*-
// ====================================================================
// ====================================================================
//
// Module: array_lower.cxx
// $Revision$
// $Date$
// $Author$
// $Source$
//
// Revision history:
//  dd-mmm-95 - Original Version
//
// Description:
// 
//  Lower all accesses to reshaped array variables.
//
// 
// Implementation Notes:
//
//  During array lowering we need to generate divfloor, divceil and 
//  modulus operations.  Since all arrays always start at 0, the array
//  index functions are always positive.  Also, the number of processors
//  and the block sizes are always positive.  We use this information to
//  perform the following optimizations:
// 
//  divfloor(a,b)  ==>  a / b
//  divceil(a,b)   ==>  (a + b - 1) / b
//  mod(a,b)       ==>  rem(a,b)
// 
//  We can use either mod or rem for the modulus operation since they have
//  the same value when both operands are the same sign.
//  Since the global optimizer can optimize div and rem operations
//  on the same operands and only perform a single divide for both, we
//  generate rem operations.
//
// ====================================================================
// ====================================================================
//
// TODO:
//
// o Try to form valid array expressions out of invalid ones by de-linearizing
// o Can turn I/O statements IO_ITEMs of kind IOL_ARRAY into implied dos
// 
// ====================================================================

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#include <sys/types.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>

#include "pu_info.h"
#include "opt_du.h" 
#include "wn.h"
#include "lego.h"
#include "array_lower.h"
#include "al_loop.h"

/* ========================================================================
   Public Function Declarations
   ======================================================================== */

extern void Lower_Array_Accesses(WN* func_nd);

/* ========================================================================
   Public Function Implementations
   ======================================================================== */

void
Lower_Array_Accesses(WN *func_nd)
{
  if (WN_operator(func_nd) != OPR_FUNC_ENTRY) {
    DevWarn ("Lower_Array_Accesses called with non-func node (opcode=%d)\n",
             WN_opcode(func_nd));
  }

  ARRAY_LOWER_LOOP *root_node = 
    CXX_NEW(ARRAY_LOWER_LOOP(NULL, func_nd, -1), LEGO_pool);

  root_node->Build_Loop(WN_func_body(func_nd));
  root_node->Process_Loop();

  CXX_DELETE(root_node, LEGO_pool);
}
