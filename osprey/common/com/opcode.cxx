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


/**
*** Implementation of external functions from opcode.h.
**/

/** $Revision: 1.1.1.1 $
*** $Date: 2005/10/21 19:00:00 $
*** $Author: marcel $
*** $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/opcode.cxx,v $
**/

#include "opcode.h"

#define opcode_C      "opcode.c"


/**
*** Looks up the name of this operator in the table from opcode_gen.c
**/

const char *OPERATOR_name(OPERATOR opr)
{
  Is_True(opr >= OPERATOR_FIRST && opr <= OPERATOR_LAST,
	  ("Bad OPERATOR %d", opr));

  return (const char *) OPERATOR_info[opr]._name;
}

/**
*** To make this lookup routine routine simple but efficient, we use a closed
*** hashing scheme.
**/

BOOL Operator_To_Opcode_Table_Inited = FALSE;
void Init_Operator_To_Opcode_Table(void)
{
    Operator_To_Opcode_Table_Inited = TRUE;
}


/* ====================================================================
 *
 * OPCODE OPCODE_commutative_op(OPCODE opc)
 *
 * If opc is commutative, return the opcode for whatever operation
 * gives equivalent results. If the operator isn't commutative, return 0.
 *
 * ====================================================================
 */

OPCODE OPCODE_commutative_op( OPCODE opc )
{
   
   OPCODE rop = (OPCODE) 0;
   OPERATOR opr = OPCODE_operator(opc);
   TYPE_ID rtype = OPCODE_rtype(opc);
   TYPE_ID desc = OPCODE_desc(opc);
   
   switch (opr) {
      /* These ops are commutative and don't need to be altered */
    case OPR_ADD:
    case OPR_MPY:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BNOR:
    case OPR_BXOR:
    case OPR_LAND:
    case OPR_LIOR:
    case OPR_EQ:
    case OPR_NE:
      rop = opc;
      break;

      /* these are treated specially */
    case OPR_GT:
      rop = OPCODE_make_op(OPR_LT, rtype, desc);
      break;
    case OPR_GE:
      rop = OPCODE_make_op(OPR_LE, rtype, desc);
      break;
    case OPR_LT:
      rop = OPCODE_make_op(OPR_GT, rtype, desc);
      break;
    case OPR_LE:
      rop = OPCODE_make_op(OPR_GE, rtype, desc);
      break;

      /* Anything else is a null */
    default:
      break;
   }

   return (rop);
}
